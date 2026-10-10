#!/usr/bin/env python3
"""Prepare the focused scalar-coefficient experiment; do not run FORM.

This is a test adapter, not a second public exporter. Input must be a trusted
version-one CalcFormExport program and its mapping. The source prefix through
normal Lorentz processing is reused verbatim. All algebra, denominator-key
extraction, rational cancellation, factorisation and equality testing run in
FORM. Python only renders the procedure and copies the mapping.

Deliberate limits: a single complete propagator product, no free indices,
complex coefficients, scalar functions or non-rational abbreviations. The
normal converter is unchanged. Use a new destination directory for each run.
"""
import argparse
import hashlib
import itertools
import json
from pathlib import Path


def prepare(source: Path, mapping: Path, destination: Path):
    text = source.read_text()
    raw_mapping = mapping.read_bytes()
    data = json.loads(raw_mapping)
    if data.get('Version') != 1:
        raise ValueError('This experiment requires a version-one mapping.')
    entries = data['Entries']
    allowed = {'Scalar', 'Vector', 'Abbreviation', 'Denominator'}
    if any(e['Kind'] not in allowed for e in entries):
        raise ValueError('Free indices and matrix/epsilon/colour objects are excluded.')
    boundary = '* Dedicated, overwritten result file.'
    if text.count(boundary) != 1:
        raise ValueError('The generated-program boundary is absent or ambiguous.')
    prefix = text.split(boundary)[0]
    if 'i_' in prefix:
        raise ValueError('Complex coefficients are outside this focused experiment.')
    digest = hashlib.sha256(raw_mapping).hexdigest()
    if '"CFC1 ' + digest + '"' not in text:
        raise ValueError('Source and mapping digests do not agree.')
    scalars = {tuple(e['Expression']): e['Name'] for e in entries if e['Kind'] == 'Scalar'}

    def polynomial(expr):
        tag, *args = expr
        if tag == 'Symbol':
            if tuple(expr) not in scalars:
                raise ValueError('An abbreviation contains an unmapped scalar.')
            return scalars[tuple(expr)]
        if tag == 'Integer':
            return str(int(args[0]))
        if tag == 'Rational':
            return '(' + str(int(args[0])) + '/' + str(int(args[1])) + ')'
        if tag in ('Plus', 'Times'):
            return '(' + ('+' if tag == 'Plus' else '*').join(map(polynomial, args)) + ')'
        if tag == 'Power' and args[1][0] == 'Integer' and int(args[1][1]) >= 0:
            return '(' + polynomial(args[0]) + ')^' + str(int(args[1][1]))
        raise ValueError('Only polynomial bases of reciprocal integer powers are supported.')

    rules = []
    for entry in entries:
        if entry['Kind'] == 'Abbreviation':
            expr = entry['Expression']
            if len(expr) != 3 or expr[0] != 'Power' or expr[2][0] != 'Integer' or int(expr[2][1]) >= 0:
                raise ValueError('A scalar abbreviation is not a reciprocal polynomial.')
            rules.append(f"id {entry['Name']}=cfcProbeRat(1,({polynomial(expr[1])})^{-int(expr[2][1])});")
    vectors = [e['Name'] for e in entries if e['Kind'] == 'Vector']
    dots = [(f'cfcProbeX{i + 1}', a + '.' + b)
            for i, (a, b) in enumerate(itertools.combinations_with_replacement(vectors, 2))]
    rules.extend(f'id {dot}={name};' for name, dot in dots)
    for name in list(scalars.values()) + [name for name, _ in dots]:
        rules.extend([f'id {name}^cfcProbePower?pos_=cfcProbeRat({name}^cfcProbePower,1);',
                      f'id {name}^cfcProbePower?neg_=cfcProbeRat(1,{name}^(-cfcProbePower));'])
    rules.extend(['Multiply cfcProbeRat(1,1);', '.sort'])
    reduction = '\n'.join(rules) + '\n'
    extra = ['cfcProbePower', 'cfcProbeNum', 'cfcProbeDen'] + [name for name, _ in dots]
    declarations = 'Symbols ' + ','.join(extra) + ';\nCFunction cfcProbeRat;\n'
    # The declarations must precede every Local in the reused generated prefix.
    prefix = prefix.replace('Off Statistics;', 'Off Statistics;\n' + declarations, 1)
    denominators = [e['Name'] for e in entries if e['Kind'] == 'Denominator']
    extraction = '''
#$cfcProbeKey=1;
'''
    if denominators:
        extraction = '''
#$cfcProbeKey=0;
Keep Brackets;
$cfcProbeKey=$cfcProbeKey+term_;
Bracket+ ''' + ','.join(denominators) + ''';
ModuleOption noparallel;
.sort
#$cfcProbeKeyCount=termsin_($cfcProbeKey);
#if `$cfcProbeKeyCount' != 1
#message Expected exactly one complete propagator product.
#terminate 1
#endif
'''
    body = prefix + extraction + '''
Hide cfcResult;
.sort
PolyRatFun cfcProbeRat;
Local cfcProbe=cfcResult/($cfcProbeKey);
''' + reduction + '''
* After reduction the coefficient must be one rational polynomial or zero.
PolyRatFun;
#$cfcProbeNum=0;
#$cfcProbeDen=1;
if (match(cfcProbeRat(cfcProbeNum?$cfcProbeNum,cfcProbeDen?$cfcProbeDen)));
  id cfcProbeRat(?a)=1;
endif;
ModuleOption noparallel;
.sort
#$cfcProbeResidual=cfcProbe;
#if ( "`$cfcProbeResidual'" != "1" ) && ( "`$cfcProbeResidual'" != "0" )
#message The coefficient contains unsupported residual objects.
#terminate 1
#endif
Drop cfcProbe;
Local cfcProbeNumerator=$cfcProbeNum;
Local cfcProbeDenominator=$cfcProbeDen;
Factorize cfcProbeNumerator,cfcProbeDenominator;
.sort
Format nospaces;
#create <candidate.out>
'''
    body += f'#write <candidate.out> "CFC1 {digest}"\n'
    body += '#write <candidate.out> "%$*(",$cfcProbeKey\n'
    for k, expression in enumerate(['cfcProbeNumerator', 'cfcProbeDenominator']):
        include = 'numerator.inc' if k == 0 else 'denominator.inc'
        if k:
            body += '#write <candidate.out> "/"\n'
        body += f'#create <{include}>\n'
        # factor_ is an internal FORM representation: only the factor values
        # are printed. It must never leak into a converter result.
        body += f'''#$cfcProbeFactorCount=numfactors_({expression});
#if `$cfcProbeFactorCount' == 0
#write <candidate.out> "(%E)",{expression}
#write <{include}> "(%E)",{expression}
#else
#write <candidate.out> "(1"
#write <{include}> "("
#do cfcProbeFactor=1,`$cfcProbeFactorCount'
#$cfcProbeFactorValue={expression}[factor_^`cfcProbeFactor'];
#inside $cfcProbeFactorValue
'''
        body += '\n'.join(f'id {name}={dot};' for name, dot in dots)
        body += f'''
#endinside
#write <candidate.out> "*(%$)",$cfcProbeFactorValue
#write <{include}> "(%$)*",$cfcProbeFactorValue
#enddo
#write <candidate.out> ")"
#write <{include}> "1)"
#endif
#close <{include}>
'''
    body += '''#write <candidate.out> ")"
#close <candidate.out>
* Independently reread the printed factors. This checks both the algebra and
* the output serialisation by cross-multiplication against the original.
PolyRatFun cfcProbeRat;
Drop cfcProbeNumerator,cfcProbeDenominator;
Local cfcProbeDifference=cfcResult*(
#include denominator.inc
)-($cfcProbeKey)*(
#include numerator.inc
);
''' + reduction + '''
#create <difference.out>
#write <difference.out> "%E",cfcProbeDifference
#close <difference.out>
.end
'''
    destination.mkdir(parents=True, exist_ok=False)
    (destination / 'candidate.frm').write_text(body)
    (destination / 'calculate.frm').write_text(body.split('* Independently reread')[0] + '.end\n')
    (destination / 'candidate.map.json').write_bytes(raw_mapping)
    # Keep the baseline runnable entirely within this owned directory.
    # Only output paths are changed; expression and processing stay unchanged.
    baseline = text
    import re
    baseline = re.sub(r'<[^<>\n]*\.out>', '<baseline.out>', baseline)
    (destination / 'baseline.frm').write_text(baseline)
    return destination


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('mapping', type=Path)
    parser.add_argument('destination', type=Path)
    args = parser.parse_args()
    print(prepare(args.source, args.mapping, args.destination))
