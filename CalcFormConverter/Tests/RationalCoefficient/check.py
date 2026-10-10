#!/usr/bin/env python3
"""Bounded FORM/TFORM checks for the isolated rational-coefficient experiment.

No package edits, downloads or installations. Every invocation uses a fresh
working directory. Pass that directory explicitly to retain sources/results.
The 60-second timeout bounds each small FORM test, not a public calculation.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
from prepare import prepare


def call(engine, work, source, success=True):
    result = subprocess.run(engine + ['-q', source], cwd=work, capture_output=True,
                            text=True, timeout=60)
    if success:
        assert result.returncode == 0, result.stdout + result.stderr
    else:
        assert result.returncode != 0, 'Unsupported input was accepted.'
    return result


def control(root, label, expression, abbreviations=(), extra='', denominator=False):
    entries = [{'Name': x, 'Kind': 'Scalar', 'Expression': ['Symbol', 'Global`' + x]}
               for x in ('x', 'y')]
    entries += list(abbreviations)
    if denominator:
        entries += [{'Name': name, 'Kind': 'Denominator', 'Expression': []} for name in ('cfd1', 'cfd2')]
    raw = json.dumps({'Version': 1, 'Entries': entries}).encode()
    path = root / label
    path.mkdir()
    mapping = path / 'input.map.json'
    mapping.write_bytes(raw)
    digest = hashlib.sha256(raw).hexdigest()
    names = ','.join(e['Name'] for e in entries)
    source = path / 'input.frm'
    source.write_text(f'''Off Statistics;
Symbols {names};
{extra}
Local cfcResult={expression};
''' + ('Bracket+ cfd1,cfd2;\n' if denominator else '') + f'''.sort
* Dedicated, overwritten result file.
#create <baseline.out>
#write <baseline.out> "CFC1 {digest}"
#write <baseline.out> "%E",cfcResult
#close <baseline.out>
.end
''')
    return source, mapping


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', type=Path)
    args = parser.parse_args()
    root = args.directory.resolve()
    root.mkdir(parents=True, exist_ok=False)
    here = Path(__file__).resolve().parent
    engines = [('FORM', [shutil.which('form')])]
    if engines[0][1][0] is None:
        raise SystemExit('FORM is required.')
    if shutil.which('tform'):
        engines.append(('TFORM10', [shutil.which('tform'), '-w10']))
    checks = []
    for name, engine in engines:
        work = prepare(here / 'Original.frm', here / 'Original.map.json', root / name)
        call(engine, work, 'candidate.frm')
        assert (work / 'difference.out').read_text().strip() == '0'
        checks.append(name + ': exact original coefficient equality')
        # Independently transcribed saved notebook formula, including its sign.
        for item in ('NotebookNumerator.inc', 'NotebookDenominator.inc'):
            shutil.copyfile(here / item, work / item)
        source = (work / 'candidate.frm').read_text()
        rules = source[source.index('id cfa1='):source.index('* After reduction')]
        comparison = '''Drop cfcProbeDifference;
Local cfcNotebookDifference=cfcResult*(
#include NotebookDenominator.inc
)-($cfcProbeKey)*(
#include NotebookNumerator.inc
);
''' + rules + '''#create <notebook-difference.out>
#write <notebook-difference.out> "%E",cfcNotebookDifference
#close <notebook-difference.out>
.end
'''
        (work / 'notebook-check.frm').write_text(source.rsplit('.end', 1)[0] + comparison)
        call(engine, work, 'notebook-check.frm')
        assert (work / 'notebook-difference.out').read_text().strip() == '0'
        checks.append(name + ': exact saved notebook formula equality')
        assert 'cfcProbe' not in (work / 'candidate.out').read_text()
        assert 'factor_' not in (work / 'candidate.out').read_text()
        checks.append(name + ': no internal identifiers in result')

    inverse = {'Name': 'cfa1', 'Kind': 'Abbreviation', 'Expression':
               ['Power', ['Plus', ['Symbol', 'Global`x'], ['Integer', '-1']], ['Integer', '-1']]}
    controls = [('polynomial', 'x^2-2*x*y+y^2', ()),
                ('rational', '(x^2-1)*cfa1', (inverse,)),
                ('cancellation', '(x-1)*cfa1-1', (inverse,)),
                ('negative_powers', '(x+y)/x^2-y/x^2', ()),
                ('rational_numbers', '(2*x^2-4*x*y+2*y^2)/3', ()),
                ('unit', '1', ()), ('zero', '0', ())]
    for label, expression, abbr in controls:
        source, mapping = control(root, label, expression, abbr)
        for name, engine in engines:
            work = prepare(source, mapping, root / (label + '-' + name))
            call(engine, work, 'candidate.frm')
            assert (work / 'difference.out').read_text().strip() == '0'
            checks.append(name + ': ' + label)

    for label, expression, extra, denominator in [
            ('functions', 'f(x)+y', 'CFunction f;', False),
            ('two_products', 'cfd1*x+cfd2*y', '', True)]:
        source, mapping = control(root, label, expression, extra=extra, denominator=denominator)
        for name, engine in engines:
            work = prepare(source, mapping, root / (label + '-' + name))
            call(engine, work, 'candidate.frm', success=False)
            checks.append(name + ': rejects ' + label)

    # Preparation failures must not create a destination or change inputs.
    source, mapping = control(root, 'unsupported', 'x+i_*y')
    for label, edit_source, edit_mapping in [
            ('complex', lambda s: s, lambda d: d),
            ('version', lambda s: s.replace('i_', ''), lambda d: dict(d, Version=2)),
            ('header', lambda s: s.replace('i_', '').replace('CFC1 ', 'CFC1 bad'), lambda d: d),
            ('boundary', lambda s: s.replace('i_', '').replace('* Dedicated, overwritten result file.', ''), lambda d: d)]:
        s = source.read_text()
        d = json.loads(mapping.read_bytes())
        test_source = root / (label + '.frm')
        test_map = root / (label + '.json')
        test_source.write_text(edit_source(s))
        test_map.write_text(json.dumps(edit_mapping(d)))
        destination = root / (label + '-rejected')
        try:
            prepare(test_source, test_map, destination)
        except ValueError:
            assert not destination.exists()
            checks.append('preparation rejects ' + label)
        else:
            raise AssertionError(label)
    (root / 'checks.json').write_text(json.dumps(checks, indent=2))
    print(f'{len(checks)} checks passed. Artifacts: {root}')


if __name__ == '__main__':
    main()
