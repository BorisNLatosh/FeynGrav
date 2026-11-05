"""Exact cubic/quartic comparison; run in a bounded scope with QG_RULE_ROOT and QG_OUTPUT."""
import ast
import json
import os
import re
import sys
from collections import defaultdict
from fractions import Fraction
from functools import lru_cache
from itertools import product
from pathlib import Path

from curvature import Expansion, dot, key, tidy

# Parse only the flat Pair-product grammar used by the two checked-in files.
PAIR = re.compile(r'Pair\[(LorentzIndex|Momentum)\[([mnp])(\d+),\s*D\],\s*(LorentzIndex|Momentum)\[([mnp])(\d+),\s*D\]\](?:\^(\d+))?')
WEIGHT = re.compile(r'^[0-9()*/+\-]+$')
MASS = re.compile(r'/\\\[GothicM\]([02])\^2$')


@lru_cache(None)
def number(s):
    if not WEIGHT.fullmatch(s):
        raise ValueError(s)
    def walk(node):
        if isinstance(node, ast.Constant) and type(node.value) is int:
            return Fraction(node.value)
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
            return -walk(node.operand)
        if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Mult, ast.Div)):
            a, b = walk(node.left), walk(node.right)
            return a*b if isinstance(node.op, ast.Mult) else a/b
        raise ValueError(s)
    return walk(ast.parse(s, mode='eval').body)


def read_rules(path,n):
    text = path.read_text().strip()
    pieces = re.split(r'\s+([+-])\s+',text)
    sectors = [defaultdict(int) for _ in range(3)]
    for sign,term in zip(['+']+pieces[1::2],pieces[::2]):
        pairs = []
        def read_pair(m):
            ends = []
            for head,kind,digits in (m.groups()[:3],m.groups()[3:6]):
                i = int(digits)-1
                if not (0<=i<n and (head=='Momentum')==(kind=='p')):
                    raise ValueError(m.group())
                ends.append('mnp'.index(kind)*n+i)
            pairs.extend([dot(*ends)]*int(m[7] or 1))
            return '1'
        coeff = PAIR.sub(read_pair,term).replace(' ','').replace('\n','')
        if sorted(j for pair in pairs for j in pair if j<2*n)!=list(range(2*n)):
            raise ValueError('missing/duplicated external slot')
        mass = MASS.search(coeff)
        sector = {'0':1,'2':2}[mass[1]] if mass else 0
        if mass: coeff = coeff[:mass.start()]
        kappa = r'\[Kappa]' + ('^2' if n==4 else '')
        if coeff.count(kappa)!=1 or coeff.count('I')!=1:
            raise ValueError('coupling or phase mismatch')
        weight = number(coeff.replace(kappa,'1').replace('I','1'))
        sectors[sector][key(*pairs)] += weight if sign=='+' else -weight
    return list(map(tidy,sectors)),len(pieces[::2])


def relabel(poly,fn):
    out = defaultdict(int)
    for mon,weight in poly.items():
        out[key(*(dot(fn(i),fn(j)) for i,j in mon))] += weight
    return tidy(out)


def combine(*items):
    out = defaultdict(int)
    for multiplier,poly in items:
        for mon,weight in poly.items():
            out[mon] += multiplier*weight
    return tidy(out)


def conserve(poly,n):
    """Eliminate only p_n = -sum of the other incoming momenta."""
    old = 2*n-1
    choices = lambda v: range(n,old) if v==old else (v,)
    out = defaultdict(int)
    for mon,weight in poly.items():
        sign = (-1)**sum(v==old for pair in mon for v in pair)
        options = [list(product(choices(a),choices(b))) for a,b in mon]
        for choice in product(*options):
            out[key(*(dot(*pair) for pair in choice))] += sign*weight
    return tidy(out)


# Trace directly on the stored free slots, or by half the auxiliary-vector
# Laplacian on the homogeneous degree-two action polynomial. (-1,-1) denotes D.
def trace(poly, left, right):
    out = defaultdict(int)
    for mon,weight in poly.items():
        keep,ends=[],[]
        for a,b in mon:
            if a in (left,right) and b in (left,right):
                keep.append((-1,-1))
            elif a in (left,right): ends.append(b)
            elif b in (left,right): ends.append(a)
            else: keep.append((a,b))
        if ends:
            if len(ends)!=2: raise ValueError('incomplete trace')
            keep.append(dot(*ends))
        out[key(*keep)] += weight
    return tidy(out)


def check(n,root):
    R,R2,Ric2=Expansion(n).sectors()
    expected=[combine((-2,R)),combine((Fraction(1,3),R2)),
              combine((Fraction(2,3),R2),(-2,Ric2))]
    stored,rows=read_rules(root/'Libs'/f'QuadraticGravityVertex_{n-2}',n)
    project=lambda i: i if i<0 else (i%n if i<2*n else i-n)
    result={'order':n,'stored_terms':rows,'sectors':{}}
    for name,raw,ref in zip(('Einstein','1/m0^2','1/m2^2'),stored,expected):
        pair_sym=all(relabel(raw,lambda i: (i+n if i==j else i-n if i==j+n else i))==raw for j in range(n))
        leg_sym=all(relabel(raw,lambda i: (i+1 if i%n==j else i-1 if i%n==j+1 else i))==raw for j in range(n-1))
        actual=relabel(raw,project)
        delta=combine((1,ref),(-1,actual)); residual=conserve(delta,n)
        trace_ref=conserve(trace(ref,0,0),n)
        trace_actual=conserve(relabel(trace(raw,0,n),project),n)
        trace_delta=combine((1,trace_ref),(-1,trace_actual))
        record={'raw_pair_symmetry':pair_sym,'raw_leg_symmetry':leg_sym,
                'stored_projected_monomials':len(actual),'pre_conservation_residual':len(delta),
                'conserved_residual':len(residual),'trace_residual':len(trace_delta),
                'trace_has_D':any((-1,-1) in m for m in trace_ref),
                'wrong_sign_detected':bool(conserve(combine((1,ref),(1,actual)),n)),
                'example_nonzero_residual':repr(next(iter(residual.items()),None))}
        record['passed']=pair_sym and leg_sym and not residual and not trace_delta and record['trace_has_D'] and record['wrong_sign_detected']
        result['sectors'][name]=record
    return result


if __name__=='__main__':
    n=int(sys.argv[1]);root=Path(os.environ['QG_RULE_ROOT']);dest=Path(os.environ['QG_OUTPUT'])
    result=check(n,root);(dest/'vertices.json').write_text(json.dumps(result,indent=2))
    ok=all(x['passed'] for x in result['sectors'].values())
    print(json.dumps(result),flush=True)
    (dest/'completion.json').write_text(json.dumps(dict(Status='Completed' if ok else 'Mismatch',Passed=int(ok),Failed=int(not ok))))
    sys.exit(0 if ok else 1)
