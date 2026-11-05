"""Check the Einstein Gamma-Gamma representative before momentum conservation."""
import json
import os
from collections import defaultdict
from pathlib import Path

from curvature import Expansion, dot, key, splits, tidy
from check_vertices import read_rules, relabel, combine


def bulk_vertex(n):
    jet = Expansion(n)
    terms = defaultdict(int)
    for volume,rest in splits(jet.full):
        for inverse,connections in splits(rest):
            for left,right in splits(connections):
                for w,f,a,b,c in jet.gamma(left):
                    for z,h,d,u,v in jet.gamma(right):
                        # In -2 sqrt(-g) g^{mn}(Gamma^r_ms Gamma^s_nr
                        # - Gamma^r_mn Gamma^s_rs), the two i's give -1.
                        for sign, ends, lines in (
                            (1,(b,u),(dot(a,v),dot(c,d))),
                            (-1,(b,c),(dot(a,u),dot(d,v)))):
                            for wi,fi in jet.inverse(*ends,inverse):
                                for wv,fv in jet.volume(volume):
                                    terms[key(*f,*h,*lines,*fi,*fv)] += 2*sign*w*z*wi*wv
    return tidy(terms)


if __name__ == '__main__':
    root=Path(os.environ['QG_RULE_ROOT']);out=Path(os.environ['QG_OUTPUT'])
    report={}
    for n in (3,4):
        sectors,_=read_rules(root/'Libs'/f'QuadraticGravityVertex_{n-2}',n)
        raw=relabel(sectors[0],lambda i: i%n if i<2*n else i-n)
        bulk=bulk_vertex(n)
        report[n]={'reference_monomials':len(bulk),'stored_monomials':len(raw),
                   'residual_monomials':len(combine((1,bulk),(-1,raw))),
                   'opposite_sign_detected':bool(combine((1,bulk),(1,raw)))}
    (out/'bulk.json').write_text(json.dumps(report,indent=2))
    assert all(v['residual_monomials']==0 and v['opposite_sign_detected'] for v in report.values()),report
    print(report)
    (out/'completion.json').write_text(json.dumps(dict(Status='Completed',Passed=1,Failed=0)))
