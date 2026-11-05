"""Leading Laurent coefficient of the complete five-point tree at a soft outgoing leg."""
from fractions import Fraction as Q
from math import lcm,prod
import time
from PhysicalTree import NativeGraph, Tree, cuts, relabel, read, dot
from SourceTree import Source

SOFT=( -1,0,-1,0)
X,Y,Z=(0,1,0,0),(0,0,1,0),(0,0,0,1)


def path(t):
    # Outgoing p3 remains along x. Outgoing p4 recoils against p3 and t*q.
    e3=Q(12*(6-t),12-t)
    e4=12-t-e3
    ps=((6,0,0,6),(6,0,0,-6),(-e3,-e3,0,0),(-e4,e3,t,0),tuple(t*v for v in SOFT))
    assert all(dot(p,p)==0 for p in ps) and not any(map(sum,zip(*ps)))
    rotated_y=(0,-t/e4,e3/e4,0)
    frames=((X,Y),(X,Y),(Y,Z),(rotated_y,Z),(X,Z))
    states=[]
    for j,(a,b) in enumerate(frames):
        c=1 if j==4 else j+1
        u=tuple(i+c*k for i,k in zip(a,b));v=tuple(c*i-k for i,k in zip(a,b))
        assert dot(a,b)==0 and dot(a,a)==dot(b,b)==-1
        assert dot(ps[j],u)==dot(ps[j],v)==dot(u,v)==0
        states.append((u,v))
    return ps,tuple(states)


def clear_denominators(vectors):
    scale=lcm(*(Q(x).denominator for v in vectors for x in v))
    return scale,tuple(tuple(int(scale*x) for x in v) for v in vectors)


def run_soft(data):
    start=time.monotonic()
    ps,states=path(Q(0))
    _,ps=clear_denominators(ps)
    states=tuple(tuple(tuple(map(int,v)) for v in pair) for pair in states)
    source=Source()
    m4=source.four_point(ps[:4],states[:4])
    assert m4
    u,v=states[4]
    prediction={i:m4*Q(dot(k,u)*dot(k,v),2*dot(k,SOFT)) for i,k in enumerate(ps[:4])}
    assert all(prediction.values()) and sum(prediction.values())
    census=read(data/'graphs.json');templates={i:read(data/f'graph-{i}.json') for i in (2,14)}
    by_leg={i:Q(0) for i in range(4)};residues={}
    for row in census:
        key=cuts(row)
        singular=[]
        for pair in key:
            p=tuple(sum(ps[i][j] for i in pair) for j in range(4))
            if not dot(p,p):
                assert 4 in pair
                singular.append(pair)
        # The contact is polynomial; every other regular graph has finite denominators.
        if not singular:
            residues[row['Position']]=Q(0)
            continue
        (pair,)=singular
        h=next(i for i in pair if i!=4)
        perm=relabel(key)
        template=templates[2 if len(key)==1 else 14]
        local=lambda pair:tuple(sorted(perm.index(i) for i in pair))
        # (p_h(t)+t*q)^2 = 2*t*p_h(t).q, including along the recoiling hard leg.
        g=NativeGraph(template,tuple(ps[j] for j in perm),tuple(local(e) for e in key),
                      (local(pair),2*dot(ps[h],SOFT)))
        value=g.value(tuple(states[j] for j in perm))
        residues[row['Position']]=value
        by_leg[h]+=value
    assert len(residues)==26 and by_leg==prediction,(by_leg,prediction)
    expected=sum(prediction.values())
    assert expected!=-expected and all(expected-b!=expected for b in by_leg.values())

    samples=[]
    for t in map(Q,['1/16','1/32','1/64']):
        p,pol=path(t)
        pscale,integer_p=clear_denominators(p)
        normalisations,integer_pol=zip(*(clear_denominators(s) for s in pol))
        values=Tree(data,integer_p).pieces(integer_pol)
        # GR trees have net momentum degree two. Each leg is a bilinear dyad.
        total=sum(values.values())/(pscale**2*prod(n*n for n in normalisations))
        samples.append({'t':t,'t_times_M5':t*total,'difference':t*total-expected})
    assert 0<abs(samples[2]['difference'])<abs(samples[1]['difference'])<abs(samples[0]['difference'])
    return {'hard_M4':m4,'soft_direction':SOFT,'hard_point':ps[:4],'polarisations':states,
            'expected_by_leg':prediction,'actual_by_leg':by_leg,'leading_residue':expected,
            'individual_residues':residues,'finite_samples':samples,'seconds':time.monotonic()-start}
