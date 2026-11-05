"""Exact component Ward tests on native FeynArts graph factors (signature +---).

Evaluate the intact contact, or sew two cubic currents with the native tensor
line by summing its four Lorentz indices. Every repeated index gets one metric
sign. Dynamic arithmetic is integer-valued after clearing the fixed rational
denominator of each node. Keep the powers of i supplied by FeynArts.
"""
from dataclasses import dataclass
from fractions import Fraction as Q
from functools import lru_cache
from itertools import product
from math import lcm, prod
from pathlib import Path
import json
import sys


def dot(a,b):
    return a[0]*b[0]-a[1]*b[1]-a[2]*b[2]-a[3]*b[3]


BASIS=tuple(tuple(int(i==j) for j in range(4)) for i in range(4))
SIG=(1,-1,-1,-1)


@dataclass
class Node:
    den: int
    phase: int
    slots: frozenset
    code: str
    children: tuple=()


def const(value,phase=0):
    value=Q(value)
    return Node(value.denominator,phase,frozenset(),str(value.numerator))


class Graphs:
    def __init__(self, data, momenta, masses, epsilon, limit=False):
        self.ps=momenta
        self.routes={}
        self.nodes=[None]
        self.factors={}
        self.limit=limit
        self.masses=masses
        self.epsilon=epsilon
        self.rows=data['Nodes']
        for row in self.rows:
            self.nodes.append(self.node(row))
        self.diagrams={}
        for roots in data['Factors']:
            factors=[self.factor(k) for k in roots]
            active=[f for f in factors if f['slots']]
            ext=[i for f in active for i in f['slots'] if i<=8]
            assert sorted(ext)==list(range(1,9))
            phase=sum(f['phase'] for f in factors)
            assert phase%2==1, 'expected an imaginary tree amplitude'
            sign=(-1)**(phase//2)
            weight=sign*prod(f['value'](()) for f in factors if not f['slots'])
            if len(active)==1:
                self.diagrams['contact']=(weight,active[0])
                continue
            middle,= [f for f in active if min(f['slots'])>8]
            currents=[f for f in active if f is not middle]
            assert len(currents)==2 and len(middle['slots'])==4
            left,right=sorted(currents,key=lambda f:min(f['slots']))
            ends=tuple(sorted(i for i in left['slots'] if i>8))+tuple(sorted(i for i in right['slots'] if i>8))
            assert set(ends)==set(middle['slots']) and len(set(ends))==4
            pair=tuple(sorted({(i+1)//2 for i in left['slots'] if i<=8}))
            channel={(1,2):'s',(1,3):'t',(1,4):'u'}[pair]
            assert channel not in self.diagrams
            matrix={}
            for components in product(range(4),repeat=4):
                slots={i:BASIS[j] for i,j in zip(ends,components)}
                z=middle['value'](tuple(slots[i] for i in middle['slots']))
                matrix[components]=prod(SIG[j] for j in components)*z
            self.diagrams[channel]=(weight,left,right,matrix)
        assert set(self.diagrams)=={'contact','s','t','u'}

    def momentum(self,route):
        return tuple(sum(c*p[j] for c,p in zip(route,self.ps)) for j in range(4))

    def node(self,row):
        op,*a=row
        if op in ('q','imag'):
            return const(Q(*a),int(op=='imag'))
        if op=='complex': # Original QG export contained only the exact integer i.
            assert a==[0,1] and all(type(x) is int for x in a)
            return const(1,1)
        if op=='gauge':return const(Q(self.epsilon,2))
        if op in ('m0','m2'):return const(self.masses[op=='m2'])
        if op=='d':
            route,pole,power=a
            if self.limit and pole:return const(0)
            q=self.momentum(route)
            m={0:0,1:self.masses[0],2:self.masses[1]}[pole]
            return const(Q(1,(dot(q,q)-m*m)**power))
        if op=='s':return const(dot(self.momentum(a[0]),self.momentum(a[1])))
        if op=='g':
            i,j=a
            return Node(1,0,frozenset({i,j}),f'dot(v[{i}],v[{j}])')
        if op=='v':
            route,i=a
            return Node(1,0,frozenset({i}),f'dot({self.momentum(route)!r},v[{i}])')
        kids=tuple(a if op!='pow' else a[:1])
        ns=[self.nodes[i] for i in kids]
        slots=frozenset().union(*(n.slots for n in ns))
        if op=='pow':
            n,=ns;k=a[1]
            if self.limit and self.rows[kids[0]-1][0] in ('m0','m2'):
                assert k<0
                return const(0)
            phase=n.phase*k
            if not slots:
                return const((-1)**(phase//2)*Q(int(n.code),n.den)**k,phase%2)
            assert k>0
            return Node(n.den**k,phase%2,slots,f'{(-1)**(phase//2)}*x{kids[0]}**{k}',kids)
        if op=='mul':
            if any(not n.slots and n.code=='0' for n in ns):return const(0)
            phase=sum(n.phase for n in ns)
            code=str((-1)**(phase//2))+'*'+'*'.join(f'x{i}' for i in kids)
            den=prod(n.den for n in ns)
            if not slots:return const(Q((-1)**(phase//2)*prod(int(n.code) for n in ns),den),phase%2)
            return Node(den,phase%2,slots,code,kids)
        if op!='add':raise ValueError(op)
        kids=tuple(i for i in kids if self.nodes[i].slots or self.nodes[i].code!='0')
        if not kids:return const(0)
        ns=[self.nodes[i] for i in kids]
        phases={n.phase for n in ns};assert len(phases)==1
        phase=phases.pop();den=lcm(*(n.den for n in ns))
        if not slots:return const(sum(Q(int(n.code),n.den) for n in ns),phase)
        code='sum(('+','.join(f'{den//n.den}*x{i}' for i,n in zip(kids,ns))+',))'
        return Node(den,phase,slots,code,kids)

    def factor(self,root):
        n=self.nodes[root];needed=set();pending=[root]
        while pending:
            i=pending.pop()
            if i not in needed:
                needed.add(i);pending.extend(self.nodes[i].children)
        # Emit arithmetic solely from validated operations and integer node IDs.
        lines=[f' x{i}={self.nodes[i].code}' for i in sorted(needed)]
        slots=tuple(sorted(n.slots))
        env={'dot':dot}
        exec('def evaluate(v):\n'+'\n'.join(lines)+f'\n return x{root}',env)
        @lru_cache(maxsize=512)
        def value(vectors):
            return Q(env['evaluate'](dict(zip(slots,vectors))),n.den)
        return dict(slots=slots,phase=n.phase,value=value)

    def pieces(self,states):
        ext={i+1:v for i,v in enumerate(x for pair in states for x in pair)}
        def current(f):
            values={}
            for a,b in product(range(4),repeat=2):
                ids=sorted(i for i in f['slots'] if i>8)
                slots=ext|dict(zip(ids,(BASIS[a],BASIS[b])))
                values[a,b]=f['value'](tuple(slots[i] for i in f['slots']))
            return values
        out={}
        for ch,diagram in self.diagrams.items():
            if ch=='contact':
                weight,f=diagram;out[ch]=weight*f['value'](tuple(ext[i] for i in f['slots']))
            else:
                weight,left,right,matrix=diagram
                a,b=current(left),current(right)
                out[ch]=weight*sum(value*a[i,j]*b[k,l] for (i,j,k,l),value in matrix.items() if value)
        return out


def geometry(E,s,c):
    assert E**2==s*s+c*c
    p=((E,0,0,E),(E,0,0,-E),(-E,-s,0,-c),(-E,s,0,c))
    assert not any(map(sum,zip(*p))) and all(dot(x,x)==0 for x in p)
    b=(0,0,E,0)
    options=[]
    for j,k in enumerate(p):
        a=(0,E,0,0) if j<2 else (0,c,0,-s)
        # C = sym(a b), P = sym((a+b)(a-b)) = aa-bb. The generated
        # vertex is pair symmetric, so evaluating an ordered dyad suffices.
        plus=tuple(x+y for x,y in zip(a,b)),tuple(x-y for x,y in zip(a,b))
        opts=((a,b),plus)
        assert all(dot(k,u)==dot(k,v)==dot(u,v)==0 for u,v in opts)
        options.append(opts)
    return p,options


def run(qg_data,gr_data,fixture,out):
    E,s,c,m0,m2,epsilon=fixture
    ps,options=geometry(E,s,c)
    qg=Graphs(qg_data,ps,(m0,m2),epsilon)
    gr=Graphs(gr_data,ps,(m0,m2),epsilon)
    decoupled=Graphs(qg_data,ps,(m0,m2),epsilon,limit=True)
    invariants=[2*dot(ps[0],ps[j]) for j in (1,2,3)]
    assert sum(invariants)==0 and len(set(invariants))==3 and all(invariants)
    assert not {0,m0*m0,m2*m2}.intersection(invariants)
    physical=[];ward=[];controls={}
    def record(row):return {k:str(v) for k,v in row.items()}
    def save():
        out.write_text(json.dumps(dict(fixture=fixture,invariants=invariants,
            physical=physical,ward=ward,omission_controls=controls),indent=2)+'\n')
    for bits in product(range(2),repeat=4):
        states=tuple(options[j][b] for j,b in enumerate(bits))
        q=qg.pieces(states);e=gr.pieces(states);lim=decoupled.pieces(states)
        physical.append(dict(states=bits,QG=record(q),GR=record(e),limit=record(lim),
                             total=str(sum(q.values()))))
        save()
        assert q.keys()==e.keys() and lim==e,('Einstein limit',bits,lim,e)
        assert sum(q.values())==sum(e.values()),('finite-mass physical sum',bits,q,e)
    assert any(Q(row['total']) for row in physical)
    assert any(row['QG']!=row['GR'] for row in physical)
    for leg in range(4):
        for bits in product(range(2),repeat=3):
            keep=iter(bits)
            states=[None if j==leg else options[j][next(keep)] for j in range(4)]
            for direction,xi in enumerate(BASIS):
                # Homogeneous Ward identity: sym(p xi) differs from the full
                # gauge variation only by an overall factor of two.
                states[leg]=(ps[leg],xi)
                pieces=qg.pieces(states);total=sum(pieces.values())
                ward.append(dict(leg=leg+1,pol=bits,direction=direction,
                                 pieces=record(pieces),residual=str(total)))
                if total:
                    save();raise AssertionError(('Ward residual',ward[-1]))
                for omitted in pieces:
                    incomplete=sum(v for ch,v in pieces.items() if ch!=omitted)
                    if incomplete and omitted not in controls:
                        controls[omitted]=dict(leg=leg+1,basis_direction=direction,
                                               other_polarisations=bits,residual=str(incomplete))
    save()
    assert set(controls)=={'contact','s','t','u'}
    assert all(any(w['leg']==i and any(Q(z) for z in w['pieces'].values()) for w in ward)
               for i in range(1,5))
    print('PASS 16 physical amplitudes; 64 per-channel Einstein limits; 128 Ward replacements; all four omission controls',flush=True)


if __name__=='__main__':
    qg,gr,destination,frame=sys.argv[1:]
    dest=Path(destination)
    fixtures=[(5,3,4,7,11,2),(13,12,5,17,31,5)]
    if not dest.is_dir() or any((dest/name).exists() for name in ('ward-results.json','completion.json')):
        raise SystemExit('Supply a fresh existing output directory')
    quadratic=json.loads(Path(qg).read_text())
    einstein=json.loads(Path(gr).read_text())
    assert quadratic['Model']=='QuadraticGravity' and einstein['Model']=='GR'
    run(quadratic,einstein,fixtures[int(frame)],dest/'ward-results.json')
    (dest/'completion.json').write_text(json.dumps(dict(Status='Completed',Passed=1,Failed=0)))
