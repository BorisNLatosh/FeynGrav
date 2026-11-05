"""Independent Cartesian sewing of the checked-in cubic and quartic GR rules."""
import sys
from fractions import Fraction as Q
from functools import lru_cache
from itertools import product, combinations
from math import lcm,prod
from pathlib import Path

VERIFY=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(VERIFY/'QuadraticGravity/Interactions'))
from check_vertices import read_rules
sys.path.insert(0,str(VERIFY/'QuadraticGravity/Ward'))
from ward import BASIS,SIG,dot

SLOTS=tuple(product(range(4),repeat=2))
def add(a,b):return tuple(i+j for i,j in zip(a,b))
def neg(a):return tuple(-i for i in a)
def unit(ij):return tuple(BASIS[i] for i in ij)


@lru_cache(None)
def propagator(x,y):
    i,j=x; m,n=y
    metric=lambda a,b:SIG[a]*int(a==b)
    return Q(prod(SIG[z] for z in (*x,*y)),2)*(metric(i,m)*metric(j,n)+metric(i,n)*metric(j,m)-metric(i,j)*metric(m,n))


class Source:
    def __init__(self):
        self.rules={}
        for n in (3,4):
            sectors,_=read_rules(VERIFY.parents[1]/'Libs'/f'GravitonVertex_{n-2}',n)
            assert not sectors[1] and not sectors[2]
            d=lcm(*(w.denominator for w in sectors[0].values()))
            self.rules[n]=(d,[(int(w*d),mon) for mon,w in sectors[0].items()])

    def vertex(self,ps,states):
        d,terms=self.rules[len(ps)]
        vs=tuple(v for v,w in states)+tuple(w for v,w in states)+tuple(ps)
        pairs=[[dot(v,w) for w in vs] for v in vs]
        return Q(sum(c*prod(pairs[i][j] for i,j in mon) for c,mon in terms),d)

    def current(self,i,j,ps,states):
        q=add(ps[i],ps[j])
        end={x:self.vertex((ps[i],ps[j],neg(q)),(states[i],states[j],unit(x))) for x in SLOTS}
        return q,{y:sum(val*propagator(x,y) for x,val in end.items())/dot(q,q) for y in SLOTS}

    def exchanges(self,ps,states,graphs):
        currents={p:self.current(*p,ps,states) for p in combinations(range(5),2)}
        values={}
        for idx,(key,perm,_) in graphs.items():
            q,left=currents[key[0]]
            if len(key)==1:
                a,b,c=perm[2:]
                values[idx]=-sum(v*self.vertex((q,ps[a],ps[b],ps[c]),(unit(x),states[a],states[b],states[c])) for x,v in left.items() if v)
            else:
                mid=perm[2];r,right=currents[key[1]]
                values[idx]=sum(v*w*self.vertex((q,ps[mid],r),(unit(x),states[mid],unit(y)))
                                for x,v in left.items() for y,w in right.items() if v and w)
        return values

    def four_point(self,ps,states):
        result=self.vertex(ps,states)
        for j in (1,2,3):
            q,left=self.current(0,j,ps,states)
            a,b=sorted(set(range(4))-{0,j})
            result-=sum(v*self.vertex((q,ps[a],ps[b]),(unit(x),states[a],states[b])) for x,v in left.items() if v)
        return result
