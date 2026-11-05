"""Exact D=4 five-graviton trees from the labelled native FeynArts graph factors.

All momenta are incoming. External states are symmetric dyads. Results have
i*kappa**(n-2) removed. The native probe and source-sewing check are independent.
"""
import argparse
import json
import time
from collections import Counter
from fractions import Fraction as Q
from functools import lru_cache
from itertools import combinations, product
from math import prod
from pathlib import Path
import sys

VERIFY = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(VERIFY / 'QuadraticGravity/Ward'))
from ward import BASIS, SIG, Graphs, const, dot

POINT = ((6,0,0,6),(6,0,0,-6),(-3,-3,0,0),(-4,0,-4,0),(-5,3,4,0))
AXES = range(4)


def read(path):
    return json.loads(path.read_text())


def cuts(graph):
    """The two external legs meeting at each terminal cubic vertex, zero-based."""
    leaves = {}
    for kind, degree, first, valence, vertex in graph['Edges']:
        if kind != 'Internal':
            assert degree == 1
            leaves.setdefault((valence, vertex), []).append(first-1)
    return tuple(sorted(tuple(sorted(ids)) for (n,_),ids in leaves.items()
                        if n == 3 and len(ids) == 2))


def relabel(pairs):
    rest = sorted(set(range(5)).difference(*pairs))
    if len(pairs) == 1:
        return (*pairs[0], *rest)
    middle, = rest
    return (*pairs[0], middle, *pairs[1])


def tensor(factor):
    """Tabulate only the open internal slots. Reuse unaffected currents for Ward tests."""
    slots = factor['slots']
    external = [s for s in slots if s <= 10]
    internal = tuple(s for s in slots if s > 10)

    @lru_cache(maxsize=64)
    def values(probes):
        vectors = dict(zip(external, probes))
        table = {}
        for ij in product(AXES, repeat=len(internal)):
            vectors.update((s,BASIS[i]) for s,i in zip(internal,ij))
            val = factor['value'](tuple(vectors[s] for s in slots))
            if val:
                # Every summed Lorentz index belongs to precisely one tensor line.
                table[ij] = val if external else val * prod(SIG[i] for i in ij)
        return internal, table
    return lambda states: values(tuple(states[(s-1)//2][(s-1)%2] for s in external))


def contract(left, right):
    ls, lv = left
    rs, rv = right
    shared = sorted(set(ls) & set(rs))
    li, ri = [ls.index(s) for s in shared], [rs.index(s) for s in shared]
    la, ra = [i for i,s in enumerate(ls) if s not in shared], [i for i,s in enumerate(rs) if s not in shared]
    by_key = {}
    for j,val in rv.items():
        by_key.setdefault(tuple(j[i] for i in ri),[]).append((tuple(j[i] for i in ra),val))
    out = Counter()
    for j,val in lv.items():
        key = tuple(j[i] for i in la)
        for tail,b in by_key.get(tuple(j[i] for i in li),()):
            out[key+tail] += val*b
    return tuple(ls[i] for i in la)+tuple(rs[i] for i in ra), out


class NativeGraph(Graphs):
    """Reuse the exact native-factor decoder of the four-point Ward check."""
    def __init__(self, graph, momenta, expected_cuts, pole_residue=None):
        self.ps = momenta
        self.limit = False
        self.rows = graph['Nodes']
        self.nodes = [None]
        self.masses = (0,0)
        self.poles = []
        self.pole_residue = pole_residue
        for row in self.rows:
            self.nodes.append(self.node(row))
        assert sorted(self.poles) == sorted(expected_cuts)
        factors = [self.factor(r) for r in graph['Factors']]
        active = [f for f in factors if f['slots']]
        counts = Counter(s for f in active for s in f['slots'])
        assert all(counts[i] == 1 for i in range(1,11))
        assert all(n == 2 for i,n in counts.items() if i > 10)
        lines = [s for f in active if min(f['slots']) > 10 for s in f['slots']]
        assert sorted(lines) == sorted(i for i in counts if i > 10)
        phase = sum(f['phase'] for f in factors)
        assert phase % 2 == 1
        self.weight = (-1)**(phase//2) * prod(f['value'](()) for f in factors if not f['slots'])
        self.tensors = [tensor(f) for f in active]
        self.nodes.clear()  # Generated factor functions retain only their dependencies.

    def node(self,row):
        if row[0] == 'd':
            route, mass, power = row[1:]
            assert mass == 0 and power == 1
            ones = {i for i,v in enumerate(route) if v != min(route)}
            assert max(route)-min(route) == 1
            pair = tuple(sorted(ones if len(ones)==2 else set(range(5))-ones))
            assert len(pair)==2
            self.poles.append(pair)
            if self.pole_residue is not None and pair == self.pole_residue[0]:
                assert dot(self.momentum(route),self.momentum(route)) == 0
                return const(Q(1,self.pole_residue[1]))
        return super().node(row)

    def value(self,states):
        parts = [t(states) for t in self.tensors]
        while len(parts)>1:
            # Sew an end or a current first, keeping at most four free indices.
            _,i,j = min((len(set(a[0]) ^ set(b[0])),i,j)
                        for i,a in enumerate(parts) for j,b in enumerate(parts[:i]) if set(a[0]) & set(b[0]))
            combined = contract(parts[i],parts[j])
            parts.pop(i); parts.pop(j); parts.append(combined)
        slots, values = parts[0]
        assert not slots
        return self.weight * values.get((),0)


class Contact:
    def __init__(self, path):
        self.rows = [tuple(map(int,l.split(','))) for l in path.read_text().splitlines()]
        assert len(self.rows)==76230 and len({r[1:] for r in self.rows})==len(self.rows)
        assert all(len(row)==7 and row[0] for row in self.rows)

    def value(self,ps,states):
        vectors = tuple(v for v,w in states)+tuple(w for v,w in states)+tuple(ps)
        dots = [dot(a,b) for a in vectors for b in vectors]
        return Q(sum(n*dots[a]*dots[b]*dots[c]*dots[d]*dots[e]*dots[f]
                     for n,a,b,c,d,e,f in self.rows),16)


class Tree:
    def __init__(self,path,point):
        self.point = point
        assert not any(map(sum,zip(*point))) and all(dot(p,p)==0 for p in point)
        self.contact = Contact(path/'contact.csv')
        self.graphs = {}
        templates = {i:read(path/f'graph-{i}.json') for i in (2,14)}
        census = read(path/'graphs.json')
        all_pairs = set(combinations(range(5),2))
        keys = [cuts(g) for g in census]
        expected = {()} | {(a,) for a in all_pairs} | {tuple(sorted((a,b))) for a,b in combinations(all_pairs,2) if not set(a)&set(b)}
        assert len(census)==len(expected)==26 and set(keys)==expected
        for graph,key in zip(census,keys):
            assert graph['WeightDenominator']==1
            if not key:
                assert graph['Position']==1
                continue
            perm = relabel(key)
            template = 2 if len(key)==1 else 14
            expected_cuts = tuple(tuple(sorted(perm.index(i) for i in pair)) for pair in key)
            native = NativeGraph(templates[template],tuple(point[i] for i in perm),expected_cuts)
            self.graphs[graph['Position']] = (key,perm,native)

    def pieces(self,states):
        return {1:self.contact.value(self.point,states)} | {
            i:g.value(tuple(states[j] for j in perm)) for i,(_,perm,g) in self.graphs.items()}


def physical_states(ps):
    """A real transverse traceless tensor per leg, without imposing a Ward identity."""
    frames = []
    for j,k in enumerate(ps):
        x,y,z = k[1:]
        a = (-y,x,0) if x or y else (1,0,0)
        b = ((y*a[2]-z*a[1]),(z*a[0]-x*a[2]),(x*a[1]-y*a[0]))
        # Scale both real transverse directions to the same length, with integers.
        a = tuple(abs(k[0])*i for i in a)
        r = j+1
        u = (0,*(i+r*l for i,l in zip(a,b)))
        v = (0,*(r*i-l for i,l in zip(a,b)))
        from math import gcd
        u = tuple(i//gcd(*u) for i in u); v = tuple(i//gcd(*v) for i in v)
        assert dot(k,u)==dot(k,v)==dot(u,v)==0 and dot(u,u)<0 and dot(v,v)<0
        frames.append((u,v))
    return tuple(frames)


def encode(obj):
    return json.loads(json.dumps(obj,default=str))


def run_physical(data, mode):
    start = time.monotonic()
    tree = Tree(data,POINT)
    states = physical_states(POINT)
    pieces = tree.pieces(states)
    total = sum(pieces.values())
    assert total and pieces[1]
    result = {'point':POINT,'states':states,'diagrams':pieces,'total':total,
              'seconds_through_first_complete_amplitude':time.monotonic()-start}
    if mode=='amplitude':
        return result
    if mode=='physical':
        from SourceTree import Source
        source=Source()
        independent=source.exchanges(POINT,states,tree.graphs)
        assert all(independent[i]==pieces[i] for i in tree.graphs)
        # Directly generated native topologies with different external labels.
        direct={}
        for i in (3,15):
            key,_,_=tree.graphs[i]
            graph=NativeGraph(read(data/f'graph-{i}.json'),POINT,key)
            direct[i]=graph.value(states)
            assert direct[i]==pieces[i]
        result.update(independent_exchanges=independent,direct_native=direct)
        # Generic probes retain the two previously measured independent normalisations.
        a,b=(1,2,1,3),(2,-1,3,1)
        gen=tree.pieces(tuple((v,v) for v in (a,b,a,b,a)))
        assert gen[2]==-3203257 and gen[14]==Q(-65089547,32)
        result['generic_probes']=gen
    else:
        wards=[]
        for leg in range(5):
            for direction in BASIS:
                external=list(states);external[leg]=(POINT[leg],direction)
                values=tree.pieces(external)
                residual=sum(values.values())
                assert residual==0,(leg,direction,residual)
                wards.append({'leg':leg+1,'direction':direction,'sum':residual,'without_contact':-values[1]})
        assert all(any(w['without_contact'] for w in wards if w['leg']==i) for i in range(1,6))
        result['wards']=wards
    result['seconds']=time.monotonic()-start
    return result


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data',type=Path)
    parser.add_argument('suite',choices=['amplitude','physical','ward','soft'])
    parser.add_argument('output',type=Path)
    args=parser.parse_args()
    if args.suite=='soft':
        from SoftLimit import run_soft
        result=run_soft(args.data)
    else:
        result=run_physical(args.data,args.suite)
    with args.output.open('x') as output:
        json.dump(encode(result),output,indent=2)
    print('PASS',args.suite)
