"""Labelled weak-field expansion of sqrt(-g), Ricci, R and both curvature squares.

Each symmetric graviton is h_j = e_j e_j exp(i p_j.x). A term carries an
exact weight, a product of formal scalar products and, for a tensor, ordered
uncontracted vector slots. Disjoint label sets implement t_j^2 = 0. No
component basis, field equation, dimension or gauge condition is used.
"""
from collections import defaultdict
from fractions import Fraction as F
from functools import lru_cache
from itertools import permutations


def active(mask):
    return [i for i in range(mask.bit_length()) if mask & (1 << i)]


def splits(mask):
    """All ordered pairs of disjoint subsets partitioning mask."""
    sub = mask
    while True:
        yield sub, mask ^ sub
        if not sub:
            break
        sub = (sub - 1) & mask


def dot(a, b):
    return tuple(sorted((a, b)))


def key(*pairs):
    return tuple(sorted(pairs))


def tidy(poly):
    return {m: c for m, c in poly.items() if c}


class Expansion:
    def __init__(self, n):
        self.n = n
        self.full = (1 << n) - 1
        # Integers 0..n-1 denote e; n..2n-1 denote p.
        self.mom = lambda j: n + j

    @lru_cache(None)
    def inverse(self, a, b, mask):
        """Coefficient of a.g^-1.b from the Neumann series."""
        out = []
        for order in permutations(active(mask)):
            chain = (a, *order, b)
            out.append(((-1)**len(order), tuple(map(dot, chain[:-1], chain[1:]))))
        return out

    @lru_cache(None)
    def volume(self, mask):
        """exp(Tr log(1+h)/2): each permutation cycle gives a trace."""
        ids = active(mask)
        for_order = []
        for perm in permutations(ids):
            cycles, seen = 0, set()
            successor = dict(zip(ids, perm))
            for i in ids:
                if i not in seen:
                    cycles += 1
                    while i not in seen:
                        seen.add(i)
                        i = successor[i]
            weight = F((-1)**(len(ids)-cycles), 2**cycles)
            for_order.append((weight, tuple(map(dot, ids, perm))))
        return for_order

    @lru_cache(None)
    def gamma(self, mask):
        """Rows of Gamma^a_bc / i, with the derivative on one labelled field."""
        out = []
        for j in active(mask):
            p = self.mom(j)
            for sign, a, b, c in ((1,j,p,j),(1,j,j,p),(-1,p,j,j)):
                for chain in permutations(active(mask ^ (1 << j))):
                    path = (*chain, a)
                    factors = tuple(map(dot, path[:-1], path[1:]))
                    out.append((F(sign*(-1)**len(chain),2), factors, path[0], b, c))
        return out

    @lru_cache(None)
    def ricci(self, mask):
        terms = defaultdict(int)
        def add(u,v,coef,factors):
            terms[(u,v,key(*factors))] += coef
        # d_a Gamma^a_uv - d_v Gamma^a_au. Each derivative on the
        # whole connection inserts i times its total incoming momentum.
        for w, f, a,u,v in self.gamma(mask):
            for j in active(mask):
                p = self.mom(j)
                add(u,v,-w,(*f,dot(p,a)))
                add(v,p,w,(*f,dot(a,u)))
        for left,right in splits(mask):
            if not left or not right:
                continue
            for w,f,a,b,c in self.gamma(left):
                for z,h,d,u,v in self.gamma(right):
                    # Gamma^a_ab Gamma^b_uv - Gamma^a_ub Gamma^b_av,
                    # with the minus from the two Fourier derivatives.
                    add(u,v,-w*z,(*f,*h,dot(a,b),dot(c,d)))
                    add(b,v,w*z,(*f,*h,dot(a,u),dot(c,d)))
        return [(w,f,u,v) for (u,v,f),w in terms.items() if w]

    @lru_cache(None)
    def scalar(self, mask):
        terms = defaultdict(int)
        for metric,curv in splits(mask):
            for w,f,u,v in self.ricci(curv):
                for z,h in self.inverse(u,v,metric):
                    terms[key(*f,*h)] += w*z
        return tidy(terms)

    def sectors(self):
        """Return sqrt(-g) times R, R^2 and Ricci^2 respectively."""
        sectors = [defaultdict(int) for _ in range(3)]
        for vec, rest in splits(self.full):
            vol = self.volume(vec)
            def add(i,c,f):
                for z,h in vol:
                    sectors[i][key(*h,*f)] += z*c
            for f,w in self.scalar(rest).items():
                add(0,w,f)
            for l,r in splits(rest):
                if not l or not r:
                    continue
                for f,w in self.scalar(l).items():
                    for h,z in self.scalar(r).items():
                        add(1,w*z,(*f,*h))
            for l,rem in splits(rest):
                for r,metrics in splits(rem):
                    if not l or not r:
                        continue
                    for w,f,u,v in self.ricci(l):
                        for z,h,a,b in self.ricci(r):
                            for m1,m2 in splits(metrics):
                                for w1,f1 in self.inverse(u,a,m1):
                                    for w2,f2 in self.inverse(v,b,m2):
                                        add(2,w*z*w1*w2,(*f,*h,*f1,*f2))
        return list(map(tidy,sectors))
