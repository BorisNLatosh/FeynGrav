"""Regenerate the GR quintic FORM-independent FeynArts template from a stored rule.

Usage: python3 GenerateSeed.py PATH_TO_GravitonVertex_3 FRESH_OUTPUT_FILE
"""
from collections import Counter
from itertools import permutations
from pathlib import Path
import re
import sys

integration = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(integration))
sys.path.insert(0, str(integration / 'Verification/QuadraticGravity/Interactions'))
from BuildQuadraticGravity import factor_terms, native_atom
from check_vertices import PAIR, number


def read_quintic(path):
    pieces = re.split(r'\s+([+-])\s+', path.read_text().strip())
    polynomial = {}
    for sign, term in zip(['+'] + pieces[1::2], pieces[::2]):
        pairs = []

        def read_pair(match):
            ends = []
            for head, role, digits in (match.groups()[:3], match.groups()[3:6]):
                leg = int(digits)
                if leg not in range(1, 6) or (head == 'Momentum') != (role == 'p'):
                    raise ValueError(f'Unexpected tensor slot: {match.group()}')
                ends.append('mnp'.index(role) * 5 + leg - 1)
            pairs.extend([tuple(sorted(ends))] * int(match[7] or 1))
            return '1'

        coeff = ''.join(PAIR.sub(read_pair, term).split())
        coupling = r'\[Kappa]^3'
        if coeff.count(coupling) != 1 or coeff.count('I') != 1:
            raise ValueError(f'Unexpected coupling or phase: {coeff}')
        weight = number(coeff.replace(coupling, '1').replace('I', '1'))
        slots = [end for pair in pairs for end in pair]
        if sorted(i for i in slots if i < 10) != list(range(10)) or sum(i >= 10 for i in slots) != 2:
            raise ValueError('Expected ten free slots and two momenta')
        monomial = tuple(sorted(pairs))
        if monomial in polynomial or not weight:
            raise ValueError('Expected distinct nonzero source monomials')
        polynomial[monomial] = weight if sign == '+' else -weight
    return polynomial


def relabel(monomial, mapping):
    return tuple(sorted(tuple(sorted((mapping[a], mapping[b]))) for a, b in monomial))


def symmetric_seed(poly):
    for leg in range(5):
        mapping = list(range(15))
        mapping[leg], mapping[leg + 5] = mapping[leg + 5], mapping[leg]
        if any(poly.get(relabel(mon, mapping)) != weight for mon, weight in poly.items()):
            raise ValueError(f'Asymmetric index pair on leg {leg + 1}')

    maps = [tuple(5 * (i // 5) + perm[i % 5] for i in range(15)) for perm in permutations(range(5))]
    remaining = set(poly)
    seed = {}
    for mon in sorted(poly):
        if mon not in remaining:
            continue
        orbit = {relabel(mon, mapping) for mapping in maps}
        if not orbit.issubset(remaining) or any(poly[m] != poly[mon] for m in orbit):
            raise ValueError('Unequal coefficients on a whole-leg permutation orbit')
        remaining.difference_update(orbit)
        # The native sum runs over all 120 permutations, including stabilisers.
        seed[mon] = poly[mon] * len(orbit) / len(maps)

    reconstructed = Counter()
    for mapping in maps:
        for mon, weight in seed.items():
            reconstructed[relabel(mon, mapping)] += weight
    if reconstructed != poly:
        raise ValueError('Permutation sum failed exact reconstruction')
    return seed


def native_seed(seed):
    pairs = sorted({p for mon in seed for p in mon})
    names = {p: f'z{i}' for i, p in enumerate(pairs)}
    bindings = ','.join(names[p] + '=' + native_atom(p, 5) for p in pairs)
    polynomial = factor_terms([(w, tuple(names[p] for p in m)) for m, w in seed.items()])
    patterns = ','.join(f'{{p{i}_,a{i}_,b{i}_}}' for i in range(1, 6))
    private = 'FeynGravGR5`Private`'
    return (f'Attributes[{private}legs]={{Orderless}};\n'
            f'{private}seed[{patterns}]:=With[{{{bindings}}},\n{polynomial}];\n'
            f'{private}tensor[{private}legs[ls__List]] /; !FreeQ[{{ls}},_FourMomentum] := '
            f'Total[({private}seed@@# &)/@Permutations[{{ls}}]];\n')


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    terms = read_quintic(Path(sys.argv[1]))
    seed = symmetric_seed(terms)
    with Path(sys.argv[2]).open('x') as output:
        output.write(native_seed(seed))
    print(f'{len(terms)} exact monomials, {len(seed)} permutation representatives')
