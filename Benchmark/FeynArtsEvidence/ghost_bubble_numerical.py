#!/usr/bin/env python3
"""Small, off-shell NUMERICAL diagnostic; never performs loop integration.

Reads only the SHA-pinned converted ghost bubble. Pair is interpreted with
Minkowski (+---) and FeynCalc's covariant vector components; Eq32 is evaluated
independently by summing its alpha, beta, lambda indices (FeynGrav.wl:623).
"""
import ast
import hashlib
import itertools
from pathlib import Path
import sys

INPUT = Path(__file__).with_name("ghost-bubble-fc-20261014.wl")
SHA256 = "cde96daf3c35d75e76d2c336187464027d6fb82c952f295070ec63ad779bb525"
ETA = (1, -1, -1, -1)
P = (2, 1, 0, 0)
ELL = (3, 0, 1, 0)
NAMES = {"p": P, "ell": ELL, "D": 4, "I": 1j, "kappa": 1}
NAMES.update({"Lor%d" % i: ("index", i) for i in range(1, 9)})
CALLS = {"Pair", "LorentzIndex", "Momentum", "FeynAmpDenominator", "PropagatorDenominator"}


def vec(x):
    return isinstance(x, tuple) and len(x) == 4 and all(isinstance(v, (int, float)) for v in x)


def plus(a, b):
    if vec(a) and vec(b):
        return tuple(x + y for x, y in zip(a, b))
    return a + b


def neg(a):
    return tuple(-x for x in a) if vec(a) else -a


def metric(a, b):
    return ETA[a] if a == b else 0


def lower(v, i):
    return ETA[i] * v[i]


def dot(a, b):
    return sum(ETA[i] * a[i] * b[i] for i in range(4))


def evaluate(node, indices):
    """Interpret a whitelist of arithmetic AST nodes; never call Python eval."""
    if isinstance(node, ast.Constant) and type(node.value) is int:
        return node.value
    if isinstance(node, ast.Name) and node.id in NAMES:
        v = NAMES[node.id]
        return indices[v[1]] if isinstance(v, tuple) and len(v) == 2 and v[0] == "index" else v
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.UAdd, ast.USub)):
        x = evaluate(node.operand, indices)
        return x if isinstance(node.op, ast.UAdd) else neg(x)
    if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Sub, ast.Mult, ast.Div)):
        a, b = evaluate(node.left, indices), evaluate(node.right, indices)
        if isinstance(node.op, ast.Add):
            return plus(a, b)
        if isinstance(node.op, ast.Sub):
            return plus(a, neg(b))
        if isinstance(node.op, ast.Mult):
            return a * b
        return a / b
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id in CALLS and not node.keywords:
        kind = node.func.id
        if kind == "FeynAmpDenominator":
            if len(node.args) != 2:
                raise ValueError("expected exactly two bubble propagators")
            a, b = (evaluate(arg, indices) for arg in node.args)
            return a * b
        arity = {"Pair": 2, "LorentzIndex": 2, "Momentum": 2, "PropagatorDenominator": 2}[kind]
        if len(node.args) != arity:
            raise ValueError("unexpected call arity: " + kind)
        a, b = (evaluate(arg, indices) for arg in node.args)
        if kind == "LorentzIndex":
            if b != 4 or not isinstance(a, int) or a not in range(4):
                raise ValueError("bad Lorentz index or dimension")
            return ("lorentz", a)
        if kind == "Momentum":
            if b != 4 or not vec(a):
                raise ValueError("bad momentum or dimension")
            return ("momentum", a)
        if kind == "PropagatorDenominator":
            if b != 0 or a[0] != "momentum":
                raise ValueError("not a massless vector propagator")
            q2 = dot(a[1], a[1])
            if q2 == 0:
                raise ValueError("singular numerical denominator")
            return 1 / q2
        if a[0] == b[0] == "lorentz":
            return metric(a[1], b[1])
        if a[0] == "lorentz" and b[0] == "momentum":
            return lower(b[1], a[1])
        if a[0] == "momentum" and b[0] == "lorentz":
            return lower(a[1], b[1])
        if a[0] == b[0] == "momentum":
            return dot(a[1], b[1])
        raise ValueError("unknown Pair argument types")
    raise ValueError("unsafe or unsupported InputForm AST node: " + ast.dump(node)[:150])


def source_vertex(rho, sigma, k, mu, p1, nu, p2):
    """Literal three-sum Eq32: ghost first (mu,p1), antighost second (nu,p2)."""
    result = 0
    for lam, alpha, beta in itertools.product(range(4), repeat=3):
        first = (metric(nu, alpha) * metric(lam, beta)
                 + metric(nu, beta) * metric(lam, alpha)
                 - metric(nu, lam) * metric(alpha, beta))
        second = (lower(p1, alpha) * (metric(beta, rho) * metric(mu, sigma)
                                      + metric(beta, sigma) * metric(mu, rho)) / 2
                  + lower(p1, beta) * (metric(alpha, rho) * metric(mu, sigma)
                                      + metric(alpha, sigma) * metric(mu, rho)) / 2
                  + lower(k, mu) * (metric(alpha, rho) * metric(beta, sigma)
                                    + metric(beta, rho) * metric(alpha, sigma)) / 2)
        result += lower(p2, lam) * first * second
    return 0.5j * result  # kappa = 1


def flattened_product(node):
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Mult):
        return flattened_product(node.left) + flattened_product(node.right)
    return [node]


def close(a, b):
    return abs(a - b) < 1e-10


def main():
    raw = INPUT.read_bytes()
    digest = hashlib.sha256(raw).hexdigest()
    if len(raw) != 3350 or digest != SHA256:
        raise ValueError("saved converted integrand size/SHA256 mismatch")
    text = raw.decode("utf-8")
    # Only delimiters and the one escaped InputForm identifier are translated.
    translated = text.replace(r"\[Kappa]", "kappa").replace("[", "(").replace("]", ")").replace("\n", " ")
    root = ast.parse(translated, mode="eval").body
    factors = flattened_product(root)
    if len(factors) != 5:
        raise ValueError("expected denominator, two propagator metrics, two vertices")
    den, metric1, metric2, vfirst, vsecond = factors
    if not isinstance(vfirst, ast.BinOp) or not isinstance(vsecond, ast.BinOp):
        raise ValueError("missing two vertex polynomials")
    # Directly validate the full AST once, and do not accept unknown Python syntax.
    testidx = {i: 0 for i in range(1, 9)}
    evaluate(root, testidx)
    print("NUMERICAL ONLY: off-shell, unintegrated Minkowski (+---), D=4, kappa=1")
    print("NUMERICAL INPUT bytes=3350 sha256=" + digest)
    print("NUMERICAL p=(2,1,0,0) ell=(3,0,1,0); p^2=%s ell^2=%s (ell-p)^2=%s" %
          (dot(P, P), dot(ELL, ELL), dot(plus(ELL, neg(P)), plus(ELL, neg(P)))))
    # All-incoming momentum routing: first (p, -ell, ell-p), second (-p, ell, p-ell).
    # Also test orientation/index alternatives; matching a total alone cannot resolve routing.
    configs = []
    for name, k, ghost, anti in (("first", P, neg(ELL), plus(ELL, neg(P))),
                                  ("second", neg(P), ELL, plus(P, neg(ELL)))):
        for mu, nu in ((5, 8), (8, 5)) if name == "first" else ((6, 7), (7, 6)):
            for flip in (False, True):
                configs.append((name, mu, nu, k, anti if flip else ghost,
                                ghost if flip else anti, flip))
    # Source 'flip' interchanges p1/p2 but retains the all-incoming set: diagnostic
    # candidate only, never silently accepted as the documented field assignment.
    resolved = {}
    for name, factor, grav, legs in (("first", vfirst, (1, 2), (5, 8)),
                                    ("second", vsecond, (3, 4), (6, 7))):
        # Select nonzero tensor components rather than cherry-picking zero probes.
        probes = []
        for components in itertools.product(range(4), repeat=4):
            idx = {j: 0 for j in range(1, 9)}
            idx.update(dict(zip(grav + legs, components)))
            if abs(evaluate(factor, idx)) > 1e-10:
                probes.append(idx)
            if len(probes) == 12:
                break
        if not probes:
            raise ValueError("vertex unexpectedly zero on all tested components")
        matches = []
        for config in configs:
            side, mu, nu, k, ghost, anti, flip = config
            if side != name:
                continue
            pairs = [(evaluate(factor, idx),
                      source_vertex(idx[grav[0]], idx[grav[1]], k, idx[mu], ghost, idx[nu], anti))
                     for idx in probes]
            agreements = sum(close(x, y) for x, y in pairs)
            print("NUMERICAL VERTEX %s ghost=Lor%d antighost=Lor%d swapped_momenta=%s agree=%d/%d first_generated=%s first_Eq32=%s" %
                  (name, mu, nu, flip, agreements, len(probes), pairs[0][0], pairs[0][1]))
            if agreements == len(probes):
                matches.append(config)
        resolved[name] = matches[0] if len(matches) == 1 and not matches[0][-1] else None
    if any(config is None for config in resolved.values()):
        print("NUMERICAL ROUTING unresolved: totals below use declared nominal all-incoming field assignment only")
    # Nominal assignments are explicitly labelled; a total match alone cannot
    # promote unresolved per-vertex comparisons to PASS.
    left = resolved["first"] or next(c for c in configs if c[0] == "first" and c[1] == 5 and not c[-1])
    right = resolved["second"] or next(c for c in configs if c[0] == "second" and c[1] == 7 and not c[-1])
    print("NUMERICAL NOMINAL routing first (rho,sigma)=(Lor1,Lor2), k=p, ghost=(Lor%d,-ell), antighost=(Lor%d,ell-p); second (rho,sigma)=(Lor3,Lor4), k=-p, ghost=(Lor%d,ell), antighost=(Lor%d,p-ell)" %
          (left[1], left[2], right[1], right[2]))
    nonzero = total_agrees = 0
    for ext in itertools.product(range(4), repeat=4):
        generated = expected = 0j
        for internals in itertools.product(range(4), repeat=4):
            idx = dict(enumerate(ext + internals, start=1))
            generated += evaluate(root, idx)
            # Two source propagators I*eta/q^2, closed ghost loop -1,
            # symmetry factor 1: (-1) * I^2 = +1 multiplies both vertices.
            m = metric(idx[5], idx[6]) * metric(idx[7], idx[8])
            if not m:
                continue
            factor = m / (dot(ELL, ELL) * dot(plus(ELL, neg(P)), plus(ELL, neg(P))))
            a = source_vertex(idx[1], idx[2], left[3], idx[left[1]], left[4], idx[left[2]], left[5])
            b = source_vertex(idx[3], idx[4], right[3], idx[right[1]], right[4], idx[right[2]], right[5])
            expected += factor * a * b
        if abs(generated) > 1e-10:
            nonzero += 1
            equal = close(generated, expected)
            total_agrees += equal
            print("NUMERICAL EXTERNAL Lor1..Lor4=%s generated=%s nominal_Eq32_times_two_I_propagators_times_loop_minus=%s match=%s" %
                  (ext, generated, expected, equal))
            if nonzero == 3:
                break
    if not nonzero or any(config is None for config in resolved.values()) or total_agrees != nonzero:
        print("NUMERICAL VERDICT INCONCLUSIVE: index raising/routing or source-to-generated sign unresolved; no loop integration, tadpole removal, on-shell limit or measure")
        return 2
    print("NUMERICAL VERDICT PASS: %d nonzero assignments; no loop integration, tadpole removal, on-shell limit or measure" % nonzero)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, SyntaxError, UnicodeError, OSError, ZeroDivisionError) as exc:
        print("NUMERICAL VERDICT INCONCLUSIVE: " + str(exc))
        sys.exit(2)
