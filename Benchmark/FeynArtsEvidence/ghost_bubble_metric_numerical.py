#!/usr/bin/env python3
"""NUMERICAL-only follow-up: metric-aware Eq32 and whole-leg ghost routing.

Requires only the standard library and the original restricted-AST diagnostic
in the same directory. Never evaluates arbitrary Python from the input file.
"""
import ast
import itertools
import sys

import ghost_bubble_numerical as base


# FVD[q,mu] = q^mu and MTD[mu,nu] = g^{mu nu}: local FeynCalc
# Documentation/Markdown/{FVD,MTD}.md, examples 15 and 30-32.
# Dummy contractions require an inverse metric for EACH repeated index.
def fvd(q, i):
    return q[i]


def eq32(rho, sigma, k, mu, p1, nu, p2):
    eta = base.ETA
    g = base.metric
    total = 0
    for lam, alpha, beta in itertools.product(range(4), repeat=3):
        first = (g(nu, alpha) * g(lam, beta)
                 + g(nu, beta) * g(lam, alpha)
                 - g(nu, lam) * g(alpha, beta))
        second = (fvd(p1, alpha) * (g(beta, rho) * g(mu, sigma)
                                    + g(beta, sigma) * g(mu, rho)) / 2
                  + fvd(p1, beta) * (g(alpha, rho) * g(mu, sigma)
                                     + g(alpha, sigma) * g(mu, rho)) / 2
                  + fvd(k, mu) * (g(alpha, rho) * g(beta, sigma)
                                  + g(beta, rho) * g(alpha, sigma)) / 2)
        total += eta[lam] * eta[alpha] * eta[beta] * fvd(p2, lam) * first * second
    return 0.5j * total  # Eq32, kappa = 1; no fitted factor or sign


def main():
    raw = base.INPUT.read_bytes()
    if len(raw) != 3350 or base.hashlib.sha256(raw).hexdigest() != base.SHA256:
        raise ValueError("pinned converted-integrand checksum/size mismatch")
    translated = raw.decode("utf-8").replace(r"\[Kappa]", "kappa").replace("[", "(").replace("]", ")").replace("\n", " ")
    root = ast.parse(translated, mode="eval").body
    factors = base.flattened_product(root)
    if len(factors) != 5:
        raise ValueError("unexpected bubble factorization")
    _, _, _, v1, v2 = factors
    # The original safe AST interpreter is reused with explicit contravariant
    # FVD components. This is a local module variable only, never file edits.
    base.lower = fvd
    base.evaluate(root, {i: 0 for i in range(1, 9)})
    p, ell = base.P, base.ELL
    first = (p, base.neg(ell), base.plus(ell, base.neg(p)))
    second = (base.neg(p), base.plus(p, base.neg(ell)), ell)
    print("NUMERICAL ONLY: off-shell unintegrated Minkowski (+---), D=4, kappa=1; FVD[q,mu]=q^mu")
    print("NUMERICAL INPUT bytes=%d sha256=%s" % (len(raw), base.SHA256))
    print("NUMERICAL p=%s ell=%s; p^2=%s ell^2=%s (ell-p)^2=%s" %
          (p, ell, base.dot(p, p), base.dot(ell, ell),
           base.dot(first[2], first[2])))
    print("NUMERICAL CONVENTION local FeynCalc Documentation/Markdown/{FVD,MTD}.md: FVD=q^mu, MTD=g^{mu nu}, both map to Pair")
    print("NUMERICAL Eq32 dummy contraction eta[lambda]*eta[alpha]*eta[beta]; NO fitted signs")
    print("NUMERICAL first: (rho,sigma)=(Lor1,Lor2), k=p; ghost=(Lor5,-ell); antighost=(Lor8,ell-p)")
    print("NUMERICAL second: (rho,sigma)=(Lor3,Lor4), k=-p; ghost=(Lor7,p-ell); antighost=(Lor6,ell)")
    matches = []
    for factor, grav, mu, nu, momenta, label in (
            (v1, (1, 2), 5, 8, first, "first"),
            (v2, (3, 4), 7, 6, second, "second")):
        same = nonzero = 0
        mismatch = None
        for r, s, u, v in itertools.product(range(4), repeat=4):
            idx = {i: 0 for i in range(1, 9)}
            idx.update({grav[0]: r, grav[1]: s, mu: u, nu: v})
            actual = base.evaluate(factor, idx)
            independent = eq32(r, s, momenta[0], u, momenta[1], v, momenta[2])
            nonzero += abs(actual) > 1e-10
            same += base.close(actual, independent)
            if mismatch is None and not base.close(actual, independent):
                mismatch = (r, s, u, v, actual, independent)
        print("NUMERICAL VERTEX %s components=%d/256 nonzero_generated=%d first_mismatch=%s" %
              (label, same, nonzero, mismatch))
        matches.append(same == 256 and nonzero > 0)
    comparisons = []
    for ext in itertools.product(range(4), repeat=4):
        generated = expected = 0j
        for i5, i7 in itertools.product(range(4), repeat=2):
            # Pair[Lor5,Lor6] Pair[Lor7,Lor8] only survives i6=i5, i8=i7.
            idx = dict(enumerate(ext + (i5, i5, i7, i7), start=1))
            metric = base.metric(i5, i5) * base.metric(i7, i7)
            generated += base.evaluate(root, idx)
            left = eq32(idx[1], idx[2], first[0], idx[5], first[1], idx[8], first[2])
            right = eq32(idx[3], idx[4], second[0], idx[7], second[1], idx[6], second[2])
            # Propagators I*eta/ell^2 and I*eta/(ell-p)^2;
            # one closed ghost-loop minus, symmetry exactly 1: -I^2 = +1.
            expected += metric * left * right / (base.dot(ell, ell) * base.dot(first[2], first[2]))
        if abs(generated) > 1e-10:
            comparisons.append(base.close(generated, expected))
            print("NUMERICAL EXTERNAL Lor1..Lor4=%s generated=%s Eq32_two_I_propagators_loop_minus_symmetry_one=%s match=%s" %
                  (ext, generated, expected, comparisons[-1]))
            if len(comparisons) == 3:
                break
    if all(matches) and len(comparisons) == 3 and all(comparisons):
        print("NUMERICAL VERDICT PASS within stated contravariant-FVD convention; simultaneous sign reversal of all momenta at one vertex is unresolvable by a quadratic vertex. No integration, FORM, tadpole removal, on-shell limit or measure.")
        return 0
    print("NUMERICAL VERDICT INCONCLUSIVE: a vertex or nonzero graph component differs")
    return 2


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, SyntaxError, UnicodeError, OSError, ZeroDivisionError) as exc:
        print("NUMERICAL VERDICT INCONCLUSIVE: " + str(exc))
        sys.exit(2)
