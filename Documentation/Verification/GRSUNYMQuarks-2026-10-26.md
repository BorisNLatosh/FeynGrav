# GR + SU(N) with one massive quark flavour

26 October 2026, `FeynArts_Integration`, built on base commit `56728b7`.
The exact tested source/model snapshot is identified below, independently of
its delivery commit. **FA-YM-02 is complete within this tree scope.**
This local delivery does not constitute a push, merge or published release.

## Delivered model

The existing `GR-SU-N-Yang-Mills` bundle now has one fundamental Dirac flavour
with arbitrary unassigned ``FeynGrav`QuarkMass``, symbolic D/N and the common
`YangMillsCoupling`. Five families (`FFV`, `FFT`, `FFTT`, `FFTV`, `FFTTV`) extend
the 16 inherited families to 21 through κ² per vertex. The five-leg hh qbar q A
contact is included. There is no qbar q AA, quark–ghost or separate mass-insertion
vertex. The field assignment is F quark, S vector gravitational ghost, U scalar
adjoint YM ghost. T/V and the accepted ghost propagator signs are preserved.
Explicit old gravitational-ghost F selections must change to S.

`FermionModelBasis.wl` preserves ordered gamma chains as kinematic factors and
sorts only commuting factors. Unsupported gamma powers/chirality are rejected;
this is a finite companion, not a general spinor framework. Generic and class
vectors have matching native closure. Fundamental colour sums are symbolic N,
independent of mass. The model files execute no libraries, FORM or builder.

All 16 inherited **rule-expression hashes** match, with TFF renamed to TSS.
This is not a claim that the enlarged generic/classes files are byte-identical
to the old pure-YM files. The four other model bundles, original 12 notebooks,
interaction rules and libraries are unchanged. The new example is
[13. Quark_Graviton_Emission_Tree_Level_FeynArts.nb](../../Examples/13.%20Quark_Graviton_Emission_Tree_Level_FeynArts.nb).

## Independent normalisation and conventions

The reference expands the symmetrised massive Dirac action with a torsion-free
spin connection and symmetric vierbein, through second order. Determinant,
inverse-vierbein, kinetic, mass and spin terms are compared off shell with
symbolic D and generic momenta. The mixed qqA references reuse the independently
checked determinant–inverse-vierbein derivation from the
[quark–gluon correction](QuarkGluonKappa-2026-10-26.md).

Raw sources use exp(+ikx); native incoming spinors use the opposite Fourier
routing. For the canonical {-F,F,...} row, source p_fermion=-mom2,
p_antifermion=-mom1 and each source graviton momentum=-momj. Matrix order is
unchanged. This uniform map preserves momentum conservation and the standard
native inverse Dirac operator. Reversing only the fermion momenta misses the
single graviton derivative in the two-graviton spin connection. An exact
comparison identified the old Ward residual as twice the independent spin-term
variation. The full map resolves it. No interaction formula or stored library
was changed for this extension.

Native contacts, including hh qbar q A, agree with independently derived rules
under this map. Normalisation controls reject a rescaled rule/amplitude.
Massive κ=0 quark-to-AA and quark-to-hh references are sewn independently from
the action vertices and standard propagators. Existing verified pure-GR vertices
are reused rather than rederived. The hh massive reference equality completed
before the later FeynCalc-only batch timed out; the separate FORM follow-up
reproduces it and completes its nonzero-rescaling and massless controls.

## Complete selected trees and Ward checks

For incoming antiquark p1 and quark p2, and outgoing p3/p4:

| Process | Species-labelled channels |
| --- | --- |
| qbar q → AA | s gluon, s graviton, t/u quark |
| qbar q → hA | contact, s gluon, t/u quark |
| qbar q → hh | contact, s graviton, t/u quark |
| qq → qq | t/u gluon and t/u graviton; relative identical-fermion signs |

The first three amplitudes pass both bosonic Ward contractions at generic
on-shell Mandelstam invariants, symbolic D, N and m. External Dirac equations
are imposed with (slash p1−m) and (slash p2+m), without selecting spin states.
Untested gravitons have factorised transverse traceless polarisations; the tested
one is replaced by p_mu xi_nu+xi_mu p_nu with arbitrary xi. Missing required
contacts yield nonzero residuals. Independent coupling limits and a nonzero
physical matrix are also checked. The identical-quark check establishes native
relative exchange signs and rejects an altered sign pattern; it is not a new
independent spin-summed cross-section calculation.

The old auxiliary-parameter graviton test could return a vacuous zero because
its scalar parameter was left inside Momentum. It is superseded by the direct
symmetric variation (A(e+p)−A(e−p))/2, with gamma/momentum linearity expanded.
AA → Ah and AA → hh match retained bosonic amplitudes exactly and pass all four
genuine Ward contractions plus missing-contact controls. The earlier ordinary
AA → AA gluon checks did not use the defective auxiliary-parameter extraction.
The portable `YangMillsTreeChecks.wl` uses the corrected variation and public
FeynGravSimplify/FORM reducer; its installed-bundle AA → hh run passed 13/13.

## Bounded evidence

Local evidence (scripts, input/source hashes, raw diagnostics, FORM jobs,
resource/termination records and historical failures):
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-26-fa-ym02/`.
`plan.md`, `convention-resolution.md`, `ward-harness-correction.md`,
`600-second-review.md` and `example-diagnosis.md` explain the progression.
The table lists accepted batches, not every historical attempt:

| Run | Scope | Passed | Kernel lifetime, s | Peak aggregate memory, MiB |
| --- | --- | ---: | ---: | ---: |
| `compat-r3` | F/S/U prototype and signs | 16/16 | 3.630 | 206.21 |
| `audit-r3` | Independent Dirac action | 26/26 | 15.741 | 230.14 |
| `spin-phase` | Full Fourier map / independent spin term | 5/5 | 64.988 | 267.97 |
| `build-corrected` | 21-family construction and independent rules | 17/17 | 9.551 | 323.38 |
| `contacts-corrected` | Five native contacts, flips and massless rules | 25/25 | 5.565 | 256.92 |
| `tree-vv-corrected` | Massive quark annihilation to AA | 12/12 | 9.576 | 280.45 |
| `tree-tv-corrected` | Massive quark annihilation to hA | 12/12 | 9.224 | 257.41 |
| `tree-tt-corrected` | Massive quark annihilation to hh | 12/12 | 51.982 | 304.60 |
| `controls-form` | FORM independent TT reference controls | 6/6 | 6.647 | 231.92 |
| `compat-final-r2` | Final ghosts, propagators and identical-quark signs | 20/20 | 4.789 | 256.73 |
| `inherited-form` | AA to Ah regression and direct Ward tests | 14/14 | 7.527 | 260.23 |
| `inherited-tt-form` | AA to hh regression and direct Ward tests | 14/14 | 20.878 | 268.86 |
| `example13-checked` | Fourteen notebook inputs and retained reference | 11/11 | 6.255 | 277.95 |
| `final-build` | Reproducibility and current source hashes | 8/8 | 9.523 | 320.44 |
| `portable-installed` | Installed standalone bundle / portable AA to hh checks | 13/13 | 20.528 | 266.55 |

Earlier batches used 120 seconds; the maintainer expressly increased the bound
to 600 seconds for the later checks. All retained accepted runs used aggregate
1 GiB, zero swap, 10-second termination grace, fresh ≥2 GiB host headroom and
fresh sequential task-owned kernels. Final accepted diagnostic logs are empty,
exits are zero and no owned calculator remains. Resource enforcement is recorded
by the external runner, not inferred from command timeouts.

The 120- and 600-second independent-sewing timeouts are preserved as incomplete
batches, not counted as passes. In the 600-second run, 6/8 assertions completed;
the timeout was in the negative control after the TT equality had completed.
The remaining controls were performed with public `FeynGravSimplify` and serial
FORM on the saved independent expressions: massive equality, rescaling rejection
and massless equality all passed in a fresh 6.647-second batch. Individual facade
calls took about 1.519, 1.300 and 0.151 seconds. These timings **exclude reference
construction**, so they do not measure a ten-minute-to-six-second full workflow
speedup. Wolfram handles notation/kinematic substitutions and final rational
comparison; FORM handles the Lorentz, Clifford and colour algebra.

Historical compatibility/translation and notebook harness failures are retained.
The final ghost fixture explicitly selects S loops: an unfiltered complete-model
fixture also contained a pure-GR loop. This was a test-selection error, not a
changed ghost sign. Notebook preparation initially missed composite/slashed
momentum routing; MomentumExpand before substitution and DiracGammaExpand before
FORM fix that issue. A final assertion counted distinct external spinors rather
than their repeated appearances across a sum. No false output was shipped.

## Example and delivery checks

All fourteen inputs of example 13 were evaluated, with eleven validation
assertions. Its five displayed checks (diagram count, two coupling limits, two
Ward identities) return True. The matrix agrees exactly with the retained native
hA amplitude. The notebook has generated diagram graphics and saved outputs;
input boxes round-trip to their held expressions. No GUI/front-end evaluation
is claimed. Outgoing polarisations are conjugated and D/N/m remain symbolic.
FORM works on the open matrix; external spinors are attached afterwards.

A fresh final build reproduces the tested .gen/.mod bytes and all 21 rule hashes,
refreshes source hashes, and initialises without warnings. The installed bundle
then passes the portable standalone check without loading FeynGrav interactions.

## Identity and limits

- `GR-SU-N-Yang-Mills.gen` SHA256: `75ce50fc481af1e3437f16a323c9e2b03b022589b324422d3bb064217de00553`.
- `GR-SU-N-Yang-Mills.mod` SHA256: `2ff5b477182b50fcb1e9e31f788e30d3dfa53e2960680936326801ae78218182`.

The installed manifest records every source/library hash, action parameters,
field/family inventory, gauges, cutoff and explicit Dirac momentum map.

Native loop amplitudes, renormalisation, additional flavours, higher κ orders,
gamma5, full five-leg scattering, arbitrary crossed processes and front-end
rendering are not certified. The closed-ghost fixture verifies combinatorics
only. Existing source/library corrections are preserved; this delivery does
not claim that all higher-order source rules have been audited.

Final static review found 156 protected source/library/notebook/other-bundle
files byte-identical and checked 157 local Markdown links with no missing
target. Manifest source hashes match the current files. Source/documentation
whitespace checks are clean; the newly staged Mathematica-serialised notebook
contains formatting-only trailing whitespace, retained to preserve its verified
bytes and saved outputs. The ignored root `AGENTS.md` remains excluded through `.git/info/exclude`.
