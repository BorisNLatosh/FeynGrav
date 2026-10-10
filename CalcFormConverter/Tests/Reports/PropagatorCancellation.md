# Numerator–propagator cancellation — 10 October 2026

## Implemented behaviour

New exports and `CalcFormCalculate` calls perform a bounded algebraic cancellation stage in FORM before final propagator grouping. Small exact rational routing matrices determine independent denominator bases; numerator expressions are not expanded or simplified in Mathematica. Scalar-product substitutions, expansion and power cancellation take place in FORM. Remaining numerator polynomials and surviving mapped propagators are restored before output.

The stage preserves momentum routing and denominator-free terms. It performs no integration, momentum shifts, scaleless-integral deletion, partial fractions or mass-difference/Gram-determinant division. Ordinary Feynman prescriptions are understood in the infinitesimal limit. Existing result formats and import interpretation remain unchanged.

There are three conservative guards:

- More than 256 independent nonempty denominator subsets: retain the previous processing for the job.
- A candidate with more than 16 expanded terms and more than twice the original term count: restore the original expression.
- A candidate with more than four times the original propagator-group count: restore the original expression.

The latter two checks run in FORM against a hidden backup after computing the candidate. They do not bound intermediate time or memory, and they do not guarantee the smallest final factored representation. Fallback is for the complete job. Irreducible numerator scalar products remain; opaque scalar functions and abbreviations are not opened by this stage.

## Verification

All 1,601 assertions passed:

| Suite | Assertions | Outcome |
|---|---:|---|
| Core, general/flat/tree parsers, transactions, installer mocks | 712 | Passed |
| Export, FORM staging and import | 235 | Passed |
| Runtime, epsilon, Dirac translation/algebra, colour, propagator grouping and rational coefficients | 589 | Passed |
| New numerator cancellation | 65 | Passed |

Automatic main-package loading and namespace isolation also passed. Fresh Wolfram kernels used installed FORM/TFORM 5.0.2. Tests did not install software.

The new tests cover serial FORM and TFORM with two workers; repeated propagator powers; negative and rational momentum routing; irreducible numerators; polynomial, radical, imaginary and named masses; separate symbol contexts; tensor, Dirac, colour and epsilon coefficients; several denominator families; two-loop routing; complete cancellation; retained constants; private-name restoration; planning-budget fallback and exact restoration after structural growth. Small cases agree with `ApartFF[..., FDS -> False, DropScaleless -> False]` and independently constructed rational identities. These comparisons are outside production processing.

One existing rational-coefficient test previously required an expanded difference with opaque propagators to be literally zero. Its comparison now exposes the small test denominators and uses an exact rational residual, because successful numerator cancellation changes the denominator basis. Its mathematical expected expression remains unchanged.

## Retained realistic coefficient

Source: the previously retained first coefficient from `One-Loop-Counterterms.nb`, stored locally as `/tmp/cfc-rational-probe/first.wl`. This is one coefficient, not the complete large result.

| Observation | Result |
|---|---:|
| Original input | 10,697 leaves |
| Unguarded cancellation candidate | 120,820 leaves, 15 groups |
| FORM expanded terms before / after cancellation | 882 / 14,980 |
| Accepted result after fallback and existing coefficient factorisation | 388 leaves, 1 group |
| Final public call, single observation, two workers | 0.681851 s |
| Peak process-tree RSS in the bounded verification run | 334,733,312 bytes |
| Exact rational residual against the input | Zero |

The candidate was mathematically correct but substantially larger. The guards now reject it and retain the previous compact result. This is evidence for protecting that example, not evidence that cancellation speeds up or compresses the complete bubble. The measured call is a single observation, with other small regression processes running during verification; no performance comparison is claimed.

Verification ran with a 300-second / 6-GiB process-tree RSS / 2-GiB available-memory monitor. No bound was reached. No Full benchmark or complete large-result cancellation run was performed. No notebooks, interaction formulae or stored libraries were changed. No commit was made.

## Reproduction

Run `python3 CalcFormConverter/Tests/run.py --suite core`, `--suite form` and `--suite runtime` from the package tree. The runtime suite includes `PropagatorCancellation.wls`. Runtime tests require a working Wolfram process environment; the tool sandbox may block subprocess execution.

Supporting temporary logs from this run: `/tmp/cfc-cancel-tests/core.log`, `form-final.log`, `runtime-final.log`, `loading.log`, and `/tmp/cfc-parallel-factor/cancellation-first-verified.json`. Compact results and source hashes are retained beside this report in `PropagatorCancellation.json`.

## Literature basis

- [Shtabovenko, Mertig and Orellana, New Developments in FeynCalc 9.0, section 3.2](https://arxiv.org/pdf/1601.01167).
- [Feng, $Apart: A Generalized Mathematica Apart Function](https://arxiv.org/pdf/1204.2314).
- [FeynCalc ApartFF documentation](https://feyncalc.github.io/FeynCalcBook/ApartFF.html), including the distinction between numerator cancellation, partial fractioning, momentum shifts and scaleless-integral removal.
