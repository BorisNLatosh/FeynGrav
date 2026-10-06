# FeynGrav function reference

Development reference checked against the working source on 6 October 2026. This describes the current checkout, not a guarantee about older releases. Start with [installation](../README.md#installation-and-help); complete calculations are in the [example notebooks](../Examples/README.md). For generator-side functions, use the [Rules contracts](../Rules/README.md) and [generator guide](../Libs/Generator.md).

## Contents

- [Conventions and return values](#conventions-and-return-values)
- [Propagators](#propagators)
- [Interaction vertices](#interaction-vertices)
- [Polarisation objects](#polarisation-objects)
- [Projectors and operators](#projectors-and-operators)
- [Library importers](#library-importers)
- [Gauge parameters and coupling](#gauge-parameters-and-coupling)
- [Command discovery and initialisation](#command-discovery-and-initialisation)
- [FORM commands](#form-commands)

## Conventions and return values

Load ``<< FeynGrav` `` in a fresh kernel. The main interface is in the `FeynGrav` context; projector utilities belong to `Nieuwenhuizen` and converter commands to `CalcFormConverter`. Their short names are available after loading. Use fully qualified names if another package exports the same spelling.

In the signatures below:

- `pairs` means a **flat** list `{mu1, nu1, ..., mun, nun}` of graviton index pairs.
- `triples` means a **flat** list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples.
- `momenta` means `{p1, ..., ps}`. Symbols such as `mu`, `p`, `m` and `g` denote indices, momenta, masses and couplings, respectively; they are not literal argument names.
- Vertex momenta are incoming. Explicit momentum arguments can contain linear combinations where the corresponding FeynCalc objects support them. Polarisation shortcuts have the stricter symbol-only signatures stated below.
- Tensor rules use FeynCalc's symbolic D-dimensional objects unless specified otherwise. Masses and couplings may remain symbolic. Propagators use `FAD` notation; expand denominators deliberately with FeynCalc when explicit rational expressions are needed.

Successful propagator and vertex calls return symbolic FeynCalc expressions, including their implemented factors of `I` and coupling constants. They do not add diagram symmetry factors, loop integration or on-shell conditions. No optional arguments are accepted unless listed. Singular parameter choices are not automatically diagnosed.

**Loaded vertices match the available library signatures.** An unavailable order, wrong list length or unmatched call can remain unevaluated; these wrappers do not promise a `Failure` for every invalid call. Rule-generation functions in `Rules` have a separate validation contract. Importers and converter commands have explicit failure handling described below. Do not interpret an unevaluated vertex or `Failure` as a zero interaction.

## Propagators

These definitions require no additional vertex library. All tensor indices are D-dimensional. The displayed signatures have no optional arguments.

| Command | Meaning and restrictions |
| --- | --- |
| `ScalarPropagator[p, m]` | Scalar propagator `I FAD[{p,m}]`; `m = 0` is supported. |
| `ProcaPropagator[mu, nu, p, m]` | Massive-vector propagator, with numerator `-I (MTD[mu,nu] - FVD[p,mu] FVD[p,nu]/m^2)`. Its massless limit cannot be obtained by substituting `m = 0`. |
| `GravitonPropagator[mu, nu, alpha, beta, p]` | General-relativity propagator using the current `GaugeFixingEpsilon`, initially 2. |
| `GravitonPropagatorMassive[mu, nu, alpha, beta, p, m]` | Massive-gravity propagator with the implemented `1/(D-1)` trace subtraction. Requires nonsingular mass and dimension choices; `m = 0` is not a massless-propagator shortcut. |
| `GhostVectorPropagator[mu, nu, p]` | Gravitational Faddeev–Popov ghost propagator `I MTD[mu,nu] FAD[p]`. |
| `GhostVectorPropagatorHD[mu, nu, p]` | Ghost propagator for higher-derivative gauge fixing; depends on `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`, including their inverse combinations. |
| `QuadraticGravityPropagator[mu, nu, alpha, beta, p, m0, m2]` | Quadratic-gravity propagator with conventional gauge fixing, using the current `GaugeFixingEpsilon`. |
| `QuadraticGravityPropagatorHD[mu, nu, alpha, beta, p, m0, m2]` | Quadratic-gravity propagator with higher-derivative gauge fixing, using all three HD gauge parameters. |
| `GravitonPropagatorCR[mu, nu, alpha, beta, p]` | Cheung–Remmen graviton propagator using the current `GaugeFixingEpsilonCR`, initially `-1/2`. |
| `GravitonPropagatorAuxiliaryCR[lambda1, mu1, nu1, lambda2, mu2, nu2]` | Algebraic auxiliary-field propagator in Cheung–Remmen variables. Each group of three indices labels one auxiliary field; there is no momentum argument. |

For both quadratic-gravity propagators, `m2` is the massive spin-2 pole parameter. `m0` equals the scalar pole mass in four dimensions. The implemented scalar pole mass squared in D dimensions is

$$m_s^2=\frac{3(D-2)m_0^2m_2^2}{(D-4)m_0^2+2(D-1)m_2^2}.$$

The parameter denominators and projector denominators must be treated symbolically before taking singular limits. See [gauge parameters](#gauge-parameters-and-coupling) for when their values are applied.

## Interaction vertices

### Scalars, fermions and vectors

The listed libraries are loaded through order 2 during successful package initialisation. For an order `n`, supply `2 n` entries in `pairs` or `3 n` in `triples`, as specified. Higher orders require the corresponding files and importer call. These signatures have no optional arguments.

| Command | Arguments and returned interaction | Library preparation |
| --- | --- | --- |
| `GravitonScalarVertex[pairs, p1, p2, m]` | Two incoming scalar momenta and their mass; minimal scalar coupling to `n` gravitons. | `importScalars[n]` |
| `GravitonScalarPotentialVertex[pairs, lambda]` | Scalar-potential vertex with the supplied potential coupling; no scalar momentum arguments. The convention is the potential monomial `lambda phi^r/r!`. | `importScalars[n]` |
| `GravitonFermionVertex[triples, p1, p2, m]` | Incoming fermion and antifermion momenta, then mass; returns a Dirac-matrix expression. | `importFermions[n]` |
| `GravitonMassiveVectorVertex[pairs, lambda1, p1, lambda2, p2, m]` | Each vector is specified by index then momentum, followed by their mass. | `importVectors[n]` |
| `GravitonVectorVertex[triples, lambda1, p1, lambda2, p2]` | Massless-vector vertex; depends on the vector gauge value used during import. | `importVectors[n]` |
| `GravitonVectorGhostVertex[pairs, p1, p2]` | Two incoming momenta of the massless-vector Faddeev–Popov ghosts. | `importVectors[n]` |

### Graviton self-interactions and gravitational ghosts

| Command | Arguments and returned interaction | Availability |
| --- | --- | --- |
| `GravitonVertex[mu1, nu1, p1, ..., mur, nur, pr]` | `r >= 3` graviton legs supplied as separate arguments, **not a list**. Each leg has index–index–momentum order. | `importGravitons[r-2]`; three- and four-leg vertices are loaded initially. |
| `QuadraticGravityVertex[triples, m0, m2]` | `r >= 3` graviton legs in one flat list, followed by the two quadratic-gravity mass parameters. | `importQuadraticGravity[r-2]`; not loaded initially. |
| `GravitonGhostVertex[rho, sigma, k, mu, p1, nu, p2]` | One graviton `(rho,sigma,k)` and two gravitational ghost legs `(mu,p1)`, `(nu,p2)`; all momenta incoming. | Direct definition, no extra import. |
| `GravitonGhostVertexHD[rho, sigma, k, mu, p1, nu, p2]` | Same leg order with higher-derivative ghost gauge fixing; depends on `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`. | Direct definition, no extra import. |

The ghost commands implement these fixed signatures; they do not generate arbitrary multi-graviton ghost vertices. Quadratic-gravity mass parameters have the meaning given in [Propagators](#propagators).

### SU(N) Yang–Mills interactions

Call `importSUNYM[n]` first. Colour labels are adjoint indices; all momenta are incoming. Gluon and ghost argument orders differ, so use the explicit signatures below.

| Command | Arguments and returned interaction |
| --- | --- |
| `GravitonGluonVertex[triples, p1, lambda1, a1, ..., pl, lambdal, al]` | `l = 2, 3, 4` gluons, each supplied as momentum–Lorentz-index–colour-index. Selects the two-, three- or four-gluon library by argument count. |
| `GravitonQuarkGluonVertex[pairs, lambda, a]` | Quark–antiquark–gluon coupling; the gluon is specified by its Lorentz and colour indices. No momentum arguments. |
| `GravitonYMGhostVertex[pairs, p1, a1, p2, a2]` | Two Yang–Mills ghost momenta and colour labels. |
| `GravitonGluonGhostVertex[pairs, lambda1, a1, p1, lambda2, a2, p2, lambda3, a3, p3]` | Gluon–ghost interaction in the existing library interface, with three index–colour–momentum groups. Retain all nine leg arguments, including the ghost-position Lorentz placeholders. |

These functions return the expressions stored in the selected libraries. The generator now preserves dimensional quark gamma matrices and named `SMP` couplings; migration did not regenerate distributed libraries. Do not assume newly generated and older stored expressions use identical colour or coupling notation. `GaugeFixingEpsilonSUNYM` is applied during import.

### Horndeski, axion and scalar–Gauss–Bonnet interactions

Each row requires its sector importer. There are no optional vertex arguments; coupling constants are explicit final arguments.

| Command | Scalar count and leg structure | Preparation |
| --- | --- | --- |
| `HorndeskiG2[pairs, momenta, b, lambda]` | `s = a + 2 b` scalar momenta; graviton indices only. | `importHorndeskiG2[]` |
| `HorndeskiG3[triples, momenta, b, lambda]` | `s = a + 2 b + 1`; graviton triples. | `importHorndeskiG3[]` |
| `HorndeskiG4[triples, momenta, b, lambda]` | `s = a + 2 b`; graviton triples. | `importHorndeskiG4[]` |
| `HorndeskiG5[triples, momenta, b, lambda]` | `s = a + 2 b + 1`; graviton triples. | `importHorndeskiG5[]` |
| `GravitonAxionVectorVertex[pairs, lambda1, p1, lambda2, p2, theta]` | Two vector index–momentum groups and axion coupling `theta`; no axion momentum argument. | `importAxionVectorVertex[n]` |
| `ScalarGaussBonnet[triples, g]` | `n >= 2` graviton legs, followed by the scalar–Gauss–Bonnet coupling; no separate scalar momentum argument. | `importScalarGaussBonnet[n]` |

For the Horndeski monomial `lambda phi^a X^b`, `a` and `b` are non-negative integers. The library filename encodes `a`, `b` and graviton count `n`; the vertex call must match an installed triple. These wrappers do not automatically generate missing combinations. Generator-specific selection filters and rule limitations are documented separately.

Around flat space, each curvature begins at first order in the graviton perturbation. The scalar–Gauss–Bonnet interaction therefore starts at two gravitons. Its one-graviton contribution is zero, but the library interface requires at least two legs; it is not a command for returning that zero.

### Cheung–Remmen vertices

These fixed D-dimensional definitions are available without additional libraries. They have no options and include the implemented factors of the gravitational coupling.

| Command | Leg order |
| --- | --- |
| `GravitonVertexCRhhh[mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3]` | Three graviton index–index–momentum triples as separate arguments. |
| `GravitonVertexCRBhh[alpha, rho, sigma, mu1, nu1, p1, mu2, nu2, p2]` | Three auxiliary-field indices, then two graviton triples. |
| `GravitonVertexCRBBh[alpha1, rho1, sigma1, alpha2, rho2, sigma2, mu, nu]` | Two groups of auxiliary-field indices, then graviton indices. No momentum arguments. |

## Polarisation objects

These shortcuts require no vertex libraries. `p`, `mu` and `nu` must be standalone symbols. Their argument order begins with the **momentum**.

| Command | Dimension and result |
| --- | --- |
| `PolarizationVectorD[p, mu, phase : I, opts]` | D-dimensional `Pair[LorentzIndex[mu,D], Momentum[Polarization[p,phase,...],D]]`. |
| `PolarizationTensor[p, mu, nu, phase : I, opts]` | Four-dimensional product of two polarisation vectors with the same momentum and phase. |
| `PolarizationTensorD[p, mu, nu, phase : I, opts]` | The corresponding D-dimensional factorised product. |

Here `phase : I` indicates an optional positional argument with default `I`; call with `I` or `-I`, not that pattern notation. `-I` denotes complex conjugation of the vector. These labels are neither multiplicative factors nor helicity labels.

The sole option is `Transversality -> False`. Omitting it leaves the internal `Polarization` object without an explicit transversality option; an explicit option is forwarded. `Transversality -> True` imposes only the momentum–polarisation contraction being zero. It does not impose mass shell, the null-vector condition or normalisation.

For nonzero massless on-shell momentum, the factorised spin-2 tensor is transverse and traceless when its underlying vector is transverse and null. The D-dimensional shortcut does not construct a complete polarisation basis. Wrong patterns can remain unevaluated rather than producing `Failure`.

```mathematica
PolarizationTensorD[p, mu, nu, -I, Transversality -> True]
```

FeynCalc supplies the four-dimensional `PolarizationVector[p, mu, ...]`; it is not a new FeynGrav function.

## Projectors and operators

These public helpers are loaded from `Nieuwenhuizen`. All tensor objects are D-dimensional; the package retains the fixed `1/3` coefficients in `NieuwenhuizenOperator0` and `NieuwenhuizenOperator2`. Do not replace them by `1/(D-1)` when interpreting this interface: the implemented basis and inverse use these conventions.

The transverse and longitudinal tensors are respectively `theta = eta - p p/p^2` and `omega = p p/p^2`. Their implementation uses FeynCalc propagator notation for inverse momentum squares. No singularity check at `p^2 = 0` is performed.

| Command | Returned object |
| --- | --- |
| `GaugeProjector[mu, nu, p]` | `theta(mu,nu)`. |
| `GaugeProjectorBar[mu, nu, p]` | `omega(mu,nu)`. |
| `NieuwenhuizenOperator1[mu, nu, alpha, beta, p]` | Half the sum of the four mixed `theta omega` terms. |
| `NieuwenhuizenOperator2[mu, nu, alpha, beta, p]` | Symmetrised transverse identity minus `theta(mu,nu) theta(alpha,beta)/3`. |
| `NieuwenhuizenOperator0[mu, nu, alpha, beta, p]` | `theta(mu,nu) theta(alpha,beta)/3`. |
| `NieuwenhuizenOperator0Bar[mu, nu, alpha, beta, p]` | `omega(mu,nu) omega(alpha,beta)`. |
| `NieuwenhuizenOperator0BarBar[mu, nu, alpha, beta, p]` | `theta(mu,nu) omega(alpha,beta) + omega(mu,nu) theta(alpha,beta)`, without an extra square-root normalisation. |
| `NieuwenhuizenOperator[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]` | Linear combination in exactly this coefficient order: `1, 2, 0, 0Bar, 0BarBar`. |
| `NieuwenhuizenOperatorInverse[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]` | The implemented D-dependent inverse in the same basis, assuming its scalar denominators are nonzero. Singular cases are not diagnosed automatically. |
| `NieuwenhuizenSymmetryCheck[T, mu, nu, alpha, beta, p]` | Checks both within-pair swaps and exchange of the two pairs. Returns `True`, `False`, or a diagnostic `Failure`; requires usable FORM. |
| `NieuwenhuizenOperatorExpansion[T, mu, nu, alpha, beta, p]` | Returns `{z1,z2,z0,zb,zbb}` only after symmetry checks, coefficient extraction and complete reconstruction verification; requires usable FORM. |

There are no `LongForm` options. Projector constructors accept symbolic expressions rather than lists or associations for each argument. Structural errors return `Failure`, and incoming dependency failures propagate. Expansion is limited to tensors representable by this five-element basis with scalar coefficients. Unsupported residual tensor structures, inconclusive verification and calculation failures have distinct diagnostics. Check `FailureQ` before using coefficients; a failed FORM call is not a failed physical symmetry identity.

## Library importers

Imports read local extensionless files; they neither download nor generate them. Successful calls normally return `Null` after installing definitions. They support `printOutput -> False`; set it to `True` for inventory and progress messages. Legacy private `printOutput` remains accepted, with an explicit public option taking precedence. A non-Boolean resolved option returns `Failure`.

For order-limited importers, omitted `n` defaults to 2. Except for Gauss–Bonnet, `n` must be a positive integer. The requested maximum is capped by the installed families, but missing intermediate files cause failure. Failed reads or invalid orders preserve previously installed definitions. A successful lower-order reimport replaces the family's definitions through the selected order; it does not retain previously loaded higher orders.

| Command | Files and order convention |
| --- | --- |
| `importGravitons[n : 2, opts]` | `GravitonVertex_j`, `1 <= j <= n`; `j + 2` graviton legs. Loaded initially with 2. |
| `importScalars[n : 2, opts]` | Scalar kinetic and potential files for 1 through `n` graviton pairs. Loaded initially with 2. |
| `importFermions[n : 2, opts]` | Fermion files for 1 through `n` graviton triples. Loaded initially with 2. |
| `importVectors[n : 2, opts]` | Massive vector, massless vector and vector ghost files through `n`. Loaded initially with 2. |
| `importSUNYM[n : 2, opts]` | Six Yang–Mills file families: two-, three- and four-gluon, quark–gluon, ghost and gluon–ghost. |
| `importAxionVectorVertex[n : 2, opts]` | Axion–vector files through `n` graviton pairs. |
| `importQuadraticGravity[n : 2, opts]` | `QuadraticGravityVertex_j`, where `j + 2` is the number of graviton legs. |
| `importScalarGaussBonnet[n : 2, opts]` | Files for 2 through `n` graviton triples; requires `n >= 2`. |
| `importHorndeskiG2[opts]` | All available G2 files; no order argument. |
| `importHorndeskiG3[opts]` | All available G3 files; no order argument. |
| `importHorndeskiG4[opts]` | All available G4 files; no order argument. |
| `importHorndeskiG5[opts]` | All available G5 files; no order argument. |

`n : 2` above describes the default; actual calls use `importScalars[]` or `importScalars[2]`. Horndeski filenames have the form `HorndeskiG2_a_b_n`, and the installed files determine the accepted vertex signatures. Use `printOutput -> True` to inspect that selection. Avoid assuming every requested model/order is distributed.

## Gauge parameters and coupling

These are public symbols, not functions or options. Initialisation assigns or clears them as below. They require no additional library to exist.

| Symbol | Initial value | Where the value is used |
| --- | --- | --- |
| `GaugeFixingEpsilon` | `2` | Conventional gravitational propagators when called. |
| `GaugeFixingEpsilonCR` | `-1/2` | Cheung–Remmen graviton propagator when called. |
| `GaugeFixingEpsilonHD` | Unassigned | Overall higher-derivative graviton gauge contribution. |
| `GaugeFixingEpsilonHD0` | Unassigned | HD graviton/ghost propagators and ghost vertex. |
| `GaugeFixingEpsilonHD1` | Unassigned | HD longitudinal coefficient in those propagators and ghost vertex; represents the surviving longitudinal combination in the package convention. |
| `GaugeFixingEpsilonVector` | `-1` | Massless-vector libraries during `importVectors`. Set before reimporting to change imported dependence. |
| `GaugeFixingEpsilonSUNYM` | `-1` | Yang–Mills libraries during `importSUNYM`. Set before importing. |
| ``FeynGrav`\[Kappa]`` | Symbolic | Gravitational coupling appearing in package expressions. The corresponding Unicode symbol is `κ`. |

Changing a gauge parameter after a library import cannot restore symbolic dependence already replaced during that import. Restarting or reloading can reset initialised defaults. These symbols themselves perform no parameter validation or singularity diagnosis.

## Command discovery and initialisation

- `FeynGravCommands[]` prints the main command list and returns `Null`; it is a discovery aid, not an exhaustive list of every dependency export.
- `?FunctionName` shows the installed usage message. `Options[FunctionName]` inspects supported options. This Markdown reference is not native F1 help.
- `DisplayInitializationMessages[]` reprints the introduction and returns `Null`; it does not import libraries or check FORM.
- `FeynGravInitialized` is the package's initialisation state flag. Successful required imports set it to `True`; initialisation failures leave it false and report `InitializationFailed`. Treat it as state to inspect, not a configuration switch to set manually.

Main-package loading imports default libraries and evaluates definitions but starts no FORM process. The separate generator loads rule implementations with some identical short names: use a **different fresh kernel** for generation to avoid shadowing the library-backed interface.

## FORM commands

These are available after main-package loading; the [converter guide](../CalcFormConverter/README.md#command-and-option-reference) is the authoritative option and failure reference.

| Command | Return and dependency |
| --- | --- |
| `CalcFormExport[expr, file, opts]` | Job-path association or `Failure`; writes source/mapping, without executing FORM. |
| `CalcFormImport[resultFile, mappingFile]` | Reconstructed FeynCalc expression or `Failure`; no executable needed. |
| `CalcFormCheck[opts]` | Availability association or an option/setup `Failure`; may launch a small probe. |
| `CalcFormInstall[opts]` | Explicit installation workflow on supported systems; never invoked implicitly. |
| `CalcFormCalculate[expr, opts]` | Algebraic result, equality result or `Failure`; requires usable FORM/TFORM. |

The default worker setting is `FORMThreads -> Automatic`. Supported Dirac algebra is enabled by `DiracAlgebra -> Automatic`; fundamental SU(N) reduction by `ColourAlgebra -> True`. Disabling either selects preservation for that space, subject to the converter's vocabulary restrictions. No command here performs loop integration.
