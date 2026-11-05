# FeynGrav function reference

Reference for the unreleased 4.2.0 development checkout. The original reference review was on 6 October 2026; later interface additions are documented below. Version labelling does not constitute a fresh check of every entry or a guarantee about older releases. Start with [installation](./UserGuide.md#Installation%20and%20help); complete calculations are in the [example notebooks](./Examples/README.md). For generator-side functions, use the [Rules contracts](./Services/RuleConstruction/Contracts.md) and [generator guide](./Services/LibraryGeneration/Guide.md).

## Contents

For individual command and parameter pages, use the [complete alphabetical index](./Symbols.md),
[service groups](./Services/README.md) and [constants and parameters](./Parameters/README.md).
The sections below retain the older overview and link targets.

- [Conventions and return values](#Conventions%20and%20return%20values)
- [Conventions report](#Conventions%20report)
- [Propagators](#Propagators)
- [Interaction vertices](#Interaction%20vertices)
- [Polarisation objects](#Polarisation%20objects)
- [Projectors and operators](#Projectors%20and%20operators)
- [Library importers](#Library%20importers)
- [Library import records](#Library%20import%20records)
- [Gauge parameters and coupling](#Gauge%20parameters%20and%20coupling)
- [Command discovery and initialisation](#Command%20discovery%20and%20initialisation)
- [FORM commands](#FORM%20commands)

<a name="Conventions and return values"></a>

## Conventions and return values

Read [Common conventions](./Conventions.md) for the shared mathematical
definitions, momentum and polarisation conventions, and integral normalisation.
For a first session, use [Getting started](./GettingStarted.md).

Load ``<< FeynGrav` `` in a fresh kernel. The main interface is in the `FeynGrav` context; projector utilities belong to `Nieuwenhuizen` and converter commands to `CalcFormConverter`. Their short names are available after loading. Use fully qualified names if another package exports the same spelling.

In the signatures below:

- `pairs` means a **flat** list `{mu1, nu1, ..., mun, nun}` of graviton index pairs.
- `triples` means a **flat** list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples.
- `momenta` means `{p1, ..., ps}`. Symbols such as `mu`, `p`, `m` and `g` denote indices, momenta, masses and couplings, respectively; they are not literal argument names.
- Vertex momenta are incoming. Explicit momentum arguments can contain linear combinations where the corresponding FeynCalc objects support them. Polarisation shortcuts have the stricter symbol-only signatures stated below.
- Tensor rules use FeynCalc's symbolic D-dimensional objects unless specified otherwise. Masses and couplings may remain symbolic. Propagators use `FAD` notation; expand denominators deliberately with FeynCalc when explicit rational expressions are needed.

Successful propagator and vertex calls return symbolic FeynCalc expressions, including their implemented factors of `I` and coupling constants. They do not add diagram symmetry factors, loop integration or on-shell conditions. No optional arguments are accepted unless listed. Singular parameter choices are not automatically diagnosed.

**Loaded vertices match the available library signatures.** An unavailable order, wrong list length or unmatched call can remain unevaluated; these wrappers do not promise a `Failure` for every invalid call. Rule-generation functions in `Rules` have a separate validation contract. Importers and converter commands have explicit failure handling described below. Do not interpret an unevaluated vertex or `Failure` as a zero interaction.

<a name="Conventions report"></a>

## Conventions report

```mathematica
FeynGravConventions[]
```

Prints a fully visible, static report and returns `Null`. The command takes no arguments or options; invalid calls return `Failure`. It is listed by [`FeynGravCommands[]`](Services/PackageInformation/FeynGravCommands.md) and has `?` help. Mathematical formulas appear in notebook notation, with tables protected by `StandardForm`; kernels without a front end receive plain text.

The six sections cover:

1. The curvature definition, positive Fourier phase, its D-dimensional extension, metric expansion and gravitational coupling normalisation. Cheung–Remmen variables are a separate parametrisation.
2. Lorentz dimensions, incoming vertex momenta and the current assignment to [`D`](./Parameters/Conventions/D.md).
3. Initial and current gauge parameters, actual loaded orders, and differences between recorded import settings and the current session.
4. Momentum-first polarisation syntax, conjugation labels, current transversality defaults and limitations of the tensor product shortcuts.
5. Rank-four epsilon conventions, trace normalisation, the current FeynCalc Dirac scheme, supported Dirac operations and fundamental SU(N) normalisation. The independent defaults of [`CalcFormExport`](./Services/Calculation/CalcFormExport.md), [`CalcFormCalculate`](./Services/Calculation/CalcFormCalculate.md), [`FeynGravFORMExport`](./Services/Calculation/FeynGravFORMExport.md) and [`FeynGravSimplify`](./Services/Calculation/FeynGravSimplify.md) are shown separately, together with the configured [`Method`](./Services/Options/Method.md) and the distinction between the full pipeline and restricted methods.
6. Factors already supplied by interaction rules and factors the converter does not add, together with the limits of ordinary `FAD` prescription information.

Fixed formulas are inert display data: assignments to symbols do not change them. Historical import settings remain held when displayed. The report reads the existing [library import records](#Library%20import%20records); it does not import libraries, calculate tensors, launch FORM, write files or change settings. It is not printed automatically on loading.

“Not recorded” means that provenance is unavailable. Records describe the last successful import, not historical generation settings, manually redefined vertices or previously calculated user expressions. Reimport a library when a gauge value was substituted into its vertices. Other setting differences need not affect every expression. Use [`FeynGravLibraryInformation[]`](Services/LibraryManagement/FeynGravLibraryInformation.md) for detailed diagnostic data, including paths and hashes.

FeynCalc's Cartesian metric setting is reported separately from the package's spacetime convention; changing it does not transform stored interactions. Likewise, displaying a FeynCalc Dirac scheme does not imply converter support for all operations in that scheme. Unavailable dependency settings are labelled explicitly. The effective omitted transversality setting for all four polarisation shortcuts is read from `Options[Polarization]`; wrapper option declarations do not override it. Delayed option defaults (`:>`), including [`Method`](./Services/Options/Method.md), are displayed as held expressions labelled “Delayed default”, without evaluating them or choosing explanatory text from their values.

Representative curvature and momentum-space checks and the display checks are recorded in [Conventions verification](./Conventions.md). They do not constitute a proof for every interaction or perturbative order.

<a name="Propagators"></a>

## Propagators

These definitions require no additional vertex library. All tensor indices are D-dimensional. The displayed signatures have no optional arguments.

| Command | Meaning and restrictions |
| --- | --- |
| [`ScalarPropagator[p, m]`](Models/Scalar/ScalarPropagator.md) | Scalar propagator `I FAD[{p,m}]`; `m = 0` is supported. |
| [`ProcaPropagator[mu, nu, p, m]`](Models/Vector/ProcaPropagator.md) | Massive-vector propagator, with numerator `-I (MTD[mu,nu] - FVD[p,mu] FVD[p,nu]/m^2)`. Its massless limit cannot be obtained by substituting `m = 0`. |
| [`GravitonPropagator[mu, nu, alpha, beta, p]`](Models/GeneralRelativity/GravitonPropagator.md) | General-relativity propagator using the current [`GaugeFixingEpsilon`](./Parameters/GaugeFixing/GaugeFixingEpsilon.md), initially 2. |
| [`GravitonPropagatorMassive[mu, nu, alpha, beta, p, m]`](Models/MassiveSpin2/GravitonPropagatorMassive.md) | Massive-gravity propagator with the implemented `1/(D-1)` trace subtraction. Requires nonsingular mass and dimension choices; `m = 0` is not a massless-propagator shortcut. |
| [`GhostVectorPropagator[mu, nu, p]`](Models/GeneralRelativity/GhostVectorPropagator.md) | Conventional gravitational Faddeev–Popov source propagator `-I MTD[mu,nu] FAD[p]` (ordered ghost–antighost contraction). The shipped `GR` model has the same negative line; its [tree-level release](./Services/FeynArtsIntegration/Guide.md#Current%20tree%20release%20%E2%80%94%2018%20October%202026) defers loop compatibility checks. |
| [`GhostVectorPropagatorHD[mu, nu, p]`](Models/QuadraticGravity/GhostVectorPropagatorHD.md) | Ghost propagator for higher-derivative gauge fixing; depends on [`GaugeFixingEpsilonHD0`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD0.md) and [`GaugeFixingEpsilonHD1`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD1.md), including their inverse combinations. |
| [`QuadraticGravityPropagator[mu, nu, alpha, beta, p, m0, m2]`](Models/QuadraticGravity/QuadraticGravityPropagator.md) | Quadratic-gravity propagator with conventional gauge fixing, using the current [`GaugeFixingEpsilon`](./Parameters/GaugeFixing/GaugeFixingEpsilon.md). |
| [`QuadraticGravityPropagatorHD[mu, nu, alpha, beta, p, m0, m2]`](Models/QuadraticGravity/QuadraticGravityPropagatorHD.md) | Quadratic-gravity propagator with higher-derivative gauge fixing, using all three HD gauge parameters. |
| [`GravitonPropagatorCR[mu, nu, alpha, beta, p]`](Models/GeneralRelativity/CheungRemmen/GravitonPropagatorCR.md) | Cheung–Remmen graviton propagator using the current [`GaugeFixingEpsilonCR`](./Parameters/GaugeFixing/GaugeFixingEpsilonCR.md), initially `-1/2`. |
| [`GravitonPropagatorAuxiliaryCR[lambda1, mu1, nu1, lambda2, mu2, nu2]`](Models/GeneralRelativity/CheungRemmen/GravitonPropagatorAuxiliaryCR.md) | Algebraic auxiliary-field propagator in Cheung–Remmen variables. Each group of three indices labels one auxiliary field; there is no momentum argument. |

For both quadratic-gravity propagators, `m2` is the massive spin-2 pole parameter. `m0` equals the scalar pole mass in four dimensions. The implemented scalar pole mass squared in D dimensions is

$$m_s^2=\frac{3(D-2)m_0^2m_2^2}{(D-4)m_0^2+2(D-1)m_2^2}.$$

The parameter denominators and projector denominators must be treated symbolically before taking singular limits. See [gauge parameters](#Gauge%20parameters%20and%20coupling) for when their values are applied.

<a name="Interaction vertices"></a>

## Interaction vertices

### Scalars, fermions and vectors

The listed libraries are loaded through order 2 during successful package initialisation. For an order `n`, supply `2 n` entries in `pairs` or `3 n` in `triples`, as specified. Higher orders require the corresponding files and importer call. These signatures have no optional arguments.

| Command | Arguments and returned interaction | Library preparation |
| --- | --- | --- |
| [`GravitonScalarVertex[pairs, p1, p2, m]`](Models/Scalar/GravitonScalarVertex.md) | Two incoming scalar momenta and their mass; minimal scalar coupling to `n` gravitons. | [`FeynGravImportScalars[n]`](Services/LibraryManagement/FeynGravImportScalars.md) |
| [`GravitonScalarPotentialVertex[pairs, lambda]`](Models/Scalar/GravitonScalarPotentialVertex.md) | Scalar-potential vertex with the supplied potential coupling; no scalar momentum arguments. The convention is the potential monomial `lambda phi^r/r!`. | [`FeynGravImportScalars[n]`](Services/LibraryManagement/FeynGravImportScalars.md) |
| [`GravitonFermionVertex[triples, p1, p2, m]`](Models/Fermion/GravitonFermionVertex.md) | Incoming fermion and antifermion momenta, then mass; returns a Dirac-matrix expression. | [`FeynGravImportFermions[n]`](Services/LibraryManagement/FeynGravImportFermions.md) |
| [`GravitonMassiveVectorVertex[pairs, lambda1, p1, lambda2, p2, m]`](Models/Vector/GravitonMassiveVectorVertex.md) | Each vector is specified by index then momentum, followed by their mass. | [`FeynGravImportVectors[n]`](Services/LibraryManagement/FeynGravImportVectors.md) |
| [`GravitonVectorVertex[triples, lambda1, p1, lambda2, p2]`](Models/Vector/GravitonVectorVertex.md) | Massless-vector vertex; depends on the vector gauge value used during import. | [`FeynGravImportVectors[n]`](Services/LibraryManagement/FeynGravImportVectors.md) |
| [`GravitonVectorGhostVertex[pairs, p1, p2]`](Models/Vector/GravitonVectorGhostVertex.md) | Two incoming momenta of the massless-vector Faddeev–Popov ghosts. | [`FeynGravImportVectors[n]`](Services/LibraryManagement/FeynGravImportVectors.md) |

<a name="Graviton self-interactions and gravitational ghosts"></a>

### Graviton self-interactions and gravitational ghosts

| Command | Arguments and returned interaction | Availability |
| --- | --- | --- |
| [`GravitonVertex[mu1, nu1, p1, ..., mur, nur, pr]`](Models/GeneralRelativity/GravitonVertex.md) | `r >= 3` graviton legs supplied as separate arguments, **not a list**. Each leg has index–index–momentum order. | [`FeynGravImportGravitons[r-2]`](Services/LibraryManagement/FeynGravImportGravitons.md); three- and four-leg vertices are loaded initially. |
| [`QuadraticGravityVertex[triples, m0, m2]`](Models/QuadraticGravity/QuadraticGravityVertex.md) | `r >= 3` graviton legs in one flat list, followed by the two quadratic-gravity mass parameters. | [`FeynGravImportQuadraticGravity[r-2]`](Services/LibraryManagement/FeynGravImportQuadraticGravity.md); not loaded initially. |
| [`GravitonGhostVertex[rho, sigma, k, mu, p1, nu, p2]`](Models/GeneralRelativity/GravitonGhostVertex.md) | One graviton `(rho,sigma,k)` and two gravitational ghost legs `(mu,p1)`, `(nu,p2)`; all momenta incoming. | Direct definition, no extra import. |
| [`GravitonGhostVertexHD[rho, sigma, k, mu, p1, nu, p2]`](Models/QuadraticGravity/GravitonGhostVertexHD.md) | Same leg order with higher-derivative ghost gauge fixing; depends on [`GaugeFixingEpsilonHD0`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD0.md) and [`GaugeFixingEpsilonHD1`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD1.md). | Direct definition, no extra import. |

The ghost commands implement these fixed signatures; they do not generate arbitrary multi-graviton ghost vertices. Quadratic-gravity mass parameters have the meaning given in [Propagators](#Propagators).

### SU(N) Yang–Mills interactions

Call [`FeynGravImportSUNYM[n]`](Services/LibraryManagement/FeynGravImportSUNYM.md) first. Colour labels are adjoint indices; all momenta are incoming. Gluon and ghost argument orders differ, so use the explicit signatures below.

| Command | Arguments and returned interaction |
| --- | --- |
| [`GravitonGluonVertex[triples, p1, lambda1, a1, ..., pl, lambdal, al]`](Models/YangMills/GravitonGluonVertex.md) | `l = 2, 3, 4` gluons, each supplied as momentum–Lorentz-index–colour-index. Selects the two-, three- or four-gluon library by argument count. |
| [`GravitonQuarkGluonVertex[pairs, lambda, a]`](Models/YangMills/GravitonQuarkGluonVertex.md) | Quark–antiquark–gluon coupling; the gluon is specified by its Lorentz and colour indices. No momentum arguments. |
| [`GravitonYMGhostVertex[pairs, p1, a1, p2, a2]`](Models/YangMills/GravitonYMGhostVertex.md) | Two Yang–Mills ghost momenta and colour labels. |
| [`GravitonGluonGhostVertex[pairs, lambda1, a1, p1, lambda2, a2, p2, lambda3, a3, p3]`](Models/YangMills/GravitonGluonGhostVertex.md) | Gluon–ghost interaction in the existing library interface, with three index–colour–momentum groups. Retain all nine leg arguments, including the ghost-position Lorentz placeholders. |

These functions return the expressions stored in the selected libraries. The generator now preserves dimensional quark gamma matrices and named `SMP` couplings; migration did not regenerate distributed libraries. Do not assume newly generated and older stored expressions use identical colour or coupling notation. [`GaugeFixingEpsilonSUNYM`](./Parameters/GaugeFixing/GaugeFixingEpsilonSUNYM.md) is applied during import.

### Horndeski, axion and scalar–Gauss–Bonnet interactions

Read the [Horndeski model page](./Models/Horndeski/README.md) for the action, definition of X,
coupling normalisations and installed selections. Its corrected conventions are
the implementation-audit target. The [Scientific scope](./ScientificScope.md#Horndeski) and
[Scientific scope](./ScientificScope.md#Horndeski) identify the checked libraries and finite verification scope.

Each row requires its sector importer. There are no optional vertex arguments; coupling constants are explicit final arguments.

| Command | Scalar count and leg structure | Preparation |
| --- | --- | --- |
| [`HorndeskiG2[pairs, momenta, b, lambda]`](Models/Horndeski/HorndeskiG2.md) | `s = a + 2 b` scalar momenta; graviton indices only. | [`FeynGravImportHorndeskiG2[]`](Services/LibraryManagement/FeynGravImportHorndeskiG2.md) |
| [`HorndeskiG3[triples, momenta, b, lambda]`](Models/Horndeski/HorndeskiG3.md) | `s = a + 2 b + 1`; graviton triples. | [`FeynGravImportHorndeskiG3[]`](Services/LibraryManagement/FeynGravImportHorndeskiG3.md) |
| [`HorndeskiG4[triples, momenta, b, lambda]`](Models/Horndeski/HorndeskiG4.md) | `s = a + 2 b`; graviton triples. | [`FeynGravImportHorndeskiG4[]`](Services/LibraryManagement/FeynGravImportHorndeskiG4.md) |
| [`HorndeskiG5[triples, momenta, b, lambda]`](Models/Horndeski/HorndeskiG5.md) | `s = a + 2 b + 1`; graviton triples. | [`FeynGravImportHorndeskiG5[]`](Services/LibraryManagement/FeynGravImportHorndeskiG5.md) |
| [`GravitonAxionVectorVertex[pairs, lambda1, p1, lambda2, p2, theta]`](Models/AxionVector/GravitonAxionVectorVertex.md) | Two vector index–momentum groups and axion coupling `theta`; no axion momentum argument. | [`FeynGravImportAxionVectorVertex[n]`](Services/LibraryManagement/FeynGravImportAxionVectorVertex.md) |
| [`ScalarGaussBonnet[triples, g]`](Models/ScalarGaussBonnet/ScalarGaussBonnet.md) | `n >= 2` graviton legs, followed by the scalar–Gauss–Bonnet coupling; no separate scalar momentum argument. | [`FeynGravImportScalarGaussBonnet[n]`](Services/LibraryManagement/FeynGravImportScalarGaussBonnet.md) |

For the Horndeski monomial `lambda phi^a X^b`, `a` and `b` are non-negative integers. The library filename encodes `a`, `b` and graviton count `n`; the vertex call must match an installed triple. These wrappers do not automatically generate missing combinations. Generator-specific selection filters and rule limitations are documented separately.

Around flat space, each curvature begins at first order in the graviton perturbation. The scalar–Gauss–Bonnet interaction therefore starts at two gravitons. Its one-graviton contribution is zero, but the library interface requires at least two legs; it is not a command for returning that zero.

<a name="Cheung–Remmen vertices"></a>

### Cheung–Remmen vertices

These fixed D-dimensional definitions are available without additional libraries. They have no options and include the implemented factors of the gravitational coupling.

| Command | Leg order |
| --- | --- |
| [`GravitonVertexCRhhh[mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3]`](Models/GeneralRelativity/CheungRemmen/GravitonVertexCRhhh.md) | Three graviton index–index–momentum triples as separate arguments. |
| [`GravitonVertexCRBhh[alpha, rho, sigma, mu1, nu1, p1, mu2, nu2, p2]`](Models/GeneralRelativity/CheungRemmen/GravitonVertexCRBhh.md) | Three auxiliary-field indices, then two graviton triples. |
| [`GravitonVertexCRBBh[alpha1, rho1, sigma1, alpha2, rho2, sigma2, mu, nu]`](Models/GeneralRelativity/CheungRemmen/GravitonVertexCRBBh.md) | Two groups of auxiliary-field indices, then graviton indices. No momentum arguments. |

<a name="Polarisation objects"></a>

## Polarisation objects

These shortcuts require no vertex libraries. `p`, `mu` and `nu` must be standalone symbols. Their argument order begins with the **momentum**.

| Command | Dimension and result |
| --- | --- |
| [`PolarizationVectorD[p, mu, phase : I, opts]`](Services/Polarisation/PolarizationVectorD.md) | D-dimensional `Pair[LorentzIndex[mu,D], Momentum[Polarization[p,phase,...],D]]`. |
| [`PolarizationTensor[p, mu, nu, phase : I, opts]`](Services/Polarisation/PolarizationTensor.md) | Four-dimensional product of two polarisation vectors with the same momentum and phase. |
| [`PolarizationTensorD[p, mu, nu, phase : I, opts]`](Services/Polarisation/PolarizationTensorD.md) | The corresponding D-dimensional factorised product. |

Here `phase : I` indicates an optional positional argument with default `I`; call with `I` or `-I`, not that pattern notation. `-I` denotes complex conjugation of the vector. These labels are neither multiplicative factors nor helicity labels.

The sole option is `Transversality -> False`. Omitting it leaves the internal `Polarization` object without an explicit transversality option; an explicit option is forwarded. `Transversality -> True` imposes only the momentum–polarisation contraction being zero. It does not impose mass shell, the null-vector condition or normalisation.

For nonzero massless on-shell momentum, the factorised spin-2 tensor is transverse and traceless when its underlying vector is transverse and null. The D-dimensional shortcut does not construct a complete polarisation basis. Wrong patterns can remain unevaluated rather than producing `Failure`.

```mathematica
PolarizationTensorD[p, mu, nu, -I, Transversality -> True]
```

FeynCalc supplies the four-dimensional `PolarizationVector[p, mu, ...]`; it is not a new FeynGrav function.

<a name="Projectors and operators"></a>

## Projectors and operators

These public helpers are loaded from `Nieuwenhuizen`. All tensor objects are D-dimensional; the package retains the fixed `1/3` coefficients in [`NieuwenhuizenOperator0`](./Services/Projectors/NieuwenhuizenOperator0.md) and [`NieuwenhuizenOperator2`](./Services/Projectors/NieuwenhuizenOperator2.md). Do not replace them by `1/(D-1)` when interpreting this interface: the implemented basis and inverse use these conventions.

The transverse and longitudinal tensors are respectively `theta = eta - p p/p^2` and `omega = p p/p^2`. Their implementation uses FeynCalc propagator notation for inverse momentum squares. No singularity check at `p^2 = 0` is performed.

| Command | Returned object |
| --- | --- |
| [`GaugeProjector[mu, nu, p]`](Services/Projectors/GaugeProjector.md) | `theta(mu,nu)`. |
| [`GaugeProjectorBar[mu, nu, p]`](Services/Projectors/GaugeProjectorBar.md) | `omega(mu,nu)`. |
| [`NieuwenhuizenOperator1[mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator1.md) | Half the sum of the four mixed `theta omega` terms. |
| [`NieuwenhuizenOperator2[mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator2.md) | Symmetrised transverse identity minus `theta(mu,nu) theta(alpha,beta)/3`. |
| [`NieuwenhuizenOperator0[mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator0.md) | `theta(mu,nu) theta(alpha,beta)/3`. |
| [`NieuwenhuizenOperator0Bar[mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator0Bar.md) | `omega(mu,nu) omega(alpha,beta)`. |
| [`NieuwenhuizenOperator0BarBar[mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator0BarBar.md) | `theta(mu,nu) omega(alpha,beta) + omega(mu,nu) theta(alpha,beta)`, without an extra square-root normalisation. |
| [`NieuwenhuizenOperator[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperator.md) | Linear combination in exactly this coefficient order: `1, 2, 0, 0Bar, 0BarBar`. |
| [`NieuwenhuizenOperatorInverse[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperatorInverse.md) | The implemented D-dependent inverse in the same basis, assuming its scalar denominators are nonzero. Singular cases are not diagnosed automatically. |
| ``Nieuwenhuizen`Private`NieuwenhuizenSymmetryCheck[T, mu, nu, alpha, beta, p]`` | Internal helper used by [`NieuwenhuizenOperatorExpansion`](./Services/Projectors/NieuwenhuizenOperatorExpansion.md), not an exported command. Checks both within-pair swaps and exchange of the two pairs; requires usable FORM. |
| [`NieuwenhuizenOperatorExpansion[T, mu, nu, alpha, beta, p]`](Services/Projectors/NieuwenhuizenOperatorExpansion.md) | Returns `{z1,z2,z0,zb,zbb}` only after symmetry checks, coefficient extraction and complete reconstruction verification; requires usable FORM. |

There are no `LongForm` options. Projector constructors accept symbolic expressions rather than lists or associations for each argument. Structural errors return `Failure`, and incoming dependency failures propagate. Expansion is limited to tensors representable by this five-element basis with scalar coefficients. Unsupported residual tensor structures, inconclusive verification and calculation failures have distinct diagnostics. Check `FailureQ` before using coefficients; a failed FORM call is not a failed physical symmetry identity.

<a name="Library importers"></a>

## Library importers

**Importer naming update:** the former `import…` commands are now `FeynGravImport…`, retaining their suffixes and argument signatures. Replace `printOutput` (including its old private form) with [`Verbose`](./Services/Options/Verbose.md). Old names are not compatibility aliases. Restart the kernel after updating to avoid retaining previous definitions. Imports are silent by default; use, for example, [`FeynGravImportScalars[2, Verbose -> True]`](Services/LibraryManagement/FeynGravImportScalars.md) to print diagnostics. [`FeynGravLibraryInformation`](./Services/LibraryManagement/FeynGravLibraryInformation.md) accepts the renamed symbols and reports their fully qualified names. Historical reports and saved notebook outputs retain their original spelling.

Imports read local extensionless files; they neither download nor generate them. Successful calls normally return `Null` after installing definitions. They use `Verbose -> False` by default. Set `Verbose -> True` to print inventory and progress messages. Unknown options and non-Boolean values return `Failure`.

For order-limited importers, omitted `n` defaults to 2. Except for Gauss–Bonnet, `n` must be a positive integer. The requested maximum is capped by the installed families, but missing intermediate files cause failure. Failed reads or invalid orders preserve previously installed definitions. A successful lower-order reimport replaces the family's definitions through the selected order; it does not retain previously loaded higher orders.

| Command | Files and order convention |
| --- | --- |
| [`FeynGravImportGravitons[n : 2, opts]`](Services/LibraryManagement/FeynGravImportGravitons.md) | `GravitonVertex_j`, `1 <= j <= n`; `j + 2` graviton legs. Loaded initially with 2. |
| [`FeynGravImportScalars[n : 2, opts]`](Services/LibraryManagement/FeynGravImportScalars.md) | Scalar kinetic and potential files for 1 through `n` graviton pairs. Loaded initially with 2. |
| [`FeynGravImportFermions[n : 2, opts]`](Services/LibraryManagement/FeynGravImportFermions.md) | Fermion files for 1 through `n` graviton triples. Loaded initially with 2. |
| [`FeynGravImportVectors[n : 2, opts]`](Services/LibraryManagement/FeynGravImportVectors.md) | Massive vector, massless vector and vector ghost files through `n`. Loaded initially with 2. |
| [`FeynGravImportSUNYM[n : 2, opts]`](Services/LibraryManagement/FeynGravImportSUNYM.md) | Six Yang–Mills file families: two-, three- and four-gluon, quark–gluon, ghost and gluon–ghost. |
| [`FeynGravImportAxionVectorVertex[n : 2, opts]`](Services/LibraryManagement/FeynGravImportAxionVectorVertex.md) | Axion–vector files through `n` graviton pairs. |
| [`FeynGravImportQuadraticGravity[n : 2, opts]`](Services/LibraryManagement/FeynGravImportQuadraticGravity.md) | `QuadraticGravityVertex_j`, where `j + 2` is the number of graviton legs. |
| [`FeynGravImportScalarGaussBonnet[n : 2, opts]`](Services/LibraryManagement/FeynGravImportScalarGaussBonnet.md) | Files for 2 through `n` graviton triples; requires `n >= 2`. |
| [`FeynGravImportHorndeskiG2[opts]`](Services/LibraryManagement/FeynGravImportHorndeskiG2.md) | All available G2 files; no order argument. |
| [`FeynGravImportHorndeskiG3[opts]`](Services/LibraryManagement/FeynGravImportHorndeskiG3.md) | All available G3 files; no order argument. |
| [`FeynGravImportHorndeskiG4[opts]`](Services/LibraryManagement/FeynGravImportHorndeskiG4.md) | All available G4 files; no order argument. |
| [`FeynGravImportHorndeskiG5[opts]`](Services/LibraryManagement/FeynGravImportHorndeskiG5.md) | All available G5 files; no order argument. |

`n : 2` above describes the default; actual calls use [`FeynGravImportScalars[]`](Services/LibraryManagement/FeynGravImportScalars.md) or [`FeynGravImportScalars[2]`](Services/LibraryManagement/FeynGravImportScalars.md). Horndeski filenames have the form `HorndeskiG2_a_b_n`, and the installed files determine the accepted vertex signatures. Use `Verbose -> True` to inspect that selection. Avoid assuming every requested model/order is distributed.

<a name="Library import records"></a>

## Library import records

[`FeynGravLibraryInformation[]`](Services/LibraryManagement/FeynGravLibraryInformation.md) returns an association keyed by fully qualified importer names. [`FeynGravLibraryInformation[FeynGravImportVectors]`](Services/LibraryManagement/FeynGravLibraryInformation.md) selects the last successful vector import; pass the importer symbol without `[]`. Unknown importers or malformed query arguments return `Failure`. Families with no recorded import return `Missing["NotRecorded"]`.

Every successful import, including initialisation, publishes its record together with the new vertex definitions. A failed or aborted import preserves both. Reimporting replaces the record for that importer, so it describes the currently installed import rather than accumulating a history. Package reloads preserve records for optional families that are not reimported.

| Field | Meaning |
| --- | --- |
| `Importer`, `Targets` | Fully qualified importer and vertex-function names. |
| `Orders` | Actual orders loaded, grouped by library filename family; these may be fewer than requested. |
| `Files` | Absolute path, SHA-256 hash, filename family, order and complete filename indices for each loaded file. Horndeski indices retain `{a,b,n}`. |
| `ImportedAt` | UTC `DateObject` recorded on successful import. |
| `Settings` | Held snapshots of the relevant settings when importing. |
| `CurrentSettings` | Corresponding settings when the query runs. |
| `ChangedSettings` | Names of settings whose current values differ from their import snapshots. |
| `GenerationSettings` | `Missing["NotRecorded"]`: historical library-generation settings cannot be recovered from these files. |

Snapshots cover `FeynGrav`'s gravitational coupling and [`D`](./Parameters/Conventions/D.md) for every importer, the vector/Yang–Mills gauge parameter for its corresponding importer, and `$LeviCivitaSign` for axion–vector imports. They describe the import environment, not necessarily a nontrivial dependence of every individual vertex. Propagator-only gauge parameters do not enter these records. Arbitrary user-defined FeynCalc scalar products and transformation rules are not captured.

Values are wrapped in `HoldComplete` to prevent an unassigned symbol in an old record from acquiring a later assignment. Do not release that hold to reconstruct past settings: it allows evaluation in the current session.

```mathematica
information = FeynGravLibraryInformation[FeynGravImportVectors];
information["Settings"]
information["ChangedSettings"]
```

For example, if vectors were imported with `GaugeFixingEpsilonVector = -1`, then changing it to `-2` makes `ChangedSettings` identify that parameter. Reimport the required vector order to update the loaded definitions and record together. A change indicates a difference in settings, not a proof that a particular expression has changed.

The query reads only in-memory records and settings: it does not reopen files, recalculate hashes, import libraries or launch FORM. Hashes identify the files as read, not their contents at query time. The importer checks hashes before and after reading and rejects a detected change. Import records do not track manual vertex redefinitions or expressions computed earlier by the user. Successful queries return associations; they do not print a conventions report. See the [Library imports](./Services/LibraryManagement/README.md) for the tested scope.

<a name="Gauge parameters and coupling"></a>

## Gauge parameters and coupling

These are public symbols, not functions or options. Initialisation assigns or clears them as below. They require no additional library to exist.

| Symbol | Initial value | Where the value is used |
| --- | --- | --- |
| [`GaugeFixingEpsilon`](./Parameters/GaugeFixing/GaugeFixingEpsilon.md) | `2` | Conventional gravitational propagators when called. |
| [`GaugeFixingEpsilonCR`](./Parameters/GaugeFixing/GaugeFixingEpsilonCR.md) | `-1/2` | Cheung–Remmen graviton propagator when called. |
| [`GaugeFixingEpsilonHD`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD.md) | Unassigned | Overall higher-derivative graviton gauge contribution. |
| [`GaugeFixingEpsilonHD0`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD0.md) | Unassigned | HD graviton/ghost propagators and ghost vertex. |
| [`GaugeFixingEpsilonHD1`](./Parameters/GaugeFixing/GaugeFixingEpsilonHD1.md) | Unassigned | HD longitudinal coefficient in those propagators and ghost vertex; represents the surviving longitudinal combination in the package convention. |
| [`GaugeFixingEpsilonVector`](./Parameters/GaugeFixing/GaugeFixingEpsilonVector.md) | `-1` | Massless-vector libraries during [`FeynGravImportVectors`](./Services/LibraryManagement/FeynGravImportVectors.md). Set before reimporting to change imported dependence. |
| [`GaugeFixingEpsilonSUNYM`](./Parameters/GaugeFixing/GaugeFixingEpsilonSUNYM.md) | `-1` | Yang–Mills libraries during [`FeynGravImportSUNYM`](./Services/LibraryManagement/FeynGravImportSUNYM.md). Set before importing. |
| ``FeynGrav`\[Kappa]`` | Symbolic | Gravitational coupling appearing in package expressions. The corresponding Unicode symbol is `κ`. |

Changing a gauge parameter after a library import cannot restore symbolic dependence already replaced during that import. Restarting or reloading can reset initialised defaults. These symbols themselves perform no parameter validation or singularity diagnosis.

<a name="Command discovery and initialisation"></a>

## Command discovery and initialisation

- [`FeynGravCommands[]`](Services/PackageInformation/FeynGravCommands.md) prints the main command list and returns `Null`; it is a discovery aid, not an exhaustive list of every dependency export.
- `?FunctionName` shows the installed usage message. `Options[FunctionName]` inspects supported options. This Markdown reference is not native F1 help.
- [`DisplayInitializationMessages[]`](Services/PackageInformation/DisplayInitializationMessages.md) reprints the introduction and returns `Null`; it does not import libraries or check FORM.
- `FeynGravInitialized` is the package's initialisation state flag. Successful required imports set it to `True`; initialisation failures leave it false and report `InitializationFailed`. Treat it as state to inspect, not a configuration switch to set manually.

Main-package loading imports default libraries and evaluates definitions but starts no FORM process. The separate generator loads rule implementations with some identical short names: use a **different fresh kernel** for generation to avoid shadowing the library-backed interface.

<a name="FORM commands"></a>

## FORM commands

These are available after main-package loading; the [converter guide](./Services/Calculation/Guide.md#Command%20and%20option%20reference) is the authoritative option and failure reference.

| Command | Return and dependency |
| --- | --- |
| [`CalcFormExport[expr, file, opts]`](Services/Calculation/CalcFormExport.md) | Job-path association or `Failure`; writes source/mapping, without executing FORM. |
| [`CalcFormImport[resultFile, mappingFile]`](Services/Calculation/CalcFormImport.md) | Reconstructed FeynCalc expression or `Failure`; no executable needed. |
| [`CalcFormCheck[opts]`](Services/Calculation/CalcFormCheck.md) | Availability association or an option/setup `Failure`; may launch a small probe. |
| [`CalcFormInstall[opts]`](Services/Calculation/CalcFormInstall.md) | Explicit installation workflow on supported systems; never invoked implicitly. |
| [`CalcFormCalculate[expr, opts]`](Services/Calculation/CalcFormCalculate.md) | Algebraic result, equality result or `Failure`; requires usable FORM/TFORM. |

The default worker setting is `FORMThreads -> Automatic`. Supported Dirac algebra is enabled by `DiracAlgebra -> Automatic`; fundamental SU(N) reduction by `ColourAlgebra -> True`. Disabling either selects preservation for that space, subject to the converter's vocabulary restrictions. With one explicitly declared loop momentum, `TensorDecomposition -> True` (the default) returns integral coefficient functions with FeynCalc's `I Pi^2` normalisation. `DropScaleless -> False` preserves formal scaleless terms. Empty loop metadata or disabled decomposition retains integrand algebra. Default `ReduceToScalarMasters -> True` reduces supported tensor coefficients through boxes to rank-zero PaVe masters and includes necessary power reduction. False retains coefficient functions. Saved coefficients can be reduced without a new integration variable. Exceptional cases remain explicit and are reported as partial reduction. Masters are not analytically evaluated; see the [converter guide](./Services/Calculation/Guide.md#One-loop%20tensor%20decomposition).


<a name="Simplified calculation front end (development, unreleased)"></a>

### Simplified calculation front end (development, unreleased)

Five short facade commands are supplied as a convenient public front end to the
converter pipeline and, without any fallback, to a restricted benchmarking
adapter. They are implemented in `FeynGravSimplify/` and described in
[Calculation interface](./Services/Calculation/FeynGravSimplify.md). They are
planned for [4.2.0](./ReleaseNotes-4.2.0.md), which remains unreleased, and are
not part of the published 4.1.0 interface.

- [`FeynGravSimplify[expr, opts]`](Services/Calculation/FeynGravSimplify.md) — one-call simplified calculation. [`Method`](./Services/Options/Method.md) is
  the Wolfram System option symbol: `Automatic` (default) and `"FORM"` dispatch to the full
  converter pipeline ([`CalcFormCalculate`](./Services/Calculation/CalcFormCalculate.md) semantics, no [`Method`](./Services/Options/Method.md) forwarding);
  `"Contractor"` and `"FORMBenchmark"` dispatch to the restricted external
  adapter. There is no automatic backend fallback. `PaVeToABCD -> True` converts
  supported final PaVe functions to named functions; `False` preserves prior
  output. Both settings are no-ops on restricted methods. See the
  [conversion contract](./Services/Calculation/Guide.md#Final%20PaVe%20conversion%20%28%60FeynGravSimplify%60%20only%29).
- [`FeynGravFORMExport[expr, file, opts]`](Services/Calculation/FeynGravFORMExport.md), [`FeynGravFORMImport[resultFile, mappingFile]`](Services/Calculation/FeynGravFORMImport.md),
  [`FeynGravFORMCheck[opts]`](Services/Calculation/FeynGravFORMCheck.md) and [`FeynGravFORMInstall[opts]`](Services/Calculation/FeynGravFORMInstall.md) are wrappers over
  the corresponding `CalcForm*` commands, each with its own option store and
  otherwise identical semantics; see `CalcFormConverter/UserGuide.md`.

All five commands own their option stores in the ``CalcFormConverter` `` context and accept
explicit call options over configured defaults. Restricted methods accept only
the fixed capability baseline and refuse other full-pipeline settings with
`MethodCapability`. [`CalcFormCalculate`](./Services/Calculation/CalcFormCalculate.md) and all `CalcForm*` commands are
unchanged and remain the published interface. See
[the facade reference block in the converter guide](./Services/Calculation/Guide.md#Simplified%20front%20end%20%28development%29)
for the full option and failure contract.


### Reducing repeated-power loop coefficients

[`CalcFormCalculate[answer, ReduceLoopCoefficients -> True]`](Services/Calculation/CalcFormCalculate.md) can reduce saved
[`LoopTensorCoefficient`](./Services/Calculation/LoopTensorCoefficient.md) expressions without declaring [`LoopMomenta`](./Services/Options/LoopMomenta.md) or
repeating their original tensor calculation. The option defaults to `False`.
It also applies after active one-loop decomposition. Supported nonsingular
families reduce to ordinary unit-power PaVe coefficients; this is not analytic
evaluation. Scaleless terms remain unless `DropScaleless -> True` is specified.
See the [converter guide](./Services/Calculation/Guide.md#Optional%20reduction%20of%20repeated%20powers)
for topology limits, singular cases and normalisation.
## Native scalar extensions — 18 October 2026

`Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet` are supplied alongside `GR`, with matching generic/classes files in individual model folders. Use a fresh kernel and the same name for `Model` and `GenericModel`. The optional pure-GR bridge remains restricted to its existing processes. See [model index](./Models/FeynArts.md) for actions, native examples and verification limits. The two-graviton EsGB source and library have been corrected against an independent curvature derivation; higher-order EsGB sources require a separate audit. Vertex order is limited to kappa squared; potential contacts still require up to six legs. Loops are not validated.

The native EsGB model uses unassigned [`GaussBonnetG1`](./Parameters/ScalarGaussBonnet/GaussBonnetG1.md) and [`GaussBonnetG2`](./Parameters/ScalarGaussBonnet/GaussBonnetG2.md) for the positive terms phi G and phi^2 G / 2!, respectively. [`ScalarMass`](./Parameters/Scalar/ScalarMass.md) remains independent. Select `Model -> "Einstein-scalar-Gauss-Bonnet"` and `GenericModel -> "Einstein-scalar-Gauss-Bonnet"`; specialise parameters by replacement after conversion. Two TTS vertices generate g1-squared diagrams of total order kappa^4 even though each vertex is at most order kappa^2. The [model guide](./Models/ScalarGaussBonnet/NativeModel.md) gives the exact action and native recipe.

`GR-SU-N-Yang-Mills` contains SU(N) Yang–Mills and one fundamental Dirac quark
flavour with symbolic [`SUNN`](./Parameters/Conventions/SUNN.md), unassigned [`YangMillsCoupling`](./Parameters/YangMills/YangMillsCoupling.md) and [`QuarkMass`](./Parameters/YangMills/QuarkMass.md).
`F[1]`/`-F[1]` are quark/antiquark, `S[1]`/`-S[1]` the vector gravitational
ghosts (formerly `F`), and `U[1]`/`-U[1]` the scalar YM ghosts. Both gauges are
fixed (GR 2, YM −1). The [model guide](./Models/YangMills/NativeModel.md)
documents its 21 families, colour/fermion conventions and native workflow.
See the [Scientific scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions), including
selected symbolic-D/mass Ward checks and the FORM example. Native loop validation
remains deferred. The previous [Scientific scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions)
for mixed gluon κ powers is retained separately.

## Citation guidance

[`FeynGravHowToCite[]`](Services/PackageInformation/FeynGravHowToCite.md) prints the four FeynGrav references with links, followed by a reminder to cite dependencies when used, and returns `Null`. Cite the publications relevant to the version and features used. Notebook references are clickable; a kernel without a front end prints citation text and URLs. No libraries, settings, network requests or calculations are involved. Any supplied arguments return `Failure["InvalidArgumentCount", ...]`. The command appears in [`FeynGravCommands[]`](Services/PackageInformation/FeynGravCommands.md) and in startup help.

The initialisation banner identifies the default import requests: cubic/quartic gravity and matter interactions with up to two gravitons. It lists the six supported FeynArts models by their descriptive names, without initialising them. The banner uses FeynCalc-style text with automatic wrapping and links to the help commands; publication references are shown by [`FeynGravHowToCite[]`](Services/PackageInformation/FeynGravHowToCite.md). Actual import failures remain reported by the existing initialisation diagnostics; consult [`FeynGravLibraryInformation[]`](Services/LibraryManagement/FeynGravLibraryInformation.md) for successful imports.
