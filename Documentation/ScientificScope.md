# Scientific scope and limitations

[Documentation index](./README.md) · [Models](./Models.md) · [Conventions](./Conventions.md)

This page describes the scope of the 4.2.0 development checkout. Checks of
specified interactions do not establish every higher-order vertex, complete
amplitude or loop calculation. Library availability, source construction and
mathematical validation are distinct. Read the action and library inventory
on the model page before choosing a process.

Verification statements below summarise the recorded checks applicable to the
supplied interactions. They are separate from historical corrections and
upgrade instructions; a dated correction is not a verification date for the
whole checkout. Model checks also do not certify a notebook’s saved output.

<a name="General relativity"></a>

## General relativity

The ordinary gravitational ghost propagator is `-I MTD[mu,nu] FAD[p]` with the
ordered ghost convention in the [GR guide](./Models/GeneralRelativity/README.md).
The supplied native GR model contains cubic and quartic graviton interactions
and the cubic ghost coupling, through κ² per vertex. Selected native tree
checks establish their stated scope; native loops remain unvalidated.

The stored five-graviton Einstein–Hilbert vertex has an independent complete
off-shell comparison at symbolic D, including pair/leg symmetries and traces.
It is not part of the shipped native GR bundle. Its source check does not
establish a complete five-point amplitude or a practical runtime for such a process.
The Cheung–Remmen interface has separate conventions and does not inherit these checks.

<a name="Vertex multiplicities"></a>

## Vertex multiplicities

Metric-expansion tensors contain averaged coefficients. Constructing a vertex
requires the differentiation multiplicities of the labelled external fields;
terms already summed over those permutations must not receive another factorial.
The current scalar kinetic, mass and potential rules, and affected vector,
Yang–Mills, G2 and axion routines include the corrected factors. Public stored
vertices include their prescribed phase, couplings and multiplicities.

Earlier two-scalar/two-graviton results were smaller by a factor of two; vector
connection terms already carrying explicit permutations must not be uniformly
rescaled. Recompute affected old amplitudes from the corrected rules. These
corrections and regression comparisons do not independently validate every
model action, particularly the unresolved axion convention.

<a name="Scalar models"></a>

## Scalar models

The corrected minimal two-scalar/two-graviton vertex agrees with the canonical
action off shell, separately for kinetic and mass terms at symbolic D. The
former value was smaller by a factor of two. The minimal scalar, John and
Einstein–scalar–Gauss–Bonnet native models incorporate the corrected contact.
Selected scalar-to-two-graviton trees satisfy both outgoing graviton Ward
identities with symbolic D, mass and couplings, on shell and with momentum
conservation. This is a check of those processes, not all amplitudes.

The [John model](./Models/Horndeski/John/README.md) additionally has independent
cubic/quartic off-shell action comparisons. The EsGB two-graviton source has an
independent curvature derivation. **Higher-order EsGB sources and the stored
`ScalarGaussBonnet_3` are not independently validated.** The native EsGB model
uses the checked order-two source; the higher-order library is not automatically
rejected. Validate it separately before using it for physical predictions.

<a name="Horndeski"></a>

## Horndeski

Use the [Horndeski action and convention translation](./Models/Horndeski/README.md#Translation%20from%20the%20literature).
With signature (+---) and X without a half, the relative Hessian coefficients
are -2 G4,X and +G5,X/3. The [G4 derivation](./Models/Horndeski/G4Derivation.md)
explains their geometric expansion and differentiation multiplicities.

| Sector | Checked scope | Remaining limits |
| --- | --- | --- |
| G2/G3 | All shipped selections at one and two gravitons were compared with independent references; direct labelled-leg and index handling was corrected. | Finite symbolic-reduction checks do not certify all higher orders. |
| G4 | Both source variants were checked in specified one-/two-graviton cases; a complete n=3,b=2 comparison uses all source-seed permutations. All 14 shipped libraries and ordinary imports were independently checked. | The generic b=5 comparison remains incomplete; arbitrary orders and loop amplitudes are not certified. |
| G5 | All shipped selections at one/two gravitons and the complete n=3,a=0,b=1 source were checked. Higher-order comparisons include the complete n=4,a=0,b=1 vertex and selected three-connection sectors at n=4,b=2 and n=5,b=1. | No complete five-graviton or arbitrary-order runtime certification. |
| John | Equivalent G4/G5 representations agree for the checked one-/two-graviton interactions. | This does not validate general G4/G5 functions. |

The source supports only the finite inverse-metric bounds stated on the model
and constructor pages. All 60 shipped Horndeski libraries have recorded file
identities. Four G5 b=1 files and all 14 G4 files were corrected. Restart the
kernel and recompute results using those old libraries; no single global
rescaling repairs old amplitudes. Notebook outputs are not updated automatically.
See [scientific corrections](./ReleaseNotes-4.2.0.md#Horndeski%20scientific%20corrections).

<a name="Yang–Mills and fermions"></a>

## Yang–Mills and fermions

The [GR–SU(N) model](./Models/YangMills/README.md) supplies 21 interaction families
through κ² per vertex, symbolic D/N, one massive fundamental Dirac flavour and
both ghost species. Independent action comparisons cover the supplied contacts,
including the fermion kinetic, mass and spin-connection terms. Selected gluon
and massive-quark trees and their Ward checks pass within the model guide's
scope. General native loop amplitudes are not certified.

Missing κ powers were restored in mixed cubic/quartic gluon and quark–gluon
source rules and their one-/two-graviton libraries. Reimport and recompute
expressions using affected old rules. The flat quark–gluon vertex is unchanged.
Source and native spinor momentum conventions differ; follow the model guide's
conversion rather than combining their momenta directly.

<a name="Other interactions"></a>

## Other interactions

The axion epsilon-density/action convention remains unresolved. Corrected
multiplicities and explicit epsilon slots do not settle the metric dependence
of that interaction. The [axion page](./Models/AxionVector/README.md) states this limitation.

The [quadratic-gravity](./Models/QuadraticGravity/README.md),
[vector](./Models/Vector/README.md) and higher-order matter interfaces describe
the implemented rules. Structural validation and regression agreement do not
establish every action or amplitude independently. The [native quadratic-gravity model](./Models/QuadraticGravity/NativeModel.md)
reproduces the conventional-gauge propagator and verified off-shell cubic, quartic
and ghost rules. The complete massless four-graviton tree passed all-leg Ward and Einstein-limit checks at two exact four-dimensional kinematic points. The massive spin-2 command is a propagator
utility, not a complete interacting massive-gravity theory.

<a name="Calculation and performance"></a>

## Calculation and performance

`FeynGravSimplify` and the converter perform the supported algebra and one-loop
reductions described in the [calculation guide](./Services/Calculation/Guide.md).
Scalar-master conversion is not analytic evaluation, renormalisation or a
Laurent expansion. Singular, unsupported or preserved scaleless cases can remain
formal. Partial reduction does not mean that the unreduced coefficients vanish.

Exact comparisons include specified three-box, QED and scalar/gravitation
workloads. Complete triangle results agree in serial FORM and TFORM for the
checked symbolic kinematics, with all five scalar masters. These finite examples
do not guarantee maximal simplification, arbitrary-kinematics identities or
completion within a universal time or memory limit. The larger selected QED
full-tadpole and 1,490-term symbolic comparison remain unverified.
Use the [benchmark notebooks](./Benchmarks/README.md) for measurements on
your system; those measurements do not validate a physical model.

<a name="Examples and saved outputs"></a>

## Examples and saved outputs

The [per-notebook status table](./Examples/README.md#Verification%20and%20saved-output%20status) separates syntax/interface auditing, selected model or diagram checks, complete recorded kernel evaluations and saved-output provenance. Examples 01–11 retain historical outputs that were not refreshed during the calculation-interface migration. Examples 12 and 13 have recorded full kernel evaluations and saved results, but these do not certify a rerun against later package changes. Example 14 checks the three GR helicity expressions of example 04 for the complete massless quadratic-gravity tree. These symbolic checks passed after applying the physical scalar products during each FORM contraction. The separate [model-level Ward and GR checks](./Models/QuadraticGravity/NativeModel.md#Einstein%20limit%20and%20complete%20tree%20checks) cover the kinematic points specified there.

In particular, example 08 requires downstream recomputation with its corrected identical-graviton bubble weight. Examples 07, 09 and 11 retain outputs predating their input repairs. Restart the kernel and rerun affected calculations rather than treating a saved display or a model-level check as a fresh notebook result. The [benchmark guide](./Benchmarks/README.md#Verification%20and%20saved-output%20status) separately describes its notebooks’ saved timing output and run-specific validation.

<a name="Installation and compatibility"></a>

## Installation and compatibility

The [installer](./UserGuide.md#Installation%20and%20help) checks the selected archive,
dependencies and representative loading in fresh kernels before replacing an
installation. That check does not validate all interaction physics or certify
every supported operating system and Wolfram version. Consult the documented
requirements and known restrictions of each external calculation method.
