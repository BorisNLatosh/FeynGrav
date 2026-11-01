# Getting started with FeynGrav

[Documentation index](README.md) · [Common conventions](Conventions.md)

**This manual describes the 4.2.0 development checkout.** It includes features
beyond the published 4.1.0 interface. Use the documentation supplied with your
checkout; a release tag preserves the manual for that version. See the
[draft release notes](ReleaseNotes-4.2.0.md) for the current development scope.

FeynGrav supplies gravitational Feynman rules as symbolic FeynCalc expressions
in Mathematica. You can use its propagators and vertices to construct diagrams,
use supplied FeynArts models to generate selected processes, and simplify
supported expressions with `FeynGravSimplify` and FORM.

Begin with the short session below, then read the
[common conventions](Conventions.md) before assembling an amplitude.

## Install and load the package

Follow the [requirements and installation guide](UserGuide.md#requirements)
if FeynGrav is not installed. FeynCalc is required. FORM is needed when you
execute a FORM calculation; it is not needed for the short session below.
FeynArts is required only for workflows that use it.

The default installer selects the current `main` branch. It does not select
the latest release automatically or install unmerged changes from another
development branch. The installation guide also explains how to select a
specific version.

Start a fresh Mathematica kernel and evaluate:

```mathematica
<< FeynGrav`
```

FeynGrav loads FeynCalc when needed and imports the default graviton, scalar,
fermion and vector libraries through their documented order 2. Additional
sectors require their own library imports; consult the
[library guide](UserGuide.md#interaction-libraries).

If you plan to use FeynArts, start instead with the
[native-model loading instructions](../FeynArts/Models/ModelCatalogue.md#common-loading-workflow)
in a fresh kernel. They arrange for FeynArts to be loaded before FeynGrav.

## Retrieve a propagator and an interaction vertex

With `p`, `p1`, `p2` and `m` unassigned, evaluate:

```mathematica
ScalarPropagator[p, m]
```

This returns the scalar propagator `I FAD[{p, m}]` in FeynCalc notation.
`m` is the mass, rather than its square. The factor of `I` is already included.

Now retrieve the interaction of one graviton with two massive scalar fields:

```mathematica
vertex = GravitonScalarVertex[{\[Mu], \[Nu]}, p1, p2, m];
vertex
```

The pair `{\[Mu], \[Nu]}` contains the graviton's Lorentz indices, and `p1`
and `p2` are incoming scalar momenta. The result retains symbolic `D`, mass
and gravitational coupling. This command returns a vertex; it does not build
a complete scattering amplitude. The semicolon stores the result without
printing it, and the following line displays it.

For a complete calculation, choose a notebook from the
[numbered examples](../Examples/README.md). Each example lists its additional
requirements. Read its normalisation and kinematics before adapting it.

## Inspect conventions and command help

```mathematica
FeynGravConventions[]
```

This prints the fixed conventions together with relevant current settings and
loaded-library information. It changes no settings. The
[common-conventions page](Conventions.md) explains their mathematical meaning.

```mathematica
FeynGravCommands[]
```

Use this list to discover the interface. Evaluate a help query in a separate
input cell:

```mathematica
?GravitonScalarVertex
```

The same `?FunctionName` syntax works for other commands, such as
`?FeynGravSimplify`. The [command reference](Reference.md) gives signatures, options
and restrictions. An unavailable vertex can remain unevaluated; that does
not mean the interaction is zero.

## Choose your next page

| What you want to do | Read next |
| --- | --- |
| Set up a calculation | [Installation, libraries and troubleshooting](UserGuide.md) |
| Understand signs, dimensions and normalisations | [Common conventions](Conventions.md) |
| Find rules for a physical model | [Model index](Models.md), starting with [general relativity](Models/GeneralRelativity.md) |
| Look up a command, option or parameter | [Command reference](Reference.md) |
| Construct and simplify an amplitude | [Example notebooks](../Examples/README.md); [FeynGravSimplify guide](../CalcFormConverter/UserGuide.md#simplified-front-end-development) |
| Extend rules, libraries or calculation tools | [Rule-generation contracts](../Rules/Contracts.md), [library generation](../Libs/Generator.md) and [converter development](../CalcFormConverter/DEVELOPER.md) |

## Interpret a result

Rules include their prescribed couplings and factors of `I`. When assembling
diagrams manually, you must supply the appropriate diagram weights, closed-loop
signs and integration normalisation. When using FeynArts, account for the
factors it already supplies. See [diagram and loop conventions](Conventions.md#diagrams-and-loop-integrals).

Keep symbolic `D` unless your calculation calls for a specific dimensional
prescription. On-shell conditions, helicity assignments and renormalisation
are separate steps. Use a fresh kernel for independent examples: scalar-product
assignments and other settings persist between evaluations.

Model guides state their supported interactions and verified scope. A passing
example or a recorded tree-level check does not establish every process or
loop order. Dated verification reports are linked from the
[documentation index](README.md#verification-and-history).

For citation instructions, evaluate `FeynGravHowToCite[]` or read
[citations and licence](UserGuide.md#citations-and-licence).

**Continue with [Common conventions](Conventions.md).**
