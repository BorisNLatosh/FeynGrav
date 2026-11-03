# General relativity in Cheung–Remmen variables

[General relativity](../README.md) · [Common conventions](../../../Conventions.md) · [Documentation home](../../../README.md)

**4.2.0 development interface.**

FeynGrav also exposes a separate GR parametrisation using a densitised inverse
metric and an auxiliary field. The original action and field redefinitions
are given in [FeynGrav 4.0, Section 3](https://arxiv.org/html/2510.17320v1#S3).
The current public commands are:

| Command | Object |
| --- | --- |
| [`GravitonPropagatorCR`](./GravitonPropagatorCR.md) | Propagating CR graviton |
| [`GravitonPropagatorAuxiliaryCR`](./GravitonPropagatorAuxiliaryCR.md) | Algebraic auxiliary-field propagator, six index arguments and no momentum |
| [`GravitonVertexCRhhh`](./GravitonVertexCRhhh.md) | Three CR gravitons |
| [`GravitonVertexCRBhh`](./GravitonVertexCRBhh.md) | One auxiliary field and two CR gravitons |
| [`GravitonVertexCRBBh`](./GravitonVertexCRBBh.md) | Two auxiliary fields and one CR graviton |

These are direct definitions, not imported conventional vertex libraries.
`GaugeFixingEpsilonCR` defaults to `-1/2`. Full argument orders are in the
[propagator reference](../../../Reference.md#Propagators) and
[CR vertex reference](../../../Reference.md#Cheung%E2%80%93Remmen%20vertices).
Their κ powers and normalisations must be read as defined; the conventional
$\kappa^{r-2}$ library-order rule is not an instruction to rescale CR commands.

There is no supplied CR FeynArts model and no dedicated public CR ghost-vertex
command in the current interface. In particular, the conventional
`GravitonGhostVertex` is not documented as a substitute for the CR ghost
interaction. The cubic bosonic commands alone do not establish a complete
gauge-fixed CR calculation. The standard `GR` model's source comparisons and
ghost checks do not validate CR normalisations or a mixed-parametrisation
amplitude. Use the [conventional formulation](../README.md#Native%20FeynArts%20model) for the verified native workflow.

## Loading and use

Load FeynGrav in a fresh kernel with symbolic `D` and κ. The five commands are available directly; conventional graviton imports do not supply CR vertices. Each command page gives the exact argument order and a retrieval example. Keep the returned coupling powers and phases unchanged.

```mathematica
<< FeynGrav`
```

<a name="Verification boundary"></a>

## Verification boundary

The [conventional-model verification](../README.md#Verification%20and%20limitations) is not a mathematical validation of this alternative formulation. These pages reorganise the existing interface descriptions; they add no CR amplitude or ghost-completeness claim.
