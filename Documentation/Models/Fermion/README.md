# Minimally coupled Dirac fermion

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0 development. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

The field is a massive Dirac spinor coupled to a torsion-free metric through the vierbein and spin connection. The action convention used by the native quark model is

$$
S_\psi=\int d^Dx\sqrt{-g}\left[\frac{i}{2}\bar\psi e_a{}^\mu\gamma^a\overleftrightarrow{\nabla}_\mu\psi-m\bar\psi\psi\right].
$$

Use the symmetric vierbein expansion and the [common positive Fourier phase](../../Conventions.md). The direct vertex takes the incoming fermion momentum before the incoming antifermion momentum. Preserve gamma-matrix order. Its mass is an explicit argument, with a supported zero-mass value.

## Gauge fixing, libraries and native support

The ordinary gravitational gauge fixing and ghosts are those of GR. Initialisation loads fermion libraries through two attached gravitons. Use `FeynGravImportFermions[n]` for additional installed orders. FeynGrav does not export a separate fermion-propagator command.

There is no standalone native fermion bundle; one massive fundamental Dirac flavour appears in `GR-SU-N-Yang-Mills`. Native FeynArts uses a different spinor momentum routing; the guide below describes the conversion, including the graviton momenta in the spin-connection term.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonFermionVertex](./GravitonFermionVertex.md) | Incoming fermion and antifermion momenta, then mass; returns a Dirac-matrix expression. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](../YangMills/NativeModel.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The [Scientific scope](../../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions) records independent native action comparisons and their scope. It does not establish every higher-order fermion library or general loop amplitudes.
