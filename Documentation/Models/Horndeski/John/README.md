# John derivative coupling

[Horndeski overview](../README.md) · [Model index](../../../Models.md)

The native `John` model contains GR and one canonical massive scalar with the additional density

$$
\Delta\mathcal L=\beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi.
$$

There is no extra factor of one-half. `JohnBeta` is the coupling and `ScalarMass` is the scalar mass; keep both symbolic when loading. The metric, Fourier and curvature conventions are those of [Horndeski](../README.md#Fields%20and%20conventions).

## Commands and equivalent representations

There is no separate public `JohnVertex` command. Up to a boundary term, John is represented by either `G4 = -beta X/2` or `G5 = -beta phi`. Do not add both representations. The native model uses G5 with a = 1, b = 0 and coupling `-beta`.

In a fresh kernel, the stored one-graviton interaction can be retrieved with:

```mathematica
<< FeynGrav`
FeynGravImportHorndeskiG5[];
vertex = HorndeskiG5[{mu, nu, k}, {p1, p2}, 0, -beta];
```

See [HorndeskiG5](../HorndeskiG5.md) for the exact interface and [HorndeskiG4](../HorndeskiG4.md) for the alternative representation. This is a retrieval example, not a new verification result.

## Free fields and native model

Expansion is about flat spacetime and zero scalar background. The canonical GR and scalar propagators and gravitational ghosts are unchanged; the native bundle fixes conventional gravity gauge 2. Its cutoff is κ² per vertex. The standalone model files need no library generation during normal use.

The [native John guide](./NativeModel.md) contains the complete action, loading instructions, field content and finite vertex inventory. Example 10 (`Examples/10. Scalar_Scattering_John_Tree_Level_FeynArts.nb`) constructs scalar elastic scattering.

## Verification and limitations

See [the scalar-model checks](../../../ScientificScope.md#Scalar%20models) and [the relation to Horndeski](../../../ScientificScope.md#Horndeski). John checks apply to this derivative coupling and the stated orders; they do not certify general G4/G5 interactions or loops.

For the supplied FeynArts bundle, see the [native model guide](./NativeModel.md) and [order limits](./HigherOrders.md).
