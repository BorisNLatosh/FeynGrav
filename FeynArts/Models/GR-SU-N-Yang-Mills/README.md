# GR coupled to SU(N) Yang–Mills and one quark flavour

Native bundle `GR-SU-N-Yang-Mills`, with arbitrary `QuarkMass`, symbolic D/N,
and all 21 interaction families through κ² per vertex.

- [Model guide](ModelGuide.md): action, field labels, gauges, inventory and use.
- [Higher orders](HigherOrderVertices.md): extending the finite snapshot.
- [Quark verification](../../../Documentation/Verification/GRSUNYMQuarks-2026-10-26.md)
  and the earlier [pure-YM audit](../../../Documentation/Verification/GRSUNYM-2026-10-26.md).
- [Example 12: gluon graviton emission](../../../Examples/12.%20Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb).
- [Example 13: quark annihilation with graviton emission](../../../Examples/13.%20Quark_Graviton_Emission_Tree_Level_FeynArts.nb),
  using FeynGravSimplify/FORM and symbolic-D Ward checks.
- [All models](../README.md).

Use matching `Model` and `GenericModel` names in a fresh kernel. `F[1]` now
means a quark; the gravitational vector ghost moved to `S[1]`. The standalone
model files require neither source libraries nor FORM. Example 13 requires FORM.
Loop validation remains deferred.
