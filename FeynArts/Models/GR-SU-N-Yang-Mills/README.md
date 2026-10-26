# GR coupled to pure SU(N) Yang–Mills

Native bundle `GR-SU-N-Yang-Mills`, with symbolic D and N and interactions
through κ² per vertex. Quarks are not included.

- [Model guide](ModelGuide.md): action, gauges, two ghost species, all 16
  coupling families, loading and native calculations.
- [Higher orders](HigherOrderVertices.md): how to extend the finite snapshot.
- [Verification](../../../Documentation/Verification/GRSUNYM-2026-10-26.md):
  independent source audit, exact contacts and selected tree Ward identities.
- [Worked graviton-emission example](../../../Examples/12.%20Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb):
  four diagrams, on-shell amplitude and both gauge symmetries.
- [All models](../README.md)

Use the exact name for both `Model` and `GenericModel`, in a fresh kernel with
FeynCalc-patched FeynArts. The model files require no libraries or FORM run.
Loop validation remains deferred.
