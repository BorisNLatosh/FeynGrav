# FeynGravCreateAmplitude

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude``. **Baseline:** 4.2.0 development.

Optional pure-GR companion interface. It is narrower than ordinary native FeynArts use with the supplied models.

## Syntax and arguments

```mathematica
FeynGravCreateAmplitude[model, process, incomingMomenta, outgoingMomenta]
FeynGravCreateAmplitude[model, process, incomingMomenta, outgoingMomenta, {loopMomentum}]
```

`model` is the bundle descriptor (`Directory`, `ModelName`, `Manifest`) shown in the bridge guide. `process` is the allowed pure-GR process association. The two lists supply matching, distinct external momentum symbols. The fifth-argument loop interface is retained historically but the corrected shipped model returns `GhostCompletenessGate` before topology generation.

## Loading

Use a fresh kernel with FeynCalc-patched FeynArts and load `FeynArtsIntegration/FeynArtsIntegration.wl` explicitly before native model initialisation. The companion is not loaded by ordinary FeynGrav initialisation. Loading its definitions alone starts no process, initialises no model and writes no bundle.

## Options

No options are advertised for this interface.

## Result and limits

Constructs complete three-/four-graviton trees, retaining per-diagram open-index FeynCalc expressions, graph IDs, raw FeynArts amplitudes and drawable insertions. It inserts no on-shell conditions, external wavefunctions, spin sums, loop measures or scalar-integral evaluation. Preserve the result association and diagram integrity metadata for subsequent algebra.

The corrected GR release supports the stated trees; loop compatibility remains deferred. General matter processes, higher-multiplicity trees and automatic model extension are outside this companion's scope. Use native model workflows for their separately documented supported sectors.

## Worked workflow and evidence

- [Complete bridge example](./Guide.md#Small%20explicit%20tree%20workflow)
- [Native GR guide](../../Models/GeneralRelativity/NativeModel.md)
- [Scientific scope](../../ScientificScope.md#General%20relativity)
- Source (`FeynArtsIntegration/FeynArtsIntegration.wl`)
