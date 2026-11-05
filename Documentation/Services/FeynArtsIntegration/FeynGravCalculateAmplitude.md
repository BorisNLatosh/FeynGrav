# FeynGravCalculateAmplitude

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude``. **Baseline:** 4.2.0.

Optional pure-GR companion interface. It is narrower than ordinary native FeynArts use with the supplied models.

## Syntax and arguments

```mathematica
FeynGravCalculateAmplitude[constructedAmplitude, opts]
```

`constructedAmplitude` must be an intact tree result from [FeynGravCreateAmplitude](./FeynGravCreateAmplitude.md). Real FORM algebra verifies model provenance, the complete diagram set and integrity hashes. Only distinct immediate option rules from the documented whitelist are accepted.

## Loading

Use a fresh kernel with FeynCalc-patched FeynArts and load `FeynArtsIntegration/FeynArtsIntegration.wl` explicitly before native model initialisation. The companion is not loaded by ordinary FeynGrav initialisation. Loading its definitions alone starts no process, initialises no model and writes no bundle.

## Options

The forwarding whitelist is `ColourAlgebra`, `DiracAlgebra`, `Dimension`, `LoopMomenta`, `FORMExecutable`, `TimeConstraint`, `WorkingDirectory`, `KeepFiles`, `ShowTiming`, `ShowProgress` and `FORMThreads`, with their fully qualified converter/FeynCalc/System symbols. Nonempty `LoopMomenta` is refused. Omitted converter options use the loaded `CalcFormCalculate` defaults. Other converter options are not automatically accepted just because the converter supports them.

## Result and limits

Applies `CalcFormCalculate` sequentially to diagrams. Returns an association with per-diagram results, metadata, status and failed IDs. If any diagram fails or aborts, `Status` is `"Incomplete"` and `Total` is `Missing`, not a partial amplitude. This adapter offers tree integrand algebra only.

The corrected GR release supports the stated trees; loop compatibility remains deferred. General matter processes, higher-multiplicity trees and automatic model extension are outside this companion's scope. Use native model workflows for their separately documented supported sectors.

## Worked workflow and evidence

- [Complete bridge example](./Guide.md#Small%20explicit%20tree%20workflow)
- [Native GR guide](../../Models/GeneralRelativity/NativeModel.md)
- [Scientific scope](../../ScientificScope.md#General%20relativity)
- Source (`FeynArtsIntegration/Process.wl`)
