# FeynGravWithAllowedProcess

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`FeynGravWithAllowedProcess``. **Baseline:** 4.2.0 development.

Optional pure-GR companion interface. It is narrower than ordinary native FeynArts use with the five supplied models.

## Syntax and arguments

```mathematica
FeynGravWithAllowedProcess[spec, action]
```

`spec` has the [validation](./FeynGravValidateProcess.md) structure. `action` is held until validation succeeds; supply it directly as the second argument if it initialises FeynArts.

## Loading

Use a fresh kernel with FeynCalc-patched FeynArts and load `FeynArtsIntegration/FeynArtsIntegration.wl` explicitly before native model initialisation. The companion is not loaded by ordinary FeynGrav initialisation. Loading its definitions alone starts no process, initialises no model and writes no bundle.

## Options

No options are advertised for this interface.

## Result and limits

Returns the validation failure or evaluates the accepted action. The helper is `HoldRest`; it is not a general proof that an arbitrary action has appropriate physics or no side effects.

The corrected GR release supports the stated trees; loop compatibility remains deferred. General matter processes, higher-multiplicity trees and automatic model extension are outside this companion's scope. Use native model workflows for their separately documented supported sectors.

## Worked workflow and evidence

- [Complete bridge example](./Guide.md#Small%20explicit%20tree%20workflow)
- [Native GR guide](../../Models/GeneralRelativity/NativeModel.md)
- [Scientific scope](../../ScientificScope.md#General%20relativity)
- Source (`FeynArtsIntegration/Process.wl`)
