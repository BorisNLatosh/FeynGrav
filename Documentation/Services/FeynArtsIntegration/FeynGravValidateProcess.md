# FeynGravValidateProcess

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`FeynGravValidateProcess``. **Baseline:** 4.2.0 development.

Optional pure-GR companion interface. It is narrower than ordinary native FeynArts use with the supplied models.

## Syntax and arguments

```mathematica
FeynGravValidateProcess[spec]
```

`spec` is an association with `Model`, `Incoming`, `Outgoing`, `LoopOrder` and optional `OneParticleIrreducible` keys. Only `"GR"` is accepted. External fields are ``FeynArts`T[1]``. Tree processes have one or more legs on each side and three or four legs in total, with `LoopOrder -> 0` and no 1PI request. The historical one-loop allowlist case is one incoming and one outgoing graviton with `OneParticleIrreducible -> True`.

## Loading

Use a fresh kernel with FeynCalc-patched FeynArts and load `FeynArtsIntegration/FeynArtsIntegration.wl` explicitly before native model initialisation. The companion is not loaded by ordinary FeynGrav initialisation. Loading its definitions alone starts no process, initialises no model and writes no bundle.

## Options

No options are advertised for this interface.

## Result and limits

Returns an allowed-process association or `Failure`, without initialising FeynArts. Allowlist success is not permission from the later model-fingerprint gate: the corrected model still refuses loop construction.

The corrected GR release supports the stated trees; loop compatibility remains deferred. General matter processes, higher-multiplicity trees and automatic model extension are outside this companion's scope. Use native model workflows for their separately documented supported sectors.

## Worked workflow and evidence

- [Complete bridge example](./Guide.md#Small%20explicit%20tree%20workflow)
- [Native GR guide](../../Models/GeneralRelativity/NativeModel.md)
- [Scientific scope](../../ScientificScope.md#General%20relativity)
- Source (`FeynArtsIntegration/Process.wl`)
