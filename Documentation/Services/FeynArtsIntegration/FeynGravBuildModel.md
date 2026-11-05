# FeynGravBuildModel

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`FeynGravBuildModel``. **Baseline:** 4.2.0 development.

Optional pure-GR companion interface. It is narrower than ordinary native FeynArts use with the supplied models.

## Syntax and arguments

```mathematica
FeynGravBuildModel[absoluteNewDirectory, opts]
```

`absoluteNewDirectory` must be a fresh absolute destination; an existing directory is never overwritten. `GravitonOrder -> 2` is the default, building cubic and quartic graviton vertices and the ghost sector. `3` adds the five-graviton contact. The verified library uses a compact symmetric component. Other libraries may incur the cost of a fully expanded basis. See [order limits and checks](../../Models/GeneralRelativity/HigherOrders.md). Other orders fail before rule construction. A missing requested library fails instead of silently reducing the snapshot.

## Loading

Use a fresh kernel with FeynCalc-patched FeynArts and load `FeynArtsIntegration/FeynArtsIntegration.wl` explicitly before native model initialisation. The companion is not loaded by ordinary FeynGrav initialisation. Loading its definitions alone starts no process, initialises no model and writes no bundle.

## Options

The sole option is [GravitonOrder](../Options/GravitonOrder.md), initially 2.

## Result and limits

Writes the matched finite native model bundle and manifest. For the verified quintic, the manifest records `QuinticRepresentation -> "SymmetricSeed-v1"`. Its snapshot fingerprint uses the quintic rule's digest with the lower rules and propagators. Rebuilding an older expanded snapshot can therefore give a different fingerprint for the same contact. Use the supplied bundle for ordinary tree calculations; rebuilding requires a separately validated model snapshot. A new snapshot does not inherit historical loop-verification fingerprints.

The corrected GR release supports the stated trees; loop compatibility remains deferred. General matter processes, higher-multiplicity trees and automatic model extension are outside this companion's scope. Use native model workflows for their separately documented supported sectors.

## Worked workflow and evidence

- [Complete bridge example](./Guide.md#Small%20explicit%20tree%20workflow)
- [Native GR guide](../../Models/GeneralRelativity/NativeModel.md)
- [Scientific scope](../../ScientificScope.md#General%20relativity)
- Source (`FeynArtsIntegration/FeynArtsIntegration.wl`)
