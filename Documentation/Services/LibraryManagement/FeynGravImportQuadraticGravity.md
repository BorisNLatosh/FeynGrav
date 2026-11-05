# FeynGravImportQuadraticGravity

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynGravImportQuadraticGravity``. **Baseline:** 4.2.0.

`QuadraticGravityVertex_j`, where `j + 2` is the number of graviton legs.

## Syntax and arguments

```mathematica
FeynGravImportQuadraticGravity[n : 2, opts]
```

The omitted maximum defaults to 2; supply an explicit positive integer. In the signature `opts` denotes option rules. `n : 2` describes the default, not syntax to copy into a call.

## Options

`Verbose -> False` is the sole option. `True` prints the inventory and progress. Unknown options and non-Boolean values return `Failure`.

## Result and replacement behaviour

Success returns `Null` after loading local extensionless libraries. Imports do not download or generate rules. Order-limited requests are capped at installed availability, but missing intermediate files cause failure. Failed reads or invalid orders preserve existing definitions and records. A successful lower-order reimport replaces the family and drops previously loaded higher orders.

Every successful import records file paths, hashes, actual orders and relevant settings. Query [FeynGravLibraryInformation](./FeynGravLibraryInformation.md) to inspect them. Records do not certify the physics of a library or change expressions calculated earlier.

## Example

```mathematica
<< FeynGrav`
FeynGravImportQuadraticGravity[2];
FeynGravLibraryInformation[FeynGravImportQuadraticGravity]
```

Use a fresh kernel with symbolic D and κ. Set any family-specific gauge value before import. This illustrates the interface; no new evaluation result is asserted.

## Compatibility and scope

The former `import*` names and `printOutput` option are not aliases. Use the `FeynGravImport*` name and `Verbose`. The [library management overview](./README.md) explains orders, settings and failures; each [model overview](../../Models.md) states the relevant action and verification limits.
