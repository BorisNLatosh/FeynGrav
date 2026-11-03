# FeynGravImportHorndeskiG5

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynGravImportHorndeskiG5``. **Baseline:** 4.2.0 development.

All available G5 files; no order argument.

## Syntax and arguments

```mathematica
FeynGravImportHorndeskiG5[opts]
```

Takes no positional order argument; imports all available matching `(a,b,n)` files. In the signature `opts` denotes option rules. There is no default or requested graviton order; the installed files determine the selection.

## Options

`Verbose -> False` is the sole option. `True` prints the inventory and progress. Unknown options and non-Boolean values return `Failure`.

## Result and replacement behaviour

Success returns `Null` after loading every matching extensionless
`HorndeskiG5_a_b_n` file from the installed package's `Libs` directory.
Imports do not download or generate rules. The selection may be sparse in
(a,b,n); no contiguous sequence of orders or complete set of monomials is
required.

A successful reimport replaces the family's vertex definitions with those
from the currently available files. Previously loaded selections no longer
present in that file set are removed. If no valid matching files exist, a
selected file cannot be read, or option validation fails, the import returns
`Failure` and preserves the previous definitions and import record.

Every successful import records file paths, hashes, actual orders and relevant settings. Query [FeynGravLibraryInformation](./FeynGravLibraryInformation.md) to inspect them. Records do not certify the physics of a library or change expressions calculated earlier.

## Example

```mathematica
<< FeynGrav`
FeynGravImportHorndeskiG5[];
FeynGravLibraryInformation[FeynGravImportHorndeskiG5]
```

Use a fresh kernel with D, κ and the monomial couplings unassigned. This illustrates the interface; no new evaluation result is asserted.

## Compatibility and scope

The former `import*` names and `printOutput` option are not aliases. Use the `FeynGravImport*` name and `Verbose`. The [library management overview](./README.md) explains orders, settings and failures; each [model overview](../../Models.md) states the relevant action and verification limits.
