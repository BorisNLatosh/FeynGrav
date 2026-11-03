# CheckQuadraticGravityVertex

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`CheckQuadraticGravityVertex``. **Baseline:** 4.2.0 development.

## Purpose and arguments

CheckQuadraticGravityVertex (without brackets) prints canonical QuadraticGravityVertex_n filenames in the generator's Libs directory and returns Null. The suffix n corresponds to n + 2 graviton legs. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.

## Syntax

```mathematica
CheckQuadraticGravityVertex
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Scope

This is an explicit inventory or availability query. It does not construct interaction rules or certify their physics. `Check*` commands inspect canonical filenames only, and are evaluated **without brackets**; they ignore `OutputDirectory`. FORM information/status commands instead run the documented explicit executable probe.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
