# CheckScalarGaussBonnet

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`CheckScalarGaussBonnet``. **Baseline:** 4.2.0.

## Purpose and arguments

CheckScalarGaussBonnet (without brackets) prints canonical ScalarGaussBonnet_n filenames in the generator's Libs directory and returns Null. Generated interaction libraries start at n = 2. This is a filename inventory, not a check of file contents or the physical order; it launches no FORM process and ignores OutputDirectory.

## Syntax

```mathematica
CheckScalarGaussBonnet
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Scope

This is an explicit inventory or availability query. It does not construct interaction rules or certify their physics. `Check*` commands inspect canonical filenames only, and are evaluated **without brackets**; they ignore `OutputDirectory`. FORM information/status commands instead run the documented explicit executable probe.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
