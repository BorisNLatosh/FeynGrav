# FeynGravLibrariesGeneratorPrintFORMStatus

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`FeynGravLibrariesGeneratorPrintFORMStatus``. **Baseline:** 4.2.0 development.

## Purpose and arguments

FeynGravLibrariesGeneratorPrintFORMStatus[] calls FeynGravLibrariesGeneratorFORMInformation[], prints the availability and diagnostic association, and returns it. This is an explicit check; it does not install software.

## Syntax

```mathematica
FeynGravLibrariesGeneratorPrintFORMStatus[]
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Scope

This is an explicit inventory or availability query. It does not construct interaction rules or certify their physics. `Check*` commands inspect canonical filenames only, and are evaluated **without brackets**; they ignore `OutputDirectory`. FORM information/status commands instead run the documented explicit executable probe.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
