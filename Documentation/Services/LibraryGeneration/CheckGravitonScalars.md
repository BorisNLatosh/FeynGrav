# CheckGravitonScalars

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`CheckGravitonScalars``. **Baseline:** 4.2.0.

## Purpose and arguments

CheckGravitonScalars (without brackets) prints canonical filenames for the scalar kinetic and scalar potential interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.

## Syntax

```mathematica
CheckGravitonScalars
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Scope

This is an explicit inventory or availability query. It does not construct interaction rules or certify their physics. `Check*` commands inspect canonical filenames only, and are evaluated **without brackets**; they ignore `OutputDirectory`. FORM information/status commands instead run the documented explicit executable probe.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
