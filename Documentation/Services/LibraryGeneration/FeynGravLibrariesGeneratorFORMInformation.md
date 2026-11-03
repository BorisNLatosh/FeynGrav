# FeynGravLibrariesGeneratorFORMInformation

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`FeynGravLibrariesGeneratorFORMInformation``. **Baseline:** 4.2.0 development.

## Purpose and arguments

FeynGravLibrariesGeneratorFORMInformation[] calls CalcFormCheck and returns its availability and diagnostic association. It honours an existing legacy executable setting; otherwise it uses automatic FORM/TFORM selection. FeynGravLibrariesGeneratorFORMInformation[executable_String] checks that executable with one worker. For worker counts or other check options, call CalcFormCheck directly. No availability check or installation runs when the generator is loaded.

## Syntax

```mathematica
FeynGravLibrariesGeneratorFORMInformation[]
FeynGravLibrariesGeneratorFORMInformation[executableString]
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Scope

This is an explicit inventory or availability query. It does not construct interaction rules or certify their physics. `Check*` commands inspect canonical filenames only, and are evaluated **without brackets**; they ignore `OutputDirectory`. FORM information/status commands instead run the documented explicit executable probe.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
