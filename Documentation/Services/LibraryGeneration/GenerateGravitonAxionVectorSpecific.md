# GenerateGravitonAxionVectorSpecific

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`GenerateGravitonAxionVectorSpecific``. **Baseline:** 4.2.0.

## Purpose and arguments

GenerateGravitonAxionVectorSpecific[n, opts] generates the scalar-axion–vector interaction libraries for exactly n external gravitons: GravitonAxionVectorVertex_n. n must be an explicit positive integer.

## Syntax

```mathematica
GenerateGravitonAxionVectorSpecific[n, opts]
```

Load `Libs/Generator.wl` in a **separate fresh kernel**, not one already using the main library-backed FeynGrav interface. Loading the generator defines functions but does not probe or install FORM.

## Options and side effects

| Option | Default |
| --- | --- |
| [OutputDirectory](../Options/OutputDirectory.md) | `Automatic` |
| [FORMExecutable](../Options/FORMExecutable.md) | `Automatic` |
| [FORMThreads](../Options/FORMThreads.md) | `Automatic` |
| [TimeConstraint](../Options/TimeConstraint.md) | `Infinity` |
| [WorkingDirectory](../Options/WorkingDirectory.md) | `Automatic` |
| [KeepFiles](../Options/KeepFiles.md) | `False` |
| [ShowTiming](../Options/ShowTiming.md) | `False` |
| [ShowProgress](../Options/ShowProgress.md) | `False` |
| [DiracAlgebra](../Options/DiracAlgebra.md) | `Automatic` |
| [ColourAlgebra](../Options/ColourAlgebra.md) | `True` |

An explicit `OutputDirectory` must already exist. Its automatic value is the generator's own `Libs` directory, where successful generation replaces the corresponding libraries. Use a fresh candidate directory when developing rules. Existing targets are replaced only after calculation and checked read-back. `WorkingDirectory` instead controls temporary converter jobs.

Success returns `Null`. Invalid arguments, assigned formal placeholders or failed stages return `Failure`; existing user definitions are preserved. Batches stop at the first failure and record completed files. Failed converter jobs retain diagnostics. Generation never installs FORM.

Every command prints overview, stage progress and final outcome even with `ShowProgress -> False`; `ShowTiming -> True` adds timing detail. The timeout covers FORM execution only, not rule construction or total elapsed time. Automatic workers prefer TFORM with up to eight workers, with serial fallback only when automatic TFORM discovery fails.

The invoked command's option store is honoured; batch defaults are independent of specific-command defaults. The first explicit occurrence of a repeated option wins. `ColourAlgebra -> Automatic` is an alias for True; `DiracAlgebra -> False` disables optional Dirac processing.

## Mathematical limits

Generating and reading back a library proves neither its action convention nor its physical completeness. Follow the model's derivation and independent verification before replacing a trusted library. In particular, the axion epsilon-density convention and higher-order legacy Gauss–Bonnet rules remain open; Horndeski sources have the finite inverse-metric bounds described in their model overview.

## Sources and workflow

- [Group overview](./README.md)
- Source (`Libs/Generator.wl`)
- [Generator workflow](./Guide.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
