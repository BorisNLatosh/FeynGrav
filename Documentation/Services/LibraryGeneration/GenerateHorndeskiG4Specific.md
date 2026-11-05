# GenerateHorndeskiG4Specific

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`GenerateHorndeskiG4Specific``. **Baseline:** 4.2.0.

## Purpose and arguments

GenerateHorndeskiG4Specific[a, b, n, opts] generates HorndeskiG4_a_b_n from the uncontracted G4 rule, with n external gravitons and a + 2 b scalar momentum entries. a and b must be explicit non-negative integers; n must be an explicit positive integer. The underlying rule supplies any additional structural requirements. Unlike the batch command, this specific command does not impose the batch's minimum scalar-count filter.

**Construction bounds:** 0 <= b <= 5 at n = 1, and 0 <= b <= 4 at n >= 2; a remains a
nonnegative integer. These limits come from the shared seven-inverse-metric
construction and apply in addition to the argument and selection conditions
above. They do not guarantee practical runtime, independent verification of
every allowed case, or availability of a shipped library. See the
[Horndeski model overview](../../Models/Horndeski/README.md#Rule%20generation%20and%20higher%20orders).

Bypassing the batch scalar-count filters does not bypass these construction
bounds.

## Syntax

```mathematica
GenerateHorndeskiG4Specific[a, b, n, opts]
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
