# GenerateHorndeskiG3

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`GenerateHorndeskiG3``. **Baseline:** 4.2.0 development.

## Purpose and arguments

GenerateHorndeskiG3[numberOfScalars, n, opts] generates selected G3 libraries for graviton orders 1 through n. numberOfScalars is an explicit non-negative integer upper bound on the number of scalar momentum entries; n is an explicit positive integer. It enumerates a = 0 through numberOfScalars and b = 0 through Ceiling[numberOfScalars/2], retaining only 3 <= a + 2 b + 1 <= numberOfScalars. An empty selection returns Null without calculation or files. Use GenerateHorndeskiG3Specific[a, b, n, opts] for one parameter triple.

**Construction bounds:** 0 <= b <= 5 at every generated graviton order n >= 1; a remains a
nonnegative integer. These limits come from the shared seven-inverse-metric
construction and apply in addition to the argument and selection conditions
above. They do not guarantee practical runtime, independent verification of
every allowed case, or availability of a shipped library. See the
[Horndeski model overview](../../Models/Horndeski/README.md#Rule%20generation%20and%20higher%20orders).

For a batch, n in these bounds denotes the graviton order of each selected
library. Batch selection does not enforce these construction bounds. A selected
unsupported case can fail; successfully completed earlier files remain.

## Syntax

```mathematica
GenerateHorndeskiG3[numberOfScalars, n, opts]
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
