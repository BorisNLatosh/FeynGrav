# Generating FeynGrav libraries

The generator constructs interaction rules, calculates them with
`CalcFormCalculate`, and writes extensionless Wolfram-expression files for
FeynGrav's existing import commands. FORM performs the configured Lorentz,
Dirac and fundamental SU(N) colour algebra. No integration or extra symmetry
factor is introduced by the generator.

## Loading and first calculation

Use a separate fresh kernel with FeynCalc installed. Do not load the main FeynGrav interface in that kernel: generator rule packages export some of the same short names and can produce shadowing warnings. Load the generator by its filename:

```mathematica
Get["/path/to/FeynGrav/Libs/FeynGravLibrariesGenerator.wl"];
CheckGravitonScalars
```

Loading starts no external processes, installs nothing and preserves
`Directory[]`. The `Check*` commands list canonical library filenames beside
the generator; staging files, backups, directories and FORM job files are excluded. Names must
use canonical integers without leading zeroes: one positive order for ordinary
families, or two non-negative counts and one positive order for Horndeski.

An explicit availability check is optional:

```mathematica
CalcFormCheck[]
```

Generate into an existing separate directory first:

```mathematica
output = CreateDirectory[];
GenerateGravitonScalarsSpecific[1,
    OutputDirectory -> output,
    FORMThreads -> 1,
    ShowTiming -> True];
FileNames["GravitonScalar*", output]
```

This writes both the kinetic and potential scalar libraries. No existing
package library is modified in this example. Omit `OutputDirectory` to replace
libraries in the generator's own `Libs` directory after successful calculation
and validation.

## Commands and arguments

The existing command names and positional arguments are retained:

| Family | Batch | Specific order |
|---|---|---|
| Scalars | `GenerateGravitonScalars[n]` | `GenerateGravitonScalarsSpecific[n]` |
| Fermions | `GenerateGravitonFermions[n]` | `GenerateGravitonFermionsSpecific[n]` |
| Vectors and vector ghosts | `GenerateGravitonVectors[n]` | `GenerateGravitonVectorsSpecific[n]` |
| Pure gravity | `GenerateGravitonVertex[n]` | `GenerateGravitonVertexSpecific[n]` |
| Yang–Mills and ghosts | `GenerateGravitonSUNYM[n]` | `GenerateGravitonSUNYMSpecific[n]` |
| Horndeski G2–G5 | `GenerateHorndeskiG2[numberOfScalars, n]`, etc. | `GenerateHorndeskiG2Specific[a, b, n]`, etc. |
| Scalar–Gauss–Bonnet | `GenerateScalarGaussBonnet[n]` | `GenerateScalarGaussBonnetSpecific[n]` |
| Axion–vector | `GenerateGravitonAxionVector[n]` | `GenerateGravitonAxionVectorSpecific[n]` |
| Quadratic gravity | `GenerateQuadraticGravityVertex[n]` | `GenerateQuadraticGravityVertexSpecific[n]` |

Counts must be explicit non-negative integers, and `n` must be positive.
Ordinary batches generate every order from 1 through `n`; Gauss–Bonnet starts at 2. Pure and quadratic gravity use `n + 2` external gravitons. Other ordinary families use `n` graviton legs.

Horndeski batches enumerate `a = 0..S`, with `S = numberOfScalars`, and every graviton order `1..n`. The scalar filters are:

| Family | Enumerated `b` | Retained scalar count |
| --- | --- | --- |
| G2 | `1..Ceiling[S/2]` | `3 <= a + 2 b <= S` |
| G3 | `0..Ceiling[S/2]` | `3 <= a + 2 b + 1 <= S` |
| G4 | `0..Ceiling[S/2]` | `2 <= a + 2 b <= S` |
| G5 | `0..Ceiling[S/2]` | `3 <= a + 2 b + 1 <= S` |

An empty selection returns `Null` without calculations or files. Specific Horndeski commands accept one non-negative `a,b` pair and positive `n` without the batch scalar-count filters; underlying rule constraints still apply.

### Files produced at each selected order

| Family | Extensionless filename stems (append `_n`) |
| --- | --- |
| Scalars | `GravitonScalarVertex`, `GravitonScalarPotentialVertex` |
| Fermions | `GravitonFermionVertex` |
| Vectors | `GravitonMassiveVectorVertex`, `GravitonVectorVertex`, `GravitonVectorGhostVertex` |
| Yang–Mills | `GravitonQuarkGluonVertex`, `GravitonGluonVertex`, `GravitonThreeGluonVertex`, `GravitonFourGluonVertex`, `GravitonYMGhostVertex`, `GravitonGluonGhostVertex` |
| Pure / quadratic gravity | `GravitonVertex` / `QuadraticGravityVertex` |
| Axion / Gauss–Bonnet | `GravitonAxionVectorVertex` / `ScalarGaussBonnet` |
| Horndeski | `HorndeskiG2_a_b_n` through `HorndeskiG5_a_b_n` (already complete patterns) |

**Gauss–Bonnet starts at two gravitons:** around a flat background each curvature
is at least first order in the graviton perturbation. The curvature-squared
combination therefore starts at second order, and its one-graviton contribution
vanishes. There is no missing one-graviton interaction to derive.

`GenerateScalarGaussBonnet[n]` generates orders `2` through `n`. For `n = 1`,
it returns `Null` without constructing rules, launching FORM or writing files.
Use `GenerateScalarGaussBonnetSpecific[n]` to generate one order with `n >= 2`.
The specific-generation and rule interfaces still require two or more gravitons;
they do not create a one-graviton zero library.

Success returns `Null`, accompanied by completion messages. Inspect `FailureQ`
before assuming files were produced. A batch runs sequentially and stops at
its first failed library; its failure lists the already completed files.

## Options

Every batch and specific command accepts the same options:

| Option | Default | Meaning |
|---|---|---|
| `OutputDirectory` | `Automatic` | Existing destination directory; automatic means the generator's `Libs`. |
| `FORMExecutable` | `Automatic` | Use converter discovery, or supply a name/path. |
| `FORMThreads` | `Automatic` | Prefer TFORM with up to eight workers; use serial FORM if TFORM is missing. |
| `TimeConstraint` | `Infinity` | Limit FORM execution, not rule construction or the complete generation call. |
| `WorkingDirectory` | `Automatic` | Parent of unique temporary converter jobs. |
| `KeepFiles` | `False` | Retain successful converter jobs when true; failed jobs retain diagnostics. |
| `ShowTiming` | `False` | Show converter execution timing and separate construction/public-command wall times. |
| `ShowProgress` | `False` | Forward converter progress reporting. |
| `DiracAlgebra` | `Automatic` | Enable supported Dirac processing; `False` preserves chains/traces. |
| `ColourAlgebra` | `True` | Enable SU(N) processing; `Automatic` is equivalent, `False` preserves colour structures. |

Explicit worker counts retain converter semantics: one selects FORM, larger
counts require TFORM. A discovered but unusable TFORM does not trigger silent
fallback. An explicit executable with automatic threads uses one worker.
No generator call installs FORM automatically.

`SetOptions` changes to the invoked command are honoured. Options may be given
as individual rules, lists or nested lists; the first explicit occurrence wins.
A batch uses its own defaults for all its members, independently of defaults
set on a specific-generation command.

Executable precedence is: an explicit option (including `Automatic`), then a
non-automatic default set on the invoked command, then the deprecated legacy
setting, then automatic converter discovery. A qualified legacy setting
``FeynGravLibrariesGenerator`$FeynGravFORMExecutable`` takes precedence over the
old `Global` spelling. Legacy settings are read only if they already exist;
loading does not create duplicate executable or startup-setting symbols.
Use normal options in new code.

`FeynGravLibrariesGeneratorFORMInformation[]` and
`FeynGravLibrariesGeneratorPrintFORMStatus[]` delegate to `CalcFormCheck`.
The former also accepts an executable string. The old
`$FeynGravLibrariesGeneratorFORMCheck` startup switch no longer launches a check.
`$FeynGravLibrariesGeneratorStartupMessage = False` suppresses the introduction.

## Mathematical and library conventions

- Rule formulas supply normalisation, momentum routing and physical factors of
  `I`. The generator adds no compensating phase.
- The uncontracted quark–gluon rule retains the D-dimensional gamma matrix from
  `QuarkGluonVertex[..., Explicit -> True]` and `SMP["g_s"]`.
- Both axion rules use four-slot `Eps` tensors with D-dimensional Lorentz indices
  and D-dimensional momentum components. Four slots do not mean rank D.
  FeynCalc's `$LeviCivitaSign` convention is captured and checked by the converter.
- Colour reduction may replace products of structure constants with an equivalent
  trace basis. Longer irreducible traces are valid output. Dirac and colour
  processing are independently selectable.
- Only declared formal placeholders and the gravitational coupling are mapped
  into the contexts expected by the existing FeynGrav importer. FeynCalc heads,
  matrix order, named couplings and distinct index spaces are preserved.
- Library files contain a single Wolfram expression, with explicit contexts where
  needed. They remain extensionless and are read by the existing `import*`
  commands. No changes to those importers are required.

The generator uses a dedicated parameter context. Before construction it checks
the declared source and destination placeholders without evaluating their values.
Own-values, down-values, up-values or sub-values cause `AssignedLibrarySymbol`
before calculation or file replacement. User definitions are left untouched;
use unassigned placeholders or a fresh kernel. The rule's `Global` gravitational
coupling and the public gauge parameters are explicitly localised as needed.

## Failures and safe replacement

Construction, calculation, serialisation and publication are separate stages.
A `LibraryGenerationFailed` diagnostic identifies the family, order, stage,
destination and nested cause. Converter failures retain their job directory,
exit status and log paths inside that cause. Invalid input is rejected before
construction.

The generator writes a unique staging file beside the destination, reads it back
under FeynGrav's importer context, and requires exact agreement with the
calculated expression. Only then does an abort-protected replacement begin.
The previous file is backed up until installation succeeds. If installation
fails, the old file is restored; a failed recovery reports the backup path.
Backup-cleanup failure explicitly reports `Published -> True` at the outer
failure level and includes the installed destination in `CompletedFiles`. Staging cleanup affects only the file created by the current call.

A user abort propagates after cleanup. Completed earlier batch members remain
installed; batches do not promise an all-or-nothing transaction across families.
Use `OutputDirectory` to generate a separate library set before adopting it.

## Maintenance and verification

The generator has one shared calculation/publication helper. Family
specifications contain held rule builders; they do not execute while the
specification list is assembled. Formal-symbol inspection evaluates only the
argument constructors, leaving rule evaluation to the construction stage.

Process launch, timeout, cancellation, logs, FORM syntax and restricted result
parsing belong to CalcFormConverter. There is no generator-side FORM parser,
shell command or textual gamma/colour dictionary.

See [the migration verification report](../Documentation/Verification/GeneratorVerification.md) for checked
families, regression results and the Gauss–Bonnet batch starting-order correction.
