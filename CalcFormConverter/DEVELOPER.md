# Developer guide

This guide describes implementation and maintenance of CalcFormConverter. Start with [README.md](README.md) for commands and workflows; use [FORMAT.md](FORMAT.md) for the compatibility contract. Private helper names and associations may change without changing the public interface or saved format.

## Contents

- [Architecture and extension points](#architecture-and-extension-points)
- [Evaluation and resource invariants](#evaluation-and-resource-invariants)
- [Testing](#testing)
- [Performance measurement](#performance-measurement)

## Architecture and extension points

Keep the public export/import signatures independent of internal refactoring. The private export stages are:

1. `buildExportData[expression, dimension, loopMomenta]`: normalize and inspect the input, register symbols, and return the initial expression text, staged multiplication texts, factor macros and mapping data. It performs no file access.
2. `renderExport[data, resultPath, templateText]`: generate program and JSON text in memory. It performs no file access.
3. `writeExport[paths, rendered, overwrite]`: write the prepared files. Path checks and template reading are separate helpers used by the public command.

The export registry uses held expression keys containing both the symbol kind and value. This keeps identical names in different contexts and the same symbol in different roles distinct, without repeatedly encoding values as text. Each new mapping entry is encoded once. Staging must preserve the original traversal order so identifiers remain deterministic.

Complete native vector-component and metric calls are recognized as single import tokens and decoded lazily through the normal identifier and argument checks. Repeated calls reuse their validated values within that import. Other syntax, including nested arguments and dot chains, keeps the ordinary parser; successful dot reconstruction is cached locally. This preserves error order and avoids repeatedly parsing the same short tensor calls. The measured median paired improvement was about 29.5% across ten full-import comparisons on one retained result; scalar and master-function inputs showed little benefit. Cache memory grows with distinct calls and vector pairs, and peak memory has not been measured.

Repeated tensor, denominator and scalar-abbreviation conversions are cached within each export call. The first occurrence still performs validation and symbol registration; later occurrences reuse its serialized fragment. No cache is shared between jobs, and general expression emission is not cached because sums allocate ordered macros. This benefits expressions with repeated structures; mostly unique inputs can incur extra lookup and memory costs. Cache storage grows with the number and size of distinct cached expressions.

For a new scalar master function, add its head, FORM name, argument count and argument category to `$expressionSpecs`. Mapping decoding, export/import validation, serialization and FORM declarations use that specification. Add an explicit mathematical round-trip test and document the new vocabulary. Supporting a new tensor structure can still require translation and parser rules; the specification does not supply those algorithms.

Mapping prefixes, declaration classes and value checks live in `$kindSpecs`. Consult [the format contract](FORMAT.md) before changing persisted fields or their meaning. Preserve the existing version-one fixture when introducing another format version.

The import stages validate file correspondence and mapping names, decode all mapping entries into a job-local typed dictionary with `decodeEntries`, and reconstruct the result with `parseResult`. Every mapping expression is validated once, including entries absent from the result; repeated identifiers reuse their decoded value. The parser classifies each distinct lexical token once and uses documented `Reap`/`Sow` collectors for sums and products, constructing `Plus` and `Times` only after collecting and checking their operands. Keep vector/index validation before arithmetic evaluation so cancellation cannot hide an invalid token. The dictionary is local to each import; no decoded mapping state is shared between jobs.

## Evaluation and resource invariants

- Preserve job-local lexical state. Parser cursor, recursive locals, exporter registry and counters must not be shared between calls.
- Keep `SetDelayed` where calls must observe current state or perform work later. An immediate assignment to a process-launch or cleanup helper can execute its body during definition.
- `take[]` checks the original token count before advancing once. The appended `END` sentinel is for `peek[]` only; input validation precedes appending it, and the trailing-token check still uses the original count.
- `atom[]` binds its immutable token with `With`. Only function calls allocate an argument list. Parentheses parse their value before consuming the closing delimiter.
- `power[]` retains a mutable value per call; exponent-specific state is allocated inside the exponent branch. Sums and products allocate tail state only when an operator follows. Never move recursive mutable state into shared outer variables.
- Keep `Reap` boundaries around each nested sum/product. Validate typed operands before arithmetic can hide a vector/index through cancellation, zero multiplication or a zero exponent.
- `register` uses a held structural key on every lookup, but allocates insertion locals and encodes data only for a new entry. Preserve role-sensitive keys, deterministic traversal, counter updates and metadata.
- Loading defines runtime functions without probing or launching FORM. Export/import remain independent of external process execution.
- Runtime acquisition, process execution, output draining and final inspection remain inside protected cleanup boundaries. Both output streams must close on ordinary and nonlocal exits. Installation transactions are allowed to finish when aborted; calculation processes are cancellable.
- Export's write transaction preserves the previous program/mapping pair on recoverable failures. Do not treat that guarantee as crash recovery or concurrent-writer isolation.

## Testing

The cleanup implementation uses Wolfram Language's built-in [WithCleanup](https://reference.wolfram.com/language/ref/WithCleanup.html). Use a kernel that provides this function; verification was performed with Wolfram 15.0.1.

Run from this directory or use the full path:

```sh
python3 Tests/run.py
```

The default runs all four suites. Installer tests always use mocks and never modify system packages. To run them separately:

```sh
python3 Tests/run.py --suite core
python3 Tests/run.py --suite form
python3 Tests/run.py --suite runtime
python3 Tests/run.py --suite integration
```

| Suite | Dependencies | Coverage |
| --- | --- | --- |
| `core` | WolframKernel and FeynCalc | Mapping/parser checks, long and nested sums/products, typed-token rejection before cancellation, persisted version-one fixture, in-memory rendering, export rollback/recovery and mocked installation decisions |
| `form` | Core dependencies plus FORM | Bosonic export–FORM–import comparisons, staged and independent monolithic programs, free/contracted indices, mapping order and fallback cases |
| `runtime` | FORM dependencies; Linux/POSIX test environment | Real probes/calculations, optional installed TFORM checks, reporting options, controlled failing executables, logging, timeout, abort at acquisition/polling/finalization and cleanup |
| `integration` | FORM dependencies plus FeynGrav's cubic quadratic-gravity library | Vertex/propagator comparisons, loading and namespace isolation, complete bubble export |

Executables must be on `PATH`; `core` does not locate or launch FORM. The `runtime` suite requires Mathematica to be allowed to start external processes; a restricted execution sandbox may block it. The driver uses separate temporary directories and fresh kernels. Tests identify cases by descriptive keys, so adding a case does not change other tests' meaning. The fixed compatibility artifacts in `Tests/Fixtures` are read without regeneration.

The complete example bubble was constructed and exported on the development machine using FORM 4.3 as the available test backend. A recorded export after staged generation took **2.039 seconds**, producing **480,503 bytes** of FORM source and **6,685 bytes** of mapping from an expression with **178,961 leaves**. This measures conversion only. Full-bubble FORM contraction was not benchmarked, and no speedup claim is made. Smaller generated programs are executed in the test suite and compared with FeynCalc. The full-bubble test checks its mapping against the original propagators, masses, indices, abbreviations and loop momenta without executing the complete FORM job. Source size varies slightly with the output path embedded in the program.


### Earlier recorded performance checks

On the development machine, a retained 636 KB result containing 6,244 terms was used for paired full-import comparisons. The original importer took 7.626 and 7.683 seconds; the optimized importer took 2.989 and 3.573 seconds in the corresponding runs. All imported expressions were exactly equal under `SameQ`. These are measurements on one result, not a general performance guarantee.

A separate bounded comparison used the retained program that produced that result:

| Program structure | Serial FORM | TFORM, four workers |
| --- | ---: | ---: |
| One defining module | 0.861 s | 0.921 s |
| Staged multiplication in original factor order | 0.338 s | 0.180 s |

All four result files were byte-for-byte identical. These single-run measurements illustrate the effect of the generated program's structure; the complete scalar bubble was not evaluated for this comparison. Export-registry microbenchmarks also showed approximately 19–23% improvement on synthetic repeated-symbol expressions, with identical generated data.

Recorded verification covered all four suites, package loading, exact equality on the retained 6,244-term result, full-bubble export metadata and independent staged-program checks under TFORM. Assertion counts change as cases are added; use the current test output for the current revision. No system packages were installed during testing.


## Performance measurement

Separate in-memory conversion, file I/O, external FORM execution, import and notebook display. Full calculation comparisons should use `AbsoluteTiming`; an internal `buildExportData` measurement does not include rendering, file writes, the availability probe or FORM. `ShowTiming` reports elapsed execution time, not total calculation time or aggregate worker CPU time.

For a small change, compare baseline and candidate in the same fresh kernel, warm the relevant paths, alternate measurement order, repeat bounded runs and compare outputs. Synthetic examples help identify repeated work but do not establish an end-to-end improvement. Preserve malformed-input checks alongside successful examples. Use exact `SameQ` when a refactor is intended to preserve reconstruction; use an appropriate algebraic comparison for legitimate FORM transformations.

### Subsequent targeted measurements

On Wolfram 15.0.1, a 636 KB retained result with 6,244 terms was used to compare the already optimized token reader against additional parser changes: branch-local allocation, cached original token count and sentinel lookahead. Three counterbalanced full-import pairs were:

| Pair | Token-reader baseline | Additional parser changes |
| --- | ---: | ---: |
| 1 | 3.484508 s | 2.256755 s |
| 2 | 3.281041 s | 2.165659 s |
| 3 | 3.078409 s | 2.036773 s |

The medians were 3.281041 and 2.165659 seconds, about 34% lower. Every result was `SameQ`; focused malformed/valid inputs and the existing parser regressions also passed. The comparison did not rerun FORM, and it does not predict the speedup of a complete calculation.

A separate registry comparison constructed export data for a representative expression with 178,961 leaves. Four alternating pairs had medians 1.385728 and 1.274246 seconds, about 8% lower, with identical export data. This measurement excludes rendering and writing files. It is distinct from the earlier repeated-symbol microbenchmarks above.

These are recorded development observations, not performance requirements or current-machine guarantees. Later regression counts and timing runs should be reported with their actual revision, environment, scope and baseline. Faster functional syntax is not an optimization rule: fresh symbols, repeated evaluation, list copying and built-in bulk operations must be assessed in the actual path. Retain validation, evaluation order, compatibility and cleanup behavior when optimizing.

### Polarization identities

`vectorIdentityQ` recognizes momentum symbols and constrained FeynCalc
polarization identities. `physicalMomentumLabelQ` also permits exact rational
linear momentum labels after routing substitutions. Register the complete
polarization as one vector; do not distribute it over that routing. Dedicated encoding/decoding preserves the `I` versus
`-I` label and an optional Boolean `Transversality` setting without enabling
general rule decoding. Vector reconstruction still goes through `Momentum`
and `Pair`, so current FeynCalc scalar-product definitions apply at import.
Propagator routing explicitly excludes polarizations. Keep tests for free
components, contractions, conjugation, transversality, dimensions and rejected
identities when extending this vocabulary.
