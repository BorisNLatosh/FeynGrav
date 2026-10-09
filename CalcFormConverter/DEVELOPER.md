# Developer guide

This guide describes implementation and maintenance of CalcFormConverter. Start with [README.md](README.md) for commands and workflows; use [FORMAT.md](FORMAT.md) for the compatibility contract. Private helper names and associations may change without changing the public interface or saved format.

## Contents

- [Architecture and extension points](#architecture-and-extension-points)
- [Evaluation and resource invariants](#evaluation-and-resource-invariants)
- [Testing](#testing)
- [Performance measurement](#performance-measurement)
- [Epsilon translation](#epsilon-translation)
- [Dirac/colour companion](#diraccolour-companion)
- [Dirac processing companion](#dirac-processing-companion)
- [SU(N) colour processing](#sun-colour-processing)

## Architecture and extension points

| Module | Responsibility |
| --- | --- |
| `CalcFormConverter.wl` | Common vocabulary, export orchestration, mappings, restricted parsing and reconstruction |
| `FORMRuntime.wl` | Executable selection, probes, process lifecycle and calculation orchestration |
| `DiracColour.wl` | Typed matrix words, local noncommutative normalisation and reversible translation |
| `DiracAlgebra.wl` | Open-chain processing, explicit Dirac traces and spin-line metadata |
| `ColourAlgebra.wl` | Typed colour indices/endpoints, procedure configuration and scalar Casimir presentation |
| `Templates/` | Main program plus embedded Dirac/colour processing procedures |

Export validates and normalises the input, selects typed dictionaries and format metadata, serialises, then renders and publishes files. Enabled colour processing precedes Dirac processing and final Lorentz contraction in FORM. Import validates the mapping and result, reconstructs typed expressions and applies scalar coefficient presentation for processed colour jobs; it does not run tensor colour reduction in Mathematica. Companion loading launches no processes.


Keep the public export/import signatures independent of internal refactoring. The private export stages are:

1. `buildExportData[expression, dimension, loopMomenta, diracMode, colourMode]`: normalise and inspect the input, register symbols, and return the initial expression text, staged multiplication texts, factor macros and mapping data. It performs no file access.
2. `renderExport[data, resultPath, templateText]`: generate program and JSON text in memory. It performs no file access.
3. `writeExport[paths, rendered, overwrite]`: write the prepared files. Path checks and template reading are separate helpers used by the public command.

Validate required and unknown placeholders in the original template before substitution. Replacement values, including result paths, are data and must not be scanned for template placeholders. The importer uses `$flatParserMinimumCharacters` for its fast-parser length gate; tune that constant with parser-dispatch regression coverage.

The export registry uses held expression keys containing both the symbol kind and value. This keeps identical names in different contexts and the same symbol in different roles distinct, without repeatedly encoding values as text. Each new mapping entry is encoded once. Serialisation must preserve the original traversal order so identifiers remain deterministic. Stage planning runs only after serialisation and cannot change mappings or macros.

`connectedStageOrder[stages, signatures]` retains the previous connected greedy path as its baseline. For at most eight stages, it tries each tensor-bearing starting point using the same next-stage rule. Candidate scores minimise maximum open-index width, total width, then the reversed sequence of intermediate widths. The last criterion favours narrower late intermediates. Exact score ties prefer the baseline, then deterministic original positions. This explores at most eight paths, not factorially many permutations, and does not contract or expand Wolfram expressions. Larger products retain the baseline; failed signatures and scalar-only products retain their established fallbacks. Epsilon, matrix and processed-colour staging exclusions are unchanged. See [order-search verification](Tests/Reports/ContractionOrderSearch.md).

Complete native vector-component and metric calls are recognised as single import tokens and decoded lazily through the normal identifier and argument checks. Repeated calls reuse their validated values within that import. Other syntax, including nested arguments and dot chains, keeps the ordinary parser; successful dot reconstruction is cached locally. This preserves error order and avoids repeatedly parsing the same short tensor calls. The measured median paired improvement was about 29.5% across ten full-import comparisons on one retained result; scalar and master-function inputs showed little benefit. Cache memory grows with distinct calls and vector pairs, and peak memory has not been measured.

Repeated tensor, denominator and scalar-abbreviation conversions are cached within each export call. The first occurrence still performs validation and symbol registration; later occurrences reuse its serialised fragment. No cache is shared between jobs, and general expression emission is not cached because sums allocate ordered macros. This benefits expressions with repeated structures; mostly unique inputs can incur extra lookup and memory costs. Cache storage grows with the number and size of distinct cached expressions.

For a new scalar master function, add its head, FORM name, argument count and argument category to `$expressionSpecs`. Mapping decoding, export/import validation, serialisation and FORM declarations use that specification. Add an explicit mathematical round-trip test and document the new vocabulary. Supporting a new tensor structure can still require translation and parser rules; the specification does not supply those algorithms.

Mapping prefixes, declaration classes and value checks live in `$kindSpecs`. Consult [the format contract](FORMAT.md) before changing persisted fields or their meaning. Preserve the existing version-one fixture when introducing another format version.

The import stages validate file correspondence and mapping names, decode all mapping entries into a job-local typed dictionary with `decodeEntries[entries, dimension]`, and reconstruct the result with `parseResult`. Every mapping expression is validated once, including entries absent from the result; repeated identifiers reuse their decoded value. The parser classifies each distinct lexical token once and uses documented `Reap`/`Sow` collectors for sums and products, constructing `Plus` and `Times` only after collecting and checking their operands. Keep vector/index validation before arithmetic evaluation so cancellation cannot hide an invalid token. The dictionary is local to each import; no decoded mapping state is shared between jobs.

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
- Propagator routing and mass validation must agree between export and import; import also checks authoritative expression dimensions, including unused entries. Symbol names are reconstructed through `Symbol`, never `ToExpression`; context qualification and kernel name validation both apply.
- Failed temporary-file deletion is reported through a warning and retained paths. A committed export remains successful with an additional `CleanupFailure` diagnostic.
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
| `runtime` | FORM dependencies; Linux/POSIX test environment | Runtime probes, timeout/cancellation, log/cleanup checks; epsilon, Dirac translation, Dirac algebra and colour algebra; unchanged upstream colour preflight |
| `integration` | FORM dependencies plus FeynGrav's cubic quadratic-gravity library | Vertex/propagator comparisons, loading and namespace isolation, Dirac/colour rule comparisons and complete bubble export |

Executables must be on `PATH`; `core` does not locate or launch FORM. The `runtime` suite requires Mathematica to be allowed to start external processes; a restricted execution sandbox may block it. The driver uses separate temporary directories and fresh kernels. Tests identify cases by descriptive keys, so adding a case does not change other tests' meaning. The fixed compatibility artifacts in `Tests/Fixtures` are read without regeneration.

The complete example bubble was constructed and exported on the development machine using FORM 4.3 as the available test backend. A recorded export after staged generation took **2.039 seconds**, producing **480,503 bytes** of FORM source and **6,685 bytes** of mapping from an expression with **178,961 leaves**. This measures conversion only. That initial staging check did not benchmark full-bubble FORM contraction; the later factor-normalisation measurements below cover it separately. Smaller generated programs are executed in the test suite and compared with FeynCalc. The full-bubble test checks its mapping against the original propagators, masses, indices, abbreviations and loop momenta without executing the complete FORM job. Source size varies slightly with the output path embedded in the program.


## Performance measurement

Use the [benchmark notebooks](../Benchmark/README.md) for current user-facing measurements. Compare identical workloads in sequential fresh kernels, record preparation separately, validate outside timing and retain failed/inconclusive observations. More workers are not a guarantee of better performance. Avoid inferring total-call performance from an isolated stage.

Historical optimisation measurements have moved to the [dated performance report](../Documentation/Verification/ConverterPerformance.md). Its measurements use different revisions and workloads and must not be combined into a single claimed speedup.

<a id="earlier-recorded-performance-checks"></a>
<a id="subsequent-targeted-measurements"></a>
<a id="polarization-identities"></a>
<a id="connected-tensor-stage-ordering"></a>
<a id="measurement-against-b73c1a7"></a>
<a id="reproducing-an-execution-comparison"></a>
<a id="sources-and-interpretation"></a>
<a id="normalizing-large-form-stage-factors"></a>
<a id="incremental-execution-results"></a>
<a id="reusing-repeated-factors-in-large-wolfram-imports"></a>
<a id="token-eligibility-follow-up-measurement"></a>
<a id="public-command-measurements-original-repeated-factor-optimization"></a>
<a id="memory-and-cold-call-caveats"></a>
<a id="rejected-candidates-and-verification"></a>
<a id="bare-tensor-factors-in-the-flat-parser-3-october-2026"></a>

The anchors above preserve links to earlier guide sections. Follow the historical report for their measurements and implementation-stage narratives. The active regression suites remain authoritative for current parser/staging behaviour.

## Epsilon translation

Rank-four Eps serialisation shares vector/index registration with Pair. Only
linear slot routing is distributed. Eps is never an opaque scalar abbreviation.
Index signatures count epsilon slots, but epsilon-containing exports bypass the
stage planner and prepared expressions for now. Their FORM processing appends
`contract 0;` before the final sort without changing the template placeholder API.
The general parser reconstructs typed slots; the flat parser grammar is unchanged.
The import translation factor is dynamically scoped to one import, defaults to
None for version one, and cannot leak into subsequent imports.

`Tests/Epsilon.wls` runs real FORM and TFORM convention and identity comparisons,
plus malformed-output and metadata checks. It is included in the runtime suite.
The sign factor was established by real-process contraction tests rather than a
literal comparison of printed epsilon component conventions. See FORMAT.md.


## Dirac/colour companion

DiracColour.wl loads inside the private converter context. Its declarative
vocabulary extends the expression/kind dictionaries at load time without
processes, changes to FeynCalc definitions or global option changes.

The normaliser keeps coefficients, Dirac slots and adjoint words separate.
Only Dot forces local multilinearity; outer tensor sums/products remain
factored. A Times with multiple implicit chains in either space is ambiguous.
Scalar branches of a Dirac Dot use native gi_(1). With colour processing disabled, whole colour tensors and whole ordered colour
words are reversible symbols. Enabled colour processing uses the separate
ColourAlgebra companion described below.

Exports containing matrix words bypass staged multiplication planning; commuting colour tensors can retain it only in translation-only mode and without colour traces. Any version-five colour representation bypasses staging; scalar couplings alone do not disable it. Native gamma
serialisation shares vector/index registration. The general parser uses typed
matrix words and validates products and powers before arithmetic can hide an
invalid expression. The reserved import line is dynamically scoped; legacy
imports cannot use gamma calls. The original fast parser is unchanged.

Tests/DiracColour.wls is part of the runtime suite. It checks matrix order,
colour vocabulary, malformed output and real FORM/TFORM round trips.
Tests/DiracColourRules.wls is part of integration, comparing representative
fermion and Yang–Mills expressions with independent FeynCalc algebra.
Neither suite edits stored libraries. The quark–gluon rule now retains
D-dimensional gamma matrices and is compared directly. Synthetic mixed-space
inputs remain covered as failures in converter tests.


## Dirac processing companion

`DiracAlgebra.wl` validates traces, allocates independent spin lines, records
version-four metadata and specialises `Templates/DiracAlgebra.frm.in`.
The complete procedure is embedded during export. Package loading only
installs definitions. It does not read the procedure or launch FORM.

Automatic open chains are emitted as the noncommuting FORM tensor `cfcOpen`.
Using a tensor retains native Lorentz contractions, while allowing ordinary
pattern matching on the word. Native gamma objects are used directly for
traces. After `tracen` on each explicit trace line, a FORM `repeat` procedure
reduces adjacent equal arguments and swaps inverted adjacent arguments using
the Clifford relation. The swap term reduces inversions; its companion term
shortens the word. Shortening may introduce Lorentz contractions, so the loop
reapplies the rules before converting `cfcOpen` to native `g_(1,...)`.
All mapped indices/vectors participate in ordering, since metric/vector
contraction can introduce a label not originally inside a gamma word.

The finite rules are specialised to the dictionary, with quadratic source
size in its number of Lorentz objects. This release prioritises correctness;
no performance claim is made. Do not add an arbitrary rewrite-count cutoff
that silently returns a partially ordered result.

Trace bodies use the existing local Dot multilinearity machinery. Allocate
lines per emitted occurrence, not per distinct expression. Never memoise the
emitted text of trace-containing subexpressions: two occurrences sharing one
native line would become one trace of a product. Trace metadata and import
state are local to a single job. Keep convention validation active even when
FORM eliminates every trace or epsilon tensor.

`Tests/DiracAlgebra.wls` runs independent FeynCalc comparisons with
`DiracOrder -> True`, since FeynCalc does not order open chains by default.
It covers serial FORM, TFORM, trace normalisation, preserved traces, malformed
metadata/results, cache isolation and bounded generated words. The existing
translation-order test explicitly requests `DiracAlgebra -> False`.


## SU(N) colour processing

`ColourAlgebra.wl` owns trace validation, implicit identity promotion, typed
colour dictionaries, version-five validation, reconstruction and scalar Casimir
presentation. `Templates/ColourAlgebra.frm.in` owns tensor reduction and embeds
the namespaced upstream completeness procedure. `ThirdParty/FORMColour` holds
the unchanged reference, GPL text, checksums and adaptation notes.

The pipeline is local matrix/trace normalisation → typed emission → colour
procedure → Dirac procedure → final sort → restricted reconstruction → scalar
coefficient presentation. Disable tensor staging for every version-five job.
Do not mix input-basis f/d-to-trace rules with output-basis three-trace rules:
that introduces a rewrite cycle. Longer traces are valid terminal objects.

Fundamental and adjoint declarations have dimensions N and N^2-1 independently
of the Lorentz declarations. FORM's `sum` uses the current default dimension
for generated indices: switch it to N around the colour procedure, and restore
the Lorentz dimension in a fresh module before Dirac processing. Positive
substitutions do not remove inverse normalisation parameters; handle both.

An implicit word uses one pair of temporary free endpoints shared across its
matrix-valued sum, including scalar identity summands. `caLift` walks only that
sum/product skeleton. Never globally expand the Mathematica input. On import,
validate all generated endpoint/dummy multiplicities before replacing typed
tensor tokens. Restore SUNT only if the endpoints remain together on one
unambiguous chain in every term; otherwise promote the complete result.

`caPresentScalar` divides polynomial numerator/denominator factors by N^2-1
before factorisation can split it into N±1. Each division lowers degree by two.
This is scalar presentation only; do not introduce SUNSimplify into import.

`Tests/ColourAlgebra.wls` exercises the options, tensor identities, endpoint
promotion, formats and invalid inputs/results with FORM and TFORM.
`Tests/ColourRules.wls` compares interaction rules in FeynCalc's common trace
basis using Explicit -> True, SUNTraceEvaluate -> False and SUNNToCACF -> False.
Comparing a trace basis directly with an f/d basis may leave a nonzero-looking
identity; that alone is not evidence of a wrong result. Preserve the independent
translation-only rule tests and existing compatibility fixtures.

### Generator integration and colour-option compatibility

The library generator now calls `CalcFormCalculate` directly. Public
`ColourAlgebra -> True` is the default; `Automatic` enables the same processing.
Version-five mappings still record the enabled mode as `"Automatic"`; no saved
format changed. Both aliases must remain covered by the colour regression suite.

The uncontracted quark–gluon rule now retains its D-dimensional gamma matrix,
and the axion rule uses explicit rank-four dimensional `Eps` and momentum
components. Earlier verification reports describe the mixed-dimensional inputs
that existed before this migration. Current rule tests exercise the corrected
inputs directly. See [the generator guide](../Libs/Generator.md).

## Incremental propagator groups

`PropagatorGroups.wl` is a private companion loaded without I/O or processes. `buildExportData` adds layout metadata only when the registry contains denominator entries; `renderExport` fills `@GROUPING@` from those entries. The template brackets after all configured algebra and before the final sort. No `Collect` wrapper or FORM single-term argument-size dependency is introduced.

`pgWithStream` owns an input stream and closes it on normal completion, tagged failure or abort. `pgHeader` reads only the fixed marker/digest length and its line ending, so a missing newline cannot trigger an unbounded header read. `pgReadSummands` uses 1 MiB byte chunks and a nesting-aware scanner. Parenthesised coefficients are buffered as fragments; only signs at the top level are considered summand boundaries, and exponent/operator signs are preserved across chunks. The private chunk size can be reduced in tests.

`pgImport` temporarily maps denominators to private inert tokens, parses each summand through the existing restricted parsers, separates its prefactor, and retains parsed coefficients keyed by the canonical prefactor. Caches and values remain local. `pgParse` recognises native denominator-monomial times parenthesised-coefficient output and parses the coefficient separately, retaining eligibility for the existing flat parser. Other shapes use the general path. Text/token storage is released between groups. `caImplicitEligible` validates colour connectivity and contributes to one global endpoint choice; reconstruction and scalar Casimir presentation happen within each final coefficient. Legacy ungrouped files retain the existing parser path.

`Tests/PropagatorGroups.wls`, included in the runtime suite, covers FORM/TFORM equality, factored structure, tiny read chunks, signed exponents, malformed data, stream cleanup, no whole-result text import and global colour endpoint promotion. See [the measured large-case report](Tests/Reports/PropagatorGroups.md). Grouping does not remove the memory needed for the final expression or a single large coefficient.


### Common-factor extraction

`pgFactorisationQ[data]` selects grouped version-one jobs; the private name is retained for compatibility. `pgOutput[data, path]` embeds `Templates/PropagatorFactors.frm.in`. The exported program needs no installed package files.

The template enumerates denominator monomials once, retains the full result in hidden storage, and prepares up to four coefficients per worker. Free-index checks and the 20,000-term guard remain conservative eligibility conditions. A zero coefficient bypasses extraction to avoid division by zero. Each eligible coefficient's `content_` is evaluated once in the preprocessor. Scalar PaVe functions are removed from that content, so only invertible numerical, symbol and scalar-product monomials are divided out. Direct division is essential: `div_` on Laurent expressions can discard negative-power terms and must not be substituted here. No polynomial factor search is performed.

`ModuleOption inparallel` includes the newly defined primitive expressions in whole-expression scheduling. Common-factor dollar variables are read-only during that module. Output writes the common monomial and residual separately in stable group order. Temporary expressions and dollar values are cleared per batch. The queue limits expression count, not RAM; runtime timeout and cancellation remain unchanged.

`pgParseCoefficient[text, parse]` continues to parse individual factors through the restricted parser, preserving the flat-parser optimisation. No new heads, metadata or saved-format versions are introduced. Matrix, colour and epsilon jobs retain ordinary grouping. The previous native full-factorisation implementation and measurements are historical: see [parallel factorisation](Tests/Reports/ParallelCoefficientFactorisation.md). Current checks and measurements are in [common-factor extraction](Tests/Reports/CommonFactorExtraction.md).

### Second-level coefficient grouping

After common-factor extraction, `@COEFFICIENTBRACKET@` emits a native `Bracket` over the validated `Vector` entries. The bracket applies to temporary primitive expressions, not the hidden full result. Native `%E` output therefore groups momentum monomials inside each already separated denominator coefficient. With no vector entries the placeholder is empty. FORM's vector bracket also includes free components; their indices and existing free-index exclusion from content extraction remain intact.

This is the best of the tested momentum-, scalar-symbol- and dimension-based groupings on the retained sample, not a claim that one variable order is optimal for every expression. No adaptive polynomial factor search or per-subgroup content scan is introduced. The existing common factor remains outside the nested sum. An additional scan was not needed to obtain the measured benefit. Arbitrary scalar-only expressions keep the previous behaviour.

`pgParseNested` decomposes version-one coefficient sums and bracketed products before tokenisation. Adjacent summands are batched up to a private 32,768-character target; small leaves reuse the restricted flat/general parsers, preserving factored products. This target is not a hard memory bound: indivisible polynomials and fallback shapes can be larger. Parser-local recursive definitions and token references are cleared on success, failure and abort. Rejected decompositions are reparsed through the original path to preserve diagnostics. Calls, bracket powers, divisions at the product level and unrecognised shapes retain the normal parser path; recursion is capped at 64 levels. Versions containing epsilon, Dirac or colour objects retain their existing reconstruction path. The grouped reader still buffers the text of one complete propagator coefficient; this change reduces tokenisation temporaries, not the size of the final expression. There is no mapping-version or metadata change. Native output avoids `Collect` and single-term function wrappers. See [nested-grouping verification](Tests/Reports/NestedCoefficientGrouping.md).

See [nested-import measurements and limits](Tests/Reports/NestedCoefficientImport.md) for the parsing extension and memory comparison.

### Fast leaves and import-local parser state

For eligible grouped version-one imports, `withImportParser` owns the validated dictionary, dimension, token-classification cache and successful factor cache. The no-UpValues check is performed once before entering this environment. `Block` releases the state on success, failure and abort; later imports see current scalar-product definitions and a fresh dictionary. Every public import masks an outer active parser environment before selecting its own path, so a nested import cannot inherit another dictionary. Lower private test thresholds remain effective.

`pgParseNested` now descends through bracketed batches to polynomial leaves instead of passing a small but still nested batch to the general parser. Inside this environment, flat-path eligibility starts at 32 characters and does not require four local occurrences per factor: reusable factors can recur across leaves and groups. The existing restricted lexical checks and general-parser fallback remain in place. Smaller recurrent monomials use the same bounded factor decoder. Ungrouped and epsilon/Dirac/colour jobs retain their previous dispatch thresholds.

The shared decoder handles exact ASCII integer literals with `FromDigits`, the imaginary unit, and already validated scalar dictionary entries directly. It does not evaluate Wolfram source text. Other factors use the general parser, with successful results cached. Distinct integer coefficients are deliberately not cached, to avoid displacing reusable powers and momentum monomials. A leaf-local memo table avoids repeated shared-cache lookups and is explicitly cleared after the leaf.

Each shared cache has a 4,096-entry limit and a 4 MiB accounting budget. Factor keys longer than 512 characters and entries exceeding 16 KiB are not retained. A full cache is cleared before another admissible entry is inserted. These are cache limits, not a bound on process RSS, source text or the final expression. Token buffers remain temporary. `Tests/SharedParser.wls` covers differential values and failures, fast dispatch below the old threshold, bounded caches, dictionary and scalar-product isolation, and abort cleanup.

See [the matched comparison](Tests/Reports/FastNestedImport.md) for measured results and verification limits.
