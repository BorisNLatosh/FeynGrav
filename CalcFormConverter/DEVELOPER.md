# Developer guide

This guide describes implementation and maintenance of CalcFormConverter. Start with [README.md](UserGuide.md) for commands and workflows; use the original `CalcFormConverter/FORMAT.md` archived in `AGENTS.md` for the compatibility contract. Private helper names and associations may change without changing the public interface or saved format.

## Contents

- [Architecture and extension points](#architecture-and-extension-points)
- [Evaluation and resource invariants](#evaluation-and-resource-invariants)
- [Testing](#testing)
- [Performance measurement](#performance-measurement)
- [Epsilon translation](#epsilon-translation)
- [Dirac/colour companion](#diraccolour-companion)
- [Dirac processing companion](#dirac-processing-companion)
- [SU(N) colour processing](#sun-colour-processing)
- [Incremental propagator groups](#incremental-propagator-groups)
- [Single-pass commuting coefficient parser](#single-pass-commuting-coefficient-parser)
- [Numerator cancellation](#numerator-cancellation)
- [Dimension-only coefficients and direct massless cancellation](#dimension-only-coefficients-and-direct-massless-cancellation)

## Architecture and extension points

The private processing plan and first PERF-05 increment are documented in
[ProcessingContracts.md](ProcessingContracts.md). The existing preparers and renderer
remain authoritative. The common policy, pre/post collection, job-local reuse
and coordinated work limits are implemented; see [DependencyPaths.md](DependencyPaths.md)
and final verification (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)).

| Module | Responsibility |
| --- | --- |
| `CalcFormConverter.wl` | Package shell, public vocabulary and option declarations, shared failure/serialization primitives, companion loading and facade guard |
| `MappingEncoding.wl` | Supported-head tables, reversible symbol/mapping encoding and decoding, dimension selection and structural validation |
| `ExportAssembly.wl` | Stage planning, in-memory conversion, FORM rendering, template validation, transactional publication and `CalcFormExport` |
| `ImportReconstruction.wl` | Restricted and grouped result grammars, mapping validation, incremental reconstruction and `CalcFormImport` |
| `TensorDecomposition.wl` | Integral-family validation, embedded FORM recurrence and typed coefficient reconstruction |
| `LoopCoefficientReduction.wl` | Optional partial fractions/IBP in FORM, scalar recurrence planning and saved-coefficient mode |
| `FORMRuntime.wl` | Executable selection, probes, process lifecycle and calculation orchestration |
| `DiracColour.wl` | Typed matrix words, local noncommutative normalisation and reversible translation |
| `DiracAlgebra.wl` | Open-chain processing, explicit Dirac traces and spin-line metadata |
| `ColourAlgebra.wl` | Typed colour indices/endpoints, procedure configuration and scalar Casimir presentation |
| `PropagatorGroups.wl` | Propagator layout, streaming input and grouped reconstruction |
| `PropagatorCancellation.wl` | Exact routing-matrix bases and FORM numerator cancellation before grouping |
| `DimensionCoefficients.wl` | Univariate dimension simplification and ordinary-arithmetic output for remaining version-one coefficients |
| `RationalCoefficients.wl` | Shared scalar policy, supplied-kinematics snapshot, spectator classification and embedded FORM rational/factor procedures |
| `CoefficientParser.wl` | Restricted syntax validation and held reconstruction of commuting coefficients |
| `StageSymmetry.wl` | Verified pair symmetries and canonical intermediate tensors in prepared version-one jobs |
| `Templates/` | Main program, grouped coefficient output, rational/dimension processing and embedded Dirac/colour procedures |

The converter shell loads `MappingEncoding.wl`, `ExportAssembly.wl` and `ImportReconstruction.wl` first, in that order, then the specialised Dirac/colour, algebra, decomposition and runtime companions. Every companion is read inside the same converter-private context, so definition names, option symbols and the public context are unchanged. Loading performs no process execution and no directory changes. The installer's revision-aware dependency rows follow the same edges: a source that names a companion requires it, while pre-Stage-B archives that do not name these files remain installable.

A development-only `FeynGravSimplify/` directory (repository root, not under `CalcFormConverter/`) supplies the unreleased facade commands `FeynGravSimplify` and `FeynGravFORM*`; `CalcFormConverter.wl` loads its five restricted modules through a guarded one-time `Get` block so re-loading the converter cannot reset configured options. That directory also holds the restricted external adapter and its own verification suites. The facade is documented as unreleased in `Documentation/ReleaseNotes-API01-Development.md`; the published converter interface and formats are unaffected.

Companion state is explicit rather than ambient. Each companion's
`$moduleDirectory` is set while loading and retained for later template,
procedure and executable resolution; it is not a load-only local. The facade
`$FacadeLoadedMark` is the persistent readiness marker, while
`facadeGuardLoaded` and `facadeLoadResults` are short-lived loader scratch
symbols. `$showProgress` and `$backendLabel` are dynamically scoped for one
restricted call, and `$progressIntervalSeconds` is a constant reporting
interval. The initial `Facade.wl` evaluation assigns defaults to the four
independent public option stores; the converter's guarded load skips `Facade`
on a successful later `Needs`/re-`Get`, so user `SetOptions` survives. Directly
re-evaluating `Facade.wl` would re-run those initial assignments rather than
preserve them. Export registries, import dictionaries, parser caches and decoder
state are job-local. Loading launches no process or download.

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

Before choosing a connected order, `attachStageFactors[factors, ranges]`
assigns non-sum tensors to their unique neighbouring sum, using validated index
signatures. All indices of a moved factor must occur once in that factor and in
one sum. Tensors joining several sums, scalar factors and ambiguous signatures
retain their original grouping. This changes factor positions only after the
original traversal has assigned every mapping and macro identifier.

`ssLinks` uses validated, reordered stage signatures to find exactly two indices
shared by two factors. `ssPlan` emits FORM checks of the remaining partner minus
its index-swapped copy. Only a zero sorted residual enables that pair. Symmetric
placeholder tensors and native dummy-index normalisation then combine equivalent
intermediate terms; replacing the placeholders by metrics restores the original
free indices. This is a contraction identity under a verified partner symmetry,
not an assumption about an interaction family. Every other free index is retained.
Matrix/epsilon/colour jobs, ambiguous signatures, four-index connections and
networks exceeding eight prepared stages bypass this strategy. No symmetry data
is written to the mapping. `Tests/StageSymmetry.wls` compares serial and threaded
runs with the disabled strategy and independent FeynCalc contractions, including
antisymmetric and non-symmetric partners, traces and free indices.

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

The default runs all four converter suites. Installer tests always use mocks and never modify system packages. To run them separately:

```sh
python3 Tests/run.py --suite core
python3 Tests/run.py --suite form
python3 Tests/run.py --suite runtime
python3 Tests/run.py --suite integration
```

The explicit facade entry point runs the focused `FeynGravSimplify/Tests` contract
and lifecycle scripts in fresh kernels:

```sh
FG_CONTRACTOR_TEST_FIXTURE=/absolute/path/to/fixture-process \
  python3 Tests/run.py --suite facade
```

`facade` runs `Adapter`, `Loading`, `Standalone`, `Reliability` and `Facade` in
that order by default. It requires `WolframKernel`, FeynCalc, FORM and an
existing Go-built test double; it does not require a real Contractor binary and
is not part of the default `all` selection. `FORM.wls` and `Integration.wls`
remain explicit standalone suites because they require the restricted FORM
backend and a real Contractor executable respectively. The driver selects
suites only: use the task-owned bounded runner for resource enforcement. It
never installs packages.

For the planned two-kernel bounded correction, select the two phases explicitly:

```sh
FG_CONTRACTOR_TEST_FIXTURE=/absolute/path/to/fixture-process \
  python3 Tests/run.py --suite facade \
    --facade-phase Loading --facade-phase Facade
```

Every selected facade phase must write `suite-completed-<Phase>.json` with
`Suite`, integer `Passed`/`Failed` counts and `Status == "Completed"`. The driver
clears the record before the launch and requires exit 0 plus a fresh matching
record with positive passing and zero failed assertions. Add `--evidence-dir
<directory>` to retain per-phase stdout/stderr, the raw completion record and
the phase PID/exit log outside the temporary work directory. The default suite
selection is unchanged. The selector and completion protocol passed the final Stage E Loading/Facade
check (151/151), repeated after the directory rename. Other completion writers
were not rerun in those batches. Historical failed runs remain documented in
the Stage E verification report.

| Suite | Dependencies | Coverage |
| --- | --- | --- |
| `core` | WolframKernel and FeynCalc | Mapping/parser checks, nested zero-divisor diagnostics, observed native dispatch and hostile-source rejection, persisted version-one fixture, in-memory rendering, export rollback/recovery and mocked installation decisions |
| `form` | Core dependencies plus FORM | Bosonic export–FORM–import comparisons, staged and independent monolithic programs, free/contracted indices, mapping order and fallback cases |
| `runtime` | FORM dependencies; Linux/POSIX test environment | Runtime probes, timeout/cancellation and cleanup; epsilon, Dirac and colour processing; grouped input, coefficient simplification, observed cancellation sectors, dimension-stage hooks and verified stage symmetry; unchanged upstream colour preflight |
| `integration` | FORM dependencies plus FeynGrav's cubic quadratic-gravity library | Vertex/propagator comparisons, loading and namespace isolation, Dirac/colour rule comparisons and complete bubble export |
| `facade` | FORM, FeynCalc, Go test double; no real Contractor | Codec, installer/load guard, standalone loading and option stores, recovery and process reporting, facade contract and failure payloads |

Executables must be on `PATH`; `core` does not locate or launch FORM. The `runtime` suite requires Mathematica to be allowed to start external processes; a restricted execution sandbox may block it. The driver uses separate temporary directories and fresh kernels. Tests identify cases by descriptive keys, so adding a case does not change other tests' meaning. The fixed compatibility artifacts in `Tests/Fixtures` are read without regeneration.

The complete example bubble was constructed and exported on the development machine using FORM 4.3 as the available test backend. A recorded export after staged generation took **2.039 seconds**, producing **480,503 bytes** of FORM source and **6,685 bytes** of mapping from an expression with **178,961 leaves**. This measures conversion only. That initial staging check did not benchmark full-bubble FORM contraction; the later factor-normalisation measurements below cover it separately. Smaller generated programs are executed in the test suite and compared with FeynCalc. The full-bubble test checks its mapping against the original propagators, masses, indices, abbreviations and loop momenta without executing the complete FORM job. Source size varies slightly with the output path embedded in the program.


## Performance measurement

Use the [benchmark notebooks](../Benchmark/UserGuide.md) for current user-facing measurements. Compare identical workloads in sequential fresh kernels, record preparation separately, validate outside timing and retain failed/inconclusive observations. More workers are not a guarantee of better performance. Avoid inferring total-call performance from an isolated stage.

Historical optimisation measurements have moved to the dated performance report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)). Its measurements use different revisions and workloads and must not be combined into a single claimed speedup.

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

`PropagatorGroups.wl` is a private companion loaded without I/O or processes. `buildExportData` adds layout metadata only when the registry contains denominator entries; `renderExport` fills `@GROUPING@` from those entries. The template brackets after all configured algebra and before the final sort. The fallback grouping uses no `Collect` wrapper. The separately guarded rational procedure does use polynomial function arguments, so its selection limits must remain conservative.

`pgWithStream` owns an input stream and closes it on normal completion, tagged failure or abort. `pgHeader` reads only the fixed marker/digest length and its line ending, so a missing newline cannot trigger an unbounded header read. `pgReadSummands` uses 1 MiB byte chunks and a nesting-aware scanner. Parenthesised coefficients are buffered as fragments; only signs at the top level are considered summand boundaries, and exponent/operator signs are preserved across chunks. The private chunk size can be reduced in tests.

Grouped coefficient eligibility uses bulk parenthesis depths before splitting the propagator prefactor. The parenthesised-factor recogniser skips its scan when the first non-whitespace character already excludes that shape. These are syntax-only decisions: identifiers and arithmetic still pass through the existing validated parsers, including malformed-input fallback.


`pgImport` temporarily maps denominators to private inert tokens, parses each summand through the existing restricted parsers, separates its prefactor, and retains parsed coefficients keyed by the canonical prefactor. Caches and values remain local. `pgParse` recognises native denominator-monomial times parenthesised-coefficient output and parses the coefficient separately, retaining eligibility for the existing flat parser. Other shapes use the general path. Text/token storage is released between groups. `caImplicitEligible` validates colour connectivity and contributes to one global endpoint choice; reconstruction and scalar Casimir presentation happen within each final coefficient. Legacy ungrouped files retain the existing parser path.

The summand reader computes parenthesis depths in bulk for each fixed-size
chunk. Only openings and closings at depth zero need individual handling; nested
spans remain intact. Negative-depth events are retained in source order, so an
unknown identifier in an earlier complete summand still takes precedence over a
later unmatched closing parenthesis. Depth and signed-exponent state carry across
chunk boundaries. No whole-file text buffer is introduced.

`Tests/PropagatorGroups.wls`, included in the runtime suite, covers FORM/TFORM equality, factored structure, tiny read chunks, signed exponents, malformed data, stream cleanup, no whole-result text import and global colour endpoint promotion. See [the measured large-case report](Tests/Reports/PropagatorGroups.md). Grouping does not remove the memory needed for the final expression or a single large coefficient.


### Common-factor extraction

`pgFactorisationQ[data]` selects grouped version-one jobs; the private name is retained for compatibility. `pgOutput[data, path, rationalPlan, dimensionPlan]` embeds `Templates/PropagatorFactors.frm.in`. The exported program needs no installed package files.

The template enumerates denominator monomials once, retains the full result in hidden storage, and prepares up to four coefficients per worker. Free-index checks and the 20,000-term guard remain conservative eligibility conditions. A zero coefficient bypasses extraction to avoid division by zero. Each eligible coefficient's `content_` is evaluated once in the preprocessor. Scalar PaVe functions are removed from that content, so only invertible numerical, symbol and scalar-product monomials are divided out. Direct division is essential: `div_` on Laurent expressions can discard negative-power terms and must not be substituted here. This describes the fallback path; the bounded rational procedure below performs a factor search only for selected coefficients.

`ModuleOption inparallel` includes the newly defined primitive expressions in whole-expression scheduling. Common-factor dollar variables are read-only during that module. Output writes the common monomial and residual separately in stable group order. Temporary expressions and dollar values are cleared per batch. The queue limits expression count, not RAM; runtime timeout and cancellation remain unchanged.

`pgParseCoefficient[text, parse]` continues to parse individual factors through the restricted parser, preserving the flat-parser optimisation. No new heads, metadata or saved-format versions are introduced. Matrix, colour and epsilon jobs retain ordinary grouping. The previous native full-factorisation implementation and measurements are historical: see [parallel factorisation](Tests/Reports/ParallelCoefficientFactorisation.md). The [common-factor extraction report](Tests/Reports/CommonFactorExtraction.md) records that intermediate implementation; later complete-call and review checks are in [EarlyDimensionCompaction.md](Tests/Reports/EarlyDimensionCompaction.md).

### Second-level coefficient grouping

After common-factor extraction, `@COEFFICIENTBRACKET@` emits a native `Bracket` over the validated `Vector` entries. The bracket applies to temporary primitive expressions, not the hidden full result. Native `%E` output therefore groups momentum monomials inside each already separated denominator coefficient. With no vector entries the placeholder is empty. FORM's vector bracket also includes free components; their indices and existing free-index exclusion from content extraction remain intact.

This is the best of the tested momentum-, scalar-symbol- and dimension-based groupings on the retained sample, not a claim that one variable order is optimal for every expression. No adaptive polynomial factor search or per-subgroup content scan is introduced. The existing common factor remains outside the nested sum. An additional scan was not needed to obtain the measured benefit. Arbitrary scalar-only expressions keep the previous behaviour.

`pgParseNested` decomposes version-one coefficient sums and bracketed products before tokenisation. Adjacent summands are batched up to a private 32,768-character target; small leaves reuse the restricted flat/general parsers, preserving factored products. This target is not a hard memory bound: indivisible polynomials and fallback shapes can be larger. Parser-local recursive definitions and token references are cleared on success, failure and abort. Rejected decompositions are reparsed through the original path to preserve diagnostics. Calls, bracket powers, divisions at the product level and unrecognised shapes retain the normal parser path; recursion is capped at 64 levels. Versions containing epsilon, Dirac or colour objects retain their existing reconstruction path. The grouped reader still buffers the text of one complete propagator coefficient; this change reduces tokenisation temporaries, not the size of the final expression. There is no mapping-version or metadata change. Native output avoids `Collect` and single-term function wrappers. See [nested-grouping verification](Tests/Reports/NestedCoefficientGrouping.md).

These are the retained smaller-piece fallback paths. The single-pass parser below is tried first on eligible bracketed coefficients. See [nested-import measurements and limits](Tests/Reports/NestedCoefficientImport.md) for their historical development and memory comparison.

### Fast leaves and import-local parser state

For eligible grouped version-one imports, `withImportParser` owns the validated dictionary, dimension, token-classification cache and successful factor cache. The no-UpValues check is performed once before entering this environment. `Block` releases the state on success, failure and abort; later imports see current scalar-product definitions and a fresh dictionary. Every public import masks an outer active parser environment before selecting its own path, so a nested import cannot inherit another dictionary. Lower private test thresholds remain effective.

`pgParseNested` now descends through bracketed batches to polynomial leaves instead of passing a small but still nested batch to the general parser. Inside this environment, flat-path eligibility starts at 32 characters and does not require four local occurrences per factor: reusable factors can recur across leaves and groups. The existing restricted lexical checks and general-parser fallback remain in place. Smaller recurrent monomials use the same bounded factor decoder. Ungrouped and epsilon/Dirac/colour jobs retain their previous dispatch thresholds.

The shared decoder handles exact ASCII integer literals with `FromDigits`, the imaginary unit, and already validated scalar dictionary entries directly. It does not evaluate Wolfram source text. Other factors use the general parser, with successful results cached. Distinct integer coefficients are deliberately not cached, to avoid displacing reusable powers and momentum monomials. For grouped commuting leaves, a local association decodes distinct factors once; bulk lookup and additive-boundary lists then construct each product once. Typed operands and zero divisors are checked before reconstruction. A speculative failure replays the original ordered parser to preserve the first diagnostic. Imports outside the shared environment retain the ordered parser and its leaf-local memo table, which is explicitly cleared after the leaf.

Each shared cache has a 4,096-entry limit and a 4 MiB accounting budget. Factor keys longer than 512 characters and entries exceeding 16 KiB are not retained. A full cache is cleared before another admissible entry is inserted. These are cache limits, not a bound on process RSS, source text or the final expression. Token buffers remain temporary. `Tests/SharedParser.wls` covers differential values and failures, fast dispatch below the old threshold, bounded caches, dictionary and scalar-product isolation, and abort cleanup.

See [the matched comparison](Tests/Reports/FastNestedImport.md) for measured results and verification limits.

Historical intermediate measurements for bulk polynomial reconstruction, before the complete-import target was met, are recorded in [bulk coefficient import](Tests/Reports/BulkCoefficientImport.md).

## Single-pass commuting coefficient parser

`pgParseNestedChecked` first tries `pgParseCoefficientTree` on bracketed text of
at least 128 characters in the shared version-one environment. Other mapping
versions, symbols with UpValues, smaller inputs and flat polynomials retain the
previous paths. This is an internal dispatch threshold, not a public option.

The lexer reuses the flat-factor vocabulary plus parentheses and arithmetic
operators, and requires exact lexical coverage. `pgCoefficientSyntax[]` lazily
builds a WVM `CompiledFunction` that checks integer token codes, operand/operator
alternation and parenthesis depth. It evaluates no mapped values and allocates
no node array. The 64-level parenthesis limit selects fallback for deeper input.

After syntax validation, `importFactor` decodes each distinct factor. The parser
then builds a **new, trusted string** from numbered slots (`#1`, `#2`, ...) and
six fixed arithmetic operators. No source identifier, numeric literal, mapping
value or source substring is copied into it. Spaces around signs prevent native
increment/decrement syntax. `ToExpression[..., InputForm, HoldComplete]` parses
this generated construction without evaluating it; the slots are subsequently
replaced with validated dictionary values using a local dispatch table. This is
not a route for evaluating FORM text as Wolfram code. See Wolfram's
[`HoldComplete` documentation](https://reference.wolfram.com/language/ref/HoldComplete.html)
for the held construction and input-transformation behaviour.

Reciprocal operands are checked from the inside out before releasing the final
arithmetic, including denominators inside a term multiplied by zero. Typed
operands have already been checked. Failed speculation replays the original
nested/general path to retain first-failure ordering. Abort propagates. The
compiled validator stores no dictionaries or input arrays; all coefficient data
is local. No C compiler, process launch, `Simplify` or expansion is introduced.

This path tokenises one complete coefficient, not the complete output file.
Working memory therefore scales with the largest coefficient and the returned
expression. The smaller-piece parser remains the fallback. No saved-format,
public-API or mathematical-vocabulary changes are involved. `Tests/TreeParser.wls`
covers structure, arithmetic, fallback, first-failure order and cache isolation.
See the [original parser measurements](Tests/Reports/CoefficientTreeImport.md) for the earlier depth-by-depth reconstruction, and the [current optimisation and review record](Tests/Reports/EarlyDimensionCompaction.md#independent-review-corrections) for held reconstruction and its regression coverage.

### Focused rational-coefficient experiment

[Tests/RationalCoefficient](Tests/RationalCoefficient/VerificationGuide.md) reproduces the
compact first-term result from `One-Loop-Counterterms.nb` using only FORM
rational-polynomial arithmetic and factorisation. It includes directly runnable
FORM files, a test-only preparation adapter, serial/TFORM equality checks and
[a measured report](Tests/Reports/RationalCoefficient.md).
This records the preliminary experiment. A guarded version is now enabled in the public export/calculation pipeline; its single-term reduction must not be extrapolated to all coefficients. See the integration notes below.

### Integrated rational coefficient processing

`renderExport` constructs one `rcPlan` from the validated encoded mapping and
passes it to declarations and `pgOutput`. It recognises reciprocal polynomial
abbreviations structurally, without `ToExpression`, expansion or simplification
in Mathematica. Unsupported abbreviation vocabularies and excessive variable
counts disable this procedure for the job. The initial work limits are in
`RationalCoefficients.wl` and are private implementation choices.

`RationalCoefficients.frm.in` selects coefficients by term count and excludes
free indices, imaginary factors and scalar master functions. Original
coefficients remain available for fallback. `PolyRatFun` collects each selected
coefficient; numerator/denominator capture is serial and separately initialised
for empty expressions. Factorisation uses whole-expression scheduling. A
residual object causes fallback rather than being discarded.

`RationalCoefficientOutput.frm.in` writes factor values and restores temporary
scalar products. Keep a newline before the generated `#else`: otherwise FORM
can skip the fallback output entirely. Unsupported coefficients in mixed and
all-unsupported batches are explicit regression cases. Restore ordinary
expression selection before creating fallback primitives and keep factorised
numerator/denominator expressions out of their momentum-bracketing module.

No new public options, mapping version, mathematical heads or importer changes
are involved. `Tests/RationalCoefficients.wls` runs through the real public API
with serial FORM and TFORM and is part of the runtime suite. The
[integration report](Tests/Reports/RationalCoefficientIntegration.md) records
all-suite results and the realistic replay, including the failed wider-limit
trial. A term/variable limit is not a hard bound on polynomial arithmetic cost;
normal runtime timeout, cancellation and failure-file retention still apply.

## Numerator cancellation

`buildExportData` records private `CancellationData` with denominator names, rational routing coefficients and mass squares emitted through the existing scalar serializer. This data is not a mapping-format extension. `pcPlan[data]` constructs independent row subsets of the routing matrix and deterministic pivot columns. Only small rational matrices are inverted; numerator expressions are never expanded or simplified in Wolfram.

The FORM stage runs after colour/Dirac/epsilon processing and a normalising sort, before final bracketing. Native scalar products become temporary commuting scalar aliases. An if/elseif chain selects the first, largest independent subset whose propagators are present in that term. Pivot substitutions contain only reciprocal denominator identifiers and non-pivot scalar products. FORM expands and cancels powers; residual negative powers of denominator identifiers are restored to their quadratic polynomials, then aliases become native scalar products again. A final sort combines results before grouping. The ordering is acyclic; there are no repeated reciprocal substitutions or partial-fraction rules.

The private `$pcMaximumBases = 256` budget limits generated branching. An exceeded budget skips the general basis stage; preceding direct massless cancellations remain intact. It does not reject the input or bound execution time. Zero rows do not enter a basis. `LoopMomenta` affects pivot priority, not algebraic validity. Constants, irreducible numerators, common infinitesimal prescriptions and positive repeated propagator powers are preserved under formal rational identities. Matrix/colour factors commute with the scalar substitutions.

`Tests/PropagatorCancellation.wls` checks serial and TFORM results against explicit rational identities and FeynCalc with FDS and DropScaleless disabled. It also checks fallback, retained constants and absence of private output symbols. No Mathematica Simplify is used in the production stage.

Before the substitutions, FORM keeps a hidden backup of the complete expression. It compares sorted term and denominator-group counts after cancellation, restoring the backup if terms exceed both 16 and twice the original count, or groups exceed four times the original count. Small growth is allowed because even `l^2/(l^2-m^2)` needs two terms. The group-count guard matters: a candidate can have similar expanded size yet lose compact coefficient factorisation by spreading across many propagator products. Counting uses bracket keys with a serial dollar collector; all backups are dropped before the existing grouping stage. This is a job-wide fallback, not a guarantee of minimal size or bounded intermediate memory.

`pcSectorProcessing` can apply the same rewrite to independent sectors before
assembling a candidate. The planner restricts this to version-one jobs and
vectors absent from every denominator routing. Monomials involving those vectors
cannot change under the scalar-product substitutions; consequently terms in
different sectors cannot cancel each other. The sum of completed sectors' sorted
term counts is therefore an exact lower bound for the complete candidate. Once
it exceeds both 16 and twice the original term count, the original growth guard
must reject the job: remaining sectors need not be evaluated. Otherwise all
sectors are reassembled before the group-count guard. The default strategy
requires at least 1,024 input terms and between 2 and 32 sectors; other cases use
the whole-expression path. A term-count rejection also skips the unnecessary
post-cancellation denominator-group count. The backup, acceptance thresholds and
cancellation identities are unchanged. Tests force both strategies on small
serial/TFORM cases and compare their returned expressions exactly. Temporary source probes additionally assert accepted assembly, rejection after sector 1 of 2 and restoration of the backup. A generated-code substring alone does not establish execution of these paths.


## Dimension-only coefficients and direct massless cancellation

`pcDirectProcessing` reads validated `CancellationData`. For zero mass and a single nonzero rational routing coefficient `a`, it emits `p.p*P = 1/a^2` in a repeated FORM module. Composite routes and nonzero masses stay with `pcPlan`. The general stage takes its backup afterwards; its growth guard therefore preserves direct cancellations.

`dcfPlan` uses the encoded dimension and scalar dictionary, without evaluating saved expressions. It recognises inverse dimension-only polynomials (outer powers down to -8, polynomial powers up to 32). Unrecognised abbreviations stay opaque. `dcfReduction` runs after bounded multivariate reduction, only on remaining coefficients containing the dimension or recognised abbreviations. It does not move other scalar variables or tensor factors into `PolyRatFun`. Coefficients are scheduled as whole expressions in the existing bounded batches.

`DimensionCoefficientOutput.frm.in` uses native `#do item=expression` iteration over the reduced terms and writes their commuting/tensor factors multiplied by ordinary dimension quotients. The iterator takes one expression snapshot rather than repeatedly copying the remaining polynomial and subtracting its first term. Temporary dollar expressions are cleared after each coefficient. The existing restricted importer handles this grammar unchanged. This reduces intermediate algebra compared with multivariate factorisation, but neither output traversal nor any single coefficient has a fixed memory/time guarantee. Epsilon, Dirac and colour coefficients retain their existing output paths.

`Tests/DimensionCoefficients.wls` tests both engines, dimension aliases, opaque factors, free indices, complex coefficients, scalar functions, batching, repeated massless cancellations and exclusions. See the [integration report](Tests/Reports/DimensionCoefficients.md) for measured scope and limits.

### Early dimension compaction

`renderExport` calls `dcfStageSort[plan, beforeSort]`, passing `pcDirectRules` explicitly as the non-growing pre-sort hook. An empty plan gives an ordinary sort. The dimension module owns abbreviation restoration through `dcfStageRestore[plan]`; the renderer does not insert code by replacing sort strings. Dimension compaction is selected when a version-one algebra capability set and prepared tensor factors are both present. Active tensor-decomposition jobs use their `AlgebraVersion` for pre-integration dimension compaction, verified stage symmetries and cancellation-sector planning; their saved mapping and output remain version six. Do not gate these pre-integration plans on the result-format version, or enabling decomposition silently disables the existing large-expression optimisations. Each boundary first sorts ordinary tensor algebra with rational functions inert, enables `PolyRatFun` for the dimension-only rules on the collected terms, then disables it and restores numerical ratios. `ToPolynomial,OnlyFunctions,cfcDimRat` abbreviates only the resulting scalar rational functions during multiplication. `FromPolynomial` restores them before each rational pass and after the last prepared stage. `pcDirectRules` supplies the same non-growing massless identities before each stage sort and in the final direct cancellation pass. Keeping `PolyRatFun` enabled throughout tensor multiplication was tested and rejected as too costly.

`dcfPrecondition` runs after direct massless cancellation and before `pcPlan` processing when an eligible result exceeds the private 1,000-term threshold. The threshold selects work; it is not a time/memory limit. The original general cancellation pass, backup and growth guards are retained. The sector strategy can reject some candidates early; the guards still cannot prevent every expensive candidate.

The group selector recognises existing `cfcDimRat` coefficients and sends them to dimension-only output. They bypass multivariate selection because their compressed term count understates the original polynomial complexity. No private function may enter a result file. `Tests/DimensionPreconditioning.wls` forces the early path and prepared stages on small cases, comparing both engines against independent FeynCalc algebra.

Performance evidence and power-state qualifications are recorded in [EarlyDimensionCompaction.md](Tests/Reports/EarlyDimensionCompaction.md).

### Independent optimisation review follow-up

The coefficient parser checks reciprocal operands in the inside-out order
returned by `Cases`, before releasing the held arithmetic. Reversing that order
can emit a kernel division-by-zero message before the intended failure. Tests
now compare message behaviour for nested zero divisors, call the native parser
directly with hostile source text, and instrument the dispatcher to prove it
actually attempts the new path.

Cancellation tests instrument temporary generated programs to observe visited
sectors, accepted assembly, rejection before the final sector and restoration
of the backup. Both serial FORM and TFORM must produce the whole-expression
reference result. The shipped programs contain no test instrumentation.

## One-loop tensor decomposition

`tdPrepare` is called after ordinary serialisation, before rendering or publication. It validates the two boolean options, rejects multiple loops and hidden loop dependence, normalises rational routing coefficients and coalesces identical denominators. Small routing expressions may be expanded; the supplied tensor expression is not globally expanded in Wolfram.

`tdProgram` embeds `cfcTensorDecompose` in the generated source. Each monomial acquires a denominator-power tuple. The first active denominator fixes the shift, which applies to the complete numerator. `ToTensor functions` exposes loop occurrences; the recurrence assigns its first slot to an active chord or pairs it with one remaining slot using `distrib_`. Labels are sorted only after recurrence. Vacuum tensors use `dd_`. Odd vacuum ranks are explicitly removed. FORM sorts and brackets the resulting `cfcTC` functions.

The recurrence is independent of rank and has no hand-written high-rank table. Test placement multiplicities before changing it: repeated labels collect algebraically, but distinct metric pairings must each have unit weight. Scalar-master reduction and Gram inversion do not belong here.

`tdAlgebraMapping` selects legacy capability validation. `tdImportSetup`, `tdCall` and `tdValidateGroup` validate metadata and the integral grammar. The streaming propagator reader also accepts typed integral prefactors; final `tdReconstruct` supplies PaVe or the inert generalised coefficient. Scaleless classification takes place here, using known masses and external invariants, without changing global FeynCalc settings. Cache state is import-local.

Tests: `TensorDecomposition.wls` includes independent TID comparisons, repeated-power mass derivatives, convergent radial moments, mixed matrix/epsilon calculations, serial/TFORM agreement and malformed streaming results. Existing integrand benchmarks explicitly disable decomposition. Run the runtime suite outside sandboxes that block Wolfram subprocess creation. See [the verification report](Tests/Reports/TensorDecomposition.md) for completed coverage and limits.

### Definition at degenerate kinematics

For a nonempty family let \(\nu=\sum_j\nu_j\), \(r_0=0\), and
\[
\Delta(x)=\sum_jx_jm_j^2-\sum_{i<j}x_ix_j(r_i-r_j)^2,
\qquad \sum_jx_j=1.
\]
In the explicit normalisation \(1/(i\pi^2)\int d^Dq\), the coefficient with
\(k\) metric pairs and \(t\) chord slots has the parameter definition
\[
C_{0^{2k}i_1\cdots i_t}=
\frac{(-1)^{\nu+k+t}\pi^{D/2-2}\Gamma(\nu-D/2-k)}
 {2^k\prod_j\Gamma(\nu_j)}
\int_{x_j\geq0}\!\left[\prod_j dx_j\,x_j^{\nu_j-1}\right]
\delta\!\left(1-\sum_jx_j\right)
\left[\prod_{a=1}^{t}x_{i_a}\right]
[\Delta(x)-i0]^{D/2+k-\nu}.
\]
This fixes coefficient labels even when the tensor basis is redundant. It is a
convention/analytic-continuation definition, not an integration algorithm in
this release. Scaleless objects remain formal by default. No additional
renormalisation-scale or measure prefactor is inferred from PaVe notation.
For empty families, the even-rank coefficient is the formal radial moment
\(\int d^Dq\,(q^2)^k/[i\pi^2 D(D+2)\cdots(D+2k-2)]\); odd moments vanish.

Version-one-only coefficient compression remains restricted to its established
vocabulary. Version-six output uses ordinary FORM sorting and integral grouping;
no speedup or bounded final-expression memory is promised by this extension.

### Tensor-decomposition performance safeguards

Pre-integration plans use `AlgebraVersion`, not the version-six result marker.
This retains verified tensor-stage symmetries, dimension compaction and
cancellation-sector separation for eligible commuting Lorentz expressions.
After decomposition, another dimension-only rational pass collects the
coefficients produced by metric contractions. Version-six reconstruction
validates `cfcDimRat` as an exact rational function of the recorded dimension.

The tensor procedure selects an even number of metric slots with native
`sum_` and `distrib_`, then pairs those slots using `dd_`. The loop tensor is
symmetric, so sorting its slots places repeated arguments together and lets
FORM account for their multiplicities. Only the remaining chord assignments
use the recurrence. This avoids generating metric placements by a recursive
pairing tree; no rank ceiling or factorial averaging is introduced.
See the [FORM function reference](https://form-dev.github.io/form-docs/stable/manual/)
and the [tensor-decomposition verification](Tests/Reports/TensorDecomposition.md).

The grouped reader also selects the existing bounded coefficient parser by
algebra capability. An integral-coefficient prefactor is split from its scalar
coefficient, checked by `tdCall`, and retained for group validation. Complete
`cfcDimRat` calls are reusable lexical factors only inside active decomposition;
the general parser validates them before they enter the import-local cache.
Matrix/epsilon/colour coefficients and symbols with arithmetic UpValues retain
the general path. The reader continues to buffer at most one group, with
coefficient-sized temporary data, rather than the entire result file.

For the integer-polynomial subset of `cfcDimRat`, `tdFastDimensionRatio` checks
complete lexical coverage and mandatory signs between monomials, then constructs
exact powers and sums using `FromDigits`. It never evaluates result text as
Wolfram code. Denominator-polynomial caching is scoped by `withImportParser`,
bounded to 512 entries of at most 16 KiB each, and reset between imports.
Unknown variables, adjacent unsigned monomials, nested calls and other grammar
return to the general parser.


## Repeated-power coefficient reduction

`LoopCoefficientReduction.wl` owns the opt-in `ReduceLoopCoefficients` stage.
`lrPrepare` follows `tdPrepare`; it either extends an active decomposition job
or creates the explicitly marked saved-coefficient mode. `lrPlan` prepares
small exact scalar Cayley matrices, using `Together` and `Inverse` only on those
matrices. It registers their inverse entries as ordinary scalar abbreviations.
No amplitude-wide Mathematica simplification or FeynCalc reduction is used.

`lrIsolatedProcessing` collects distinct original integral keys using
`Keep Brackets`, then hides the amplitude. The unchanged recurrence runs on a
lookup expression whose terms carry their original key. Canonicalisation
compares ordered masses and relative chords for formal coefficients, and
ordered masses and invariants for ordinary unit-power PaVe coefficients.
`lrOutput` indexes the lookup by the **final** integral key and assembles its
complete multiplier. Laurent-monomial abbreviations are exposed so native
mass factors can cancel. The dimension-only scalar pass combines rational
functions of the dimension. `LoopDenominatorGroups.wl` describes the rational
abbreviation vocabulary without simplifying it in Wolfram. FORM factors the
small abbreviation table, normalises reciprocal factors and collects complete
denominator products in each final multiplier. Scalar identity groups and
preserved singular families follow the same path. Products of coefficients
retain the general recurrence.

Nonzero linear scalar-master jobs with algebra version one always use shared
numerator factors. Their small input dictionaries can grow substantially during
reduction, so the input term count is not a suitable output-size predictor.
Repeated-power-only jobs retain the 4,096-term input threshold. The nonlinear
coefficient-product and algebra eligibility guards are unchanged. This selection
reuses the existing writer and mapping capabilities; it changes no recurrence,
format version or importer. `Tests/MasterOutputSharing.wls` compares it exactly
with the former direct-output selector on manageable fixtures.

Final commuting master multipliers now have a bounded rational pass before
complete denominator grouping. `rcMasterPlan` and
`Templates/MasterRationalCoefficients.frm.in` reuse the native power translation
from `RationalCoefficients.wl`; the original propagator-specific guards remain
unchanged. This pass does not invoke Wolfram `Simplify` or consume `$Assumptions`.

Each shared definition receives conservative metadata: an expanded-term estimate,
a maximum absolute degree per term (including denominator factors), and the union
of scalar variables. The metadata contains counts/markers, not expanded copies of
shared polynomials. Child estimates are combined multiplicatively for products
and additively for sums. Unknown or unavailable dependencies reject the candidate.
The current selection limits are 1,000 estimated terms, degree 24 and 12 actual
variables per coefficient; unrelated mapping entries do not count. These limits
are heuristics for selecting work, not hard resource or GCD-complexity bounds.

Only selected coefficients expose their shared definitions in a temporary FORM
expression. `PolyRatFun` combines fractions and cancels polynomial factors.
Temporary dot-product symbols and the polynomial placeholder for `i_` are restored
before output. Zero coefficients reconstruct as zero; their empty group frames
are retained to honour the existing incremental grammar. Other results regain
shared numerator/denominator definitions. Newly created denominator references
must be included in the pending-definition scan, even when nested in `cfcLRDen`.
No mapping version, recurrence identity or reader behaviour changes.

FORM `Skip` applies only to its current module: the metadata and arithmetic
procedures renew expression selection after each sort. Table metadata uses a
preprocessor snapshot of its value, not a reference to a mutable dollar variable.
These invariants prevent inspection from changing the coefficient being inspected.
`Tests/MasterRationalCoefficients.wls` covers exact cancellation, complex arithmetic,
dot products, fallback, manual/tiny-chunk import and unchanged mappings. Matrix,
epsilon, nonlinear coefficient-product and unshared paths retain their existing
behaviour; this is not unrestricted rational simplification of every output.

The embedded
`LoopDenominatorGroups.frm.in` first exposes denominator factors in the original
amplitude blocks and recurrence weights separately. Final assembly joins them,
cancels matching exposed factors and brackets complete denominator products.
Cheap common-factor extraction retains numerator sharing. `ldgCommonFactors`
first shields non-polynomial objects with `ToPolynomial` inside the arguments,
uses `OldFactArg`, then restores them with `FromPolynomial`. This round trip is
necessary: the tests include a three-term mixed-function/scalar-product argument
for which unshielded `OldFactArg` in FORM 5.0.2 changes a dot-product power.
`ldgNegativePowerRules` uses the native left-hand-side syntax `p.q^n?neg_`;
parenthesising that base as `(p.q)^n?neg_` does not match inverse scalar products.
Arguments containing free Lorentz indices skip this optional content pass,
while retaining denominator collection and sharing. The guard temporarily
represents native metrics with `cfcDGMetric`, because FORM's `occurs` condition
does not inspect the indices in `d_`. That marker is restored before output.
Matrix and epsilon jobs keep direct assembly.

The writer records identifiers actually used by each group, using a sparse
FORM table of previously written identifiers. Allocation ranges are insufficient:
FORM can reuse an old extra-symbol identifier. It restores temporary polynomial
aliases before writing definitions and never dumps the entire extra-symbol table.

`SharedCoefficientReader.wl` owns legacy and incremental dictionary reading.
A 16 KiB buffer handles records across read boundaries. Batches contain at most
64 records and stop accumulating at 256 KiB; a single record may be larger.
Speculative batch parsing falls back to ordered parsing on failure. Pending
truncation errors are reported after earlier complete records have been checked.
Definitions may reference only earlier validated definitions when authorised by
metadata. Integral tokens and undecomposed loop dependence are rejected before
replacement; validated subtrees need not be scanned repeatedly.

Framed integral groups are parsed separately. Definitions and caches are local
to the import; the existing stream owner closes files on success, failure and
abort. Reconstruction preserves ordinary products and sums without executing
result text as Wolfram assignments. These buffers bound temporary text, not the
size of a coefficient or the final expression.

Earlier `cfcLRReduced`/`cfcLRReducedPart` files remain readable under their
`CoefficientBlocks` metadata. Those wrappers represent a reduced combination
times an original amplitude bracket, rather than the new final-key grouping.
Coefficient reconstruction keeps its existing bounded per-import cache. Future
algebra must process hidden lookup values as well as the amplitude: changing
only `cfcResult` after isolation leaves the lookup untouched. See the official
FORM descriptions of [brackets and extra symbols](https://form-dev.github.io/form-docs/stable/manual/).

Dimension-coefficient compaction runs before isolated reduction, so downstream
sorts cannot expand the reduced combinations through the amplitude.

All coefficient replacement, power lowering and branching execute in the
standalone FORM program. The implementation uses these identities:

- For a positive tensor label j,
  `C[...,j](nu) = -2 nu_j C[...,0,0](nu+e_j)`.
- Same-route denominators satisfy `D_j-D_i = m_i^2-m_j^2`; partial fractions
  lower their total power before any matrix recurrence.
- With `Y_ij = m_i^2+m_j^2-(r_i-r_j)^2` and `d = D+2h`, integration by parts gives
  `Sum_j Y_ij nu_j J(nu+e_j) = (d-Sum_j nu_j) J(nu)
   - Sum_j nu_j J(nu+e_j-e_i)`.
  Select a raised power, solve with the corresponding row of inverse Y, and
  express every RHS integral at strictly smaller total power. Here `2h` is the
  number of zero tensor labels. All terms share the same conversion between
  these scalar integrals and metric coefficients, so no additional pi or
  dimension-shift normalisation enters the recurrence.

This is a coefficient-basis use of the dimension-shift strategy described by
[Davydychev and Tarasov's one-loop reduction literature](https://arxiv.org/abs/hep-ph/9907327).
The explicit recurrence above fixes the implementation's signs and convention.
Unit-power coefficients with their original tensor labels are left alone.
The reduction considers at most four distinct routes per active subfamily;
subsets are generated from route groups, avoiding the full mass-dictionary
powerset. Singular matrices are skipped. Remaining formal coefficients are
reconstructed faithfully rather than labelled as fully reduced.

The importer distinguishes integral mode from saved-coefficient mode before
validating groups or applying normalisation. Caches stay import-local. The
scalar dictionary also validates planner-generated expressions before files
are written. `Tests/LoopCoefficientReduction.wls` checks exact identities,
independent parameter integrals, both execution engines, saved expressions,
scaleless and singular cases, mapping failures and chunk boundaries.


With master reduction disabled, saved-coefficient jobs use version seven because they must not acquire the
`I Pi^2` factor associated with integrating an integrand. Active decomposition
continues to use version six. Active master jobs use version eight and record the source interpretation. A mode/version mismatch is an error. Reduction
metadata freezes scalar-product invariants: validate this snapshot before
restoring inverse scalar abbreviations, so a changed singular point cannot
trigger division by zero during decoding.

Convert labelled parameter moments **before** merging coincident denominator
slots. Their Pochhammer factors depend on the original individual powers;
merging first changes the answer. The production FORM loop performs one
lowering step per term followed by `.sort`, with a maximum-reduced change flag
for TFORM. This combines duplicate intermediate integrals before branching
again. Direct reduction at a numerical dimension is disabled, since a zero
recurrence factor can multiply an ultraviolet pole. The focused tests include
both of these boundary cases. `LoopCoefficientParameters.wls` supplies eight
independent massive Euclidean triangle/box parameter comparisons, using only
Wolfram's built-in numerical linear algebra and two fixed quadrature grids.


## Scalar-master reduction

`ScalarMasterReduction.wl` supplies private family recurrences and the
`ReduceToScalarMasters` orchestration. The default enabled stage includes the
existing power reduction, while disabling it leaves historical behaviour intact.

For h metric pairs the normalised coefficient is (-1/2)^h Pi^(D/2-2) times
the scalar integral in D + 2h with the dimension-normalised measure. The
common Pi factor does not change during the recurrence. Positive labels first
raise powers using the existing parameter identity. Equations (15), (17) and
(33) of Fleischer–Jegerlehner–Tarasov (hep-ph/9907327) then lower dimensions or
pinch a line. Each dimension-lowering step has an additional -1/2. Massive
one-point coefficients use their Gamma-function ratio. At zero momentum, equal
mass bubble parameter polynomials coalesce exactly; unequal masses use the
zero-Gram pinch identity. No epsilon expansion is performed.

The planner reuses exact small family matrices, omits incompatible invariant
families and selects at most one mass per momentum route after partial fractions.
The existing coefficient-key isolation keeps amplitude multipliers outside the
recurrence. One recurrence step per module allows FORM sorting to combine
branches before the next layer. Final assembly reuses denominator collection
and incremental shared-block output. There is no persistent kinematic cache.

Private `smChord` values encode only ordered invariant distances for saved
PaVe inputs. They are never declared as FORM vectors. Same-family distances are
validated; cross-family combinations are excluded. Known singular and scaleless
input states are protected before positive-label conversion so unsupported
states can retain their original coefficient interpretation.

The new tests compare with independent PaVeReduce results and analytic
zero-momentum identities. Existing parameter quadrature tests also exercise
master reduction. Historical coefficient-only tests opt out explicitly. See the
verification report for completed checks and outstanding limitations.

Within each recurrence layer, `Bracket+ cfcIR,cfcTC,cfcSMHold` followed by a sort
and `Keep Brackets` applies the next rule once to each shared intermediate state.
The original coefficient identifiers stay in the bracket payload. This shares
work across distinct requested coefficients without exposing amplitude factors.
The private `$smShareStates` switch exists for equivalence measurements only.
Family eligibility is cached within each export/import and includes the
scaleless policy; it is never retained between calls.

FORM can wrap very large exact integers using a backslash followed by a newline.
`normaliseFORMNumbers` joins only decimal-digit continuations. It runs on an
individual parsed group or shared definition, never on the entire streamed file.
Identifier continuation remains invalid. General, grouped and shared readers
use the same rule. See [the verification report](Tests/Reports/ScalarMasterReduction.md)
and [raw performance observations](Tests/Reports/ScalarMasterPerformance.json).

Scalar unit-power tokens also merge under permutations of their complete mass
and invariant-distance graph. Tensor coefficients are excluded from this rule.
The chosen representative is an existing family, so no new routing is invented.

### Parallel materialisation before integral block expansion

`Templates/LoopDenominatorGroups.frm.in` sorts an extracted integral bracket before substituting original amplitude blocks when `NTHREADS_ > 2`. FORM includes its master in that count. This gives multiple TFORM workers terms to expand; serial FORM and one-worker TFORM skip the barrier. The change affects scheduling only, with no public-option or format change. `Tests/Fixtures/MaterialiseBlocks.frm`, exercised by `LoopDenominatorGroups.wls`, compares baseline and materialised results including cancelled, missing and free-index blocks.

A bounded realistic trial completed FORM after this change, but its import remained incomplete. Sampled per-process RSS limits do not constitute an enforced aggregate memory cap. Do not infer complete-call success from backend completion.

## Scalar-master export planning

Coefficient registration in `lrPlan` uses a job-local association keyed by held,
evaluated expressions. Decode existing scalar/abbreviation entries once and keep
the first matching name. Preserve structural equality, allocation order and
validation; do not introduce algebraic normalisation or a persistent cache.

`tdDenominatorSupports` records which denominator identifiers can coexist in the
normalised expression without expanding its numerator. `tdPrepare` maps these to
loop slots, excluding spectator denominators. Saved coefficients provide their
own supports through `lrSavedSupports`. `lrReachableFamilies` supplies possible
pinches to the power/master planners and canonicalisers. Tensor moments change
powers within an existing support; the current cancellation/recurrence rules do
not introduce a new denominator slot. Revisit that invariant when adding a new
identity. Canonical representatives must belong to the retained closure.

Support analysis is conservative: more than 256 alternatives are replaced by
their union; missing support metadata selects the old dictionary-wide planner.
These are private planning data, not a saved-format extension. Restricting the
families can change abbreviation numbering and remove unused generic restrictions
from newly generated mappings, while their interpretation remains unchanged.

`Tests/ExportPlanning.wls` is part of the `runtime` suite. See
export-planning verification (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
for exact comparisons, timing boundaries and the incomplete full-box import.


## Polarisation grouping at final scalar-master assembly

**Accepted within the recorded development scope, 25 October 2026.** The
complete retained three-box target and affected focused regressions pass. This
is not a claim of complete simplification or validation of the larger QED cases.
See PERF-04 acceptance (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)).

`rcMasterPlan` recognises encoded `Polarization` entries by role. It assigns one
temporary commuting symbol to each unordered vector pair containing a
polarisation, preserving the complete encoded polarisation label. The dedicated
`Templates/PolarisationCoefficients.frm.in` is concatenated with the rational
procedure at the existing final-assembly hook. No mapping or public API extension
is involved; non-polarisation jobs retain the old hook.

The FORM procedures have four responsibilities:

1. `cfcPGInspect`, `cfcPGRegister` and `cfcPGRegisterScalars` classify and cache
   shared definitions in dependency order. Polynomial scalar blocks stay shared;
   polarisation-dependent definitions collect each monomial's scalar coefficient
   into a shared block. Native `denom_` objects introduced by FORM's dimension
   arithmetic are inspected as denominators, never as new independent scalars.
2. `cfcPGMasterParts` combines structural layers before the next substitution
   would exceed its budget. These are structural additions, not separate
   rational simplifications of arbitrary batches of summands.
3. `cfcPGFinal` extracts complete groups, removes common content and invokes the
   existing rational selector per group. `cfcPGExposeDenominators` exposes bounded
   rational skeletons while retaining polynomial blocks. Larger groups are
   collected by their complete denominator products. Eligible numerators are
   simplified, denominator factors are attempted individually, and a finite
   overlap pass collects terms sharing an explicit denominator factor. At most
   two such rounds are attempted, with the second requiring a strictly smaller
   reachable term/atom score; every rational call keeps the same limits.
4. `cfcPGMeasure`, `cfcPGScore` and `cfcPGChoose` compare reachable storage costs.
   Distinct shared definitions count once; private identifier spelling does not
   contribute. A scalar symbol contributes its occurrence and, for a non-unit
   exponent, its power node. Original forms win ties. Both each coefficient and
   the assembled master must pass this comparison.

The private initial limits are 1,024 structures per master and 10,000 structural
terms per substitution layer. Rational selection remains at 1,000 estimated
terms, 12 actual scalar variables and degree 24. Polarisation atoms are outside
that scalar budget. Full `FactArg` is attempted only on reduced arguments with
at most 250 terms, eight variables and degree 12. Common-content extraction uses
`OldFactArg` locally; the surrounding shared-output pipeline enters and leaves
with full factorisation mode restored. Factors are interned separately. A
runtime FORM error or resource failure propagates normally; there is no retry in
this code. These guards bound selected work, not GCD complexity or physical RAM.

The private `$rcPolarisationMaximumRationalPasses = 1024` is a separate,
job-wide allowance. `cfcMRFinal` charges each attempted pass while `cfcPGActive`
is defined, including cache inspection and bounded factor trials within it.
A denied pass sets the exhaustion flag and leaves the expression alone.
Denominator loops stop and subsequent overlap passes are skipped; incomplete
denominator/group cache writes are
suppressed, and `cfcPGFinal` restores the complete original master. Completed
subproblem entries remain reusable. Later masters skip polarisation processing;
non-polarisation rational passes are unaffected. This is a deterministic quality
tradeoff, not a time estimate; traversal order can affect which masters receive
additional simplification. Do not infer savings from cache-hit counts alone.

The original master's storage score is computed once. Each completed prefix
is scored after restoring its polarisation atoms. Different complete monomials
cannot cancel across groups, so reachable term/reference cost is monotonic.
Once that prefix loses, restore the whole original rather than process the
remaining groups. Hide the original/bracketed master and keys before scoring:
a helper sort must not erase the brackets used to extract the next group.
Inactive snapshots and the completed accumulator remain hidden during local work.
The denominator-group visibility was deliberately retained after regression checks.

`LoopDenominatorGroups.frm.in` numbers emitted shared definitions densely.
`cfcKnown` records the output ID for each known internal ID, rather than just a
presence bit. The disposable definition/group expression is renamed at emission;
internal interning tables and algebra retain their IDs. Increasing dependency
order and backward-only references are preserved. Speculative discarded blocks
therefore cannot lengthen output labels. The mapping schema is unchanged.

Cache tables are FORM-job-local and each entry is written once. Known reference
IDs are filtered before their definitions are materialised. Both successful and
rejected numerical inspections are keyed by the complete sorted shared expression; unresolved
metadata and oversized keys are not cached. `cfcPGCoefficientKey` must explicitly
bracket the complete expression before `Collect`; an unset key must never enter
a cache. Completed normalised candidates are reused before common content is
restored, with representation selection still performed per use. A still-rejected
coefficient is not retried only to enable factorisation. New result blocks carry
transitive dependency sets. Successful intermediate algebra reuses the inspection
key, with separate rational-only and factorisation modes. Complete denominator
products have an exact subproblem cache before factor planning. Constants and
single monomials bypass full factorisation inspection after common-content
extraction. The writer emits required children before references; the existing
restricted incremental reader reconstructs ordinary arithmetic and PaVe objects.
No temporary polarisation, scoring or cache head may appear in imported results.
Do not replace the dependency walk by a flat expansion of all kinematic blocks.

`CFC_PG_SUMMARY` reports attempted structures, improved/zero/partial groups,
factorisation calls, whether the whole-master candidate was selected, and FORM
running-time counters in milliseconds, rejected/result cache hits and unchanged
fallback rounds (the existing `unchanged_fallbacks` label includes rounds skipped
for lack of score improvement). The counters split factorisation from
rational processing and remaining grouping/scoring work; they are not external
wall-clock measurements. A rejected whole-master candidate can have improved
individual groups: interpret its `selected=0` field before claiming a delivered
simplification. Skip diagnostics identify unsupported structures, intermediate
limits, rational-skeleton limits, `work_limit`, `work_limit_restore` and
`dominated_prefix`. The latter two restore the complete master; improvement
counters from its discarded groups are not delivered simplifications.
Individual rational diagnostics report the
estimated cost, degree and variable count.

`Tests/PolarisationCoefficients.wls` covers exact identities, conjugation, powers,
complex numbers, symbolic D, separate masters, protected paths, lowered selector
boundaries, manual export/import, tiny read chunks and serial/TFORM agreement.
`Tests/PolarisationWorkBudget.wls` adds exhaustion/restore boundaries and dense
output-reference checks and is registered in the integration suite.
The retained workload report distinguishes these fixtures from unresolved large
coefficient checks. Production performs no Wolfram `Simplify`, `Together` or
`Factor` calls. See the verification report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
and the [FORM manual](https://form-dev.github.io/form-docs/stable/manual/),
sections 7.56, 8.11 and 11, for factorisation and native denominator conventions.

## API-03 final output boundary — 30 October 2026

`FeynGravSimplify/Facade.wl` owns `executeFullBackend`: it delegates the unchanged
converter rules, then optionally invokes ``FeynCalc`PaVeToABCD`` once on successful
PaVe-containing output. The Boolean option belongs only to the facade store.
Conversion diagnostics/aborts produce stage-labelled failures; total timing
includes conversion. Raw reconstruction, shared-output format and legacy stores
are untouched. The readiness inventory includes the helper, and failed loads
clear its private context before retrying. See the
[public contract](UserGuide.md#final-pave-conversion-feyngravsimplify-only).

`FeynGravSimplify/Tests/PaVeOutput.wls` is registered as the `PaVeOutput` facade
phase in `Tests/run.py`. It combines explicit named-function expectations,
controlled converter outcomes and small real serial FORM jobs. Existing facade,
loading and standalone test expectations include the intentional nineteenth
option. The verification report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
records which tests actually ran; registration is not full-suite certification.

## Shared coefficient policy — PERF-05 increment 2

The existing arithmetic planners share the mapping-role, polynomial-domain and
allowance helpers in `RationalCoefficients.wl`; dimension-only compilation uses
an explicit one-variable dictionary. FORM execution, semantic normalisation,
shared-definition inspection, cache/score/rollback ownership are retained.
See [CoefficientPolicy.md](CoefficientPolicy.md) for the actual domains, selection
profiles and conservative exclusions. `CoefficientPolicy` and
`CoefficientArithmetic` are registered runtime suites; the dated report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
records two bounded focused runs (72/72 and 52/52), not complete-suite or
whole-call performance acceptance. API-03 conversion still follows processing.

## Collection around reduction — PERF-05 increment 3

`lrCoefficientCollectionPlan` applies the shared scalar policy at the complete
linear `cfcTC` bracket boundary. Safe Laurent-monomial definitions are exposed
first; generated recurrence weights must not disable this pass merely because
they are not inverse polynomials. A disposable native FORM procedure collects
pre-reduction weights, removes exact-zero keys, and processes direct-output
multipliers after reduction. Existing shared-output final processing stays in
charge for large jobs. See [IntegralCollection.md](IntegralCollection.md) for
key/scope/fallback and counter ownership, and the verification report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
for the final 69/69 checks and both retained failures. `IntegralCollection` is
registered in the runtime suite. No new public option or mapping grammar.

## Changed-coefficient reuse — PERF-05 increment 4

The collector now separates its single-attempt algebra from bounded job-local
reuse. Complete sorted keys distinguish changed coefficients; successful-output
aliases and conservative rejections prevent redundant work. The mapping/rules
context is fixed for one job, and no cache survives it. Arithmetic attempts,
eligible visits and retained entries have separate counters and conservative
stopping behaviour. Existing polarisation caches and resource ownership remain
unchanged. See [IntegralCollection.md](IntegralCollection.md#changed-coefficient-reuse--increment-4).

`Tests/CoefficientReuse.wls` exercises the actual FORM template with controlled
literal rules; `IntegralCollection` checks actual planner/export/import paths.
The final combined check passed 130/130 after two documented harness corrections.
See the report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)) for exact
scope, source pins and the omitted performance/large-case gates. Registration
in the runtime suite does not mean the complete suite was executed.

## Coordinated work limits — PERF-05 increment 5

Optional rational processing and full factorisation share job-wide allowances
while retaining local guards and complete-original fallback. See
[WorkLimits.md](WorkLimits.md) for ownership and exclusions. The corrected combined
batch passes 495/495 assertions under unchanged bounded resources; the report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
retains earlier failures and the final evidence. The final verification (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
completes dependency, retained QED and three-box checks with default named output.
This is not a general performance guarantee.

## Scalar coupling-group follow-up

PERF-07 connects `rcKinematicPlan` in `lrPrepare`, before denominator sharing and
final coefficient inspection. Rendering only consumes the captured identities;
`tdImportSetup` validates them before reconstruction. `rcMasterPlan` admits native
polynomial denominators in both inspection and computation. For scalar spectator
groups, `rcPolarisationPlan` reuses the structural engine with a Laurent-capable
classification; polarisation jobs keep their original partition. The shared-output
template registers reachable dependencies before filling coefficient metadata.

Keep scalar-role classification independent of parameter spelling, restore the
complete original on work exhaustion, and do not infer mathematical success
from a completed FORM process. `Tests/ScalarCoefficientGroups.wls` is included
in the runtime suite and passes 57/57 assertions on the current candidate.
`cfcCGInspect` separates legacy and complete-group rejection caches; the retry
charges the existing job and local allowances and precedes denominator fallback.
`cfcCGCombine` expands immutable shared definitions once per job, exposes and sorts
one factor per layer, includes literal inverse polynomials, and builds named
numerator/denominator expressions without hiding negative powers. Native `gcd_`,
two `div_` calls and two checked `rem_` calls charge five work units. Zero is handled
before content extraction. Numerator output is bracketed before `Collect`; the
complete reduced denominator is wrapped once after a 4,096-word admission check.
The source remains intact until success. Legacy PolyRatFun handling is unchanged
apart from an explicit module boundary before renewing expression selection.

**Complete-call repair verified:** the unmodified retained triangle completes in
serial FORM and eight-worker TFORM, with exact agreement for all five masters and
the 225-term tadpole coefficient. The complete-group path uses native named-expression
GCD and exact division instead of packing oversized rational arguments. Additional
finite expansion/denominator guards preserve the original on predictable rejection;
actual execution errors remain failures. See the verification report for measured
resources, regression evidence and the unchanged larger-QED limitations.

Earlier selected-master checks passed within their recorded scope. Reference closure must sort `cfcRefs+cfcRefChildren` before removing its
coefficients; otherwise overlapping identifiers multiply the emitted polynomial.
Keep this normalisation scoped to the reference list. The live-template regression
`Tests/SharedDependencyRegistration.wls` covers overlapping roots/children,
multilevel closure, serial/TFORM agreement and a broken-code negative control.
All seven groups and all five triangle masters agree exactly. Fresh regression
checks pass 263 assertions; the unchanged dependency template retains its earlier
10 passing assertions. QED/three-box gates pass; see the
verification report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
for timing, memory and remaining larger-case limitations. Resource and legacy
algebraic limits are unchanged.
