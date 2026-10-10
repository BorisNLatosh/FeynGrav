# Single-pass coefficient import

## Change

Eligible nested coefficients in version-one grouped results now use a restricted
arithmetic tree. The lexer retains the existing factor vocabulary. A lazily
compiled Wolfram virtual-machine function recognises parentheses and arithmetic
structure using integer tokens; it neither evaluates source text nor reconstructs
symbols. Existing typed decoders supply factor values. Native list operations
then reconstruct sums and products by depth, preserving factorisation.

This requires no C compiler, external process or new installation. Other mapping
versions, symbols with UpValues, unsupported syntax and small/flat inputs retain
the existing paths. Rejected speculation replays the original parser for its
ordered diagnostic. Public commands, options, saved formats and FORM algebra are
unchanged.

## Complete-result timing

The retained output contains 171,993,554 bytes and 1,023 top-level groups,
including an initial zero. The resulting expression has 1,022 top-level terms.
Original files were preserved. Output SHA-256:

`5990abf716a1d9b7a889282ea11adf5553345cd4188364c8d8a15d8154e79eaa`

| Trial | Complete import | Outcome |
|---|---:|---|
| Initial prototype | 164.216769 s | Import returned successfully; later whole-expression hashing hit the memory cap |
| Integrated source | 139.924678 s | Import returned successfully; kernel exited normally |

The integrated run used a fresh kernel and measured the public `CalcFormImport`,
including file reading, validation and reconstruction. Kernel setup and
post-import descriptive measurements were outside that timer. The total process
lasted 143.256851 seconds, with peak process-tree RSS 1,377,652,736 bytes
(approximately 1.28 GiB). The returned expression's `ByteCount` was
2,173,428,952 bytes; retained Wolfram kernel memory was 1,186,895,656 bytes.
These measures differ because expression `ByteCount` is not process RSS and
subexpressions can be shared.

Trials stop at 300 seconds, 6 GiB process-tree RSS or less than 2 GiB available
system memory. In the prototype trial, the import itself completed before the
subsequent `Hash[result]` allocation exceeded the RSS cap. The integrated trial
omitted that whole-expression hash. This does not promise bounded memory for
arbitrary output or an arbitrarily large individual coefficient.

## Complete-result validation

Reconstructing the exact requested input with the installed FeynGrav libraries
produced the same normalised expression fingerprint as the retained mapping:

`d43dcf3b6594db5d58df9f3d2af85ecd6283d304cb271c8a35bb87769405295b`

All 1,023 original groups were compared in eight consecutive chunks, each with
the original mapping. Concatenating the chunk bodies reproduced the original
body byte for byte. Each chunk was imported with both the previous nested/ordered
parser and the new parser. **All eight comparisons passed `SameQ`**, without
expanding expressions or hashing the full reconstructed result. Every comparison
process exited normally and stayed within the resource limits.

The reference used the unchanged `pgParseNested` and `parseFlatResultOrdered`
implementations through the public importer, temporarily bypassing both new
optimisations. Group accumulation, propagator restoration and final assembly are
unchanged by this work. The comparison therefore covers every coefficient, not
just a selected sample. Chunk timings are diagnostic observations; their sum is
not presented as a measured complete-import time.

## Regressions and delivery checks

All **1,498 assertions** pass: Core 111, Parser 121, SharedParser 148,
CoefficientTree 250, Transactions 20, Installer 62, Export 31, FormStages 137,
Import 67, Runtime 82, Epsilon 48, DiracColour 65, DiracAlgebra 104,
ColourAlgebra 139 and PropagatorGroups 113. Loading and namespace-isolation checks
also pass, including unchanged working directory, FeynCalc options and existing
definitions, and no processes launched by loading.

Tree checks cover nested sums/products, signs and division, scalar powers,
components and metrics, malformed and unsupported syntax, first-failure order,
200 deterministic random comparisons, bounded nesting, changed scalar-product
definitions, lazy compilation and abort cleanup. Existing suites retain the
saved-format, stream-cleanup, process and algebra coverage.

The measured source SHA-256 values still match the final implementation. Raw
measurements, source hashes and per-chunk comparisons are retained in the
[verification data](CoefficientTreeImport.json). These are workload-specific
observations from fresh kernels, not a portable speed guarantee. No Full
benchmark suite, interaction formulas, stored libraries or notebook outputs
were changed. No commit was made.
