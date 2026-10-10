# Bulk reconstruction of commuting polynomial leaves

## Scope and status

This records an intermediate optimisation towards importing the complete retained
polarisation result within 180 seconds. **That stage did not meet the target.**
It is superseded by [the coefficient tree parser](CoefficientTreeImport.md). No FORM
processing rules, saved formats or mathematical vocabulary changed.

For eligible grouped version-one imports, the flat parser decodes distinct
factors into a local association, locates additive boundaries in the validated
token list, and constructs products using `TakeList` and `Apply`. Rejected
speculative decoding replays the original ordered parser, preserving diagnostics.
Other imports keep the ordered parser. User symbols with UpValues still disable
the grouped shared environment.

## Measurements so far

Trials used fresh Wolfram kernels and unchanged saved output/mapping pairs.
Timing excludes hash computation. These are individual observations, not medians.

| Fixture / implementation | Import seconds | Outcome |
|---|---:|---|
| 8,277,174-byte, 64-group fixture, initial bulk implementation | 14.499189 | Completed; exact expression hash matched |
| Same fixture, bulk implementation with native list partitioning | 13.804751 | Completed; exact expression hash matched |
| Same fixture, experimental WVM bracket scanner | 15.258734 | Completed; hash matched; scanner not retained |
| Complete 171,993,554-byte output, initial bulk implementation | — | Incomplete at the 300-second trial limit |

The smaller fixture retained the same 102,295,120-byte expression. Its SHA-256
Wolfram expression hash (decimal) was
`36398911113692716794460706418958620760734602569645192792205934814723259467366`.
Native list partitioning retained 198,462,688 bytes of kernel memory in this trial;
process-tree peak RSS, including loading and subsequent hashing, was
1,059,635,200 bytes.

The complete-output trial peaked at 905,220,096 bytes process-tree RSS before
termination. It has **no successful full-import timing or correctness result**.
Trials were limited to 300 seconds, 6 GiB process-tree RSS and at least 2 GiB
available system memory. Original files were preserved.

A profile of eight largest coefficients from the complete output completed in
5.836668 seconds, including instrumentation: 5.068548 seconds were inside
`pgParse`, of which 2.256879 seconds were inside `parseResult`. This points to
nested coefficient traversal as well as flat reconstruction. The complete import
is eligible for the shared parser; its mapped symbols have no UpValues.

The instrumented complete-output trial reached group 1,000 after 284.087465
seconds, with 268.859834 seconds accumulated inside `pgParse`. It too reached
the 300-second limit before a completed import was recorded. Its peak
process-tree RSS was 904,683,520 bytes. This establishes coefficient parsing as
the main remaining cost; it does not establish a full-import time.

## Verification

The core suite passes: Core 111, Parser 121, SharedParser 148, Transactions 20,
and Installer 62 assertions. SharedParser now includes direct bulk-path
comparisons for signs, division, scalar powers, momentum products, components,
metrics, malformed input and first-failure order, plus 100 deterministic random
polynomial comparisons. No installation was performed.

The FORM suite also passes (Export 31, FormStages 137 and Import 67 assertions),
as does the runtime suite (Runtime 82, Epsilon 48, DiracColour 65, DiracAlgebra
104, ColourAlgebra 139 and PropagatorGroups 113). Together with the core suite,
these are 1,248 passing assertions. The broader interaction integration suite
and the Full benchmark suite were not run.

These intermediate checks did not establish achievement of the 180-second
target. See the subsequent coefficient-tree report for the completed trials.
