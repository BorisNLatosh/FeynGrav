# Denominator collection inside PaVe multipliers: investigation

Date: 11 October 2026. **Outcome: the experimental implementation was not
accepted. The previous working calculation path was restored.**

## Observed limitation

The saved displays in `One-Loop-Counterterms.nb` contain repeated denominator
products within the first two PaVe multipliers. The notebook stores shortened
displays, not the complete value of `Pi`. The large experiments therefore used
the retained programme for the same representative quadratic-gravity expression.
The notebook and its original calculation files were not modified.

The current writer collects contributions by the final integral coefficient.
For large commuting expressions, it preserves shared, already contracted
amplitude blocks. Those blocks can contain rational denominators and spectator
propagators. Dimension-dependent coefficients outside the blocks are combined,
but equal denominator products hidden in different blocks are not globally
collected. This is a representation limitation, not evidence of an incorrect
integral reduction.

## Experiments and limits

The prototypes exposed rational factors in FORM, normalised polynomial factors,
collected complete denominator products, and reconstructed ordinary arithmetic.
The small dedicated suite passed 31 checks, including tensor/matrix coefficients,
FORM/TFORM agreement, hidden scalar/vector dictionary entries, malformed input
and chunk boundaries. That result did not establish acceptable large-case costs.

Several variants tested shared numerator factors, a cached factor dictionary,
univariate rational processing and preservation of mixed mass polynomials.
Reopening all shared blocks was too expensive. Retaining polynomial factors
reduced some costs but did not make complete collection acceptable.

| Observation | Execution | Output | Result |
|---|---:|---:|---|
| Retained working baseline | 118.21 s | 40,097,562 bytes | Complete; 1,273 final integral groups |
| Last collection-only prototype | Stopped at 188.11 s | 328,044,466 bytes | Incomplete; only six groups written |

The baseline is an earlier retained measurement, not a fresh alternating timing
comparison. The final prototype reached 4,210,876 KiB process RSS before it was
stopped. FORM returned a signal-related failure during termination; this is not
a successful calculation. Other incomplete trials reached the 300-second limit
or were stopped earlier for excessive output or memory growth. Large trials ran
sequentially with guards at 300 seconds, 6 GiB process RSS and 2 GiB available
system memory. No incomplete file was treated as a full mathematical or import
validation. No Full benchmark suite was run.

## Working state and next design question

The experimental exporter, reader additions and automatic activation were
removed from the checkout; their source, programmes and logs were retained in
the local temporary investigation directory. Existing uncommitted development
work, interaction rules, libraries and notebooks were preserved. The restored
loop-coefficient suite passed all 131 assertions. The core suite also passed
all 766 assertions (core validation, parsers, export transactions and mocked
installer decisions). No owned FORM experiment remained running afterwards.

A further attempt should first test a shared expression representation that
records numerator and denominator parts separately, while retaining sums and
products of reused numerator blocks. Flattening each full multiplier into a
polynomial before collection has failed the practical acceptance check. Allowing
shared definitions to reference earlier definitions would require explicit
acyclic-dependency validation and a documented saved-format extension; it is a
proposal, not an implemented capability or a measured performance improvement.

## Follow-up: separate amplitude and recurrence numerators

A later prototype splits the existing amplitude blocks and recurrence weights
by their denominators independently, then joins references to their numerators.
It uses one polynomial-factor dictionary for numerator and denominator factors
so matching factors can cancel across those two stages. A small diagnostic
verified cancellation of both a mass power and a matching dimension polynomial.

The full retained result contained 1,273 final coefficient groups. The
post-processing prototype completed in 116.45 s, with a peak process RSS of
2,154,444 KiB. This measures rearrangement of an already computed result, **not**
a complete `CalcFormCalculate` call. It wrote 413,145,349 bytes, including
43,216 shared definitions. Its largest coefficient occupied about 35 MB of
source text. The guarded import did not finish: it reached the 300-second time
limit and was terminated after 305.11 s, with peak RSS 685,600 KiB. A buffered
reader did not resolve that overall cost. This candidate is not accepted.

The first two FORM output groups agreed with the original at four exact
rational sample points. These samples retain integral heads and spectator
propagators symbolically, but they are not a proof of the complete result at
generic kinematics, nor are FORM output order and Mathematica part order assumed
to coincide.

A subsequent temporary prototype also shares the combined numerator sums. Its
small two-group probe passed the same four samples; full-case verification is
pending. This would require explicitly validated, acyclic references between
saved scalar definitions. No such extension has been enabled in the production
checkout. The temporary prototypes are kept outside the repository.

### Subsequent reader and dictionary probes

The combined-numerator prototype completed FORM post-processing in 153.79 s
and produced 268,725,410 bytes. Import attempts exposed two fast-parser
eligibility gaps in the prototype: positive powers of shared references and
line breaks immediately inside reciprocal-marker calls. The latter occurred
more than 400,000 times in the non-nested output. Correcting eligibility retained
the original typed identifier checks and improved the small comparison; the
non-nested full import nevertheless reached its 300-second limit (peak RSS
1,648,108 KiB).

Reading shared definitions in bounded batches reduced the nested dictionary
stage from 222.85 s to 103.85 s in these sequential observations. These were
prototype runs, not alternating performance trials. The first complete parse
attempt then correctly failed on two missing identifiers. FORM had reused
older extra-symbol identifiers which a range-based writer had not recorded.
No failure was counted as a successful import.

The revised writer tracks identifiers actually used by each group. Its full
post-processing run completed in 129.72 s, with peak RSS 2,527,888 KiB, and wrote
268,476,208 bytes. An independent streaming audit found 230,983 definitions,
1,273 groups, and no missing, duplicate or forward references. The corrected file imported successfully in 259.13 s. The complete verification
script finished in 279.21 s, with peak process RSS 1,554,872 KiB and retained
Wolfram kernel memory 931,516,600 bytes. It returned 1,273 terms containing
1,273 PaVe groups and no tested private parser heads. The small two-group probe
again passed the four exact rational sample checks.

The experimental batching code is not production-ready: malformed-input and
first-failure ordering, buffered stream cleanup, incremental definition framing,
and integration into the public export/calculation path still require checks.
A temporary file-layout adapter was used for these importer probes. This is
not evidence of a completed public workflow. Production code remains unchanged.

This successful large import establishes feasibility under the development
resource limits. It does not establish a speedup over the earlier working
baseline: the new output is larger and collection adds work. Complete public
workflow integration, generic mapping-driven generation, malformed-file tests,
legacy compatibility and a stronger algebraic comparison remain outstanding.
No production implementation has been enabled on the strength of this probe.


## Integrated candidate verification — 11 October 2026

The generic writer and native framed reader were tested in an isolated package
copy. This supersedes the temporary layout adapter used in the earlier probes.
The results below record successive candidates; only the final verified candidate is intended for the production checkout.

The complete retained workload exported in 1.23 seconds. TFORM with ten workers
finished in 230.35 seconds (observed peak RSS 3,696,448 KiB),
producing 288,040,066 bytes. Public `CalcFormImport` completed in 261.27 seconds.
The import verification process completed in 290.73 seconds, with
observed peak RSS 1,564,992 KiB and retained kernel memory
669,477,800 bytes. Its result contained 1,273 terms and 1,273 PaVe groups, with
none of the checked private reconstruction heads. This run used the native
output directly, without reordering or rewriting it before import.

These are single observations, not an alternating speed comparison. The
collection adds work and produces a larger file than the opaque-block baseline.
Trials used a 300-second limit, 6 GiB process-RSS limit and 2 GiB available-memory
floor. The final expression still has to fit in memory.

The existing core/parser suite passed 766 assertions; the existing runtime and
algebra suites passed 1,194 assertions, including all 131 loop-reduction checks.
Focused reader checks covered chunk sizes 1, 3, 17 and 16,384 bytes, CRLF,
missing final newlines, malformed/truncated frames and successful retry after
failure. A missing final newline exposed a reader bug, which was corrected;
truncated records still fail. Legacy fixtures explicitly remove the new optional
metadata when exercising the legacy grammar.

The first full-output comparison found one differing multiplier among 1,273
at two finite-field sample points. Independent FORM evaluation confirmed the
difference. That candidate was not applied to the working package.

Stage-by-stage checks isolated a native `OldFactArg` edge case: certain mixed
arguments containing scalar products and several indexed commuting functions
acquired an incorrect common scalar-product power. A three-term reproducer was
extracted. The common-content helper now temporarily applies `ToPolynomial`
inside its arguments, performs the cheap content extraction, then restores the
original objects with `FromPolynomial`. This is not full factorisation.
An independent pattern error was also corrected: a powered scalar product on
a FORM left-hand side must use `p.q^n?`, rather than `(p.q)^n?`, to match the
negative powers used here.

The focused suite now passes 53 assertions, including exact serial FORM and
TFORM checks for both cases. The FORM export/import suite passed 257 assertions;
the package-integration suite passed its loading checks, 50 assertions and the
complete-bubble export metadata checks before these final corrections. The
last loop-reduction rerun passed all 131 assertions. It first exposed a
free-index case rejected by `ToPolynomial`; tensor-bearing terms now retain
collection and sharing while skipping optional common-content extraction.
Native metrics are explicitly recognised by that guard. Serial and TFORM
free-index comparisons pass.

The corrected scalar workload completed TFORM in 255.86 seconds, with peak
process RSS 3,610,176 KiB, and produced 290,014,749 bytes. Its public import
completed in 258.50 seconds; the complete verification process exited normally
in 288.73 seconds with peak RSS 1,610,116 KiB. Retained kernel memory was
669,255,080 bytes. It returned 1,273 PaVe groups and no checked private heads.
Every one of the 1,273 multipliers agreed with the earlier representation at
two independently chosen sample points modulo 998244353. This sampling is not
a full symbolic identity proof; manageable exact comparisons and the extracted
native counterexample are covered by the regression tests.

These measurements preceded the final tensor-only guard. The final complete
TFORM run with that guard took 252.36 seconds, with observed peak RSS
3,698,112 KiB, and produced 289,614,445 bytes. All 1,273 coefficient groups
agreed with the baseline at both independent finite-field sample points.
The final public import completed in 259.05 seconds. The verification process
completed normally in 288.98 seconds, with observed peak RSS 1,590,848 KiB
and retained kernel memory 668,503,536 bytes. It returned 1,273 terms and
1,273 PaVe groups, with none of the checked private reconstruction heads.
This final run used the native output and mapping directly. The scoped patch
was then applied after checking that its production source files had not
changed during verification.
No notebook or stored library was changed; no Full benchmark was run.
