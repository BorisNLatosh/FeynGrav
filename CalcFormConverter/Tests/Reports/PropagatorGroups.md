# Automatic propagator grouping — 9 October 2026

## Implementation and scope

Exports containing mapped denominators now emit `Bracket` for all those identifiers after the configured algebra and before the final sort. `%E` writes ordinary denominator monomials multiplying coefficients. Powers are preserved; propagator-free terms have unit prefactor. No new public option, cancellation rule, coefficient simplification or physical assumption is introduced.

The optional `ResultLayout -> "PropagatorGroups"` metadata is included in the mapping digest. Mapping versions one through five remain supported. Files without this metadata retain their existing interpretation; denominator-free exports retain their existing path.

`PropagatorGroups.wl` reads fixed-size byte chunks and buffers one top-level summand. It reuses the restricted parsers, separates the denominator prefactor, combines repeated group keys and reconstructs coefficients without distributing their propagators. Native parenthesised coefficients remain eligible for the existing flat parser. Colour connectivity is checked per additive coefficient term, with one implicit/explicit endpoint decision across the whole result. Streams close on success, failure and abort.

Incremental input avoids a complete result string and complete token array. It does **not** bound the memory needed for parsed coefficients, reconstruction or the final expression.

## Regression checks

The following suites passed during implementation:

| Suite | Passed assertions |
| --- | ---: |
| Core | 111 |
| Parser | 121 |
| Export transactions | 20 |
| Installer mocks | 62 |
| FORM export | 31 |
| FORM stages | 128 |
| FORM import | 65 |
| Runtime | 82 |
| Epsilon | 48 |
| Dirac/colour translation | 65 |
| Dirac algebra | 104 |
| Colour algebra | 139 |

After the final coefficient-parser adjustment, the focused grouping suite passed **47 assertions**, and automatic loading/namespace isolation passed. The broad suites above preceded that final adjustment; they are not represented as a second complete regression run.

Focused checks include serial FORM and TFORM; repeated powers and complete products; masses and routing; zero and unit groups; factored reconstruction; chunk sizes down to one byte; whitespace, CRLF and signed exponents; malformed/truncated results and unknown identifiers; digest mismatch; stream cleanup on failure and abort; no whole-result text import; tensor, epsilon, Dirac and colour coefficients; global endpoint promotion; all four colour/Dirac processing combinations; epsilon convention mismatch and legacy translation-only gamma reconstruction. Existing saved-format fixtures were exercised by the regression suites.

## Large-case measurements

The user's original `One-Loop_Polarization_Operator_Raw.frm`, `.out`, `.map.json` and `.log` were preserved. Sizes, SHA-256 hashes and modification times were checked again after the trials. All generated candidates and comparison files were placed separately under `/tmp/cfc-propagator-large`.

Trials ran sequentially with a monitor enforcing 300 seconds, 6 GiB process-tree RSS and at least 2 GiB available system memory. Sampling and process termination can slightly exceed a threshold. Reported monitored wall times include process/kernel startup and termination; these are single observations, not benchmark medians.

| Quantity | Observation |
| --- | ---: |
| Original output | 692,259,000 bytes |
| Grouped output | 542,150,420 bytes |
| Output size reduction | 21.68% |
| Complete propagator groups | 1,022 |
| Expanded coefficient terms | 5,508,839, unchanged |
| Largest group, excluding whitespace | 1,302,907 bytes |
| Grouped TFORM generation, eight workers | 221.16 s; 5,007,544,320 bytes peak RSS |

The temporary candidate was made from the original program with grouping and redirected output. It includes an additional final sort. The original log's historical runtime is not a controlled timing comparison; no generation speedup is claimed.

### Exact equality in FORM

A single full-result comparison exceeded the 6 GiB limit and was stopped. The complete comparison was then partitioned deterministically by complete propagator key into 16 smaller jobs. Preparation checked identical key sets and matching term counts in every partition. Serial FORM returned **exactly zero for all 16 differences**, establishing equality of the complete original and grouped results without importing either full expression into Mathematica. Each comparison remained below the agreed resource limits. The partition manifest and individual logs are retained with the local artefacts.

### Import limitations

| Trial | Outcome |
| --- | --- |
| Preliminary full import, before coefficient fast-path adjustment | Stopped at 300 s; peak RSS 4,459,679,744 bytes; incomplete |
| Full import with the final coefficient path | Stopped at the 6 GiB threshold after 139.94 s; sampled peak RSS 6,449,807,360 bytes; incomplete |
| Initial 64-group subset trial | Interrupted at the user's request to pause; not a failure or completed timing |
| Resumed 64-group subset, 26,012,080-byte output | Completed: import 68.89 s; monitored process 74.28 s; peak RSS 2,736,328,704 bytes |

**The complete result has not been successfully imported within the agreed memory budget.** Grouping reduces repeated propagator text, but leaves millions of coefficient terms. The stopped trial does not establish a lower bound on the final expression's intrinsic memory requirement. The successful subset produced a 456,613,224-byte expression with 16,579,226 leaves; retained Wolfram kernel memory was 2,587,226,616 bytes. These kernel statistics differ from process RSS. Success is evidence only for that 64-group subset, not the full result.

## Delivery boundaries

No interaction formulas, stored libraries or benchmark notebooks were changed. No Full benchmark was run and no commit was made. The main README contained separate user edits and was left untouched. The implementation and this report do not claim that arbitrary large results will fit in RAM.

Local evidence: `/tmp/cfc-group-test` contains verification scripts and suite logs; `/tmp/cfc-propagator-large` contains candidates, manifests and monitored trial logs. These temporary artefacts are evidence from this development session, not package runtime dependencies.
