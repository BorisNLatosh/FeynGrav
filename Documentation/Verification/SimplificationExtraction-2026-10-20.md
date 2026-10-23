> Source-path update (21 October 2026): the current `Simplification/` directory is now `FeynGravSimplify/`. Paths and hashes below describe their historical snapshots; external evidence archives were not moved.

# Simplification extraction and API-01 facade — provenance and verification

Date: 20 October 2026. Task: API-01, unified public calculation interface.
Status: **API-01 accepted on 20 October 2026 within the verification scope
below; uncommitted, unreleased development work, not release evidence.**

Repository: `/home/latosh/.Wolfram/Applications/FeynGrav`, branch
`FORM_Simplification`, checked-out revision
`fa4fe3518c470a30449ff48e3c6ce7539ac6bce7`. The donor implementation came from
branch `Contraction_Integration_and_Benchmarking` at
`2f7f1656a1d06b991e2c2070756cee7aa195d868`. Evidence, runner and test fixture
are retained outside the repository at
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-interface/`.

## API-01 acceptance — 20 October 2026

**Accepted on `FORM_Simplification`, uncommitted and unreleased.** The tested
working tree is based on `fa4fe3518c470a30449ff48e3c6ce7539ac6bce7`; HEAD alone
does not identify the implementation. Use the source hashes and modification
register in repository `Documentation/Verification/SimplificationExtraction-2026-10-20.md`.
The retained evidence is `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-interface/`.

The final targeted batch passed 43 substantive checks. Six message-audit checks
were ineffective because the harness tried to reset protected `$MessageList`;
manual review of the retained log found only the resulting harness warnings and
expected partial-reduction notices. This is not a warning-free or automated
message-audit pass. The legacy ProcessLink warning was separately reproduced
with unchanged code and remains unfixed. Real Contractor is **not tested**;
its protocol was checked with a process fixture. No full regression rerun,
performance claim, loop certification or release readiness is implied.

Acceptance uses the targeted final checks together with the applicable earlier
evidence, not a claim that every suite ran again on the final source. No further
calculation is required solely to close API-01. Fix the message-audit harness
before reusing it in a future authorised run. Historical failures remain retained.
ARCH-01 is the next planned task, but is not started or authorised by this
acceptance. No commit, push, tag or publication is authorised.

## Donor extraction identity

Eleven files were extracted from the donor commit without content edits and
hash-checked at extraction time (Git blob SHA-1 and SHA-256 of the extracted
bytes):

| File | Blob SHA-1 | SHA-256 (extracted) |
| --- | --- | --- |
| `Simplification/Contractor.wl` | `c428ca69705a0aa3957f4a76e17b6b62bf76ea17` | `28469aa101d6a08e224010ae90e1021b670d3374f12b0d2005bbd27a5df848d2` |
| `Simplification/FORM.wl` | `bb3fe55dbf56212470d6cd31c21b74496cfcad3f` | `5ec5d86833fd0cc02d9d7cea1cb133dc23ccfca712e34c87e611a4a27774092e` |
| `Simplification/FORM.md` | `17167dcb977bd9985eb9acd2e968a854808ed233` | `a1dcfcb8b0605461e8f05d55773c9239c31859109dae1f19e433dd14222fbe1f` |
| `Simplification/README.md` | `e9dde116a0b52d8642d3c71f26fcc1d9030ba3ed` | `581b94536415c0ee767e3162dd4fcb326ee8849ef5f787d325115de1501f88ca` |
| `Simplification/Verification.md` | `fb9a01ee5f24edcf3aaaed21e273586471f7db6a` | `f0e690aa1d5ea41fc363c7d7f03f8ba6d6b212f71ca22d60df00eb901ace5f1e` |
| `Simplification/FORMVerification.md` | `f2d9640a3bf7a59e48607142d5141796b3b34990` | `2800e0369db40058d29771cbd598b2d829578522ee1ef102a80159cfc51899e9` |
| `Simplification/Tests/Adapter.wls` | `00948d2c00196fad64a0ba7979e88504972f5b8a` | `b4cefbe5cdab4ebd367c6abda83b5f5937007126f45f9b6db90bb5564af79dee` |
| `Simplification/Tests/FORM.wls` | `c2f064e9bcce6836f07abf5be1b8e808ff1a91ad` | `fa80201a7f0331290bd5763e4873ffd407f3ddf84ff94967ec69ff77d62eeae6` |
| `Simplification/Tests/Integration.wls` | `eb65c242718b53765117a9904e788fb873bb82c8` | `a6eb7069a22e89b64ce63dd722786926484fed64102af911101967b7541620e4` |
| `Simplification/Tests/Loading.wls` | `a2bd45f09e79c9eb9248ac1f0c239af07ef1b6c9` | `70be14fd1038e7fe063201c5e6af3432b447ad8c63eac984d728e8af130da7d2` |
| `Simplification/Tests/ProcessFixture.go` | `1adc7808cb5d75dccffeb777df385af2467b6171` | `5f3ca4ba7d35610b2fb01527a024c3cf22f8b5708e96d055117c38cb0ef74ac6` |

The hash manifest is retained as `extraction-blob-identities.csv` in the
evidence directory. The donor `Reports/` directory was not copied; donor
reports are historical claims for another branch and are not evidence here.
`Verification.md` and `FORMVerification.md` were extracted but left
byte-identical to the donor: they describe the donor branch and must not be
read as statements about this checkout.

## Post-extraction modification register

Hashes above describe the extraction moment. Files below changed afterwards:

| File | Changes |
| --- | --- |
| `Simplification/Contractor.wl` | Adapter only (67 changed lines vs donor). Removed the donor option store and ``FeynGrav`FeynGravSimplify`` messages; requalified `::files`/`::cleanup` onto ``CalcFormConverter`FeynGravSimplify``; repointed `simplify` to ``OptionsPattern[CalcFormConverter`FeynGravSimplify]`` with name validation against the facade store; removed the donor `Automatic -> "Contractor"` resolution; restricted method vocabulary now `"Contractor"`/`"FORMBenchmark"`; backend-dependent log/name/decode branches test `backend =!= "Contractor"` (the donor's `backend === "FORM"` selection broke under the renamed method and was corrected during testing); replaced the unconditional dual-pair validation: only the selected backend's executable/worker pair is read and validated, the unselected pair is never evaluated; deleted the donor public dispatcher (further corrected in the supervisor review round below). |
| `Simplification/FORM.wl` | Byte-identical to donor (0 changed lines). |
| `Simplification/Facade.wl` | New. Defines the five facade commands in the ``CalcFormConverter` `` context, the union option store, capability matrix, full/restricted dispatch, wrapper stores, failure helpers and usage declarations for ``FeynGrav`ContractorExecutable`` and ``FeynGrav`ContractorThreads``. The restricted branch resolves and forwards only the selected backend's executable/worker pair (supervisor review round); the unselected pair is neither evaluated nor forwarded. |
| `Simplification/README.md` | Updated (70 changed lines) to facade semantics, defaults, store ownership and current test invocation. |
| `Simplification/FORM.md` | Updated (13 changed lines): restriction method name and worker defaults (`ContractorThreads -> 8`; `FORMThreads -> Automatic`, effectively up to eight workers by processor count, serial at one). |
| `Simplification/Tests/Adapter.wls` | Byte-identical to donor. |
| `Simplification/Tests/ProcessFixture.go` | Byte-identical to donor. |
| `Simplification/Tests/FORM.wls` | Adapted (94 changed lines): `Method -> "FORMBenchmark"`, renamed failure tags (`FORMBenchmarkProcessFailed`, `FORMBenchmarkTimedOut`, `FORMBenchmarkLaunchFailed`), job prefixes, real-FORM full-pipeline comparisons, replaced the inactive-value expectation with the accepted contract (unselected backend values ignored), documented workspace filter for owner ProcessLink streams. |
| `Simplification/Tests/Integration.wls` | Adapted (89 changed lines): explicit `Method -> "Contractor"` for the helper; dispatch-scoped value checks called directly; documented workspace filter for owner ProcessLink streams. |
| `Simplification/Tests/Loading.wls` | Adapted (93 changed lines): standalone load, one-time guard, independent stores, optional-module installer contract (pre-facade layout passes; stepwise missing-companion rejection and one-at-a-time removal). |
| `Simplification/Tests/Standalone.wls` | New: standalone converter load, facade identity, stores, validation and arity without FeynCalc main-package loading. |
| `Simplification/Tests/Facade.wls` | New, then strengthened in the supervisor review round: supported-success checks (tensor, Dirac square/trace, colour Casimir, loop vacuum scalar) with known answers, Failure guards and algebraic agreement for the tensor fixture; failure-tag parity only for intentional invalid fixtures; conflicting duplicates; option lists and an observable nested-option control (nested restricted method plus non-baseline `LoopMomenta` must refuse and report that setting); stored delayed values and explicit-over-stored precedence; plus capability, wrapper and retained-file behaviour. |
| `CalcFormConverter/CalcFormConverter.wl` | Guarded one-time `Get` of `Simplification/{Contractor,FORM,Facade}.wl` from the repository root at the end of the converter package. The guard tests symbol existence with `Names[...] === {}`. |
| `FeynGrav.wl` | Startup command list extended with the five facade names. |
| `install.m` | Three revision-aware dependency rows activate on the quoted basenames inside the converter `Get` block. The modules are **not** unconditional `requiredFiles` entries, so pre-facade archives (published main/v4.1.0) still validate (supervisor review correction). |
| `Documentation/Reference.md`, `Documentation/UserGuide.md`, `CalcFormConverter/UserGuide.md`, `CalcFormConverter/DEVELOPER.md` | New facade sections, marked development/unreleased; the developer guide's module map names the guarded `Simplification/` load. |
| `Documentation/ReleaseNotes-API01-Development.md` | New: the unreleased development note and migration record. |
| This file | New: extraction provenance, modification register, test evidence and warning analysis. |

Implementation note on the load guard: during debugging, `NameQ` on the
guard symbol returned `False` in the statement immediately preceding the guard
and then `True` inside the guard within one converter `Get`, with no definition
between the evaluations (probes `run-p9-guard-trace`, `run-p9b-guard-trace`);
a clean-scope control (`run-p8-nameq-scope`) returned `False` twice and created
no symbols. The guard therefore uses a side-effect-free `Names[...] === {}`
existence test, verified by `run-p9c-names-guard` and every loading suite since.
The observation is recorded as evaluator behaviour; its cause was not
established and is not claimed.

## Verification method

- Every Wolfram launch ran in a fresh kernel under the task-owned runner
  (`runner/api01-run.py` with `run-in-scope.sh` and `owned_guard.py`): unique
  systemd scope, `MemoryMax=1G`, `MemorySwapMax=0`, `OOMPolicy=kill`,
  `KillMode=control-group`, `RuntimeMaxSec=120s`, `TimeoutStopSec=10s`,
  fresh-host-headroom check, one launch per run directory, sequential launches.
  User kernels were never touched.
- The runner launched the source `.wls` path and verified a copy against the
  source hash before launch (`API01_WLS_SOURCE`/`script-hashes.json`).
- The restricted Contractor protocol used the Go-built test double
  `fixture/fixture-process` (Go 1.27.2 linux/amd64, SHA-256
  `2b26368b9e2070b9840a85b6e27140e6e06bbaa786c8b24fa94a6f03a4a876e9`); no real
  Contractor binary exists on this host. The restricted FORM method and full
  pipeline used real `/usr/local/bin/form` and `tform`.
- Environment variables for backend suites:
  `FG_CONTRACTOR_TEST_BINARY` and `FG_CONTRACTOR_TEST_FIXTURE` point at the
  same fixture binary.

## Final test evidence

All runs below are POLICY-VALID bounded launches; assertion counts are taken
from the kernel stdout log in each run directory.

| Suite | Run directory | Assertions | Unset::norep warnings |
| --- | --- | --- | --- |
| Loading | `run-t7-loading-final` | 35 passed, 0 failed | none |
| Standalone | `run-t7-standalone-final` | 15 passed, 0 failed | none |
| Adapter | `run-t8-adapter` | 42 passed, 0 failed | none |
| Facade | `run-t8-facade` | 41 passed, 0 failed | none |
| FORM | `run-t8-form` | 90 passed, 0 failed | **present** |
| Integration | `run-t8-integration` | 74 passed, 0 failed | **present** |

Total: 297 assertions, 0 failed, in six fresh kernels. The Loading, Standalone,
Adapter and Facade suites executed the complete current source. The last
adapter edit (symmetric `ContractorExecutable` validation) is exercised
end-to-end by the post-edit Adapter, Facade, FORM and Integration runs; Loading
and Standalone reach restricted dispatch only through capability refusals,
which return before the edited adapter code, so their `t7` runs remain valid.
After their final runs, `Tests/FORM.wls` and `Tests/Integration.wls` received
comment-only documentation at the `Processes[]` assertions; no executable
statement changed, and each run directory preserves the exact script copy under
`scripts-as-run/`.

### Warning qualification

The `Unset::norep` messages in the FORM and Integration runs are **not**
failed assertions and are **not** caused by the facade, the adapter or the
tests' own fixture handling. Established as follows:

- The messages come from ``ProcessLink`Private`removeRunningProcesses``, which
  unconditionally `Unset`s all three cached stream entries for every reaped
  process (`.../ProcessLink/Kernel/ProcessLink.m`, lines 943-944); `Unset`
  warns when a specific entry does not exist.
- `Processes[]` triggers reaping of every finished, unfreed child. Both suites
  call `Processes[]` in their final donor-inherited "no running children"
  assertions.
- The facade's adapter always requests all three streams and closes them; a
  raw-ProcessLink control with that exact pattern reaped silently (probe
  `run-p16-processlink-control`), and probes `p12`-`p15` found no stream leaks
  or cache warnings for any restricted call type.
- The owner runtime `CalcFormConverter/FORMRuntime.wl` requests only
  `StandardOutput`/`StandardError` (`drain[]`, line 110) and never caches
  `StandardInput`; its completed children therefore hold no `StandardInput`
  entry, and only that stream type appears in the warnings.
- Correlation across runs: these ProcessLink warnings appear exactly in the
  suites that run full-pipeline calls and end with a `Processes[]` assertion;
  the facade-only suites show no ProcessLink warnings. This statement is about
  ProcessLink only; expected partial-reduction diagnostics are classified
  separately below.

Proposed correction (outside API-01 scope because it changes `CalcForm*`
implementation): have `runtimeProcess` request and close the child's
`StandardInput` stream (populating the cache), or explicitly free finished
children after collecting diagnostics. Donor reports contain no captured
messages at all, so they neither confirm nor contradict the donor-run
behaviour. These two runs are therefore recorded as **warning-bearing
verification**, distinct from the clean runs above.

## Correction round — supervisor review, 20 October 2026

Three further bounded launches were authorised and used; no further launches
remain in that round. These results are separate from the earlier tables above;
the later single final batch is recorded in its own section below.

| Check | Run directory | Result | Warnings |
| --- | --- | --- | --- |
| Baseline warning control (unchanged fa4fe35 converter) | `run-w1-baseline-warning-control` | reproduced; result `D`; reaped uids 0 and 1 | `Unset::norep` (intended observation) |
| Installer fixtures and option resolution | `run-w2-installer-options` | 21 passed, 0 failed | none |
| Strengthened facade suite | `run-w3-facade-supported` | 62 passed, **2 failed** | expected `CalcFormImport::partialmasters` ×3 + `General::stop` (partial scalar-master reduction of the loop fixture) |

**Baseline warning control (item 4).** The control loaded the **unchanged**
`CalcFormConverter` subtree extracted with `git archive` from `fa4fe35` into
`/tmp/opencode/api01-baseline-fa4fe35` (manifest
`/tmp/opencode/api01-baseline-fa4fe35-manifest.txt`; identity record
`baseline-identities.txt`; no checkout switch and no production files touched).
`FORMRuntime.wl` is byte-identical between that baseline and the working tree
(sha256 `f69ac526…`); `CalcFormConverter.wl` differs only by the appended
guarded `Simplification` load. One minimal `CalcFormCalculate[MTD[mu,mu]]`
returned `D`; the subsequent `Processes[]` reaping emitted
`Unset::norep` for `$StreamObjectCache[0, StandardInput]` and
`$StreamObjectCache[1, StandardInput]`. The warnings are therefore reproduced
by legacy code alone and are classified as an **existing limitation of the
branch's `CalcForm*` process hygiene with Wolfram ProcessLink**, retained as an
unfixed limitation at acceptance. They are not caused by the facade, the
adapter or the tests. The earlier three-stream control (`p16`) remains a
negative control; the earlier log references above are retained unchanged.

**Installer fixtures and option resolution (items 1 and 2).** All 21 checks
passed, including: optional modules absent from unconditional `requiredFiles`;
revision-aware dependency edges retained; a pre-facade archive layout validates;
with the guarded `Get` edges present, each missing companion
(`Contractor.wl`, `FORM.wl`, `Facade.wl`, both stepwise and one-at-a-time) is
rejected by name and the complete layout validates; delayed counters prove that
`FORMBenchmark` does not evaluate delayed `ContractorExecutable`/
`ContractorThreads`, `Contractor` does not evaluate delayed
`FORMExecutable`/`FORMThreads`, and the full pipeline does not evaluate
Contractor options; selected delayed values resolve exactly once; invalid
selected values are rejected; invalid unselected values are inert; unknown
names and invalid shared runtime values are still rejected on both restricted
methods. No installation, download or user-path change occurred.

**Strengthened facade suite (item 3).** The suite now separates supported
successes from deliberate failure parity. 62 checks passed and two failed:
`tensor chain supported known answer` and `nested option list rejected`. Both
are **test-expectation errors identified from the retained log**, not
implementation defects: the tensor-space result is algebraically but not
structurally identical to the `SPD` shorthand (the same fixture used algebraic
equality before), and the nested option list was accepted by the evaluator
(no `Failure` returned), so standard semantics are acceptance rather than
rejection. The expectations were corrected statically (algebraic comparison for
the tensor chain; nested-list acceptance recorded) but **could not be re-run
within this round's three-launch budget**. The corrected suite has not yet been
verified in a fresh kernel; run `w3` retains the pre-correction test file under
`scripts-as-run/`. All other strengthened checks passed: supported success,
known answers and agreement for Dirac square/trace, colour Casimir and the loop
vacuum scalar; failure-tag parity for the two intentional invalid fixtures;
conflicting duplicate options (loop and method); option-list and nested-list
call forms; stored delayed selected/unselected values; explicit-over-stored
precedence and explicit capability overrides. The run exited nonzero and the
runner retained `execution-invalid.json`: one launch, kernel exit 1, 5.5 s,
peak cgroup memory about 230 MB, MemoryMax 1 GiB and swap 0 enforced.

**Affected regressions after this round.** `install.m`, `Simplification/Facade.wl`,
`Simplification/Contractor.wl`, `Simplification/Tests/FORM.wls` and
`Simplification/Tests/Loading.wls` changed after the earlier `t8` evidence. The
installer change is covered by `run-w2-installer-options`; the option-resolution
change is covered by `w2` and `w3` (both restricted methods, fixture and real
FORM, full pipeline); the replaced `FORM.wls` expectation (inactive Contractor
value) is asserted by `w2`; the `Loading.wls` installer section now mirrors the
`w2` fixtures. The six earlier suites were not re-run in this budget, so their
`t7`/`t8` counts above apply to the pre-correction source and are not claimed
for the corrected tree.

## Final verification batch — 20 October 2026

One further authorised fresh-kernel launch, `run-final-api01`, used the narrow
combined batch `controls/final-batch.wls` (single orchestrator, no `Quit`-based
inclusion). Source identity at launch: repository HEAD
`fa4fe3518c470a30449ff48e3c6ce7539ac6bce7`; working-tree `Simplification/Facade.wl`
sha256 `6fe8b052…`, `Simplification/Contractor.wl` `2b758f0d…`,
`Simplification/Tests/Facade.wls` `7d23b419…`, `install.m` `8b85738f…`; the
exact script copy is retained under `run-final-api01/scripts-as-run/`.
Runner: one launch, kernel exit 0, 5.3 s, peak cgroup memory about 236 MB,
`MemoryMax=1 GiB` and `MemorySwapMax=0` enforced, repository snapshots unchanged.

**Per-section substantive counts (all passed):**

| Section | Checks | Result |
| --- | --- | --- |
| S1 supported successes (5 fixtures × success/known answer/agreement) | 15 | 15 passed |
| S2 failure parity (intentional invalid fixtures) | 2 | 2 passed |
| S3 conflicting duplicate options | 4 | 4 passed |
| S4 option lists (flat and observable nested control) | 2 | 2 passed |
| S5 stored delayed settings and precedence | 5 | 5 passed |
| S6 affected restricted-backend regressions | 15 | 15 passed |
| Message-classification checks | 6 | vacuous, see below |

Total counted: 49 passed, 0 failed. The per-section printouts in the retained
log include the preceding section's message-audit check, so they read one higher
than the substantive counts above; the audit checks are the six vacuous ones
listed separately. The S4 nested control is the strengthened
test requested in review: the nested option list
`{{Method -> "FORMBenchmark", LoopMomenta -> {q}}}` returned `MethodCapability`
with `Method == "FORMBenchmark"` and refused `LoopMomenta`, proving the nested
options were honoured (ignoring them would have selected the full pipeline and
returned `D`). Supported-success checks now reject `Failure` explicitly on every
branch, and the tensor-chain known answer uses the retained algebraic
comparison; deliberate failure parity remains a separate section.

**Message audit limitation.** The batch attempted to classify messages by
resetting `$MessageList`; that symbol is Protected on Wolfram 15.0.1, so the six
message-classification checks were vacuous and the reset itself emitted
`Set::wrsym` (three printed messages plus one `General::stop` notice). Full
message inventory of the run, classified manually from the retained log:

- `CalcFormImport::partialmasters` ×3 (+ one `General::stop`): expected partial
  scalar-master reduction for the loop vacuum fixture, whose coefficient remains
  formal by design. This is not a full-reduction claim.
- `Set::wrsym` ×3 (+ one `General::stop`): test-harness defect in the batch's
  audit reset, not an implementation message. No implementation message was
  suppressed; no `Quiet` was used around the mathematical calls.
- No other messages occur in the run. The substantive assertions are unaffected,
  but the batch did **not** achieve automated message auditing; the next
  authorised run must replace this mechanism (for example by capturing
  `Messages` output or auditing the retained log) before a clean-run claim.

## Correction and budget audit

Runs are counted by retained run directory (one kernel launch each; 56
directories at this hand-off: the 52 behind the earlier tables, the three
supervisor-review runs and the single final batch).

| Failure class | Corrective rounds | Notes |
| --- | --- | --- |
| Runner/source-path harness defects (`t1`, `t1b`) | 2 | Invalid harness state, not implementation defects. |
| One-time load guard (`t1c`, `t1d`, `t1e`) | 2 | Guard existence test then option-store precedence; final pass. |
| Facade suite expectations (`t4`, `t4b`, `t4c`, `t4e`) | 2 corrective hypotheses + reconfirmations | One edit accident (unbalanced bracket, `t4b`) and one diagnostic-detected owner stream issue; `t4e` clean. |
| Restricted FORM retention/warnings (`t5`-`t5d`) | 2 | Log-name vocabulary bug, then stream-scope test adjustment; four read-only diagnostic probes (`p12`-`p15`) preceded. |
| Integration expectations (`t6`, `t6b`, `t6c`) | 2 | Dispatch-scoped validation and helper duplicate-rule semantics; `t6c` clean. |
| Warning investigation | 1 control probe | Causation established without corrective launches. |
| Post-reconciliation revalidation (`t8`) | 1 | Adapter validation change; four suites re-run; all clean except the documented owner diagnostics. |
| Supervisor review round 2 (`w1`-`w3`) | 3 launches, exactly the authorised budget | Baseline warning control reproduced the legacy warning; installer/option batch 21/21; strengthened facade 62/64 with two test-expectation errors diagnosed statically and corrected without re-running (budget exhausted). No automatic retry. |
| Final single batch (`final-api01`) | 1 launch, exactly the authorised final batch | Corrected facade option tests and affected restricted regressions: 43 substantive checks passed, 0 failed; 6 message-audit checks vacuous because `$MessageList` is Protected (harness defect, documented). No retry. |

Diagnostic overhead (option-semantics and load-guard probes, `p1`-`p9c`)
exceeded the nominal per-failure budget during the guard investigation; those
launches were read-only probes, several invalid because of probe-side errors,
and are retained rather than hidden. No resource limit was raised, no launch
was retried under the same name, and no repeated attempt was made after a
provider-quota failure.

## Limits of this record

- Linux x86-64 only; Wolfram 15.0.1 with FeynCalc 10.2.1; other systems and
  front-end rendering untested.
- No real Contractor binary: the Contractor protocol is verified against the
  test double and unit adapter suite only.
- No Full benchmarks, no performance claims, no library or model regeneration,
  no installation workflows, no loop-level certification.
- `CalcFormCalculate`/`CalcForm*` behaviour is asserted unchanged by the
  suites (`converter unchanged`, option and downvalue snapshots); this record
  does not re-certify the published 4.1.0 converter.
- The warning is reproduced by unchanged legacy code (`w1`); it is classified as
  an existing limitation retained at acceptance. Warning-bearing suites
  are not clean-run claims, and no fix is claimed.
- The corrected option expectations were verified by the final batch
  (`run-final-api01`), but the Facade suite file as a whole has not been re-run
  since its strengthening; its other sections are covered by earlier `t8`/`w3`
  evidence. The six earlier suites have not been re-run since the installer and
  option-resolution corrections; their results apply to the pre-correction
  source. Acceptance uses those applicable earlier results together with the final
  targeted batch; a complete final-source rerun is not claimed or required for
  closing API-01. Future changes require checks appropriate to their scope.
- The final batch's automated message classification was ineffective
  (`$MessageList` Protected); messages were classified manually from the
  retained log. Any clean-run claim must replace that mechanism.
- Real Contractor remains unverified: only the process double was available.

## Reproduction

From the evidence directory, with a fresh run name per suite:

```sh
python3 runner/api01-run.py --name <fresh> \
  --wls /home/latosh/.Wolfram/Applications/FeynGrav/Simplification/Tests/<Suite>.wls \
  [--env FG_CONTRACTOR_TEST_BINARY=<fixture> --env FG_CONTRACTOR_TEST_FIXTURE=<fixture>]
```

The control scripts used in the supervisor review round are retained at
`controls/w1-baseline-warning-control.wls` and
`controls/w2-installer-options.wls`; both need a new run name, and `w2` and the
facade suite need `FG_CONTRACTOR_TEST_FIXTURE`. The baseline control needs the
git-archive copy at `/tmp/opencode/api01-baseline-fa4fe35` (identity record
above).

The fixture may be rebuilt with
`go build -o fixture/fixture-process fixture/ProcessFixture.go`; record the new
hash if it changes. Do not reuse consumed run names.
