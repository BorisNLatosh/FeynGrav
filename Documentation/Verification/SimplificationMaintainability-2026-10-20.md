> Source-path update (21 October 2026): the current `Simplification/` directory is now `FeynGravSimplify/`. Paths and hashes below describe their historical snapshots; external evidence archives were not moved.

# Simplification maintainability review and candidate verification — 20 October 2026

Status: **accepted on 20 October 2026; uncommitted and unreleased.**

## Maintainability changes accepted — 20 October 2026

The user accepted the five-finding maintainability changes on
`FORM_Simplification`; they remain uncommitted and unreleased. The facade now
separates capability checking and restricted execution, uses fixed option
defaults, documents module ownership and includes the corrected regression
checks in repository tests. The original loading guard remains intact.

The retained `run-fiveplan-verify` batch passed **78/78 checks**, including
standalone/main loading and option-store preservation. This was a composed
batch, not an individual rerun of every repository suite. Its log retains the
test-variable shadowing warning and expected partial-reduction notices; it is
not a warning-free run. Real Contractor remains fixture-tested only, and the
legacy ProcessLink limitation is unchanged. No performance, full-regression,
loop-certification or release-readiness claim follows from acceptance.

The repository facade test's `check` parameter was subsequently renamed from
`name` to `caseLabel` to avoid the collision. This is a static, test-only rename;
no new kernel was run and the old logs/scripts remain unchanged. The tested
facade-test hash was `635a5e86…`; the post-cleanup SHA-256 is
`fda7a797a34db2f632cd4f625bdd098947d84c79a2ef17a01528fc8b79b4ad47`.
Production source identities and full evidence are recorded in repository
`Documentation/Verification/SimplificationMaintainability-2026-10-20.md`.
ARCH-01 remains unstarted and requires a separate assignment. Nothing committed.

Branch `FORM_Simplification`, HEAD
`fa4fe3518c470a30449ff48e3c6ce7539ac6bce7`. The working tree was first returned
byte-for-byte to the accepted implementation:

- `Simplification/Facade.wl` sha256
  `6fe8b05281edd119144a17bcdcf046c87a929d3c088c2f0d5a630113798f5c71`
  (matches the accepted record's `6fe8b052…`);
- `CalcFormConverter/CalcFormConverter.wl` sha256
  `6799c1ec258c0a56a3988ff70d37e4a57e0aab60582e5fd91a1030b4cddb58a1`.

Pre-edit hashes, post-edit copies and unified diffs of the attempted refactor are
retained at
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-maintainability/`
(`pre-edit-state.txt`, `snapshot/`, `post-edit/`, `diff-Facade.wl.patch`,
`diff-CalcFormConverter.wl.patch`).

## Critical findings

1. `Simplification/Facade.wl` defined
   `CalcFormConverter`FeynGravSimplify::restricted` and never emitted it — dead
   public message surface.
2. `$simplifyRestrictedBaseline` was defined but never used; the nine baseline
   pairs were duplicated inline in the `MethodCapability` refusal block, giving
   two sources of truth for the same contract constants.
3. Four commands (simplify, Export, Check, Install) repeated the same
   option-name/stored-name validation block with slightly different failure
   data; the duplicated code is easy to let drift.
4. The guarded load created a temporary private symbol
   (`CalcFormConverter`Private`facadeGuardNames`) and then cleared it; the
   indirection had no purpose.
5. The `FeynGrav`ContractorExecutable`/`ContractorThreads` usage strings said
   unselected values are "validated but stay inactive", contradicting the
   accepted selected-only resolution (they are never evaluated).

## Prepared changes (not retained)

- Remove the dead `::restricted` message.
- Build refusals from `$simplifyRestrictedBaseline` and explicit per-call
  resolution, removing duplicated baseline literals; behaviour and refusal order
  preserve the accepted contract.
- One `validateOptionNames[command, optsList, allowed, withSupported]` helper
  used by the four commands, preserving failure tags, message text, and data
  keys (`UnsupportedOptions`, `SupportedOptions` only for simplify,
  `UnsupportedStoredOptions`).
- Inline the load guard as `If[Names["…$FacadeLoadedMark"] === {}, …]`, dropping
  the temporary symbol and `ClearAll`.
- Correct the two usage strings to "never evaluated and stays inactive".

## Verification attempt

One authorised fresh-kernel batch
(`run-m1-maintainability`, script `controls/maintainability-batch.wls` in the
accepted evidence directory) was launched against the patched bytes. It failed:
11 passed, 52 failed; kernel exit 1, 3.6 s, peak cgroup memory about 229 MB,
1 GiB cap and zero swap enforced, repository unchanged. The facade commands were
absent in that kernel — calls returned unevaluated,
`Options[CalcFormConverter`FeynGravSimplify]` was empty, and
`SetOptions` reported the documented option names as unknown, while
`CalcFormCalculate` and the owner pipeline worked (a `CalcFormImport::partialmasters`
diagnostic was produced). The log contains no syntax, `Get::noopen` or other
load diagnostics.

Statically excluded: bracket/comment/string balance in both edited files; the
`Names[...]` guard string is byte-identical to the accepted form; the batch's
load lines are identical to the accepted `run-final-api01` script. The cause of
the missing module load is **not established**. Under the production-mismatch
rule no further kernels were launched, and the accepted bytes were restored.

### Follow-up correction — guard semantics (reviewer diagnosis)

The earlier conclusion that the temporary guard variable had "no purpose" and
that the cause was "not established" is corrected. Source/evaluation-semantics
review establishes that the separate statement is intentional: Mathematica
interns the literal `FeynGrav`Simplification`Private`$FacadeLoadedMark` symbol
while reading the whole `If` expression, before its condition evaluates, so a
combined guard always sees the marker and skips the module loads. The original
guard is therefore preserved exactly (separate `Names` statement and
`ClearAll`), now with an explanatory comment; no loader redesign. That comment
is the only retained production-text change and was exercised by the successful
load below.

## Candidate re-verification — 20 October 2026

One authorised fresh-kernel batch ran the corrected candidate: script
`controls/verify-batch.wls` in the maintainability-verify evidence directory
(`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-maintainability-verify/`);
the verified runner creates the run directory under its fixed accepted evidence
root as `2026-10-20-api01-interface/run-m2-maintainability-verify`. Source identity at launch: HEAD `fa4fe35`;
candidate `Simplification/Facade.wl` sha256 `2c55e9f6…`;
`CalcFormConverter/CalcFormConverter.wl` `4004507d…` (guard restored; comment
only); runner and fixture hashes unchanged from the accepted record. Kernel exit
1 (failed assertions), 9.4 s, peak cgroup memory about 235 MB, 1 GiB cap and
zero swap enforced.

The early fail-fast passed (`LOAD-CHECK: stores 18/9/3/1`), so the facade
definitions and stores loaded. Substantive sections all passed:

| Section | Checks | Result |
| --- | --- | --- |
| S1 supported successes | 15 | 15 passed |
| S2 failure parity | 2 | 2 passed |
| S3 duplicate options | 4 | 4 passed |
| S4 option lists and observable nested control | 2 | 2 passed |
| S5 stored delayed and precedence | 5 | 5 passed |
| S6 restricted-backend regressions | 17 | 17 passed |
| S7 facade validation | 18 | 13 passed, **5 failed** |

Total 58 passed, 5 failed. The five failures are all newly added S7 assertions:
`simplify unknown option data`, `export unknown option data`,
`check stored option data`, `simplify stored option rejected` (data leg) and
`guard scratch cleaned`. Their tag and message-text counterparts passed. The
retained log does not print the actual failure data, and no second kernel was
authorised, so the discrepancy between the assertions and the produced failure
data is not resolved. It may be a test-expectation defect or an error-data
representation change; it is not established either way.

Diagnostics, classified manually from the raw log: three expected
`CalcFormImport::partialmasters` partial scalar-master reduction notices (loop
fixture, formal coefficient) plus one `General::stop` suppression notice; no
other messages, no ProcessLink warnings (the batch does not reap processes).

Outcome: the candidate `Simplification/Facade.wl` was **not retained** and the
file was restored to the accepted bytes
(`6fe8b05281edd119144a17bcdcf046c87a929d3c088c2f0d5a630113798f5c71`). The
converter keeps the guard-rationale comment (`4004507d…`). This run verifies
the corrected loading guard and the accepted contract sections, but does not
verify or accept the facade refactor.

### Diagnosis of the five S7 failures

The failure review corrected two defective test expectations without touching
production data:

- Four error-data assertions compared against bare strings (`"MadeUpOption"`,
  `"FORMRates"`). Production serialises option symbols with
  `ToString[..., InputForm]` in the running formatting context, so
  context-qualified names are valid. The corrected checks compute the expected
  associations with serialization blocks copied verbatim from the accepted
  implementation snapshot, run in the same batch context, and compare the full
  failure associations (keys, message text and serialized names) by `===`.
  Complete symbol identity is checked before serialization; no error text is
  parsed with `ToExpression`. On mismatch the actual and expected data are
  printed.
- The cleanup assertion inspected a holder variable (`Symbol[...]`) whose own
  `OwnValues` were non-empty. The corrected check reads the scratch symbol
  presence first, then inspects `OwnValues`, `DownValues`, `UpValues` and
  `Attributes` of the fully qualified
  `CalcFormConverter`Private`facadeGuardNames`. `ClearAll` clears values and
  attributes but does not remove the symbol, so existence is expected.

The stale S7 comment describing an inline guard was replaced; the load
fail-fast remained.

## Candidate verification — 20 October 2026 (passing; expected partial-reduction notices)

One authorised fresh-kernel batch (`run-m3-maintainability-final`, script
`2026-10-20-api01-maintainability-final/controls/final-verify-batch.wls`)
verified the retained candidate. Source identity at launch: HEAD `fa4fe35`;
`Simplification/Facade.wl` sha256 `2c55e9f6…`;
`CalcFormConverter/CalcFormConverter.wl` `4004507d…` (accepted guard plus
comment only); runner and fixture hashes unchanged. One launch, kernel exit 0,
9.3 s, peak cgroup memory about 235 MB, 1 GiB cap and zero swap enforced.

Fail-fast passed (`LOAD-CHECK: stores 18/9/3/1`). All sections passed:
S1 supported successes 15, S2 failure parity 2, S3 duplicate options 4,
S4 option lists 2, S5 stored delayed 5, S6 restricted regressions 17,
S7 facade validation 18 — **63 passed, 0 failed**. The S7 comparisons confirm
the candidate's failure data matches the accepted serialization exactly.

Diagnostics, classified manually from the raw log: three expected
`CalcFormImport::partialmasters` partial scalar-master reduction notices (loop
fixture, formal coefficient) plus one `General::stop` suppression notice; no
other messages and no ProcessLink warnings (the batch does not reap processes).
Loop-fixture checks remain partial-reduction results, not full-reduction claims.

The candidate `Simplification/Facade.wl` (`2c55e9f6…`) is retained for review;
the accepted baseline remains `6fe8b052…` in the accepted record. The converter
retains only the guard-rationale comment (`4004507d…`, accepted `6799c1ec…`).
Real Contractor remains untested (fixture double only); the ProcessLink warning
limitation is unchanged. This note does not accept the refactor.

Evidence: failed run `run-m1-maintainability` (retained log, resource record,
`execution-invalid.json`) and the maintainability directory above; accepted
history remains in
[SimplificationExtraction-2026-10-20.md](SimplificationExtraction-2026-10-20.md).


## Five-finding implementation and verification — 20 October 2026

Status: implemented per the reviewed plan (dependency notes and durable tests;
immutable option declarations and pure capability comparison; coordinator
extraction), with the four reviewer corrections. **Retained for Codex/user
review; not self-accepted.** The accepted API-01 baseline record remains
`SimplificationExtraction-2026-10-20.md`; this candidate awaited separate review at submission; acceptance is recorded above.

### Changes

1. **Coordinator extraction** (`Facade.wl`): `FeynGravSimplify` remains the
   ordered `OptionsPattern` coordinator with all `OptionValue` reads lexical,
   ordered and once-only. Two private operations were extracted:
   `restrictedCapabilityRefusals[resolved_Association, baseline_List]` (pure,
   SameQ, baseline order) and `executeRestrictedBackend[expression, method,
   adapterRules, showProgress, showTiming, startTime]`, which owns its own
   `result`/`status` `Module` state, the `Block[{$showProgress, $backendLabel}]`
   scope, the inherited `WithCleanup` shape and the single terminal report. The
   full-pipeline forwarding block is unchanged and explicit.
2. **Immutable declarations** (`Facade.wl`): `$simplifyOptionDefaults` +
   `$simplifyOptionNames`, `$exportWrapperDefaults` + explicit
   `$exportWrapperOptionNames`, and derived check/install name lists; stores are
   initialised from the defaults once and remain independent and mutable; the
   validators use the immutable names. `FeynGravFORMImport` has no options and
   no store.
3. **Capability policy**: keyed `Association` lookup instead of symbolic
   replacement; completeness surfaces as `Missing[...]` in the private helper
   and is tested, with no new public error paths. Public refusal records, order,
   tag and message are unchanged.
4. **Dependency notes**: `Facade.wl` header block and `Contractor.wl` executor
   contract comment; `Simplification/README.md` module map stating load order,
   ownership, imported private operations and dynamic reporting variables, and
   the four (not five) option stores.
5. **Durable tests**: `Simplification/Tests/Facade.wls` now carries fail-fast
   prerequisites, independent accepted-contract literals, store/default/
   independence checks, the capability invariant, full error-association
   comparisons against accepted serialization blocks with identity checks,
   guard-scratch cleanup and the single-terminal-report capture.
   `Standalone.wls` adds store/default consistency and owner-isolation checks.

### Verification

One authorised fresh-kernel batch (`run-fiveplan-verify`, composed script
`controls/combined-verify.wls` in the five-finding evidence directory) started
with standalone loading, set sentinel wrapper options, re-loaded the standalone
converter, then loaded the main package and confirmed sentinels survived and
owner stores were untouched before restoring them. Sections: S0 standalone/store
13, S1 supported 15, S2 failure parity 2, S3 duplicates 4, S4 option lists 2,
S5 stored/delayed 5, S6 restricted regressions 17, S7 contract/error-data/
reporting 20 — **78 passed, 0 failed**. `LOAD-CHECK: stores 18/9/3/1`; kernel
exit 0, 9.5 s, peak cgroup memory about 235 MB, 1 GiB cap and zero swap
enforced; repository snapshots unchanged.

Diagnostics, classified from the raw log: three expected
`CalcFormImport::partialmasters` partial scalar-master reduction notices plus
`General::stop` (loop fixture; partial-reduction qualification, not full
reduction), and one `name::shdw` test-harness collision between the batch's
`check[name_, …]` variable and `FeynGrav`name` after the standalone-first load.
The shadow warning is test-only, did not alter counts or results, and was not
suppressed. The real Contractor backend remains fixture-double-only; the legacy
ProcessLink limitation is unchanged.

Candidate identities: `Simplification/Facade.wl` `f3e1c0f8…`,
`Simplification/Contractor.wl` `0471075a…`, `Simplification/README.md`
`8759c596…`, `Simplification/Tests/Facade.wls` `635a5e86…`,
`Simplification/Tests/Standalone.wls` `4cfba23b…`; `CalcFormConverter.wl`
`4004507d…` (accepted guard plus comment). Evidence: `2026-10-20-api01-fiveplan/`
(pre-edit snapshot, post-edit copies, diffs, pre-run state, composed batch) and
`2026-10-20-api01-interface/run-fiveplan-verify/` (raw log, script hashes,
resource and termination records).
