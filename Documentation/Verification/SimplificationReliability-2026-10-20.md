> Source-path update (21 October 2026): the current `Simplification/` directory is now `FeynGravSimplify/`. Paths and hashes below describe their historical snapshots; external evidence archives were not moved.

# Reliability fixes — 20 October 2026 (initial attempt blocked; corrected repair verified)

Status: the first attempt was blocked by a production defect in the new
load-guard completeness check and its evidence is retained below unchanged. A
corrected repair was then authorised, implemented and verified in one fresh
batch: the load guard now clears partial definitions per attempt, records each
`Get` outcome, requires callable definitions (DownValues) before marking
success, and the reporting path distinguishes Complete/Failed/Aborted with
the abort re-raised. **The corrected repair is retained for review; it is not
self-accepted.** The accepted API-01 and maintainability records remain
unchanged. This note does not revise
[SimplificationMaintainability-2026-10-20.md](SimplificationMaintainability-2026-10-20.md)
or the accepted API-01 history.

## Attempted changes (retained only as evidence)

- Abort reporting in `executeRestrictedBackend`: `status = "Aborted"` default and
  `CheckAbort[simplify[...], status = "Aborted"; Abort[]]` so the terminal
  report names the abort and the abort is re-raised unchanged.
- Load guard in `CalcFormConverter.wl`: marker **value** checked in a separate
  statement (`TrueQ[...]`), modules loaded, then the marker set only when an
  essential interface check passes.
- Tests: abort/failure terminal-status capture with progress/timing enabled,
  leaked-private-state and child-stop checks, and an isolated missing-companion
  repair/reload section in `Loading.wls`.

Diffs and the composed batch are retained in
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-reliability/`
(`snapshot/` holds the accepted pre-edit bytes; the composed batch is
`controls/reliability-verify.wls`, sha256 `4ff19e80…`).

## Verification result

One authorised launch `run-reliability-verify` (existing runner; fixture; 1 GiB
cap, zero swap, 120 s + 10 s): kernel exit 1, 5.4 s, peak cgroup memory about
236 MB. Sections: S-LOAD loader recovery 2/3, S0 standalone/store 13/13,
S1 supported 15/15, S2 failure parity 2/2, S3 duplicates 4/4, S4 option lists
2/2, S5 stored/delayed 5/5, S6 restricted regressions 17/17, S7 contract 22/25
— **82 passed, 4 failed**.

Diagnostics: one controlled `Get::noopen` for the deliberately missing temporary
companion; three expected `CalcFormImport::partialmasters` plus
`General::stop`; one `FeynGravSimplify::files` retention notice from the abort
test; no unexplained product message.

### Failure diagnosis

1. **Loader recovery (production defect, confirmed statically).** The new
   completeness check used `Names[...]` (symbol existence). Reading the
   companion files interns `FeynGrav`Simplification`Private`failure` (FORM.wl
   body) and `simplify` (Facade.wl body), so existence is satisfied even when
   `Contractor.wl` is missing. The guard therefore set the marker on an
   incomplete interface, and the test's own existence assertion failed for the
   same reason. The check must require actual definitions (for example
   `Length[DownValues[symbol]] >= 1` or `OwnValues`), not names.
2. **Guard-scratch assertion (test-harness defect).** The composed batch still
   looked up the old scratch name `facadeGuardNames`; the guard now uses
   `facadeGuardLoaded`. A script-only rename defect.
3. **Failure/abort terminal text (unresolved).** Propagation, leaked-private-
   state and child-stop checks passed, but the captured terminal lines did not
   match `Failed`/`Aborted`. Whether this is a capture limitation
   (`$Output`/`WithCleanup` interaction during unwinding) or a reporting defect
   was not established; a second launch was not authorised after failure 1.

## Restoration and next action

Production and test files were restored to the accepted five-finding bytes:
`Facade.wl` `f3e1c0f8…`, `CalcFormConverter.wl` `4004507d…`, `Facade.wls`
`635a5e86…`, `Loading.wls` `3680f04d…` (loader section excised; diff shows only
that block), `Contractor.wl` unchanged. The accepted 78/78 record remains valid
for that snapshot.

Next authorised round should: (a) replace the guard's existence test with a
definition test for each private helper and re-run the missing/repair fixture;
(b) fix the scratch-name assertion; (c) instrument the failure/abort capture to
print the actual captured text before asserting; then re-verify. Real Contractor
remains fixture-only; the legacy ProcessLink limitation and expected
partial-reduction diagnostics are unchanged.


## Corrected repair — 20 October 2026 (verified)

Production changes, compared with the accepted five-finding snapshot
(`2026-10-20-api01-reliability2/diff-*.patch`):

- `CalcFormConverter.wl` guard: marker **value** read in a separate statement;
  on a not-yet-loaded attempt the partial `FeynGrav`Simplification`Private``
  definitions and the five public facade commands are cleared, so stale
  definitions cannot certify the current files; the three `Get` outcomes are
  recorded and must not be `$Failed`; success additionally requires
  `Length[Options[FeynGravSimplify]] > 0` and `Length[DownValues[...]] > 0` for a
  short explicit inventory (private `failure`, `reportProgress`, `simplify`,
  `formConfiguration`, `formProgram`, `formDecode`, `validateOptionNames`,
  `restrictedCapabilityRefusals`, `executeRestrictedBackend`; the five public
  facade commands). Failed or aborted attempts leave the marker unset so
  repaired files can be retried; a successful load preserves configured stores
  across `Needs`/re-Get/main load. Partially initialised settings may be reset
  on recovery; no transactional rollback of arbitrary broken code is promised.
- `Facade.wl` restricted reporting: `status` initialised to `"Aborted"`;
  `CheckAbort[simplify[...], status = "Aborted"; Abort[]]` keeps the abort
  local for the terminal report and re-raises it unchanged. Process
  cancellation, retained-file cleanup and `runJob` are untouched.

New self-contained repository suite `Simplification/Tests/Reliability.wls`
(candidate sha256 `efdd90ba…`), run once as
`run-reliability2b-verify` under the verified runner with the fixture:
kernel exit 0, 3.3 s, peak cgroup memory about 228 MB, 1 GiB cap and zero swap
enforced. Sections: L load-guard recovery **10/10**, S store preservation
**5/5**, R reporting **14/14** — **29 passed, 0 failed**. A preceding run
(`run-reliability2-verify`) failed one harness assertion because `CopyFile`
did not overwrite the existing empty companion; the production loader behaved
correctly (empty companion rejected) and only the test's repair step was
changed (`OverwriteTarget -> True`), which is the authorised test-harness-only
correction.

Diagnostics, classified from the raw log: one controlled `Get::noopen` for the
deliberately missing temporary companion; two expected
`FeynGravSimplify::files` retention notices from the controlled failure and
abort, whose job directories are retained for review
(`/tmp/feyngrav-formbenchmark-7e3b1585…` and `36a36a76…`). No loop fixture was
used in this suite, so no `partialmasters` notice appears; no unexplained
message was emitted. Captured payloads are printed in the raw log for the
success, failure and abort paths: terminal statuses `Complete`, `Failed` and
`Aborted` respectively, one timing record and one terminal progress record
each; the abort case also confirms the fixture child is stopped.

Candidate identities: `Facade.wl` `c22c4c6e…`, `CalcFormConverter.wl`
`d288d229…`, `Reliability.wls` `efdd90ba…`. Evidence:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-api01-reliability2/`
(accepted snapshot, diffs, pre-run state, post-edit copies) and
`.../api01-interface/run-reliability2b-verify/` (raw log, payloads, resource and
termination records). The earlier blocked run `run-reliability-verify` and its
(pre-edit-labelled after correction) directory remain untouched.

### Post-run static test alignment (not executed)

The retained `Simplification/Tests/Facade.wls` guard-scratch assertion still
inspected the superseded scratch name `facadeGuardNames`. This was a static
integration defect: the current guard uses `facadeGuardLoaded` and
`facadeLoadResults`. The check was corrected after the run to evaluate `Names`
in separate statements before the literal symbols are interned, then verify
both current scratch symbols have no `OwnValues`, `DownValues`, `UpValues` or
attributes (`ClearAll` keeps the symbols but clears their state). **This
correction was not executed in reliability2b; the two-launch budget was
consumed. It is a static alignment only, to be exercised by the suite's next
run.** Historical notes mentioning `facadeGuardNames` describe the earlier
five-finding snapshot and are preserved unchanged.

Diff: `2026-10-20-api01-reliability2/static-alignment/diff-Facade.wls.patch`;
`Facade.wls` changed from `635a5e86…` (accepted five-finding) to `f47e2d01…`.

### Limitations (continued)

Interrupted companion execution was **not** directly tested. The loader
scenarios covered a missing companion, an empty companion and repair/retry;
they do not establish recovery from arbitrary corrupt or partially executing
companion code, and no transactional rollback is claimed. Real Contractor
remains fixture-only; the accepted 78-check maintainability batch was not
repeated; the legacy ProcessLink limitation is unchanged; loop
partial-reduction qualifications still apply to suites that use loop fixtures.
No commit or push.
