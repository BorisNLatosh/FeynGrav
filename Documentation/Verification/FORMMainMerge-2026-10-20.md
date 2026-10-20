# Main integrated into FORM_Simplification — 20 October 2026

## Source and scope

Merge of published-main revision `ad2a050efa3f3463230add76b46bf2ad0d9aa8eb`
into FORM revision `0b9ab7ed867960fefca7dca997924cf8e3b23fc1`.
The user-created `FORM_Simplification_backup` retains the original FORM tip.
Neither main nor the published tag is changed. The Contractor branch is separate.

Five documentation conflicts were resolved. Concise README entry points and
main's guide organisation are retained; branch-specific loop documentation is
carried into the detailed guides. Main's native-model, citation and importer
reference entries are retained alongside the loop-coefficient reference.

Installer validation now covers five additional loop companions and the
LoopDenominatorGroups FORM template. These requirements remain conditional on
references in the owning source, preserving the historical-layout mechanism.
The branch is explicitly labelled as containing extensions beyond released 4.1.0;
the historical release notes are unchanged.

## Checks on the combined working tree

- **10 focused checks passed:** current importer availability, default tensor and
  scalar-master options, negative gravitational ghost propagator, ordinary FORM
  algebra, rank-two bubble normalisation against independent FeynCalc TID/PaVeReduce,
  coefficient-only mode, serial/TFORM agreement, disabled integration and a
  repeated-power reduction.
- **766 core assertions passed:** Core 111, Parser 121, SharedParser 148,
  TreeParser 304, Transactions 20 and Installer mocks 62. These include the
  core suite's legacy-fixture coverage; they are not a rerun of every saved format.
- **26 layout checks passed:** complete temporary fixture and missing-file rejection
  for each of 25 runtime dependency edges, including all six added edges.
- Rules, Libs, Examples, FeynArts model bundles and FeynGrav.wl match main exactly.
  No interaction, library, model or notebook was regenerated during integration.
- No unresolved Git conflicts remain. Markdown relative links were checked;
  mathematical bracket expressions in the developer guide were excluded as false
  positives. Source/documentation whitespace passed; inherited notebook whitespace
  was left unchanged.

Each sequential test batch used a task-owned systemd service with 1 GiB whole-tree
memory, zero swap, 120 seconds runtime and 10 seconds stop grace. Fresh host
headroom of at least 2 GiB was required. A harmless payload checked the wrapper
before scientific execution. All batches exited zero without memory-limit/OOM
counter events; no Wolfram warning messages were observed.

| Batch | Service wall time | Retained cgroup memory peak (bytes) |
| --- | --- | --- |
| Focused integration | 4.631 s | 401805312 |
| Core suite | 30.432 s | 644132864 |
| Installer layout | 2.676 s | 120979456 |

These are verification timings, not performance comparisons. Memory values are
retained cgroup peaks, not individual kernel RSS or the final service summary.

Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-20-form-main-merge/`.
The directory retains scripts, logs, result JSON, memory counters and
`tested-source-hashes.json` identifying the tested uncommitted merge snapshot.
Subsequent changes before committing were documentation only.

## Limits

No Full benchmark, large realistic import, native-model rebuild, comprehensive
loop-reduction regression or published-archive installation was performed.
The historical loop-reduction and performance limitations remain; this merge
check does not establish full release readiness of the combined branch.
