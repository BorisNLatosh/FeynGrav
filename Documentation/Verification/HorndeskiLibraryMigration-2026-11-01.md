# RULE-05: Horndeski library migration

1 November 2026; `FeynArts_Integration` at `a7c2a9a`, uncommitted.
**RULE-05 is resolved for all 60 shipped Horndeski library selections.**
All 14 G4 files now come from the corrected source, and the ordinary
four-argument `HorndeskiG4` interface returns those corrected rules.

## What changed

The [accepted G4 source](HorndeskiG4-2026-11-01.md) differentiates

$$
\lambda\sqrt{-g}\,\phi^a\left[X^bR-2bX^{b-1}\mathcal H_2\right],
\qquad X=(\partial\phi)^2,
\qquad\mathcal H_2=(\Box\phi)^2-H_{\mu\nu}H^{\mu\nu}.
$$

Use the (+---), curvature and positive Fourier conventions on the
[model page](../Models/Horndeski.md). The coupling remains the coefficient of
`phi^a X^b`, without hidden factorials. No source formula or importer code was
changed during this library migration.

| G4 selections, each at n=1,2 | Files | Exact old/new relation |
| --- | --- | --- |
| (a,b)=(0,1),(0,2),(1,1),(2,1) | 8 | Old = R - H/2; new = R + H, where H is the complete corrected Hessian contribution |
| (a,b)=(2,0),(3,0),(4,0) | 6 | New = a! times old: factors 2, 6 and 24 |

The real generator `GenerateHorndeskiG4Specific` constructed each candidate
from the accepted uncontracted source. Candidates were simplified with serial
FORM, written and read back by the generator, then compared to an independent
metric/connection/Hessian expansion. Only matching files were installed.
No hidden importer rescaling was introduced.

The [earlier G2/G3/G5 audit](HorndeskiAudit-2026-11-01.md) already corrected
four G5 b=1 libraries. Its 46 library hashes, accepted source/test hashes and
all listed retained result hashes were rechecked before reusing that evidence.
They are unchanged by this G4 migration. Across the combined audits, 18 of the
60 files changed and 42 remained byte-identical. The per-file inventory below
and the accompanying JSON retain both original and final identities.

## Exact checks

All new vertex comparisons keep symbolic D and generic off-shell momenta.
Unrestricted auxiliary vectors test the full separately symmetric graviton
tensors; every index-pair symmetry is checked independently. They are not
transverse or helicity-constrained polarisation states. The reference uses
nilpotent labelled fields and calls no production C/I/Gamma tensor or G4 kernel.

| Batch | Passing assertions | Scope |
| --- | --- | --- |
| G4 b=0 generation | 59/59 | Six files; independent action, changed-factor control, historical relation, pair symmetries, resolved output and exact inventory |
| G4 b=1 generation | 59/59 | Six files; the same checks with the corrected Hessian blocks |
| G4 b=2 generation | 21/21 | Two files; the same checks at four scalar legs |
| Candidate public interface and consumers | 109/109 | All 14 files with coupling 7+2i, zero/equal-momentum cases, pair symmetries, exact provenance, transactional recovery and John identities |
| Installed public interface | 109/109 | The same script in a fresh kernel against the installed files, with identical tested script bytes |

Thus 357 assertions passed in the new completed batches. These are assertion
counts, not 357 independent physical processes. The first public-harness
attempt stopped before assertions on `name::shdw`: its test variable `name`
collided with a package symbol. Only that test variable was renamed. The
failed script/log remains retained; diagnostics were not suppressed and no
mathematical expectation was changed.

The public checks reject an empty family and a late corrupt file while
preserving the previous definitions and provenance. Cancellation propagates
without partially replacing either. Repair restores the exact file bytes,
and retry succeeds with correct hashes and paths. The three-argument source
and four-argument library interfaces remain distinct; the importer still
accepts no order argument. Unrelated G5 definitions and records are unchanged.

For n=1,2, the actual library-backed G4=-beta X/2 and G5=-beta phi vertices
agree with each other and with an independent Einstein-tensor derivative
coupling. Only these integration-by-parts checks impose total incoming
momentum conservation. They use no field equations or on-shell conditions.
Wrong-factor and wrong-sign negative controls give nonzero residuals.

## Downstream consumers

The executable/native/numbered-example search found no general G4 consumer.
`FeynArtsIntegration/ScalarModelBuilder.wl` builds John from G5 with exactly
two scalar momenta, b=0 and coupling -JohnBeta at n=1,2. Its inputs are
`HorndeskiG5_1_0_1` and `_1_0_2`, which remain byte-identical. The corrected
G5 b=1 files imported for the builder's broader inventory do not enter those
vertices. Its manifest remains a historical build record, not a new rebuild.

The minimal-scalar and Einstein-scalar-Gauss-Bonnet models use separate rules.
No shipped native amplitude, diagram weight or Ward check depends on the
changed G4 files, so no affected complete-tree test requires rerunning. The
new John action identities provide the relevant cross-sector check; existing
John tree/Ward evidence retains its original scope. No new native or loop
amplitude certification is claimed.

`Benchmark/Support/GeneratorSupport.wl` includes the G4 (0,1,1) generator case.
The b=1 generation batch exercises this same command selection. The benchmark
notebook's saved timings remain historical; no Full benchmark or notebook
calculation was rerun. All model, example, benchmark and user-notebook files
remain unchanged by this migration.

## Scientific migration

Restart the Wolfram kernel after updating, import the required Horndeski
families again, and recompute results that used any historical G4 file or the
four corrected G5 b=1 files. Re-importing after a clean restart avoids using
old definitions and cached results. Saved notebook outputs do not update
when the files change.

For an isolated G4 b=0 vertex the correction is the stated a! factor. For
b>0 it changes relative action blocks, so a single global rescaling cannot
repair an old vertex or an amplitude assembled from it. The coupling's
monomial convention is unchanged: do not silently redefine lambda to hide a
previous implementation error. Historical user amplitudes were not rewritten.

## Reproduction and resource record

The reusable check is
[HorndeskiG4LibraryChecks.wls](../../Rules/Verification/HorndeskiG4LibraryChecks.wls),
with the existing independent references beside it. See
[the check instructions](../../Rules/Verification/README.md#g4-stored-library-and-import-regression).
The checked local executable is `/usr/local/bin/form`; the script uses one
serial FORM call. It requires an external resource wrapper and an isolated
output directory. It is a verification script, not a memory-limiting launcher.

Six fresh sequential kernel scopes were used: three generation scopes, the
failed loading harness, its one reserved correction, and the installed check.
The candidate-interface and consumer checks were combined, avoiding a duplicate
consumer run. Every scope had aggregate 1 GiB RAM, zero swap, 120 seconds plus
10 seconds termination grace, and fresh host headroom of at least 2 GiB.
Generation made at most two serial FORM calls per file (construction processing
and independent projection); each public suite made one. A serial test adapter
also kept source helper work within the owned process tree.

| Scope | Kernel/wrapper seconds | Highest retained cgroup bytes | Result |
| --- | ---: | ---: | --- |
| generate-b0 | 4.782 | 215,912,448 | passed |
| generate-b1 | 14.635 | 402,890,752 | passed |
| generate-b2 | 12.024 | 426,893,312 | passed |
| candidate-public | 3.258 | 213,385,216 | harness-load-failed |
| candidate-public-corrected | 7.394 | 323,301,376 | passed |
| installed-public | 7.502 | 323,182,592 | passed |

All scopes had zero memory-limit/OOM events and zero swap; all owned processes
terminated. The accepted scopes had empty diagnostic logs. Memory values are
sampled cgroup peaks, not isolated kernel RSS; timings are local verification
measurements, not general generator benchmarks. The initial automatic-approval
rejections launched no kernel; the maintainer subsequently explicitly confirmed
RULE-05 execution and installation without committing.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-rule05-migration/`.
Original libraries, exact generated files, FORM jobs, comparison WXF files,
script/source hashes, raw diagnostics, resource observations, installation
receipts and protected-file snapshots are retained. The JSON manifest records
source identities and hashes for the raw evidence. Earlier failures and reports
are preserved rather than relabelled as successful current runs.

## Scope limits and task closure

This closes the stored-library/consumer migration for the shipped 16 G2,
20 G3, 14 G4 and 10 G5 selections at one and two gravitons. It does not extend
any source domain. RULE-02 remains the separate G5 TII extension beyond three
gravitons. The earlier optional generic G4 n=1,b=5 timeout remains unverified;
no unrestricted parallel-generation, arbitrary-order runtime, general
Horndeski FeynArts or loop-amplitude claim follows from this delivery.

No commit, push, model rebuild, notebook rewrite or unrelated source change
is included. The source repair and earlier uncommitted work are preserved.

## Per-file inventory

Full SHA-256 values, comparison locations and before/after provenance are in
[the JSON manifest](HorndeskiLibraryMigration-2026-11-01.json). Short hashes here
are navigation aids only. “G5 earlier” refers to the separately retained audit.

| File | Classification | Before SHA prefix | Installed SHA prefix |
| --- | --- | --- | --- |
| `Libs/HorndeskiG2_0_2_1` | Unchanged | `654509d32676` | `654509d32676` |
| `Libs/HorndeskiG2_0_2_2` | Unchanged | `36ffc68462f7` | `36ffc68462f7` |
| `Libs/HorndeskiG2_0_3_1` | Unchanged | `c79454585a57` | `c79454585a57` |
| `Libs/HorndeskiG2_0_3_2` | Unchanged | `53d5f4a19c70` | `53d5f4a19c70` |
| `Libs/HorndeskiG2_1_1_1` | Unchanged | `b4c027aba86e` | `b4c027aba86e` |
| `Libs/HorndeskiG2_1_1_2` | Unchanged | `c3db40fbecb0` | `c3db40fbecb0` |
| `Libs/HorndeskiG2_1_2_1` | Unchanged | `c2462423fda7` | `c2462423fda7` |
| `Libs/HorndeskiG2_1_2_2` | Unchanged | `d5efddbd12b1` | `d5efddbd12b1` |
| `Libs/HorndeskiG2_2_1_1` | Unchanged | `f7274d64cddf` | `f7274d64cddf` |
| `Libs/HorndeskiG2_2_1_2` | Unchanged | `0b5a0b688558` | `0b5a0b688558` |
| `Libs/HorndeskiG2_2_2_1` | Unchanged | `958f73071f1c` | `958f73071f1c` |
| `Libs/HorndeskiG2_2_2_2` | Unchanged | `9103e4def23b` | `9103e4def23b` |
| `Libs/HorndeskiG2_3_1_1` | Unchanged | `11ba34532572` | `11ba34532572` |
| `Libs/HorndeskiG2_3_1_2` | Unchanged | `53f490492d84` | `53f490492d84` |
| `Libs/HorndeskiG2_4_1_1` | Unchanged | `fe6bdad50464` | `fe6bdad50464` |
| `Libs/HorndeskiG2_4_1_2` | Unchanged | `91ec571eae5c` | `91ec571eae5c` |
| `Libs/HorndeskiG3_0_1_1` | Unchanged | `53313021e185` | `53313021e185` |
| `Libs/HorndeskiG3_0_1_2` | Unchanged | `8f6eb6ace9ed` | `8f6eb6ace9ed` |
| `Libs/HorndeskiG3_0_2_1` | Unchanged | `95e0768aee22` | `95e0768aee22` |
| `Libs/HorndeskiG3_0_2_2` | Unchanged | `4a4c1f8cb5da` | `4a4c1f8cb5da` |
| `Libs/HorndeskiG3_1_1_1` | Unchanged | `15553b2d0bcc` | `15553b2d0bcc` |
| `Libs/HorndeskiG3_1_1_2` | Unchanged | `0f2f73d83f45` | `0f2f73d83f45` |
| `Libs/HorndeskiG3_1_2_1` | Unchanged | `875d0677ef37` | `875d0677ef37` |
| `Libs/HorndeskiG3_1_2_2` | Unchanged | `9d86d0f8f23d` | `9d86d0f8f23d` |
| `Libs/HorndeskiG3_2_0_1` | Unchanged | `3da9761d62b4` | `3da9761d62b4` |
| `Libs/HorndeskiG3_2_0_2` | Unchanged | `ddcc9ba3365b` | `ddcc9ba3365b` |
| `Libs/HorndeskiG3_2_1_1` | Unchanged | `de313870928a` | `de313870928a` |
| `Libs/HorndeskiG3_2_1_2` | Unchanged | `e7fa2ad7283f` | `e7fa2ad7283f` |
| `Libs/HorndeskiG3_3_0_1` | Unchanged | `a6146a71640c` | `a6146a71640c` |
| `Libs/HorndeskiG3_3_0_2` | Unchanged | `8f0d00651d2f` | `8f0d00651d2f` |
| `Libs/HorndeskiG3_3_1_1` | Unchanged | `6d134833ec70` | `6d134833ec70` |
| `Libs/HorndeskiG3_3_1_2` | Unchanged | `b01f7fd824a6` | `b01f7fd824a6` |
| `Libs/HorndeskiG3_4_0_1` | Unchanged | `b3394410f136` | `b3394410f136` |
| `Libs/HorndeskiG3_4_0_2` | Unchanged | `4b1215ab057e` | `4b1215ab057e` |
| `Libs/HorndeskiG3_5_0_1` | Unchanged | `a8916c6fbfe5` | `a8916c6fbfe5` |
| `Libs/HorndeskiG3_5_0_2` | Unchanged | `88155530e2b2` | `88155530e2b2` |
| `Libs/HorndeskiG4_0_1_1` | G4 corrected now | `f5ee22d134fe` | `e91eeb0724ee` |
| `Libs/HorndeskiG4_0_1_2` | G4 corrected now | `a2ac7b3ad3c6` | `1aa223e22da4` |
| `Libs/HorndeskiG4_0_2_1` | G4 corrected now | `1f8e7d08cf3d` | `c64ff6dc7bf8` |
| `Libs/HorndeskiG4_0_2_2` | G4 corrected now | `21e2f6ae24f4` | `9ff4849256d8` |
| `Libs/HorndeskiG4_1_1_1` | G4 corrected now | `55a8f588b370` | `372907a22a7f` |
| `Libs/HorndeskiG4_1_1_2` | G4 corrected now | `f914716f775f` | `897dbb00e1c2` |
| `Libs/HorndeskiG4_2_0_1` | G4 corrected now | `e666d9606a1b` | `30f5e621ba3e` |
| `Libs/HorndeskiG4_2_0_2` | G4 corrected now | `e50301331c7d` | `1467e88a8eb4` |
| `Libs/HorndeskiG4_2_1_1` | G4 corrected now | `c733d9c87df9` | `d441e64fd23f` |
| `Libs/HorndeskiG4_2_1_2` | G4 corrected now | `df092282c826` | `714fb9708d40` |
| `Libs/HorndeskiG4_3_0_1` | G4 corrected now | `e666d9606a1b` | `5813e96b27c6` |
| `Libs/HorndeskiG4_3_0_2` | G4 corrected now | `e50301331c7d` | `c0ae17cacd9c` |
| `Libs/HorndeskiG4_4_0_1` | G4 corrected now | `e666d9606a1b` | `73123648f8dd` |
| `Libs/HorndeskiG4_4_0_2` | G4 corrected now | `e50301331c7d` | `078c5e530e1f` |
| `Libs/HorndeskiG5_0_1_1` | G5 earlier | `90856ba9136a` | `eaeb02dc8d95` |
| `Libs/HorndeskiG5_0_1_2` | G5 earlier | `c3f34ccfdda6` | `bbe1077fd406` |
| `Libs/HorndeskiG5_1_0_1` | Unchanged | `52b9c51bf4a8` | `52b9c51bf4a8` |
| `Libs/HorndeskiG5_1_0_2` | Unchanged | `dfb23b014e49` | `dfb23b014e49` |
| `Libs/HorndeskiG5_1_1_1` | G5 earlier | `d184f4413841` | `6cb635465f86` |
| `Libs/HorndeskiG5_1_1_2` | G5 earlier | `03e2b5908951` | `539d50a64176` |
| `Libs/HorndeskiG5_2_0_1` | Unchanged | `e26eeab462b4` | `e26eeab462b4` |
| `Libs/HorndeskiG5_2_0_2` | Unchanged | `d23d04b8c014` | `d23d04b8c014` |
| `Libs/HorndeskiG5_3_0_1` | Unchanged | `484eac452636` | `484eac452636` |
| `Libs/HorndeskiG5_3_0_2` | Unchanged | `ebad622c92b6` | `ebad622c92b6` |
