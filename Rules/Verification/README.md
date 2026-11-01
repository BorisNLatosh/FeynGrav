# Interaction-rule verification

## Horndeski regression checks

`HorndeskiChecks.wls` runs 48 focused assertions against the independent labelled
action in `HorndeskiReference.wl`. It covers both direct variants, equal scalar
momenta, temporary-index collisions, one-/two-graviton G2/G3/G5 action checks,
negative controls and the G5 inverse-metric bound. The larger inventory and three-graviton
checks are recorded in the [audit report](../../Documentation/Verification/HorndeskiAudit-2026-11-01.md).

Use a fresh kernel with FeynCalc/FeynGrav and FORM available. Set
`FEYNGRAV_RULE_AUDIT_OUTPUT` to an existing external evidence directory.
`FEYNGRAV_RULE_AUDIT_ROOT` optionally selects a candidate package root;
`FEYNGRAV_RULE_AUDIT_FORM` optionally selects the FORM executable. Defaults use
this checkout's root and normal executable discovery with one worker.

Run `WolframKernel -script Rules/Verification/HorndeskiChecks.wls` **inside a
verified external resource wrapper**. The script is not a launcher and does
not establish memory or time limits. The accepted run used whole-tree 1 GiB,
zero swap, 120+10 seconds and at least 2 GiB fresh host headroom. It writes
assertions, diagnostics and retained FORM jobs only to the supplied directory;
it neither generates libraries nor edits notebooks. An unexpected diagnostic or
failed assertion stops the check.

## G4 source regression

`HorndeskiG4Checks.wls` performs 95 assertions using the independent common
reference and `HorndeskiG4Reference.wl`. It checks actual public contracted and
uncontracted source calls through n=2 and the complete n=3,b=2 vertex by exact
source-seed contraction followed by every labelled permutation. It includes
symmetries, degrees and sensitive wrong-sign/omitted-block controls. The
[G4 report](../../Documentation/Verification/HorndeskiG4-2026-11-01.md) records
additional interface, higher-b and all-library classification checks and limits.

Use the same three `FEYNGRAV_RULE_AUDIT_*` variables above, a fresh external
output directory and a separate kernel. Run
`WolframKernel -script Rules/Verification/HorndeskiG4Checks.wls` inside the
external wrapper. This larger suite was verified with whole-tree 4 GiB,
zero swap, 600+10 seconds and fresh 2 GiB host headroom. It makes at most two
sequential serial FORM simplification calls, each with a 120-second internal
limit; the external wrapper enforces the aggregate limit. It does not load
the main package, regenerate libraries or validate the four-argument importer.
Old output files cause an early refusal rather than overwriting evidence.
The separate [RULE-05 migration](../../Documentation/Verification/HorndeskiLibraryMigration-2026-11-01.md) now verifies all 14 shipped G4 libraries and ordinary imports.

## G4 stored-library and import regression

`HorndeskiG4LibraryChecks.wls` runs 109 assertions against all 14 shipped G4
selections and the independent references beside it. It checks non-unit and
zero couplings, equal momenta, pair symmetries, provenance, empty/corrupt import
recovery, cancellation, retry, and the G4/G5/Einstein John identities at n=1,2.
The latter impose total incoming momentum conservation only; no on-shell or
four-dimensional conditions are used.

Use a fresh kernel with FeynCalc and the local serial FORM executable
`/usr/local/bin/form`. Set `HORNAUDIT_RUN_DIR` to a fresh external output
directory. `FG_HORNDESKI_REPOSITORY` optionally selects a checkout (default:
this script's repository); `FG_HORNDESKI_LIBRARY_ROOT` optionally selects a
candidate directory containing `Libs/` (default: that checkout). The script
loads the main package and deliberately does not load the source G4 package.

Run `WolframKernel -script Rules/Verification/HorndeskiG4LibraryChecks.wls`
inside the external wrapper. Candidate and installed runs both passed under
aggregate 1 GiB RAM, zero swap, 120+10 seconds and fresh 2 GiB headroom. Each
uses one serial FORM call with a 60-second inner limit. Failure fixtures are
copies inside the evidence directory; selected libraries are never overwritten.
The script itself does not enforce the external memory/time limits.

## G5 higher-order source regression

`HorndeskiG5HigherOrderChecks.wls` runs 33 assertions. It checks dispatcher
assembly through n=6 using marked kernels, early inverse-metric bounds in both
public variants, b=0 isolation, failure/abort/cache/retry behaviour, and independent
determinant/inverse-metric identities. Actual source T8 is compared with an
independently differentiated connection sector at n=4,b=2 and n=5,b=1; both have
sensitive wrong-factor controls. Those sector checks are not full vertices.

Use the same `FEYNGRAV_RULE_AUDIT_OUTPUT`, optional `_ROOT` and `_FORM` settings
as the first suite, a fresh kernel, and a separate externally bounded launch.
Existing scientific output files are refused. The installed run used whole-tree
1 GiB, zero swap, 120+10 seconds and fresh 2 GiB headroom, with two sequential
serial FORM calls. The script does not enforce those external limits itself.

The [RULE-02 report](../../Documentation/Verification/HorndeskiG5HigherOrders-2026-11-01.md)
retains the full n=4,a=0,b=1 comparison, all labelled permutations and its
source/script/output identities. To remain within the cap, that comparison was
split across fresh reference, source-seed and sector-comparison processes.
The independent reference now constructs the volume at general label count;
its previous n<=3 formula must not be extrapolated to higher orders.

## Five-graviton Einstein-Hilbert checks

`GravitonQuinticChecks.wls` compares stored graviton libraries with the independent
labelled Einstein–Hilbert action in `GravitonQuinticReference.wl`. Select one suite
with `FEYNGRAV_GR_CHECK_SUITE`: `Lower` (11 assertions), `Quintic` (23, default),
or `Trace` (10). Run each in a separate fresh kernel and output directory.
The [derivation and evidence](../../Documentation/Verification/GravitonQuintic-2026-11-01.md)
explain why unrestricted auxiliary vectors plus stored index-pair symmetry test
the complete tensor, and why the single/double traces retain symbolic D.

Set `FEYNGRAV_RULE_AUDIT_OUTPUT` to an existing external directory with no prior
scientific outputs; existing outputs are refused. `FEYNGRAV_RULE_AUDIT_ROOT`
optionally selects a checkout (default: this script's repository). FeynCalc is
needed; FeynGrav production tensor packages and FORM are not loaded. Only the
Lower suite loads the test-only `HorndeskiReference.wl` for a separate direct-Ricci
boundary-term control. The quintic reference is independent of that file.

Run `WolframKernel -noinit -script Rules/Verification/GravitonQuinticChecks.wls`
**inside a verified external resource wrapper**. The script does not establish
memory/time bounds. All three installed suites passed with aggregate 1 GiB RAM,
zero swap, 120+10 seconds, fresh 2 GiB available host memory and clean termination.
It writes diagnostics, ordered assertion ledgers and exact WXF results to the
output directory, and stops on an unexpected message, failure or abort. It does
not edit libraries, models or notebooks. No five-point amplitude or higher-order
FeynArts build is part of these tests.
