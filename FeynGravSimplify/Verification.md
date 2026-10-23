# Contractor adapter verification — 10 October 2026

Implementation branch: `Contraction_Integration_and_Benchmarking`, based on
FeynGrav `dfb9490b56ad2dd3663ea05be7b230131ea934e3`.
Environment: Wolfram 15.0.1, FeynCalc 10.2.1, Linux x86-64.

## Pinned backend

The integration suite used a fresh build from the clean Contractor checkout:

- Source: `641febdaeb6b30835a5f7f846b9a26198ac20313` (`scalar-handling`, 0.1.0).
- Build: `GOEXPERIMENT=jsonv2 go build -o /tmp/fgsimplify-dev/contractor ./cmd/contractor` (with a temporary Go build cache).
- Compiler: Go 1.26.8, jsonv2 enabled; `go version -m` reports the exact source revision and `vcs.modified=false`.
- Binary SHA-256: `9d02cac519cbc3d818965bf42499dfa6c18539ce63f88b2bcaea2d92ab84ad0b`.

The build was kept outside the repository. Contractor source and the existing
installed executable were not changed.

## Installed executable at initial verification, before rebuilding

At initial verification, automatic discovery selected
`/home/latosh/.Wolfram/Applications/contractor/bin/contractor` on this machine.
That initial binary's embedded Go metadata identified source revision
`22bbc5161cdb0d63cf3f85657d6c17e9155cffc0`, built with Go 1.26.3,
`vcs.modified=false`. Its SHA-256 was
`558c62e143af82d812a2a391f98964057a7800e9f0f9d2a731b8fd0afe48ed32`.

A single metric/vector contraction passed through automatic discovery. This
is not full compatibility verification of that older build. Use an explicit
`ContractorExecutable` pointing to the pinned build for subsequent comparisons.
The temporary pinned build path may disappear after temporary-directory cleanup.

## Acceptance checks

- Adapter suite: **42 checks passed**. Covers exact numeric coefficients,
  notation conversion, symbol/context preservation, dimensions, composite momenta,
  denominators, rejected tensor heads/powers, malformed or unexpected JSON and
  absence of explicit Mathematica simplification calls.
- Integration suite: **69 checks passed** against the pinned binary and the
  test-only process fixture. Includes scalar potential and kinetic FeynGrav
  vertices checked independently with FeynCalc; a >120 KB request/response;
  2 MB stderr; method/option handling; launch/nonzero-exit/output failures;
  successful-file cleanup and retention; timeout after input consumption; abort
  propagation; checks for leftover streams/running processes; default eight-worker
  forwarding, explicit Automatic, quiet defaults, progress/timing output, invalid
  reporting options and the notebook polarization regression.
- Loading/installer suite: **15 checks passed**. Repeated loading launches no
  process or discovery, preserves FeynCalc definitions/options and the current
  directory, exposes command help, preserves CalcFormCalculate definitions/options,
  and detects a missing adapter in an installation archive.
- Independent FORM smoke test: `CalcFormCalculate` still returns the expected
  result for a metric/vector contraction. No CalcFormConverter files were changed.

These are correctness and integration checks, not a performance benchmark or a
validation of every FeynGrav interaction. Test-side algebraic normalization is
used only to compare mathematically equivalent outputs; it is not part of the
production command.

## Accepted native I/O limitation

A native Mathematica write to a non-reading subprocess can defer timeout and
abort delivery. This was observed in bounded preliminary probes. Per the user's
choice, the implementation uses native Mathematica process I/O without a Python
transport or watchdog. The passing timeout/abort tests concern execution after
stdin has been consumed and do not establish a hard bound during blocked writes.

## Reporting and notebook diagnosis

The updated adapter passed all 126 automated checks (42 representation,
69 integration, 15 loading). A separate contraction with the pinned Contractor
binary passed with the default eight workers and both reporting options enabled.
The Contractor source creates `poolSize` contraction goroutines and accepts
`-threads`; this is a parallel worker count, not CPU affinity or a total CPU cap.

The saved `Out[8]` in `One-Loop-Counterterms.nb` reports `UnsupportedMomentum`.
Its `In[8]` includes two `PolarizationTensorD` factors. A fresh-kernel minimal
reproduction, `FeynGravSimplify[PolarizationTensorD[p,mu,nu]]`, gives the same
failure: the tensor contains `Momentum[Polarization[p,I],D]`, whose source
`Polarization[p,I]` is outside the adapter's accepted momentum grammar.
This occurs during encoding before executable discovery or process launch.
The failure now includes `Stage -> "Encode"` and the held offending source.
Polarization support has not been added; the notebook was inspected without
modification, and the complete one-loop contraction was not executed.

## Periodic elapsed-time progress update

`ShowProgress -> True` now prints elapsed Contractor execution time approximately
every ten seconds while waiting for the child process. This is independent of
`ShowTiming` and starts its clock after launch. Blocking native I/O can delay
updates, as documented in the adapter guide.

After this change, 73 integration checks and 15 loading checks passed. The
integration suite used the rebuilt installed binary at revision `641febda`
(SHA-256 `9d02cac519cbc3d818965bf42499dfa6c18539ce63f88b2bcaea2d92ab84ad0b`).
A deliberately slow test child exercised the real ten-second interval. Additional
checks covered disabled reporting and periodic output followed by timeout cleanup.
The large one-loop calculation was not rerun for this reporting change.
