# FeynArts scattering notebook: on-shell s-channel

Executed 12 October 2026 with Wolfram 15.0.1, FeynCalc 10.2.1, patched FeynArts 3.12 and the existing CalcFormConverter/FORM installation. No integration convenience wrapper was loaded. Only the s-channel contribution was calculated; this is not the complete gauge-invariant amplitude.

## Result

The 12 input cells of `candidate.nb` were evaluated in order in a fresh kernel, substituting the actual Examples directory for the front-end-only `NotebookDirectory[]`. Nine checks passed, shell exit 0, including exact agreement with the original notebook's saved s-channel result:

```wl
AmplitudeSChannel = 9 I FeynGrav`\[Kappa]^2 t u (1 + h1 h2) (1 + h3 h4)/(16 s)
sChannelReferenceResidual = 0
sChannelMatchesReference = True
```

The topology is selected by the two incoming external lines meeting at the same vertex, not by its position in a list. The converted all-incoming amplitude is amputated and has one massless pole, which reduces to `1/s`. The kinematic polarisation tables were compared to the original notebook's actual input cells: all 32 products agree exactly. The saved reference is used only after the independent FeynArts/FORM calculation.

`CalcFormCalculate` ran with one worker, symbolic D and a 60-second per-stage limit. The complete contraction/simplification cell took 0.787 seconds. The verified 1-GiB aggregate cgroup (including FORM), zero swap and group-OOM policy recorded peak memory 281,530,368 bytes and zero cap/OOM events. Host headroom before starting was approximately 8.3 GiB. These headless tests verify evaluation, not graphical front-end rendering.

## Files

- `initialisation-only.nb`: preserved notebook before this addition.
- `build-notebook.wls`: constructs the candidate from that initialisation snapshot without running physics. Its input path was changed from the original notebook to the preserved snapshot after the successful run, to keep rebuilding possible.
- `candidate.nb`: unevaluated candidate used by the test.
- `check-notebook.wls/.log`: actual notebook-cell evaluation, exact reference comparison and kinematic-data cross-checks.
- `validated.nb`: the candidate with four genuine evaluated output cells (routing, pole, amplitude, reference check), copied byte-for-byte to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`.
- Installed notebook SHA-256: `3e639647fcb7574f362ba47342c10d59c83871130de21aa7060575e5aa740af9`.

FORM files from this run are retained at `/tmp/fgfa-s-channel-f921e1d8-4c14-4603-be04-eb2620041d05/calcform-job-952ea876-ab3e-49e7-bc68-8a12074518bf`. This is a temporary run path, not a notebook dependency; re-evaluation creates its own directory.

Reproduction from `Benchmark/` (check host headroom first):

```bash
unit="fgfa-s-channel-$(date +%s)-$$"
systemd-run --user --scope --same-dir --unit="$unit" \
  -p MemoryAccounting=yes -p MemoryMax=1G -p MemorySwapMax=0 \
  -p OOMPolicy=kill -p KillMode=control-group -p TasksMax=48 \
  -p RuntimeMaxSec=210s -p TimeoutStopSec=7s \
  /usr/bin/timeout --kill-after=5s 190s \
  /usr/local/bin/WolframKernel -noinit -script \
  FeynArtsEvidence/notebook-s-channel-2026-10-12/check-notebook.wls
```

The original scattering notebook, model files and package source were not changed. No commit or merge was made.
