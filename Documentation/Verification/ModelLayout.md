# Uniform FeynArts model folders — 18 October 2026

## Change

Each supplied model now uses `FeynArts/Models/<Model>/` containing `<Model>.gen`, `<Model>.mod`, `manifest.json` and `README.md`. The parent README is the shared index. PureGR's three data files moved unchanged into `PureGR/`; GRScalar's data files stayed in place. There are no duplicate flat model files or forwarding stubs.

Only FeynGrav's path-registration block changed: when FeynArts is already loaded, it discovers immediate folders with matching `.gen` and `.mod` basenames and adds missing directories to `$ModelPath`. Existing paths remain; repeated registration does not duplicate a directory. It does not load FeynArts or initialise a model. Folder discovery does not evaluate model files or manifests. Builders still write to their caller-selected destination; no public builder signature changed.

The source SHA-256 after the registration edit is `d1b819fae0c866359004c196b517b19fd8a62c884975603b18eb3ae9273014d6`. All six model/manifest hashes remain those of the accepted [PureGR](PureGRTreeRelease.md) and [GRScalar](GRScalarTreeRelease.md) snapshots. Their historical source hashes were retained. Formulae, stored libraries, saved notebooks, Contractor and loop pins were unchanged. Old paths in saved notebook output remain historical; current notebook input selecting PureGR by name uses the new registration.

## Focused verification

One bounded kernel passed nine checks: package/dependency loading, equal four-file folder inventories, automatic registration, idempotence, the FeynArts-absent guard, incomplete-folder filtering with a spaced path, preservation of existing search paths, bridge acceptance of relocated PureGR, native PureGR initialisation and cubic conversion (some assertions combine these observations).

That run then failed its tenth assertion when switching from PureGR after conversion to GRScalar in the same kernel: FeynArts emitted `RuleDelayed::rhs` warnings during classes initialisation. The warning-bearing run remains a failed run, not a complete pass. No model was changed and no warning was suppressed.

A separate fresh-kernel GRScalar check passed all five assertions without warnings: automatic registration, initialisation with five couplings, symbolic mass and statistics, native scalar cubic conversion and unchanged working directory. **Start a fresh kernel when switching models.** The checks support this documented workflow; same-kernel switching after conversion is not verified. The underlying warning mechanism was not investigated as part of this directory change.

Both runs used an enforced whole-tree 1 GiB RAM limit, zero swap, 120 seconds plus 10-second termination grace and fresh host headroom of at least 2 GiB. No memory-limit/OOM event or surviving owned process occurred. The first kernel ran 6.104 s with highest retained cgroup peak 321,531,904 bytes; the fresh scalar check ran 4.664 s with 280,735,744 bytes. These are single observations, not benchmarks or kernel-RSS measurements. User kernels were untouched.

Static verification matched all moved/retained model hashes and manifests, checked links and the source diff, and confirmed unchanged notebook and interaction-library bytes. No model rebuild, full algebra rerun, loop check, Full benchmark or commit was performed. Retained scripts/logs/resources are indexed in the private model-layout verification note.
