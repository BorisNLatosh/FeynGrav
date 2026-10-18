# Pure GR tree model release — 18 October 2026

## Delivered scope

The shipped `PureGR` generic/classes files and manifest are the byte-identical candidate used by the accepted native ghost and tree checks. The only generic-file change from the old shipped bundle is the internal ghost propagator from `+I` to `-I`; the classes file, graviton rules and cubic ghost rule are unchanged. The manifest records the matching generic hash and snapshot.

This release supports native three-/four-graviton tree construction with symbolic D, gauge epsilon 2 and two external indices per graviton. Source translation is verified; general physical on-shell/helicity/Ward validation remains separate.

## Retained checks

| Check | Outcome and limits |
| --- | --- |
| Temporary model build | 20 model checks and 18 byte-comparator fixtures passed; 11.021 s; retained cgroup peak 323,973,120 bytes. |
| Corrected native ghost check | 28 requirements and six arrow fixtures passed; insertion/free ratio -Hκ and incorrect-sign controls checked. Seven separate process-guard checks passed. Kernel 4.703 s; retained cgroup peak 283,844,608 bytes. |
| Native trees | 32 checks passed; 15 exact zero rational residuals. Cubic/quartic contacts, whole-leg permutations, renamed labels, composite momenta, contact plus s/t/u graph inventory and all exchange factors/routings. |
| Completeness failure | Real temporary quartic-coupling exclusion returned `IncompleteInsertion`: four requested topologies, three insertions, missing contact. No amplitude constructed; native settings restored. |
| Tree resources | Final kernel 26.478 s; highest retained cgroup peak 305,307,648 bytes. 1 GiB aggregate cap, zero swap, 120 s plus 10 s grace, fresh host headroom at least 2 GiB. No final-run warnings or sampled memory events; owned processes terminated. |
| Release verification | Installed bytes and manifest checked against the accepted candidate and its source/library identities. No new kernel, model regeneration, FORM run or loop calculation. |

The initial tree batch stopped at `mom::shdw` after 27 passes and before five checks. The sole correction loaded the optional companion before model initialisation and passed all 32 checks without suppressing warnings. Both attempts remain preserved. Retained cgroup peaks are neither final peaks nor process RSS. Tests used Wolfram 15.0.1, FeynCalc 10.2.1 and FeynCalc-patched FeynArts 3.12; these observations do not establish minimum supported versions or portable timings.

## Deferred loop compatibility

The user explicitly deferred the corrected model's loop checks. `Loop.wl` and its old pins are unchanged. The corrected manifest snapshot and generic hash differ from those pins; the bridge returns `GhostCompletenessGate` before constructing loops. Native FeynArts does not apply that gate. Historical positive-ghost snapshot loop checks do not validate this release. No loop integration, full-product loop expansion or fingerprint bypass was performed.

## Evidence identity

The detailed scripts, raw logs, resource records and independent reviews remain in the maintainer's external evidence archive; they are not runtime dependencies or claimed to ship with the package. Records: `2026-10-15-gr-02-b1-helper-correction-34_zbzrg/run-B1`, `2026-10-16-gr-b2-owned-guard-cv844hfn`, and `2026-10-18-gr-trees-order-correction-utyywb1v`. Before publication the supervisor independently matched 112 tree-package hashes, 301 protected inputs and 586 historical files. The release record is `2026-10-18-gr-tree-release`.

The tested checkout was `1685e07619c231d75cb05f4ea867b135abb1aa5e` with recorded uncommitted changes; HEAD alone is not its identity. Source `FeynGrav.wl` SHA-256: `a3b24436ef92f078f6c05cdd0bf8c4ec2cacb3d9274b20ea0245f69fffed6ceb`. Model hashes are listed in the [model guide](../../FeynArts/Models/PureGR/README.md) and manifest. The [native recipe](../../FeynArts/Models/PureGR/README.md#construct-and-convert-a-four-graviton-tree) explains the supported workflow.

## Subsequent directory layout

The later [model-folder standardisation](ModelLayout.md) moved PureGR into its named subdirectory and updated only FeynGrav's path registration. All model and manifest bytes remain identical to this report's tested snapshot. Its source hashes remain historical identities; see the layout record for the changed loader and focused loading checks.
