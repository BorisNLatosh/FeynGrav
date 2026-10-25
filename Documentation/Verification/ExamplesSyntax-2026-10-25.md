# EXAMPLES-01: syntax and interface audit — 25 October 2026

**Accepted for the documented syntax/interface scope. Verification preceded the delivery commit.**
Base: `FORM_Simplification`, commit `988c88c533ea283ba3a06e0a9e3931e5ee5eb331`.
Wolfram Language 15.0.1 and FeynCalc 10.2.1 were used. This is not a fresh
validation of every physical result saved in the notebooks.

## Corrections

- **07:** four projector-table `Collect` patterns used `B0[_]`, which cannot
  match the three-argument bubble integral. They now use `B0[__]`. The integral
  expressions and Ward identities are unchanged.
- **09:** `mGravity` incorrectly equalled the entire amplitude, including the
  potential terms. It now specialises both potential couplings to zero. The
  native tree amplitude, exact contribution reconstruction and all six existing
  checks pass; the old assignment is rejected by a negative control.
- **11:** two `Contract[...] = ...` assignments silently added definitions to
  FeynCalc's contraction routine. The earlier scalar-product cells already
  impose the intended relations. The replacement tests return `{True, True}`
  and leave `DownValues[Contract]` unchanged. Restart a kernel in which the old
  cell was evaluated. No interaction formula or helicity convention changed.
- The examples index now accurately describes example 07's sectors and which
  loop steps use FORM decomposition versus FeynCalc `TID`.

Only six original input cells changed. Two explanatory text cells were added.
All other original cells, including all **123 saved Output cells** and all
Print cells, remain byte-identical. Trailing notebook outline caches with stale
byte offsets were removed only from the three edited notebooks. The eight
other notebooks are byte-identical to the baseline. Saved output is historical:
07's four tables, 09's contribution presentation/checks and 11's dependent
amplitude/check displays were not overwritten. Notes in 09/11 and the index
identify the need to reevaluate after the repair.

## Notebook ledger

All entries passed held parsing and interface review. “FE” applies only to
rendered copies of changed inputs; unchanged notebooks were not opened or run
in the front end. All full loop calculations were omitted.

| Example | Executable cells | Workflow/dependencies | Review and focused execution | FE |
| --- | ---: | --- | --- | --- |
| 01 | 27 | Projector algebra; FeynGrav/FeynCalc + FORM | Names, index arguments, explicit denominators and stated D=4 projector checks reviewed; unchanged. | Not rerun |
| 02 | 9 | Manual massive-scalar tree; FeynGrav/FeynCalc + FORM | All-incoming momentum convention, Mandelstam setup, amplitude/cross-section dependencies reviewed; unchanged. | Not rerun |
| 03 | 39 | Manual graviton tree; FeynGrav/FeynCalc + FORM | Exchange definition before on-shell setup, polarisation labels and named channels reviewed; unchanged. | Not rerun |
| 04 | 19 | Native GR tree; patched FeynArts + FORM | Code cell, loading order, matching model names, FCFAConvert options and diagram rendering reviewed; unchanged. | Not rerun |
| 05 | 9 | Gravity/ghost self-energy; FeynGrav/FeynCalc + FORM | Declared-loop tadpole and algebra-only bubble preprocessing before TID distinguished; unchanged. | Not rerun |
| 06 | 52 | Matter self-energies; FeynGrav/FeynCalc + FORM | SUNYM import, six sectors, declared-loop tadpoles, TID bubbles and reduced-coefficient calls reviewed; unchanged. | Not rerun |
| 07 | 48 | Matter contributions to graviton self-energy; FeynGrav/FeynCalc + FORM | Four B0 collection patterns repaired; symbolic-D loop declarations and tadpole-dependent Ward inputs preserved. | Changed inputs inspected |
| 08 | 36 | Native one-loop scalar–graviton vertex; patched FeynArts/FeynHelpers/Package-X + FORM | Potential set to zero; native five-diagram inventory, integrand mode before TID, local helpers and later PaX/IR dependencies reviewed; unchanged. | Not rerun |
| 09 | 10 | Native potential-scalar tree; patched FeynArts | Gravity-only extraction repaired; seven-diagram amplitude and six notebook checks executed. | Changed inputs inspected |
| 10 | 10 | Native John tree; patched FeynArts | Matching model name, beta decomposition, on-shell setup and conversion options reviewed; unchanged. | Not rerun |
| 11 | 15 | Native EsGB scalar annihilation; patched FeynArts | Two assignments to Contract replaced by two relation checks; outgoing conjugate labels, symbolic D and generic Ward setup preserved. | Changed inputs inspected |

## Verification

1. All 11 notebook expressions and **274 executable cells / 425 independent
   input expressions** parse under `HoldComplete`. No `ErrorBox`, syntax
   diagnostic or unintended evaluation was found. Both Input and Code styles
   are covered. Top-level list-valued `BoxData` is parsed expression by
   expression, retaining multiline input, nested boxes, comments and strings.
2. Exact held comparisons cover all 274 cells: 268 unchanged cells are identical;
   the six changes equal their declared replacements. Three replacement
   fragments were made with native `MakeBoxes[Defer[...]]` and round-tripped
   exactly. No custom code tokenizer was used.
3. A fresh package-loaded inventory checked **133 calls** against current
   option tables (`FeynGravSimplify`, `FCFAConvert`, `TID`, native FeynArts
   constructors, `SUNSimplify`, `DiracSimplify` and the SUNYM importer).
   There are no unknown option names. Remaining non-system/non-package heads
   are notebook-defined helpers. Function signatures, model folder names,
   definition order, dimensions, scalar-product setup and integrand/coefficient
   distinctions were reviewed from held inputs and installed sources.
4. **10/10 focused assertions** pass: six existing native scalar-tree checks
   grouped in one assertion, gravity-only extraction/reconstruction and a
   negative control; two EsGB setup relations, unchanged Contract definitions
   and a generic contraction; old/new B0 pattern matching and exact collection
   equality. No FORM process or full loop calculation was run.
5. The six changed inputs were rendered with a task-owned Mathematica front
   end and visually inspected as PDF/PNG copies. Greek symbols, equality tests,
   patterns and delimiters display correctly. Original notebooks were not
   opened or evaluated in that front end.

### Resource and harness qualifications

Every launch used an aggregate 1 GiB cap, zero swap, 120 seconds plus 10 seconds
termination grace, fresh host headroom of at least 2 GiB and task-owned process
control. The successful held parse took about 2 seconds; the package/interface
inventory 7.42 seconds; the focused checks 10.00 seconds with 288,649,216 bytes
peak cgroup memory. Accepted computational checks have empty diagnostic logs,
zero resource events and no remaining owned calculators.

Historical harness attempts are retained. The first parser joined independent
BoxData items and was rejected as an intended-structure test; a subsequent
report-writer error and an unheld inventory pattern were corrected before the
accepted runs. No notebook repair was based on those artefacts.

The two front-end preview runs produced the PDFs without diagnostics, but the
runner rejected immediate cleanup: Mathematica started a front-end-owned
kernel even with the preview evaluator disabled, and it was still shutting
down at the primary exit. Later exact-scope queries confirmed inactive, empty
scopes with successful termination. These runs are **visual evidence with a
lifecycle qualification**, not clean one-kernel verification passes. No user
kernel was used or stopped, and no further preview retry was made. The core
held/interface/focused checks above have clean lifecycle records.

## Identities and retained evidence

Full before/after SHA-256 hashes, input UUIDs, held expressions, exact comparison
results, preserved-output hashes, scripts, pins, diagnostics and resource records:

`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-25-examples01/`

- `preservation.json`: all notebook identities, changed UUIDs and saved outputs.
- `run-final-parse/`: authoritative per-cell syntax ledger and 274 comparisons.
- `run-interfaces-r2/`: current contexts/options and original Contract-assignment
  side-effect evidence.
- `run-focused/`: exact assertions, native scalar amplitude and diagnostics.
- `run-render-r2/`: inspected front-end previews and later scope status.

| Example | Before SHA-256 (prefix) | Delivered SHA-256 (prefix) |
| --- | --- | --- |
| 01 | `dc21be7a94ee05e6` | `dc21be7a94ee05e6` |
| 02 | `4db1c7f052de0ba4` | `4db1c7f052de0ba4` |
| 03 | `6d6d88a4d8360a01` | `6d6d88a4d8360a01` |
| 04 | `0994535d0660a4e5` | `0994535d0660a4e5` |
| 05 | `499a3d10425a7942` | `499a3d10425a7942` |
| 06 | `fe8c7e63b2d42420` | `fe8c7e63b2d42420` |
| 07 | `9fb172fc0b8417a6` | `75a649c58c2544d0` |
| 08 | `15cf51d610d9dedb` | `15cf51d610d9dedb` |
| 09 | `2a9d133520184da7` | `d563279363384a0f` |
| 10 | `d5076f0af1fffb98` | `d5076f0af1fffb98` |
| 11 | `004facbd17f77c7d` | `919d1c4ae1622f8c` |

## Remaining limits

No general loop-physics, renormalisation, all-model Ward or performance claim
follows from this audit. The loop examples' historical outputs were preserved;
example 08's downstream reduction/UV/IR results still require recomputation
with its previously corrected bubble weight. Examples 01–08 were not rerun
end to end. The John/EsGB amplitudes and the full GR tree were not recalculated;
only example 09's small native tree and the stated setup/algebra checks were
executed. Future user edits require a new syntax check of the changed snapshot.
