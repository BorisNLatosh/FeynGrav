# Example 08: native FeynArts construction

Checked on 23 October 2026 on `FORM_Simplification`, based on `fa4fe35` with existing uncommitted changes. No commit or release is implied.

## Change and mathematical scope

`Examples/08. Graviton_Scalar_Vertex_One_Loop.nb` now uses the standalone `Single-Scalar-Tensor-Minimal-Coupling` bundle. The potential field points are excluded, and `ScalarLambda3 -> 0`, `ScalarLambda4 -> 0`, `ScalarMass -> m` are applied after conversion. The parameter symbols remain unassigned. Native FeynArts generates one-loop 1PI topologies for two incoming scalars and one outgoing graviton. The incoming graviton momentum in the rule convention is therefore `-p1-p2`.

`PreFactor -> 1`, `Prefactor -> 1` and `Truncated -> True` preserve the library factors of `i` and the native diagram weights, without adding the default FeynArts measure or external wavefunctions. The tensors retain symbolic `D` and two free graviton indices. No on-shell substitutions are imposed during construction or verification.

| Group | Internal lines | Vertices | Weight |
| --- | --- | --- | --- |
| V1 | One scalar and two gravitons | Two TSS and one TTT | 1 |
| V2 | Two scalars and one graviton | Three TSS | 1 |
| V3 | Two gravitons | TTT and TTSS | **1/2** |
| V4 | One scalar and one graviton, with either scalar attachment | TSS and TTSS | 1 for each of two diagrams |

The former manual V3 expression had weight 1. Interchanging its identical parallel internal graviton lines is an automorphism with fixed labelled external legs, giving the factor 1/2. The triangles have no such nontrivial automorphism. The mixed bubbles have distinguishable internal species, so each has weight 1. Both mixed bubbles are generated natively and must not be doubled by another scalar-leg exchange.

The notebook identifies the four groups from their complete denominator mass inventories, rather than native diagram numbers. Verification additionally checks their species-labelled vertices, signed momentum flow and full tensor integrands; the mass-count guard alone is not a general completeness proof.

### Omitted contact

At one loop with three external legs, the possible connected 1PI arrangements are three cubic vertices, a cubic and a quartic vertex, or one quintic vertex. With the potential couplings zero, the missing quintic interaction is `hhhφφ`, of order κ³. Closing two graviton legs gives a massless tadpole. Its denominator depends only on the loop momentum; external momenta and scalar mass enter only the polynomial numerator. Each resulting loop integral is scaleless and vanishes in dimensional regularisation. This argument does not assign separate UV and IR values and does not supply the omitted vertex itself.

The supplied model is sufficient for the non-vanishing minimal correction under this prescription; it does not contain the complete κ³ vertex inventory. Reducible external-leg corrections, counterterms and external-state renormalisation are outside this example's 1PI construction.

`reduceVertex` also removes denominator-free polynomials before calling `TID`, since `TID` leaves loop-independent constants unchanged. This helper is local to the present integrands, whose propagator denominators all depend on `l`; it is not a general replacement for declaring an integration variable in arbitrary expressions.

## Exact checks

The final fresh-kernel batch passed **23/23 assertions**:

- Actual edited notebook syntax, dependency loading and seven model/construction/group/helper input cells.
- Five native amplitudes, five actual `DiagramGraphics` objects, and the exact propagator-species inventory.
- Potential exclusion and replacement, unassigned model parameters and absence of unconverted coupling/wavefunction objects.
- Signed momentum conservation at all 12 vertices; exact species-labelled vertex inventories.
- Independent fixed-external-leg automorphism weights `{1,1,1,1,1/2}` in native order.
- **Five exact zero residuals** between native integrands and source-rule products with the expected weights, using serial FORM and symbolic `D`.
- The four groups reconstruct the five native diagrams exactly; V3 is the single graviton bubble and V4 contains both mixed bubbles once.
- Denominator-free polynomial integrals vanish; a massive scalar tadpole is retained as `I Pi^2 A0[m^2]`.
- A missing-TTSS negative case removes all three bubbles, leaving two triangles.
- No unexpected Wolfram diagnostics and no assignment to `D`.

The native order is V2, V1, the first mixed bubble, the second mixed bubble, V3. For the two mixed bubbles the source comparisons use changes of integration variable `l -> p1-l` and `l -> p2-l`; all other comparisons use the same loop variable. This uses translation invariance of dimensional regularisation. Denominator momenta were normalised before comparison, including invariance under reversal. No numerical sampling or on-shell relation was used.

The source-rule comparison tests model translation, routing and weights; it is not an independent derivation of the interaction rules.

## Resources and provenance

Final kernel: **7.661 seconds**, highest retained aggregate cgroup memory **365,400,064 bytes** (about 349 MiB), exit 0. Whole-process-tree limit 1 GiB, zero swap, 120 seconds plus 10 seconds grace; fresh host headroom exceeded 2 GiB. No sampled memory events, swap use or surviving task-owned calculators. Existing user kernels were untouched.

Candidate notebook SHA256:
`7a1a82c4e2dfa93d496d5a2f2c8191be85dd002b54b8c1e2375208ba71c6d46a`.

The scalar model files match their manifest:

- Generic: `be3581b4be2b098119a3ed7fa3fd9ebf295aecc7bdf4d7b4f2129e12f4552aec`.
- Classes: `0bd46f30cda5438b2fb42b1053f113518691371a8bf33868beb475851c6ac894`.

Local retained evidence:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-example08-native/`.
The final run is `run-final-native-clean/`; it retains the candidate, exact scripts, assertion ledger, five residuals, routing/weight records, diagnostics, source hashes and resource/termination records. `preservation.json` and `notebook.patch` record the scoped edit. All 77 unaffected leaf cells were preserved byte-for-byte; this source snapshot contained no saved Output or Print cells.

### Historical attempts

All attempts remain retained. The initial native inventory passed 4/4 and the canonical source comparison passed 8/8. Earlier unsuccessful verification attempts exposed test-harness defects: unmatched equivalent denominator representations; an incorrect assumption that FeynHelpers uses its own public package context; unsorted expected vertex lists and creation of an unintended `FeynArts` context symbol; and a loop counter named `i` colliding with a dummy Lorentz index, causing a `MixedDimensions` failure. These are not counted as passes. The final script corrects them without modifying production rules or weakening the tensor comparisons. The historical shadowing warning is absent from the final run.

FeynArts' shape-progress text prints “0 diagrams” in this installation even though the returned graphics contain five diagrams. The verification counts the actual graphics objects instead of relying on that progress text.

## Limits

The later off-shell tensor reduction, Package-X UV/IR extraction, symmetry/longitudinal checks and infrared/Fourier analysis were preserved but **not rerun** in this migration. Recalculate them with the corrected V3 weight before relying on their values. No general loop validation, complete Ward identity, renormalised observable or infrared asymptotic completeness is established. Kernel-side graph construction and rendering were exercised; a Mathematica front-end visual inspection was not performed. Interaction formulae, stored libraries, model bundles and other notebooks were unchanged by this task.

## Interface references

- [FeynArts User's Guide](https://feynarts.de/FA3Guide.pdf), §§3.5, 4.2, 5 and 6.2: topology restrictions, insertion, drawing and amplitude construction.
- [FeynCalc FCFAConvert documentation](https://feyncalc.github.io/FeynCalcBook/FCFAConvert.html): conversion of native amplitudes.

## Notebook syntax correction — 23 October 2026

The initial edit used a hand-written box tokenizer that incorrectly treated adjacent function-closing brackets as a Part delimiter. The selected native-construction checks did not cover the four modified FORM/TID cells, so the earlier verification did **not** establish syntax validity of every input cell. The subsequent front-end save exposed malformed bracket nesting.

The repair starts from the user's latest saved notebook and replaces only 13 generated input-cell box expressions using Mathematica's native formatter under `Defer`. The existing table cell, other input/text cells, cell metadata and all newly saved outputs remain unchanged. No interaction rules or calculations were changed.

The final bounded syntax-only check parsed **all 36 input cells** under `HoldComplete`, with **13/13 exact held-expression round trips** against the intended current cell text. The final diagnostic log is empty; no FeynGrav load, FORM call, integral reduction or notebook computation was evaluated. It used one fresh kernel, 1.013 seconds and 127,746,048 bytes peak aggregate memory under the existing 1 GiB/no-swap/120-second policy, with no resource events or surviving owned calculators. This is a kernel box-parser check, not a visual front-end inspection or a rerun of the UV/IR results.

Earlier syntax-check harness attempts are retained: they exposed incomplete multiline string parsing, incorrect handling of list-valued BoxData, an invalid diagnostic serializer and a formatting round-trip change to the Grid cell. Those attempts are not counted as passes; the original Grid cell was preserved.

Corrected notebook SHA256: `7174fa509d6ba8f0533ccb456aab93ab354d79a11c6cb520b6256f761ae73a16`.
Local evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-example08-syntax/`, final run `run-delivery-syntax/`.
