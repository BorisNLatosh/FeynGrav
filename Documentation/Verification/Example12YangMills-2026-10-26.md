# Example 12: Yang–Mills scattering with graviton emission

Verified 26 October 2026 on `FeynArts_Integration`, based on `0564ed0`, with the
[FA-YM-01 model delivery](GRSUNYM-2026-10-26.md). The maintainer accepted the
example and authorised committing it with the resolved model task on 26 October.
This is development work, not a publication or release.

## Delivered workflow

[Example 12](../../Examples/12.%20Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb)
initially contained 13 executable cells and five saved outputs for

\[
 A^a(p_1)+A^b(p_2)\longrightarrow A^c(p_3)+h(p_4),\qquad
 \mathcal M=g_s\kappa f^{abc}\mathcal K.
\]

The notebook loads FeynGrav and the matching native model, constructs and draws
four diagrams, fixes the massless Mandelstam products, constructs the tensor
amplitude, attaches physical states and displays the colour-stripped multiplier.
Incoming momenta are p1/p2 and outgoing momenta p3/p4. Outgoing states are
conjugated. D and N remain symbolic. The factorised graviton state is transverse
and traceless; it is a convenient family, not a complete D-dimensional basis.
The initial delivery did not specialise helicities. The extension below adds that
specialisation; colour averages, cross sections, quarks and loops remain outside scope.
FORM is not used.

The contact and s/t/u gauge exchanges each have coupling order gs κ. Native
FeynArts graph weights and source normalisation are retained with `PreFactor -> 1`;
division by I defines the displayed amplitude. Internal colour sums are checked
monomial by monomial before repeated-index contraction. Only external-state sum
markers are removed directly.

## Executed checks

The format-only final batch passed 3/3 assertions, including exact held-expression
round trips for every input cell. The final evaluation executes those notebook
boxes sequentially in a fresh kernel and passes 19/19 assertions:

- All 13 inputs evaluate; the four native diagrams have the species-labelled
  channels `contact`, `V:12|34`, `V:13|24`, `V:14|23`.
- D and N remain unassigned, conversion leaves no unresolved sum/conversion
  objects, and contraction of internal indices leaves precisely five external
  Lorentz indices.
- The coupling powers, nonzero physical amplitude, colour-factor reconstruction,
  and zero-gravity/zero-Yang–Mills-coupling limits agree.
- Each of the three gauge-field Ward residuals is exactly zero.
- The graviton contraction with $p_4^\alpha r^\beta+p_4^\beta r^\alpha$ is exactly
  zero for arbitrary r, without imposing $r\cdot p_4=0$.
- Removing the hAAA contact yields a nonzero first gauge-field Ward residual.
- Outgoing conjugation, one rendered diagram sheet, and preservation of all
  original input cells in the evaluated notebook are checked.
- After matching labels and conventions, the amplitude equals the previously
  verified native VVVT tree exactly, with a literal zero residual. This is a
  workflow regression comparison; the underlying model's independent action
  checks are recorded in its separate report.
- A diagram export is generated; see the visual-review qualification below.

The five saved outputs are the four-diagram sheet, kinematic amplitude, coupling
limits/reconstruction, four Ward results, and missing-contact detection. The
colour-stripped amplitude has 1,144 leaves in the recorded representation.

## Resources, retained corrections and limits

The accepted evaluation kernel took 8.456 s; peak aggregate cgroup memory was
289,275,904 bytes (about 276 MiB). Whole-tree limits were 1 GiB RAM, zero swap,
120 s plus at most 10 s termination grace, with fresh available memory above
2 GiB. The final diagnostic log is empty, memory events are zero, exit is zero,
and no task-owned calculators survive. User kernels were not used.

Earlier format-only attempts exposed multiline box parsing and held-expression
round-trip differences. They were corrected before scientific evaluation and
remain retained. The first evaluation ran all cells but failed a harness assertion
that counted internal dummy indices as external indices; the test was corrected
to apply Contract first. The unchanged notebook then passed the final evaluation.
Historical failed assertions are not counted as passes.

The saved Mathematica Graphics output contains all four diagrams. A raster
preview of the diagram geometry and labels was inspected. The installed FeynArts
EPS exporter omitted its page wrapper; Ghostscript rejected that original file.
A preview-only copy added the existing bounding box operands and page wrapper,
without changing diagram geometry, notebook boxes or package code. This is not
certification of native EPS export or a complete Mathematica front-end rendering
check. The notebook itself displays the saved Graphics output and does not use EPS.

The preceding eleven notebooks and their saved outputs are unchanged. This task
changes no rules, libraries, generated model files or other implementation code.
No new loop, other-process, polarisation-basis or release validation is claimed.

## Reproducibility

Evidence root: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-26-example12`.
`cells.json` retains the source specification; `build-r4.wls` performs the held
round trips. `evaluate-r2.wls` evaluates the notebook and loads
`evaluate-checks-r2.wl` only after package loading, avoiding pre-load context
binding in the checks. `run-evaluate-r2/` contains exact results, raw diagnostics,
cell timings, source identities, resource observations and termination records.
The comparison source is the earlier `2026-10-26-fa-ym/native/run-trees-vvvt-r3/`
retained tree. No recalculation is needed to inspect this evidence.

Initial delivered notebook SHA-256: `4de893d259de28fd185c8435ae40c3aa2e195b8f39c429ec8b423df5c8ade3f7`.


## Helicity subsection — 26 October 2026

After the maintainer's edits and the separation of checks into explanatory text
and direct Boolean cells, section 7 adds six inputs and two saved outputs. All
19 existing inputs and all existing saved outputs are preserved. Only the
introductory sentence excluding helicities is updated.

The new result is one expression in h1, h2, h3, h4, s and t, retaining gs, κ and
f^{abc}. Each hi is ±1; the graviton helicity is 2h4. The initial local replacement rules
specified all 16 momentum–polarisation products and the ten independent symmetric
polarisation–polarisation products. They preserve outgoing conjugation and use
s > 0, −s < t < 0, with positive square-root branches and u = −s − t. The final
result is specialised to four-dimensional external states; the earlier
symbolic-D amplitude and Ward checks remain unchanged. No scalar products or
`$Assumptions` were assigned globally by that initial version. The subsequent
assignment-style revision is recorded below.

For an independent derivation, use the +--- metric, E = √s/2,
c = 1 + 2t/s and d = 2√[−t(s+t)]/s. The momenta are

\[
 p_1=E(1,0,0,1),\quad p_2=E(1,0,0,-1),\quad
 p_3=E(1,d,0,c),\quad p_4=E(1,-d,0,-c).
\]

The vectors contracted into the amplitude are

\[
 e_1=\frac{(0,ih_1,-1,0)}{\sqrt2},\quad
 e_2=\frac{(0,-ih_2,-1,0)}{\sqrt2},\quad
 e_3^*=\frac{(0,-ih_3c,-1,ih_3d)}{\sqrt2},\quad
 e_4^*=\frac{(0,ih_4c,-1,-ih_4d)}{\sqrt2}.
\]

These equations fix phases as well as helicity signs. The last vector forms the
outgoing graviton tensor by multiplication with itself. The test substitutes a
positive rational direction parameter, c = (1−v²)/(1+v²), d = 2v/(1+v²), to
compare exact expressions without unresolved square-root branches.

The final bounded run passes **17/17 assertions**: exact held round trips for all
new inputs; prior reconstruction/limits and Ward checks; on-shell momenta and
Mandelstam conventions; transversality, nullness and conjugate normalisation;
physical spin-1 helicity eigenvalues; all 26 independently calculated products;
exact full-amplitude agreement with the Cartesian contraction of the original
tensor, both modulo hi²−1 and separately for all 16 helicity assignments; and an
altered-amplitude negative control. The result has 124 leaves. The notebook
presents one general expression, not a helicity table.

The accepted evaluation takes 8.306 s including package loading, previous cells
and independent checks; the helicity simplification itself takes 0.569 s in this
run. Peak aggregate memory is 291,151,872 bytes (about 278 MiB). Limits remain
1 GiB, no swap, 120+10 s and fresh 2 GiB headroom. Diagnostics and memory events are
empty/zero, exit is zero and no owned calculators survive. Earlier stopped
harness attempts exposed top-level multiline parsing and pre-load shadow symbols;
the final harness uses single-expression cells and an isolated formatting context.
Those failed attempts are preserved, not counted as passes.

The final format-only check confirms that all six display-box expressions match
the verified source exactly. Its strict ReadList assertion initially rejected
comment/blank records: inspection of the retained parse shows exactly one
Notebook and six Hold[Null] records, with no unexpected expressions. The notebook
therefore parses correctly; the failed strict assertion is retained. This is
kernel syntax verification, not full front-end visual certification.
Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-26-example12-helicity/`,
particularly `run-verify-r4/` and `run-display-check/`.
The FeynCalc conventions are documented under
[Polarization](https://feyncalc.github.io/FeynCalcBook/Polarization.html) and
[Pair](https://feyncalc.github.io/FeynCalcBook/Pair.html).

Helicity-extension notebook SHA-256: `19034148b71bfad38da7cf2a8ba4cb04a428aedf8fa7c76cbe543be28e7ca3d5`.

## Explicit Pair assignments — 26 October 2026

At the maintainer's request, the two scalar-product cells now use explicit
`Pair[Momentum[... , D], Momentum[Polarization[...], D]] = ...;`
assignments. All 26 products retain the same values, including conjugate outgoing
states. The amplitude cell no longer applies replacement lists; its stray
`direct` factor was removed. The text explains that these definitions persist
in the kernel: evaluate general Ward checks first, and restart the kernel to
return to unfixed polarisation states. No assignment is made to D itself.

Three inputs and four explanatory text cells were changed. All other input
cells and every saved output are byte-preserved from the maintainer's current
notebook. The whole notebook was evaluated in one fresh bounded kernel. All
**9/9 assertions** passed, covering exact changed-input parsing, previous
amplitude/Ward checks, all 26 products against the verified reference, exact
helicity-amplitude equality and a negative control, absence of leftover objects,
repeatable assignments and unassigned D/N. This reuses the earlier independent
helicity derivation; it does not claim a new independent derivation or front-end
visual inspection.

The kernel completed in 8.288 s, with peak aggregate memory
289,509,376 bytes. The 1 GiB/no-swap/120+10 s/fresh
2 GiB limits were retained; diagnostics and resource events were absent, exit
was zero and no task-owned calculators survived.
Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-26-example12-pair-assignments/`.
Notebook SHA-256: `e2368a5dd27c393abeb0b087313f2608a3ddb7fc16db8ab5cdc5009595039afc`.

## Closure review

The maintainer's subsequent front-end save is preserved, including the updated
saved outputs. Static comparison with the final bounded-check candidate finds
23 input boxes identical apart from layout whitespace; the two Pair-assignment
cells have unchanged ordered code tokens with different RowBox nesting and
whitespace. All 25 executable input expressions are retained. No calculation
or front-end validation was repeated for this commit. The model/source
manifest hashes and documentation links were checked again before staging.

Notebook SHA-256 at closure: `08fd281365554d15bff337790264f22eb809826e111ca2bafc2b3de818f4bdc7`.
