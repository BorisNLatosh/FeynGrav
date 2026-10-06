# SU(N) colour preflight — 6 October 2026

> Historical implementation record. Forward-looking statements describe that stage. For current capabilities use the [converter guide](../../README.md). The original observations and limitations below are retained.

## Status

**Historical preflight report.** The redistribution decision was subsequently
resolved on the documented GPL basis. See `ColourAlgebra.md` for completed
integration and regression verification. The investigation below records the
evidence and status at the preflight stage.

**At the end of the initial preflight, converter integration was pending the
redistribution decision.** No production definitions or defaults were
changed in this stage. Existing uncommitted Dirac and epsilon work is preserved.

The approved plan requires confirmation that redistribution terms cover
`SUn.prc` before bundling it or its adaptation. At that stage this had not been established.
This is an unresolved provenance question, not a conclusion that redistribution
is prohibited.

## Provenance investigation

The [official package page](https://www.nikhef.nl/~form/maindir/packages/color/color.html)
links the archive and describes it as example programs. The downloaded archive
contains `SOn.prc`, `Spn.prc`, `SUn.prc`, `su.frm` and `tloop.frm`; no licence file
or explicit redistribution notice was found in those files. The procedure
attributes itself to J. Vermaseren, 7 January 1997.

The [FORM licence page](https://www.nikhef.nl/~form/license/license.html) states
GPL version 3 or later for FORM. It does not explicitly identify the separate
colour archive. The current official FORM repository tree includes a colour
regression example but no `SUn.prc`; that example is not evidence of a licence
for every member of this archive. In accordance with the approved plan, the
executable's licence has not been assigned to the procedure by inference.

Exact archive/member checksums are in `../ColourPreflight/Upstream.json`.
No upstream source or adaptation has been copied into the repository. The
unchanged source used for local testing remains outside the repository.

## Completed verification

| Check | Result |
|---|---|
| Unmodified SUn, serial FORM 4.3 | 18/18 zero residuals |
| Unmodified SUn, TFORM 4.3, two workers | 18/18 zero residuals |
| Independent FeynCalc 10.2.1 reference | 18/18 zero residuals |
| Empty trace | Upstream retains `Tr()`; adapter handling required |
| Three-generator trace | Retained; output-basis conversion required |
| Four-distinct-generator trace | Retained, consistent with planned output basis |

The test sources and expected output are retained in `../ColourPreflight`.
The tests confirm both symmetric-constant contractions and mixed f/d zeroes,
in addition to the earlier Casimir/completeness probes. They also establish
that inverse normalisation/flavour powers need explicit substitutions.

## Work identified at the preflight stage (subsequently completed)

Obtain an applicable upstream licence statement or permission covering
redistribution and adaptation of this exact procedure. Then complete the
namespaced adaptation, ColourAlgebra companion, typed translation and index
validation, endpoint promotion, version-five mappings and parser, independent
Dirac/colour option wiring, scalar Casimir presentation and documentation.

The planned export–FORM–import colour tests, mixed-structure cases, malformed
format checks, legacy compatibility checks and full converter/runtime regression
run were then outstanding. The standalone identities above do not establish those
acceptance conditions. No benchmark suite was run and no commit was made.
