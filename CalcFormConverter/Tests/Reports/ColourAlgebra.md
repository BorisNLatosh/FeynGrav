# SU(N) colour algebra verification — 6 October 2026

> Follow-up: the [generator migration](../../../Documentation/Verification/GeneratorVerification.md)
> changes the public default to `ColourAlgebra -> True`, retaining `Automatic`
> compatibility, and corrects the dimensional quark and axion rule inputs.
> The results below record the preceding colour-layer implementation.

## Delivered

`ColourAlgebra -> Automatic` is the export/calculation default; `False` preserves
colour objects and unevaluated SUNTrace. The official SUn algorithm is embedded
in standalone generated programs with a namespaced adaptation. Colour and
Dirac processing are independently selectable. Version-five files carry typed
indices, conventions, procedure identity and generated endpoint information;
versions one through four remain readable.

The unchanged upstream reference, GPL text, source checksums, licence basis and
adaptation notes are in `../../ThirdParty/FORMColour`. The earlier
`ColourPreflight.md` records the initial investigation, not the current delivery
status. Integration proceeded on the documented GPLv3-or-later basis following
the user's instruction.

## Completed checks

| Suite | Passed assertions |
| --- | ---: |
| Core | 111 |
| Parser | 121 |
| Export transactions | 20 |
| Installer decisions (mocked) | 62 |
| FORM export | 31 |
| FORM stages | 128 |
| FORM import, including fixed version-one fixtures | 65 |
| Runtime | 82 |
| Epsilon | 48 |
| Dirac/colour translation | 65 |
| Dirac algebra | 104 |
| New colour algebra | 137 |
| Existing translation-only interaction rules | 23 |
| Colour-enabled interaction rules | 23 |
| **Total** | **1020** |

All listed assertions passed. Independent and automatic package loading also
passed: no process launch or directory change; existing FeynCalc definitions
and options remained unchanged. The unchanged upstream procedure separately
passed 18 exact identities in FORM 4.3 and TFORM 4.3 with two workers; the
independent FeynCalc 10.2.1 reference passed those same 18 identities.

The new colour suite covers both engines, all four combinations of colour and
Dirac switches with epsilon expressions, free index spaces, symbolic Casimirs,
Fierz completeness, f/f, d/d and f/d contractions, trace products/powers,
cyclic order without reversal, scalar identity branches and endpoint promotion.
It also checks generated dummy multiplicities, wrong index spaces, leaked
internal objects, malformed metadata/results, cache isolation, version
selection and preservation mode. Generated files have no include dependency.

The interaction suite compares zero-, one- and two-graviton fermion and
Yang–Mills families, including the four-gluon vertex. Original mixed-dimensional
quark–gluon expressions remain explicitly rejected; only the separate comparison
copy is converted to one Lorentz space. No interaction formula was edited.

## Findings resolved during verification

- FORM's generated fundamental sums require a temporary default dimension N.
  The Lorentz dimension is restored before the Dirac procedure.
- Inverse normalisation/flavour powers need explicit substitutions; positive
  powers alone leave internal parameters in trace commutators.
- A four-generator trace is a valid terminal object. Interaction comparisons
  require a common trace basis: FeynCalc's `Explicit -> True`,
  `SUNTraceEvaluate -> False`, `SUNNToCACF -> False`. Initial comparisons across
  different bases left unresolved residuals (and one timeout); the common-basis
  symbolic comparisons all passed. No numerical sample was substituted for
  those identities.
- The old unsupported-SUNTrace test was removed because the vocabulary now
  deliberately supports it. Legacy dictionary/mapping tests explicitly select
  `ColourAlgebra -> False` to retain their version-three fixtures.

## Reproduce

From `CalcFormConverter`, run the developer driver with `--suite core`,
`--suite form` and `--suite runtime`. Runtime tests include the pinned unchanged
procedure and the new colour suite. Run `Tests/Loading.wls`,
`Tests/DiracColourRules.wls` and `Tests/ColourRules.wls` separately in fresh
kernels with `CFC_TEST_DIR` set to an existing temporary directory. Those
separate scripts avoid the driver's additional bubble-export integration job.
The developer driver uses Python; normal package use needs only Wolfram,
FeynCalc and FORM/TFORM. No tests install packages.

No Full benchmark suite was run, no performance claim is made, no libraries
were regenerated, and no commit was created. The library generator is unchanged.
