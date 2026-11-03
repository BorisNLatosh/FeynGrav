# Saved calculations and compatibility

Keep each FORM result together with the exact mapping file created by the same export. `CalcFormImport` checks their correspondence before reconstructing the FeynCalc expression. Renaming or moving the pair is possible if you supply their new paths; editing or reformatting the mapping breaks its digest, even when the JSON appears equivalent.

## Supported versions

The current importer accepts mapping versions 1 through 8. Export selects the format required by the expression and enabled calculation stages; users do not need to select a version manually. Versions cover ordinary tensor/scalar algebra, epsilon conventions, Dirac/colour processing, one-loop tensor decomposition, saved coefficient reduction and scalar-master reduction. Unsupported versions return `Failure["InvalidMapping", ...]`.

Older supported files remain importable. Use documentation matching the installed package; a newer export need not be readable by an older package. The restricted `FORMBenchmark` method uses different internal job files; do not pass those files to the full-pipeline import commands.

<a name="Conventions and integrity"></a>

## Conventions and integrity

Saved mappings preserve symbol contexts, dimensions and the conventions used by the selected features. Relevant epsilon, colour and scalar-product settings are checked at import. A `CoefficientConventionMismatch` means that the supplied scalar-product identities no longer match the export: re-export under the intended definitions. Do not edit a mapping to bypass a mismatch.

The correspondence digest detects mismatched files; it is not authentication or an independent check of the FORM calculation. Retained jobs contain the symbolic input and diagnostic files. Keep them deliberately, and keep the matching mapping whenever preserving a result for later import.

See the [manual workflow](./Guide.md#Manual%20workflow), [import command](./CalcFormImport.md) and [restricted FORM method](./RestrictedFORM.md).

<!-- Compatibility anchors for former guide sections. -->
<a id="persisted-formats-versions-1-through-8"></a><a id="version-and-correspondence"></a><a id="version-selection"></a><a id="mapping-fields"></a><a id="denominator-authority-and-multiplicity"></a><a id="restricted-mathematica-expression-encoding"></a><a id="form-result-grammar"></a><a id="tokenisation-and-precedence"></a><a id="compatibility-fixture"></a><a id="polarisation-vector-extension"></a><a id="version-two-lorentz-epsilon-tensors"></a><a id="version-three-dirac-and-colour-translation"></a><a id="version-four-configured-dirac-processing-and-explicit-traces"></a><a id="version-five-sun-colour-processing"></a><a id="grouped-propagator-results"></a><a id="factorised-coefficients"></a><a id="bounded-rational-coefficient-output"></a><a id="numerator-cancellation-without-a-format-change"></a><a id="dimension-only-rational-coefficients"></a><a id="version-six-one-loop-tensor-coefficients"></a><a id="optional-reduction-metadata"></a><a id="version-seven-reduction-of-existing-coefficients"></a><a id="optional-compact-reduction-blocks-versions-six-and-seven"></a><a id="optional-shared-coefficient-definitions-versions-six-and-seven"></a><a id="incremental-denominator-groups-versions-six-and-seven"></a><a id="version-eight-scalar-master-reduction"></a><a id="optional-coefficient-kinematics-snapshot"></a>
