# Native-model order limits

The supplied native model stops at **κ² per vertex**. Generating or importing additional FeynGrav libraries does not enlarge its saved FeynArts vertex inventory. A diagram made from allowed vertices can have a total coupling order above κ². Higher-order native models need separate construction and scientific validation; successful file generation alone does not establish a physical amplitude.

[Native model guide](./NativeModel.md) · [Library-generation guide](../../Services/LibraryGeneration/Guide.md)

Only the two-graviton Gauss–Bonnet source has the independent corrected curvature check. Higher-order legacy sources and the stored order-three library require a separate mathematical audit. There are no zero- or one-graviton Gauss–Bonnet vertices. The $g_2\phi^2/2!$ factorial cancels under scalar differentiation; do not add another factor of two.

<!-- Compatibility anchors for former guide sections. -->
<a id="adding-higher-orders-in-κ"></a><a id="1-audit-and-prepare-the-source-libraries"></a><a id="2-extend-the-builder-consistently"></a><a id="3-build-and-validate-a-separate-candidate"></a><a id="4-install-the-matched-bundle"></a>
