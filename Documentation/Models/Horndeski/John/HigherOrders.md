# Native-model order limits

The supplied native model stops at **κ² per vertex**. Generating or importing additional FeynGrav libraries does not enlarge its saved FeynArts vertex inventory. A diagram made from allowed vertices can have a total coupling order above κ². Higher-order native models need separate construction and scientific validation; successful file generation alone does not establish a physical amplitude.

[Native model guide](./NativeModel.md) · [Library-generation guide](../../../Services/LibraryGeneration/Guide.md)

Each John vertex is linear in β; a diagram may contain β² and higher powers. The scalar and John library order counts attached gravitons, whereas the pure-gravity order counts two fewer than the graviton legs. An order-three extension would need both minimal and John contributions to `TTTSS`, alongside the five-graviton contact.

<!-- Compatibility anchors for former guide sections. -->
<a id="adding-higher-orders-in-κ"></a><a id="1-prepare-and-independently-verify-the-source-libraries"></a><a id="2-extend-the-shared-builder"></a><a id="3-build-and-validate-a-separate-candidate"></a><a id="4-install-the-matched-bundle"></a>
