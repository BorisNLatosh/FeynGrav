# Native-model order limits

The supplied native model stops at **κ² per vertex**. Generating or importing additional FeynGrav libraries does not enlarge its saved FeynArts vertex inventory. A diagram made from allowed vertices can have a total coupling order above κ². Higher-order native models need separate construction and scientific validation; successful file generation alone does not establish a physical amplitude.

[Native model guide](./NativeModel.md) · [Library-generation guide](../../Services/LibraryGeneration/Guide.md)

Pure-graviton library order `n` means `n + 2` graviton legs. The current model supplies the three- and four-graviton contacts. The independently checked quintic source is distinct from a validated native five-point amplitude.

`FeynGravBuildModel` accepts `GravitonOrder -> 2` or `3`; larger values are unsupported. Order three requires `Libs/GravitonVertex_3`. The native order-three path remains outside the accepted tree scope and can be expensive: historical initialisation took about 486 seconds and 6.07 GB peak memory, and a complete five-point construction did not finish within 900 seconds. These are single-machine observations, not guarantees. The 600-second initialisation timeout is not a memory cap. The optional bridge does not admit five-point processes. See the [bridge guide](../../Services/FeynArtsIntegration/Guide.md#Optional%20model%20building).

<!-- Compatibility anchors for former guide sections. -->
<a id="adding-higher-order-graviton-vertices"></a><a id="five-gravitons-the-existing-order-three-builder"></a><a id="six-or-more-gravitons-extend-the-builder-first"></a>
