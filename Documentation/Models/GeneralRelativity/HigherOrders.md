# Native-model order limits

The supplied native model stops at **κ² per vertex**. Generating or importing additional FeynGrav libraries does not enlarge its saved FeynArts vertex inventory. A diagram made from allowed vertices can have a total coupling order above κ². Higher-order native models need separate construction and scientific validation; successful file generation alone does not establish a physical amplitude.

[Native model guide](./NativeModel.md) · [Library-generation guide](../../Services/LibraryGeneration/Guide.md)

Pure-graviton library order `n` means `n + 2` graviton legs. The current model supplies the three- and four-graviton contacts. The opt-in order-three model adds the five-graviton contact needed for a complete five-point tree.

`FeynGravBuildModel` accepts `GravitonOrder -> 2` (default) or `3`. Order three requires `Libs/GravitonVertex_3`. With the verified generic library, the quintic contact uses one FeynArts component. Its 722 permutation representatives reproduce all 76,230 monomials when the five momenta are supplied. A changed library uses the general basis builder. Start with unassigned external kinematics in a fresh kernel.

## Five-graviton scattering

For two incoming and three outgoing gravitons, request `Adjacencies -> {3,4,5}` in native FeynArts and check that all 26 labelled trees have insertions. There is one quintic contact, ten cubic–quartic exchanges and fifteen exchanges built from three cubic vertices. Each graph has unit topology weight. Construct one exchange from each family and permute **both the momenta and the polarisation tensors** to obtain the other channels. Applying the physical external states before summing the diagrams avoids expanding the full free-index tree.

At one exact, conserved massless point in four dimensions, all 25 exchange contributions agreed separately with an independent contraction of the stored vertices. The complete tree cancelled exactly for a pure-gauge replacement on each of the five legs. Removing the contact gave a nonzero residual in all 20 tested components. Along a separate conserving, on-shell deformation, the coefficient of the leading soft pole agreed exactly with the universal soft factor times the four-graviton amplitude, also for each hard emitting leg separately. These are physical projections and a leading soft limit. The open-index, symbolic-`D` comparison applies to the quintic **contact**. [Leading soft-graviton theorem](https://arxiv.org/abs/1404.4091)

The order-three build took about 10 seconds in a bounded check, and the saved model initialised in 2.3 seconds. From a fresh kernel and the saved order-three model, one full run through construction, exact physical projection and comparison took 57.74 seconds, with a highest recorded 277 MiB for the process tree. This timing uses the three graph families at one four-dimensional point. Use native FeynArts for five-point construction. The optional process bridge covers its documented lower multiplicities. [Native model and bridge guide](../../Services/FeynArtsIntegration/Guide.md#Optional%20model%20building)

<!-- Compatibility anchors for former guide sections. -->
<a id="adding-higher-order-graviton-vertices"></a><a id="five-gravitons-the-existing-order-three-builder"></a><a id="six-or-more-gravitons-extend-the-builder-first"></a>
