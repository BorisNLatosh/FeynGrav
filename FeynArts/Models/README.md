# Native FeynArts models

Each bundle contains matching `.gen` and `.mod` files, a provenance manifest
and a model guide. The vertex cutoff is κ², not a total diagram-order cutoff.
Loop validation is deferred.

| Model | Contents |
| --- | --- |
| [GR](GR/README.md) | General relativity and gravitational ghosts |
| [Single-Scalar-Tensor-Minimal-Coupling](Single-Scalar-Tensor-Minimal-Coupling/README.md) | Massive scalar with cubic/quartic potential |
| [John](John/README.md) | Massive scalar with non-minimal kinetic coupling |
| [Einstein-scalar-Gauss-Bonnet](Einstein-scalar-Gauss-Bonnet/README.md) | Massive scalar with linear/quadratic Gauss–Bonnet couplings |

See the [catalogue and common loading guide](ModelCatalogue.md) for registration,
standalone use, naming migration and verification. Use a fresh kernel and
matching `Model` and `GenericModel` strings with FeynCalc-patched FeynArts.

<!-- Compatibility anchors for links to the former detailed README. -->
<a id="supplied-feynarts-models"></a><a id="common-loading-workflow"></a><a id="naming-migration--19-october-2026"></a><a id="scalar-extension-verification"></a>
Detailed sections: [ModelCatalogue](ModelCatalogue.md).
