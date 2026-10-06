# Official FORM SU(N) procedure

- Author: J. Vermaseren, 7 January 1997, as stated in the original header.
- Source: https://www.nikhef.nl/~form/maindir/packages/color/color.tar.gz
- Package documentation: https://www.nikhef.nl/~form/maindir/packages/color/color.html
- Archive SHA-256: `f648fd368a03c0d7237f40ed42768095d7cc84f47009af61ddc4d421392f46f5`.
- Unchanged `SUn.prc` SHA-256: `05548386b1e5a2872224a78bad6a424fb13f812ee6ab35d89d1c77a32f7034c3`.
- Adaptation: `../../Templates/ColourAlgebra.frm.in`, procedure identity `CFC-SUn-1`.
- Adaptation SHA-256: `0473a32d59a72160cb85931482e1d57f70e50e7a2d10cc0a0fdc3b0cf8d0eaac`.

## Licence and provenance

Distributed here on the GPL version 3 or later basis adopted for this integration.
The included `COPYING` is the GPLv3 text from the official FORM repository.
The original source remains byte-for-byte unchanged and retains its attribution.
The adaptation carries a separate notice and this list of changes.

The official [licence page](https://www.nikhef.nl/~form/license/license.html)
states GPLv3 or later for FORM. The official
[reference manual](https://form-dev.github.io/form-docs/stable/manual/)
identifies the colour package as part of the distribution. The archive itself
has no separate licence header; this record does not claim otherwise.
Supporting reuse precedents include HepLib (arXiv:2103.08507, section 2.2) and
FormTracer's attributed SUNfund adaptation in its GPL-licensed source.
No FormTracer code or runtime dependency is included here.

## Changes from the reference

1. Prefix the procedure, temporary tensors/functions, symbolic parameters,
   wildcard fields, loop variables and internal indices to prevent collisions.
2. Substitute the mapped symbolic rank for NF and configure adjoint dimension
   N^2-1, generator normalisation 1/2 and flavour multiplicity one.
3. Remove unused upstream cF/cA presentation substitutions. Scalar Casimir
   presentation is applied after parsing, independently of tensor reduction.
4. Handle empty traces before the procedure and convert three-generator traces
   to symmetric/antisymmetric tensors only after reduction. Longer traces stay.
5. Explicitly substitute inverse normalisation/flavour powers as well as their
   positive powers. The original mathematical completeness algorithm is retained.
6. The Wolfram companion embeds the complete adaptation and configures/restores
   the default dimension around fundamental sums. Generated programs need no
   include path, package installation or network access.

The production template is exercised by complete FORM/TFORM round trips.
The standalone preflight fixture separately checks the unchanged original.
