# Library imports and provenance

[Documentation index](../../README.md) · [Complete symbol index](../../Symbols.md)

**Importer naming update:** the former `import…` commands are now `FeynGravImport…`, retaining their suffixes and argument signatures. Replace `printOutput` (including its old private form) with `Verbose`. Old names are not compatibility aliases. Restart the kernel after updating to avoid retaining previous definitions. Imports are silent by default; use, for example, [`FeynGravImportScalars[2, Verbose -> True]`](FeynGravImportScalars.md) to print diagnostics. [`FeynGravLibraryInformation`](./FeynGravLibraryInformation.md) accepts the renamed symbols and reports their fully qualified names. Historical reports and saved notebook outputs retain their original spelling.

Imports read local extensionless files; they neither download nor generate them. Successful calls normally return `Null` after installing definitions. They use `Verbose -> False` by default. Set `Verbose -> True` to print inventory and progress messages. Unknown options and non-Boolean values return `Failure`.

For order-limited importers, omitted `n` defaults to 2. Except for Gauss–Bonnet, `n` must be a positive integer. The requested maximum is capped by the installed families, but missing intermediate files cause failure. Failed reads or invalid orders preserve previously installed definitions. A successful lower-order reimport replaces the family's definitions through the selected order; it does not retain previously loaded higher orders.

| Command | Files and order convention |
| --- | --- |
| [`FeynGravImportGravitons[n : 2, opts]`](FeynGravImportGravitons.md) | `GravitonVertex_j`, `1 <= j <= n`; `j + 2` graviton legs. Loaded initially with 2. |
| [`FeynGravImportScalars[n : 2, opts]`](FeynGravImportScalars.md) | Scalar kinetic and potential files for 1 through `n` graviton pairs. Loaded initially with 2. |
| [`FeynGravImportFermions[n : 2, opts]`](FeynGravImportFermions.md) | Fermion files for 1 through `n` graviton triples. Loaded initially with 2. |
| [`FeynGravImportVectors[n : 2, opts]`](FeynGravImportVectors.md) | Massive vector, massless vector and vector ghost files through `n`. Loaded initially with 2. |
| [`FeynGravImportSUNYM[n : 2, opts]`](FeynGravImportSUNYM.md) | Six Yang–Mills file families: two-, three- and four-gluon, quark–gluon, ghost and gluon–ghost. |
| [`FeynGravImportAxionVectorVertex[n : 2, opts]`](FeynGravImportAxionVectorVertex.md) | Axion–vector files through `n` graviton pairs. |
| [`FeynGravImportQuadraticGravity[n : 2, opts]`](FeynGravImportQuadraticGravity.md) | `QuadraticGravityVertex_j`, where `j + 2` is the number of graviton legs. |
| [`FeynGravImportScalarGaussBonnet[n : 2, opts]`](FeynGravImportScalarGaussBonnet.md) | Files for 2 through `n` graviton triples; requires `n >= 2`. |
| [`FeynGravImportHorndeskiG2[opts]`](FeynGravImportHorndeskiG2.md) | All available G2 files; no order argument. |
| [`FeynGravImportHorndeskiG3[opts]`](FeynGravImportHorndeskiG3.md) | All available G3 files; no order argument. |
| [`FeynGravImportHorndeskiG4[opts]`](FeynGravImportHorndeskiG4.md) | All available G4 files; no order argument. |
| [`FeynGravImportHorndeskiG5[opts]`](FeynGravImportHorndeskiG5.md) | All available G5 files; no order argument. |

`n : 2` above describes the default; actual calls use [`FeynGravImportScalars[]`](FeynGravImportScalars.md) or [`FeynGravImportScalars[2]`](FeynGravImportScalars.md). Horndeski filenames have the form `HorndeskiG2_a_b_n`, and the installed files determine the accepted vertex signatures. Use `Verbose -> True` to inspect that selection. Avoid assuming every requested model/order is distributed.

## Individual pages

| Symbol | Kind | Audience |
| --- | --- | --- |
| [``FeynGrav`FeynGravImportAxionVectorVertex``](./FeynGravImportAxionVectorVertex.md) | command | user |
| [``FeynGrav`FeynGravImportFermions``](./FeynGravImportFermions.md) | command | user |
| [``FeynGrav`FeynGravImportGravitons``](./FeynGravImportGravitons.md) | command | user |
| [``FeynGrav`FeynGravImportHorndeskiG2``](./FeynGravImportHorndeskiG2.md) | command | user |
| [``FeynGrav`FeynGravImportHorndeskiG3``](./FeynGravImportHorndeskiG3.md) | command | user |
| [``FeynGrav`FeynGravImportHorndeskiG4``](./FeynGravImportHorndeskiG4.md) | command | user |
| [``FeynGrav`FeynGravImportHorndeskiG5``](./FeynGravImportHorndeskiG5.md) | command | user |
| [``FeynGrav`FeynGravImportQuadraticGravity``](./FeynGravImportQuadraticGravity.md) | command | user |
| [``FeynGrav`FeynGravImportSUNYM``](./FeynGravImportSUNYM.md) | command | user |
| [``FeynGrav`FeynGravImportScalarGaussBonnet``](./FeynGravImportScalarGaussBonnet.md) | command | user |
| [``FeynGrav`FeynGravImportScalars``](./FeynGravImportScalars.md) | command | user |
| [``FeynGrav`FeynGravImportVectors``](./FeynGravImportVectors.md) | command | user |
| [``FeynGrav`FeynGravLibraryInformation``](./FeynGravLibraryInformation.md) | command | user |
