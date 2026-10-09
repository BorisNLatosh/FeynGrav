# Library import bookkeeping — 9 October 2026

## Scope

All twelve main-package library importers now record successful imports in memory. `FeynGravLibraryInformation[]` exposes the records, and `FeynGravLibraryInformation[importVectors]` selects one importer. See the [reference](../Reference.md#library-import-records) for fields and limitations.

Records and vertex definitions are published together after all files have loaded successfully. Records include actual filename families and orders, complete Horndeski parameter tuples, absolute paths, SHA-256 hashes, UTC import time and held snapshots of relevant settings. Hashes are checked before and after each read. Failed reads, aborts, hashing failures and detected changes during import leave the previous definitions and records intact.

This records the import environment, not the provenance of historical library generation. It does not track manual vertex redefinitions or expressions evaluated earlier. It does not implement the proposed full conventions report.

## Verification

Temporary Wolfram scripts were run in fresh kernels; no top-level Tests folder was added.

- **51 focused assertions passed:** automatic initial records; all twelve real importers; unknown records and invalid queries; actual family/order coverage; SHA-256 verification; Horndeski filename tuples; UTC timestamps; gauge, coupling and epsilon snapshots; changes in current settings; unassigned-symbol stability; successful reimport; read-only queries; invalid import options/orders; late read failure; abort; changed-file rejection; hash failure; successful retry; settings changed during import; optional-family records across reload; automatic settings reset on reload; and unchanged working directory.
- **18 vertex definition sets matched the pre-change implementation exactly by SHA-256 of their DownValues**, in separate fresh kernels. These cover the four automatic imports through order two, SUNYM and axion-vector imports through order one, all installed Horndeski files, scalar–Gauss–Bonnet order two and quadratic gravity order one. The comparison verifies that adding metadata did not alter the loaded definitions at those orders; it is not a new derivation of the interaction formulas.
- Failure injection used temporary scalar libraries or dynamically scoped private helpers. Repository libraries were not changed or regenerated.
- `git diff --check` passed. No FORM process or benchmark suite was needed for this import-only change.

Local verification evidence was produced under `/tmp/feyngrav-bookkeeping-audit`, including `verify.wls`, `verify.log`, `results.m`, and the before/after definition hashes. These temporary paths are local evidence, not distributed dependencies.
