# FeynGravLibraryInformation

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynGravLibraryInformation``. **Baseline:** 4.2.0 development.

## Syntax

```mathematica
FeynGravLibraryInformation[]
FeynGravLibraryInformation[importerSymbol]
```

`FeynGravLibraryInformation[]` returns an association keyed by fully qualified importer names. `FeynGravLibraryInformation[FeynGravImportVectors]` selects the last successful vector import; pass the importer symbol without `[]`. Unknown importers or malformed query arguments return `Failure`. Families with no recorded import return `Missing["NotRecorded"]`.

Every successful import, including initialisation, publishes its record together with the new vertex definitions. A failed or aborted import preserves both. Reimporting replaces the record for that importer, so it describes the currently installed import rather than accumulating a history. Package reloads preserve records for optional families that are not reimported.

| Field | Meaning |
| --- | --- |
| `Importer`, `Targets` | Fully qualified importer and vertex-function names. |
| `Orders` | Actual orders loaded, grouped by library filename family; these may be fewer than requested. |
| `Files` | Absolute path, SHA-256 hash, filename family, order and complete filename indices for each loaded file. Horndeski indices retain `{a,b,n}`. |
| `ImportedAt` | UTC `DateObject` recorded on successful import. |
| `Settings` | Held snapshots of the relevant settings when importing. |
| `CurrentSettings` | Corresponding settings when the query runs. |
| `ChangedSettings` | Names of settings whose current values differ from their import snapshots. |
| `GenerationSettings` | `Missing["NotRecorded"]`: historical library-generation settings cannot be recovered from these files. |

Snapshots cover `FeynGrav`'s gravitational coupling and `D` for every importer, the vector/Yang–Mills gauge parameter for its corresponding importer, and `$LeviCivitaSign` for axion–vector imports. They describe the import environment, not necessarily a nontrivial dependence of every individual vertex. Propagator-only gauge parameters do not enter these records. Arbitrary user-defined FeynCalc scalar products and transformation rules are not captured.

Values are wrapped in `HoldComplete` to prevent an unassigned symbol in an old record from acquiring a later assignment. Do not release that hold to reconstruct past settings: it allows evaluation in the current session.

```mathematica
information = FeynGravLibraryInformation[FeynGravImportVectors];
information["Settings"]
information["ChangedSettings"]
```

For example, if vectors were imported with `GaugeFixingEpsilonVector = -1`, then changing it to `-2` makes `ChangedSettings` identify that parameter. Reimport the required vector order to update the loaded definitions and record together. A change indicates a difference in settings, not a proof that a particular expression has changed.

The query reads only in-memory records and settings: it does not reopen files, recalculate hashes, import libraries or launch FORM. Hashes identify the files as read, not their contents at query time. The importer checks hashes before and after reading and rejects a detected change. Import records do not track manual vertex redefinitions or expressions computed earlier by the user. Successful queries return associations; they do not print a conventions report. See the [Library imports](./README.md) for the tested scope.
