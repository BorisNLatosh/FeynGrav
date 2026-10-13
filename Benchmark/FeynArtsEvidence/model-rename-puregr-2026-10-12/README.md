# Renaming the pure-GR model to `PureGR`

Executed 12 October 2026. The model bytes were not touched; only names changed. No rule, library or other package code changed apart from the two wrapper lines described below. No commit or merge was made.

## Why the old name existed

`FeynArtsIntegration/ModelBuilder.wl:122` generated it:

```wl
hash = Hash[Join[rules, props], "SHA256", "HexString"];
name = "FGPureGR" <> StringTake[hash, 16];
```

so `FGPureGR5a149c98161e2b2e` was "FeynGrav pure GR" plus the first 16 hex digits of the SHA-256 over the exact rule and propagator set, and `Diagram.wl:12` enforced that shape with `RegularExpression["FGPureGR[0-9a-f]{16}"]`. The intent was that the filename itself identifies the snapshot. The cost was a name no one wants to type or read, in a package whose other model names are short (`SM`, `MSSM`, `Lorentz`).

## Why not `Pure_General_Relativity`

That name does not parse as a symbol in Wolfram Language:

```
Hold[Pure_General_Relativity]   →   (Pure_General)*_Relativity
Head[...]                       →   Times
```

The underscores are read as `Pattern`/`Blank`, so the name would work only as a quoted string; anything unquoted — `Model -> Pure_General_Relativity`, or a reader copying it from the docs — becomes a pattern expression, and FeynArts would look for a file literally named `(Pure_General)*_Relativity.gen`. `PureGR` parses as a symbol and matches FeynArts' convention.

## What changed

| Item | Before | After |
| --- | --- | --- |
| `FeynArts/Models/*` | `FGPureGR5a149c98161e2b2e.gen`, `.mod` | `PureGR.gen`, `PureGR.mod` (bytes identical) |
| `FeynArts/Models/manifest.json` | `"ModelName":"FGPureGR5a149c98161e2b2e"` | `"ModelName":"PureGR"` |
| `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb` | `grModel = "FGPureGR5a149c98161e2b2e"` | `grModel = "PureGR"` |
| `FeynArtsIntegration/ModelBuilder.wl:122` | `name = "FGPureGR" <> StringTake[hash, 16]` | `name = "PureGR"` |
| `FeynArtsIntegration/Diagram.wl:12` | name must match `FGPureGR[0-9a-f]{16}` | name must match `[A-Za-z][A-Za-z0-9]{0,63}` |
| `FeynArts/Models/README.md`, `README.md` | old name in examples and tables | new name, with a note on the rename |

Snapshot pinning is retained, just not in the filename: `manifest.json` still records `SnapshotSHA256` (the full hash) and `ModelFileSHA256` (per-file hashes), and the validator still compares the manifest's `ModelName` with the supplied one and the recorded hashes with the actual file bytes (`Diagram.wl:20-32`).

The shape check is now deliberately looser in one respect — any identifier passes — but it is not vacuous: it rejects non-identifiers, and identity is carried by the manifest equality and byte-hash checks. Both negative cases are exercised below.

## Validation

**Notebook, 19 checks passed** (`check-notebook.log`, exit 0). The same harness as before, with the expected model updated to `PureGR`: every input and code cell evaluated, the packaged directory on `$ModelPath`, `$Model`/`$GenericModel` both `{"PureGR"}`, twenty unqualified names resolving to `FeynArts``, four diagrams with denominators `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, four scalar FORM results, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, the contact equal to the source, retained off-shell virtuality/longitudinal/trace terms, no `Transversality` option, no context prefixes and no benchmark path. Peak memory 318,828,544 bytes under a verified 1-GiB cgroup, zero swap, zero cap/OOM events.

**Wrapper, 13 checks passed** (`check-wrapper-model-name.log`, exit 0). A fresh model was built with the changed builder and driven through the public API:

- builder result carries `ModelName -> PureGR`, and the directory holds exactly `PureGR.gen`, `PureGR.mod`, `manifest.json`;
- the manifest records `PureGR`, still matches the actual file hashes, and still records a 64-character `SnapshotSHA256`;
- `FeynGravCreateAmplitude` accepts the built model: the three-point diagram is the exact source cubic, and the four-point tree gives one contact plus three exchanges with the contact equal to the exact source quartic;
- `ModelName -> "Bad-Name"` is rejected as `InvalidModel` (shape check);
- `ModelName -> "OtherName"` is rejected as `InvalidModel` (manifest disagrees).

Peak memory 318,812,160 bytes, zero cap/OOM events. The built model was left in `/tmp/fgfa-rename-check-e26fc66e-e05d-4df8-96ef-d6a98fc1559b` for inspection.

## Superseded run

The first attempt at the wrapper check is not recorded as evidence: it was launched with the working directory set to the repository root, so the shell redirect for its log file failed before the kernel started (`No such file or directory`, exit 1). Re-running from `Benchmark/` produced the log above.

## Historical references left untouched

Earlier evidence logs and probe scripts still use `FGPureGR5a149c98161e2b2e`, and many of them point at `/tmp` model directories from that period. They are records of runs made before the rename and were deliberately not rewritten. The same applies to `model-relocation-2026-10-12/README.md`, whose file listing predates the rename; a pointer note was added there. Those probe scripts would still satisfy the new identifier check, since the old name is a valid identifier.
