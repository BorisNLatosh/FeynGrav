# Simplifying the notebook initialisation

Bounded probe of simpler, documentation-grounded ways to load FeynGrav, FeynArts and the saved pure-GR model. Recorded 12 October 2026 with Wolfram 15.0.1, FeynCalc 10.2.1 and patched FeynArts 3.12. No package, model or library file was changed.

## What the long block actually does

| Statement | Purpose | Verdict |
| --- | --- | --- |
| `Get[FileNameJoin[{feynGravDirectory, "FeynGrav.wl"}]]` | locate the checkout explicitly | replaceable by `<< FeynGrav`` — the package resolves from `$Path` (`check-loading-options.log`: `<<FeynGrav`` succeeded on a bare kernel) |
| `FeynGrav`importGravitons[2]` | import graviton rules | **redundant**: package initialisation already calls `importGravitons[2]` (`FeynGrav.wl:1085-1096`); a quartic vertex evaluates with no explicit call and `FeynGravLibraryInformation[]` reports `GravitonVertex -> {1, 2}` |
| `$ModelPath = Prepend[DeleteCases[...], dir]` | make the model findable | `DeleteCases` only guards re-evaluation; `PrependTo` suffices, or the whole statement is unnecessary if the model file is named directly |
| `Reinitialize -> False` | skip a reload | unnecessary; default `True` merely re-reads the same file |
| `If[... =!= True, Print[...]; Abort[]]` | report a missing model | unnecessary; `InitializeModel` itself messages `InitializeModel::noopen` and aborts (`Initialize.m:63-65`, confirmed by `MISSING_MODEL_RESULT=$Aborted` in `check-simplified-init.log`) |

## One part must stay

`GenericModel -> grModel` is required. `InitializeModel[modname]` loads `modname.mod` **plus the generic model named by the `GenericModel` option, whose default is `"Lorentz"`** (FeynArts manual, `InitializeModel` table; `FeynArts/Initialize.m:235-301`). Our `.mod` file does not name its own generic model (`FGPureGR5a149c98161e2b2e.mod` contains only `M$ClassesDescription` and `M$CouplingMatrices`), so omitting the option would silently load the stock Lorentz `.gen` instead of the GR one.

`$ModelPath` is the documented search path: "a list of directory names which is searched for model files" (`FeynArts.m:812-814`), used as FeynArts' `$Path` when opening `name.gen`/`name.mod` (`FeynArts/Initialize.m:60-66`).

## Verified simpler forms

Recommended (no `$ModelPath` change; the model is named by file, which `FindFile` resolves):

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;

grModel = FileNameJoin[{NotebookDirectory[], "..", "Benchmark", "FeynArtsEvidence",
   "model-first-2026-10-12", "model", "FGPureGR5a149c98161e2b2e"}];
InitializeModel[grModel, GenericModel -> grModel];
```

Documented `$ModelPath` form, which keeps the short name used later by `InsertFields`:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;

grModelDirectory = FileNameJoin[{NotebookDirectory[], "..", "Benchmark",
   "FeynArtsEvidence", "model-first-2026-10-12", "model"}];
PrependTo[FeynArts`$ModelPath, grModelDirectory];
InitializeModel[grModel = "FGPureGR5a149c98161e2b2e", GenericModel -> grModel];
```

## Evidence

- `check-loading-options.wls/.log`: `<< FeynGrav`` works without an explicit `Get`; graviton orders `{1,2}` are present after a plain load; a model named by absolute file path initialises with `$ModelPath` untouched (`INITIALIZE_RESULT=True`, `MODELPATH_UNCHANGED`).
- `check-simplified-init-2.wls/.log`: the absolute-path route and the `$ModelPath`+short-name route produce **identical** converted diagrams (`ROUTES_AGREE=True`), four diagrams each. This is the decisive equivalence check.
- `check-contact-signs.wls/.log`: with the absolute/`$ModelPath` loading, the contact diagram equals the FeynGrav quartic source exactly (`CONTACT_DIRECT_SIGN_MATCH=True`, all 24 leg orders match) and the exchange routes are `{-p2+p3, -p2+p4, p3+p4}`, matching the already-validated notebook.
- `check-contact-convention.wls/.log`: re-evaluating `PrependTo[FeynArts`$ModelPath, dir]` twice duplicates the entry (cosmetically untidy but harmless); `InitializeModel` still returns `True` both times.

Two earlier comparison lines in these logs are **superseded test errors, not model defects**: `check-simplified-init.log` and `check-contact-convention.log` compared the contact against a source with the wrong outgoing signs (`OutgoingMomenta -> {p3,p4}` means the all-incoming vertex sees `-p3,-p4`, the convention used by the validated minimal example). `check-contact-signs.log` corrects this and passes.

## Caveat

Naming a model by absolute path is not stated in the FeynArts manual; it works because FeynArts opens model files through `FindFile`. With it, `$Model`/`$GenericModel` hold the long path, so any later cell written as `Model -> "FGPureGR5a149c98161e2b2e"` would not match. The notebook passes `Model -> grModel` (a variable), so both forms are safe. No commit or merge was made.
