(* ::Package:: *)
(* Unified public calculation facade (API-01 implementation). Loaded by
   CalcFormConverter after the restricted-backend adapter modules. Defines the
   preferred commands in the CalcFormConverter` context: FeynGravSimplify,
   FeynGravFORMExport, FeynGravFORMImport, FeynGravFORMCheck and
   FeynGravFORMInstall. No process, download, library or model loading occurs at
   load time, and no option store is reset after its initial assignment.
   Migration from the unpublished donor branch: FeynGrav`FeynGravSimplify becomes
   CalcFormConverter`FeynGravSimplify; Method -> Automatic selects the full
   converter pipeline; the former restricted FORM selection is Method ->
   "FORMBenchmark" (the donor's Method -> "FORM" restricted meaning is not
   kept on this branch). See Documentation/ReleaseNotes-API01-Development.md. *)

(* Dependencies and ownership. Load order in CalcFormConverter.wl: codec,
   Contractor, FORM and coordination companions, then this file. This file owns
   four public option stores (FeynGravSimplify, FeynGravFORMExport,
   FeynGravFORMCheck and FeynGravFORMInstall); FeynGravFORMImport takes no
   options. It consumes the private failure/codec, backend preparation,
   simplify/reportProgress and runtime definitions, and owns facadeReadyQ[],
   the readiness inventory called by the parent loader after all Get outcomes.
   The restricted adapter consumes $showProgress and $backendLabel, which
   executeRestrictedBackend sets for the duration of one call. Loading creates
   no processes, downloads or stored libraries. *)

Begin["FeynGrav`Simplification`Private`"];

CalcFormConverter`FeynGravSimplify::usage = "FeynGravSimplify[expr, options] is the unified calculation entry point. Method -> Automatic (the default) and Method -> \"FORM\" select the full CalcFormConverter pipeline; Method -> \"Contractor\" and Method -> \"FORMBenchmark\" select the restricted external backends. No automatic fallback occurs. Wolfram performs validation, representation conversion and reconstruction only. Full-pipeline binary equalities are calculated as lhs - rhs; True and False pass through. Restricted methods accept only exact commuting scalar/Lorentz expressions and no equations. The listed full-pipeline settings are accepted on restricted methods only at their documented baseline; non-default values return a MethodCapability failure. Options: Method -> Automatic, ContractorExecutable -> Automatic, ContractorThreads -> 8, FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> Infinity, KeepFiles -> False, ShowProgress -> False, ShowTiming -> False, WorkingDirectory -> Automatic, TensorDecomposition -> True, ReduceLoopCoefficients -> False, ReduceToScalarMasters -> True, DropScaleless -> False, DiracAlgebra -> Automatic, ColourAlgebra -> True, Dimension -> Automatic, LoopMomenta -> {}. ContractorExecutable and ContractorThreads use the FeynGrav` context in standalone sessions. Failed restricted jobs are retained and reported; full-pipeline failure handling belongs to CalcFormCalculate.";
CalcFormConverter`FeynGravFORMExport::usage = "FeynGravFORMExport[expr, filename, options] is the preferred export wrapper for CalcFormExport, with its own independent option store starting from the same documented defaults. It forwards the resolved effective values of every supported option to CalcFormExport; unknown option names (including names added with SetOptions) return an InvalidOption failure without touching files. Options: TensorDecomposition -> True, ReduceLoopCoefficients -> False, ReduceToScalarMasters -> True, DropScaleless -> False, DiracAlgebra -> Automatic, ColourAlgebra -> True, Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False.";
CalcFormConverter`FeynGravFORMImport::usage = "FeynGravFORMImport[resultFile, mappingFile] is the preferred wrapper for CalcFormImport. It takes exactly the two file argument strings and no options; any additional argument returns an InvalidArguments failure.";
CalcFormConverter`FeynGravFORMCheck::usage = "FeynGravFORMCheck[options] is the preferred wrapper for CalcFormCheck with its own independent option store. It probes FORM on demand and returns availability, version and diagnostics. Options: FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> 10.";
CalcFormConverter`FeynGravFORMInstall::usage = "FeynGravFORMInstall[options] is the preferred wrapper for CalcFormInstall with its own independent option store. It explicitly attempts Debian/Ubuntu installation of missing FORM or TFORM using system authorization and then verifies the requested configuration. It is never called automatically. Option: FORMThreads -> Automatic.";

(* Immutable ordered defaults are the single source for the public store and for
   the name whitelist. Validation uses $simplifyOptionNames, never the mutable
   store, so keys injected with SetOptions or direct assignment are still
   rejected. Method/TimeConstraint are System` symbols (verified stable across
   loaders and callers); the Contractor pair stays in the FeynGrav` context by
   contract. FORMThreads defaults to Automatic (not the donor's 8). *)
$simplifyOptionDefaults = {
    System`Method -> Automatic, FeynGrav`ContractorExecutable -> Automatic,
    FeynGrav`ContractorThreads -> 8, System`TimeConstraint -> Infinity,
    CalcFormConverter`FORMExecutable -> Automatic, CalcFormConverter`FORMThreads -> Automatic,
    CalcFormConverter`KeepFiles -> False, CalcFormConverter`ShowProgress -> False,
    CalcFormConverter`ShowTiming -> False, CalcFormConverter`WorkingDirectory -> Automatic,
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {}
};
$simplifyOptionNames = First /@ $simplifyOptionDefaults;
(* One owner assigns the public store on first package loading; later Needs must
   not re-run this file (CalcFormConverter guards the module Gets). *)
Options[CalcFormConverter`FeynGravSimplify] = $simplifyOptionDefaults;

(* The restricted backends accept only these exact effective values of the
   full-pipeline settings (the converter-neutral baseline); anything else is a
   full-pipeline configuration request. Comparison is SameQ on resolved values,
   so equivalent explicit and stored requests behave identically. *)
$simplifyRestrictedBaseline = {
    FeynCalc`LoopMomenta -> {}, CalcFormConverter`TensorDecomposition -> True,
    CalcFormConverter`ReduceLoopCoefficients -> False, CalcFormConverter`ReduceToScalarMasters -> True,
    FeynCalc`DropScaleless -> False, CalcFormConverter`DiracAlgebra -> Automatic,
    CalcFormConverter`ColourAlgebra -> True, FeynCalc`Dimension -> Automatic,
    CalcFormConverter`WorkingDirectory -> Automatic};

(* Shared option-name validation for the facade commands. The allowed set is the
   documented constant, so keys added to a store by direct assignment are still
   rejected. Returns a Failure or Null; tags, keys and messages are the accepted
   public contract. *)
validateOptionNames[command_, optsList_List, allowed_List, withSupported_: False] := Module[
    {label = SymbolName[command], unknown, stored},
    unknown = Complement[DeleteDuplicates[First /@ Flatten[optsList]], allowed];
    If[unknown =!= {},
        Return[failure["InvalidOption", "Unknown option for " <> label <> ".",
            Join[<|"UnsupportedOptions" -> ToString[#, InputForm] & /@ unknown|>,
                If[withSupported, <|"SupportedOptions" -> ToString[#, InputForm] & /@ allowed|>, <||>]]]]];
    stored = Select[First /@ Options[command], !MemberQ[allowed, #] &];
    If[stored =!= {},
        Return[failure["InvalidOption", label <> " holds an unsupported stored option.",
            <|"UnsupportedStoredOptions" -> ToString[#, InputForm] & /@ stored|>]]];
    Null
];

(* Pure restricted capability policy. Iterates the ordered baseline, compares
   each already-resolved value with its documented baseline using SameQ, and
   builds the public refusal records in baseline order. Missing resolved keys
   surface as Missing[...] values (tested invariant); the public coordinator
   always supplies every baseline setting. No delayed option is evaluated
   here. *)
restrictedCapabilityRefusals[resolved_Association, baseline_List] := Select[
    Map[Function[baselineEntry,
        <|"Setting" -> First[baselineEntry],
          "RequestedValue" -> Lookup[resolved, First[baselineEntry]],
          "RestrictedBaseline" -> Last[baselineEntry]|>], baseline],
    Function[item, !SameQ[item["RequestedValue"], item["RestrictedBaseline"]]]];

(* Restricted execution and reporting owner. Receives already-resolved ordinary
   option rules from the coordinator and owns its result/status state, the
   dynamic progress scope and the single terminal total-time report. It does not
   resolve options, validate names or choose the method. The Block/WithCleanup
   shape is inherited from the adapter integration and is kept unchanged. *)
executeRestrictedBackend[expression_, method_String, adapterRules_List,
    showProgress_, showTiming_, startTime_] := Module[{result, status = "Aborted"},
    Block[{$showProgress = TrueQ[showProgress], $backendLabel = method},
        WithCleanup[Null,
            result = CheckAbort[simplify[expression, Sequence @@ adapterRules],
                status = "Aborted"; Abort[]];
            status = If[FailureQ[result], "Failed", "Complete"];
            result,
            reportProgress[status];
            If[TrueQ[showTiming],
                Print["FeynGravSimplify total (", method, ", ", status, "): ",
                    AbsoluteTime[] - startTime, " s elapsed (wall clock)."]]
        ]]
];

(* Public coordinator: validate names, select the path once, resolve only the
   values that path uses (each exactly once), refuse non-baseline restricted
   requests before any process starts, and hand the restricted call to
   executeRestrictedBackend. The full-pipeline forwarding list is intentional
   and explicit; CalcFormCalculate owns its remaining validation. *)
CalcFormConverter`FeynGravSimplify[expression_, opts : OptionsPattern[]] := Module[
    {method, restricted, bad, forward, started = AbsoluteTime[],
     resolvedSettings, refusals, timeConstraint, keepFiles, progressSetting,
     timingSetting, backendRules, adapterRules},
    bad = validateOptionNames[CalcFormConverter`FeynGravSimplify, {opts}, $simplifyOptionNames, True];
    If[bad =!= Null, Return[bad]];
    method = OptionValue[System`Method];
    If[!MemberQ[{Automatic, "FORM", "FORMBenchmark", "Contractor"}, method],
        Return[failure["UnsupportedMethod",
            "Method must be Automatic, \"FORM\", \"FORMBenchmark\" or \"Contractor\"."]]];
    restricted = MemberQ[{"FORMBenchmark", "Contractor"}, method];
    If[!restricted,
        (* Full pipeline: CalcFormCalculate owns every remaining validation. *)
        forward = {
            CalcFormConverter`TensorDecomposition -> OptionValue[CalcFormConverter`TensorDecomposition],
            CalcFormConverter`ReduceLoopCoefficients -> OptionValue[CalcFormConverter`ReduceLoopCoefficients],
            CalcFormConverter`ReduceToScalarMasters -> OptionValue[CalcFormConverter`ReduceToScalarMasters],
            FeynCalc`DropScaleless -> OptionValue[FeynCalc`DropScaleless],
            CalcFormConverter`DiracAlgebra -> OptionValue[CalcFormConverter`DiracAlgebra],
            CalcFormConverter`ColourAlgebra -> OptionValue[CalcFormConverter`ColourAlgebra],
            FeynCalc`Dimension -> OptionValue[FeynCalc`Dimension],
            FeynCalc`LoopMomenta -> OptionValue[FeynCalc`LoopMomenta],
            CalcFormConverter`FORMExecutable -> OptionValue[CalcFormConverter`FORMExecutable],
            System`TimeConstraint -> OptionValue[System`TimeConstraint],
            CalcFormConverter`WorkingDirectory -> OptionValue[CalcFormConverter`WorkingDirectory],
            CalcFormConverter`KeepFiles -> OptionValue[CalcFormConverter`KeepFiles],
            CalcFormConverter`ShowTiming -> OptionValue[CalcFormConverter`ShowTiming],
            CalcFormConverter`ShowProgress -> OptionValue[CalcFormConverter`ShowProgress],
            CalcFormConverter`FORMThreads -> OptionValue[CalcFormConverter`FORMThreads]};
        Return[CalcFormConverter`CalcFormCalculate[expression, Sequence @@ forward]]];
    (* Restricted methods: resolve the capability settings once and compare each
       with its documented baseline; the baseline list is the single source of
       truth for the accepted restricted configuration. *)
    resolvedSettings = <|
        FeynCalc`LoopMomenta -> OptionValue[FeynCalc`LoopMomenta],
        CalcFormConverter`TensorDecomposition -> OptionValue[CalcFormConverter`TensorDecomposition],
        CalcFormConverter`ReduceLoopCoefficients -> OptionValue[CalcFormConverter`ReduceLoopCoefficients],
        CalcFormConverter`ReduceToScalarMasters -> OptionValue[CalcFormConverter`ReduceToScalarMasters],
        FeynCalc`DropScaleless -> OptionValue[FeynCalc`DropScaleless],
        CalcFormConverter`DiracAlgebra -> OptionValue[CalcFormConverter`DiracAlgebra],
        CalcFormConverter`ColourAlgebra -> OptionValue[CalcFormConverter`ColourAlgebra],
        FeynCalc`Dimension -> OptionValue[FeynCalc`Dimension],
        CalcFormConverter`WorkingDirectory -> OptionValue[CalcFormConverter`WorkingDirectory]|>;
    refusals = restrictedCapabilityRefusals[resolvedSettings, $simplifyRestrictedBaseline];
    If[refusals =!= {},
        Return[failure["MethodCapability",
            "Method \"" <> method <> "\" accepts only the restricted baseline for the listed full-pipeline settings; they are ignored on restricted backends. Use Method -> \"FORM\" to request full-pipeline configuration.",
            <|"Method" -> method, "RefusedRequests" -> refusals|>]]];
    (* Shared runtime settings and only the selected backend's pair are resolved
       once and forwarded as ordinary rules. Unselected delayed values are not
       resolved here. Immediate rules still undergo normal argument evaluation
       before entering the facade. *)
    timeConstraint = OptionValue[System`TimeConstraint];
    keepFiles = OptionValue[CalcFormConverter`KeepFiles];
    progressSetting = OptionValue[CalcFormConverter`ShowProgress];
    timingSetting = OptionValue[CalcFormConverter`ShowTiming];
    If[method === "Contractor",
        backendRules = {FeynGrav`ContractorExecutable -> OptionValue[FeynGrav`ContractorExecutable],
            FeynGrav`ContractorThreads -> OptionValue[FeynGrav`ContractorThreads]},
        backendRules = {CalcFormConverter`FORMExecutable -> OptionValue[CalcFormConverter`FORMExecutable],
            CalcFormConverter`FORMThreads -> OptionValue[CalcFormConverter`FORMThreads]}];
    adapterRules = Join[{
        System`Method -> method,
        System`TimeConstraint -> timeConstraint,
        CalcFormConverter`KeepFiles -> keepFiles,
        CalcFormConverter`ShowProgress -> progressSetting,
        CalcFormConverter`ShowTiming -> timingSetting}, backendRules];
    executeRestrictedBackend[expression, method, adapterRules, progressSetting, timingSetting, started]
];
CalcFormConverter`FeynGravSimplify[___] := failure["InvalidArguments",
    "Use FeynGravSimplify[expression, options]."];

(* Export wrapper: independent store, owner defaults, strict name validation,
   one resolution per supported option, forwarding of ordinary resolved rules. *)
$exportWrapperDefaults = {
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {}, System`OverwriteTarget -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True};
(* Membership is derived from the immutable default store. The validator uses
   membership only and this wrapper does not emit SupportedOptions, so the list
   order is not observable in its public failure payloads. *)
$exportWrapperOptionNames = First /@ $exportWrapperDefaults;
Options[CalcFormConverter`FeynGravFORMExport] = $exportWrapperDefaults;
CalcFormConverter`FeynGravFORMExport[expression_, file_, opts : OptionsPattern[]] := Module[
    {bad},
    bad = validateOptionNames[CalcFormConverter`FeynGravFORMExport, {opts}, $exportWrapperOptionNames];
    If[bad =!= Null, Return[bad]];
    If[!StringQ[file],
        Return[failure["InvalidArguments",
            "Use FeynGravFORMExport[expression, filename, options] with a filename string."]]];
    CalcFormConverter`CalcFormExport[expression, file, Sequence @@ {
        CalcFormConverter`TensorDecomposition -> OptionValue[CalcFormConverter`TensorDecomposition],
        CalcFormConverter`ReduceLoopCoefficients -> OptionValue[CalcFormConverter`ReduceLoopCoefficients],
        CalcFormConverter`ReduceToScalarMasters -> OptionValue[CalcFormConverter`ReduceToScalarMasters],
        FeynCalc`DropScaleless -> OptionValue[FeynCalc`DropScaleless],
        CalcFormConverter`DiracAlgebra -> OptionValue[CalcFormConverter`DiracAlgebra],
        CalcFormConverter`ColourAlgebra -> OptionValue[CalcFormConverter`ColourAlgebra],
        FeynCalc`Dimension -> OptionValue[FeynCalc`Dimension],
        FeynCalc`LoopMomenta -> OptionValue[FeynCalc`LoopMomenta],
        System`OverwriteTarget -> OptionValue[System`OverwriteTarget]}]
];
CalcFormConverter`FeynGravFORMExport[___] := failure["InvalidArguments",
    "Use FeynGravFORMExport[expression, filename, options]."];

(* Import wrapper: exactly two file arguments, no options, no option symbols. *)
CalcFormConverter`FeynGravFORMImport[resultFile_, mappingFile_] :=
    If[StringQ[resultFile] && StringQ[mappingFile],
        CalcFormConverter`CalcFormImport[resultFile, mappingFile],
        failure["InvalidArguments",
            "Use FeynGravFORMImport[resultFile, mappingFile] with two file path strings."]];
CalcFormConverter`FeynGravFORMImport[___] := failure["InvalidArguments",
    "Use FeynGravFORMImport[resultFile, mappingFile] with two file path strings."];

(* Check wrapper: independent store mirroring the CalcFormCheck defaults. *)
$checkWrapperDefaults = {CalcFormConverter`FORMExecutable -> Automatic,
    CalcFormConverter`FORMThreads -> Automatic, System`TimeConstraint -> 10};
$checkWrapperOptionNames = First /@ $checkWrapperDefaults;
Options[CalcFormConverter`FeynGravFORMCheck] = $checkWrapperDefaults;
CalcFormConverter`FeynGravFORMCheck[opts : OptionsPattern[]] := Module[
    {bad, formExecutable, formThreads, timeConstraint},
    bad = validateOptionNames[CalcFormConverter`FeynGravFORMCheck, {opts}, $checkWrapperOptionNames];
    If[bad =!= Null, Return[bad]];
    formExecutable = OptionValue[CalcFormConverter`FORMExecutable];
    formThreads = OptionValue[CalcFormConverter`FORMThreads];
    timeConstraint = OptionValue[System`TimeConstraint];
    CalcFormConverter`CalcFormCheck[Sequence @@ {
        CalcFormConverter`FORMExecutable -> formExecutable,
        CalcFormConverter`FORMThreads -> formThreads,
        System`TimeConstraint -> timeConstraint}]
];
CalcFormConverter`FeynGravFORMCheck[___] := failure["InvalidArguments", "Use FeynGravFORMCheck[options]."];

(* Install wrapper: independent store mirroring the CalcFormInstall defaults. *)
$installWrapperDefaults = {CalcFormConverter`FORMThreads -> Automatic};
$installWrapperOptionNames = First /@ $installWrapperDefaults;
Options[CalcFormConverter`FeynGravFORMInstall] = $installWrapperDefaults;
CalcFormConverter`FeynGravFORMInstall[opts : OptionsPattern[]] := Module[
    {bad, formThreads},
    bad = validateOptionNames[CalcFormConverter`FeynGravFORMInstall, {opts}, $installWrapperOptionNames];
    If[bad =!= Null, Return[bad]];
    formThreads = OptionValue[CalcFormConverter`FORMThreads];
    CalcFormConverter`CalcFormInstall[CalcFormConverter`FORMThreads -> formThreads]
];
CalcFormConverter`FeynGravFORMInstall[___] := failure["InvalidArguments", "Use FeynGravFORMInstall[options]."];

(* Preserve the documented Contractor option-symbol identities when the main
   package is not loaded; usage declarations only, no stores. Main-package usage
   resolves them normally through the context path. *)
FeynGrav`ContractorExecutable::usage = "ContractorExecutable is an option of FeynGravSimplify selecting the Contractor backend executable: Automatic locates an installed contractor next to the package or on the kernel's PATH; a nonempty string names or gives the path of the executable (an explicit selection never falls back). It is resolved only for Method -> \"Contractor\"; unselected delayed values stay inactive. Ordinary argument evaluation still applies.";
FeynGrav`ContractorThreads::usage = "ContractorThreads is an option of FeynGravSimplify selecting the Contractor worker count: Automatic and positive integers are accepted; the effective bound follows the installed Contractor. It is resolved only for Method -> \"Contractor\"; unselected delayed values stay inactive. Ordinary argument evaluation still applies.";

(* Readiness inventory owned by the facade subsystem. The parent loader calls
   this only after all five Get outcomes are non-failed/non-aborted and after
   checking this function itself has a DownValue. It inspects current callable
   definitions, including both backend dependencies, rather than symbol names or
   the load marker. It is deliberately defined last so a truncated or empty
   Facade file cannot provide a readiness shortcut. *)
facadeReadyQ[] := Module[{required},
    required = {
        failure, encode, decode,
        resolveExecutable, contractorPreparation,
        formConfiguration, formProgram, formDecode, formPreparation,
        runJob, reportProgress, simplify,
        validateOptionNames, restrictedCapabilityRefusals,
        executeRestrictedBackend,
        CalcFormConverter`FeynGravSimplify, CalcFormConverter`FeynGravFORMExport,
        CalcFormConverter`FeynGravFORMCheck, CalcFormConverter`FeynGravFORMInstall,
        CalcFormConverter`FeynGravFORMImport};
    Length[Options[CalcFormConverter`FeynGravSimplify]] > 0 &&
    AllTrue[required, Length[DownValues[#]] > 0 &]
];

End[];
