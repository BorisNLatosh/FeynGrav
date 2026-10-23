(* ::Package:: *)

(* ::Title:: *)
(*CalcFormConverter*)


(* ::Text:: *)
(*FeynCalc \[LeftRightArrow] FORM conversion with optional ordinary Dirac algebra. Runtime definitions load separately. Ordinary propagators support algebraic numerator cancellation before grouping. Eligible version-one scalar coefficients expose reciprocal polynomials for bounded FORM rational simplification. The mapping is JSON data, not executable Wolfram Language source.*)


(* ::Section:: *)
(*Public interface*)


(* ::Input::Initialization:: *)
BeginPackage["CalcFormConverter`", {"FeynCalc`"}];


(* ::Text:: *)
(*Public functions and options.*)


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport::usage ="CalcFormExport[expr, file, opts] exports an exact supported FeynCalc expression, including ordinary Dirac words, SU(N) colour structures and traces and rank-four single-space Lorentz Eps tensors, to a complete FORM program and a reversible JSON mapping. Prints a readable file summary and returns an association with InputFile, MappingFile and ResultFile. Options: TensorDecomposition -> True, ReduceLoopCoefficients -> False, ReduceToScalarMasters -> True, DropScaleless -> False, Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False, DiracAlgebra -> Automatic, ColourAlgebra -> True. The generated program simplifies ordinary open chains and evaluates explicit DiracTrace expressions; DiracAlgebra -> False selects translation only. ColourAlgebra -> True (the default) or Automatic reduces fundamental SU(N) colour expressions; False preserves them and unevaluated SUNTrace. Dimension dependence can be compacted between prepared tensor stages and before general numerator cancellation. Propagator products are grouped automatically. Eligible small scalar coefficients undergo rational cancellation and numerator/denominator factorisation in FORM. Remaining version-one coefficients can simplify rational dependence on the symbolic Lorentz dimension alone; other cases retain common-factor extraction and momentum grouping. A direct massless monomial pass followed by a bounded denominator-basis stage cancels polynomial numerators against ordinary propagators before grouping, retaining constant terms and momentum routing. No Mathematica Simplify is called. Export launches no FORM process. With exactly one declared loop momentum and TensorDecomposition -> True, the generated program decomposes the integral into coefficient functions with FeynCalc normalisation I Pi^2. DropScaleless -> False retains formal scaleless integrals; True drops provably scaleless cases. TensorDecomposition -> False retains integrand algebra. ReduceLoopCoefficients -> True lowers repeated propagator powers in FORM where supported, also for saved LoopTensorCoefficient expressions without a declared loop. It may introduce inverse mass differences and Cayley determinants; singular and scaleless families can remain formal. Linear reduced results collect complete rational denominator products within each final integral multiplier in FORM. ReduceToScalarMasters -> True (the default) reduces supported tensor coefficients through boxes to rank-zero PaVe masters and includes required power reduction. False disables that stage. Scalar integrals are not analytically evaluated.";


(* ::Input::Initialization:: *)
CalcFormConverter`ColourAlgebra::usage = "ColourAlgebra selects SU(N) reduction (True or Automatic) or preservation (False).";

CalcFormConverter`DiracAlgebra::usage = "DiracAlgebra selects ordinary open-chain simplification and explicit trace evaluation in FORM (Automatic), or translation only (False).";

CalcFormConverter`CalcFormImport::usage = "CalcFormImport[resultFile, mappingFile] reads the dedicated result of an exported FORM program and reconstructs FeynCalc internal notation. It does not execute Wolfram Language source or run tensor/integral reduction. Epsilon results require the exported $LeviCivitaSign setting. Versions one through eight are supported. Version-eight master jobs retain the recorded integral or saved-coefficient normalisation and report unreduced coefficients. Version-seven saved-coefficient jobs preserve their existing normalisation. Version-six integral groups reconstruct PaVe or LoopTensorCoefficient with I Pi^2 normalisation and the recorded scaleless setting. New grouped results are read incrementally and retain propagator products and coefficient factors; the final expression and individual coefficients still require kernel memory. Version-four results distinguish processed open chains from preserved explicit traces; import performs scalar Casimir presentation for processed colour jobs but no Dirac or colour tensor reduction.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCheck::usage = "CalcFormCheck[opts] probes FORM on demand and returns availability, version and diagnostics. Options: FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> 10.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormInstall::usage = "CalcFormInstall[opts] explicitly attempts Debian/Ubuntu installation of missing FORM or TFORM using system authorization, then verifies the requested configuration. FORMThreads -> Automatic prefers up to eight TFORM workers and accepts serial FORM when TFORM is missing; explicit values above 1 require TFORM. It is never called automatically.";


(* ::Input::Initialization:: *)
CalcFormConverter`TensorDecomposition::usage = "TensorDecomposition is an option for CalcFormExport and CalcFormCalculate, default True. With exactly one declared LoopMomenta entry, interpret the expression as a one-loop integral and decompose its polynomial numerator into coefficient functions in FORM. False retains integrand algebra.";
CalcFormConverter`ReduceToScalarMasters::usage = "ReduceToScalarMasters is an option for CalcFormExport and CalcFormCalculate, default True. It reduces supported one-loop coefficients through boxes to scalar PaVe masters in FORM and includes necessary repeated-power reduction. False retains the existing coefficient basis. Exceptional or scaleless coefficients can remain formal. No scalar integral evaluation or dimensional expansion is performed.";
CalcFormConverter`ReduceLoopCoefficients::usage = "ReduceLoopCoefficients is an option for CalcFormExport and CalcFormCalculate, default False. Enabled ReduceToScalarMasters includes this reduction regardless of this option. With master reduction off, True lowers repeated propagator powers in one-loop coefficients through partial fractions and integration-by-parts identities executed in FORM. It can also process existing LoopTensorCoefficient expressions without declaring a loop momentum. For symbolic D, subfamilies with at most four distinct routes use ordinary PaVe coefficients where possible; scaleless, singular and unsupported families remain formal. Linear results are collected by final integral coefficient, with rational dimension coefficients simplified in FORM and shared amplitude blocks for large commuting expressions. Denominators inside those shared blocks are not globally collected. Generic reductions may introduce inverse mass differences and Cayley determinants. No analytic integral evaluation is performed.";
CalcFormConverter`LoopTensorCoefficient::usage = "LoopTensorCoefficient[indices, chords, massesSquared, powers, dimension] is an inert normalised one-loop tensor coefficient. Chords begin with zero and label denominators (q+chord)^2-massSquared; powers are positive integers. Paired zero labels denote metrics; {0} denotes a scalar. Empty families preserve polynomial vacuum integrals. The unnormalised integral carries I Pi^2. CalcFormCalculate[..., ReduceLoopCoefficients -> True] can lower supported repeated powers. ReduceToScalarMasters -> True (the default) additionally reduces supported coefficients through boxes to scalar PaVe masters.";

CalcFormConverter`CalcFormCalculate::usage = "CalcFormCalculate[expr, opts] exports, executes FORM and imports its result. For a binary equality lhs == rhs, it calculates lhs - rhs and compares the imported residual with zero; the result may remain symbolic. True and False are returned directly. Options include TensorDecomposition (default True), ReduceLoopCoefficients (default False), ReduceToScalarMasters (default True), DropScaleless (default False), Dimension, LoopMomenta, FORMExecutable, TimeConstraint, WorkingDirectory, KeepFiles, ShowTiming, ShowProgress, FORMThreads, DiracAlgebra and ColourAlgebra. Failed jobs are retained. Polynomial numerators are cancelled against ordinary propagators using a direct massless monomial pass followed by a bounded independent-basis stage; in integrand mode no momentum shifts or scaleless removal are applied. With exactly one LoopMomenta entry, enabled TensorDecomposition performs routing shifts and covariant decomposition in FORM and returns I Pi^2 times coefficient functions. DropScaleless -> False preserves formal scaleless terms; True drops demonstrably scaleless terms. ReduceLoopCoefficients -> True optionally reduces repeated-power coefficients in FORM, including saved coefficient expressions with empty LoopMomenta; it does not analytically evaluate integrals. ReduceToScalarMasters -> True (the default) includes necessary power reduction and reduces supported coefficients through boxes to scalar PaVe masters. It also accepts saved PaVe coefficients without interpreting their multipliers as integrands. Exceptional cases can remain formal with a partial-reduction message. Linear reduced results collect complete rational denominator products within each final integral multiplier in FORM. Otherwise empty LoopMomenta keeps integrand algebra; multiple loop momenta require TensorDecomposition -> False. Equality then means equality of integrals. Dimension dependence can be compacted between prepared tensor stages and before general cancellation. Surviving propagator products are grouped automatically and imported incrementally. Eligible small scalar coefficients are rationally simplified and factorised in FORM; remaining version-one coefficients can simplify rational dependence on the symbolic Lorentz dimension alone. Other cases retain the existing grouped path. No Mathematica Simplify is called. DiracAlgebra -> Automatic simplifies ordinary open chains and evaluates explicit DiracTrace expressions; False selects translation only. ColourAlgebra -> True (the default) or Automatic reduces fundamental SU(N) colour structures; False preserves colour objects and unevaluated SUNTrace.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMExecutable::usage = "FORMExecutable selects the FORM executable by name or path; Automatic searches the kernel PATH according to FORMThreads. An explicit executable with automatic threads runs with one worker.";


(* ::Input::Initialization:: *)
CalcFormConverter`WorkingDirectory::usage = "WorkingDirectory specifies an existing parent directory for unique FORM jobs; Automatic uses the system temporary directory.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowTiming::usage = "ShowTiming -> True prints FORM process elapsed wall-clock seconds, excluding the availability probe, export and import. Defaults to False; the returned expression is unchanged.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowProgress::usage = "ShowProgress -> True prints calculation stages and elapsed FORM execution time every ten seconds. It does not estimate a completion percentage. Defaults to False.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMThreads::usage = "FORMThreads accepts Automatic (default) or a positive integer for CalcFormCheck, CalcFormInstall and CalcFormCalculate. Automatic prefers TFORM with Min[8, $ProcessorCount] workers, falling back to serial FORM only when TFORM is missing. Invalid processor counts use one worker. Explicit executables use one worker with Automatic. Explicit 1 selects serial FORM; larger counts require TFORM without fallback or capping.";


(* ::Input::Initialization:: *)
CalcFormConverter`KeepFiles::usage = "KeepFiles -> True retains successful FORM calculation files and reports their location. Failed jobs are always retained.";


(* ::Subsection:: *)
(*Private initialization and export options*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Text:: *)
(*$InputFileName identifies the file currently being read by Get or Needs. Save its directory while loading this package so templates and FORMRuntime.wl can be located relative to it. Outside file loading, $InputFileName is normally an empty string; this initialization is intended for package loading, not independent notebook evaluation.*)


(* ::Input::Initialization:: *)
$moduleDirectory = DirectoryName[$InputFileName];


(* ::Text:: *)
(*Human-readable name of the converter. Plain string data; no evaluation semantics. It is the identity half of the pair used to stamp and later recognize generated artefacts.*)


(* ::Input::Initialization:: *)
$formatName = "CalcFormConverter";


(* ::Text:: *)
(*Version of the persisted mapping and result format. Changes to format semantics require a compatibility decision; changing this integer alone does not implement migration or support for older versions.*)


(* ::Input::Initialization:: *)
$formatVersion = 1;


(* ::Text:: *)
(*resultMarker[] builds the result-header marker, currently "CFC1". The template also includes it when writing the result. The JSON mapping stores Format and Version separately. SetDelayed (:=) reads $formatVersion when the helper is called.*)

(* ::Input::Initialization:: *)
resultMarker[version_ : $formatVersion] := "CFC" <> IntegerString[version];

(* Bound only during import; legacy jobs cannot introduce epsilon calls. *)
$epsilonImportFactor = None;


(* ::Text:: *)
(*intString[n] renders an exact integer as decimal digits with a minus sign only for negative values. IntegerString[Abs[n]] supplies the digits and <> joins the strings. The n_Integer pattern limits this definition to integer arguments; other calls remain unevaluated.*)
(**)
(*intString[7] -> "7"*)
(*intString[-7] -> "-7"*)
(*intString[0] -> "0"*)

(* ::Input::Initialization:: *)
intString[n_Integer] := If[n < 0, "-", ""] <> IntegerString[Abs[n]];


(* ::Text:: *)
(*Create a fresh symbol for the private Throw/Catch tag. Unique prevents accidental name collisions within the kernel; the tag is not secret or unforgeable. Conversion helpers throw failures with this tag so public entry points can return them without catching unrelated throws.*)

(* ::Input::Initialization:: *)
$failureTag = Unique["CalcFormFailure"];


(* ::Text:: *)
(*makeFailure constructs the package's one structured failure shape. Association Join is right-biased, so caller data may deliberately override MessageTemplate. throwFailure is the tagged nonlocal-exit form used by deep conversion and parser helpers. Runtime code and invalid-argument fallbacks return makeFailure directly.*)

(* ::Input::Initialization:: *)
makeFailure[tag_String, message_String, data_ : <||>] :=
	Failure[tag, Join[<|"MessageTemplate" -> message|>, data]];
throwFailure[tag_String, message_String, data_ : <||>] :=
	Throw[makeFailure[tag, message, data], $failureTag];


(* ::Text:: *)
(*Give CalcFormConverter`CalcFormExport its public option defaults. Options[symbol] = {...} is a plain assignment to the symbol's own Options value: it stores the list of default rules that OptionsPattern[] and OptionValue[] will consult when the function is called. The left-hand side is deliberately written fully qualified as CalcFormConverter`CalcFormExport, so the assignment lands on the package's symbol even if this header is evaluated from a context where the short name would resolve elsewhere (or create a stray Global` one).*)

(* ::Input::Initialization:: *)
Options[CalcFormConverter`CalcFormExport] = {
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False, CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    (*FeynCalc owns these two option symbols. Qualifying them preserves their historical identity while making it independent of context.*)
    FeynCalc`Dimension -> Automatic,
    FeynCalc`LoopMomenta -> {},
    System`OverwriteTarget -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True
};


(* ::Section:: *)
(*Load companion definitions without executing processes*)


(* ::Text:: *)
(*Load the private converter companions from the same directory as this package: the mapping, export and import layers first, then the specialised Dirac/colour, algebra and runtime definitions. $moduleDirectory was computed in the header with DirectoryName[$InputFileName], so this resolves relative to the FILE rather than to Directory[] (the current working directory) -- which is what makes the package relocatable and independent of how the caller's session happens to be positioned.*)

(* ::Input::Initialization:: *)
Get[FileNameJoin[{$moduleDirectory, "MappingEncoding.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "ExportAssembly.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "ImportReconstruction.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "DiracColour.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "DiracAlgebra.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "ColourAlgebra.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "CoefficientParser.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "PropagatorGroups.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "RationalCoefficients.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "DimensionCoefficients.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "PropagatorCancellation.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "StageSymmetry.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "TensorDecomposition.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "LoopCoefficientReduction.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "ScalarMasterReduction.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "LoopDenominatorGroups.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "SharedCoefficientReader.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "FORMRuntime.wl"}]];


(* ::Section:: *)
(*Unified calculation facade modules (API-01)*)


(* ::Text:: *)
(*The simplified public interface modules (FeynGravSimplify/Facade.wl and the restricted backends Contractor.wl and FORM.wl) are loaded once. The guard prevents a later Needs["CalcFormConverter`", ...] or explicit re-Get from re-running the module definitions and thereby resetting any user SetOptions on CalcFormConverter`FeynGravSimplify. Standalone converter loading declares the facade symbols and the FeynGrav`ContractorExecutable/ContractorThreads usage strings without loading FeynGrav.wl; no processes, downloads, stored libraries or models are touched here.*)


(* ::Input::Initialization:: *)
(* Load-once guard. The marker VALUE is read in a separate statement: reading a
   guard expression interns its marker symbol, so an existence test would be
   satisfied after a failed attempt. Each not-yet-loaded attempt first clears the
   partial facade/back-end definitions, so stale definitions cannot certify the
   current files, then records the five Get outcomes. The facade subsystem owns
   the readiness inventory in facadeReadyQ; the parent checks that the helper
   itself is actually defined and returns True. The helper inspects current
   callable definitions, including both backend dependencies, never symbol names.
   $Failed and $Aborted outcomes fail; the marker is set only when this attempt
   succeeds; failed or interrupted attempts leave it unset so repaired files can
   be retried. A successful load keeps its configured stores on later Needs or
   re-Get. facadeGuardLoaded and facadeLoadResults are short-lived scratch names,
   cleared after use. *)
facadeGuardLoaded = TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark];
If[!facadeGuardLoaded,
    ClearAll["FeynGrav`Simplification`Private`*"];
    ClearAll[CalcFormConverter`FeynGravSimplify, CalcFormConverter`FeynGravFORMExport,
        CalcFormConverter`FeynGravFORMCheck, CalcFormConverter`FeynGravFORMInstall,
        CalcFormConverter`FeynGravFORMImport];
    facadeLoadResults = {
        Get[FileNameJoin[{DirectoryName[$moduleDirectory], "FeynGravSimplify", "RestrictedCodec.wl"}]],
        Get[FileNameJoin[{DirectoryName[$moduleDirectory], "FeynGravSimplify", "Contractor.wl"}]],
        Get[FileNameJoin[{DirectoryName[$moduleDirectory], "FeynGravSimplify", "FORM.wl"}]],
        Get[FileNameJoin[{DirectoryName[$moduleDirectory], "FeynGravSimplify", "RestrictedCoordination.wl"}]],
        Get[FileNameJoin[{DirectoryName[$moduleDirectory], "FeynGravSimplify", "Facade.wl"}]]};
    If[
        AllTrue[facadeLoadResults, # =!= $Failed && # =!= $Aborted &] &&
        Length[DownValues[FeynGrav`Simplification`Private`facadeReadyQ]] > 0 &&
        TrueQ[FeynGrav`Simplification`Private`facadeReadyQ[]],
        FeynGrav`Simplification`Private`$FacadeLoadedMark = True]
];
ClearAll[facadeGuardLoaded, facadeLoadResults];


(* ::Text:: *)
(*The companion file supplies definitions in the current private context without launching processes. Reloading evaluates those definitions again. There is no structured missing-runtime error at this boundary: an unreadable file produces Get's own diagnostic. Restart the kernel after package updates to avoid retaining obsolete definitions.*)


(* ::Section:: *)
(*Close the package*)


(* ::Input::Initialization:: *)
End[];
EndPackage[];
