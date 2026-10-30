(* Fresh-kernel API-01 facade suite. Real FORM runs with small bounded
   expressions only; no large workload and no benchmark. Checks forwarding
   equivalence with the owner commands, the capability policy, once-per-call
   resolution, duplicates, delayed values and the new wrappers. Adapted from the
   donor-branch test style; see the extraction provenance record. *)
$FeynCalcStartupMessages=False;Get["FeynCalc`"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"Completion.wl"}]];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"FeynGrav.wl"}]];
passed=0;failed=0;
check[name_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",name]];
tag[x_]:=If[FailureQ[x],x[[1]],None];
removeFailure[x_]:=If[FailureQ[x]&&StringQ[Lookup[x[[2]],"JobDirectory",None]]&&DirectoryQ[x[[2]]["JobDirectory"]],DeleteDirectory[x[[2]]["JobDirectory"],DeleteContents->True]];
equal[a_,b_]:=!FailureQ[a]&&!FailureQ[b]&&TrueQ[Together[ExpandScalarProduct[Contract[FeynAmpDenominatorExplicit[FCI[PaVeToABCD[a]-PaVeToABCD[b]]]]]]===0];
(* Per-call emitted-diagnostic capture for the four intentional loop-vacuum
   calls. Only $Messages is redirected; System`Message is not replaced, so
   internal Quiet/Check behaviour is preserved. $MessageList appends each
   emitted name during the current input line; the before/after suffix is
   therefore chronological and prefix-compatible. It is not reset. General::stop
   is not switched off or accepted. *)
FacadeCapture::computed = "computed diagnostic `1`.";
FacadeCapture::expected = "expected diagnostic `1`.";
FacadeCapture::unexpected = "unexpected diagnostic `1`.";
FacadeCapture::extra = "extra diagnostic `1`.";
FacadeCapture::hidden = "hidden diagnostic.";
partialDiagnosticText = "CalcFormImport::partialmasters: Scalar-master reduction is partial: 1 distinct coefficient(s) remain formal or unreduced (exceptional kinematics, preserved scaleless integrals, unsupported dimension or topology).";
SetAttributes[captureFacadeMessages,HoldAll];
captureFacadeMessages[expr_]:=Module[{path,stream,value,messages,before,diagnosticText},
  path=FileNameJoin[{$TemporaryDirectory,"fg-facade-messages-"<>CreateUUID[]<>".log"}];
  stream=OpenWrite[path];
  before=Length[$MessageList];
  Block[{$Messages={stream}},value=expr];
  messages=Drop[$MessageList,before];
  Close[stream];
  diagnosticText=Quiet[Check[Import[path,"Text"],$Failed]];
  DeleteFile[path];
  <|"Value"->value,"Messages"->messages,"DiagnosticText"->diagnosticText|>
];
normaliseDiagnosticText[text_String]:=StringRiffle[StringSplit[text]," "];
captureExpectationQ[capture_Association,message_,text_String]:=
  Length[capture["Messages"]]===1&&First[capture["Messages"]]===message&&
  StringQ[capture["DiagnosticText"]]&&normaliseDiagnosticText[capture["DiagnosticText"]]===normaliseDiagnosticText[text];
partialCaptureQ[capture_Association]:=
  captureExpectationQ[capture,HoldForm[CalcFormConverter`CalcFormImport::partialmasters],
    partialDiagnosticText];
checkPartialCapture[label_,capture_]:=(
  Print["CAPTURE partialmasters ",label," ",ToString[capture,InputForm]];
  check[label<>" partialmasters",AssociationQ[capture]&&partialCaptureQ[capture]]);
(* The warning is verified once. Other result checks suppress only that known
   message; every other emitted diagnostic or raw text still fails. *)
SetAttributes[captureWithoutPartialWarning,HoldAll];
captureWithoutPartialWarning[expr_]:=captureFacadeMessages[
  Quiet[expr,CalcFormConverter`CalcFormImport::partialmasters]];
checkCleanCapture[label_,capture_]:=(
  Print["CAPTURE quiet partialmasters ",label," ",ToString[capture,InputForm]];
  check[label<>" no unexpected diagnostics",AssociationQ[capture]&&
    capture["Messages"]==={}&&StringQ[capture["DiagnosticText"]]&&
    StringTrim[capture["DiagnosticText"]]===""]);
(* Small in-suite fixtures for the capture mechanics. These execute in the same
   Facade kernel, not a separate launch. The text stream exposes formatted
   arguments; $MessageList exposes emitted names only. *)
captureComputed=captureFacadeMessages[Module[{unresolved={1,2,3}},
  Message[FacadeCapture::computed,Length[unresolved]];7]];
Print["CAPTURE fixture computed ",ToString[captureComputed,InputForm]];
check["partial capture computed integer",
  captureComputed["Value"]===7&&
  captureExpectationQ[captureComputed,HoldForm[FacadeCapture::computed],
    "FacadeCapture::computed: computed diagnostic 3."]];
captureUnexpected=captureFacadeMessages[Module[{},Message[FacadeCapture::unexpected,9];8]];
Print["CAPTURE fixture unexpected ",ToString[captureUnexpected,InputForm]];
check["partial capture unexpected identity",
  captureUnexpected["Value"]===8&&
  captureUnexpected["Messages"]==={HoldForm[FacadeCapture::unexpected]}&&
  StringTrim[captureUnexpected["DiagnosticText"]]===
    "FacadeCapture::unexpected: unexpected diagnostic 9."&&
  !captureExpectationQ[captureUnexpected,HoldForm[FacadeCapture::computed],
    "FacadeCapture::computed: computed diagnostic 3."]];
captureExtra=captureFacadeMessages[Module[{},
  Message[FacadeCapture::expected,3];Message[FacadeCapture::extra,4];9]];
Print["CAPTURE fixture extra ",ToString[captureExtra,InputForm]];
check["partial capture extra message",
  captureExtra["Value"]===9&&
  captureExtra["Messages"]==={HoldForm[FacadeCapture::expected],HoldForm[FacadeCapture::extra]}&&
  !captureExpectationQ[captureExtra,HoldForm[FacadeCapture::expected],
    "FacadeCapture::expected: expected diagnostic 3."]];
captureAdditionalText=captureFacadeMessages[Module[{stream=First[$Messages]},
  Message[FacadeCapture::expected,3];WriteString[stream,"additional raw text\n"];10]];
Print["CAPTURE fixture additional text ",ToString[captureAdditionalText,InputForm]];
check["partial capture additional raw text",
  captureAdditionalText["Value"]===10&&
  captureAdditionalText["Messages"]==={HoldForm[FacadeCapture::expected]}&&
  !captureExpectationQ[captureAdditionalText,HoldForm[FacadeCapture::expected],
    "FacadeCapture::expected: expected diagnostic 3."]&&
  StringContainsQ[captureAdditionalText["DiagnosticText"],"additional raw text"]];
captureSuppressed=captureFacadeMessages[Module[{},
  Quiet[Message[FacadeCapture::hidden],FacadeCapture::hidden];11]];
Print["CAPTURE fixture suppressed ",ToString[captureSuppressed,InputForm]];
check["partial capture suppressed not emitted",
  captureSuppressed["Value"]===11&&captureSuppressed["Messages"]==={}&&
  captureSuppressed["DiagnosticText"]===""];
captureCheck=captureFacadeMessages[Module[{value},
  value=Check[Message[FacadeCapture::computed,7];"after","caught",{FacadeCapture::computed}];
  value]];
Print["CAPTURE fixture check ",ToString[captureCheck,InputForm]];
check["partial capture preserves Check",
  captureCheck["Value"]==="caught"&&
  captureExpectationQ[captureCheck,HoldForm[FacadeCapture::computed],
    "FacadeCapture::computed: computed diagnostic 7."]];
check["diagnostic whitespace normalisation",
 captureExpectationQ[<|"Messages"->{HoldForm[FacadeCapture::expected]},
 "DiagnosticText"->"  FacadeCapture::expected: expected\n  diagnostic 3.  "|>,
 HoldForm[FacadeCapture::expected],"FacadeCapture::expected: expected diagnostic 3."]];
check["diagnostic normalisation preserves extra words",
 !captureExpectationQ[<|"Messages"->{HoldForm[FacadeCapture::expected]},
 "DiagnosticText"->"FacadeCapture::expected: expected diagnostic 3. extra"|>,
 HoldForm[FacadeCapture::expected],"FacadeCapture::expected: expected diagnostic 3."]];
streamSnapshot=Streams[];
storeSymbols={CalcFormConverter`CalcFormCalculate,CalcFormConverter`CalcFormCheck,
    CalcFormConverter`CalcFormInstall,CalcFormConverter`CalcFormExport,CalcFormConverter`CalcFormImport};
storeSnapshot=Options /@ storeSymbols;
(* Forwarding equivalence is checked on genuinely supported inputs: both the
   facade and the owner command must succeed, return the documented result and
   agree algebraically. Failure-tag parity is reserved for intentional invalid
   fixtures below; a matching failure therefore fails a supported-success check. *)
fixture=Environment["FG_CONTRACTOR_TEST_FIXTURE"];
(* Fail-fast: an unloaded facade or missing fixture must stop the suite instead
   of counting unevaluated calls as success. Only qualified names and string
   patterns appear in this expression. *)
facadeLoaded=Names["CalcFormConverter`FeynGravSimplify"]=!={}&&
  Length[Options[CalcFormConverter`FeynGravSimplify]]===19&&
  Length[DownValues[CalcFormConverter`FeynGravSimplify]]>=2&&
  Length[Options[CalcFormConverter`FeynGravFORMExport]]===9&&
  Length[Options[CalcFormConverter`FeynGravFORMCheck]]===3&&
  Length[Options[CalcFormConverter`FeynGravFORMInstall]]===1&&
  Length[DownValues[CalcFormConverter`FeynGravFORMImport]]>=2&&
  Names["Global`FeynGravSimplify"]==={}&&StringQ[fixture]&&FileExistsQ[fixture];
If[!TrueQ[facadeLoaded],
  Print["FAIL-FAST: facade definitions, stores or fixture missing; aborting."];
  Quit[1]];
(* Accepted contract literals copied from the accepted Facade.wl snapshot
   (sha256 6fe8b052…; evidence 2026-10-20-api01-maintainability/snapshot/Facade.wl).
   API-03 adds the explicit PaVeToABCD -> True entry and name below.
   They are the independent reference: expectations must not be derived from the
   implementation under test. *)
acceptedSimplifyDefaults = {
    System`Method -> Automatic, FeynGrav`ContractorExecutable -> Automatic,
    FeynGrav`ContractorThreads -> 8, System`TimeConstraint -> Infinity,
    CalcFormConverter`FORMExecutable -> Automatic, CalcFormConverter`FORMThreads -> Automatic,
    CalcFormConverter`KeepFiles -> False, CalcFormConverter`ShowProgress -> False,
    CalcFormConverter`ShowTiming -> False, CalcFormConverter`WorkingDirectory -> Automatic,
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {}, FeynCalc`PaVeToABCD -> True};
acceptedSimplifyOptionNames = {System`Method, FeynGrav`ContractorExecutable,
    FeynGrav`ContractorThreads, System`TimeConstraint, CalcFormConverter`FORMExecutable,
    CalcFormConverter`FORMThreads, CalcFormConverter`KeepFiles,
    CalcFormConverter`ShowProgress, CalcFormConverter`ShowTiming,
    CalcFormConverter`WorkingDirectory, CalcFormConverter`TensorDecomposition,
    CalcFormConverter`ReduceLoopCoefficients, CalcFormConverter`ReduceToScalarMasters,
    FeynCalc`DropScaleless, CalcFormConverter`DiracAlgebra,
    CalcFormConverter`ColourAlgebra, FeynCalc`Dimension, FeynCalc`LoopMomenta, FeynCalc`PaVeToABCD};
acceptedRestrictedBaseline = {
    FeynCalc`LoopMomenta -> {}, CalcFormConverter`TensorDecomposition -> True,
    CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True,
    FeynCalc`Dimension -> Automatic, CalcFormConverter`WorkingDirectory -> Automatic};
acceptedExportDefaults = {
    CalcFormConverter`TensorDecomposition -> True,
    CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {},
    System`OverwriteTarget -> False, CalcFormConverter`DiracAlgebra -> Automatic,
    CalcFormConverter`ColourAlgebra -> True};
acceptedExportOptionNames = {CalcFormConverter`TensorDecomposition,
    CalcFormConverter`ReduceLoopCoefficients, CalcFormConverter`ReduceToScalarMasters,
    FeynCalc`DropScaleless, CalcFormConverter`DiracAlgebra,
    CalcFormConverter`ColourAlgebra, FeynCalc`Dimension, FeynCalc`LoopMomenta,
    System`OverwriteTarget};
acceptedCheckDefaults = {CalcFormConverter`FORMExecutable -> Automatic,
    CalcFormConverter`FORMThreads -> Automatic, System`TimeConstraint -> 10};
acceptedCheckOptionNames = {CalcFormConverter`FORMExecutable,
    CalcFormConverter`FORMThreads, System`TimeConstraint};
acceptedInstallDefaults = {CalcFormConverter`FORMThreads -> Automatic};
acceptedInstallOptionNames = {CalcFormConverter`FORMThreads};
check["simplify defaults with API-03",Options[CalcFormConverter`FeynGravSimplify]===acceptedSimplifyDefaults];
check["simplify names with API-03",FeynGrav`Simplification`Private`$simplifyOptionNames===acceptedSimplifyOptionNames];
check["restricted baseline unchanged",FeynGrav`Simplification`Private`$simplifyRestrictedBaseline===acceptedRestrictedBaseline];
check["export defaults unchanged",Options[CalcFormConverter`FeynGravFORMExport]===acceptedExportDefaults];
(* The export validator uses membership only and emits no SupportedOptions
   payload, so its allowed-name order is not observable. Compare the set. *)
check["export names unchanged",Sort[FeynGrav`Simplification`Private`$exportWrapperOptionNames]===Sort[acceptedExportOptionNames]];
check["check defaults unchanged",Options[CalcFormConverter`FeynGravFORMCheck]===acceptedCheckDefaults];
check["check names unchanged",FeynGrav`Simplification`Private`$checkWrapperOptionNames===acceptedCheckOptionNames];
check["install defaults unchanged",Options[CalcFormConverter`FeynGravFORMInstall]===acceptedInstallDefaults];
check["install names unchanged",FeynGrav`Simplification`Private`$installWrapperOptionNames===acceptedInstallOptionNames];
independenceStores=Options /@ {CalcFormConverter`FeynGravSimplify,CalcFormConverter`FeynGravFORMExport,
    CalcFormConverter`FeynGravFORMCheck,CalcFormConverter`FeynGravFORMInstall};
SetOptions[CalcFormConverter`FeynGravFORMExport,CalcFormConverter`TensorDecomposition -> False];
check["store independence",
  Options[CalcFormConverter`FeynGravSimplify]===independenceStores[[1]]&&
  Options[CalcFormConverter`FeynGravFORMCheck]===independenceStores[[3]]&&
  Options[CalcFormConverter`FeynGravFORMInstall]===independenceStores[[4]]&&
  (CalcFormConverter`TensorDecomposition /. Options[CalcFormConverter`FeynGravFORMExport])===False];
Options[CalcFormConverter`FeynGravFORMExport]=independenceStores[[2]];
capabilityProbe=FeynGrav`Simplification`Private`restrictedCapabilityRefusals[
  <|FeynCalc`LoopMomenta -> {},CalcFormConverter`TensorDecomposition -> True,
    CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True,FeynCalc`DropScaleless -> False,
    CalcFormConverter`DiracAlgebra -> Automatic,CalcFormConverter`ColourAlgebra -> True,
    FeynCalc`Dimension -> Automatic|>,
  FeynGrav`Simplification`Private`$simplifyRestrictedBaseline];
check["capability reports missing resolved key",Length[capabilityProbe]===1&&
  capabilityProbe[[1]]["Setting"]===CalcFormConverter`WorkingDirectory&&
  MissingQ[capabilityProbe[[1]]["RequestedValue"]]];
supportedFixtures={
  {"tensor chain",MTD[mu,nu] FVD[p,mu] FVD[q,nu],{},SPD[p,q]},
  {"dirac square",GAD[mu].GAD[mu],{},D},
  {"dirac trace",DiracTrace[GAD[mu].GAD[mu]],{},4 D},
  {"colour Casimir",SUNT[a,a],{},CF}};
Do[Module[{facadeResult,ownerResult},
   facadeResult=FeynGravSimplify[item[[2]],Sequence@@item[[3]],System`Method -> "FORM",
     CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20];
   ownerResult=CalcFormCalculate[item[[2]],Sequence@@item[[3]],
     CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20];
   check[item[[1]]<>" supported success",!FailureQ[facadeResult]&&!FailureQ[ownerResult]];
   check[item[[1]]<>" supported known answer",
     If[item[[1]]==="tensor chain",
       equal[facadeResult,item[[4]]]&&equal[ownerResult,item[[4]]],
       TrueQ[facadeResult===item[[4]]]&&TrueQ[ownerResult===item[[4]]]]];
   check[item[[1]]<>" supported agreement",equal[facadeResult,ownerResult]]],
 {item,supportedFixtures}];
(* Verify the partial-reduction diagnostic once; retain all four result checks.
   Separate file expressions do not reset suppression in this script runner. *)
loopVacuum=I Pi^2 LoopTensorCoefficient[{0},{},{},{},D];
loopFacadeCapture=captureFacadeMessages[FeynGravSimplify[1,
  FeynCalc`LoopMomenta->{q},System`Method -> "FORM",
  CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]];
loopOwnerCapture=captureWithoutPartialWarning[CalcFormCalculate[1,
  FeynCalc`LoopMomenta->{q},CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]];
loopFacadeResult=loopFacadeCapture["Value"];
loopOwnerResult=loopOwnerCapture["Value"];
checkPartialCapture["loop vacuum facade",loopFacadeCapture];
checkCleanCapture["loop vacuum owner",loopOwnerCapture];
check["loop vacuum scalar supported success",!FailureQ[loopFacadeResult]&&!FailureQ[loopOwnerResult]];
check["loop vacuum scalar supported known answer",
  TrueQ[loopFacadeResult===loopVacuum]&&TrueQ[loopOwnerResult===loopVacuum]];
check["loop vacuum scalar supported agreement",
  !FailureQ[loopFacadeResult]&&!FailureQ[loopOwnerResult]&&TrueQ[loopFacadeResult===loopOwnerResult]];
(* Intentional invalid fixtures: both paths must fail and the failure tags must
   match. This is failure parity only, never evidence of supported equivalence. *)
Do[Module[{facadeResult,ownerResult},
   facadeResult=Quiet[FeynGravSimplify[item[[2]],Sequence@@item[[3]],System`Method -> "FORM",
     CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]];
   ownerResult=Quiet[CalcFormCalculate[item[[2]],Sequence@@item[[3]],
     CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]];
   check[item[[1]]<>" failure parity",FailureQ[facadeResult]&&FailureQ[ownerResult]&&
     tag[facadeResult]===tag[ownerResult]]],
 {item,
  {{"unsupported loop configuration",1,{FeynCalc`LoopMomenta -> {q,k}}},
   {"hidden loop dependence",1/(SPD[q]+1),{FeynCalc`LoopMomenta -> {q}}}}}];
(* Equality and Boolean semantics preserved on the full pipeline. *)
check["zero equality resolves",FeynGravSimplify[MTD[mu,mu] == System`D,System`Method -> "FORM",
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]===True];
equalityResult=FeynGravSimplify[MTD[mu,mu] == 4,System`Method -> "FORM",
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20];
check["nonzero equality remains",Head[equalityResult]===Equal];
check["boolean passthrough",FeynGravSimplify[True,System`Method -> "FORM"]===True];
check["restricted boolean rejected consistently",tag[FeynGravSimplify[True,System`Method -> "FORMBenchmark"]]===tag[FeynGravSimplify[False,System`Method -> "Contractor"]]];
(* Restricted eligibility with explicit and stored baselines. *)
check["explicit baselines eligible",FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    FeynCalc`LoopMomenta -> {},CalcFormConverter`TensorDecomposition -> True,
    CalcFormConverter`ReduceLoopCoefficients -> False,CalcFormConverter`ReduceToScalarMasters -> True,
    FeynCalc`DropScaleless -> False,FeynCalc`Dimension -> Automatic,
    CalcFormConverter`DiracAlgebra -> Automatic,CalcFormConverter`ColourAlgebra -> True,
    CalcFormConverter`WorkingDirectory -> Automatic,CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]===D];
check["baseline stays eligible when stored",FeynGravSimplify[MTD[mu,mu],
    System`Method -> "FORMBenchmark",CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10]===D];
(* WorkingDirectory is honoured by the full pipeline. *)
workingDirectory=CreateDirectory[];
check["working directory honoured",equal[FeynGravSimplify[MTD[mu,nu] FVD[p,mu],
    System`Method -> "FORM",CalcFormConverter`WorkingDirectory -> workingDirectory,
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20],FVD[p,nu]]];
check["working directory left clean",DirectoryQ[workingDirectory]&&FileNames["*",workingDirectory]==={}];
DeleteDirectory[workingDirectory];
(* Refusal detail: every monitored setting that deviates is listed once. *)
refusal=FeynGravSimplify[MTD[mu,mu],System`Method -> "Contractor",FeynCalc`LoopMomenta -> {q},
    CalcFormConverter`ReduceToScalarMasters -> False];
check["refusal shape",FailureQ[refusal]&&refusal[[1]]==="MethodCapability"&&
    Length[refusal[[2]]["RefusedRequests"]]===2&&
    AllTrue[{FeynCalc`LoopMomenta,CalcFormConverter`ReduceToScalarMasters},
        MemberQ[Lookup[refusal[[2]]["RefusedRequests"],"Setting"],#]&]];
Do[
 check[item[[1]]<>" refusal",FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
     item[[2]]][[1]]==="MethodCapability"],
 {item,
  {{"dimension",FeynCalc`Dimension -> 4},
   {"working directory",CalcFormConverter`WorkingDirectory -> $TemporaryDirectory},
   {"dirac algebra",CalcFormConverter`DiracAlgebra -> False},
   {"colour algebra",CalcFormConverter`ColourAlgebra -> False},
   {"scaleless",FeynCalc`DropScaleless -> True},
   {"loop reduction",CalcFormConverter`ReduceLoopCoefficients -> True},
   {"tensor decomposition",CalcFormConverter`TensorDecomposition -> False}}}];
(* Stored deviations and resets behave identically. *)
SetOptions[CalcFormConverter`FeynGravSimplify,FeynCalc`LoopMomenta -> {q}];
check["stored deviation refuses",tag[FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10]]==="MethodCapability"];
SetOptions[CalcFormConverter`FeynGravSimplify,FeynCalc`LoopMomenta -> {}];
check["reset restores eligibility",FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10]===D];
SetOptions[CalcFormConverter`FeynGravSimplify,FeynCalc`DropScaleless -> True];
check["explicit matches stored refusal",tag[FeynGravSimplify[MTD[mu,mu],
    System`Method -> "FORMBenchmark",CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]]===tag[FeynGravSimplify[MTD[mu,mu],
    System`Method -> "FORMBenchmark",FeynCalc`DropScaleless -> True]]];
SetOptions[CalcFormConverter`FeynGravSimplify,FeynCalc`DropScaleless -> False];
(* One resolution per call: delayed settings are read exactly once. *)
counter=0;
check["delayed method resolves once",
  FeynGravSimplify[MTD[mu,mu],System`Method :> (counter++; "FORM"),
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20]===D&&counter===1];
counter=0;
check["delayed loop refuses once",
  FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    FeynCalc`LoopMomenta :> (counter++; {q})][[1]]==="MethodCapability"&&
  counter===1];
refusal=FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",FeynCalc`LoopMomenta :> {q, r}];
check["delayed loop value reported",FailureQ[refusal]&&
    refusal[[2]]["RefusedRequests"][[1]]["RequestedValue"]==={q, r}];
counter=0;
check["duplicate forwarding",FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    CalcFormConverter`FORMThreads -> 1,CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10]===D];
(* Conflicting duplicates follow first-wins semantics. Suppress only the
   warning already verified above, preserving all result assertions. *)
loopFirstCapture=captureWithoutPartialWarning[FeynGravSimplify[1,System`Method -> "FORM",
    FeynCalc`LoopMomenta -> {q},FeynCalc`LoopMomenta -> {},CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 20]];
checkCleanCapture["duplicate loop first wins",loopFirstCapture];
check["duplicate conflicting loop first wins",loopFirstCapture["Value"]===loopVacuum];
check["duplicate conflicting loop reversed",FeynGravSimplify[1,System`Method -> "FORM",
    FeynCalc`LoopMomenta -> {},FeynCalc`LoopMomenta -> {q},CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 20]===1];
methodFirstCapture=captureWithoutPartialWarning[FeynGravSimplify[1,System`Method -> "FORM",
    System`Method -> "FORMBenchmark",FeynCalc`LoopMomenta -> {q},CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 20]];
checkCleanCapture["duplicate method first wins",methodFirstCapture];
check["duplicate conflicting method first wins",methodFirstCapture["Value"]===loopVacuum];
check["duplicate conflicting method reversed",tag[FeynGravSimplify[1,
    System`Method -> "FORMBenchmark",System`Method -> "FORM",FeynCalc`LoopMomenta -> {q}]]===
    "MethodCapability"];
(* Option lists are a standard call form. The nested control must actually take
   effect: the nested restricted method plus a non-baseline loop setting has to
   refuse before any backend starts. Ignoring the nested rules would select the
   full pipeline and return D instead, so this distinguishes the two. *)
check["option list accepted",FeynGravSimplify[MTD[mu,mu],{System`Method -> "FORM",
    CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20}]===D];
nestedResult=FeynGravSimplify[MTD[mu,mu],{{System`Method -> "FORMBenchmark",
    FeynCalc`LoopMomenta -> {q}}}];
check["nested option list honoured",
  tag[nestedResult]==="MethodCapability"&&nestedResult[[2]]["Method"]==="FORMBenchmark"&&
  MemberQ[Lookup[nestedResult[[2]]["RefusedRequests"],"Setting"],FeynCalc`LoopMomenta]];
(* Stored delayed settings: a selected stored delayed value resolves once; an
   unselected stored delayed value stays unevaluated; explicit options override
   stored rules without evaluating them. *)
savedSimplifyStore=Options[CalcFormConverter`FeynGravSimplify];
SetOptions[CalcFormConverter`FeynGravSimplify,
    CalcFormConverter`FORMExecutable :> (storedCounter++; fixture)];
storedCounter=0;
check["stored delayed selected resolves once",FeynGravSimplify[a,
    System`Method -> "FORMBenchmark",CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]===0&&storedCounter===1];
storedCounter=0;
check["stored delayed unselected inert",FeynGravSimplify[a,System`Method -> "Contractor",
    FeynGrav`ContractorExecutable -> fixture,FeynGrav`ContractorThreads -> 1,
    System`TimeConstraint -> 10]===a&&storedCounter===0];
storedCounter=0;
check["explicit overrides stored delayed",FeynGravSimplify[a,System`Method -> "FORMBenchmark",
    CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]===0&&storedCounter===0];
Options[CalcFormConverter`FeynGravSimplify]=savedSimplifyStore;
SetOptions[CalcFormConverter`FeynGravSimplify,FeynCalc`LoopMomenta :> (storedCounter++; {q})];
storedCounter=0;
check["explicit capability override avoids stored evaluation",FeynGravSimplify[MTD[mu,mu],
    System`Method -> "FORMBenchmark",FeynCalc`LoopMomenta -> {},CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]===D&&storedCounter===0];
storedCounter=0;
check["stored delayed capability refusal",tag[FeynGravSimplify[MTD[mu,mu],
    System`Method -> "FORMBenchmark",CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10]]==="MethodCapability"&&storedCounter===1];
Options[CalcFormConverter`FeynGravSimplify]=savedSimplifyStore;
(* Failure-data contract: compare against the accepted implementation's own
   serialization blocks (copied from the accepted snapshot above), computed in
   this suite's formatting context. Full associations are compared, symbol
   identity is checked before serialization, and mismatches print actual and
   expected data. No error text is parsed with ToExpression. *)
baselineSimplifyUnknown[unknown_] := <|
    "UnsupportedOptions" -> ToString[#, InputForm] & /@ unknown,
    "SupportedOptions" -> ToString[#, InputForm] & /@ acceptedSimplifyOptionNames|>;
baselineExportUnknown[unknown_] := <|
    "UnsupportedOptions" -> ToString[#, InputForm] & /@ unknown|>;
baselineStored[stored_] := <|
    "UnsupportedStoredOptions" -> ToString[#, InputForm] & /@ stored|>;
simplifyUnknown=Complement[DeleteDuplicates[First /@ Flatten[{{MadeUpOption -> 1}}]],
    FeynGrav`Simplification`Private`$simplifyOptionNames];
check["unknown option identity",simplifyUnknown==={MadeUpOption}];
badSimplify=FeynGravSimplify[a,MadeUpOption -> 1];
check["simplify unknown option tag",tag[badSimplify]==="InvalidOption"];
expectedSimplify=Join[<|"MessageTemplate" -> "Unknown option for FeynGravSimplify."|>,
    baselineSimplifyUnknown[simplifyUnknown]];
check["simplify unknown option data",badSimplify[[2]]===expectedSimplify];
If[badSimplify[[2]]=!=expectedSimplify,
  Print["MISMATCH simplify unknown actual=",ToString[badSimplify[[2]],InputForm],
        " expected=",ToString[expectedSimplify,InputForm]]];
badExport=FeynGravFORMExport[MTD[mu,mu],"unused.frm",MadeUpOption -> 1];
check["export unknown option tag",tag[badExport]==="InvalidOption"];
expectedExport=Join[<|"MessageTemplate" -> "Unknown option for FeynGravFORMExport."|>,
    baselineExportUnknown[simplifyUnknown]];
check["export unknown option data",badExport[[2]]===expectedExport];
If[badExport[[2]]=!=expectedExport,
  Print["MISMATCH export unknown actual=",ToString[badExport[[2]],InputForm],
        " expected=",ToString[expectedExport,InputForm]]];
badCheck=FeynGravFORMCheck[MadeUpOption -> 1];
check["check unknown option tag",tag[badCheck]==="InvalidOption"];
check["check unknown option message",
  badCheck[[2]]["MessageTemplate"]==="Unknown option for FeynGravFORMCheck."];
badInstall=FeynGravFORMInstall[MadeUpOption -> 1];
check["install unknown option tag",tag[badInstall]==="InvalidOption"];
check["install unknown option message",
  badInstall[[2]]["MessageTemplate"]==="Unknown option for FeynGravFORMInstall."];
check["import arity invalid",tag[FeynGravFORMImport["a","b","c"]]==="InvalidArguments"&&
  tag[FeynGravFORMImport[]]==="InvalidArguments"];
savedCheckStore=Options[CalcFormConverter`FeynGravFORMCheck];
Options[CalcFormConverter`FeynGravFORMCheck]=Append[savedCheckStore,CalcFormConverter`FORMRates -> 1];
checkStoreStored=Select[First /@ Options[CalcFormConverter`FeynGravFORMCheck],
    !MemberQ[acceptedCheckOptionNames,#]&];
check["stored option identity",checkStoreStored==={CalcFormConverter`FORMRates}];
badStoredCheck=FeynGravFORMCheck[];
check["check stored option tag",tag[badStoredCheck]==="InvalidOption"];
expectedStored=Join[<|"MessageTemplate" -> "FeynGravFORMCheck holds an unsupported stored option."|>,
    baselineStored[checkStoreStored]];
check["check stored option data",badStoredCheck[[2]]===expectedStored];
If[badStoredCheck[[2]]=!=expectedStored,
  Print["MISMATCH check stored actual=",ToString[badStoredCheck[[2]],InputForm],
        " expected=",ToString[expectedStored,InputForm]]];
Options[CalcFormConverter`FeynGravFORMCheck]=savedCheckStore;
savedSimplifyStore2=Options[CalcFormConverter`FeynGravSimplify];
Options[CalcFormConverter`FeynGravSimplify]=Append[savedSimplifyStore2,CalcFormConverter`FORMRates -> 1];
simplifyStored=Select[First /@ Options[CalcFormConverter`FeynGravSimplify],
    !MemberQ[acceptedSimplifyOptionNames,#]&];
check["simplify stored option identity",simplifyStored==={CalcFormConverter`FORMRates}];
badStoredSimplify=FeynGravSimplify[a];
expectedStoredSimplify=Join[<|"MessageTemplate" -> "FeynGravSimplify holds an unsupported stored option."|>,
    baselineStored[simplifyStored]];
check["simplify stored option rejected",tag[badStoredSimplify]==="InvalidOption"&&
  badStoredSimplify[[2]]===expectedStoredSimplify];
If[tag[badStoredSimplify]==="InvalidOption"&&badStoredSimplify[[2]]=!=expectedStoredSimplify,
  Print["MISMATCH simplify stored actual=",ToString[badStoredSimplify[[2]],InputForm],
        " expected=",ToString[expectedStoredSimplify,InputForm]]];
Options[CalcFormConverter`FeynGravSimplify]=savedSimplifyStore2;
check["stores restored",Options[CalcFormConverter`FeynGravFORMCheck]===savedCheckStore&&
  Options[CalcFormConverter`FeynGravSimplify]===savedSimplifyStore2];
(* Loader scratch-state assertions are intentionally isolated in Loading.wls;
   this suite keeps behavioural facade and store contracts. *)
(* Extracted restricted execution/reporting owns one terminal total-time line. *)
reportFile=FileNameJoin[{$TemporaryDirectory,"fg-report-"<>CreateUUID[]<>".txt"}];
reportStream=OpenWrite[reportFile];
Block[{$Output=reportStream},
  reportResult=FeynGravSimplify[a,System`Method -> "FORMBenchmark",
    CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
    System`TimeConstraint -> 10,CalcFormConverter`ShowTiming -> True]];
Close[reportStream];
reportText=Import[reportFile,"Text"];
DeleteFile[reportFile];
check["single terminal report",reportResult===0&&
  StringCount[reportText,"FeynGravSimplify total ("]===1];
If[StringCount[reportText,"FeynGravSimplify total ("]=!=1,
  Print["MISMATCH terminal report count actual=",
        StringCount[reportText,"FeynGravSimplify total ("]," expected=1"]];

(* Exports, imports, checks and install wrapper shapes. *)
exportExpr=MTD[mu,nu] FVD[p,mu];
exportUuid=CreateUUID[];
wrapperFiles=FeynGravFORMExport[exportExpr,
    FileNameJoin[{$TemporaryDirectory,"fa-wrapper-"<>exportUuid<>".frm"}],System`OverwriteTarget -> True];
ownerFiles=CalcFormExport[exportExpr,
    FileNameJoin[{$TemporaryDirectory,"fa-owner-"<>exportUuid<>".frm"}],System`OverwriteTarget -> True,
    CalcFormConverter`TensorDecomposition -> True,CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True,FeynCalc`DropScaleless -> False,
    FeynCalc`Dimension -> Automatic,FeynCalc`LoopMomenta -> {},
    CalcFormConverter`DiracAlgebra -> Automatic,CalcFormConverter`ColourAlgebra -> True];
check["wrapper export succeeds",AssociationQ[wrapperFiles]&&!FailureQ[wrapperFiles]];
check["owner export succeeds",AssociationQ[ownerFiles]&&!FailureQ[ownerFiles]];
check["wrapper mapping matches owner",!FailureQ[wrapperFiles]&&!FailureQ[ownerFiles]&&
    Import[wrapperFiles["MappingFile"],"Text"]===Import[ownerFiles["MappingFile"],"Text"]];
(* Imports reproduce the pipeline output from retained job files. *)
beforeJobs=FileNames["calcform-job-*",$TemporaryDirectory];
pipelineResult=Quiet[FeynGravSimplify[exportExpr,System`Method -> "FORM",
    CalcFormConverter`KeepFiles -> True,CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20],
    CalcFormCalculate::files];
check["pipeline result",equal[pipelineResult,FVD[p,nu]]];
keptJobs=Complement[FileNames["calcform-job-*",$TemporaryDirectory],beforeJobs];
check["retained job present",Length[keptJobs]===1];
If[Length[keptJobs]===1,
  imported=FeynGravFORMImport[FileNameJoin[{First[keptJobs],"job.out"}],
      FileNameJoin[{First[keptJobs],"job.map.json"}]];
  check["import reproduces pipeline",equal[imported,pipelineResult]];
  DeleteDirectory[First[keptJobs],DeleteContents -> True]];
check["import extra argument invalid",FeynGravFORMImport["a","b","c"][[1]]==="InvalidArguments"];
check["check wrapper runs",AssociationQ[FeynGravFORMCheck[System`TimeConstraint -> 10]]];
check["check wrapper missing executable",FeynGravFORMCheck[CalcFormConverter`FORMExecutable -> "/missing/form",System`TimeConstraint -> 5]["Available"]===False];
check["install wrapper unknown option",FeynGravFORMInstall[MadeUpInstallOption -> 1][[1]]==="InvalidOption"];
check["install wrapper arity",FeynGravFORMInstall[1][[1]]==="InvalidArguments"];
(* No call mutated any owner or wrapper option store. *)
check["option stores untouched",(Options /@ storeSymbols)===storeSnapshot];
(* ProcessLink pipe streams are registered by the owner's process handling on
   this branch and are outside the facade's scope; all other streams must close. *)
processStreams=Complement[Streams[],streamSnapshot];
regularStreams=Select[processStreams,!StringContainsQ[ToString[#[[1]],InputForm],"ProcessLink"]&];
check["regular streams closed",regularStreams==={}];
writeSuiteCompletion["Facade", passed, failed];
Print["Facade: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
