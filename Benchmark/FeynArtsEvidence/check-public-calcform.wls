(* One physically sourced cubic diagram, bounded FORM algebra only; no loop reduction. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
If[DownValues[CalcFormConverter`CalcFormCalculate]==={},
  Get["/home/latosh/.Wolfram/Applications/FeynGrav/CalcFormConverter/CalcFormConverter.wl"]];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
spec=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
proof=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,spec,{p},{q1,q2}];
Print["SOURCE_AMPLITUDE_READY=",AssociationQ[proof]];
If[!AssociationQ[proof],Quit[2]];
wrong=FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[KeyDrop[proof,"RawAmplitude"]];
Print["INCOMPLETE_INPUT_REJECT=",FailureQ[wrong]," TAG=",If[FailureQ[wrong],wrong[[1]],None]];
altered=Join[First[proof["Diagrams"]],
  <|"Expression"->First[proof["Diagrams"]]["Expression"]+1|>];
tampered=Join[proof,<|"Diagrams"->{altered}|>];
bad=FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[tampered];
Print["MUTATED_DIAGRAM_REJECT=",FailureQ[bad]," TAG=",If[FailureQ[bad],bad[[1]],None]];
If[!FailureQ[wrong]||!MatchQ[bad,Failure["UnverifiedAmplitude",_]],Quit[4]];
parent="/tmp/fgfa_form_tiny_20261014";If[!DirectoryQ[parent],CreateDirectory[parent]];
r=TimeConstrained[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[proof,
  FeynCalc`Dimension->D,CalcFormConverter`WorkingDirectory->parent,
  CalcFormConverter`KeepFiles->True,CalcFormConverter`FORMThreads->1,
  CalcFormConverter`ShowProgress->False,System`TimeConstraint->20],55,$TimedOut];
Print["FORM_STAGE=",If[AssociationQ[r],r["Status"],InputForm[r]]];
If[AssociationQ[r],Print["DIAGRAM_STATUS=",InputForm[Lookup[r["Diagrams"],"Status"]],
   " RESULT_BYTES=",ByteCount[r["Diagrams"][[1,"Result"]]],
   " FAILED_IDS=",InputForm[r["FailedDiagramIDs"]],
   " TOTAL_PRESENT=",!MissingQ[r["Total"]]],
 Print["NO_RESULT=",InputForm[r]]];
Quit[If[AssociationQ[r]&&r["Status"]==="Success",0,3]];
