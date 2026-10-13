(* Revise the preserved previous notebook; never evaluate physics while constructing boxes. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
nb=Get[FileNameJoin[{DirectoryName[caseDirectory],"notebook-s-channel-2026-10-12","validated.nb"}]];
cells=TakeWhile[nb[[1]],!MatchQ[#,Cell["3. Apply the original massless helicity conventions","Section",___]]&];
cells=DeleteCases[cells,Cell[_,"Output",___]];
cells[[2]]=Cell["Construct the off-shell s-channel four-graviton tree contribution using FeynArts, attach external polarisation tensors and contract their indices. No mass-shell, transversality, tracelessness or helicity conditions are imposed. The other channels and quartic contact are not included yet.","Text"];
cells=cells/.Cell[text_String,"Text",opts___]:>Cell[StringReplace[text,{
 "Start with a fresh kernel and evaluate the cell below with"->"Start with a fresh kernel (especially after running the previous on-shell version) and evaluate the cell below with",
 "All p1,...,p4 below are incoming, so the physical outgoing momenta passed to FCFAConvert are -p3 and -p4."->"Use incoming momenta p1, p2 and outgoing momenta p3, p4. Their virtualities and scalar products remain symbolic; FeynArts supplies the signs of momenta entering individual vertices.",
 "In the all-incoming convention, the exchanged momentum is -(p3+p4) = p1+p2 and its pole is 1/s. The tensor is amputated: external helicity tensors are supplied explicitly below."->"The exchanged momentum is p3+p4 = p1+p2 by overall momentum conservation. Its propagator is kept as 1/(p3+p4)^2, not replaced by a Mandelstam variable. The tensor is amputated; external polarisation tensors are attached below. No external momentum is eliminated or put on shell."}],"Text",opts];
(* Remove repeated package prefixes only from input boxes, retaining the explicit
   FeynGrav import in the loading cell, which is parsed before that package loads. *)
cells=cells/.Cell[BoxData[b_],"Input",opts___]:>Cell[BoxData[
 If[!FreeQ[b,"$LoadFeynArts"],b,
  b/.str_String:>StringReplace[str,{"FeynCalc`"->"","FeynGrav`"->"","CalcFormConverter`"->""}]]],"Input",opts];
(* Only the retained conversion cell contains negative p3/p4 boxes. *)
cells=cells/.RowBox[{"-","p3"}]->"p3"/.RowBox[{"-","p4"}]->"p4";
SetAttributes[input,HoldAllComplete];
input[code_]:=Cell[BoxData[ToBoxes[Defer[code],StandardForm]],"Input",InitializationCell->False];
(* This file is parsed in a packaging kernel without FeynCalc loaded. Short command
   names in these boxes resolve normally when each notebook cell is evaluated. *)
newCells={
 Cell["3. Attach unconstrained external polarisations","Section"],
 Cell["PolarizationTensorD represents a factorised tensor epsilon^mu epsilon^nu. Keep these polarisation vectors formal: neither p.epsilon nor epsilon.epsilon is set to zero, and no normalisation or helicity is chosen. Transversality -> False is explicit on every leg. I and -I select incoming polarisations and conjugated outgoing polarisations; they do not choose helicities. This is an off-shell contracted Green-function contribution, not yet a physical on-shell scattering amplitude.","Text"],
 input[
  sExternalTensors=PolarizationTensorD[p1,mu1,nu1,I,Transversality->False]*
   PolarizationTensorD[p2,mu2,nu2,I,Transversality->False]*
   PolarizationTensorD[p3,mu3,nu3,-I,Transversality->False]*
   PolarizationTensorD[p4,mu4,nu4,-I,Transversality->False];
  sChannelWithPolarisations=sChannelTensor*sExternalTensors;
  sChannelPole=FeynAmpDenominatorExplicit[
   First[Cases[sChannelTensor,_FeynAmpDenominator,Infinity]]];
 ],
 input[sChannelPole],
 Cell["4. Contract the off-shell s-channel","Section"],
 Cell["CalcFormCalculate contracts indices while retaining arbitrary momentum and polarisation scalar products, including longitudinal and trace terms. There are no SetMandelstam calls, scalar-product assignments, helicity tables or reference-amplitude comparisons. FORM runs with one worker and a 60-second per-stage limit; its files are retained in sChannelWorkDirectory. No extra graph factor is inserted. The result remains a gauge-dependent s-channel contribution.","Text"],
 input[
  sChannelWorkDirectory=CreateDirectory[FileNameJoin[{$TemporaryDirectory,"fgfa-s-channel-offshell-"<>CreateUUID[]}]];
  AmplitudeSChannel=CalcFormCalculate[
   FeynAmpDenominatorExplicit[sChannelWithPolarisations],
   Dimension->D,FORMThreads->1,TimeConstraint->60,
   WorkingDirectory->sChannelWorkDirectory,KeepFiles->True,ShowProgress->False];
  If[FailureQ[AmplitudeSChannel]||MemberQ[{$Failed,$Aborted},AmplitudeSChannel]||
   !FreeQ[AmplitudeSChannel,_LorentzIndex|_CalcFormCalculate],
   Print["The contraction did not complete. Inspect AmplitudeSChannel and sChannelWorkDirectory."];Abort[]];
 ],
 Cell["The full scalar-product expression is stored in AmplitudeSChannel. Short displays only a preview; it does not change the stored expression. Momentum squares and polarisation contractions remain unevaluated.","Text"],
 Cell[BoxData[RowBox[{"Short","[",RowBox[{"AmplitudeSChannel",",","4"}],"]"}]],"Input",InitializationCell->False]
};
nb[[1]]=Join[cells,newCells];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["INPUT_CELLS=",Length[Cases[nb,Cell[_,"Input",___],Infinity]]];
Quit[0];
