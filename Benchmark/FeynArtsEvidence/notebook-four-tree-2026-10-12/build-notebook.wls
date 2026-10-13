(* Extend the actual user-saved notebook, preserving its initialisation. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
nb=Get[FileNameJoin[{caseDirectory,"before.nb"}]];
leafCells=Cases[nb,c:Cell[_,("Title"|"Text"|"Section"|"Input"),___]:>c,Infinity];
initialCells=TakeWhile[leafCells,!MatchQ[#,Cell["2. Generate and select the s-channel","Section",___]]&];
If[Length[Cases[initialCells,Cell[_,"Input",___],Infinity]]=!=1,Print["UNEXPECTED_INITIALISATION"];Quit[2]];
(* Rebox the unchanged loading commands as one CompoundExpression: the front-end
   saved BoxData list otherwise parses as several top-level expressions headlessly. *)
initCode=Defer[
 $LoadFeynArts=True;
 feynGravDirectory=ExpandFileName[FileNameJoin[{NotebookDirectory[],".."}]];
 Get[FileNameJoin[{feynGravDirectory,"FeynGrav.wl"}]];
 FeynGrav`importGravitons[2];
 grModelDirectory=FileNameJoin[{feynGravDirectory,"Benchmark","FeynArtsEvidence","model-first-2026-10-12","model"}];
 grModel="FGPureGR5a149c98161e2b2e";
 FeynArts`$ModelPath=Prepend[DeleteCases[FeynArts`$ModelPath,grModelDirectory],grModelDirectory];
 If[FeynArts`InitializeModel[grModel,FeynArts`GenericModel->grModel,FeynArts`Reinitialize->False]=!=True,
  Print["The pure-GR model could not be loaded. Check grModelDirectory and restart with a fresh kernel."];Abort[]];
];
initialCells=initialCells/.Cell[_BoxData,"Input",___]:>
 Cell[BoxData[ToBoxes[initCode,StandardForm]],"Input",InitializationCell->True];
initialCells[[2]]=Cell["Generate and display all four off-shell 2 -> 2 graviton tree diagrams with FeynArts: the three exchange channels and the quartic contact. Attach unconstrained external polarisation tensors, contract each contribution, and store their sum in \[ScriptCapitalM]. No mass-shell, transversality, tracelessness or helicity conditions are imposed.","Text"];
SetAttributes[input,HoldAllComplete];
input[code_]:=Cell[BoxData[ToBoxes[Defer[code],StandardForm]],"Input",InitializationCell->False];
newCells={
 Cell["2. Generate and display the contributing diagrams","Section"],
 Cell["Including both cubic and quartic vertices generates the s-, t- and u-channel exchanges and the four-graviton contact. Do not select a single channel. The diagram sheet is displayed before CreateFeynAmp or any FORM calculation is called.","Text"],
 input[
  treeTopologies=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}];
  treeInsertions=FeynArts`InsertFields[treeTopologies,
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->grModel,FeynArts`GenericModel->grModel,
   FeynArts`InsertionLevel->{FeynArts`Classes}];
 ],
 input[
  diagramGraphics=FeynArts`Paint[treeInsertions,
   FeynArts`ColumnsXRows->{2,2},FeynArts`Numbering->FeynArts`Simple,
   FeynArts`SheetHeader->"Four-graviton tree diagrams",FeynArts`AutoEdit->False,
   DisplayFunction->Identity];
  diagramSheet=Show[First[Show[diagramGraphics]],ImageSize->640]
 ],
 Cell["Paint supplies the actual inserted FeynArts diagrams. The four numbers identify the contact and three exchange contributions; they are not extra multiplicative weights. FeynArts already supplies the graph factors.","Text"],
 Cell["3. Build the four off-shell amplitudes","Section"],
 Cell["Use incoming momenta p1,p2 and outgoing momenta p3,p4. All virtualities and scalar products remain symbolic. MomentumConservation -> True supplies internal routing at each vertex without imposing mass-shell conditions or eliminating an external momentum. Truncated -> True removes external propagators; polarisations are attached explicitly in the next cell.","Text"],
 input[
  treeRawAmplitude=FeynArts`CreateFeynAmp[treeInsertions,FeynArts`PreFactor->1,
   FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True];
  diagramTensors=FCFAConvert[treeRawAmplitude,
   IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},
   LorentzIndexNames->{mu1,nu1,mu2,nu2,mu3,nu3,mu4,nu4,alpha1,beta1,alpha2,beta2},
   ChangeDimension->D,Contract->False,List->True,Prefactor->1];
  If[!ListQ[diagramTensors]||Length[diagramTensors]=!=4,
   Print["Expected the contact and three exchange diagrams."];Abort[]];
 ],
 Cell["PolarizationTensorD represents the formal factorised tensor epsilon^mu epsilon^nu. Neither p.epsilon nor epsilon.epsilon is set to zero; no normalisation or helicity is chosen. I and -I specify incoming polarisations and conjugated outgoing polarisations, not helicities.","Text"],
 input[
  externalTensors=PolarizationTensorD[p1,mu1,nu1,I,Transversality->False]*
   PolarizationTensorD[p2,mu2,nu2,I,Transversality->False]*
   PolarizationTensorD[p3,mu3,nu3,-I,Transversality->False]*
   PolarizationTensorD[p4,mu4,nu4,-I,Transversality->False];
 ],
 Cell["4. Contract each diagram and form the total \[ScriptCapitalM]","Section"],
 Cell["Contract the diagrams separately with FORM to limit intermediate expression sizes. The list diagramAmplitudes retains the individual scalar-product expressions. No scalar-product assignments, helicity tables, mass-shell conditions or extra graph factors are introduced. FORM uses one worker and a 60-second per-stage limit; job files are retained under treeWorkDirectory.","Text"],
 input[
  Clear[diagramAmplitudes,\[ScriptCapitalM]];
  treeWorkDirectory=CreateDirectory[FileNameJoin[{$TemporaryDirectory,"fgfa-four-tree-offshell-"<>CreateUUID[]}]];
  diagramAmplitudes=Map[Function[tensor,
   Module[{result},
    result=CalcFormCalculate[FeynAmpDenominatorExplicit[tensor]*externalTensors,
     Dimension->D,FORMThreads->1,TimeConstraint->60,
     WorkingDirectory->treeWorkDirectory,KeepFiles->True,ShowProgress->False];
    If[FailureQ[result]||MemberQ[{$Failed,$Aborted},result]||
     !FreeQ[result,_LorentzIndex|_CalcFormCalculate],
     Print["A diagram contraction did not complete. Inspect treeWorkDirectory."];Abort[]];
    result]],diagramTensors];
 ],
 input[
  If[!ListQ[diagramAmplitudes]||Length[diagramAmplitudes]=!=4,
   Print["All four diagram amplitudes must be calculated before summing."];Abort[]];
  \[ScriptCapitalM]=Total[diagramAmplitudes];
 ],
 Cell["The complete polarisation-contracted, amputated off-shell tree expression is stored in \[ScriptCapitalM] (Mathematica's ScriptCapitalM symbol). Its preview below is shortened only for display. This is not yet an on-shell physical scattering amplitude: the external virtualities, longitudinal contractions and trace terms remain unconstrained. No loop measure or spin sum is included.","Text"],
 Cell[BoxData[RowBox[{"Short","[",RowBox[{"\[ScriptCapitalM]",",","4"}],"]"}]],"Input",InitializationCell->False]
};
nb[[1]]=Join[initialCells,newCells];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["INPUT_CELLS=",Length[Cases[nb,Cell[_,"Input",___],Infinity]]];
Quit[0];
