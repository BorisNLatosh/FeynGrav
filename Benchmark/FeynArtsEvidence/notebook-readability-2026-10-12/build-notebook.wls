(* Readability pass over the existing notebook. Cells are matched structurally and
   replaced one at a time; every other cell, including saved outputs, is kept.
   Replacement boxes are built with ToBoxes[Defer[...]] so no physics is executed
   while the candidate is assembled. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
repo=ExpandFileName[FileNameJoin[{caseDirectory,"..","..",".."}]];
source=FileNameJoin[{repo,"Examples","Graviton_Scattering_Tree_Level_FeynArt.nb"}];
If[!FileExistsQ[source],Print["SOURCE_NOT_FOUND=",source];Quit[2]];
before=FileNameJoin[{caseDirectory,"before.nb"}];
CopyFile[source,before,OverwriteTarget->True];
nb=Get[before];
If[Head[nb]=!=Notebook,Print["NOTEBOOK_PARSE_FAILED"];Quit[2]];
heldOf[b_]:=ToExpression[If[Head[b]===List,RowBox[Riffle[b,";"]],b],StandardForm,HoldComplete];
contains[held_,head_]:=!FreeQ[held,head];
replace[test_,boxes_]:=Module[{count=0},
  nb=nb/.Cell[BoxData[b_],style:("Input"|"Code"),rest___]/;test[heldOf[b]]:>
    (count++;Cell[BoxData[boxes],style,rest]);
  Print["REPLACEMENTS=",count];
  If[count=!=1,Print["REPLACEMENT_COUNT_WRONG"];Quit[6]]];

paintBoxes=ToBoxes[Defer[
 diagramGraphics=Paint[treeInsertions,
   ColumnsXRows->{2,2},Numbering->Simple,
   SheetHeader->"Four-graviton tree diagrams",AutoEdit->False,
   DisplayFunction->Identity];
 diagramSheets=Show[diagramGraphics];
 diagramSheet=Show[First[diagramSheets],ImageSize->640];
],StandardForm];

convertBoxes=ToBoxes[Defer[
 treeRawAmplitude=CreateFeynAmp[treeInsertions,
   PreFactor->1,GaugeRules->{},MomentumConservation->True,Truncated->True];
 diagramTensors=FCFAConvert[treeRawAmplitude,
   IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},
   LorentzIndexNames->{mu1,nu1,mu2,nu2,mu3,nu3,mu4,nu4,alpha1,beta1,alpha2,beta2},
   ChangeDimension->D];
 If[Length[diagramTensors]=!=4,
   Print["Expected the contact and three exchange diagrams."];Abort[]];
],StandardForm];

contractBoxes=ToBoxes[Defer[
 Clear[diagramAmplitudes,\[ScriptCapitalM]];
 treeWorkDirectory=CreateDirectory[FileNameJoin[{$TemporaryDirectory,
   "fgfa-four-tree-offshell-"<>CreateUUID[]}]];
 contractDiagram[tensor_]:=Module[{result},
   result=CalcFormCalculate[FeynAmpDenominatorExplicit[tensor] externalTensors,
     Dimension->D,FORMThreads->1,TimeConstraint->60,
     WorkingDirectory->treeWorkDirectory,KeepFiles->True];
   If[FailureQ[result]||MemberQ[{$Failed,$Aborted},result]||
     !FreeQ[result,_LorentzIndex|_CalcFormCalculate],
     Print["A diagram contraction did not complete. Inspect ",treeWorkDirectory];
     Abort[]];
   result];
 diagramAmplitudes=contractDiagram/@diagramTensors;
],StandardForm];

totalBoxes=ToBoxes[Defer[\[ScriptCapitalM]=Total[diagramAmplitudes];],StandardForm];

replace[contains[#,Paint]&,paintBoxes];
replace[contains[#,FCFAConvert]&,convertBoxes];
replace[contains[#,CalcFormCalculate]&,contractBoxes];
replace[contains[#,Total]&,totalBoxes];

(* prose: state why GenericModel is explicit and what the contraction sets *)
nb=nb/.Cell[t_String,"Text",rest___]:>Cell[StringReplace[t,{
 "No diagrams are generated and FORM is not executed here."->
 "GenericModel is given explicitly because the classes file does not name its generic model and the FeynArts default is the stock \"Lorentz\" file. No diagrams are generated and FORM is not executed here.",
 "FORM uses one worker and a 60-second per-stage limit; job files are retained under treeWorkDirectory."->
 "The helper contractDiagram[diagram] performs one contraction: it sets Dimension, one FORM worker, a 60-second per-stage limit, the working directory and file retention, and aborts unless the result is a scalar free of unconverted heads. Job files are retained under treeWorkDirectory."
 }],"Text",rest];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity],
 " OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
