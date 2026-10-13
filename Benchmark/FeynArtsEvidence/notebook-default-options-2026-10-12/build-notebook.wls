(* Simplify the contraction cell: default CalcFormCalculate options and a plain
   per-diagram map instead of a Module/Function wrapper. *)
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
If[Count[nb,Cell[BoxData[b_],("Input"|"Code"),___]/;contains[heldOf[b],CalcFormCalculate],Infinity]=!=1,
 Print["EXPECTED_ONE_CONTRACTION_CELL"];Quit[3]];

contractionBoxes=ToBoxes[Defer[
 diagramAmplitudes=
   CalcFormCalculate[FeynAmpDenominatorExplicit[#] externalTensors]&/@diagramTensors;
 If[!FreeQ[diagramAmplitudes,$Failed|$Aborted|_LorentzIndex|_CalcFormCalculate],
   Print["A diagram contraction did not complete."];Abort[]];
],StandardForm];

count=0;
nb=nb/.Cell[BoxData[b_],style:("Input"|"Code"),rest___]/;contains[heldOf[b],CalcFormCalculate]:>
  (count++;Cell[BoxData[contractionBoxes],style,rest]);
Print["REPLACEMENTS=",count];
If[count=!=1,Print["REPLACEMENT_COUNT_WRONG"];Quit[6]];

nb=nb/.Cell[t_String,"Text",rest___]:>Cell[StringReplace[t,
 "The helper contractDiagram[diagram] performs one contraction: it sets Dimension, one FORM worker, a 60-second per-stage limit, the working directory and file retention, and aborts unless the result is a scalar free of unconverted heads. Job files are retained under treeWorkDirectory."->
 "Each contraction uses CalcFormCalculate with its default options: the Lorentz dimension is inferred from the expression, which carries symbolic D, and successful FORM jobs are cleaned up automatically. The results are checked for a surviving Lorentz index or an unconverted head before the total is formed."
 ],"Text",rest];
inputBoxes=Cases[nb,Cell[b_BoxData,("Input"|"Code"),___]:>b,Infinity];
Print["OPTION_MENTIONS_LEFT=",InputFormat@Count[inputBoxes,_String?(StringContainsQ[#,"treeWorkDirectory"|"FORMThreads"|"TimeConstraint"|"KeepFiles"]&),Infinity]];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity],
 " OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
