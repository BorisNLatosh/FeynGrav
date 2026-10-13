(* Rename the model reference in the notebook to the new short name. *)
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
If[Count[nb,Cell[BoxData[b_],("Input"|"Code"),___]/;contains[heldOf[b],InitializeModel],Infinity]=!=1,
 Print["EXPECTED_ONE_MODEL_CELL"];Quit[3]];
initBoxes=ToBoxes[Defer[
 grModel="PureGR";
 InitializeModel[grModel,GenericModel->grModel];
],StandardForm];
count=0;
nb=nb/.Cell[BoxData[b_],style:("Input"|"Code"),rest___]/;contains[heldOf[b],InitializeModel]:>
  (count++;Cell[BoxData[initBoxes],style,rest]);
Print["REPLACEMENTS=",count];
If[count=!=1,Print["REPLACEMENT_COUNT_WRONG"];Quit[6]];
(* Only the initialisation cell may mention the model; saved outputs still show
   the old name and are regenerated when the notebook is re-evaluated. *)
inputBoxes=Cases[nb,Cell[b_BoxData,("Input"|"Code"),___]:>b,Infinity];
Print["OLD_NAME_IN_INPUTS=",Count[inputBoxes,_String?(StringContainsQ[#,"FGPureGR"]&),Infinity]];
Print["NEW_NAME_IN_INPUTS=",Count[inputBoxes,_String?(StringContainsQ[#,"PureGR"]&),Infinity]];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity],
 " OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
