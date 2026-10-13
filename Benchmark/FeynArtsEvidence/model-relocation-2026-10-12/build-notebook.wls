(* Point the notebook's initialisation at the packaged FeynArts model directory.
   Only that one Input cell changes; every other cell, including all saved
   Output/Print/Message cells, is preserved. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
repo=ExpandFileName[FileNameJoin[{caseDirectory,"..","..",".."}]];
source=FileNameJoin[{repo,"Examples","Graviton_Scattering_Tree_Level_FeynArt.nb"}];
If[!FileExistsQ[source],Print["SOURCE_NOT_FOUND=",source];Quit[2]];
before=FileNameJoin[{caseDirectory,"before.nb"}];
If[FileExistsQ[before],CopyFile[source,before,OverwriteTarget->True],CopyFile[source,before]];
nb=Get[before];
If[Head[nb]=!=Notebook,Print["NOTEBOOK_PARSE_FAILED"];Quit[2]];
initUUID="41642cfa-ffbe-4ce4-ac10-74c85809b4c0";
If[Count[nb,Cell[___,ExpressionUUID->initUUID,___],Infinity]=!=1,
 Print["INITIALISATION_CELL_NOT_UNIQUELY_IDENTIFIED"];Quit[3]];
initBoxes=ToBoxes[Defer[
 grModel="FGPureGR5a149c98161e2b2e";
 InitializeModel[grModel,GenericModel->grModel];
],StandardForm];
newInit=Cell[BoxData[initBoxes],"Input",InitializationCell->True,ExpressionUUID->initUUID];
nb=nb/.Cell[BoxData[_],"Input",___,ExpressionUUID->initUUID,___]:>newInit;
If[Count[nb,Cell[___,ExpressionUUID->initUUID,___],Infinity]=!=1,Print["REPLACEMENT_FAILED"];Quit[4]];
(* The package now imports graviton orders {1,2} on load, so the explicit call in
   the loading cell is redundant and creates a shadowing Global symbol. *)
codeUUID="2da8c400-44f9-4e33-b46c-24d635d8fb3d";
codeBoxes=ToBoxes[Defer[
 $LoadAddOns={"FeynArts"};
 <<FeynGrav`;
],StandardForm];
nb=nb/.Cell[BoxData[_],style:("Input"|"Code"),___,ExpressionUUID->codeUUID,___]:>
 Cell[BoxData[codeBoxes],style,ExpressionUUID->codeUUID];
If[Count[nb,_String?(StringContainsQ[#,"importGravitons"]&),Infinity]=!=0,
 Print["IMPORT_GRAVITONS_STILL_PRESENT_IN_INPUTS"];Quit[5]];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity]];
Print["PRESERVED_OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Print["MODEL_DIRECTORY_EXISTS=",DirectoryQ[FileNameJoin[{repo,"FeynArts","Models"}]]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
