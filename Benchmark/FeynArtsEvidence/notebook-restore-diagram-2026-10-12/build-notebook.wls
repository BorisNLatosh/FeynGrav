(* Restore the diagram display: the sheet was computed but the trailing semicolon
   suppressed the cell output. Only that cell is rewritten. *)
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
paintCells=Count[nb,Cell[BoxData[b_],("Input"|"Code"),___]/;contains[heldOf[b],Paint],Infinity];
Print["PAINT_CELLS=",paintCells];
If[paintCells=!=1,Print["EXPECTED_ONE_PAINT_CELL"];Quit[3]];

paintBoxes=ToBoxes[Defer[
 diagramGraphics=Paint[treeInsertions,
   ColumnsXRows->{2,2},Numbering->Simple,
   SheetHeader->"Four-graviton tree diagrams",AutoEdit->False,
   DisplayFunction->Identity];
 diagramSheets=Show[diagramGraphics];
 diagramSheet=Show[First[diagramSheets],ImageSize->640]
],StandardForm];

count=0;
nb=nb/.Cell[BoxData[b_],style:("Input"|"Code"),rest___]/;contains[heldOf[b],Paint]:>
  (count++;Cell[BoxData[paintBoxes],style,rest]);
Print["REPLACEMENTS=",count];
If[count=!=1,Print["REPLACEMENT_COUNT_WRONG"];Quit[6]];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity],
 " OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
