(* Surgical rewrite of the user's current notebook.
   Only the initialisation Input cell and the text of two Text cells change;
   every other cell, including all saved Output/Print/Message cells, is preserved. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
repo=ExpandFileName[FileNameJoin[{caseDirectory,"..","..",".."}]];
source=FileNameJoin[{repo,"Examples","Graviton_Scattering_Tree_Level_FeynArt.nb"}];
If[!FileExistsQ[source],Print["SOURCE_NOT_FOUND=",source];Quit[2]];
Print["SOURCE=",source];
before=FileNameJoin[{caseDirectory,"before.nb"}];
CopyFile[source,before];
Print["SOURCE_SHA256=",IntegerString[Hash[Import[source,"Byte"],"SHA256"],16,64]];
nb=Get[before];
If[Head[nb]=!=Notebook,Print["NOTEBOOK_PARSE_FAILED"];Quit[2]];
initUUID="41642cfa-ffbe-4ce4-ac10-74c85809b4c0";
If[Count[nb,Cell[___,ExpressionUUID->initUUID,___],Infinity]=!=1,
 Print["INITIALISATION_CELL_NOT_UNIQUELY_IDENTIFIED"];Quit[3]];
(* The whole initialisation reduces to selecting the saved model, then loading it.
   GenericModel must be given: the .mod file does not name its generic model and
   the FeynArts default is the stock "Lorentz" file. *)
initBoxes=ToBoxes[Defer[
 grModel=FileNameJoin[{NotebookDirectory[],"..","Benchmark","FeynArtsEvidence",
  "model-first-2026-10-12","model","FGPureGR5a149c98161e2b2e"}];
 InitializeModel[grModel,GenericModel->grModel];
],StandardForm];
newInit=Cell[BoxData[initBoxes],"Input",InitializationCell->True,ExpressionUUID->initUUID];
nb=nb/.Cell[BoxData[_],"Input",___,ExpressionUUID->initUUID,___]:>newInit;
If[Count[nb,Cell[___,ExpressionUUID->initUUID,___],Infinity]=!=1,
 Print["REPLACEMENT_FAILED"];Quit[4]];
(* Drop the FeynArts context prefix inside input-like cells only. *)
strip[b_]:=b/.s_String:>StringReplace[s,"FeynArts`"->""];
nb=nb/.Cell[BoxData[b_],style:("Input"|"Code"),rest___]:>Cell[BoxData[strip[b]],style,rest];
(* Keep the prose consistent with the unqualified commands. *)
nb=nb/.Cell[t_String,"Text",rest___]:>Cell[StringReplace[t,{
  "FeynArts`Paint"->"Paint",
  "and evaluate the cell below with Shift+Enter. FeynGrav loads FeynCalc and CalcFormConverter; setting $LoadFeynArts first also loads the installed FeynCalc-patched FeynArts."->
  "and evaluate the two cells below with Shift+Enter. The first loads FeynGrav, which loads FeynCalc and CalcFormConverter, and asks FeynCalc to load the installed patched FeynArts."
 }],"Text",rest];
remaining=Count[nb/.Cell[BoxData[b_],("Input"|"Code"),___]:>b,_String?(StringContainsQ[#,"FeynArts`"]&),Infinity];
Print["REMAINING_FEYNARTS_PREFIXES_IN_INPUTS=",remaining];
If[remaining=!=0,Print["PREFIX_STRIP_INCOMPLETE"];Quit[5]];
Print["PRESERVED_OUTPUT_CELLS=",Count[nb,Cell[_,("Output"|"Print"|"Message"),___],Infinity]];
Print["INPUT_CELLS=",Count[nb,Cell[_BoxData,("Input"|"Code"),___],Infinity]];
Print["NEW_INITIALISATION_PLAIN=",ToString[InputForm[Defer[
 grModel=FileNameJoin[{NotebookDirectory[],"..","Benchmark","FeynArtsEvidence","model-first-2026-10-12","model","FGPureGR5a149c98161e2b2e"}];
 InitializeModel[grModel,GenericModel->grModel];]]]];
Put[nb,FileNameJoin[{caseDirectory,"candidate.nb"}]];
Print["CANDIDATE_WRITTEN"];
Quit[0];
