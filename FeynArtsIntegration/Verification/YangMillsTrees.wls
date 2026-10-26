(* Run one selected tree in a fresh externally bounded kernel. No FORM needed. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;$LoadAddOns={"FeynArts"};
treeRoot=DirectoryName[$InputFileName];
treeRepo=DirectoryName[DirectoryName[treeRoot]];
treeCandidate=FileNameJoin[{treeRepo,"FeynArts","Models","GR-SU-N-Yang-Mills"}];
treeCase=Environment["FG_YM_PROCESS"];
treeRun=Environment["FG_YM_EVIDENCE"];
If[!MemberQ[{"VVVV","VVVT","VVTT"},treeCase]||!StringQ[treeRun]||!StringStartsQ[treeRun,"/"]||FileExistsQ[treeRun]||!DirectoryQ[DirectoryName[treeRun]],
 Print["Supply FG_YM_PROCESS=VVVV, VVVT or VVTT and a fresh absolute FG_YM_EVIDENCE directory."];Quit[2]];
CreateDirectory[treeRun];treeCount=0;
treeDiag=OpenWrite[FileNameJoin[{treeRun,"wolfram-diagnostics.log"}]];$Messages={treeDiag};
treeStop[s_]:=(Print["FAIL: ",s];Close[treeDiag];Quit[1]);
SetAttributes[treeNeed,HoldRest];
treeNeed[s_,e_]:=If[TrueQ[Check[e,False]],treeCount++;Print["PASS ",s],treeStop[s]];
Get[FileNameJoin[{treeRoot,"YangMillsTreeChecks.wl"}]];
