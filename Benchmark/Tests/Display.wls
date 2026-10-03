(* Display-only checks: no benchmark preparation, FORM jobs, or report writes. *)
$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"Support","Benchmark.wl"}]];
checks=0; failures=0;
assert[name_,test_] := (checks++;If[!TrueQ[test],failures++;Print["FAIL: ",name]]);
Do[
 nb=Get[file];
 inputs=Cases[nb,Cell[BoxData[b_],"Input",___]:>b,Infinity];
 tables=Select[inputs,!FreeQ[#,"Dataset"|"BenchmarkSummary"]&];
 assert[FileNameTake[file]<>" three protected table inputs",Length[tables]===3 && AllTrue[tables,MatchQ[ToExpression[#,StandardForm,HoldComplete],HoldComplete[StandardForm[_]]]&]];
 outputs=Cases[nb,Cell[BoxData[b_],"Output",___]:>b,Infinity];
 tables=Select[outputs,!FreeQ[#,Dataset`InterpretDataset]&];
 assert[FileNameTake[file]<>" saved tables in StandardForm",MemberQ[{0,3},Length[tables]] && AllTrue[tables,MatchQ[#,FormBox[_,StandardForm]]&]];
 assert[FileNameTake[file]<>" all inputs parse",AllTrue[inputs,Quiet[Check[ToExpression[#,StandardForm,HoldComplete];True,False]]&]];
 assert[FileNameTake[file]<>" no automatic evaluation",FreeQ[nb,InitializationCell->True]],
 {file,FileNames["*.nb",root]}];
summary=BenchmarkSummary[<|"Summary"->{<|"Case"->"display test","MedianSeconds"->1|>}|>];
assert["summary explicitly formatted",MatchQ[summary,StandardForm[_Dataset]]];
assert["summary retains underlying data",Normal[First[summary]]==={<|"Case"->"display test","MedianSeconds"->1|>}];
Print["Benchmark display: ",checks," assertions; ",failures," failures."];
Quit[If[failures===0,0,1]];
