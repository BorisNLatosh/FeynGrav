(* Regression for shared-definition multiplicity, using the live template closure.
   The fixture has a root that is also a child, overlapping parents and two
   dependency levels. The broken pre-sort variant is a sensitive control. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
source=Import[FileNameJoin[{DirectoryName[$testRoot],"Templates","LoopDenominatorGroups.frm.in"}],"Text"];
start="#do cfcRefPass=1,1\n";stop="\nNSkip;\n#endif\nHide cfcRefs;";
closure=start<>First[StringSplit[StringSplit[source,start][[2]],stop]];
fixture=Import[FileNameJoin[{$testRoot,"Fixtures","SharedDependencies.frm"}],"Text"];
old="Local cfcRefUnion=cfcRefs+cfcRefChildren;\nDropCoefficient;\n.sort";
new="Local cfcRefUnion=cfcRefs+cfcRefChildren;\n.sort\n* Deduplicate the sorted union, not its incoming terms: a root may also be\n* another root's dependency. Its multiplicity must never scale its definition.\nSkip;\nNSkip cfcRefUnion;\nDropCoefficient;\n.sort";
assert["live closure normalises after sort",StringCount[closure,new]===1];
expected={"DEFINITION1-1+D","DEFINITION23+2*cfcShare(1)",
 "DEFINITION3cfcShare(1)+cfcShare(2)","DEFINITION4cfcShare(2)+cfcShare(3)"};
Do[
 program=StringReplace[fixture,"@DEPENDENCY_CLOSURE@"->If[mode==="old",StringReplace[closure,new->old],closure]];
 path=FileNameJoin[{testDirectory,"dependency-"<>mode<>".frm"}];Export[path,program,"Text"];
 run=RunProcess[If[mode==="threaded",{"/usr/local/bin/tform","-w2","-q",path},{"/usr/local/bin/form","-q",path}]];
 Export[FileNameJoin[{testDirectory,"dependency-"<>mode<>".stdout"}],run["StandardOutput"],"Text"];
 Export[FileNameJoin[{testDirectory,"dependency-"<>mode<>".stderr"}],run["StandardError"],"Text"];
 assert[mode<>" child successful",run["ExitCode"]===0&&run["StandardError"]===""];
 text=run["StandardOutput"];
 assert[mode<>" reference set",StringContainsQ[text,If[mode==="old","SET_RESIDUAL 3","SET_RESIDUAL 0"]]];
 definitions=Select[StringSplit[text,"\n"],StringStartsQ[#,"DEFINITION"]&];
 definitions=StringReplace[definitions,WhitespaceCharacter->""];
 assert[mode<>" complete definitions",If[mode==="old",definitions=!=expected,definitions===expected]],
 {mode,{"old","fixed","threaded"}}];
finish["Shared dependency registration"];
