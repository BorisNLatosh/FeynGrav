(* Exact reference comparison for dependency-ready rational metadata batches.
   The 32-slot window preserves registration order and all existing guards. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
r=testDirectory;diag=OpenWrite[FileNameJoin[{r,"diagnostics.log"}]];$Messages={diag};
fail[x_]:=(Print["STOP ",InputForm[x]];Quit[1]);
checks=0;assert[label_,v_]:=(checks++;If[!TrueQ[v],fail[label]]);
dump="\n"<>StringJoin[Map[Function[name,"#$cfcMBTest="<>name<>"(`$index');\n#write \"MBMETA "<>name<>" `$index' %$\",$cfcMBTest\n"],{"cfcMRMeta","cfcPGTerms","cfcPGAtoms","cfcPGReach","cfcPGSize","cfcPGDenOK","cfcPGParts"}]];
fixtures={
 {"many",A0[m^2] Sum[(x+j y+z^2)^2/(u+j v),{j,1,40}],{}},
 {"nested",A0[m^2] ((x+y)^2+(x+y)(x-y))/(1+z)+B0[s,m^2,m^2] ((x+y)^3+(x-y)^2)/(1+z^2),{}},
 {"pol",A0[m^2](SPD[p,Polarization[p,I]](x+y)/(1+x)+SPD[q,Polarization[p,I]](x+y)^2/(1+y)),{}},
 {"protected",A0[m^2] MTD[mu,nu](x+y)^2,{}},
 {"zero",A0[m^2]((x^2-y^2)/(x-y)-x-y),{}}
};
ledgers={};
Do[
 {label,expr,opts}=fixture;results={};outputs={};metadata={};
 Do[
  stem=label<>If[enabled,"-batch","-reference"];path=FileNameJoin[{r,stem<>".frm"}];
  job=Block[{CalcFormConverter`Private`$rcBatchRationalMetadata=enabled},CalcFormExport[expr,path,Sequence@@opts]];
  assert[stem<>" export",AssociationQ[job]];
  program=Import[path,"Text"];
  program=StringReplace[program,"#call cfcPGRegister(def,`$index')\n"->("#call cfcPGRegister(def,`$index')\n"<>dump)];
  (* No macro instance may escape the completed window. *)
  If[enabled,program=StringReplace[program,
    "#write <"<>job["ResultFile"]<>"> \"CFC_GROUP_BODY\"" ->
      "#ifdef `cfcRBId1'\n#message FAIL leaked batch identifier\n#terminate 1\n#endif\n" <>
      "#ifdef `cfcRBActive'\n#message FAIL leaked batch flag\n#terminate 1\n#endif\n" <>
      "#write <"<>job["ResultFile"]<>"> \"CFC_GROUP_BODY\""]];
  Export[path,program,"Text"];
  run=RunProcess[{"/usr/local/bin/form","-q",path}];
  Export[FileNameJoin[{r,stem<>".stdout"}],run["StandardOutput"],"Text"];Export[FileNameJoin[{r,stem<>".stderr"}],run["StandardError"],"Text"];
  assert[stem<>" exit",run["ExitCode"]===0&&run["StandardError"]===""];
  assert[stem<>" diagnostics",!StringContainsQ[ToLowerCase[run["StandardOutput"]],{"warning","error","illegal","fatal"}]];
  value=CalcFormImport[job["ResultFile"],job["MappingFile"]];assert[stem<>" imported",!FailureQ[value]];
  AppendTo[results,value];AppendTo[outputs,Import[job["ResultFile"],"Text"]];
  (* Diagnostic blocks end at the next header; FORM may wrap long values. *)
  text=run["StandardOutput"];records=StringCases[text,RegularExpression["(?ms)^MBMETA (.*?)(?=^MBMETA |^CFC_|\\z)"]->"$1"];
  AppendTo[metadata,StringReplace[records,WhitespaceCharacter->""]];
  If[label==="many"&&enabled,
    thread=RunProcess[{"/usr/local/bin/tform","-w2","-q",path}];
    Export[FileNameJoin[{r,stem<>"-tform.stdout"}],thread["StandardOutput"],"Text"];Export[FileNameJoin[{r,stem<>"-tform.stderr"}],thread["StandardError"],"Text"];
    assert["TFORM exit",thread["ExitCode"]===0&&thread["StandardError"]===""];
    threadValue=CalcFormImport[job["ResultFile"],job["MappingFile"]];
    assert["TFORM exact result",SameQ[threadValue,Last[results]] || Together[threadValue-Last[results]]===0]];
 ,{enabled,{False,True}}];
 assert[label<>" same output",outputs[[1]]===outputs[[2]]];
 assert[label<>" same result",results[[1]]===results[[2]]];
 assert[label<>" negative control",results[[1]]=!=results[[2]]+1];
 assert[label<>" same metadata",metadata[[1]]===metadata[[2]]];
 If[label==="many",assert["multiple batches exercised",Length[metadata[[1]]]>32*7]];
 If[label==="zero",assert["exact cancellation",results[[2]]===0]];
 AppendTo[ledgers,<|"Fixture"->label,"MetadataRecords"->Length[First[metadata]],"ExactResult"->True,"ExactOutput"->True|>];
,{fixture,fixtures}];
(* Exercise the actual rendered inspectors at small private admission limits. *)
boundaryPath=FileNameJoin[{r,"boundaries.frm"}];
boundaryJob=Block[{CalcFormConverter`Private`$rcMasterMaximumTerms=4,
 CalcFormConverter`Private`$rcMasterMaximumDegree=4,
 CalcFormConverter`Private`$rcMaximumVariables=3},
 CalcFormExport[A0[m^2](x+y+z+u+v+w)^2/(1+x),boundaryPath]];
assert["boundary export",AssociationQ[boundaryJob]];
boundaryProgram=Import[boundaryPath,"Text"];
cut="* Collect complete denominator products while retaining shared numerators.";
assert["boundary insertion unique",StringCount[boundaryProgram,cut]===1];
boundaryProgram=First[StringSplit[boundaryProgram,cut]] <>
 Import[FileNameJoin[{DirectoryName[$InputFileName],"RationalMetadataBoundaries.frm"}],"Text"];
Export[boundaryPath,boundaryProgram,"Text"];
boundaryRun=RunProcess[{"/usr/local/bin/form","-q",boundaryPath}];
Export[FileNameJoin[{r,"boundaries.stdout"}],boundaryRun["StandardOutput"],"Text"];
Export[FileNameJoin[{r,"boundaries.stderr"}],boundaryRun["StandardError"],"Text"];
assert["boundary exit",boundaryRun["ExitCode"]===0&&boundaryRun["StandardError"]===""];
assert["boundary diagnostics",!StringContainsQ[ToLowerCase[boundaryRun["StandardOutput"]],{"fail","warning","error","illegal","fatal"}]];
assert["all boundaries completed",StringCount[boundaryRun["StandardOutput"],RegularExpression["(?m)^RBBOUND [^\\n]+ PASS$"]]===16 && StringContainsQ[boundaryRun["StandardOutput"],"RBBOUND COMPLETE 16"]];
checks+=176; (* 16 completed cases: readiness, eight fields, eligibility, metadata application. *)
Close[diag];assert["no messages",FileByteCount[FileNameJoin[{r,"diagnostics.log"}]]===0];
Export[FileNameJoin[{r,"focused-checks.json"}],<|"Assertions"->checks,"Fixtures"->ledgers|>,"RawJSON"];
finish["Rational metadata batching"];
