(* Compare independent-metadata batching with the original per-definition path.
   The multi-batch case compares every emitted metadata field, not just timing.
   Native extra-symbol IDs may differ between FORM and TFORM: compare their
   reconstructed expressions exactly, while serial off/on output stays identical. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
r=testDirectory;
fail[x_]:=(Print["FAIL: ",InputForm[x]];Quit[1]);
assert[label_,v_]:=(checks++;If[!TrueQ[v],fail[label]]);
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
  job=Block[{CalcFormConverter`Private`$pgBatchMetadata=enabled},CalcFormExport[expr,path,Sequence@@opts]];
  assert[stem<>" export",AssociationQ[job]];
  program=Import[path,"Text"];
  program=StringReplace[program,"#call cfcPGRegister(def,`$index')\n"->("#call cfcPGRegister(def,`$index')\n"<>dump)];
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
    assert["TFORM diagnostics",!StringContainsQ[ToLowerCase[thread["StandardOutput"]],{"warning","error","illegal","fatal"}]];
    threadValue=CalcFormImport[job["ResultFile"],job["MappingFile"]];
    assert["TFORM exact result",SameQ[threadValue,Last[results]] || Together[threadValue-Last[results]]===0]];
 ,{enabled,{False,True}}];
 assert[label<>" same output",outputs[[1]]===outputs[[2]]];
 assert[label<>" same result",results[[1]]===results[[2]]];
 assert[label<>" same metadata",metadata[[1]]===metadata[[2]]];
 If[label==="many",assert["multiple batches exercised",Length[metadata[[1]]]>32*7]];
 If[label==="zero",assert["exact cancellation",results[[2]]===0]];
 AppendTo[ledgers,<|"Fixture"->label,"MetadataRecords"->Length[First[metadata]],"ExactResult"->True,"ExactOutput"->True|>];
,{fixture,fixtures}];

Export[FileNameJoin[{r,"focused-checks.json"}],<|"Assertions"->checks,"Fixtures"->ledgers|>,"RawJSON"];
finish["Shared metadata batching"];
