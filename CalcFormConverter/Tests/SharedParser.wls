Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
values=<|"cfs1"->a,"cfs2"->b,"cfv1"->CalcFormConverter`Private`vectorToken[p],
 "cfv2"->CalcFormConverter`Private`vectorToken[q],"cfi1"->CalcFormConverter`Private`indexToken[mu],
 "cfi2"->CalcFormConverter`Private`indexToken[nu]|>;
reference[t_] := Catch[CalcFormConverter`Private`parseGeneralResult[t,values,D],CalcFormConverter`Private`$failureTag];
candidate[t_] := Catch[CalcFormConverter`Private`withImportParser[values,D,
 CalcFormConverter`Private`pgParseNestedChecked[t,Function[s,CalcFormConverter`Private`parseResult[s,values,D]]]],CalcFormConverter`Private`$failureTag];
Do[assert["shared differential "<>t,SameQ[candidate[t],reference[t]]],
 {t,{"cfs1*(cfs1+cfs2)+cfs2*(cfs1-cfs2)","cfv1.cfv2*(cfs1^2+cfs2^2)+cfv1.cfv1*(cfs1+cfs2)",
 "cfv1(cfi1)*(cfs1+cfs2)+d_(cfi1,cfi2)*(cfs1-cfs2)",
 "cfs1^ -2*(cfs1+cfs2)-cfs2*(cfs1+2)","(cfs1+cfs2)^2","(cfs1+cfs2)/(cfs1-cfs2)",
 "cfA0(cfs1)*(cfs1+cfs2)","0*cfv1+unknown", "1/0+unknown", "unknown+1/0",
 "cfs1*(unknown+1)+other*(2+3)","cfs1*(1+2)+unknown*(2+@)",
 "d_(cfi1,cfv1)*(cfs1+cfs2)","cfv1(cfi1)^2*(cfs1+cfs2)","cfs1**(1+2)","cfs1*(1+)"}}];
(* Exercise the actual fast-path dispatch for a polynomial below 128 KiB. *)
text=StringRiffle[Table[ToString[k]<>"*cfs1^2*cfs2",{k,1,12}],"+"];
seen=0;
Internal`InheritedBlock[{CalcFormConverter`Private`parseFlatResult},
 DownValues[CalcFormConverter`Private`parseFlatResult]=DownValues[CalcFormConverter`Private`parseFlatResult]/.
  HoldPattern[RuleDelayed[l_,r_]]:>RuleDelayed[l,(seen++;r)];
 assert["small leaf value",candidate[text]===reference[text]]];
assert["small leaf fast path used",seen>0];
CalcFormConverter`Private`withImportParser[values,D,
 x=CalcFormConverter`Private`importFactor["cfs1^2"];
 y=CalcFormConverter`Private`importFactor["cfs1^2"];
 assert["successful factor reused",x===a^2&&y===a^2&&Length[CalcFormConverter`Private`$importFactorCache]===1];
 Catch[CalcFormConverter`Private`importFactor["unknown"],CalcFormConverter`Private`$failureTag];
 assert["integers do not fill cache",Length[CalcFormConverter`Private`$importFactorCache]===1 && CalcFormConverter`Private`importFactor["12345"]===12345 && Length[CalcFormConverter`Private`$importFactorCache]===1];
 assert["failure not cached",!KeyExistsQ[CalcFormConverter`Private`$importFactorCache,"unknown"]];
 Block[{CalcFormConverter`Private`$importParserCacheEntries=1},
  CalcFormConverter`Private`importFactor["cfs2^2"];
  assert["factor count bounded",Length[CalcFormConverter`Private`$importFactorCache]<=1];
  CalcFormConverter`Private`importTokenClass["1","nevermatch"];
  CalcFormConverter`Private`importTokenClass["2","nevermatch"];
  assert["class count bounded",Length[CalcFormConverter`Private`$importClassCache]<=1]];
 Block[{CalcFormConverter`Private`$importParserCacheBytes=1},
  before=CalcFormConverter`Private`$importFactorCache;
  CalcFormConverter`Private`importFactor["cfs1^3"];
  assert["oversized entry not retained",before===CalcFormConverter`Private`$importFactorCache]]];
assert["session state released",!TrueQ[CalcFormConverter`Private`$importParserActive]&&!ValueQ[CalcFormConverter`Private`$importFactorCache]&&!ValueQ[CalcFormConverter`Private`$importClassCache]];
changed=Join[values,<|"cfs1"->c|>];
x=CalcFormConverter`Private`withImportParser[changed,D,CalcFormConverter`Private`importFactor["cfs1^2"]];
assert["dictionary isolation",x===c^2];
FCClearScalarProducts[];
SPD[p,p]=u;
x=candidate["cfv1.cfv1*(cfs1+cfs2)"];
SPD[p,p]=v;
y=candidate["cfv1.cfv1*(cfs1+cfs2)"];
assert["changed scalar products",x===u(a+b)&&y===v(a+b)];
FCClearScalarProducts[];
x=CheckAbort[CalcFormConverter`Private`withImportParser[values,D,CalcFormConverter`Private`importFactor["cfs1^2"];Abort[]],$Aborted];
assert["abort releases shared state",x===$Aborted&&!TrueQ[CalcFormConverter`Private`$importParserActive]&&!ValueQ[CalcFormConverter`Private`$importFactorCache]];
(* Public import must not enable cache-based reassociation for user UpValues. *)
job=CalcFormExport[FAD[p](a+b),FileNameJoin[{testDirectory,"upvalue-guard.frm"}]];
json=Import[job["MappingFile"],"Text"];mapping=Import[job["MappingFile"],"RawJSON"];
entryName[expr_] := SelectFirst[mapping["Entries"],CalcFormConverter`Private`decode[#["Expression"]]===expr&]["Name"];
Export[job["ResultFile"],"CFC1 "<>Hash[json,"SHA256","HexString"]<>"\n"<>
 entryName[FCI[FAD[p]]]<>"*("<>entryName[a]<>"+"<>entryName[b]<>")","Text"];
Internal`InheritedBlock[{a},
 a /: upvalueProbe[a] := 1;
 got=Block[{CalcFormConverter`Private`withImportParser},
  CalcFormConverter`Private`withImportParser[___]:=Throw["Unexpected shared parser"];
  Catch[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
 assert["UpValues retain original import path",got===FCI[FAD[p](a+b)]]];
(* A nested public import must not reuse its caller's dictionary. *)
plain=CalcFormExport[a+b,FileNameJoin[{testDirectory,"nested-public.frm"}]];
plainJSON=Import[plain["MappingFile"],"Text"];plainMap=Import[plain["MappingFile"],"RawJSON"];
plainName[expr_] := SelectFirst[plainMap["Entries"],CalcFormConverter`Private`decode[#["Expression"]]===expr&]["Name"];
Export[plain["ResultFile"],"CFC1 "<>Hash[plainJSON,"SHA256","HexString"]<>"\n"<>plainName[a]<>"+"<>plainName[b],"Text"];
got=CalcFormConverter`Private`withImportParser[changed,D,CalcFormImport[plain["ResultFile"],plain["MappingFile"]]];
assert["nested public import dictionary isolation",got===a+b];
(* Exercise bulk reconstruction directly below its usual size threshold.
   Failure equality includes metadata and first-failure order. *)
bulkCandidate[t_] := Catch[CalcFormConverter`Private`withImportParser[values,D,
 Block[{CalcFormConverter`Private`$flatParserMinimumReusePerDistinctFactor=1},
  With[{packet=CalcFormConverter`Private`prepareFlatResult[t]},
   If[Head[packet]===CalcFormConverter`Private`flatTokenData,
    CalcFormConverter`Private`parseFlatResult[packet,values,D],
    CalcFormConverter`Private`parseGeneralResult[t,values,D]]]]],CalcFormConverter`Private`$failureTag];
Do[assert["bulk differential "<>t,SameQ[bulkCandidate[t],reference[t]]],
 {t,{"1", "-1", "cfs1", "cfs1*cfs2/2/3-cfs1/7+cfs2", "0*cfs1+2",
 "cfv1.cfv2*cfs1-cfs2*cfv1.cfv2", "d_(cfi1,cfi2)*cfs1+cfv1(cfi1)*cfs2",
 "1/0+cfs999", "cfs999+1/0", "0^0+cfs999", "cfs1/0+cfs999^2",
 "cfv999.cfv1+1/0", "1/0+cfv999.cfv1", "cfs1^ -2*cfs2/3+cfs1",
 "cfs1**cfs2", "cfs1 cfs2", "1/0+cfv1", "cfv1+1/0"}}];
BlockRandom[SeedRandom[761];
 Do[text=StringRiffle[Table[
   StringRiffle[RandomChoice[{"cfs1","cfs2","2","3","cfs1^2","cfv1.cfv2"},4],RandomChoice[{"*","/"}]],{5}],
   RandomChoice[{"+","-"}]];
  assert["bulk polynomial "<>ToString[k],SameQ[bulkCandidate[text],reference[text]]],{k,100}]];
finish["Shared parser"];
