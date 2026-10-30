$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
checks=<||>;
assert[label_,v_]:=(AssociateTo[checks,label->TrueQ[v]];If[!TrueQ[v],Print["FAIL\t",label]]);
finish[]:=(passed=Count[Values[checks],True];failed=Count[Values[checks],False];Export["baseline-result.json",<|"Completed"->True,"Passed"->passed,"Failed"->failed,"Checks"->checks|>,"RawJSON"];Export["test-completed","Reuse","Text"];Print["SUMMARY\tpassed=",passed,"\tfailed=",failed];Quit[If[failed===0,0,1]]);
template=Import[FileNameJoin[{root,"CalcFormConverter","Templates","CoefficientCollection.frm.in"}],"Text"];
powerRules=StringRiffle[CalcFormConverter`Private`rcPowerRules[{"s","t","D"},"cfcRat"],"\n"];
headers="Off Statistics;\nSymbols s,t,D,a,cfcRPower,cfcRNum,cfcRDen;\nCFunctions cfcRat,cfcLRNum,cfcLRDen;\n";
procedure[reuse_,budget_,capacity_,terms_,sign_]:=StringReplace[template,{"@REUSE@"->ToString[reuse],"@WORK@"->ToString[budget],"@CACHE@"->ToString[capacity],"@LIMIT@"->ToString[terms],"@UNSAFE@"->"occurs(i_)","@MONOMIALS@"->"","@RULES@"->("id a=cfcRat(1,s"<>sign<>"t);\n"<>powerRules<>"\nMultiply cfcRat(1,1);"),"@RESTORE@"->""}];
checkState[label_,work_,hits_]:="#if (`$cfcCCWorkUsed' == "<>ToString[work]<>") && (`$cfcCCHits' == "<>ToString[hits]<>")\n#write \"PASS "<>label<>"\"\n#else\n#write \"FAIL "<>label<>"\"\n#endif\n";
checkExact[label_,expr_,expected_,wantZero_:True]:="Skip;\nLocal Residual="<>expr<>"-("<>expected<>");\n.sort\nPolyRatFun cfcRat;\nSkip;\nNSkip Residual;\nid cfcLRNum(cfcRNum?)=cfcRat(cfcRNum,1);\nid cfcLRDen(cfcRDen?)=cfcRat(1,cfcRDen);\n"<>powerRules<>"\nMultiply cfcRat(1,1);\n.sort\nPolyRatFun;\n#if termsin(Residual) "<>If[wantZero,"==","!="]<>" 0\n#write \"PASS "<>label<>"\"\n#else\n#write \"FAIL "<>label<>"\"\n#endif\nDrop Residual;\n.sort\nNSkip;\n";
call[name_,expr_]:="Local "<>name<>"="<>expr<>";\n.sort\n#call cfcCollectScalar("<>name<>")\n";
run[label_,body_,expectedCount_,reuse_:1,budget_:1024,capacity_:64,terms_:1000,sign_:"+",binary_:"form"]:=Module[{code,proc,lines},
code=headers<>CalcFormConverter`Private`rcWorkBudget[]<>procedure[reuse,budget,capacity,terms,sign]<>"\n"<>body<>".end\n";
Export[label<>".frm",code,"Text"];
proc=RunProcess[If[binary==="form",{"form","-q",label<>".frm"},{"tform","-w2","-q",label<>".frm"}]];
Export[label<>".stdout",proc["StandardOutput"],"Text"];Export[label<>".stderr",proc["StandardError"],"Text"];
If[proc["ExitCode"]=!=0||proc["StandardError"]=!=""||StringContainsQ[ToLowerCase[proc["StandardOutput"]],"warning"|"error"|"illegal"|"fail"],Print["FAIL\t FORM ",label,"\n",proc["StandardOutput"],proc["StandardError"]];Quit[1]];
lines=Select[StringSplit[proc["StandardOutput"],"\n"],StringStartsQ[#,"PASS "]&];
assert[label<>" count",Length[lines]===expectedCount];Do[assert[label<>" "<>line,True],{line,lines}]];
body=call["F1","1/s+1/t"]<>checkState["first",1,0]<>checkExact["first-exact","F1","(s+t)/(s*t)"]<>
call["F2","1/t+1/s"]<>checkState["unchanged",1,1]<>checkExact["unchanged-exact","F2","(s+t)/(s*t)"]<>
call["F3","1/s+1/t+1"]<>checkState["changed",2,1]<>checkExact["changed-exact","F3","(s+t+s*t)/(s*t)"]<>
call["F4","F1"]<>checkState["settled-output",2,2]<>checkExact["settled-exact","F4","(s+t)/(s*t)"]<>
call["F5","i_*(1/s+1/t)"]<>checkState["reject",3,2]<>
call["F6","i_*(1/s+1/t)"]<>checkState["reject-hit",3,3]<>checkExact["reject-exact","F6","i_*(s+t)/(s*t)"]<>
call["F7","D/s+1/t"]<>checkState["dimension-change",4,3]<>checkExact["dimension-exact","F7","(D*t+s)/(s*t)"]<>
call["F8","0"]<>checkState["zero-stop",4,3]<>checkExact["zero-exact","F8","0"]<>
checkExact["negative-control","F1","(s+t)/(s*t)+1",False];
run["core",body,16];
run["threaded",body,16,1,1024,64,1000,"+","tform"];
run["capacity",call["A","1/s+1/t"]<>call["B","1/s+1/t"]<>checkState["hit-at-capacity",1,1]<>
call["C","1/s+1/t+1"]<>checkState["miss-at-capacity",1,1]<>checkExact["fallback-exact","C","1/s+1/t+1"],3,1,1024,2];
run["visits",call["A","1/s+1/t"]<>call["B","1/s+1/t"]<>call["C","1/s+1/t+1"]<>
checkState["exhaustion",1,1]<>checkExact["exhausted-exact","C","1/s+1/t+1"],2,1,2];
run["terms",call["A","1/s+1/t"]<>checkState["over-term-limit",0,0]<>
call["B","1"]<>checkState["at-term-limit",1,0]<>call["C","1"]<>checkState["constant-hit",1,1]<>checkExact["constant-exact","C","1"],4,1,1024,64,1];
run["disabled",call["A","1/s+1/t"]<>call["B","1/s+1/t"]<>checkState["reference-repeats",2,0]<>checkExact["reference-exact","B","(s+t)/(s*t)"],2,0];
run["context-plus",call["A","a"]<>checkState["fresh-context",1,0]<>checkExact["context-exact","A","cfcRat(1,s+t)"],2];
run["context-minus",call["A","a"]<>checkState["fresh-context",1,0]<>checkExact["context-exact","A","cfcRat(1,s-t)"],2,1,1024,64,1000,"-"];
run["zero-budget",call["A","1/s+1/t"]<>checkState["no-work",0,0]<>checkExact["zero-budget-exact","A","1/s+1/t"],2,1,0];
run["zero-capacity",call["A","1/s+1/t"]<>checkState["no-storage",0,0]<>checkExact["zero-capacity-exact","A","1/s+1/t"],2,1,1024,0];
(* Execute after the unchanged CoefficientReuse body, reusing its native helpers. *)
Do[
 cap=capacity; admitted=If[cap<2,0,If[cap<4,1,2]];
 body=call["A","1/s+1/t"]<>call["B","1/s+1/t+1"]<>
 checkState["reserved-slots",admitted,0]<>
 "#if (`$cfcCCEntries' <= "<>ToString[cap]<>") && (`$cfcCCEntries' == "<>ToString[2admitted]<>")\n#write \"PASS capacity-bounded\"\n#else\n#write \"FAIL capacity-bounded\"\n#endif\n"<>
 checkExact["whole-fallback","B","1/s+1/t+1"];
 run["capacity-boundary-"<>ToString[cap],body,3,1,1024,cap],
 {capacity,{0,1,2,3,4,5}}];
Do[
 cap=capacity; admitted=Min[3,Quotient[cap,2]];
 body="Local A=1;\n.sort\n#call cfcReserveFactors(2)\n#call cfcReserveFactors(2)\n#call cfcReserveFactors(2)\n"<>
 "#if `$cfcCWFactorUsed' <= "<>ToString[cap]<>"\n#write \"PASS factor-bounded\"\n#else\n#write \"FAIL factor-bounded\"\n#endif\n"<>
 "#if `$cfcCWFactorUsed' == "<>ToString[2admitted]<>"\n#write \"PASS factor-used\"\n#else\n#write \"FAIL factor-used\"\n#endif\n"<>
 "#if `$cfcCWFactorDenied' == "<>ToString[3-admitted]<>"\n#write \"PASS factor-denied\"\n#else\n#write \"FAIL factor-denied\"\n#endif\n";
 Block[{CalcFormConverter`Private`$rcMaximumFactorisations=cap},run["factor-boundary-"<>ToString[cap],body,3]],
 {capacity,{0,1,2,3,4,64}}];
body=call["A","1/s-a-t*a/s"]<>call["B","1/s-a-t*a/s"]<>
 checkState["cached-zero-hit",1,1]<>checkExact["cached-zero-exact","B","0"]<>
 call["C","1/s-a-t*a/s+1"]<>checkState["changed-zero-input",2,1]<>checkExact["changed-zero-exact","C","1"];
run["cached-zero",body,4];

finish[];
