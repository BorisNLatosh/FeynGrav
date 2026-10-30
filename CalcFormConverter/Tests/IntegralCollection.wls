$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
checks=<||>;
assert[label_,value_]:=(AssociateTo[checks,label->TrueQ[value]];If[!TrueQ[value],Print["FAIL\t",label]]);
finish[]:=(passed=Count[Values[checks],True];failed=Count[Values[checks],False];
 Export[FileNameJoin[{Directory[],"baseline-result.json"}],<|"Completed"->True,"Passed"->passed,"Failed"->failed,"Checks"->checks|>,"RawJSON"];
 Export[FileNameJoin[{Directory[],"test-completed"}],"IntegralCollection","Text"];
 Print["SUMMARY\tpassed=",passed,"\tfailed=",failed];Quit[If[failed===0,0,1]]);
SetAttributes[withoutCollection,HoldAll];
withoutCollection[body_]:=Block[{CalcFormConverter`Private`$lrCollectCoefficients=False},body];
Clear[s,t,u,a,b,m,p,q,l,mu,nu];
master=PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
second=LoopTensorCoefficient[{0},{0},{m^2},{2},D];
zero=1/s-1/(s+t)-t/(s(s+t));
equal[x_,y_]:=!FailureQ[x]&&!FailureQ[y]&&Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[x-FCI[y]]]]===0;
manual[ex_,label_,enabled_,opts___]:=Module[{job,proc,result},
 job=Block[{CalcFormConverter`Private`$lrCollectCoefficients=enabled},CalcFormExport[ex,FileNameJoin[{Directory[],label<>".frm"}],opts]];
 If[!AssociationQ[job],Print["FAIL\t export ",label," ",InputForm[job]];Quit[1]];
 proc=RunProcess[{"form","-q",job["InputFile"]}];
 Export[FileNameJoin[{Directory[],label<>".stdout"}],proc["StandardOutput"],"Text"];
 Export[FileNameJoin[{Directory[],label<>".stderr"}],proc["StandardError"],"Text"];
 If[proc["ExitCode"]=!=0||proc["StandardError"]=!=""||StringContainsQ[ToLowerCase[proc["StandardOutput"]],"warning"|"error"|"illegal"],Print["FAIL\t FORM ",label];Quit[1]];
 result=CalcFormImport[job["ResultFile"],job["MappingFile"]];
 If[FailureQ[result],Print["FAIL\t import ",label," ",InputForm[result]];Quit[1]];
 {result,job,proc["StandardOutput"]}];
rows={
 {zero master,0,{}},
 {(1/s+1/t)master,(s+t)/(s t)master,{}},
 {(D^2-1)/(D-1)master,(D+1)master,{}},
 {zero FAD[{l,m}],0,{LoopMomenta->{l}}},
 {(1/s+1/t)FAD[{l,m}],I Pi^2 (s+t)/(s t)master,{LoopMomenta->{l}}},
 {second-(D-2)/(2m^2)master,0,{}},
 {second/(s+t)-(D-2)/(2m^2(s+t))master,0,{ReduceToScalarMasters->False,ReduceLoopCoefficients->True}},
 {master+zero second,master,{}},
 {1+zero master,1,{}},
 {((s+t)^2/(s t)-s/t-t/s-2)master,0,{}}
};
Do[can=manual[rows[[j,1]],"candidate-"<>ToString[j],True,Sequence@@rows[[j,3]]];
 ref=manual[rows[[j,1]],"reference-"<>ToString[j],False,Sequence@@rows[[j,3]]];
 assert["exact reference "<>ToString[j],equal[can[[1]],ref[[1]]]];
 assert["explicit result "<>ToString[j],equal[can[[1]],rows[[j,2]]]];
 assert["mapping unchanged "<>ToString[j],Import[can[[2,"MappingFile"]],"RawJSON"]===Import[ref[[2,"MappingFile"]],"RawJSON"]];
 If[j===1,assert["zero key removed before recurrence",StringContainsQ[can[[3]],"CFC_MASTER_REQUESTED 0"]&&StringContainsQ[ref[[3]],"CFC_MASTER_REQUESTED 1"]]];
 If[MemberQ[{1,4,6,7,10},j],assert["literal zero "<>ToString[j],can[[1]]===0]];
 ,{j,Length[rows]}];
assert["negative control",!equal[master,2master]];

rows={
 {(2+3 I)(1/s+1/t)master,{},True},
 {zero master MTD[mu,nu],{},True},
 {zero master GAD[mu].GAD[nu],{DiracAlgebra->False},True},
 {Sqrt[s](1/s+1/t)master,{},True},
 {master second,{},True},
 {(1/s+1/t)FAD[{l,m}],{LoopMomenta->{l},TensorDecomposition->False},False},
 {(1/s+1/t)master,{ReduceToScalarMasters->False,ReduceLoopCoefficients->False},False},
 {zero master SPD[p,Polarization[q,I]],{},True}
};
Do[can=manual[rows[[j,1]],"protected-candidate-"<>ToString[j],True,Sequence@@rows[[j,2]]];
 ref=manual[rows[[j,1]],"protected-reference-"<>ToString[j],False,Sequence@@rows[[j,2]]];
 assert["protected exact "<>ToString[j],equal[can[[1]],ref[[1]]]];
 assert["protected mapping "<>ToString[j],Import[can[[2,"MappingFile"]],"RawJSON"]===Import[ref[[2,"MappingFile"]],"RawJSON"]];
 assert["no leaked collection object "<>ToString[j],FreeQ[can[[1]],_Symbol?(StringStartsQ[SymbolName[#],"cfcCC"]&)]];
 ,{j,Length[rows]}];
Do[got=Block[{CalcFormConverter`Private`$rcPolarisationMaximumRationalPasses=n},CalcFormCalculate[(1/s+1/t)master,FORMThreads->1]];
 assert["work limit retains full result "<>ToString[n],equal[got,(s+t)/(s t)master]],{n,{0,1,2}}];
got=Block[{CalcFormConverter`Private`$rcMaximumTerms=1},CalcFormCalculate[zero master,FORMThreads->1]];
assert["term limit exact",equal[got,0]];
assert["TFORM exact",equal[CalcFormCalculate[(1/s+1/t)master,FORMThreads->2],(s+t)/(s t)master]];
assert["API03 conversion",equal[FeynGravSimplify[(1/s+1/t)master,FORMThreads->1],(s+t)/(s t)A0[m^2]]];
assert["API03 false",equal[FeynGravSimplify[(1/s+1/t)master,PaVeToABCD->False,FORMThreads->1],(s+t)/(s t)master]];
assert["ordinary constant",CalcFormCalculate[1,FORMThreads->1]===1];
finish[];
