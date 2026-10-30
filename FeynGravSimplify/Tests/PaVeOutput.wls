$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
checks=<||>;
assert[label_,value_]:=(AssociateTo[checks,label->TrueQ[value]];If[!TrueQ[value],Print["FAIL\t",label]]);
SetAttributes[withResult,HoldRest];
withResult[value_,body_]:=Block[{mockResult=value},Internal`InheritedBlock[{CalcFormConverter`CalcFormCalculate},
 Clear[CalcFormConverter`CalcFormCalculate];CalcFormConverter`CalcFormCalculate[___]:=mockResult;body]];
plain[x___]:=PaVe[x,PaVeAutoOrder->False,PaVeAutoReduce->False];
rows={
 {plain[0,{}, {m^2}],A0[m^2]},
 {plain[0,0,{}, {m^2}],A00[m^2]},
 {plain[0,{s},{m^2,n^2}],B0[s,m^2,n^2]},
 {plain[1,{s},{m^2,n^2}],B1[s,m^2,n^2]},
 {plain[0,0,{s},{m^2,n^2}],B00[s,m^2,n^2]},
 {plain[1,1,{s},{m^2,n^2}],B11[s,m^2,n^2]},
 {plain[0,{s,t,u},{m^2,n^2,k^2}],C0[s,t,u,m^2,n^2,k^2]},
 {plain[0,{s,t,u,v,w,z},{m^2,n^2,k^2,h^2}],D0[s,t,u,v,w,z,m^2,n^2,k^2,h^2]}};
Do[
 assert["default named "<>ToString[j],withResult[rows[[j,1]],FeynGravSimplify[0]]===rows[[j,2]]];
 assert["explicit true "<>ToString[j],withResult[rows[[j,1]],FeynGravSimplify[0,PaVeToABCD->True]]===rows[[j,2]]];
 assert["false unchanged "<>ToString[j],withResult[rows[[j,1]],FeynGravSimplify[0,PaVeToABCD->False]]===rows[[j,1]]],{j,8}];
assert["default option",(PaVeToABCD/.Options[FeynGravSimplify])===True];
assert["existing option context",Context[PaVeToABCD]==="FeynCalc`"];
assert["raw calculate store unchanged",FreeQ[Options[CalcFormCalculate],PaVeToABCD]];
assert["raw export store unchanged",FreeQ[Options[FeynGravFORMExport],PaVeToABCD]];
Do[assert["passthrough "<>ToString[j],withResult[{0,True,False,Failure["fixture",<||>],x==y}[[j]],FeynGravSimplify[0]]==={0,True,False,Failure["fixture",<||>],x==y}[[j]]],{j,5}];
formal=LoopTensorCoefficient[{0},{},{},{},D];
unsupported=plain[1,2,{s,t,u},{m^2,n^2,k^2}];
assert["retained coefficients",withResult[formal+unsupported,FeynGravSimplify[0]]===formal+unsupported];
assert["mixed symbolic coefficient",withResult[(2+3 I) D rows[[1,1]]+formal,FeynGravSimplify[0]]===(2+3 I) D A0[m^2]+formal];
assert["altered coefficient negative control",withResult[2 rows[[1,1]],FeynGravSimplify[0]]=!=A0[m^2]];
counter=0;assert["delayed option once",withResult[rows[[1,1]],FeynGravSimplify[0,PaVeToABCD:>(counter++;True)]]===A0[m^2]&&counter===1];
assert["invalid Boolean",MatchQ[FeynGravSimplify[0,PaVeToABCD->Automatic],Failure["InvalidOption",_]]];
assert["duplicate first wins",withResult[rows[[1,1]],FeynGravSimplify[0,PaVeToABCD->False,PaVeToABCD->True]]===rows[[1,1]]];
store=Options[FeynGravSimplify];SetOptions[FeynGravSimplify,PaVeToABCD->False];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
assert["reload preserves false",withResult[rows[[1,1]],FeynGravSimplify[0]]===rows[[1,1]]];
assert["explicit overrides store",withResult[rows[[1,1]],FeynGravSimplify[0,PaVeToABCD->True]]===A0[m^2]];
Options[FeynGravSimplify]=store;
(* Conversion diagnostics are captured, not discarded; no FORM job is launched. *)
API03Fixture::conversion="Expected conversion diagnostic.";
stream=OpenWrite[FileNameJoin[{Directory[],"expected-conversion.log"}]];
failureResult=Internal`InheritedBlock[{FeynCalc`PaVeToABCD},Unprotect[FeynCalc`PaVeToABCD];Clear[FeynCalc`PaVeToABCD];
 FeynCalc`PaVeToABCD[x_]:=(Message[API03Fixture::conversion];x);
 Block[{$Messages={stream}},withResult[rows[[1,1]],FeynGravSimplify[0]]]];Close[stream];
assert["conversion diagnostic failure",MatchQ[failureResult,Failure["PaVeConversionFailed",_]]];
assert["diagnostic retained",StringContainsQ[Import[FileNameJoin[{Directory[],"expected-conversion.log"}],"Text"],"Expected conversion diagnostic"]];
abortResult=Internal`InheritedBlock[{FeynCalc`PaVeToABCD},Unprotect[FeynCalc`PaVeToABCD];Clear[FeynCalc`PaVeToABCD];FeynCalc`PaVeToABCD[x_]:=Abort[];
 withResult[rows[[1,1]],FeynGravSimplify[0]]];
assert["conversion abort",MatchQ[abortResult,Failure["Aborted",_]]];
prints={};
abortTimed=Block[{Print=(AppendTo[prints,{##}]&)},Internal`InheritedBlock[{FeynCalc`PaVeToABCD},Unprotect[FeynCalc`PaVeToABCD];Clear[FeynCalc`PaVeToABCD];FeynCalc`PaVeToABCD[x_]:=Abort[];
 withResult[rows[[1,1]],FeynGravSimplify[0,ShowTiming->True]]]];
assert["abort total status",MatchQ[abortTimed,Failure["Aborted",_]]&&Length[prints]===1&&MemberQ[Flatten[prints],"Aborted"]];
(* Real small serial jobs; unsupported cases use coefficient mode. *)
Do[
 ex={FAD[{l,m}],rows[[3,1]],rows[[7,1]],rows[[8,1]],rows[[4,1]],unsupported}[[j]];
 loops=If[j===1,{l},{}];
 reference=CalcFormCalculate[ex,LoopMomenta->loops,ReduceToScalarMasters->False,FORMThreads->1];
 old=FeynGravSimplify[ex,LoopMomenta->loops,ReduceToScalarMasters->False,FORMThreads->1,PaVeToABCD->False];
 new=FeynGravSimplify[ex,LoopMomenta->loops,ReduceToScalarMasters->False,FORMThreads->1];
 assert["real false parity "<>ToString[j],!FailureQ[old]&&old===reference];
 assert["real conversion "<>ToString[j],!FailureQ[new]&&new===PaVeToABCD[reference]];
 ,{j,6}];
Do[assert["restricted Boolean "<>ToString[b],FeynGravSimplify[MTD[mu,mu],Method->"FORMBenchmark",FORMThreads->1,PaVeToABCD->b]===D],{b,{True,False}}];
assert["ordinary output",FeynGravSimplify[MTD[mu,mu],FORMThreads->1]===D];
assert["Boolean output",FeynGravSimplify[True]===True];
assert["equality output",FeynGravSimplify[MTD[mu,mu]==D,FORMThreads->1]===True];
assert["named scalar round trip",FeynGravSimplify[A0[m^2]+B0[s,m^2,n^2],ReduceToScalarMasters->False,FORMThreads->1]===A0[m^2]+B0[s,m^2,n^2]];
SetAttributes[capturePartial,HoldAll];
capturePartial[body_]:=Module[{st,val,path=FileNameJoin[{Directory[],"partial-"<>CreateUUID[]<>".log"}]},st=OpenWrite[path];Block[{$Messages={st}},val=body];Close[st];{val,Import[path,"Text"]}];
partialFalse=capturePartial[FeynGravSimplify[1,LoopMomenta->{l},FORMThreads->1,PaVeToABCD->False]];
partialTrue=capturePartial[FeynGravSimplify[1,LoopMomenta->{l},FORMThreads->1]];
assert["formal scaleless retained",partialFalse[[1]]===partialTrue[[1]]&&!FailureQ[partialTrue[[1]]]];
assert["partial warning preserved",StringContainsQ[partialFalse[[2]],"partialmasters"]&&partialFalse[[2]]===partialTrue[[2]]];
prints={};timed=Block[{Print=(AppendTo[prints,{##}]&)},withResult[rows[[1,1]],FeynGravSimplify[0,ShowTiming->True]]];
assert["total includes conversion",timed===A0[m^2]&&Length[prints]===1&&StringContainsQ[ToString[prints,InputForm],"FeynGravSimplify total"]];
(* Final conversion of a retained small coefficient sum: measured separately,
   exact reference in the named basis and original sharing left to FeynCalc. *)
saved=Sum[(D+j) rows[[Mod[j,8]+1,1]],{j,1,64}];
expected=Sum[(D+j) rows[[Mod[j,8]+1,2]],{j,1,64}];
{seconds,converted}=AbsoluteTiming[withResult[saved,FeynGravSimplify[0]]];
assert["saved result equality",Expand[converted-expected]===0];
Export[FileNameJoin[{Directory[],"conversion-measurement.json"}],<|"Seconds"->seconds,"BeforeBytes"->ByteCount[saved],"AfterBytes"->ByteCount[converted]|>,"RawJSON"];
passed=Count[Values[checks],True];failed=Count[Values[checks],False];
Export[FileNameJoin[{Directory[],"baseline-result.json"}],<|"Completed"->True,"Passed"->passed,"Failed"->failed,"Checks"->checks|>,"RawJSON"];
Export[FileNameJoin[{Directory[],"test-completed"}],"PaVeOutput","Text"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"Completion.wl"}]];
writeSuiteCompletion["PaVeOutput",passed,failed];
Print["SUMMARY\tpassed=",passed,"\tfailed=",failed];Quit[If[failed===0,0,1]];
