Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
(* Algebraic comparisons deliberately expose denominators only in these small
   tests. They neither integrate nor discard scaleless contributions. *)
equal[a_, b_] := !FailureQ[a] && Together[ExpandScalarProduct[
    FeynAmpDenominatorExplicit[FCI[a-b]]]] === 0;
inputs = {
 SPD[l] FAD[{l,m}],
 (SPD[l]-m^2) FAD[{l,m}],
 (SPD[l]-m^2)^4 FAD[{l,m}]^2,
 SPD[l,p] FAD[{l,m1},{l-p,m2}],
 SPD[l,p]^3 FAD[{l,m1},{l-p,m2}]^2,
 SPD[l,p] FAD[l,l-p,{l,m}],
 SPD[l,p] FAD[{l/2-2p,m},{-l,m}],
 SPD[l,r] FAD[{l,m1},{l-p,m2}],
 SPD[l] FAD[{l,a+b}],
 SPD[l] FAD[{l,Sqrt[a]}],
 SPD[l] FAD[{l,m}] FVD[p,mu],
 SPD[l] FAD[{l,m}] GAD[mu],
 SPD[l] FAD[{l,m}] SUNDelta[a,b],
 SPD[l] FAD[{l,m}] LCD[mu,nu,rho,sigma],
 (SPD[l]-m^2)FAD[{l,m}]+2,
 FAD[l,{l,m}] SPD[l]^3,
 SPD[l]FAD[{l,m}] + SPD[q]FAD[{q,n}],
 SPD[l,p]^2 FAD[{l,m1},{l-p,m1}],
 SPD[l] FAD[{l,I m}],
 SPD[l] FAD[{l,SMP["m_u"]}],
 (SPD[l]-m^2)^2 FAD[{l,m}]^3
};
Do[
 got=CalcFormCalculate[inputs[[k]],LoopMomenta->{l},FORMThreads->workers];
 assert["algebra "<>ToString[{workers,k}],equal[got,inputs[[k]]]];
 If[FailureQ[got],Print[InputForm[got]]],
 {workers,{1,2}},{k,Length[inputs]}];
got=CalcFormCalculate[SPD[l]FAD[{l,m}],FORMThreads->1];
assert["constant retained",got===1+m^2 FCI[FAD[{l,m}]]];
got=CalcFormCalculate[(SPD[l]-m^2)FAD[{l,m}],FORMThreads->1];
assert["exact complete cancellation",got===1];
got=CalcFormCalculate[SPD[l,p]FAD[{l,m1},{l-p,m2}],LoopMomenta->{l},FORMThreads->1];
expected=1/2(FAD[{l-p,m2}]-FAD[{l,m1}]+(SPD[p]+m1^2-m2^2)FAD[{l,m1},{l-p,m2}]);
assert["bubble numerator removed",FreeQ[got,Pair[Momentum[l,D],Momentum[p,D]]] && equal[got,expected]];
Do[
 ref=ApartFF[inputs[[k]],{l},FDS->False,DropScaleless->False];
 got=CalcFormCalculate[inputs[[k]],LoopMomenta->{l},FORMThreads->1];
 assert["FeynCalc reference "<>ToString[k],equal[got,ref]],{k,{1,2,3,4,5,6,7,8,18}}];
job=CalcFormExport[SPD[l]FAD[{l,m}],FileNameJoin[{testDirectory,"cancel.frm"}]];
text=Import[job["InputFile"],"Text"];
assert["cancellation before grouping",First[StringPosition[text,"Exact numerator cancellation"]][[1]] < First[StringPosition[text,"Bracket+"]][[1]]];
assert["version unchanged",Import[job["MappingFile"],"RawJSON"]["Version"]===1];
assert["no private objects",FreeQ[got,_Symbol?(StringStartsQ[SymbolName[#],"cfcPC"]&)]];
got=Block[{CalcFormConverter`Private`$pcMaximumBases=1},CalcFormCalculate[inputs[[4]],FORMThreads->1]];
assert["planning budget fallback",equal[got,inputs[[4]]]];
(* Independent routing families, context separation and growth fallback. *)
Do[
 ex=(SPD[l,p]+k SPD[q,l])^2 FAD[{l,k m},{q,m},{l-q-p,n}];
 got=CalcFormCalculate[ex,LoopMomenta->{l,q},FORMThreads->2];
 assert["two-loop routing "<>ToString[k],equal[got,ex]],{k,1,4}];
ex=(SPD[CancelOne`p]-m^2) FAD[{CancelOne`p,m}]+SPD[CancelTwo`p] FAD[{CancelTwo`p,n}];
got=CalcFormCalculate[ex,FORMThreads->1];
assert["separate contexts",equal[got,ex]];
ex=(SPD[l,p]+1)^7 FAD[l,l-p]^4;
baseline=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[ex,FORMThreads->1]];
got=CalcFormCalculate[ex,FORMThreads->2];
assert["growth fallback exact representation",got===baseline];
assert["growth fallback algebra",equal[got,ex]];
finish["Propagator cancellation"];
