(* PERF-03: exact registry semantics are covered by retained baseline comparisons;
   these regression checks exercise support discovery and reduction equivalence. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
plan[expr_] := CalcFormConverter`Private`tdPrepare[
 CalcFormConverter`Private`buildExportData[expr,Automatic,{q},Automatic,True],True,False];
supports[expr_] := plan[expr]["LoopFamilySupports"];
closure[x_] := CalcFormConverter`Private`lrReachableFamilies[x,4];
den[r_,mass_:m] := FAD[{r,mass}];
assert["sum alternatives",Length[supports[den[q]+den[q+p]]]===2];
assert["product of sums",Sort[Length /@ supports[(den[q]+den[q+p])(den[q+k]+den[q+r])]]==={2,2,2,2}];
assert["power mixes alternatives",Sort[Length /@ supports[(den[q]+den[q+p])^2]]==={1,1,2}];
assert["large power saturates supports",supports[(den[q]+den[q+p])^20]===supports[(den[q]+den[q+p])^2]];
assert["coincident rational routing",supports[den[q]den[-2q,2m]]==={{1}}];
assert["spectator denominator excluded",supports[den[q]den[p]]==={{1}}];
assert["unit support retained",MemberQ[supports[1+den[q]],{}]];
assert["rank independent support",supports[SPD[q,k]^6 den[q]den[q+p]]===supports[den[q]den[q+p]]];
boxDen=den[q]den[q+p]den[q+p+k]den[q+p+k-r]+den[q]den[q+k]den[q+p+k]den[q+p+k-r]+den[q]den[q+p]den[q+p-r]den[q+p+k-r];
boxPlan=plan[boxDen];boxFamilies=closure[boxPlan["LoopFamilySupports"]];
assert["three box family count",Length[boxFamilies]===31];
assert["three box family sizes",Table[Count[Length /@ boxFamilies,j],{j,4}]==={6,12,10,3}];
assert["no new saved metadata",FreeQ[Keys[boxPlan["Mapping"]],"LoopFamilySupports"|"DenominatorSupports"]];
(* The fallback deliberately over-approximates alternatives; it must not omit
   any factor when the bounded support analysis cannot retain every product. *)
raw=Total[Table[FeynAmpDenominator[PropagatorDenominator[Momentum[q+j p,D],m]],{j,17}]];
raw2=Total[Table[FeynAmpDenominator[PropagatorDenominator[Momentum[q+j k,D],m]],{j,17}]];
wide=CalcFormConverter`Private`tdDenominatorSupports[raw raw2,Function[x,HoldComplete[x]]];
assert["bounded support fallback",Length[wide]===1&&Length[First[wide]]===34];
SetAttributes[unpruned,HoldAll];
unpruned[body_] := Block[{CalcFormConverter`Private`tdDenominatorSupports,CalcFormConverter`Private`lrSavedSupports},
 Clear[CalcFormConverter`Private`tdDenominatorSupports,CalcFormConverter`Private`lrSavedSupports];
 CalcFormConverter`Private`tdDenominatorSupports[___]:=Automatic;
 CalcFormConverter`Private`lrSavedSupports[___]:=Automatic;body];
run[x_,opts___]:=CalcFormCalculate[x,opts,FORMThreads->1,TimeConstraint->25];
equal[x_,y_]:=!FailureQ[x]&&!FailureQ[y]&&FreeQ[{x,y},$Aborted]&&Together[x-y]===0;
pv[ids_,inv_,ms_]:=PaVe[Sequence@@ids,inv,ms,PaVeAutoOrder->False,PaVeAutoReduce->False];
fixtures={
 {"routed scalar sum",den[q]den[q+p]+den[q+k]den[q+r],{LoopMomenta->{q}}},
 {"factored repeated powers",(den[q,m]+den[q,M])^2,{LoopMomenta->{q}}},
 {"cancellation pinches",SPD[q]den[q]den[q+p]+SPD[q,p]den[q+k]den[q+k+p],{LoopMomenta->{q}}},
 {"rank two tensor",FVD[q,mu]FVD[q,nu](den[q]den[q+p]+den[q+k]den[q+k+p]),{LoopMomenta->{q}}},
 {"saved separate families",pv[{0,0},{s},{a,b}]+pv[{1,1},{t},{c,d}],{}},
 {"saved product",pv[{0,0},{s},{a,b}]pv[{1},{t},{c,d}],{}},
 {"saved triangle",pv[{0,0},{-2,-3,-4},{2,3,5}],{}},
 {"saved rank four box",pv[{0,0,0,0},{-2,-3,-4,-5,-6,-7},{2,3,5,7}],{}},
 {"power-only",LoopTensorCoefficient[{0},{0,p},{a,b},{2,1},D],{ReduceToScalarMasters->False,ReduceLoopCoefficients->True}}
};
Do[new=run[f[[2]],Sequence@@f[[3]]];old=unpruned[run[f[[2]],Sequence@@f[[3]]]];
 assert[f[[1]]<>" unpruned equality",equal[new,old]],{f,fixtures}];
x=den[q]den[q+p]+den[q+k]den[q+r];
assert["combined vs separate",equal[run[x,LoopMomenta->{q}],run[den[q]den[q+p],LoopMomenta->{q}]+run[den[q+k]den[q+r],LoopMomenta->{q}]]];
x=pv[{0,0},{s},{a,b}]+pv[{1,1},{t},{c,d}];
assert["independent FeynCalc bubble",Together[PaVeReduce[run[x]]-PaVeReduce[x]]===0];
Do[SPD[p]=value;actual=run[FVD[q,mu]den[q]den[q+p],LoopMomenta->{q}];
 reference=I Pi^2 FCI[FVD[p,mu]] PaVeReduce[pv[{1},{value},{m^2,m^2}]];
 assert["changed kinematics "<>ToString[value],Together[FCI[PaVeReduce[actual]-reference]]===0],{value,{-2,-3}}];
FCClearScalarProducts[];
expectedLog=FileNameJoin[{testDirectory,"expected-formal-diagnostic.log"}];
expectedStream=OpenWrite[expectedLog];
formalResult=Block[{$Messages={expectedStream}},run[1,LoopMomenta->{q}]];
Close[expectedStream];expectedText=Import[expectedLog,"Text"];
assert["formal polynomial preserved",formalResult===I Pi^2 LoopTensorCoefficient[{0},{},{},{},D] &&
 StringContainsQ[expectedText,"CalcFormImport::partialmasters"] && StringCount[expectedText,"::"]===1];
assert["polynomial dropped",run[1,LoopMomenta->{q},DropScaleless->True]===0];
assert["disabled decomposition",run[den[q],LoopMomenta->{q},TensorDecomposition->False]===FCI[den[q]]];
assert["serial parallel",run[x]===CalcFormCalculate[x,FORMThreads->2,TimeConstraint->25]];
finish["Export planning"];
