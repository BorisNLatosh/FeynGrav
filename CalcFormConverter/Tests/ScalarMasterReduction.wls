Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,q,k,r,s,a,b,m,M,mu,nu];
pv[ids_,inv_,ms_] := PaVe[Sequence@@ids,inv,ms,PaVeAutoOrder->False,PaVeAutoReduce->False];
run[x_,opts___] := CalcFormCalculate[x,FORMThreads->1,TimeConstraint->30,opts];
assert["default master option",(ReduceToScalarMasters/.Options[CalcFormCalculate])===True];
assert["export default master option",(ReduceToScalarMasters/.Options[CalcFormExport])===True];
assert["invalid master option",FailureQ[run[1,ReduceToScalarMasters->Automatic]]];
assert["no implicit integration",run[SPD[q]]===FCI[SPD[q]]];
x=pv[{0,0},{s},{a,b}];
assert["disabled saved coefficient",run[x,ReduceToScalarMasters->False]===x];
Do[
 x=pv[ids,{s},{a,b}];actual=run[x];
 assert["symbolic bubble "<>ToString[ids],!FailureQ[actual]&&TimeConstrained[Together[PaVeReduce[actual]-PaVeReduce[x]]===0,30,False]],
 {ids,{{1},{0,0},{1,1},{0,0,1},{1,1,1},{0,0,0,0}}}];
(* Fixed non-exceptional invariants keep independent high-rank checks bounded. *)
Do[
 ms=Take[{2,3,5,7},n];inv=Switch[n,1,{},2,{-2},3,{-2,-13/3,-3},4,{-2,-13/3,-38/5,-5,-3,-13/2}];
 Do[ids=If[rank===0,{0},If[OddQ[rank],Join[ConstantArray[0,rank-1],{1}],ConstantArray[0,rank]]];
  If[n===1&&OddQ[rank],Continue[]];x=pv[ids,inv,ms];actual=run[x];
  assert["rank "<>ToString[{n,rank}],!FailureQ[actual]&&TimeConstrained[Together[PaVeReduce[actual]-PaVeReduce[x]]===0,30,False]],
 {rank,0,6}],{n,1,4}];
(* Exact parameter identities at zero invariant; PaVeReduce can leave B1
   and B0 at this point, so it is not a complete reference for these checks. *)
actual=run[pv[{0,0},{0},{a,b}]];
assert["zero momentum unequal masses",Together[actual-(b pv[{0},{},{b}]-a pv[{0},{},{a}])/(D(b-a))]===0];
actual=run[pv[{1,1},{0},{a,a}]];
assert["zero momentum equal masses",Together[actual-(D-2)/(6a)pv[{0},{},{a}]]===0];
x=LoopTensorCoefficient[{0,0},{0,p},{0,0},{1,1},D];
actual=Quiet[run[x],CalcFormImport::partialmasters];
assert["scaleless pinch preserved",!FailureQ[actual]&&!FreeQ[actual,_LoopTensorCoefficient]];
actual=run[x,DropScaleless->True];
assert["scaleless pinch dropped",!FailureQ[actual]&&FreeQ[actual,_LoopTensorCoefficient]];
x=FVD[q,mu]FVD[q,nu]FAD[{q,m},{q+p,M}];actual=run[x,LoopMomenta->{q}];
reference=TID[FCI[x],q,ToPaVe->True];
assert["direct tensor normalisation",!FailureQ[actual]&&Together[PaVeReduce[actual]-PaVeReduce[reference]]===0];
assert["disabled integration",run[FAD[{q,m}],LoopMomenta->{q},TensorDecomposition->False]===FCI[FAD[{q,m}]]];
x=pv[{0,0},{s},{a,b}]+pv[{1},{t},{c,d}];
assert["independent invariant families",Together[PaVeReduce[run[x]]-PaVeReduce[x]]===0];
assert["serial parallel",run[x]===CalcFormCalculate[x,FORMThreads->2,TimeConstraint->30]];
job=CalcFormExport[x,FileNameJoin[{testDirectory,"master.frm"}]];
assert["manual export",AssociationQ[job]];
assert["manual execute",RunProcess[{"form",job["InputFile"]}]["ExitCode"]===0];
assert["manual agreement",CalcFormImport[job["ResultFile"],job["MappingFile"]]===run[x]];
map=Import[job["MappingFile"],"RawJSON"];
assert["version eight",map["Version"]===8];
Do[assert["chunk "<>ToString[size],Block[{CalcFormConverter`Private`$pgReadChunkSize=size},CalcFormImport[job["ResultFile"],job["MappingFile"]]]===run[x]],{size,{1,3,17}}];
i=0;
(* Optional state sharing must preserve complete coefficient expressions. *)
Do[
 x=expr;
 shared=run[x];unshared=Block[{CalcFormConverter`Private`$smShareStates=False},run[x]];
 assert["state sharing "<>ToString[i++],!FailureQ[shared]&&!FailureQ[unshared]&&Together[shared-unshared]===0],
 {expr,{pv[{0,0},{s},{a,b}]+pv[{1,1},{s},{a,b}],pv[{1,1,1},{-2,-3,-4},{2,3,5}],pv[{0,0,0,0},{-2,-3,-4,-5,-6,-7},{2,3,5,7}]}}];
originalMap=map;originalOutput=Import[job["ResultFile"],"Text"];
writeBody[body_,metadata_] := Module[{json=ExportString[metadata,"RawJSON","Compact"->True]},
 Export[job["MappingFile"],json,"Text"];
 Export[job["ResultFile"],"CFC"<>ToString[metadata["Version"]]<>" "<>Hash[json,"SHA256","HexString"]<>"\nCFC_SHARED 0\nCFC_BODY\n"<>body,"Text"]];
Do[broken=originalMap;broken["ScalarMasterReduction"]=Join[broken["ScalarMasterReduction"],bad];writeBody["0",broken];
 assert["invalid master metadata "<>ToString[bad],FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {bad,{<|"Method"->"Unknown"|>,<|"SourceVersion"->5|>,<|"MaximumPoints"->5|>,<|"Dimension"->{"Integer","4"}|>,<|"GenericDenominators"->5|>,<|"ExceptionalFamilies"->{{999}}|>}}];
Do[writeBody[body,originalMap];assert["reject intermediate "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"cfcIR(1,-2)","cfcSMHold(1,-2)","cfcTC(1,-2,1)","unknown(","1/0"}}];
assert["failed streams closed",FreeQ[Streams[],_InputStream?(StringContainsQ[First[#],"master.out"]&)]];
writeBody["0",originalMap];assert["retry after invalid input",CalcFormImport[job["ResultFile"],job["MappingFile"]]===0];
(* Wrapped integers are valid FORM output. Identifier joining remains invalid. *)
large=(10^240+1234567)/(10^181+123);
assert["large exact scalar",run[large]===large];
assert["numeric continuation",CalcFormConverter`Private`normaliseFORMNumbers["123\\\n 456\\\r\n 789"]==="123456789"];
assert["identifier continuation retained",CalcFormConverter`Private`normaliseFORMNumbers["cfs1\\\n 2"]==="cfs1\\\n 2"];
x=pv[{1,1,1,1,1,1},{-2,-13/3,-38/5,-5,-3,-13/2},{2,3,5,7}];
shared=run[x];unshared=Block[{CalcFormConverter`Private`$smShareStates=False},run[x]];
assert["rank six long rational box",!FailureQ[shared]&&!FailureQ[unshared]&&Together[shared-unshared]===0];
x=pv[{0,0},{0,0,0},{a,a,a}];
actual=Quiet[run[x],CalcFormExport::partialmasters];
assert["singular saved coefficient retained",actual===x && FreeQ[actual,_CalcFormConverter`Private`smChord]];
x=LoopTensorCoefficient[{0},{0},{m^2},{2},4];
assert["nonstandard dimension retained",Quiet[run[x],CalcFormExport::partialmasters]===x];

(* Numeric continuations must also survive streaming chunk boundaries. *)
x=large pv[{0,0},{s},{a,b}];
longJob=CalcFormExport[x,FileNameJoin[{testDirectory,"long-master.frm"}]];
assert["long grouped export",AssociationQ[longJob]];
assert["long grouped execute",RunProcess[{"form",longJob["InputFile"]}]["ExitCode"]===0];
longReference=large run[pv[{0,0},{s},{a,b}]];
Do[longActual=Block[{CalcFormConverter`Private`$pgReadChunkSize=size},CalcFormImport[longJob["ResultFile"],longJob["MappingFile"]]];
 assert["long numeric chunk "<>ToString[size],!FailureQ[longActual]&&Together[longActual-longReference]===0],{size,{1,3,17}}];
(* Check independent matrix and epsilon spectators under both algebra modes. *)
coefficient=pv[{0,0},{s},{a,b}];
spectator=GAD[mu].GAD[nu] SUNDelta[aa,bb] Eps[LorentzIndex[rho,D],LorentzIndex[sigma,D],LorentzIndex[tau,D],LorentzIndex[lambda,D]];
Do[actual=run[coefficient spectator,DiracAlgebra->da,ColourAlgebra->ca];
 reference=run[coefficient]run[spectator,DiracAlgebra->da,ColourAlgebra->ca];
 assert["matrix epsilon spectators "<>ToString[{da,ca}],!FailureQ[actual]&&!FailureQ[reference]&&Together[actual-reference]===0],
 {da,{False,Automatic}},{ca,{False,True}}];
(* Kinematic snapshots and caches must be rebuilt for each export. *)
Do[SPD[p,p]=value;
 actual=run[LoopTensorCoefficient[{0,0},{0,p},{a,b},{1,1},D]];
 reference=pv[{0,0},{value},{a,b}];
 assert["changed scalar products "<>ToString[value],!FailureQ[actual]&&Together[PaVeReduce[actual]-PaVeReduce[reference]]===0],{value,{-2,-3}}];
FCClearScalarProducts[];
x=pv[{1},Range[10],{a,b,c,d,e}];
assert["higher point retained",Quiet[run[x],CalcFormExport::partialmasters]===x];
broken=originalMap /. {"MasterChord",n_,j_,invariants_}:>{"MasterChord",n,999,invariants};
writeBody["0",broken];
assert["invalid invariant chord",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];

assert["scalar bubble permutation",run[pv[{0},{s},{a,b}]-pv[{0},{s},{b,a}]]===0];
assert["scalar triangle permutation",run[pv[{0},{s,t,u},{a,b,c}]-pv[{0},{s,u,t},{b,a,c}]]===0];
(* Loop-dependent slashes, scalar products and epsilon slots must reduce
   to the same independently reduced B1 coefficient. *)
Do[
 input=numerator FAD[{q,m},{q+p,M}];
 actual=run[input,LoopMomenta->{q}];
 reference=I Pi^2 FCI[numerator/.q->p] pv[{1},{SPD[p]},{m^2,M^2}];
 assert["loop tensor exposure "<>ToString[Head[numerator]],!FailureQ[actual]&&
   TimeConstrained[Together[PaVeReduce[actual]-PaVeReduce[reference]]===0,30,False]],
 {numerator,{GSD[q],SPD[q,k],Eps[Momentum[q,D],LorentzIndex[mu,D],LorentzIndex[nu,D],LorentzIndex[rho,D]]}}];
finish["Scalar master reduction"];
