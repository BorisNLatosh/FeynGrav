Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[s,t,u,v,a,b,m,M,mu,nu,l];
pv[inv_,ms_]:=PaVe[0,inv,ms,PaVeAutoOrder->False,PaVeAutoReduce->False];
equal[x_,y_]:=!FailureQ[x]&&!FailureQ[y]&&Together[PaVeToABCD[x]-PaVeToABCD[y]]===0;
calc[x_,opts___]:=FeynGravSimplify[x,opts,FORMThreads->1,WorkingDirectory->testDirectory];
zero=1/s-1/(s+t)-t/(s(s+t));
masters={A0[m^2],B0[s,m^2,M^2],C0[-2,-3,-5,2,3,7],D0[-2,-3,-5,-7,-11,-13,2,3,5,7]};
pa={pv[{}, {m^2}],pv[{s},{m^2,M^2}],pv[{-2,-3,-5},{2,3,7}],pv[{-2,-3,-5,-7,-11,-13},{2,3,5,7}]};
Do[
 named=calc[(s+t)masters[[j]]/s];represented=calc[(s+t)pa[[j]]/s];
 assert["named/PaVe equality "<>ToString[j],equal[named,represented]];
 assert["input equality "<>ToString[j],equal[named,(s+t)masters[[j]]/s]];
 assert["exact cancellation "<>ToString[j],calc[zero masters[[j]]]===0];
 assert["disabled named preservation "<>ToString[j],calc[masters[[j]],ReduceToScalarMasters->False,PaVeToABCD->False]===masters[[j]]];
 ,{j,4}];
assert["equivalent mixed master",calc[masters[[1]]-pa[[1]]]===0];
assert["complex symbolic dimension",equal[calc[(1+I)(D^2-1)/(D-1)masters[[1]]],(1+I)(D+1)masters[[1]]]];
assert["unit contribution",calc[7+zero masters[[1]]]===7];
assert["nonlinear integrals retained",equal[calc[(s+t)/s masters[[1]]masters[[2]]],(s+t)/s masters[[1]]masters[[2]]]];
assert["negative control",!equal[masters[[1]],2masters[[1]]]];
assert["PaVe presentation",!FreeQ[calc[masters[[1]],PaVeToABCD->False],_PaVe]];
assert["TFORM",equal[FeynGravSimplify[(s+t)/s masters[[1]],FORMThreads->2,WorkingDirectory->testDirectory],(s+t)/s masters[[1]]]];
job=CalcFormExport[(s+t)^2+a(s+t)^3,FileNameJoin[{testDirectory,"memo.frm"}],ReduceToScalarMasters->False];
program=Import[job["InputFile"],"Text"];
assert["sum reused",Length[StringCases[program,RegularExpression["(?m)^#define CFCF[0-9]+ "]]]===2];
assert["ordinary algebra",equal[calc[(s+t)^2+a(s+t)^3],(s+t)^2+a(s+t)^3]];
assert["cache lifetime",equal[calc[(u+v)^2+a(u+v)^3],(u+v)^2+a(u+v)^3]];
assert["invalid named arity",FailureQ[CalcFormExport[C0[s],FileNameJoin[{testDirectory,"invalid.frm"}]]]];
(* Generic tensor PaVe inputs continue through their existing reduction path. *)
tensor=PaVe[0,0,{s},{m^2,M^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
assert["tensor PaVe",Together[PaVeReduce[calc[(s+t)tensor/s,PaVeToABCD->False]]-(s+t)PaVeReduce[tensor]/s]===0];
assert["disabled tensor PaVe",calc[tensor,ReduceToScalarMasters->False,PaVeToABCD->False]===tensor];
assert["free-index coefficient",equal[calc[(s+t)masters[[1]]MTD[mu,nu]/s],(s+t)masters[[1]]FCI[MTD[mu,nu]]/s]];
(* A declared loop still applies to the whole expression, including a represented integral. *)
assert["declared integrand",equal[calc[masters[[1]] FAD[{l,M}],LoopMomenta->{l}],I Pi^2 masters[[1]]A0[M^2]]];
job=CalcFormExport[(s+t)pa[[1]]/s+masters[[1]],FileNameJoin[{testDirectory,"mixed.frm"}]];
proc=RunProcess[{"form",job["InputFile"]}];
assert["standalone FORM",proc["ExitCode"]===0&&proc["StandardError"]===""];
assert["standalone import",equal[CalcFormImport[job["ResultFile"],job["MappingFile"]],(2+t/s)masters[[1]]]];
assert["zero",calc[0]===0];
(* Nested input polynomials must retain their complete scalar dependencies,
   including when none of the variables is a spectator coupling. *)
p=(s+m^2)^2+(s+m^2)(s+M^2);q=p^2+s+m^2;
x=q B0[s,m^2,M^2]/s+p A0[m^2]/(s+m^2);y=calc[x];
assert["nested polynomial exact",equal[y,x]];
assert["nested polynomial negative control",!equal[y,x+A0[m^2]]];
assert["nested polynomial TFORM",equal[FeynGravSimplify[x,FORMThreads->2,WorkingDirectory->testDirectory],x]];
assert["nested nonlinear restoration",equal[calc[q A0[m^2] B0[s,m^2,M^2]],q A0[m^2] B0[s,m^2,M^2]]];
assert["inverse polynomial",equal[calc[q A0[m^2]/(p+1)],q A0[m^2]/(p+1)]];
assert["nested complex coefficient",equal[calc[(1+I)(p+q) A0[m^2]],(1+I)(p+q) A0[m^2]]];
assert["nested unit coefficient",equal[calc[q+p A0[m^2]],q+p A0[m^2]]];
assert["nested disabled reduction",equal[calc[x,ReduceToScalarMasters->False],x]];
job=CalcFormExport[x,FileNameJoin[{testDirectory,"nested.frm"}]];
assert["polynomial sharing emitted",StringContainsQ[Import[job["InputFile"],"Text"],"Local cfcInputPolynomial"]];
assert["no private objects",FreeQ[y,_Symbol?(StringStartsQ[SymbolName[#],"cfc"]&)]];
assert["power-only input",equal[calc[(s+t)^2 LoopTensorCoefficient[{0},{0},{m^2},{2},D],ReduceToScalarMasters->False,ReduceLoopCoefficients->True],(s+t)^2 (D-2)/(2m^2) A0[m^2]]];
finish["Represented integrals and export reuse"];
