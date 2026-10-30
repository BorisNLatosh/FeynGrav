Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
Clear[s,t,u,a,b,m,p,q,l,mu,nu];
master=PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
zero=1/s-1/(s+t)-t/(s(s+t));
rows={
 {MTD[mu,nu]MTD[mu,nu],D},
 {SPD[p+q,p]-SPD[p,p]-SPD[q,p],0},
 {FAD[p](D^2-1)/(D-1),FAD[p](D+1)},
 {FAD[p](a^2-b^2)/(a-b),FAD[p](a+b)},
 {FAD[p](D/(D-1)-1-1/(D-1)),0},
 {FAD[p]I(a+b)^2,FAD[p]I(a+b)^2},
 {FAD[p]Sqrt[D](a+b),FAD[p]Sqrt[D](a+b)},
 {FAD[p]SPD[p,q]^-1(a+b),FAD[p]SPD[p,q]^-1(a+b)},
 {FAD[p](a+b)MTD[mu,nu],FAD[p](a+b)MTD[mu,nu]},
 {zero master,0},
 {(2+3 I)(1/s+1/t)master,(2+3 I)(s+t)/(s t)master},
 {(1/(s+I t)+1/(s-I t))master,2s/(s^2+t^2)master},
 {((s+t)^2/(s t)-s/t-t/s-2)master,0},
 {zero master SPD[p,Polarization[q,I]],0},
 {master (a+b)^2 SPD[p,Polarization[q,I]]^2,master (a+b)^2 SPD[p,Polarization[q,I]]^2}
};
equal[x_,y_]:=!FailureQ[x]&&Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[x-FCI[y]]]]===0;
Do[got=CalcFormCalculate[rows[[j,1]],FORMThreads->1];
 assert["explicit exact result "<>ToString[j],equal[got,rows[[j,2]]]];
 assert["no leaked policy object "<>ToString[j],FreeQ[got,_Symbol?(StringStartsQ[Context[#],"CalcFormConverter`Private`"]&)]];
 If[rows[[j,2]]===0,assert["literal zero "<>ToString[j],got===0]],{j,Length[rows]}];
assert["altered result rejected",!equal[(a+b)FCI[FAD[p]],(a+b+1)FAD[p]]];
assert["unintegrated constant",CalcFormCalculate[1,FORMThreads->1]===1];
assert["integrated zero",CalcFormCalculate[zero FAD[{l,m}],LoopMomenta->{l},FORMThreads->1]===0];
assert["massive tadpole scope",CalcFormCalculate[FAD[{l,m}],LoopMomenta->{l},FORMThreads->1]===I Pi^2 master];
assert["API03 final boundary",FeynGravSimplify[FAD[{l,m}],LoopMomenta->{l},FORMThreads->1]===I Pi^2 A0[m^2]];
assert["API03 false boundary",FeynGravSimplify[FAD[{l,m}],LoopMomenta->{l},FORMThreads->1,PaVeToABCD->False]===I Pi^2 master];
assert["matrix order retained",CalcFormCalculate[GAD[mu].GAD[nu],DiracAlgebra->False,FORMThreads->1]===FCI[GAD[mu].GAD[nu]]];
assert["approximate rejected",FailureQ[CalcFormCalculate[1.2 a,FORMThreads->1]]];
Do[got=Block[{CalcFormConverter`Private`$rcMaximumTerms=n},CalcFormCalculate[FAD[p](a^2+2a b+b^2),FORMThreads->1]];
 assert["term guard equality "<>ToString[n],equal[got,FAD[p](a+b)^2]],{n,{0,1,2,3,4}}];
got=Block[{CalcFormConverter`Private`$rcMaximumVariables=1},CalcFormCalculate[FAD[p](a^2+2a b+b^2),FORMThreads->1]];
assert["variable guard equality",equal[got,FAD[p](a+b)^2]];
got=Block[{CalcFormConverter`Private`$rcMasterMaximumTerms=1},CalcFormCalculate[zero master,FORMThreads->1]];
assert["master guard exact",equal[got,0]&&got=!=0];
got=Block[{CalcFormConverter`Private`$rcPolarisationMaximumRationalPasses=0},CalcFormCalculate[rows[[-1,1]],FORMThreads->1]];
assert["spent polarisation work restores complete result",equal[got,rows[[-1,2]]]];
assert["TFORM exact",equal[CalcFormCalculate[rows[[11,1]],FORMThreads->2],rows[[11,2]]]];
finish["Shared coefficient arithmetic"];
