Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,q,l,s,t,m,g,h,z,mu];
master=PaVe[0,{},{m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
zero=1/s-1/(s+t)-t/(s(s+t));
calculate[x_,opts___]:=Block[{CalcFormConverter`Private`$lrCollectCoefficients=False},
 CalcFormCalculate[x,FORMThreads->1,TimeConstraint->30,opts]];
ref[x_]:=Block[{CalcFormConverter`Private`$rcCouplingGroups=False,
 CalcFormConverter`Private`$rcCanonicalKinematics=False,CalcFormConverter`Private`$rcMasterMaximumTerms=0},calculate[x]];
fixtures={
 {"complete repeated group",g^2 zero master+h (1/s+1/t)master,h(s+t)/(s t)master},
 {"Laurent spectators",g^2/h^3 zero master+g/h(1/s+1/t)master,g/h(s+t)/(s t)master},
 {"shared polynomial",(g+h)^3 zero master,0},
 {"unit structure",(g zero+1/s+1/t)master,(s+t)/(s t)master},
 {"complex and D",(g+I h)(D-2)/(D-1)zero master,0},
 {"nonzero complex denominator",g(1/(s+I t)+1/(s-I t))master,2g s/(s^2+t^2)master},
 {"mixed denominator",(g/(s+g)-h/(s+h))master,(g-h)s/((s+g)(s+h))master},
 {"radical fallback",Sqrt[g+h](1/s+1/t)master,Sqrt[g+h](s+t)/(s t)master}
};
Do[result=calculate[row[[2]]]; assert[row[[1]]<>" supported",!FailureQ[result]];
 assert[row[[1]]<>" exact",Together[result-row[[3]]]===0];
 assert[row[[1]]<>" control",Together[result-row[[3]]-master]=!=0];
 If[row[[3]]===0,assert[row[[1]]<>" literal zero",result===0]],{row,fixtures}];
(* Thirteen independent spectator monomials exceed the monolithic variable
   guard. Each complete coefficient has just two kinematic variables. *)
Clear[c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13];
many=Total[{c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13}] ((s^2-t^2)/(s-t)-s-t) master;
old=Block[{CalcFormConverter`Private`$rcCouplingGroups=False},calculate[many]];
assert["monolithic negative control is unreduced",old=!=0&&!FailureQ[old]];
assert["grouping recovers complete cancellation",calculate[many]===0];
assert["unreduced control exact",Together[old]===0];
limited=Block[{CalcFormConverter`Private`$rcPolarisationMaximumStructures=1},calculate[fixtures[[1,2]]]];
assert["bounded grouping retains value",Together[limited-fixtures[[1,3]]]===0];
limited=Block[{CalcFormConverter`Private`$rcPolarisationMaximumRationalPasses=0},calculate[fixtures[[1,2]]]];
assert["exhaustion retains value",Together[limited-fixtures[[1,3]]]===0];
assert["serial and TFORM",Together[CalcFormCalculate[fixtures[[2,2]],FORMThreads->2,TimeConstraint->30]-fixtures[[2,3]]]===0];
SPD[p,p]=s;SPD[p,q]=(s+t)/2;
input=(FVD[p,mu]FVD[q,mu]+g/h)FAD[{l,m}];
job=CalcFormExport[input,FileNameJoin[{testDirectory,"assigned.frm"}],LoopMomenta->{l}];
assert["manual export",AssociationQ[job]];
pr=RunProcess[{"/usr/local/bin/form",job["InputFile"]}];
assert["manual FORM",pr["ExitCode"]===0&&pr["StandardError"]===""];
manual=Block[{CalcFormConverter`Private`$pgReadChunkSize=7},CalcFormImport[job["ResultFile"],job["MappingFile"]]];
assert["assigned extra-vector exact",Together[manual-I Pi^2((s+t)/2+g/h)master]===0];
assert["manual/full parity",Together[manual-calculate[input,LoopMomenta->{l}]]===0];
map=Import[job["MappingFile"],"RawJSON"];
assert["frozen identities",Length[map["CoefficientKinematics"]["Identities"]]===2];
validate[x_]:=Catch[CalcFormConverter`Private`rcValidateKinematics[x,D],CalcFormConverter`Private`$failureTag];
assert["snapshot valid",validate[map]===Null];
modified=map;modified["CoefficientKinematics"]=<|"Version"->2,"Identities"->{}|>;
assertFailure["unknown snapshot version","InvalidMapping",validate[modified]];
modified=map;modified["CoefficientKinematics"]=<|"Version"->1,"Identities"->Join[map["CoefficientKinematics"]["Identities"],map["CoefficientKinematics"]["Identities"]]|>;
assertFailure["duplicate identity","InvalidMapping",validate[modified]];
modified=map;modified["CoefficientKinematics"]=<|"Version"->1,"Identities"->{<|"Vectors"->First[map["CoefficientKinematics"]["Identities"]]["Vectors"],"Value"->{"Symbol","Global`unregisteredInvariant"}|>}|>;
assertFailure["unregistered invariant","InvalidMapping",validate[modified]];
Do[
 modified=map;modified["CoefficientKinematics"]=<|"Version"->1,"Identities"->{<|"Vectors"->First[map["CoefficientKinematics"]["Identities"]]["Vectors"],"Value"->bad|>}|>;
 assertFailure["malformed identity "<>ToString[bad,InputForm],"InvalidMapping",validate[modified]],
 {bad,{{},{"Power",{"Symbol","Global`s"},{"Integer","bad"}},{"Integer","bad"}}}];
SPD[p,q]=s;
assertFailure["changed extra-vector kinematics","CoefficientConventionMismatch",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
SPD[p,q]=0;
assert["zero assigned product",calculate[FVD[p,mu]FVD[q,mu]FAD[{l,m}],LoopMomenta->{l}]===0];
FClearScalarProducts[];
assert["unassigned products unchanged",Together[calculate[FVD[p,mu]FVD[q,mu]FAD[{l,m}],LoopMomenta->{l}]-I Pi^2 SPD[p,q]master]===0];
assert["no private heads",FreeQ[manual,_Symbol?(StringStartsQ[SymbolName[#],"cfc"]&)]];
finish["Canonical kinematics and coupling groups"];
