(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonFermionVertex`",{"FeynCalc`","CTensorGeneral`","CETensor`","ETensor`"}];


GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the graviton vertex of a Dirac fermion kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, and m is the fermion mass.";


GravitonFermionVertexUncontracted::usage = "GravitonFermionVertexUncontracted[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the graviton vertex of a Dirac fermion kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, and m is the fermion mass.";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


TakeMomenta = indexArray |-> If[ Length[indexArray]!=0,Flatten[(#[[3]]&)/@Partition[indexArray,3]],{}];


Clear[GravitonFermionVertex];

GravitonFermionVertex[indexArray_,p1_,p2_,m_] := GravitonFermionVertex[indexArray,p1,p2,m] =  (I Global`\[Kappa]^(Length[indexArray]/3)) ( (1/2) ExpandScalarProduct[Contract[ CEITensor[{\[ScriptM],\[ScriptN]},TakeLorenzIndices[indexArray]] GAD[\[ScriptM]]FVD[p1-p2,\[ScriptN]] ]] + Contract[(1/8)Sum[ FVD[Total[TakeMomenta[indexArray][[;;\[ScriptS]]]],\[GothicA]] EITensor[{\[GothicB],\[ScriptM]},TakeLorenzIndices[indexArray][[;;2 \[ScriptS]]]]CEIETensor[{\[GothicA],\[ScriptA]},{\[GothicB],\[ScriptB]},TakeLorenzIndices[indexArray][[2\[ScriptS]+1;;]]]  ,{\[ScriptS],0,Length[indexArray]/3}](GAD[\[ScriptB],\[ScriptA],\[ScriptM]]-GAD[\[ScriptM],\[ScriptA],\[ScriptB]])] - CTensorGeneral[{},TakeLorenzIndices[indexArray]] m ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertexUncontracted];

GravitonFermionVertexUncontracted[indexArray_,p1_,p2_,m_] := GravitonFermionVertexUncontracted[indexArray,p1,p2,m] =  (I Global`\[Kappa]^(Length[indexArray]/3)) ( (1/2) CEITensor[{\[ScriptM],\[ScriptN]},TakeLorenzIndices[indexArray]] GA[\[ScriptM]](FVD[p1,\[ScriptN]]-FVD[p2,\[ScriptN]]) (1/8)Sum[ FVD[Total[TakeMomenta[indexArray][[;;\[ScriptS]]]],\[GothicA]] EITensor[{\[GothicB],\[ScriptM]},TakeLorenzIndices[indexArray][[;;2 \[ScriptS]]]]CEIETensor[{\[GothicA],\[ScriptA]},{\[GothicB],\[ScriptB]},TakeLorenzIndices[indexArray][[2\[ScriptS]+1;;]]]  ,{\[ScriptS],0,Length[indexArray]/3}](GAD[\[ScriptB],\[ScriptA],\[ScriptM]]-GAD[\[ScriptM],\[ScriptA],\[ScriptB]])  - CTensorGeneral[{},TakeLorenzIndices[indexArray]] m )  ;


End[];


EndPackage[];
