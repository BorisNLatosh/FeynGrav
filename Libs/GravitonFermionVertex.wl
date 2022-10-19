(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonFermionVertex`",{"FeynCalc`","ITensor`","CTensor`","CETensor`","ETensor`"}];

GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)}]. Vertex for gravitational interacation of a massless Dirac fermion kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta. All momenta are in-going."
GravitonMassiveFermionVertex::usage = "GravitonMassiveFermionVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)},m]. Vertex for gravitational interacation of a Dirac fermion kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta. m is the fermion mass. All momenta are in-going."

Begin["Private`"];

TTensorGravityDirac[m_,n_,p_,q_]=Calc[1/2 GAD[m]FVD[p+q,n]];

GravitonFermionVertex=indexArray|->Calc[(I Global`\[Kappa]^((Length[indexArray]-2)/2))CETensor[{\[ScriptM],\[ScriptN]},indexArray[[;;Length[indexArray]-2]]]TTensorGravityDirac[Sequence@@Join[{\[ScriptM],\[ScriptN]},indexArray[[Length[indexArray]-1;;]]]]];

GravitonMassiveFermionVertex={indexArray,m}|->Calc[(I Global`\[Kappa]^((Length[indexArray]-2)/2))(CETensor[{\[ScriptM],\[ScriptN]},indexArray[[;;Length[indexArray]-2]]]TTensorGravityDirac[Sequence@@Join[{\[ScriptM],\[ScriptN]},indexArray[[Length[indexArray]-1;;]]]]- m CTensor[indexArray[[;;Length[indexArray]-2]]])];

End[];

EndPackage[];
