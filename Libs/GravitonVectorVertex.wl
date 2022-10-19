(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`"}];

GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)}]. Vertex for gravitational interacation of a massless vector field kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. All momenta are in-going.";

GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)},m]. Vertex for gravitational interacation of a vector field kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. m is the vector field mass. All momenta are in-going.";

Begin["Private`"];

TTensorGravityVector [\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_,p_,q_]=Calc[1/4 (FVD[p,\[Mu]]MTD[\[Alpha],\[Rho]]-FVD[p,\[Alpha]]MTD[\[Mu],\[Rho]])(FVD[q,\[Nu]]MTD[\[Beta],\[Sigma]]-FVD[q,\[Beta]]MTD[\[Nu],\[Sigma]])];

GravitonVectorVertex=indexArray|->Calc[(I Global`\[Kappa]^(Length[indexArray]/2-2))2TTensorGravityVector[Sequence@@Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray[[Length[indexArray]-3;;]]]]CIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray[[;;Length[indexArray]-4]]]];

GravitonMassiveVectorVertex=Function[{indexArray,m},Calc[(I Global`\[Kappa]^(Length[indexArray]/2-2))2(TTensorGravityVector[Sequence@@Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray[[Length[indexArray]-3;;]]]]CIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray[[;;Length[indexArray]-4]]]-m^2/2 CITensor[indexArray[[Length[indexArray]-3;;Length[indexArray]-2]],indexArray[[;;Length[indexArray]-4]]])]];


End[];

EndPackage[];
