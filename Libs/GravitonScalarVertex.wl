(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`"}];

GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)}]. Vertex for gravitational interacation of a massless scalar field kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta. All momenta are in-going.";

GravitonMassiveScalarVertex::usage = "GravitonMassiveScalarVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)},m]. Vertex for gravitational interacation of a scalar field kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta. m is the scalar field mass. All momenta are in-going.";

Begin["Private`"];

TTensorGravityScalar={m,n,p,q}|->Expand[-(1/2) 1/2 (FVD[p,m]FVD[q,n]+FVD[q,m]FVD[p,n])];

GravitonScalarVertex=Function[indexArray,Contract[2(I Global`\[Kappa]^((Length[indexArray]-2)/2))CITensor[{\[ScriptM],\[ScriptN]},indexArray[[;;Length[indexArray]-2]]]TTensorGravityScalar[Sequence@@Join[{\[ScriptM],\[ScriptN]},indexArray[[Length[indexArray]-1;;]]]]]  ];

GravitonMassiveScalarVertex=Function[{indexArray,m},Contract[2(I Global`\[Kappa]^((Length[indexArray]-2)/2))(CITensor[{\[ScriptM],\[ScriptN]},indexArray[[;;Length[indexArray]-2]]]TTensorGravityScalar[Sequence@@Join[{\[ScriptM],\[ScriptN]},indexArray[[Length[indexArray]-1;;]]]]  -m^2/2 CTensor[indexArray[[;;Length[indexArray]-2]]] )]];

End[];

EndPackage[];
