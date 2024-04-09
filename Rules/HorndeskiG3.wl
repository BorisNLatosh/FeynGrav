(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG3`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG3::usage = "HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(3\)]\) class. Function arguments are {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\),\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton Lorentz indices and momenta; \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta; b is the number of scalar field kinetic term.";


HorndeskiG3Uncontracted::usage = "HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(3\)]\) class. Function arguments are {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\),\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton Lorentz indices and momenta; \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta; b is the number of scalar field kinetic term.";


Begin["Private`"];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


takeIndices = indexArray |-> Flatten[ #[[;;2]]& /@ Partition[indexArray,3] ];


Clear[HorndeskiG3Core];

HorndeskiG3Core[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG3Core[gravitonParameters,scalarMomenta,b] = Total[Map[ MomentaWrapper[#[[;;2b]]] (CTensorGeneral[ Join[{\[ScriptM],\[ScriptN]},DummyArray2[b]], takeIndices[gravitonParameters]] FVD[#[[2b+1]],\[ScriptM]] FVD[#[[2b+1]],\[ScriptN]] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]},DummyArray2[b]], takeIndices[gravitonParameters[[;;-3-1]]]]GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[#[[2b+1]],\[ScriptS]] FVD[gravitonParameters[[-1]],\[ScriptL]] ) &, Permutations[scalarMomenta] ]] ;


Clear[HorndeskiG3];

HorndeskiG3[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG3[gravitonParameters,scalarMomenta,b] = I Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+1] Total[Map[ HorndeskiG3UncontractedCore[#,scalarMomenta,b]& , Flatten/@Permutations[Partition[gravitonParameters,3]] ]] //Calc ;


Clear[HorndeskiG3UncontractedCore];

HorndeskiG3UncontractedCore[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG3UncontractedCore[gravitonParameters,scalarMomenta,b] = Total[Map[ MomentaWrapper[#[[;;2b]]] (CTensorGeneral[ Join[{\[ScriptM],\[ScriptN]},DummyArray2[b]], takeIndices[gravitonParameters]] FVD[#[[2b+1]],\[ScriptM]] FVD[#[[2b+1]],\[ScriptN]] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]},DummyArray2[b]], takeIndices[gravitonParameters[[;;-3-1]]]]GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[#[[2b+1]],\[ScriptS]] FVD[gravitonParameters[[-1]],\[ScriptL]] ) &, Permutations[scalarMomenta] ]] ;


Clear[HorndeskiG3Uncontracted];

HorndeskiG3Uncontracted[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG3Uncontracted[gravitonParameters,scalarMomenta,b] = I Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+1] Total[Map[ HorndeskiG3UncontractedCore[#,scalarMomenta,b]& , Flatten/@Permutations[Partition[gravitonParameters,3]] ]] ;


End[];


EndPackage[];
