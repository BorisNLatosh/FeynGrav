(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`"}];

GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a vector field kinetic enery. It takes an array of graviton indices, Loretz indices and momenta of in-going vectors, and the vector field mass.";

GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of a massless vector field kinetic enery. It takes an array of graviton indices, Loretz indices and momenta of in-going vectors. The vertex is evaluated in the Lorentz gauge.";

GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of the Faddeev-Popov ghosts for a single massless vector field in the Lorentz gauge. It takes an array of graviton indices and momenta of in-going ghosts.";

GravitonVectorGhostPropagator::usage = "GravitonVectorGhostPropagator[p]. Propagator for the Faddeev-Popov ghost for a massless vector field.";

Begin["Private`"];

GVV = {indexArray,l1,p1,l2,p2,m} |-> Calc[ (I Global`\[Kappa]^(Length[indexArray]/2))(1/2 CIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray] (FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]MTD[\[ScriptA],l1]MTD[\[ScriptB],l2]-FVD[p1,\[ScriptM]]FVD[p2,\[ScriptB]]MTD[\[ScriptA],l1]MTD[\[ScriptN],l2])-m^2 CITensor[{\[ScriptM],\[ScriptN]},indexArray]MTD[\[ScriptM],l1]MTD[\[ScriptN],l2] ) ];

GVV1 = {indexArray,l1,p1,l2,p2,\[CurlyEpsilon]} |-> Calc[ (I Global`\[Kappa]^(Length[indexArray]/2)) 1/2 CIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},indexArray] (FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]MTD[\[ScriptA],l1]MTD[\[ScriptB],l2]-FVD[p1,\[ScriptM]]FVD[p2,\[ScriptB]]MTD[\[ScriptA],l1]MTD[\[ScriptN],l2]-\[CurlyEpsilon] FVD[p1,\[ScriptM]]FVD[p2,\[ScriptA]]MTD[\[ScriptN],l1]MTD[\[ScriptB],l2])  ] ;

GVV2 = {indexArray,l1,p1,l2,p2,\[CurlyEpsilon]} |-> Calc[ (I Global`\[Kappa]^(Length[indexArray]/2)) (-(\[CurlyEpsilon]/2)) CIIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]},indexArray[[3;;]]] (MTD[\[ScriptM],\[ScriptT]]ITensor[Join[{\[ScriptN],\[ScriptR]},indexArray[[;;2]]]]+MTD[\[ScriptN],\[ScriptT]]ITensor[Join[{\[ScriptM],\[ScriptR]},indexArray[[;;2]]]]-MTD[\[ScriptR],\[ScriptT]]ITensor[Join[{\[ScriptM],\[ScriptN]},indexArray[[;;2]]]] ) (FVD[p1+p2,\[ScriptT]](FVD[p2,\[ScriptA]] MTD[\[ScriptS],l1]MTD[\[ScriptB],l2]+FVD[p1,\[ScriptA]] MTD[\[ScriptS],l2]MTD[\[ScriptB],l1]))/2 ];

GVV3 = {indexArray,l1,p1,l2,p2,\[CurlyEpsilon]} |->Calc[ (I Global`\[Kappa]^(Length[indexArray]/2)) (-(\[CurlyEpsilon]/8)) CIIIITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]},indexArray[[5;;]]]FVD[p1+p2,\[Tau]1]FVD[p1+p2,\[Tau]2] (MTD[\[ScriptM],\[Tau]1]ITensor[Join[{\[ScriptN],\[ScriptR]},indexArray[[1;;2]]]]+MTD[\[ScriptN],\[Tau]1]ITensor[Join[{\[ScriptM],\[ScriptR]},indexArray[[1;;2]]]]-MTD[\[ScriptR],\[Tau]1]ITensor[Join[{\[ScriptM],\[ScriptN]},indexArray[[1;;2]]]]) (MTD[\[ScriptA],\[Tau]2]ITensor[Join[{\[ScriptB],\[ScriptT]},indexArray[[3;;4]]]]+MTD[\[ScriptB],\[Tau]2]ITensor[Join[{\[ScriptA],\[ScriptT]},indexArray[[3;;4]]]]-MTD[\[ScriptT],\[Tau]2]ITensor[Join[{\[ScriptA],\[ScriptB]},indexArray[[3;;4]]]]) MTD[\[ScriptS],l1]MTD[\[ScriptL],l2] ];

GravitonVectorVertex = {indexArray,l1,p1,l2,p2} |-> If[Length[indexArray]/2 <=2 , Calc[Total[ (GVV1[#,l1,p1,l2,p2,1]+GVV2[#,l1,p1,l2,p2,1])&/@(Flatten/@Permutations[Partition[indexArray,2]]) ]] , Calc[Total[ (GVV1[#,l1,p1,l2,p2,1]+GVV2[#,l1,p1,l2,p2,1]+GVV3[#,l1,p1,l2,p2,1])&/@(Flatten/@Permutations[Partition[indexArray,2]]) ]]];

GravitonMassiveVectorVertex = {indexArray,l1,p1,l2,p2,m}|->GVV[indexArray,l1,p1,l2,p2,m];

GravitonVectorGhostVertex = {indexArray,p1,p2} |-> Calc[- FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]] CITensor[{\[ScriptM],\[ScriptN]},indexArray]];

GravitonVectorGhostPropagator = p |-> - I FAD[p];

End[];

EndPackage[];
