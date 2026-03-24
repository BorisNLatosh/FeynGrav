(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonFermionVertex`",{"FeynCalc`","CTensorGeneral`","CETensor`","ETensor`"}];


GravitonFermionVertex::usage = 
"GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The function returns an expression for the graviton vertex of a Dirac fermion kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, and m is the fermion mass.";


GravitonFermionVertexUncontracted::usage = 
"GravitonFermionVertexUncontracted[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The function returns an expression for the graviton vertex of a Dirac fermion kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, and m is the fermion mass.";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


TakeMomenta = indexArray |-> If[ Length[indexArray]!=0,Flatten[(#[[3]]&)/@Partition[indexArray,3]],{}];


(* GravitonFermionVertex with contraction *)


Clear[GravitonFermionVertex1];

GravitonFermionVertex1[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex1[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (
			+(1/2) CEITensor[{\[ScriptM],\[ScriptN]},TakeLorenzIndices[indexArray]] GAD[\[ScriptM]]FVD[p1-p2,\[ScriptN]] //Contract//ExpandScalarProduct
		 ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertex2];

GravitonFermionVertex2[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex2[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (
			+ (1/8)(GAD[\[ScriptB],\[ScriptA],\[ScriptM]]-GAD[\[ScriptM],\[ScriptA],\[ScriptB]])Sum[ 
				FVD[ Total[TakeMomenta[indexArray][[;;\[ScriptS]]]] ,\[GothicA]]
				EITensor[{\[GothicB],\[ScriptM]},TakeLorenzIndices[indexArray][[;;2 \[ScriptS]]]]
				CEIETensor[{\[GothicA],\[ScriptA]},{\[GothicB],\[ScriptB]},TakeLorenzIndices[indexArray][[2\[ScriptS]+1;;]]]  
			,{\[ScriptS],0,Length[indexArray]/3}] //Contract
		 ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertex3];

GravitonFermionVertex3[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex3[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (- CTensorGeneral[{},TakeLorenzIndices[indexArray]] m  ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertex];

GravitonFermionVertex[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex[indexArray,p1,p2,m] =  
		+ GravitonFermionVertex1[indexArray,p1,p2,m] + GravitonFermionVertex3[indexArray,p1,p2,m] + 1/Length[indexArray/3]! Total[GravitonFermionVertex2[#,p1,p2,m]&/@Flatten/@Permutations[Partition[indexArray,3]]] //Expand//FeynCalcInternal ;


(* GravitonFermionVertex without contraction *)


Clear[GravitonFermionVertex1Uncontracted];

GravitonFermionVertex1Uncontracted[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex1Uncontracted[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (
			+(1/2) CEITensor[{\[ScriptM],\[ScriptN]},TakeLorenzIndices[indexArray]] GAD[\[ScriptM]]FVD[p1-p2,\[ScriptN]] //ExpandScalarProduct
		 ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertex2Uncontracted];

GravitonFermionVertex2Uncontracted[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex2Uncontracted[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (
			+ (1/8)(GAD[\[ScriptB],\[ScriptA],\[ScriptM]]-GAD[\[ScriptM],\[ScriptA],\[ScriptB]])Sum[ 
				FVD[ Total[TakeMomenta[indexArray][[;;\[ScriptS]]]] ,\[GothicA]]
				EITensor[{\[GothicB],\[ScriptM]},TakeLorenzIndices[indexArray][[;;2 \[ScriptS]]]]
				CEIETensor[{\[GothicA],\[ScriptA]},{\[GothicB],\[ScriptB]},TakeLorenzIndices[indexArray][[2\[ScriptS]+1;;]]]  
			,{\[ScriptS],0,Length[indexArray]/3}] 
		 ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertex3Uncontracted];

GravitonFermionVertex3Uncontracted[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertex3Uncontracted[indexArray,p1,p2,m] =  
		(I Global`\[Kappa]^(Length[indexArray]/3)) (- CTensorGeneral[{},TakeLorenzIndices[indexArray]] m  ) //Expand//FeynCalcInternal ;


Clear[GravitonFermionVertexUncontracted];

GravitonFermionVertexUncontracted[indexArray_,p1_,p2_,m_] := 
	GravitonFermionVertexUncontracted[indexArray,p1,p2,m] =  
		+ GravitonFermionVertex1[indexArray,p1,p2,m] + GravitonFermionVertex3[indexArray,p1,p2,m] + 1/Length[indexArray/3]! Total[GravitonFermionVertex2[#,p1,p2,m]&/@Flatten/@Permutations[Partition[indexArray,3]]] //Expand//FeynCalcInternal ;


End[];


EndPackage[];
