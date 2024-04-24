(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["ScalarGaussBonnet`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


ScalarGaussBonnet::usage = "ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)}].";


Begin["Private`"];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


takeIndices = Flatten[ #[[;;2]]& /@ Partition[#,3] ] &;


(* The combinations of indices were calculated sepately. *)


Clear[TensorT];

TensorT[m_,n_,a_,b_,r_,s_,l_,t_,secondArray_List,indexArray_] := TensorT[m,n,a,b,r,s,l,t,secondArray,indexArray] = CTensorGeneral[Join[{m,a,n,b,s,t,r,l},secondArray],indexArray] - 4 CTensorGeneral[Join[{m,a,n,s,b,t,r,l},secondArray],indexArray] + CTensorGeneral[Join[{m,r,n,s,a,l,b,t},secondArray],indexArray];


Clear[ScalarGaussBonnetCore];

ScalarGaussBonnetCore[gravitonParameters_List] := ScalarGaussBonnetCore[gravitonParameters] = Switch[Length[gravitonParameters]/3,
	2, TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},gravitonParameters[[;;-1-3*2]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]] ,
	3, TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},gravitonParameters[[;;-1-3*2]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]] + 2 TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1},gravitonParameters[[;;-1-3*3]]] FVD[gravitonParameters[[-1-3*2]],l1]FVD[gravitonParameters[[-1-3]],l2]FVD[gravitonParameters[[-1]],l3]FVD[gravitonParameters[[-1-3*2]],scm] GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]] GammaTensor[i1,scs,scl,l2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[j1,scr,sct,l3,gravitonParameters[[-3]],gravitonParameters[[-2]]] ,
	_, TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},gravitonParameters[[;;-1-3*2]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]] + 2 TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1},gravitonParameters[[;;-1-3*3]]] FVD[gravitonParameters[[-1-3*2]],l1]FVD[gravitonParameters[[-1-3]],l2]FVD[gravitonParameters[[-1]],l3]FVD[gravitonParameters[[-1-3*2]],scm] GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]] GammaTensor[i1,scs,scl,l2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[j1,scr,sct,l3,gravitonParameters[[-3]],gravitonParameters[[-2]]] + TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1,i2,j2},gravitonParameters[[;;-1-3*4]]] FVD[gravitonParameters[[-1-3*3]],l1] FVD[gravitonParameters[[-1-3*2]],l2] FVD[gravitonParameters[[-1-3]],l3] FVD[gravitonParameters[[-1]],l4] GammaTensor[i,scn,sca,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*3]]] GammaTensor[j1,scm,scb,l2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]] GammaTensor[i2,scs,scl,l3,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]] GammaTensor[j2,scr,sct,gravitonParameters[[-3]],gravitonParameters[[-2]]]
	];


Clear[ScalarGaussBonnet];

ScalarGaussBonnet[gravitonParameters_List] := ScalarGaussBonnet[gravitonParameters] = I Power[Global`\[Kappa],Length[gravitonParameters]/3] 4 Total[Map[ ScalarGaussBonnetCore , Flatten/@Permutations[Partition[gravitonParameters,3]] ]];


End[];


EndPackage[];
