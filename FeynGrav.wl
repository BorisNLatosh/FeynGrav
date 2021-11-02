(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print["FeynGrav: The default perturbation order is set to 4, so the package evaluate three- and four-graviton vertices."]
Print["FeynGrav: Please, be patient. It takes quite some time to calcualate."]


perturbationOrder=4


(* ITensor::usage = "I-tensors which are responsible for \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) expansion. It takes wither null or an even number of arguments which are Lorentz indices."
CTensor::usage = "C-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) expansion. It takes either null or an even number of arguments which are Lorentz indices."
Vierbein::usage = "Expansion of a vierbein \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\). It takes an even number of arguments."
CITensor::usage = "CI-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) expansion. Take 2 or more arguments."
CIITensor::usage = "CII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) expansion. Take 4 or more arguments."
CETensor::usage = "CE-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\) expansion. Take 2 or more arguments."
CIIITensor::usage = "CIII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\) expansion. Take 6 or more arguments." *)
GravitonVertex::usage = "Vertex for interaction of 3 or more gravitons. Its arguments are Lorentz indices and momenta of the corresponding gravitons. For instance GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]."
GravitonPropagator::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via FAD function."
GravitonPropagatorTop::usage = "The nominator of a graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via SPD function."
GravitonScalarVertex::usage = "Vertex for interaction between a massless scalar kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of scalars."
GravitonVectorVertex::usage = "Vertex for interaction between a massless vector field kinetic energy and gravitons. Takes 2n + 4 arguments. First 2n arguments are Lotentz indices of gravitons. The next two arguments are Lorentz indices of vectors. The last two arguments are vectors momenta."
GravitonFermionVertex::usage = "Vertex for interaction between a massless Dirac fermion kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of fermions."


(* 
The package operates with quite a few functions which should be evaluated up to a certain perturbation order.
The package evaluate these functions firstly and then provide a user to perform calculations.
*)


Begin["Private`"];


(* ITensor *)
Clear[ITensor];
ITensor[]=1;
Module[{tensorValence,generatingTerm,i},
	For[tensorValence=1,tensorValence<=perturbationOrder+1,tensorValence++,
		(*A singel term is generated*)
		generatingTerm=Times@@MTD@@@Table[{ToExpression["m"<>ToString[Mod[i+1,tensorValence,1]]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		(*It is symmetrized with respect to each index pair*)
		For[i=1,i<=tensorValence,i++,
			generatingTerm=Calc[1/2 (generatingTerm+(generatingTerm/.{ToExpression["m"<>ToString[i]]->ToExpression["n"<>ToString[i]],ToExpression["n"<>ToString[i]]->ToExpression["m"<>ToString[i]]}))];
		];
	Evaluate[ITensor@@Evaluate[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]]]=generatingTerm;
	];
]


(* CTensor *)
Clear[CTensor];
CTensor[]=1;
Module[{tensorValence,temporaryExpression,indexArrayArgumets,indexArrayM,indexArrayN,indexArray,indexArrayFull,numberOfTerms,summationIndices,summationIndicesArguments,iterationList,temporaryFactor,temporaryCTensor,tensorPermutationArray,i},
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]];
		indexArrayFull=Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		indexArrayArgumets=Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]];
		temporaryExpression=0;
		For[numberOfTerms=1,numberOfTerms<=tensorValence,numberOfTerms++,
			summationIndices=Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}];
			summationIndicesArguments=Table[ToExpression["j"<>ToString[i]<>"_"],{i,1,numberOfTerms}];
			iterationList={#,1,tensorValence}&/@summationIndices;
			Clear[temporaryFactor];
			Evaluate[temporaryFactor@@summationIndicesArguments]=KroneckerDelta[Tr[summationIndices],tensorValence](Times@@summationIndices)^-1;
			temporaryExpression=temporaryExpression+(-1)^(numberOfTerms+tensorValence)/(numberOfTerms! 2^numberOfTerms) Sum[temporaryFactor[Sequence@@summationIndices](Times@@If[Tr[summationIndices]==tensorValence,ITensor@@@TakeList[indexArray,2 summationIndices],0]),Evaluate[Sequence@@iterationList]]//Calc;
		];
		Evaluate[temporaryCTensor@@indexArrayArgumets]=temporaryExpression;
		Evaluate[CTensor@@indexArrayArgumets]=1/tensorValence! Sum[temporaryCTensor@@Flatten[Permutations[indexArrayFull][[i]]],{i,1,tensorValence!}]//Calc;
	];
]


(* Vierbein *)
Clear[Vierbein];
Module[{tensorValence},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[Vierbein@@Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]]]=Calc[Binomial[-1/2,tensorValence](ITensor@@Join[{\[ScriptM],\[ScriptN]},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]


(* CITensor *)
Clear[CITensor];
Module[{tensorValence,indexArray,indexArrayArgumets,tensorPermutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]];
		indexArrayArgumets=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]];
		tensorPermutationArray=Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		tensorPermutationArray=Permutations[tensorPermutationArray];
		tensorPermutationArray=Table[Join[{\[ScriptM],\[ScriptN]},Flatten[tensorPermutationArray[[i]]]],{i,1,tensorValence!}];
		Clear[temporaryExpression];
		Evaluate[temporaryExpression@@indexArrayArgumets]=Sum[(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[1;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2\[ScriptN]0+1;;2 tensorValence]]]),{\[ScriptN]0,0,tensorValence}]//Calc;
		Evaluate[CITensor@@indexArrayArgumets]=Evaluate[Calc[1/tensorValence! Plus@@(temporaryExpression@@@tensorPermutationArray)]];
	];
]


(* CIITensor *)
Clear[CIITensor];
Module[{tensorValence,indexArray,indexArrayArguments,tensorPermutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"],ToExpression["\[ScriptA]_"],ToExpression["\[ScriptB]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		tensorPermutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		tensorPermutationArray=Permutations[tensorPermutationArray];
		tensorPermutationArray=Table[Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},Flatten[tensorPermutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Clear[temporaryExpression];
		Evaluate[temporaryExpression@@indexArrayArguments]=Sum[If[\[ScriptN]0+\[ScriptN]1<=tensorValence,(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2 \[ScriptN]0+1;;2(\[ScriptN]0+\[ScriptN]1)]]])(ITensor@@Join[{\[ScriptA],\[ScriptB]},indexArray[[2(\[ScriptN]0+\[ScriptN]1)+1;;]]]),0],{\[ScriptN]0,0,tensorValence},{\[ScriptN]1,0,tensorValence}]//Calc;
		Evaluate[CIITensor@@indexArrayArguments]=1/tensorValence! Plus@@temporaryExpression@@@tensorPermutationArray//Calc;
	];
]


(* CETensor *)
Clear[CETensor];
Module[{tensorValence,indexArray,indexArrayArguments,permutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		permutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		permutationArray=Permutations[permutationArray];
		permutationArray=Table[Join[{\[ScriptM],\[ScriptN]},Flatten[permutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Evaluate[temporaryExpression@@indexArrayArguments]=Sum[(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[1;;2 \[ScriptN]0]])(Vierbein@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2\[ScriptN]0+1;;2 tensorValence]]]),{\[ScriptN]0,0,tensorValence}]//Calc;
		Evaluate[CETensor@@indexArrayArguments]=1/tensorValence! Plus@@(temporaryExpression@@@permutationArray)//Calc;
	];
]


(* CIIITensor *)
Clear[CIIITensor];
Module[{tensorValence,indexArray,indexArrayArguments,permutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"],ToExpression["\[ScriptA]_"],ToExpression["\[ScriptB]_"],ToExpression["\[ScriptR]_"],ToExpression["\[ScriptS]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		permutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		permutationArray=Permutations[permutationArray];
		permutationArray=Table[Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]},Flatten[permutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Evaluate[temporaryExpression@@indexArrayArguments]=Calc[Sum[If[\[ScriptN]0+\[ScriptN]1+\[ScriptN]2<=tensorValence,(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2 \[ScriptN]0+1;;2(\[ScriptN]0+\[ScriptN]1)]]])(ITensor@@Join[{\[ScriptA],\[ScriptB]},indexArray[[2(\[ScriptN]0+\[ScriptN]1)+1;;2(\[ScriptN]0+\[ScriptN]1+\[ScriptN]2)]]])(ITensor@@Join[{\[ScriptR],\[ScriptS]},indexArray[[2(\[ScriptN]0+\[ScriptN]1+\[ScriptN]2)+1;;]]]),0],{\[ScriptN]0,0,tensorValence},{\[ScriptN]1,0,tensorValence},{\[ScriptN]2,0,tensorValence}]];
		Evaluate[CIIITensor@@indexArrayArguments]=1/tensorValence! Plus@@temporaryExpression@@@permutationArray//Calc;
	];
]


(* GravitonVertex *)
Clear[GravitonVertex];
Context[\[Kappa]]="Global`"
Module[{tensorT,tensorValence,indexArray,indexArrayArgumets,indexArrayArgumetsVariables,temporaryExpression},
	tensorT[i_,j_,a_,b_,r_,s_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_]=-MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Beta],a,b]ITensor[\[Rho],\[Sigma],r,s]+MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Rho],a,b]ITensor[\[Beta],\[Sigma],r,s]-2MTD[\[Mu],i]MTD[\[Alpha],j]ITensor[\[Beta],\[Rho],a,b]ITensor[\[Nu],\[Sigma],r,s]+2MTD[\[Mu],i]MTD[\[Beta],j]ITensor[\[Nu],\[Alpha],a,b]ITensor[\[Rho],\[Sigma],r,s]//Calc;
	For[tensorValence=3,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArgumets=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]],ToExpression["p"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		indexArrayArgumetsVariables=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"],ToExpression["p"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]];
		Evaluate[temporaryExpression@@indexArrayArgumetsVariables]=FVD[p1,\[Lambda]1]FVD[p2,\[Lambda]2](tensorT@@Join[{\[Lambda]1,\[Lambda]2},indexArray[[1;;4]],Flatten[Table[{ToExpression["\[Rho]"<>ToString[\[ScriptI]]],ToExpression["\[Sigma]"<>ToString[\[ScriptI]]]},{\[ScriptI],1,3}]]])(CIIITensor@@Join[Flatten[Table[{ToExpression["\[Rho]"<>ToString[\[ScriptI]]],ToExpression["\[Sigma]"<>ToString[\[ScriptI]]]},{\[ScriptI],1,3}]],indexArray[[5;;]]])//Calc;
		Evaluate[GravitonVertex@@indexArrayArgumetsVariables]=(-I 2/\[Kappa]^2)(\[Kappa]^tensorValence/4)Plus@@temporaryExpression@@@Table[Flatten[Permutations[indexArrayArgumets][[\[ScriptI]]]],{\[ScriptI],1,tensorValence!}]//Calc;
	];
]


(* Graviton propagators *)
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


(* GravitonScalarVertex *)
Clear[GravitonScalarVertex];
Module[{TVertex,tensorValence,indexArray},
	TVertex[m_,n_,p_,q_]=-(1/2)ITensor[m,n,a,b]FVD[p,a]FVD[q,b]//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonScalarVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,p,q](CITensor@@Join[{m,n},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]


(* GravitonVectorVertex *)
Clear[GravitonVectorVertex];
Module[{TVertex,tensorValence},
	TVertex[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_,p_,q_]=1/4 FVD[p,\[Lambda]]FVD[q,\[Tau]](ITensor[\[Mu],\[Nu],\[Rho],\[Sigma]]ITensor[\[Alpha],\[Beta],\[Lambda],\[Tau]]+ITensor[\[Mu],\[Nu],\[Lambda],\[Tau]]ITensor[\[Alpha],\[Beta],\[Rho],\[Sigma]]-ITensor[\[Mu],\[Nu],\[Rho],\[Tau]]ITensor[\[Alpha],\[Beta],\[Sigma],\[Lambda]]-ITensor[\[Mu],\[Nu],\[Sigma],\[Lambda]]ITensor[\[Alpha],\[Beta],\[Rho],\[Tau]])//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonVectorVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["\[Rho]_"],ToExpression["\[Sigma]_"],ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,a,b,\[Rho],\[Sigma],p,q](CIITensor@@Join[{m,n,a,b},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]


(* GravitonFermionVertex *)
Clear[GravitonFermionVertex];
Module[{TVertex,tensorValence},
	TVertex[\[Mu]_,\[Nu]_,p_,q_]=1/2 FVD[p-q,\[Nu]]GAD[\[Mu]]//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonFermionVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,p,q](CETensor@@Join[{m,n},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]


End[];


EndPackage[];
