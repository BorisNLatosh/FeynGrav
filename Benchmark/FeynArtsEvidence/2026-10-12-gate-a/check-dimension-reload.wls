(* Two explicitly selected tiny model identities; no repository library is generated. *)
$HistoryLength=0; $LoadFeynArts=True; Get["FeynCalc`"];
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,DirectoryName[$InputFileName]];
process[name_]:=Module[{loaded,top,ins,amp,converted},
 loaded=FeynArts`InitializeModel[name,FeynArts`GenericModel->name];
 If[loaded=!=True,Return[$Failed]];
 top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3,4}];
 ins=FeynArts`InsertFields[top,FeynArts`T[1]->{FeynArts`S[1],FeynArts`S[1]},FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
 amp=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
 converted=FCFAConvert[amp,IncomingMomenta->{p},OutgoingMomenta->{q1,q2},Contract->False,ChangeDimension->D,List->True,Prefactor->1];
 If[!ListQ[converted]||Length[converted]!=1,Return[$Failed]];
 First[converted]
];
a=TimeConstrained[process["fgfa_native_tu_ordered_20261012"],20,$TimedOut];
b=TimeConstrained[process["fgfa_native_tu_second_20261012"],20,$TimedOut];
Print["FIRST=",InputForm[a]]; Print["SECOND=",InputForm[b]];
Print["NO_RESIDUAL_HEADS=",FreeQ[{a,b},_Global`FAPolarizationTensor|_FeynArts`C|_FeynArts`G]];
Print["SYMBOLIC_DIMENSION=",!FreeQ[a,FeynCalc`LorentzIndex[_,D]] && FreeQ[a,FeynCalc`LorentzIndex[_]]];
Print["DISTINCT_IDENTITY_COEFFICIENT=",TrueQ[b===2 a]];
(* A composite momentum is converted without assigning scalar products. *)
x=FCPrepareFAAmp[Global`FAFourVector[p-r,FeynArts`Index[Global`Lorentz,1]] FeynArts`FAPropagatorDenominator[p-r,0]];
x=ChangeDimension[x,D]; Print["COMPOSITE=",InputForm[x]];
Print["COMPOSITE_RETAINED=",!FreeQ[x,FeynCalc`Momentum[p-r,D]] && FreeQ[x,Global`FAFourVector|Global`FAPropagatorDenominator]];
If[!TrueQ[b===2 a]||!FreeQ[{a,b},_Global`FAPolarizationTensor|_FeynArts`C|_FeynArts`G]||FreeQ[a,FeynCalc`LorentzIndex[_,D]],Quit[2]];
Quit[0];
