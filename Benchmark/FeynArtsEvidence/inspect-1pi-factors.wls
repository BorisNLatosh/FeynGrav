(* Inspect factored three-graph off-shell 1PI diagnostic, without expanding products. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
fa=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
fc=TimeConstrained[FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{ell},
 ChangeDimension->D,Contract->False,List->True,Prefactor->1],55,$TimedOut];
If[fc===$TimedOut || !ListQ[fc] || Length[fc]=!=3,Print["FC_FAILED"];Quit[3]];
Do[
 f=List@@fc[[g]];Print["GRAPH=",g," FACTORS=",Length[f]," HEAD=",Head[fc[[g]]]];
 Do[Print[" PART=",k," HEAD=",Head[f[[k]]]," BYTES=",ByteCount[f[[k]]],
    " LABELS=",InputForm[DeleteDuplicates[Cases[f[[k]],FeynCalc`LorentzIndex[z_,___]:>z,Infinity]]],
    " MOMS=",InputForm[DeleteDuplicates[Cases[f[[k]],FeynCalc`Momentum[m_,___]:>m,Infinity]]],
    " SMALL=",If[ByteCount[f[[k]]]<1400,StringTake[ToString[InputForm[f[[k]]]],UpTo[600]],"large"]],
 {k,Length[f]}]
,{g,3}];
Print["NO_INTEGRAL_MEASURE=True DIAGNOSTIC_ONLY=True"];
Quit[0];
