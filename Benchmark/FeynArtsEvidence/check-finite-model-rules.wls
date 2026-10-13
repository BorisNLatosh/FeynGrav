(* Fixed generated snapshot, small exact source comparisons; no FORM. *)
$HistoryLength=0;$LoadFeynArts=True;Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_20261014b";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["FAIL init"];Quit[2]];
short[e_]:=StringTake[ToString[InputForm[e]],UpTo[750]];
li[i_]:=FeynArts`Index[Global`Lorentz,i];
pt=FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`T[1,p,{li[1],li[2]}->{li[3],li[4]}]];
pu=FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`U[1,p,{li[1]}->{li[2]}]];
Print["RAW_T_PROP=",short[pt]];Print["RAW_U_PROP=",short[pu]];
fcpt=ChangeDimension[FCPrepareFAAmp[pt],D];fcpu=ChangeDimension[FCPrepareFAAmp[pu],D];
Print["FC_T_PROP=",short[fcpt]];Print["FC_U_PROP=",short[fcpu]];
(* For PV wrappers, compare their argument, not an unevaluated model marker. *)
unwrap[e_]:=e/.FeynArts`PV[x_]:>x;
propt=TimeConstrained[Expand[unwrap[fcpt]-FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor1,Lor2,Lor3,Lor4,p]]],12,$TimedOut];
propu=TimeConstrained[Expand[unwrap[fcpu]-FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor1,Lor2,p]]],12,$TimedOut];
Print["T_PROP_EXACT=",TrueQ[propt===0]," U_PROP_EXACT=",TrueQ[propu===0]," residualT=",short[propt]," residualU=",short[propu]];
opts={FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}};
top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3,4}];
ins=FeynArts`InsertFields[top,{FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},Sequence@@opts];
amp=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`Truncated->True];
fc=FCFAConvert[amp,IncomingMomenta->{p},OutgoingMomenta->{q1,q2},ChangeDimension->D,Contract->False,List->True,Prefactor->1];
source=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-q1,Lor5,Lor6,-q2];
r=TimeConstrained[Expand[First[fc]-source],12,$TimedOut];
Print["THREE_TREE_COUNT=",Length[fc]," THREE_TREE_EXACT=",TrueQ[r===0]," residual=",short[r]];
Print["NO_EXTERNAL_MARKER=",FreeQ[fc,Global`FAPolarizationTensor]];
Quit[If[TrueQ[propt===0]&&TrueQ[propu===0]&&TrueQ[r===0],0,3]];
