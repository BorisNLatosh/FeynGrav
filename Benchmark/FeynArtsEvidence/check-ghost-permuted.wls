(* Exact source comparison with whole ghost/antighost leg reassignment, off shell. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_20261014b";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILURE"];Quit[2]];
top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3}];
results=Table[
  fields=If[order===1,{-FeynArts`U[1],FeynArts`U[1]},{FeynArts`U[1],-FeynArts`U[1]}];
  ins=FeynArts`InsertFields[top,FeynArts`T[1]->fields,
    FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
  amp=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
  fc=FCFAConvert[amp,IncomingMomenta->{k},OutgoingMomenta->{pA,pC},ChangeDimension->D,Contract->False,List->True,Prefactor->1];
  source=If[order===1,
    FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,Lor3,-pA,Lor4,-pC],
    FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,Lor4,-pC,Lor3,-pA]];
  plus=If[Length[fc]===1,TimeConstrained[Expand[First[fc]-source],12,$TimedOut],$Failed];
  minus=If[Length[fc]===1,TimeConstrained[Expand[First[fc]+source],12,$TimedOut],$Failed];
  Print["ORDER=",InputForm[fields]," DIAGRAMS=",Length[fc]," SOURCE_PLUS=",TrueQ[plus===0]," SOURCE_MINUS=",TrueQ[minus===0]];
  {TrueQ[plus===0],TrueQ[minus===0]}
,{order,{1,2}}];
Quit[If[And@@(Or@@#&/@results),0,3]];
