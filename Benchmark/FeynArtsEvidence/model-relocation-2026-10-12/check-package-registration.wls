(* Does FeynGrav now publish its packaged FeynArts models on $ModelPath? *)
$HistoryLength=0;
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
expected="/home/latosh/.Wolfram/Applications/FeynGrav/FeynArts/Models";
Print["FEYNARTS_LOADED=",MemberQ[$Packages,"FeynArts`"]];
Print["MODELPATH_HEAD=",InputForm[First[FeynArts`$ModelPath]]];
Print["REGISTERED=",MemberQ[ExpandFileName/@FeynArts`$ModelPath,expected]];
Print["REGISTERED_ONCE=",Count[ExpandFileName/@FeynArts`$ModelPath,expected]===1];
(* No path plumbing at all: load the model by name. *)
loaded=CheckAbort[FeynArts`InitializeModel["FGPureGR5a149c98161e2b2e",
 FeynArts`GenericModel->"FGPureGR5a149c98161e2b2e"],$Aborted];
Print["LOAD_BY_NAME_ALONE=",InputForm[loaded]," MODEL=",InputForm[FeynArts`$Model]];
converted=FeynCalc`FCFAConvert[FeynArts`CreateFeynAmp[
  FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}],
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->FeynArts`$Model,FeynArts`GenericModel->FeynArts`$GenericModel,
   FeynArts`InsertionLevel->{FeynArts`Classes}],
  FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True],
 IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},FeynCalc`ChangeDimension->D,
 Contract->False,List->True,Prefactor->1];
Print["DIAGRAMS=",Length[converted]," COUNTS=",InputForm[Sort[Count[#,_FeynCalc`PropagatorDenominator,Infinity]&/@converted]]];
contact=First[Select[converted,FreeQ[#,FeynCalc`PropagatorDenominator]&]];
Print["CONTACT_MATCHES_SOURCE=",FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[
 contact-FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,-p3,Lor7,Lor8,-p4]]]===0];
(* The legacy switch must behave identically. *)
Quit[0];
