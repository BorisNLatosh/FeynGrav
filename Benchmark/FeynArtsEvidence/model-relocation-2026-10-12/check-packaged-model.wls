$HistoryLength=0;
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
modelDirectory="/home/latosh/.Wolfram/Applications/FeynGrav/FeynArts/Models";
Print["MODELS_DIRECTORY_EXISTS=",DirectoryQ[modelDirectory],
 " CONTENTS=",InputForm[FileNames["*",modelDirectory]]];
PrependTo[FeynArts`$ModelPath,modelDirectory];
loaded=CheckAbort[FeynArts`InitializeModel["FGPureGR5a149c98161e2b2e",
 FeynArts`GenericModel->"FGPureGR5a149c98161e2b2e"],$Aborted];
Print["LOAD_BY_NAME_FROM_PACKAGE=",InputForm[loaded]," MODEL=",InputForm[FeynArts`$Model],
 " GENERIC=",InputForm[FeynArts`$GenericModel]];
topologies=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}];
insertions=FeynArts`InsertFields[topologies,{FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
 FeynArts`Model->FeynArts`$Model,FeynArts`GenericModel->FeynArts`$GenericModel,
 FeynArts`InsertionLevel->{FeynArts`Classes}];
converted=FeynCalc`FCFAConvert[FeynArts`CreateFeynAmp[insertions,FeynArts`PreFactor->1,
  FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True],
 IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},FeynCalc`ChangeDimension->D,
 Contract->False,List->True,Prefactor->1];
Print["DIAGRAMS=",Length[converted]," DENOMINATOR_COUNTS=",
 InputForm[Sort[Count[#,_FeynCalc`PropagatorDenominator,Infinity]&/@converted]]];
contact=First[Select[converted,FreeQ[#,FeynCalc`PropagatorDenominator]&]];
Print["CONTACT_MATCHES_SOURCE=",FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[
 contact-FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,-p3,Lor7,Lor8,-p4]]]===0];
Print["ROUTES=",InputForm[Sort[Flatten[Cases[#,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[q_,___],0]:>q,Infinity]&/@converted]]]];
Quit[0];
