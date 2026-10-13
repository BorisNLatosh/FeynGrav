$HistoryLength=0;
SetAttributes[input,HoldAllComplete];
(* Load by name only; no explicit importGravitons call anywhere in this file. *)
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
info=FeynGrav`FeynGravLibraryInformation[];
Print["GRAVITON_IMPORT_ORDERS_AFTER_PLAIN_LOAD=",InputForm[info[FeynGrav`importGravitons,"Orders"]]];
quartic=FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,p3,Lor7,Lor8,p4];
Print["QUARTIC_VERTEX_AVAILABLE_WITHOUT_EXPLICIT_IMPORT=",Head[quartic]=!=FeynGrav`GravitonVertex," HEAD=",Head[quartic]];
modelDirectory=FileNameJoin[{"/home/latosh/.Wolfram/Applications/FeynGrav/Benchmark/FeynArtsEvidence","model-first-2026-10-12","model"}];
shortName="FGPureGR5a149c98161e2b2e";
absoluteName=FileNameJoin[{modelDirectory,shortName}];
convert[]:=FeynCalc`FCFAConvert[FeynArts`CreateFeynAmp[
  FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}],
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->FeynArts`$Model,FeynArts`GenericModel->FeynArts`$GenericModel,
   FeynArts`InsertionLevel->{FeynArts`Classes}],
  FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True],
 IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},FeynCalc`ChangeDimension->D,
 Contract->False,List->True,Prefactor->1];
(* (a) model referred to by absolute file name, $ModelPath untouched *)
absoluteLoaded=CheckAbort[FeynArts`InitializeModel[absoluteName,FeynArts`GenericModel->absoluteName],$Aborted];
absoluteAmplitudes=convert[];
Print["ABSOLUTE_INIT=",InputForm[absoluteLoaded]," DIAGRAMS=",Length[absoluteAmplitudes],
 " MODELPATH_UNCHANGED=",InputForm[FeynArts`$ModelPath]];
(* (b) documented $ModelPath mechanism with the short model name *)
PrependTo[FeynArts`$ModelPath,modelDirectory];
pathLoaded=CheckAbort[FeynArts`InitializeModel[shortName,FeynArts`GenericModel->shortName],$Aborted];
pathAmplitudes=convert[];
Print["MODELPATH_INIT=",InputForm[pathLoaded]," DIAGRAMS=",Length[pathAmplitudes],
 " MODELPATH_HEAD=",InputForm[First[FeynArts`$ModelPath]]];
(* (c) the two routes must produce identical diagrams *)
Print["ROUTES_AGREE=",absoluteAmplitudes===pathAmplitudes];
(* (d) short-name InsertFields still works after the $ModelPath route, and the
   contact diagram (zero propagators) equals the source quartic vertex. *)
contact=First[Select[pathAmplitudes,FreeQ[#,FeynCalc`PropagatorDenominator]&]];
source=quartic/.{p1->p1,p2->p2,p3->p3,p4->p4};
Print["CONTACT_EXACT_SOURCE_MATCH=",FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[contact-source]]===0];
Print["CONTACT_BYTES=",ByteCount[contact]," SOURCE_BYTES=",ByteCount[source]];
Quit[0];
