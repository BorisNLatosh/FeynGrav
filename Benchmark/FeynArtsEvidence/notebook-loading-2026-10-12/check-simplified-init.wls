$HistoryLength=0;
(* Candidate simplified initialisation: documented FeynCalc add-on loader,
   package loaded by name, model referred to by file name. *)
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
Print["FEYNARTS_LOADED=",MemberQ[$Packages,"FeynArts`"]," FEYNCALC_LOADED=",MemberQ[$Packages,"FeynCalc`"]];
Print["GRAVITON_LIBRARY_ORDERS=",InputForm[FeynGrav`FeynGravLibraryInformation[][FeynGrav`importGravitons,"Orders"]]];
modelRoot="/home/latosh/.Wolfram/Applications/FeynGrav/Benchmark/FeynArtsEvidence/model-first-2026-10-12/model";
grModel=FileNameJoin[{modelRoot,"FGPureGR5a149c98161e2b2e"}];
Print["MODEL_PATH_UNTOUCHED_BEFORE=",InputForm[FeynArts`$ModelPath]];
loaded=CheckAbort[FeynArts`InitializeModel[grModel,FeynArts`GenericModel->grModel],$Aborted];
Print["INITIALIZE_RESULT=",InputForm[loaded]," MODELPATH_UNTOUCHED_AFTER=",InputForm[FeynArts`$ModelPath]];
topologies=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}];
insertions=FeynArts`InsertFields[topologies,{FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
 FeynArts`Model->grModel,FeynArts`GenericModel->grModel,FeynArts`InsertionLevel->{FeynArts`Classes}];
amplitudes=FeynArts`CreateFeynAmp[insertions,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},
 FeynArts`MomentumConservation->True,FeynArts`Truncated->True];
Print["INSERTIONS_WITH_ABSOLUTE_MODEL=",Length[Cases[insertions,_FeynmanGraph,Infinity]],
 " UNCONVERTED_AMPLITUDES=",Length[amplitudes]];
contactMatch=CheckAbort[Module[{converted},
 converted=FeynCalc`FCFAConvert[amplitudes,IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},
  FeynCalc`ChangeDimension->D,Contract->False,List->True,Prefactor->1];
 source=FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,p3,Lor7,Lor8,p4];
 FreeQ[converted[[1]]-source,Except[0]]],$Aborted];
Print["CONTACT_STILL_EXACT=",contactMatch];
(* Justify dropping the If[..., Abort[]] wrapper: FeynArts itself refuses a missing model. *)
bad=CheckAbort[FeynArts`InitializeModel["NoSuchModelXYZ",FeynArts`GenericModel->"NoSuchModelXYZ"],$Aborted];
Print["MISSING_MODEL_RESULT=",InputForm[bad]];
Quit[0];
