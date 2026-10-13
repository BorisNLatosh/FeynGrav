$HistoryLength=0;
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
modelDirectory=FileNameJoin[{"/home/latosh/.Wolfram/Applications/FeynGrav/Benchmark/FeynArtsEvidence","model-first-2026-10-12","model"}];
shortName="FGPureGR5a149c98161e2b2e";
(* Evaluating the recommended snippet twice must remain safe. *)
Do[PrependTo[FeynArts`$ModelPath,modelDirectory];,{2}];
Print["MODELPATH_AFTER_TWO_EVALUATIONS=",InputForm[FeynArts`$ModelPath]];
Print["REINITIALISE_TWICE=",InputForm[FeynArts`InitializeModel[shortName,FeynArts`GenericModel->shortName]],
 " AGAIN=",InputForm[FeynArts`InitializeModel[shortName,FeynArts`GenericModel->shortName]]];
converted=FeynCalc`FCFAConvert[FeynArts`CreateFeynAmp[
  FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}],
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->shortName,FeynArts`GenericModel->shortName,FeynArts`InsertionLevel->{FeynArts`Classes}],
  FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True],
 IncomingMomenta->{p1,p2},OutgoingMomenta->{-p3,-p4},FeynCalc`ChangeDimension->D,
 Contract->False,List->True,Prefactor->1];
contact=First[Select[converted,FreeQ[#,FeynCalc`PropagatorDenominator]&]];
legs={{Lor1,Lor2,p1},{Lor3,Lor4,p2},{Lor5,Lor6,-p3},{Lor7,Lor8,-p4}};
exactQ[candidate_]:=FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[contact-candidate]]===0;
matches=Select[Permutations[legs],exactQ[FeynGrav`GravitonVertex[Sequence@@Flatten[#]]]&];
Print["CONTACT_MATCHING_LEG_ORDERS=",Length[matches]];
If[Length[matches]>0,Print["CONTACT_MATCH=",InputForm[Flatten[First[matches]]]]];
Print["CONTACT_IS_PURE_QUARTIC=",Head[contact]=!=FeynCalc`PropagatorDenominator," BYTES=",ByteCount[contact]];
Quit[0];
