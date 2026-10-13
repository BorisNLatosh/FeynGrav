$HistoryLength=0;
$LoadAddOns={"FeynArts"};
Get["FeynGrav`"];
modelDirectory=FileNameJoin[{"/home/latosh/.Wolfram/Applications/FeynGrav/Benchmark/FeynArtsEvidence","model-first-2026-10-12","model"}];
shortName="FGPureGR5a149c98161e2b2e";
PrependTo[FeynArts`$ModelPath,modelDirectory];
Print["INIT=",InputForm[FeynArts`InitializeModel[shortName,FeynArts`GenericModel->shortName]]];
(* OutgoingMomenta->{p3,p4} means the physical outgoing momenta are p3,p4, so an
   all-incoming vertex sees -p3,-p4.  The validated minimal example uses exactly
   this convention. *)
converted=FeynCalc`FCFAConvert[FeynArts`CreateFeynAmp[
  FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}],
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->shortName,FeynArts`GenericModel->shortName,FeynArts`InsertionLevel->{FeynArts`Classes}],
  FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True],
 IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},FeynCalc`ChangeDimension->D,
 Contract->False,List->True,Prefactor->1];
contact=First[Select[converted,FreeQ[#,FeynCalc`PropagatorDenominator]&]];
exactQ[candidate_]:=FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[contact-candidate]]===0;
direct=exactQ[FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,-p3,Lor7,Lor8,-p4]];
Print["CONTACT_DIRECT_SIGN_MATCH=",direct];
legs={{Lor1,Lor2,p1},{Lor3,Lor4,p2},{Lor5,Lor6,-p3},{Lor7,Lor8,-p4}};
matches=Select[Permutations[legs],exactQ[FeynGrav`GravitonVertex[Sequence@@Flatten[#]]]&];
Print["CONTACT_MATCHING_LEG_ORDERS=",Length[matches]," OF 24"];
routes=Sort[Flatten[Cases[#,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[q_,___],0]:>q,Infinity]&/@converted]];
Print["ROUTES=",InputForm[routes]," DIAGRAMS=",Length[converted]];
Quit[0];
