(* External ghosts are diagnostic fields, not physical asymptotic states. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"setup.wl"}]];
li[i_]:=FeynArts`Index[Global`Lorentz,i];
prop=FeynArts`AnalyticalPropagator[FeynArts`Internal][
 FeynArts`U[1,p,{li[1]}->{li[2]}]];
converted=FeynCalc`ChangeDimension[FeynCalc`FCPrepareFAAmp[prop],D]/.FeynArts`PV[x_]:>x;
assert["oriented ghost propagator equals source",same[converted,
 FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor1,Lor2,p]]]];
top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3}];
Do[
 fields=If[order===1,{-FeynArts`U[1],FeynArts`U[1]},{FeynArts`U[1],-FeynArts`U[1]}];
 ins=FeynArts`InsertFields[top,FeynArts`T[1]->fields,FeynArts`Model->name,
  FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
 amp=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},
  FeynArts`MomentumConservation->True,FeynArts`Truncated->True];
 fc=FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{k},FeynCalc`OutgoingMomenta->{pA,pC},
  FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1];
 assert["one ghost vertex for external order "<>ToString[order],ListQ[fc]&&Length[fc]===1];
 If[!ListQ[fc]||Length[fc]=!=1,Continue[]];
 source=If[order===1,
  FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,Lor3,-pA,Lor4,-pC],
  FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,Lor4,-pC,Lor3,-pA]];
 (* Minus for odd external Grassmann permutation, not a modification of the rule. *)
 assert["source vertex with expected external Grassmann sign "<>ToString[order],
  same[First[fc],If[order===1,source,-source]]];
 assert["wrong Grassmann sign is nonzero "<>ToString[order],
  !same[First[fc],If[order===1,-source,source]]];
 Print["EXTERNAL_FIELD_ORDER=",InputForm[fields]],
 {order,2}];
Print["EXTERNAL_GHOST_DIAGNOSTIC_ONLY=True FORM_EXECUTED=False"];
finish[];
