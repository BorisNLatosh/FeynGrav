(* Standard FeynArts 2->2 construction and independent source-factor checks. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"setup.wl"}]];
top=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}];
ins=FeynArts`InsertFields[top,{FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
amp=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},
 FeynArts`MomentumConservation->True,FeynArts`Truncated->True],40,$TimedOut];
If[amp===$TimedOut,assert["amplitude construction completed",False];finish[]];
fc=TimeConstrained[FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{p1,p2},FeynCalc`OutgoingMomenta->{q1,q2},
 FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1],45,$TimedOut];
assert["exactly four tree amplitudes",ListQ[fc]&&Length[fc]===4];
If[!ListQ[fc]||Length[fc]=!=4,finish[]];
denom[e_]:=Cases[e,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[m_,___],0]:>m,Infinity];
assert["one contact and three exchanges",Sort[Length/@(denom/@fc)]==={0,1,1,1}];
contact=Select[fc,denom[#]==={}&];
assert["quartic contact equals FeynGrav source",Length[contact]===1&&same[First[contact],
 FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,-q1,Lor7,Lor8,-q2],30]];
(* Independent source products, all momenta incoming to their own vertex.
   Select graphs by denominator route, never amplitude list position. *)
routes={
 {q1+q2,{Lor1,Lor2,p1,Lor3,Lor4,p2,Lor10,Lor9,-q1-q2},
  {Lor11,Lor12,q1+q2,Lor5,Lor6,-q1,Lor7,Lor8,-q2}},
 {-p2+q2,{Lor1,Lor2,p1,Lor5,Lor6,-q1,Lor10,Lor9,p2-q2},
  {Lor11,Lor12,-p2+q2,Lor3,Lor4,p2,Lor7,Lor8,-q2}},
 {-p2+q1,{Lor1,Lor2,p1,Lor7,Lor8,-q2,Lor10,Lor9,p2-q1},
  {Lor11,Lor12,-p2+q1,Lor3,Lor4,p2,Lor5,Lor6,-q1}}};
Do[
 selected=Select[fc,denom[#]==={route[[1]]}&];
 assert["unique exchange route "<>ToString[route[[1]],InputForm],Length[selected]===1];
 If[Length[selected]=!=1,Continue[]];
 factors=List@@First[selected];
 assert["factored exchange has six factors",Length[factors]===6];
 If[Length[factors]=!=6,Continue[]];
 prop=FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor10,Lor9,Lor11,Lor12,route[[1]]]];
 left=FeynGrav`GravitonVertex@@route[[2]];right=FeynGrav`GravitonVertex@@route[[3]];
 assert["exchange propagator equals source",same[Times@@Take[factors,4],prop]];
 assert["exchange left cubic equals source",same[factors[[5]],left]];
 assert["exchange right cubic equals source",same[factors[[6]],right]];
 Print["CHECKED_SOURCE_ROUTE=",InputForm[route[[1]]]],
 {route,routes}];
permuted=TimeConstrained[FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{p1,p2},FeynCalc`OutgoingMomenta->{q2,q1},
 FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1],40,$TimedOut];
assert["swapped external momentum assignment preserves complete factored list",permuted===(fc/.{q1->q2,q2->q1})];
assert["contact whole-outgoing-leg symmetry",Length[contact]===1&&same[First[contact],
 First[contact]/.{q1->q2,q2->q1,Lor5->Lor7,Lor7->Lor5,Lor6->Lor8,Lor8->Lor6},30]];
assert["no external wavefunction markers",FreeQ[fc,Global`FAPolarizationTensor]];
Print["VERTEX_MOMENTUM_CONSERVATION=True EXTERNAL_MOMENTUM_ELIMINATION=False ON_SHELL=False FORM_EXECUTED=False"];
finish[];
