(* Direct standard-model example: graviton propagator and cubic tree. No FORM. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"setup.wl"}]];
assert["tensor carries two indices",FeynArts`KinematicIndices[FeynArts`T]==={Global`Lorentz,Global`Lorentz}];
assert["gravitational ghost carries one index",FeynArts`KinematicIndices[FeynArts`U]==={Global`Lorentz}];
li[i_]:=FeynArts`Index[Global`Lorentz,i];
prop=FeynArts`AnalyticalPropagator[FeynArts`Internal][
 FeynArts`T[1,p,{li[1],li[2]}->{li[3],li[4]}]];
convertedProp=FeynCalc`ChangeDimension[FeynCalc`FCPrepareFAAmp[prop],D]/.FeynArts`PV[x_]:>x;
assert["graviton propagator equals FeynGrav source",same[convertedProp,
 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor1,Lor2,Lor3,Lor4,p]]]];
top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3,4}];
ins=FeynArts`InsertFields[top,{FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
amp=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},
 FeynArts`MomentumConservation->True,FeynArts`Truncated->True];
fc=TimeConstrained[FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{p},FeynCalc`OutgoingMomenta->{q1,q2},
 FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1],35,$TimedOut];
assert["exactly one cubic tree",ListQ[fc]&&Length[fc]===1];
If[!ListQ[fc]||Length[fc]=!=1,finish[]];
source=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-q1,Lor5,Lor6,-q2];
assert["cubic tree equals FeynGrav source",same[First[fc],source]];
assert["six external Lorentz indices survive",Sort[DeleteDuplicates[Cases[fc,FeynCalc`LorentzIndex[i_,D]:>i,Infinity]]]===Sort[{Lor1,Lor2,Lor3,Lor4,Lor5,Lor6}]];
assert["no external wavefunction or propagator",FreeQ[fc,Global`FAPolarizationTensor|FeynCalc`PropagatorDenominator]];
permuted=FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{p},FeynCalc`OutgoingMomenta->{q2,q1},
 FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1];
assert["swapped outgoing momentum assignments equal source",same[First[permuted],
 FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-q2,Lor5,Lor6,-q1]]];
assert["within-pair tensor symmetry",same[First[fc],First[fc]/.{Lor1->Lor2,Lor2->Lor1}]];
Print["VERTEX_MOMENTUM_CONSERVATION=True EXTERNAL_MOMENTUM_ELIMINATION=False ON_SHELL=False FORM_EXECUTED=False"];
finish[];
