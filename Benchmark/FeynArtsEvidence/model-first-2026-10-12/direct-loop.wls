(* Direct 1PI inventory and source factors. No integration, FORM or tadpole deletion. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"setup.wl"}]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},FeynArts`CTOrder->0,
 FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],FeynArts`Model->name,
 FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
assert["two 1PI topology types",Length[top]===2];
(* Join topology/class insertion identities to the amplitude GraphID.
   The graph family is derived from valences and inserted loop fields, not row order. *)
inventory={};
Do[
 topology=ins[[ti,1]];
 vertices=DeleteDuplicates[Cases[topology,v:FeynArts`Vertex[_][_] :> v,Infinity]];
 valences=Sort[Select[(#[[0,1]]&/@vertices),#>1&]];
 internal=Cases[topology,FeynArts`Propagator[FeynArts`FALoop[_]][_,_,field_] :> field,Infinity];
 Do[
  gi=genericEntry[[1,0,2,2]];
  Do[
   ci=graph[[0,2,2]];fields=internal/.(List@@graph);
   family=Which[
    valences==={4}&&fields==={FeynArts`T[1]},"GravitonTadpole",
    valences==={3,3}&&fields==={FeynArts`T[1],FeynArts`T[1]},"GravitonBubble",
    valences==={3,3}&&Sort[fields]===Sort[{-FeynArts`U[1],FeynArts`U[1]}],"GhostBubble",
    True,"Unexpected"];
   AppendTo[inventory,<|"Key"->{ti,gi,ci},"Family"->family,"Fields"->fields,
    "Valences"->valences,"FeynArtsSymmetryDenominator"->graph[[0,1]]|>],
   {graph,List@@genericEntry[[2]]}],
  {genericEntry,List@@ins[[ti,2]]}],
 {ti,Length[ins]}];
Print["INSERTION_INVENTORY=",InputForm[inventory]];
assert["one tadpole and two distinct bubbles",Sort[Lookup[inventory,"Family"]]===
 Sort[{"GravitonTadpole","GravitonBubble","GhostBubble"}]];
assert["unique graph identities",DuplicateFreeQ[Lookup[inventory,"Key"]]];
If[failures>0,finish[]];
amp=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},
 FeynArts`MomentumConservation->True,FeynArts`Truncated->True],60,$TimedOut];
If[amp===$TimedOut,assert["amplitude completed",False];finish[]];
ids=Cases[amp,FeynArts`FAFeynAmp[id_,__]:>id,Infinity];
fc=TimeConstrained[FeynCalc`FCFAConvert[amp,FeynCalc`IncomingMomenta->{p},FeynCalc`OutgoingMomenta->{p},
 FeynCalc`LoopMomenta->{ell},FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1],50,$TimedOut];
assert["three converted 1PI contributions",ListQ[fc]&&Length[fc]===3&&Length[ids]===3];
If[!ListQ[fc]||Length[fc]=!=3||Length[ids]=!=3,finish[]];
labelled=Table[
 key=Last/@Take[List@@ids[[i]],3];
 record=SelectFirst[inventory,#["Key"]===key&,Missing["Graph"]];
 If[MissingQ[record],assert["GraphID matches insertion",False];finish[]];
 Join[record,<|"ID"->ids[[i]],"Expression"->fc[[i]]|>],{i,Length[fc]}];
get[family_]:=SelectFirst[labelled,#["Family"]===family&];
tad=get["GravitonTadpole"];grav=get["GravitonBubble"];ghost=get["GhostBubble"];
assert["independent symmetry factors agree with insertion",Lookup[{tad,grav,ghost},"FeynArtsSymmetryDenominator"]==={2,2,1}];
denom[e_]:=Cases[e,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[m_,___],0]:>m,Infinity];
assert["tadpole routing retained",denom[tad["Expression"]]==={ell}];
assert["graviton bubble routes",Sort[denom[grav["Expression"]]]===Sort[{ell,ell-p}]];
assert["ghost bubble routes",Sort[denom[ghost["Expression"]]]===Sort[{ell,ell-p}]];
f1=List@@tad["Expression"];f2=List@@grav["Expression"];f3=List@@ghost["Expression"];
assert["source-product factorisation intact",Length/@{f1,f2,f3}==={5,7,5}];
If[failures>0,finish[]];
(* Independent graph weights: 12/4!=1/2 for tadpole;
   (2*3*3*2)/(2!*(3!)^2)=1/2 for graviton bubble;
   2/2!=1 for oriented ghost bubble, plus one closed Grassmann minus. *)
assert["tadpole source propagator with graph half",same[Times@@Take[f1,4],
 1/2 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor5,Lor6,Lor7,Lor8,ell]]]];
assert["tadpole exact source quartic",same[f1[[5]],
 FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-p,Lor5,Lor6,-ell,Lor7,Lor8,ell],30]];
gravProps=1/2 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor10,Lor9,Lor11,Lor12,ell-p]]*
 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor5,Lor6,Lor7,Lor8,ell]];
assert["graviton bubble source propagators with graph half",same[
 FeynCalc`FeynAmpDenominatorCombine[Times@@Take[f2,5]],FeynCalc`FeynAmpDenominatorCombine[gravProps]]];
assert["graviton bubble first cubic source",same[f2[[6]],
 FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor10,Lor9,ell-p,Lor5,Lor6,-ell]]];
assert["graviton bubble second cubic source",same[f2[[7]],
 FeynGrav`GravitonVertex[Lor3,Lor4,-p,Lor11,Lor12,p-ell,Lor7,Lor8,ell]]];
ghostProps=-FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor5,Lor6,ell]]*
 FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor7,Lor8,ell-p]];
assert["ghost bubble source propagators with one closed-loop minus",same[
 FeynCalc`FeynAmpDenominatorCombine[Times@@Take[f3,3]],FeynCalc`FeynAmpDenominatorCombine[ghostProps]]];
assert["ghost bubble first cubic source",same[f3[[4]],
 FeynGrav`GravitonGhostVertex[Lor1,Lor2,p,Lor5,-ell,Lor8,ell-p]]];
assert["ghost bubble second cubic source",same[f3[[5]],
 FeynGrav`GravitonGhostVertex[Lor3,Lor4,-p,Lor7,p-ell,Lor6,ell]]];
assert["no external wavefunction markers",FreeQ[fc,Global`FAPolarizationTensor]];
Print["ALL_THREE_SOURCE_PRODUCTS_EXACT=",failures===0,
 " NO_LOOP_MEASURE=True NO_TADPOLE_DELETION=True FORM_EXECUTED=False"];
finish[];
