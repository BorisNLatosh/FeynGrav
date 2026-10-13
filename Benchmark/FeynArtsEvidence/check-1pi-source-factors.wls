(* Research-only source-product audit at the first 1PI two-point integrand.
   No integral evaluation, FORM loop reduction or public loop feature. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
fa=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
fc=TimeConstrained[FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{ell},
 ChangeDimension->D,Contract->False,List->True,Prefactor->1],50,$TimedOut];
If[fc===$TimedOut||!ListQ[fc]||Length[fc]=!=3,Print["CONVERSION_FAILED"];Quit[3]];
f1=List@@fc[[1]]; f2=List@@fc[[2]]; f3=List@@fc[[3]];
same[a_,b_,sec_:18]:=TrueQ[TimeConstrained[Expand[FeynCalc`MomentumExpand[a]-FeynCalc`MomentumExpand[b]],sec,$TimedOut]===0];
(* The symmetry 1/2 multiplies a single source graviton propagator. *)
tadP=1/2 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor5,Lor6,Lor7,Lor8,ell]];
tadPExact=same[Times@@Take[f1,4],tadP];
tadVs=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-p,Lor5,Lor6,ell,Lor7,Lor8,-ell];
tadVExact=same[f1[[5]],tadVs,24];
Print["TADPOLE_HALF_PROP_EXACT=",tadPExact," QUARTIC_EXACT=",tadVExact];
(* Two source graviton propagators, one graph symmetry 1/2. *)
bubP=1/2 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor10,Lor9,Lor11,Lor12,ell-p]] *
 FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor5,Lor6,Lor7,Lor8,ell]];
bubPExact=same[FeynCalc`FeynAmpDenominatorCombine[Times@@Take[f2,5]],
 FeynCalc`FeynAmpDenominatorCombine[bubP],20];
vlcandidates={
 FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor10,Lor9,ell-p,Lor5,Lor6,-ell],
 FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor10,Lor9,-ell,Lor5,Lor6,ell-p]};
vrcandidates={
 FeynGrav`GravitonVertex[Lor3,Lor4,-p,Lor11,Lor12,ell,Lor7,Lor8,p-ell],
 FeynGrav`GravitonVertex[Lor3,Lor4,-p,Lor11,Lor12,p-ell,Lor7,Lor8,ell]};
leftMatches=(same[f2[[6]],#,12]&/@vlcandidates);
rightMatches=(same[f2[[7]],#,12]&/@vrcandidates);
Print["GRAV_BUBBLE_HALF_TWO_PROPS_EXACT=",bubPExact," LEFT_VERTEX_CANDIDATES=",InputForm[leftMatches],
 " RIGHT_VERTEX_CANDIDATES=",InputForm[rightMatches]];
(* Two oriented ghost propagators give I^2=-1; one closed-loop minus gives +1.
   Compare each cubic factor to all momentum-conserving ghost/antighost slots. *)
ghostP=FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor5,Lor6,ell]] *
 FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor7,Lor8,ell-p]];
(* Strip only the two FAD factors; denominator routing is checked separately. *)
denFA=Cases[f3[[1]],_FeynCalc`PropagatorDenominator,Infinity];
denSource=Cases[ghostP,_FeynCalc`PropagatorDenominator,Infinity];
routeExact=Sort[FeynCalc`MomentumExpand/@denFA]===Sort[FeynCalc`MomentumExpand/@denSource];
metricExact=same[f3[[2]]*f3[[3]],
 (FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor5,Lor6,ell]] /.
    _FeynCalc`FeynAmpDenominator->1)*
 (FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor7,Lor8,ell-p]] /.
    _FeynCalc`FeynAmpDenominator->1) / (-1)];
(* Raw FeynArts prefactor +1 should equal ghost-loop -1 times two I propagators. *)
Print["GHOST_ROUTES_EXACT=",routeExact," CLOSED_MINUS_TIMES_TWO_I_PROP_METRICS_EXACT=",metricExact];
search[expr_,idxs_,k_,momOptions_] := Module[{hits={}},
 Do[
  v=FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,inds[[1]],mom,inds[[2]],-k-mom];
  If[same[expr,v,6],AppendTo[hits,{inds,mom,1}]];
  If[same[expr,-v,6],AppendTo[hits,{inds,mom,-1}]],
 {inds,idxs},{mom,momOptions}];hits];
(* Second external h uses Lor3,Lor4, so substitute the graviton slots after forming a source vertex. *)
gh1=search[f3[[4]],{{Lor8,Lor5},{Lor5,Lor8}},p,{ell-p,-ell}];
gh2=search[f3[[5]]/.{Lor3->Lor1,Lor4->Lor2},
 {{Lor7,Lor6},{Lor6,Lor7}},-p,{ell,p-ell}];
Print["GHOST_VERTEX_FIRST_MATCHES=",InputForm[gh1]," SECOND_MATCHES=",InputForm[gh2]];
Print["RESEARCH_ONLY=True NO_LOOP_MEASURE=True"];
Quit[If[tadPExact&&tadVExact&&bubPExact&&MemberQ[leftMatches,True]&&MemberQ[rightMatches,True]&&
 routeExact&&metricExact&&gh1=!={}&&gh2=!={},0,4]];
