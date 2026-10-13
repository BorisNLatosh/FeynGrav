(* Isolated ghost-bubble diagnostic ONLY: cubic topologies; production inventory retains tadpole. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
fa=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
graphs=Cases[fa,_FeynArts`FAFeynAmp,Infinity];
If[Length[graphs]=!=2,Print["INCOMPLETE_BUBBLE_INSERTIONS=",Length[graphs]];Quit[3]];
Print["GHOST_GRAPH=",InputForm[Last[graphs][[1]]]];
fg=Head[fa][Last[graphs]];
fc=TimeConstrained[FCFAConvert[fg,IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{ell},
 ChangeDimension->D,Contract->False,List->True,Prefactor->1],35,$TimedOut];
If[!ListQ[fc]||Length[fc]=!=1,Print["FC_FAILED=",InputForm[fc]];Quit[4]];
f=List@@First[fc]; If[Length[f]=!=5,Print["BAD_FACTOR_SHAPE=",Length[f]];Quit[5]];
same[a_,b_]:=TrueQ[TimeConstrained[Expand[FeynCalc`MomentumExpand[a]-FeynCalc`MomentumExpand[b]],6,$TimedOut]===0];
ghostP=FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor5,Lor6,ell]]*
 FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor7,Lor8,ell-p]];
routeFA=Cases[f[[1]],_FeynCalc`PropagatorDenominator,Infinity];
routeSource=Cases[ghostP,_FeynCalc`PropagatorDenominator,Infinity];
routes=Sort[FeynCalc`MomentumExpand/@routeFA]===Sort[FeynCalc`MomentumExpand/@routeSource];
num=(-ghostP)/. _FeynCalc`FeynAmpDenominator->1;
metrics=same[f[[2]]*f[[3]],num];
search[expr_,indlist_,k_,momlist_] := Module[{hits={},v},
 Do[v=FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,inds[[1]],mom,inds[[2]],-k-mom];
    If[same[expr,v],AppendTo[hits,{inds,mom,1}]];
    If[same[expr,-v],AppendTo[hits,{inds,mom,-1}]],
  {inds,indlist},{mom,momlist}]; hits];
v1=search[f[[4]],{{Lor8,Lor5},{Lor5,Lor8}},p,{ell-p,-ell}];
v2=search[f[[5]]/.{Lor3->Lor1,Lor4->Lor2},
 {{Lor7,Lor6},{Lor6,Lor7}},-p,{ell,p-ell}];
Print["PROP_DENOMINATORS_EXACT=",routes," GHOST_MINUS_AND_I2_METRICS_EXACT=",metrics,
 " VERTEX_ONE_MATCHES=",InputForm[v1]," VERTEX_TWO_MATCHES=",InputForm[v2]];
Print["TADPOLES_NOT_EXCLUDED_IN_PRODUCTION=True ISOLATED_DIAGNOSTIC_ONLY=True"];
Quit[If[routes&&metrics&&v1=!={}&&v2=!={},0,6]];
