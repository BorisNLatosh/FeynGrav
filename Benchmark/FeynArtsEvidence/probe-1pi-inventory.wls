(* Diagnostic graph inventory ONLY; no loop FeynAmp, no integrand and no tadpole exclusion. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=TimeConstrained[FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},
  FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}],18,$TimedOut];
If[top===$TimedOut,Print["TOPOLOGY_TIMEOUT"];Quit[3]];
Print["ONE_PI_TOPOLOGIES=",Length[List@@top]];
Print["TOPOLOGY_PREVIEW=",StringTake[ToString[InputForm[top]],UpTo[6500]]];
ins=TimeConstrained[FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
  FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}],20,$TimedOut];
If[ins===$TimedOut,Print["INSERT_TIMEOUT"];Quit[4]];
Print["INSERTION_PREVIEW=",StringTake[ToString[InputForm[ins]],UpTo[8500]]];
Print["DIAGNOSTIC_ONLY=True FORM_INTEGRAND_BUILT=False TADPOLES_EXCLUDED=False"];
Quit[0];
