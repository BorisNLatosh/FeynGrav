(* Diagnostic open-index conversion ONLY; no physical loop API, FORM, or integration. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
fa=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True],45,$TimedOut];
If[fa===$TimedOut,Print["FA_TIMEOUT"];Quit[3]];
fc=TimeConstrained[FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{ell},
 ChangeDimension->D,Contract->False,List->True,Prefactor->1],55,$TimedOut];
If[fc===$TimedOut || !ListQ[fc],Print["FC_FAILURE=",InputForm[fc]];Quit[4]];
Print["DIAGNOSTIC_GRAPHS=",Length[fc]," BYTES=",ByteCount/@fc];
Do[Print["GRAPH_INDEX=",i," DENOMINATORS=",InputForm[Cases[fc[[i]],_FeynCalc`PropagatorDenominator,Infinity]],
 " UNRESOLVED_FA=",Count[fc[[i]],_Global`FAFourVector|_Global`FAMetricTensor,Infinity],
 " INTEGER_LOOP_MARKERS=",Count[fc[[i]],_FeynArts`FALoop,Infinity],
 " EXPR_PREVIEW=",StringTake[ToString[InputForm[fc[[i]]]],UpTo[600]]],{i,Length[fc]}];
Print["INTEGRATION_MEASURE_ADDED=False LOOP_API_ENABLED=False"];
Quit[If[Length[fc]===3,0,5]];
