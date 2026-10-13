(* Deliberately bounded diagnostic only; no integration, FORM, or public loop support. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["INIT_FAILED"];Quit[2]];
top=TimeConstrained[FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{3,4},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}],15,$TimedOut];
If[top===$TimedOut,Print["TOPOLOGY_TIMEOUT"];Quit[3]];
ins=TimeConstrained[FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}],15,$TimedOut];
If[ins===$TimedOut,Print["INSERTION_TIMEOUT"];Quit[4]];
a=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True],50,$TimedOut];
If[a===$TimedOut,Print["FA_AMP_TIMEOUT"];Quit[5]];
Print["GRAPH_COUNT=",Length[Cases[a,_FeynArts`FAFeynAmp,Infinity]]," TOTAL_BYTES=",ByteCount[a]];
rows=Cases[a,FeynArts`FAFeynAmp[id_,int_,expr_]:>{id,int,expr},Infinity];
Do[Print["GRAPH_ID=",InputForm[row[[1]]]," INTEGRAL=",StringTake[ToString[InputForm[row[[2]]]],UpTo[600]],
 " SIZE=",ByteCount[row[[3]]]," PROP_COUNT=",Count[row[[3]],_FeynArts`FAPropagatorDenominator,Infinity],
 " GHOST_FIELDS=",Count[row[[3]],_FeynArts`U,Infinity],
 " PREVIEW=",StringTake[ToString[InputForm[row[[3]]]],UpTo[550]]],{row,rows}];
Print["ONLY_DIAGNOSTIC=True MODEL_TADPOLE_RETAINED=True"];
Quit[If[Length[rows]===3,0,6]];
