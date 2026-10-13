(* Bounded off-shell ghost interaction diagnostic, NOT an allowed physical process. *)
$HistoryLength=0;$LoadFeynArts=True;Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_20261014b";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Print["FAIL INIT"];Quit[2]];
short[e_]:=StringTake[ToString[InputForm[e]],UpTo[950]];
opts={FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}};
Do[
  top=FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3}];
  ins=TimeConstrained[FeynArts`InsertFields[top,FeynArts`T[1]->out,Sequence@@opts],12,$TimedOut];
  Print["OUT=",InputForm[out]," INSERT=",short[ins]];
  If[ins===$TimedOut||FreeQ[ins,FeynArts`FeynmanGraph],Continue[]];
  amp=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True],12,$TimedOut];
  Print["AMP=",short[amp]];
  If[amp===$TimedOut,Continue[]];
  fc=TimeConstrained[FCFAConvert[amp,IncomingMomenta->{k},OutgoingMomenta->{pA,pC},ChangeDimension->D,Contract->False,List->True,Prefactor->1],12,$TimedOut];
  Print["FC=",short[fc]," RESIDUAL_FA=",Count[fc,_FeynArts`C,Infinity]];
  If[ListQ[fc]&&Length[fc]===1,
    source=FeynGrav`GravitonGhostVertex[Lor1,Lor2,k,Lor4,-pC,Lor3,-pA];
    exact=TimeConstrained[Expand[First[fc]-source],12,$TimedOut];
    negative=TimeConstrained[Expand[First[fc]+source],12,$TimedOut];
    Print["SOURCE_MATCH_PLUS=",TrueQ[exact===0]," SOURCE_MATCH_MINUS=",TrueQ[negative===0]," SOURCE_START=",short[source]];
  ];
,{out,{{-FeynArts`U[1],FeynArts`U[1]},{FeynArts`U[1],-FeynArts`U[1]}}}];
Quit[0];
