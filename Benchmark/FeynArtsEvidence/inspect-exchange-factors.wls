(* Identify factored, open-index exchange pieces without multiplying vertices. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
spec=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1],FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,spec,{p1,p2},{q1,q2}];
If[!AssociationQ[r],Print["FAIL_MODEL=",InputForm[r]];Quit[2]];
Do[If[d["DenominatorCount"]===1,
  x=d["Expression"];
  den=Cases[x,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[m_,___],0]:>m,Infinity];
  factors=List@@x;
  Print["CHANNEL=",InputForm[den]," FACTOR_COUNT=",Length[factors]];
  Print["PROP_FACTOR_INPUTFORM=",ToString[InputForm[Times@@Take[factors,4]]]];
  Do[Print[" FACTOR=",i," HEAD=",Head[factors[[i]]]," BYTES=",ByteCount[factors[[i]]],
     " INDEX_LABELS=",InputForm[DeleteDuplicates[Cases[factors[[i]],FeynCalc`LorentzIndex[j_,___]:>j,Infinity]]],
     " MOMS=",InputForm[DeleteDuplicates[Cases[factors[[i]],FeynCalc`Momentum[m_,___]:>m,Infinity]]]],
   {i,Length[factors]}]
],{d,r["Diagrams"]}];
Quit[0];
