(* Bounded index/momentum inventory for independent exchange projections. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1],FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p1,p2},{q1,q2}];
If[!AssociationQ[r],Print["NOT_CONSTRUCTED=",InputForm[r]];Quit[2]];
Do[If[d["DenominatorCount"]===1,
  x=d["Expression"];
  Print["GRAPH=",InputForm[d["ID"]]," HEAD=",Head[x]," BYTES=",ByteCount[x]];
  Print["MOMENTA=",InputForm[DeleteDuplicates[Cases[x,FeynCalc`Momentum[m_,___]:>m,Infinity]]]];
  Print["LORENTZ_LABELS=",InputForm[DeleteDuplicates[Cases[x,FeynCalc`LorentzIndex[i_,___]:>i,Infinity]]]];
  Print["EXPR_PREVIEW=",StringTake[ToString[InputForm[x]],UpTo[650]]]
],{d,r["Diagrams"]}];
Quit[0];
