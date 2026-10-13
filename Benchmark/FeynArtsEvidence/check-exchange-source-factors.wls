(* Independent exact factor-by-factor four-point exchange comparison; no huge product expansion. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1],FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p1,p2},{q1,q2}];
If[!AssociationQ[r],Print["FAIL_TREE=",InputForm[r]];Quit[2]];
routes={
 {q1+q2,
   {Lor1,Lor2,p1,Lor3,Lor4,p2,Lor10,Lor9,-q1-q2},
   {Lor11,Lor12,q1+q2,Lor5,Lor6,-q1,Lor7,Lor8,-q2}},
 {-p2+q2,
   {Lor1,Lor2,p1,Lor5,Lor6,-q1,Lor10,Lor9,p2-q2},
   {Lor11,Lor12,-p2+q2,Lor3,Lor4,p2,Lor7,Lor8,-q2}},
 {-p2+q1,
   {Lor1,Lor2,p1,Lor7,Lor8,-q2,Lor10,Lor9,p2-q1},
   {Lor11,Lor12,-p2+q1,Lor3,Lor4,p2,Lor5,Lor6,-q1}}
};
passed=True;
Do[
 x=SelectFirst[r["Diagrams"],
   With[{den=Cases[#["Expression"],FeynCalc`PropagatorDenominator[FeynCalc`Momentum[m_,___],0]:>m,Infinity]},
     den==={route[[1]]}]&,Missing["Route"]];
 If[MissingQ[x],Print["MISSING_ROUTE=",InputForm[route[[1]]]];passed=False;Continue[]];
 factors=List@@x["Expression"];
 If[Length[factors]=!=6,Print["UNFACTORED_ROUTE=",InputForm[route[[1]]]];passed=False;Continue[]];
 prop=FeynCalc`FCI[FeynGrav`GravitonPropagator[Lor10,Lor9,Lor11,Lor12,route[[1]]]];
 vl=FeynGrav`GravitonVertex@@route[[2]];
 vr=FeynGrav`GravitonVertex@@route[[3]];
 dp=TimeConstrained[Expand[FeynCalc`MomentumExpand[Times@@Take[factors,4]]-
   FeynCalc`MomentumExpand[prop]],12,$TimedOut];
 dl=TimeConstrained[Expand[factors[[5]]-vl],12,$TimedOut];
 dr=TimeConstrained[Expand[factors[[6]]-vr],12,$TimedOut];
 If[!TrueQ[dp===0],Print["SOURCE_PROP_PREVIEW=",StringTake[ToString[InputForm[prop]],UpTo[1100]],
   " PROP_RESIDUAL_PREVIEW=",StringTake[ToString[InputForm[dp]],UpTo[1100]]]];
 print={TrueQ[dp===0],TrueQ[dl===0],TrueQ[dr===0]};
 Print["ROUTE=",InputForm[route[[1]]]," PROP_EXACT=",print[[1]]," LEFT_EXACT=",print[[2]]," RIGHT_EXACT=",print[[3]],
  " RESIDUAL_BYTES=",ByteCount/@{dp,dl,dr}];
 passed=passed&&And@@print;
,{route,routes}];
Print["ALL_EXCHANGES_SOURCE_EXACT=",passed];
Quit[If[passed,0,3]];
