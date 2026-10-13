(* Small genuine-model whole-leg permutation; off shell and symbolic D. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},
 "Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p},{q2,q1}];
If[!AssociationQ[r],Print["CONSTRUCTION_FAILURE=",InputForm[r]];Quit[2]];
source=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-q2,Lor5,Lor6,-q1];
exact=Expand[r["Diagrams"][[1,"Expression"]]-source]===0;
Print["SWAPPED_OUTGOING_SOURCE_EXACT=",exact," SYMBOLIC_D=",!FreeQ[r["Diagrams"][[1,"Expression"]],D],
 " INTEGRITY_PRESENT=",Length[r["DiagramIntegrity"]]===1];
Quit[If[exact&&Length[r["DiagramIntegrity"]]===1,0,3]];
