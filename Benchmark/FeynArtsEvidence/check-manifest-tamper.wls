(* Copy immutable bundle once; mutate only the disposable copy. Never load FeynArts. *)
$HistoryLength=0;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
source="/tmp/fgfa_puregr_finite_public_20261014";
target="/tmp/fgfa_puregr_tampered_20261014";
If[FileExistsQ[target],Print["EXISTING_TARGET_REFUSED"];Quit[2]];
CopyDirectory[source,target];
name="FGPureGR5a149c98161e2b2e";mod=FileNameJoin[{target,name<>".mod"}];
Export[mod,Import[mod,"Text"]<>"\n(* deliberate diagnostic byte change in copied file only *)\n","Text"];
model=<|"Directory"->target,"ModelName"->name,"Manifest"->FileNameJoin[{target,"manifest.json"}]|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p},{q1,q2}];
Print["TAMPER_REJECT=",FailureQ[r]," TAG=",If[FailureQ[r],r[[1]],None],
 " FA_UNLOADED=",!MemberQ[$Packages,"FeynArts`"]];
Quit[If[FailureQ[r]&&r[[1]]==="ModelChanged",0,3]];
