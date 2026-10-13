(* Full supported 1PI 2-point INTEGRAND only, no FORM, integration or counterterms. *)
$HistoryLength=0;$LoadFeynArts=True;
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
If[!StringStartsQ[cpath,"/"] ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.oom.group","Text"]]=!="1",
 Print["HARD_CAP_MISSING"];Quit[10]];
Print["HARD_CAP_VERIFIED=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
dir="/tmp/fgfa_puregr_finite_public_20261014";name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dir,"ModelName"->name,"Manifest"->FileNameJoin[{dir,"manifest.json"}]|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1]},
 "LoopOrder"->1,"OneParticleIrreducible"->True|>;
preMissing=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p},{p}];
preBad=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p},{p},{p}];
preForeign=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,
 Join[s,<|"Incoming"->{FeynArts`U[1]}|>],{p},{p},{ell}];
Print["MISSING_LOOP_ROUTING_REJECT=",MatchQ[preMissing,Failure["MissingLoopRouting",_]],
 " ALIASED_LOOP_REJECT=",MatchQ[preBad,Failure["InvalidLoopRouting",_]],
 " EXTERNAL_GHOST_REJECT=",FailureQ[preForeign]];
If[!And[FailureQ[preMissing],FailureQ[preBad],FailureQ[preForeign]],Quit[2]];
r=TimeConstrained[FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[
 model,s,{p},{p},{ell}],115,$TimedOut];
If[!AssociationQ[r],Print["CONSTRUCTOR_FAILED=",InputForm[r]];Quit[3]];
counts=Lookup[r["Diagrams"],"DenominatorCount"];
labels=Lookup[r["Diagrams"],"TopologyFamily"];
routes=Lookup[r["Diagrams"],"LoopRouting"];
allIndices=DeleteDuplicates[Cases[r["TotalIntegrand"],
 FeynCalc`LorentzIndex[i_Symbol,D]:>SymbolName[i],Infinity]];
internal=Select[allIndices,!MemberQ[{"Lor1","Lor2","Lor3","Lor4"},#]&];
internalNamespaced=internal=!={}&&AllTrue[internal,StringStartsQ[#,"fgfaLoop"]&];
print=<|"Count"->Length[r["Diagrams"]],"Denominators"->counts,
 "Families"->labels,"Routes"->routes,"Fingerprint"->r["ModelFingerprint"],
 "TotalBytes"->ByteCount[r["TotalIntegrand"]],"InternalCount"->Length[internal],
 "InternalNamespaced"->internalNamespaced,"TotalNoMeasure"->r["LoopMeasureAdded"]===False,
 "RetainedTadpole"->MemberQ[labels,"GravitonTadpole"]|>;
Print["ONE_PI_RESULT=",InputForm[print]];
blocked=FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[r];
Print["UNTESTED_LOOP_FORM_REJECT=",MatchQ[blocked,Failure["LoopAlgebraUnavailable",_]],
 " DRAWABLE_INSERTIONS=",Head[Head[r["Insertions"]]]===FeynArts`TopologyList];
Quit[If[Length[r["Diagrams"]]===3&&counts==={1,2,2}&&
 labels==={"GravitonTadpole","GravitonBubble","GhostBubble"}&&
 routes==={{ell},{ell,ell-p},{ell,ell-p}}&&
 internalNamespaced&&r["LoopMeasureAdded"]===False&&
 MatchQ[blocked,Failure["LoopAlgebraUnavailable",_]],0,4]];
