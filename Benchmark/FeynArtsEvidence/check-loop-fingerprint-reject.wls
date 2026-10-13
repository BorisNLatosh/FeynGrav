(* Mutate ONLY a disposable manifest copy; no topology generation or FORM. *)
$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
src="/tmp/fgfa_puregr_finite_public_20261014";
dst="/tmp/fgfa_loop_bad_snapshot_20261012";
If[FileExistsQ[dst],Print["EXISTING_TARGET_REFUSED"];Quit[2]];
CopyDirectory[src,dst];
path=FileNameJoin[{dst,"manifest.json"}];
manifest=Import[path,"RawJSON"];
manifest=Join[manifest,<|"SnapshotSHA256"->StringRepeat["0",64]|>];
Export[path,manifest,"JSON"];
name="FGPureGR5a149c98161e2b2e";
model=<|"Directory"->dst,"ModelName"->name,"Manifest"->path|>;
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1]},
 "LoopOrder"->1,"OneParticleIrreducible"->True|>;
r=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model,s,{p},{p},{ell}];
Print["MUTATED_SNAPSHOT_REJECT=",MatchQ[r,Failure["GhostCompletenessGate",_]],
 " MODEL_BYTES_UNCHANGED=",And@@Table[
  FileHash[FileNameJoin[{src,name<>suffix}],"SHA256"]===
   FileHash[FileNameJoin[{dst,name<>suffix}],"SHA256"],{suffix,{".gen",".mod"}}]];
Quit[If[MatchQ[r,Failure["GhostCompletenessGate",_]],0,3]];
