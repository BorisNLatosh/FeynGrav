(* Bounded explicit pure-GR model snapshot; no diagram construction or FORM. *)
$HistoryLength=0; $LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/Basis.wl"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"model-builder-prototype.wl"}]];
target="/tmp/fgfa_puregr_finite_20261014b";
If[FileExistsQ[target],Print["STOP: target already exists; no overwrite"];Quit[2]];
result=TimeConstrained[FeynGrav`FeynArtsIntegration`Private`faBuildModel[target],110,$TimedOut];
Print["BUILD_RESULT=",InputForm[result]];
If[AssociationQ[result],Print["GEN_BYTES=",FileByteCount[result["GenericModel"]],
 " MOD_BYTES=",FileByteCount[result["ClassesModel"]]]];
Quit[If[AssociationQ[result],0,1]];
