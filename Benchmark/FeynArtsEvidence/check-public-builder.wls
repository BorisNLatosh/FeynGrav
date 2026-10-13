$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
target="/tmp/fgfa_puregr_finite_public_20261014";
If[FileExistsQ[target],Print["STOP_EXISTING=",target];Quit[2]];
b=TimeConstrained[FeynGrav`FeynArtsIntegration`FeynGravBuildModel[target],110,$TimedOut];
Print["MODEL_BUILD_SUCCESS=",AssociationQ[b]," RESULT=",InputForm[b]];
If[AssociationQ[b],Print["MANIFEST=",InputForm[Import[b["Manifest"],"RawJSON"]["ModelFileSHA256"]]]];
Quit[If[AssociationQ[b],0,3]];
