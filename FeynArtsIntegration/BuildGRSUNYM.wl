(* Explicit developer entry point. Supply one fresh absolute output directory. *)
$HistoryLength=0;
ymBuildRepo=DirectoryName[DirectoryName[$InputFileName]];
ymBuildArguments=Select[$ScriptCommandLine,StringQ[#]&&StringStartsQ[#,"/"]&];
ymBuildTarget=Environment["FG_MODEL_TARGET"];
If[!StringQ[ymBuildTarget],ymBuildTarget=If[Length[ymBuildArguments]>=2,Last[ymBuildArguments],""]];
If[!StringQ[ymBuildTarget]||!StringStartsQ[ymBuildTarget,"/"]||FileExistsQ[ymBuildTarget],Print["Supply a fresh absolute output directory."];Exit[2]];
$LoadAddOns={"FeynArts"};Get[FileNameJoin[{ymBuildRepo,"FeynGrav.wl"}]];
Get[FileNameJoin[{ymBuildRepo,"FeynArtsIntegration","FeynArtsIntegration.wl"}]];
Get[FileNameJoin[{ymBuildRepo,"FeynArtsIntegration","YangMillsModelBuilder.wl"}]];
ymBuildResult=Check[FeynGrav`FeynArtsIntegration`Private`ymBuild[ymBuildTarget],$Failed];
If[!AssociationQ[ymBuildResult],Print[InputForm[ymBuildResult]];Exit[1]];
Print["BUILT candidate GR-SU-N-Yang-Mills: ",ymBuildTarget,". Native verification is required before installation."];
