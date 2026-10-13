(* Invalid path requests must not load FeynArts or import any source library. *)
$HistoryLength=0;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
a=FeynGrav`FeynArtsIntegration`FeynGravBuildModel["relative-target"];
b=FeynGrav`FeynArtsIntegration`FeynGravBuildModel["/tmp/../tmp/fgfa-no-create"];
Print["RELATIVE_REFUSED=",MatchQ[a,Failure["InvalidModelPath",_]],
 " NON_NORMALISED_REFUSED=",MatchQ[b,Failure["InvalidModelPath",_]],
 " FA_UNLOADED=",!MemberQ[$Packages,"FeynArts`"],
 " FG_UNLOADED=",!MemberQ[$Packages,"FeynGrav`"],
 " NO_OUTPUT=",!FileExistsQ["/tmp/fgfa-no-create"]];
Quit[If[MatchQ[a,Failure["InvalidModelPath",_]]&&MatchQ[b,Failure["InvalidModelPath",_]]&&
 !MemberQ[$Packages,"FeynArts`"]&&!MemberQ[$Packages,"FeynGrav`"]&&
 !FileExistsQ["/tmp/fgfa-no-create"],0,2]];
