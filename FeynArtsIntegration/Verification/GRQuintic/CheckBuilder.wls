(* Run with patched FeynArts in a fresh, externally bounded kernel. *)
$HistoryLength=0; $FeynCalcStartupMessages=False; $LoadAddOns={"FeynArts"};
grRun=Environment["GR5_OUTPUT"]; grRoot=Environment["GR5_PACKAGE"]; grDst=Environment["GR5_MODEL"];
If[!StringQ[grRoot],grRoot=DirectoryName[$InputFileName,4]];
grOrder=Switch[Environment["GR5_ORDER"],"2",2,"3",3,_,Quit[2]];
If[!StringQ[grRun] || !DirectoryQ[grRun] || !StringQ[grDst] || FileExistsQ[grDst] ||
    AnyTrue[{"completion.json","diagnostics.log"},FileExistsQ[FileNameJoin[{grRun,#}]]&],Quit[2]];
grLog=OpenWrite[FileNameJoin[{grRun,"diagnostics.log"}]];$Messages={grLog};
grNeed[label_,ok_]:=If[TrueQ[ok],Print["PASS ",label],Print["FAIL ",label];Quit[1]];
Needs["FeynGrav`"];Get[FileNameJoin[{grRoot,"FeynArtsIntegration","FeynArtsIntegration.wl"}]];
{grSecs,grJob}=AbsoluteTiming[If[grOrder===2,FeynGrav`FeynArtsIntegration`FeynGravBuildModel[grDst],
  FeynGrav`FeynArtsIntegration`FeynGravBuildModel[grDst,FeynGrav`FeynArtsIntegration`GravitonOrder->3]]];
If[FailureQ[grJob],Print[grJob]];grNeed["public build",AssociationQ[grJob]];grManifest=Import[grJob["Manifest"],"RawJSON"];
grNeed["requested libraries loaded",FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportGravitons]["Orders"]["GravitonVertex"]===Range[grOrder]];
grNeed["covered orders",grManifest["GravitonOrders"]===Range[grOrder]];
grNeed["model hashes",FileHash[grJob["GenericModel"],"SHA256","HexString"]===grManifest["ModelFileSHA256"]["Generic"]&&
  FileHash[grJob["ClassesModel"],"SHA256","HexString"]===grManifest["ModelFileSHA256"]["Classes"]];
grNeed["existing target refused",FailureQ[FeynGrav`FeynArtsIntegration`FeynGravBuildModel[grDst,FeynGrav`FeynArtsIntegration`GravitonOrder->grOrder]]];
If[grOrder===2,
  grNeed["default basis",grJob["VertexBasisSizes"]==={231,4158,9}];
  Do[grNeed["packaged model bytes",FileHash[FileNameJoin[{grDst,f}],"SHA256"]===
      FileHash[FileNameJoin[{grRoot,"FeynArts","Models","GR",f}],"SHA256"]],{f,{"GR.gen","GR.mod"}}],
  grNeed["compact native basis",grJob["VertexBasisSizes"]==={231,4158,1,9}&&grManifest["QuinticRepresentation"]==="SymmetricSeed-v1"];
  grIdx=Table[Symbol["FeynArts`li"<>ToString[i]],{i,10}];grPs=Table[Symbol["FeynArts`mom"<>ToString[i]],{i,5}];
  grInfo=FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportGravitons];
  grNeed["changed library excluded",!FeynGrav`FeynArtsIntegration`Private`faVerifiedQuinticQ[<|"Files"->{}|>,grIdx,grPs]];
  grDirty=Internal`InheritedBlock[{FeynCalc`Pair},
      FeynCalc`Pair[FeynCalc`Momentum[FeynArts`mom1,D],FeynCalc`Momentum[FeynArts`mom1,D]]=0;
      FeynGrav`FeynArtsIntegration`Private`faVerifiedQuinticQ[grInfo,grIdx,grPs]];
  grNeed["fixed kinematics excluded",grDirty===False];
  grNeed["missing template rejected",FailureQ[Block[{FeynGrav`FeynArtsIntegration`Private`$faQuinticTemplate=FileNameJoin[{grRun,"absent"}]},
      FeynGrav`FeynArtsIntegration`Private`faQuinticRecord[grIdx,grPs]]]]
];
Close[grLog];grNeed["clean diagnostics",FileByteCount[FileNameJoin[{grRun,"diagnostics.log"}]]===0];
Export[FileNameJoin[{grRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"Seconds"->grSecs,
  "BasisSizes"->grJob["VertexBasisSizes"],"Manifest"->grJob["Manifest"]|>,"RawJSON"];Quit[0];
