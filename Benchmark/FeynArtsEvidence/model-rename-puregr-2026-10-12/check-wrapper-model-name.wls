(* The model name is now the fixed identifier "PureGR". This checks that the
   builder stamps it, that the diagram validator accepts it, and that the
   validator still rejects malformed or mismatched names. *)
$HistoryLength=0;
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
cgroup="/sys/fs/cgroup"<>cpath;
If[!StringStartsQ[cpath,"/"] ||
 StringTrim[Import[cgroup<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import[cgroup<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import[cgroup<>"/memory.oom.group","Text"]]=!="1",
 Print["REQUIRED_AGGREGATE_CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
checks=0;failures=0;
assert[label_,condition_]:=(checks++;Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],failures++]);
$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
build=FeynGrav`FeynArtsIntegration`FeynGravBuildModel;
target=FileNameJoin[{$TemporaryDirectory,"fgfa-rename-check-"<>CreateUUID[]}];
Print["TARGET=",target];
b=TimeConstrained[build[target],150,$TimedOut];
Print["BUILD_RESULT=",InputForm[b]];
assert["model built",AssociationQ[b]];
If[!AssociationQ[b],Print["COMPLETED checks=",checks," failures=",failures];Quit[3]];
assert["model name is PureGR",b["ModelName"]==="PureGR"];
files=Sort[FileNames["*",target]];
Print["FILES=",InputForm[files]];
assert["files carry the new name",Sort[FileNames["PureGR.*",target]]===
 Sort[{FileNameJoin[{target,"PureGR.gen"}],FileNameJoin[{target,"PureGR.mod"}]}]];
manifest=Import[b["Manifest"],"RawJSON"];
assert["manifest records the new name",manifest["ModelName"]==="PureGR"];
assert["manifest still pins the file bytes",
 Lookup[manifest["ModelFileSHA256"],"Generic"]===FileHash[FileNameJoin[{target,"PureGR.gen"}],"SHA256","HexString"]&&
 Lookup[manifest["ModelFileSHA256"],"Classes"]===FileHash[FileNameJoin[{target,"PureGR.mod"}],"SHA256","HexString"]];
assert["snapshot hash still recorded",StringLength[manifest["SnapshotSHA256"]]===64];
(* The public tree API validates the model through the changed name check. *)
gravField=FeynArts`T[1];
three=<|"Model"->"PureGR","Incoming"->{gravField},"Outgoing"->{gravField,gravField},"LoopOrder"->0|>;
four=<|"Model"->"PureGR","Incoming"->{gravField,gravField},"Outgoing"->{gravField,gravField},"LoopOrder"->0|>;
create=FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude;
r3=TimeConstrained[create[b,three,{p},{q1,q2}],60,$TimedOut];
assert["three-point tree accepted with the new name",AssociationQ[r3]];
If[AssociationQ[r3],
 source3=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-q1,Lor5,Lor6,-q2];
 residual=TimeConstrained[Expand[r3["Diagrams"][[1,"Expression"]]-source3],15,$TimedOut];
 assert["three-point diagram is the exact source cubic",TrueQ[residual===0]]];
r4=TimeConstrained[create[b,four,{p1,p2},{q1,q2}],90,$TimedOut];
assert["four-point tree accepted with the new name",AssociationQ[r4]];
If[AssociationQ[r4],
 counts=Lookup[r4["Diagrams"],"DenominatorCount"];
 assert["one contact and three exchanges",Sort[counts]==={0,1,1,1}];
 contact=Pick[Lookup[r4["Diagrams"],"Expression"],counts,0];
 source4=FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,-q1,Lor7,Lor8,-q2];
 residual4=If[Length[contact]===1,TimeConstrained[Expand[First[contact]-source4],20,$TimedOut],$Failed];
 assert["contact is the exact source quartic",TrueQ[residual4===0]]];
(* The replacement name check must still reject bad input. *)
badShape=<|"Directory"->target,"ModelName"->"Bad-Name","Manifest"->b["Manifest"]|>;
rejectedShape=create[badShape,three,{p},{q1,q2}];
Print["BAD_SHAPE_RESULT=",InputForm[If[FailureQ[rejectedShape],rejectedShape[[1]],rejectedShape]]];
assert["a non-identifier name is rejected",FailureQ[rejectedShape]&&rejectedShape[[1]]==="InvalidModel"];
mismatch=<|"Directory"->target,"ModelName"->"OtherName","Manifest"->b["Manifest"]|>;
rejectedMismatch=create[mismatch,three,{p},{q1,q2}];
Print["MISMATCH_RESULT=",InputForm[If[FailureQ[rejectedMismatch],rejectedMismatch[[1]],rejectedMismatch]]];
assert["a name that disagrees with the manifest is rejected",
 FailureQ[rejectedMismatch]&&rejectedMismatch[[1]]==="InvalidModel"];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
Quit[If[failures===0,0,1]];
