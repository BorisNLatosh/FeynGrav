(* Shared setup for small direct FeynArts checks. No integration wrapper is loaded. *)
$HistoryLength=0;
root=DirectoryName[$InputFileName,4];
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
cgroup="/sys/fs/cgroup"<>cpath;
If[!StringStartsQ[cpath,"/"] ||
 StringTrim[Import[cgroup<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import[cgroup<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import[cgroup<>"/memory.oom.group","Text"]]=!="1",
 Print["REQUIRED_AGGREGATE_CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
Print["CHECKOUT=",root," UTC=",DateString[Now,TimeZone->0]];
dir=Environment["FGFA_MODEL_DIR"];
(* The model moved into the package; fall back to the packaged copy when the
   environment variable is not set. Preserve the hash check in both cases. *)
If[!StringQ[dir]||!DirectoryQ[dir],dir=FileNameJoin[{root,"FeynArts","Models"}]];
Print["MODEL_DIR=",dir];
If[!StringQ[dir]||!DirectoryQ[dir],Print["SET_FGFA_MODEL_DIR"];Quit[11]];
manifest=Import[FileNameJoin[{dir,"manifest.json"}],"RawJSON"];
name=manifest["ModelName"];
files=FileNameJoin[{dir,name<>#}]&/@{".gen",".mod"};
If[(FileHash[#,"SHA256","HexString"]&/@files)=!=
 Lookup[manifest["ModelFileSHA256"],{"Generic","Classes"}],
 Print["MODEL_HASH_MISMATCH"];Quit[12]];
Print["MODEL=",name," MODEL_HASHES_VERIFIED=True"];
$LoadFeynArts=True;
Get[FileNameJoin[{root,"FeynGrav.wl"}]];
FeynGrav`importGravitons[2];
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[TimeConstrained[FeynArts`InitializeModel[name,FeynArts`GenericModel->name],70,$TimedOut]=!=True,
 Print["MODEL_INITIALISATION_FAILED"];Quit[13]];
failures=0;checks=0;
assert[label_,condition_]:=(checks++;Print[If[TrueQ[condition],"PASS ","FAIL "],label];
 If[!TrueQ[condition],failures++]);
same[a_,b_,seconds_:15]:=TrueQ[TimeConstrained[
 Expand[FeynCalc`ExpandScalarProduct[FeynCalc`MomentumExpand[a-b]]],seconds,$TimedOut]===0];
finish[]:=(
 assert["no convenience integration package loaded",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
 Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
 Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
 Print["COMPLETED checks=",checks," failures=",failures];Quit[If[failures===0,0,1]]);
