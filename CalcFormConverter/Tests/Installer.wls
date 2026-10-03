Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* Everything that might launch an installer is replaced in this suite. *)
platform=<|"Linux"->True,"DebianLike"->True,"Root"->False,"APT"->"/usr/bin/apt-get","Pkexec"->"/usr/bin/pkexec"|>;
command=CalcFormConverter`Private`installationCommand[platform];
assert["system authentication command",command==={"/usr/bin/pkexec","--disable-internal-agent","/usr/bin/apt-get","--no-remove","-y","install","form"}];
assert["root command",CalcFormConverter`Private`installationCommand[Join[platform,<|"Root"->True|>]]===Drop[command,2]];
assertFailure["unsupported platform","UnsupportedInstallation",CalcFormConverter`Private`installationCommand[Join[platform,<|"Linux"->False|>]]];
assertFailure["unsupported distribution","UnsupportedInstallation",CalcFormConverter`Private`installationCommand[Join[platform,<|"DebianLike"->False|>]]];
assertFailure["missing apt","UnsupportedInstallation",CalcFormConverter`Private`installationCommand[Join[platform,<|"APT"->Missing[]|>]]];
assertFailure["missing authentication","AuthorizationUnavailable",CalcFormConverter`Private`installationCommand[Join[platform,<|"Pkexec"->Missing[]|>]]];
result=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`runInstallation},
 CalcFormConverter`CalcFormCheck[___]:=<|"Available"->True,"Status"->"Available"|>;
 CalcFormConverter`Private`runInstallation[___]:=(assert["installer must not run",False];$Failed);
 CalcFormInstall[]];
assert["already installed no-op",TrueQ[result["Available"]]];
result=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`runInstallation},
 CalcFormConverter`CalcFormCheck[___]:=<|"Available"->False,"Status"->"LaunchFailed"|>;
 CalcFormConverter`Private`runInstallation[___]:=(assert["must not reinstall unusable FORM",False];$Failed);
 CalcFormInstall[]];
assertFailure["unusable does not reinstall","FORMUnusable",result];
KeyValueMap[Function[{scenario,expectedTag},Module[{calls=0,ran=False,result},
 result=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`installationPlatform,CalcFormConverter`Private`runInstallation},
  CalcFormConverter`CalcFormCheck[___]:=(calls++;If[calls===1,<|"Available"->False,"Status"->"NotFound"|>,<|"Available"->(scenario==="success"),"Status"->If[scenario==="success","Available","ProbeFailed"]|>]);
  CalcFormConverter`Private`installationPlatform[]:=platform;
  CalcFormConverter`Private`runInstallation[cmd_,dir_]:=(ran=True;assert[scenario<>" correct command",cmd===command];<|"Status"->"Completed","ExitCode"->If[scenario==="denied",126,0]|>);
  Quiet[CalcFormInstall[],CalcFormInstall::wait]];
 assert[scenario<>" installer called",ran];
 If[scenario==="success",assert["post-install verification",TrueQ[result["Available"]] && calls===2],
 assertFailure[scenario,expectedTag,result];
 If[FailureQ[result] && KeyExistsQ[result[[2]],"JobDirectory"],DeleteDirectory[result[[2]]["JobDirectory"],DeleteContents->True]]]
]],<|"success"->"","denied"->"InstallationFailed","verification"->"InstallationVerificationFailed"|>];
(* Model ordinary FORM as working in every case. Only TFORM changes state,
   so dropping FORMThreads on either check must fail these tests. *)
Do[Module[{seen={},ran=0,threadChecks=0,result,installedCommand=None},
 result=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`installationPlatform,CalcFormConverter`Private`runInstallation},
  CalcFormConverter`CalcFormCheck[opts___]:=Module[{n=Lookup[Association[{opts}],FORMThreads,1],status},
    AppendTo[seen,n];
    If[n===1,Return[<|"Available"->True,"Status"->"Available","FORMThreads"->1|>]];
    threadChecks++;
    status=Switch[scenario,"available","Available","unusable","LaunchFailed",_,
      If[threadChecks===1,"NotFound",If[scenario==="installed","Available","ProbeFailed"]]];
    <|"Available"->(status==="Available"),"Status"->status,"FORMThreads"->n|>];
  CalcFormConverter`Private`installationPlatform[]:=platform;
  CalcFormConverter`Private`runInstallation[cmd_,dir_]:=(ran++;installedCommand=cmd;<|"Status"->"Completed","ExitCode"->0|>);
  Quiet[CalcFormInstall[FORMThreads->4],CalcFormInstall::wait]];
 assert[scenario<>" requested workers forwarded",seen===If[MemberQ[{"installed","verification"},scenario],{4,4},{4}]];
 assert[scenario<>" expected installation attempts",ran===If[MemberQ[{"installed","verification"},scenario],1,0]];
 Switch[scenario,
  "available"|"installed",assert[scenario<>" TFORM verified",AssociationQ[result] && TrueQ[result["Available"]] && result["FORMThreads"]===4],
  "unusable",assertFailure["unusable TFORM not reinstalled","FORMUnusable",result],
  "verification",assertFailure["ordinary FORM cannot satisfy threaded verification","InstallationVerificationFailed",result]];
 If[ran===1,assert[scenario<>" same distribution package",installedCommand===command]];
 If[FailureQ[result] && KeyExistsQ[result[[2]],"JobDirectory"],DeleteDirectory[result[[2]]["JobDirectory"],DeleteContents->True]]
],{scenario,{"available","unusable","installed","verification"}}];
Block[{CalcFormConverter`CalcFormCheck},
 CalcFormConverter`CalcFormCheck[___]:=(assert["invalid option must not probe",False];$Failed);
 assertFailure["invalid installer worker count","InvalidOption",CalcFormInstall[FORMThreads->0]];
 assertFailure["fractional installer worker count","InvalidOption",CalcFormInstall[FORMThreads->2.5]]];
missingTFORM=Block[{CalcFormConverter`Private`resolveExecutable},
 CalcFormConverter`Private`resolveExecutable[name_]:=If[name==="tform",Missing["NotFound",name],"/example/form"];
 CalcFormCheck[FORMThreads->4]];
assert["missing TFORM diagnostics",missingTFORM["Status"]==="NotFound" && missingTFORM["RequestedEngine"]==="TFORM" && missingTFORM["FORMThreads"]===4];
assert["missing TFORM guidance",StringContainsQ[missingTFORM["InstallationGuidance"]["Instructions"],"CalcFormInstall[FORMThreads -> 4]"]];
abortedTFORM=Block[{CalcFormConverter`Private`resolveExecutable,CalcFormConverter`Private`runFORMProbe},
 CalcFormConverter`Private`resolveExecutable[_]:="/example/tform";
 CalcFormConverter`Private`runFORMProbe[___]:=Abort[];
 CalcFormCheck[FORMThreads->4]];
assert["aborted TFORM diagnostics",abortedTFORM["Status"]==="Aborted" && abortedTFORM["RequestedEngine"]==="TFORM" && abortedTFORM["FORMThreads"]===4];
unsupportedTFORM=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`installationPlatform},
 CalcFormConverter`CalcFormCheck[___]:=<|"Available"->False,"Status"->"NotFound"|>;
 CalcFormConverter`Private`installationPlatform[]:=Join[platform,<|"Linux"->False|>];
 CalcFormInstall[FORMThreads->4]];
assertFailure["unsupported TFORM install platform","UnsupportedInstallation",unsupportedTFORM];
assert["unsupported platform retains TFORM guidance",unsupportedTFORM[[2]]["RequestedEngine"]==="TFORM" && unsupportedTFORM[[2]]["FORMThreads"]===4];


(* Automatic selection: discovery and probing are mocked, never installation. *)
Do[
 selected=Block[{CalcFormConverter`Private`resolveExecutable,CalcFormConverter`Private`automaticWorkerCount},
  CalcFormConverter`Private`automaticWorkerCount[]:=4;
  CalcFormConverter`Private`resolveExecutable[name_]:=If[MemberQ[available,name],"/mock/"<>name,Missing["NotFound",name]];
  CalcFormConverter`Private`selectFORMConfiguration[Automatic,Automatic]];
 assert["automatic discovery "<>ToString[available],selected["FORMThreads"]===If[MemberQ[available,"tform"],4,1] && selected["RequestedFORMThreads"]===Automatic];
 assert["automatic missing state "<>ToString[available],MissingQ[selected["Executable"]]===(available==={})],
 {available,{{"form","tform"},{"form"},{"tform"},{}}}];
Scan[Function[pair,assert["processor cap "<>ToString[First[pair]],Block[{$ProcessorCount=First[pair]},CalcFormConverter`Private`automaticWorkerCount[]]===Last[pair]]],{{1,1},{4,4},{16,8},{0,1},{Missing["Unknown"],1}}];
Block[{CalcFormConverter`Private`resolveExecutable},
 CalcFormConverter`Private`resolveExecutable[x_]:=x;
 assert["explicit path automatic serial",CalcFormConverter`Private`selectFORMConfiguration["/space path/form",Automatic]["FORMThreads"]===1];
 assert["explicit workers uncapped",CalcFormConverter`Private`selectFORMConfiguration[Automatic,32]["FORMThreads"]===32]];
Do[
 searches={};
 result=Block[{CalcFormConverter`Private`resolveExecutable,CalcFormConverter`Private`automaticWorkerCount,CalcFormConverter`Private`runFORMProbe},
  CalcFormConverter`Private`automaticWorkerCount[]:=8;
  CalcFormConverter`Private`resolveExecutable[name_]:=(AppendTo[searches,name];"/mock/"<>name);
  CalcFormConverter`Private`runFORMProbe[e_,_,_,n_:1]:=CalcFormConverter`Private`checkStatus[status,e,<||>,n];
  CalcFormCheck[]];
 assert["no fallback after "<>status,result["Status"]===status && searches==={"tform"} && result["FORMThreads"]===8],
 {status,{"LaunchFailed","ProbeFailed","TimedOut","Aborted","ThreadingUnavailable"}}];
result=Block[{CalcFormConverter`Private`resolveExecutable,CalcFormConverter`Private`automaticWorkerCount,CalcFormConverter`Private`runFORMProbe,CalcFormConverter`Private`runInstallation},
 CalcFormConverter`Private`automaticWorkerCount[]:=8;
 CalcFormConverter`Private`resolveExecutable[name_]:=If[name==="tform",Missing["NotFound",name],"/mock/form"];
 CalcFormConverter`Private`runFORMProbe[e_,_,_,n_:1]:=CalcFormConverter`Private`checkStatus["Available",e,<||>,n];
 CalcFormConverter`Private`runInstallation[___]:=(assert["serial fallback must not install",False];$Failed);
 CalcFormInstall[]];
assert["installer accepts serial fallback",result["Available"] && result["FORMThreads"]===1 && result["SelectionReason"]==="TFORMNotFound"];

(* Calculation must launch the resolved configuration, never Automatic. *)
Do[
 launched={};
 result=Block[{CalcFormConverter`Private`resolveExecutable,CalcFormConverter`Private`automaticWorkerCount,CalcFormConverter`Private`runFORMProbe,CalcFormConverter`Private`runtimeProcess},
  CalcFormConverter`Private`automaticWorkerCount[]:=8;
  CalcFormConverter`Private`resolveExecutable[name_]:=If[!hasTFORM && name==="tform",Missing["NotFound",name],"/mock/"<>name];
  CalcFormConverter`Private`runFORMProbe[e_,_,_,n_:1]:=CalcFormConverter`Private`checkStatus["Available",e,<||>,n];
  CalcFormConverter`Private`runtimeProcess[cmd_,___]:=(launched=cmd;<|"Status"->"Completed","ExitCode"->1|>);
  CalcFormCalculate[1,WorkingDirectory->testDirectory]];
 assert["resolved calculation arguments "<>ToString[hasTFORM],If[hasTFORM,Take[launched,2]==={"/mock/tform","-w8"},First[launched]==="/mock/form" && Length[launched]===2]];
 If[FailureQ[result],DeleteDirectory[result[[2]]["JobDirectory"],DeleteContents->True]],
 {hasTFORM,{True,False}}];

finishedTransaction=False;
deferred=CheckAbort[Block[{CalcFormConverter`Private`runtimeProcess},
 CalcFormConverter`Private`runtimeProcess[_,_,Infinity,False]:=(Abort[];finishedTransaction=True;"finished");
 CalcFormConverter`Private`runInstallation[{"mock-installer"},testDirectory]],"deferred-abort"];
assert["installer defers abort and disables cancellation",finishedTransaction && deferred==="deferred-abort"];
finish["Installer mocks"];
