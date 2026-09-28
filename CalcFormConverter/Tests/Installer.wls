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
finishedTransaction=False;
deferred=CheckAbort[Block[{CalcFormConverter`Private`runtimeProcess},
 CalcFormConverter`Private`runtimeProcess[_,_,Infinity,False]:=(Abort[];finishedTransaction=True;"finished");
 CalcFormConverter`Private`runInstallation[{"mock-installer"},testDirectory]],"deferred-abort"];
assert["installer defers abort and disables cancellation",finishedTransaction && deferred==="deferred-abort"];
finish["Installer mocks"];
