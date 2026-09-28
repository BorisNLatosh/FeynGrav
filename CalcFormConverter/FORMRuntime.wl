(* ::Package:: *)

(* ::Title:: *)
(*FORM runtime*)

(* ::Text:: *)
(*Loaded inside CalcFormConverter`Private`. Loading defines functions only.
  Executable discovery, file creation and process launching happen on demand.*)

(* ::Section:: *)
(*Options and runtime support*)

(* ::Input::Initialization:: *)
Options[CalcFormConverter`CalcFormCheck] = {CalcFormConverter`FORMExecutable -> Automatic, CalcFormConverter`FORMThreads -> 1, TimeConstraint -> 10};
Options[CalcFormConverter`CalcFormInstall] = {CalcFormConverter`FORMThreads -> 1};
Options[CalcFormConverter`CalcFormCalculate] = {
 Dimension -> Automatic, LoopMomenta -> {}, CalcFormConverter`FORMExecutable -> Automatic,
 TimeConstraint -> Infinity, CalcFormConverter`WorkingDirectory -> Automatic, CalcFormConverter`KeepFiles -> False,
 CalcFormConverter`ShowTiming -> False, CalcFormConverter`ShowProgress -> False, CalcFormConverter`FORMThreads -> 1
};
CalcFormConverter`CalcFormCalculate::files = "FORM job files are retained in `1`.";
CalcFormConverter`CalcFormInstall::wait = "Installation uses the system authentication agent when required. An active package transaction is allowed to finish before an abort takes effect.";

runtimeFailure[tag_, message_, data_: <||>] := Failure[tag, Join[<|"MessageTemplate" -> message|>, data]];
validThreadsQ[n_] := IntegerQ[n] && n >= 1;
formCommand[executable_, source_, threads_] := Join[{executable}, If[threads > 1, {"-w" <> ToString[threads]}, {}], {source}];
validTimeLimitQ[t_] := t === Infinity || (NumberQ[t] && TrueQ[t > 0]);
installationGuidance[threads_: 1] := <|
 "RequestedEngine" -> If[threads > 1, "TFORM", "FORM"], "FORMThreads" -> threads,
 "ProjectURL" -> "https://github.com/form-dev/form",
 "DebianUbuntuCommand" -> "sudo apt-get install form",
 "Instructions" -> If[threads > 1,
   "TFORM is required for multiple workers. On Debian/Ubuntu the form package supplies tform. Use CalcFormInstall[FORMThreads -> " <> ToString[threads] <> "] to install and verify that configuration. Ensure tform is on the kernel's PATH, or supply FORMExecutable explicitly when checking/calculating. On other systems obtain TFORM from the official FORM project.",
   "On Debian/Ubuntu install the form package. On other systems obtain FORM from its official project. Put the executable on the Wolfram kernel's PATH or supply FORMExecutable explicitly."]
|>;

resolveExecutable[requested_] := Module[{name, path, candidates, suffix},
 name = If[requested === Automatic, "form", requested];
 If[!StringQ[name] || name === "", Return[runtimeFailure["InvalidOption", "FORMExecutable must be Automatic or an executable name/path."]]];
 If[StringContainsQ[name, {"/", "\\"}],
   path = ExpandFileName[name];
   Return[If[FileExistsQ[path] && !DirectoryQ[path], path, Missing["NotFound", name]]]];
 path = Environment["PATH"];
 If[!StringQ[path], Return[Missing["NotFound", name]]];
 suffix = If[$OperatingSystem === "Windows" && FileExtension[name] === "", {name <> ".exe", name}, {name}];
 candidates = Flatten[Table[FileNameJoin[{dir, n}],
   {dir, StringSplit[path, If[$OperatingSystem === "Windows", ";", ":"]]}, {n, suffix}]];
 SelectFirst[ExpandFileName /@ candidates, FileExistsQ[#] && !DirectoryQ[#] &, Missing["NotFound", name]]
];

createJobDirectory[parent_, prefix_] := Module[{base, dir},
 base = If[parent === Automatic, $TemporaryDirectory, parent];
 If[!StringQ[base] || !DirectoryQ[base], Return[runtimeFailure["InvalidDirectory", "WorkingDirectory must be an existing directory or Automatic."]]];
 dir = Quiet[Check[CreateDirectory[FileNameJoin[{ExpandFileName[base], prefix <> CreateUUID[]}]], $Failed]];
 If[StringQ[dir], dir, runtimeFailure["WriteFailed", "Cannot create a private FORM job directory."]]
];
removeJobDirectory[dir_String] := Quiet[Check[DeleteDirectory[dir, DeleteContents -> True]; True, False]];

(* ::Section:: *)
(*Process execution with streamed logs and cancellation*)

(* ::Input::Initialization:: *)
(* Own only the process started here. FORM is launched directly, without a shell.
   Keep bounded output tails in memory; complete output goes to the log files. *)
runtimePause[] := Pause[0.02];
$progressInterval = 10;
reportCalculation[event_Association] := If[KeyExistsQ[event, "ElapsedSeconds"],
 Print["FORM ", event["Stage"], ": ", ToString[NumberForm[event["ElapsedSeconds"], {12, 2}], OutputForm], " s elapsed (wall clock)."],
 Print["FORM: ", event["Stage"], "."]];
openRuntimeLog[path_] := Quiet[Check[OpenWrite[path], $Failed]];
inspectRuntimeProcess[process_] := <|
 "ProcessStatus" -> Quiet[ProcessStatus[process]],
 "ExitCode" -> Replace[Quiet[Check[ProcessInformation[process, "ExitCode"], Missing["NotReported"]]], None -> Missing["NotReported"]]|>;

runtimeProcess[command_List, directory_String, limit_, cancellable_: True, progress_: False] := Module[
 {process = None, out = None, err = None, outFile, errFile, tails = <|"StandardOutput" -> "", "StandardError" -> ""|>,
  status = "Completed", exit = Missing["NotExited"], processState = "NotStarted", started = None, elapsed, nextProgress = $progressInterval,
  drain, stop, cleanup, execute, messages = {}, ioFailed = False, inspection},
 outFile = FileNameJoin[{directory, "stdout.log"}]; errFile = FileNameJoin[{directory, "stderr.log"}];
 stop[] := If[MatchQ[process, _ProcessObject] && Quiet[ProcessStatus[process]] === "Running", Quiet[KillProcess[process]]];
 drain[] := Module[{count = 0}, Scan[Function[channel, Module[{chunk, stream},
   stream = If[channel === "StandardOutput", out, err];
   chunk = Quiet[Check[ReadString[ProcessConnection[process, channel], EndOfBuffer], $Failed]];
   If[chunk === $Failed, ioFailed = True];
   If[StringQ[chunk], count += StringLength[chunk];
     If[Quiet[Check[WriteString[stream, chunk]; True, False]] =!= True, ioFailed = True];
     AssociateTo[tails, channel -> StringTake[tails[channel] <> chunk, -Min[65536, StringLength[tails[channel] <> chunk]]]]]
 ]], {"StandardOutput", "StandardError"}]; count];
 cleanup[] := WithCleanup[
   If[MatchQ[process, _ProcessObject],
     If[cancellable, stop[],
       (* Never kill a package transaction, including on a nonlocal exit. *)
       While[ProcessStatus[process] === "Running", drain[]; Pause[0.02]]];
     While[drain[] > 0, Null];
     If[processState =!= "Finished",
       processState = Quiet[ProcessStatus[process]];
       exit = Replace[Quiet[Check[ProcessInformation[process, "ExitCode"], Missing["NotReported"]]], None -> Missing["NotReported"]]]],
   Scan[If[MatchQ[#, _OutputStream], Quiet[Close[#]]] &, {out, err}]];
 execute[] := If[!MatchQ[{out, err}, {_OutputStream, _OutputStream}], status = "LogFailed",
   AbortProtect[
     process = Block[{$MessageList = {}}, With[{p = Quiet[Check[StartProcess[command, ProcessDirectory -> directory], $Failed]]},
       messages = ToString[#, InputForm] & /@ $MessageList; p]]];
   If[!MatchQ[process, _ProcessObject], status = "LaunchFailed",
     started = AbsoluteTime[];
     While[ProcessStatus[process] === "Running",
       drain[];
       If[TrueQ[progress] && AbsoluteTime[] - started >= nextProgress,
         reportCalculation[<|"Stage" -> "Execute", "ElapsedSeconds" -> (AbsoluteTime[] - started)|>];
         nextProgress = AbsoluteTime[] - started + $progressInterval];
       If[ioFailed && cancellable, status = "LogFailed"; Break[]];
       If[limit =!= Infinity && AbsoluteTime[] - started >= limit, status = "TimedOut"; Break[]];
       runtimePause[]];
     inspection = inspectRuntimeProcess[process];
     processState = inspection["ProcessStatus"]; exit = inspection["ExitCode"]]];
 (* Acquisition is protected and registered before the interruptible body.
    Cleanup runs on normal return, Abort, Throw and constrained evaluation. *)
 CheckAbort[
   WithCleanup[
     out = openRuntimeLog[outFile]; err = openRuntimeLog[errFile],
     If[cancellable, execute[], AbortProtect[execute[]]],
     cleanup[]],
   status = "Aborted"];
 If[ioFailed && status === "Completed", status = "LogFailed"];
 elapsed = If[NumberQ[started], AbsoluteTime[] - started, Missing["NotStarted"]];
 Join[<|"Status" -> status, "ElapsedSeconds" -> elapsed, "ExitCode" -> exit, "ProcessStatus" -> processState, "Executable" -> First[command],
   "StandardOutputFile" -> outFile, "StandardErrorFile" -> errFile, "Messages" -> messages|>, tails]
];

(* ::Section:: *)
(*Availability probe*)

(* ::Input::Initialization:: *)
formVersion[text_String] := Module[{matches},
 matches = StringCases[text, RegularExpression["(?im)\\b(?:T?FORM)\\s+(?:version\\s+)?([0-9]+(?:\\.[0-9]+)*(?:[-._A-Za-z0-9]*)?)"] -> "$1"];
 If[matches === {}, Missing["NotReported"], First[matches]]
];
checkStatus[status_, executable_, details_: <||>, threads_: 1] := Join[
 <|"Available" -> (status === "Available"), "Status" -> status, "Executable" -> executable,
   "Version" -> Missing["NotReported"], "RequestedEngine" -> If[threads > 1, "TFORM", "FORM"],
   "FORMThreads" -> threads, "InstallationGuidance" -> installationGuidance[threads]|>, details];

runFORMProbe[executable_String, directory_String, limit_, threads_: 1] := Module[{source, probe, status},
 source = FileNameJoin[{directory, "probe.frm"}];
 If[Quiet[Check[Export[source, "Symbols x;\nLocal cfcProbe=1+1;\n.sort\n#create <probe.out>\n#write <probe.out> \"%E\",cfcProbe\n#close <probe.out>\n.end\n", "Text"]; True, False]] =!= True,
   Return[checkStatus["ProbeFailed", executable, <|"Diagnostic" -> "Cannot write the probe program."|>, threads]]];
 probe = runtimeProcess[formCommand[executable, source, threads], directory, limit];
 status = probe["Status"];
 If[status === "Completed",
   status = If[probe["ExitCode"] === 0 && FileExistsQ[FileNameJoin[{directory, "probe.out"}]] &&
     StringTrim[Import[FileNameJoin[{directory, "probe.out"}], "Text"]] === "2", "Available", "ProbeFailed"]];
 If[status === "Available" && threads > 1 &&
    !StringContainsQ[Lookup[probe, "StandardOutput", ""], RegularExpression["(?m)^TFORM\\s"]],
   status = "ThreadingUnavailable"];
 checkStatus[status, executable, <|"FORMThreads" -> threads, "Version" -> formVersion[Lookup[probe, "StandardOutput", ""]],
   "ExitCode" -> probe["ExitCode"], "StandardOutput" -> Lookup[probe, "StandardOutput", ""],
   "StandardError" -> Lookup[probe, "StandardError", ""], "Messages" -> Lookup[probe, "Messages", {}]|>, threads]
];

CalcFormConverter`CalcFormCheck[OptionsPattern[]] := Module[
 {limit = OptionValue[TimeConstraint], threads = OptionValue[CalcFormConverter`FORMThreads], executable, directory = None},
 If[!validTimeLimitQ[limit], Return[runtimeFailure["InvalidOption", "TimeConstraint must be positive or Infinity."]]];
 If[!validThreadsQ[threads], Return[runtimeFailure["InvalidOption", "FORMThreads must be a positive integer."]]];
 executable = resolveExecutable[Replace[OptionValue[CalcFormConverter`FORMExecutable], Automatic :> If[threads > 1, "tform", "form"]]];
 If[FailureQ[executable], Return[executable]];
 If[MissingQ[executable], Return[checkStatus["NotFound", executable, <||>, threads]]];
 CheckAbort[
   WithCleanup[
     directory = createJobDirectory[Automatic, "calcform-probe-"],
     If[FailureQ[directory], directory, If[threads === 1, runFORMProbe[executable, directory, limit], runFORMProbe[executable, directory, limit, threads]]],
     If[StringQ[directory], removeJobDirectory[directory]]],
   checkStatus["Aborted", executable, <||>, threads]]
];
CalcFormConverter`CalcFormCheck[___] := runtimeFailure["InvalidArguments", "Use CalcFormCheck[options]."];

(* ::Section:: *)
(*Complete calculation*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCalculate[expression_, OptionsPattern[]] := Module[
 {limit = OptionValue[TimeConstraint], keep = OptionValue[CalcFormConverter`KeepFiles], check, directory,
  job, run, result, stage = "Check", retainedFailure,
  timing = OptionValue[CalcFormConverter`ShowTiming], progress = OptionValue[CalcFormConverter`ShowProgress],
  threads = OptionValue[CalcFormConverter`FORMThreads], announce},
 If[!validTimeLimitQ[limit] || !BooleanQ[keep], Return[runtimeFailure["InvalidOption", "Use a positive TimeConstraint or Infinity, and KeepFiles -> True or False."]]];
 If[!BooleanQ[timing] || !BooleanQ[progress] || !validThreadsQ[threads],
   Return[runtimeFailure["InvalidOption", "ShowTiming and ShowProgress must be True or False; FORMThreads must be a positive integer."]]];
 announce[name_] := (stage = name; If[progress, reportCalculation[<|"Stage" -> name|>]]);
 announce["Check"];
 check = CalcFormConverter`CalcFormCheck[CalcFormConverter`FORMExecutable -> OptionValue[CalcFormConverter`FORMExecutable], CalcFormConverter`FORMThreads -> threads];
 If[FailureQ[check], announce["Failed"]; Return[check]];
 If[!TrueQ[check["Available"]], If[progress, reportCalculation[<|"Stage" -> "Failed"|>]]; Return[runtimeFailure["FORMUnavailable", "FORM did not pass its availability check.", <|"Stage" -> stage, "Check" -> check|>]]];
 directory = createJobDirectory[OptionValue[CalcFormConverter`WorkingDirectory], "calcform-job-"];
 If[FailureQ[directory], announce["Failed"]; Return[directory]];
 retainedFailure[tag_, message_, details_: <||>] := runtimeFailure[tag, message,
   Join[<|"Stage" -> stage, "JobDirectory" -> directory, "Executable" -> check["Executable"]|>, details]];
 result = CheckAbort[
   Catch[
     announce["Export"];
     job = CalcFormConverter`CalcFormExport[expression, FileNameJoin[{directory, "job.frm"}],
       Dimension -> OptionValue[Dimension], LoopMomenta -> OptionValue[LoopMomenta]];
     If[FailureQ[job], Throw[retainedFailure["ExportFailed", "FORM export failed; job files were retained.", <|"Cause" -> job|>], $failureTag]];
     announce["Execute"];
     run = runtimeProcess[formCommand[check["Executable"], job["InputFile"], threads], directory, limit, True, progress];
     If[timing && NumberQ[run["ElapsedSeconds"]], reportCalculation[<|"Stage" -> "execution finished (" <> run["Status"] <> ")", "ElapsedSeconds" -> run["ElapsedSeconds"]|>]];
     If[run["Status"] =!= "Completed" || run["ExitCode"] =!= 0,
       Throw[retainedFailure[If[run["Status"] === "Completed", "FORMFailed", run["Status"]],
         "FORM execution did not complete successfully; job files were retained.", <|"Process" -> run|>], $failureTag]];
     announce["Import"];
     If[!FileExistsQ[job["ResultFile"]], Throw[retainedFailure["MissingResult", "FORM did not produce its result file.", <|"Process" -> run|>], $failureTag]];
     result = CalcFormConverter`CalcFormImport[job["ResultFile"], job["MappingFile"]];
     If[FailureQ[result], retainedFailure["ImportFailed", "The FORM result could not be imported; job files were retained.", <|"Cause" -> result, "Process" -> run|>], result],
     $failureTag],
   retainedFailure["Aborted", "Calculation aborted; job files were retained."]];
 If[progress, reportCalculation[<|"Stage" -> If[FailureQ[result], "Failed", "Complete"]|>]];
 If[!FailureQ[result], AbortProtect[
   If[keep || !removeJobDirectory[directory], Message[CalcFormConverter`CalcFormCalculate::files, directory]]]];
 result
];
CalcFormConverter`CalcFormCalculate[___] := runtimeFailure["InvalidArguments", "Use CalcFormCalculate[expression, options]."];

(* ::Section:: *)
(*Explicit Debian/Ubuntu installation*)

(* ::Input::Initialization:: *)
(* Platform discovery and command selection are separate from execution so tests
   can cover installer decisions without installing anything. *)
installationPlatform[] := Module[{linux, text, values, ids, id, root = False, uid},
 linux = $OperatingSystem === "Unix" && StringStartsQ[$SystemID, "Linux"];
 text = If[linux, Quiet[Check[Import["/etc/os-release", "Text"], ""]], ""];
 values = StringCases[text, RegularExpression["(?m)^(?:ID|ID_LIKE)=(.*)$"] -> "$1"];
 ids = StringSplit[StringReplace[StringRiffle[values, " "], {"\"" -> "", "'" -> ""}]];
 id = resolveExecutable["id"];
 If[StringQ[id], uid = Quiet[Check[RunProcess[{id, "-u"}], $Failed]];
   root = AssociationQ[uid] && Lookup[uid, "ExitCode", -1] === 0 && StringTrim[Lookup[uid, "StandardOutput", ""]] === "0"];
 <|"Linux" -> linux, "DebianLike" -> AnyTrue[ids, MemberQ[{"debian", "ubuntu"}, #] &],
   "Root" -> root, "APT" -> resolveExecutable["apt-get"], "Pkexec" -> resolveExecutable["pkexec"]|>
];
installationCommand[platform_Association] := Module[{command},
 If[!TrueQ[platform["Linux"]] || !TrueQ[platform["DebianLike"]] || !StringQ[platform["APT"]],
   Return[runtimeFailure["UnsupportedInstallation", "Automatic installation is supported on Debian/Ubuntu Linux with apt-get.", installationGuidance[]]]];
 command = {platform["APT"], "--no-remove", "-y", "install", "form"};
 If[TrueQ[platform["Root"]], Return[command]];
 If[!StringQ[platform["Pkexec"]], Return[runtimeFailure["AuthorizationUnavailable", "No system authentication helper is available. Install FORM manually.", installationGuidance[]]]];
 Join[{platform["Pkexec"], "--disable-internal-agent"}, command]
];
(* Deliberately no timeout and no forced cancellation of a package transaction. *)
runInstallation[command_List, directory_String] := AbortProtect[Module[{result},
 result = runtimeProcess[command, directory, Infinity, False];
 If[AssociationQ[result] && result["Status"] === "Aborted", Abort[]];
 result
]];

CalcFormConverter`CalcFormInstall[OptionsPattern[]] := Module[
 {threads = OptionValue[CalcFormConverter`FORMThreads], check, command, directory, run, result},
 If[!validThreadsQ[threads], Return[runtimeFailure["InvalidOption", "FORMThreads must be a positive integer."]]];
 check = CalcFormConverter`CalcFormCheck[CalcFormConverter`FORMThreads -> threads];
 If[FailureQ[check], Return[check]];
 If[TrueQ[check["Available"]], Return[check]];
 If[check["Status"] =!= "NotFound", Return[runtimeFailure["FORMUnusable", "The requested FORM/TFORM executable was found but could not be used. Resolve the launch or probe failure before attempting installation.", <|"Check" -> check|>]]];
 command = installationCommand[installationPlatform[]];
 If[FailureQ[command], Return[Failure[command[[1]], Join[command[[2]], installationGuidance[threads]]]]];
 directory = createJobDirectory[Automatic, "calcform-install-"];
 If[FailureQ[directory], Return[directory]];
 Message[CalcFormConverter`CalcFormInstall::wait];
 result = CheckAbort[
   run = runInstallation[command, directory];
   If[run["Status"] =!= "Completed" || run["ExitCode"] =!= 0,
     runtimeFailure["InstallationFailed", "FORM installation or system authorization failed. Consult the retained logs or install manually.",
       Join[<|"JobDirectory" -> directory, "Process" -> run|>, installationGuidance[threads]]],
     check = CalcFormConverter`CalcFormCheck[CalcFormConverter`FORMThreads -> threads];
     If[AssociationQ[check] && TrueQ[check["Available"]], removeJobDirectory[directory]; check,
       runtimeFailure["InstallationVerificationFailed", "The installation command finished, but the requested FORM/TFORM configuration did not pass its availability check.",
         Join[<|"JobDirectory" -> directory, "Check" -> check, "Process" -> run|>, installationGuidance[threads]]]]],
   runtimeFailure["InstallationInterrupted", "Abort was deferred until the installation process finished. Run CalcFormCheck with the same FORMThreads setting to inspect its result.",
     Join[<|"JobDirectory" -> directory|>, installationGuidance[threads]]]];
 result
];
CalcFormConverter`CalcFormInstall[___] := runtimeFailure["InvalidArguments", "Use CalcFormInstall[FORMThreads -> n] or CalcFormInstall[]."];
