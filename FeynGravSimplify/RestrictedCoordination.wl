(* ::Package:: *)
(* Shared restricted job coordination: job-directory lifecycle, backend
   preparation dispatch, native process execution and result decoding. Uses the
   shared codec (RestrictedCodec.wl), Contractor discovery/preparation
   (Contractor.wl) and restricted FORM helpers (FORM.wl). Defines functions
   only and never launches programs at load time. *)
Begin["FeynGrav`Simplification`Private`"];

$progressIntervalSeconds = 10;
CalcFormConverter`FeynGravSimplify::files = "Simplification job files are retained in `1`.";
CalcFormConverter`FeynGravSimplify::cleanup = "Could not remove simplification job directory `1`.";

(* Restricted FORM benchmark preparation: resolve the executable/worker
   configuration once and build the command and generated program. Value
   validation stays in the shared coordinator. *)
formPreparation[data_, formExe_, formWorkers_] := Module[{config, program},
    config = formConfiguration[formExe, formWorkers];
    If[FailureQ[config], Return[config]];
    program = formProgram[data, config["Workers"]];
    <|"Executable" -> config["Executable"], "Workers" -> config["Workers"],
      "Commands" -> Join[{config["Executable"]},
          If[config["Workers"] > 1, {"-w" <> ToString[config["Workers"]]}, {}],
          {"-q", "input.frm"}],
      "Program" -> program["Program"], "OpaqueTrees" -> program["OpaqueTrees"]|>
];

(* Lifecycle and ownership: runJob owns one ProcessObject and the stdin/stdout/
   stderr/log streams it opens. The process is registered before user
   interruption; timeout/abort cleanup stops only that process, drains its pipes
   and closes every stream. The caller owns the job directory: failures and
   KeepFiles jobs are retained; otherwise simplify removes it after decoding.
   Native Wolfram process I/O only. WriteString can defer interruption while
   blocked in a pipe write; TimeConstraint is therefore best-effort at that
   boundary. Once control returns, timeout/abort cleanup owns only this process. *)
runJob[command_List, directory_String, limit_, backend_: "Contractor", sendInput_: True] := Module[
    {process = None, stdin = None, stdout = None, stderr = None,
     out = None, err = None, status = "Completed", phase = "Launch", exit = Missing["NotExited"],
     started = AbsoluteTime[], elapsed, result, diagnostic = "", messages = {}, input,
     drain, stop, cleanup, execute, record, ioFailed = False,
     processStarted, nextProgress = $progressIntervalSeconds, processElapsed},
    drain[] := Module[{count = 0}, Scan[Function[channel, Module[{chunk, stream, sink},
        {stream, sink} = If[channel === "StandardOutput", {stdout, out}, {stderr, err}];
        If[MatchQ[stream, _InputStream] && MatchQ[sink, _OutputStream],
            chunk = Quiet[Check[ReadString[stream, EndOfBuffer], $Failed]];
            If[chunk === $Failed, ioFailed = True];
            If[StringQ[chunk],
                count += StringLength[chunk];
                If[Quiet[Check[WriteString[sink, chunk]; True, False]] =!= True, ioFailed = True];
                If[channel === "StandardError",
                    diagnostic = StringTake[diagnostic <> chunk, -Min[65536, StringLength[diagnostic] + StringLength[chunk]]]]
            ]
        ]
    ]], {"StandardOutput", "StandardError"}]; count];
    stop[] := If[MatchQ[process, _ProcessObject] && Quiet[ProcessStatus[process]] === "Running",
        Quiet[KillProcess[process]]];
    cleanup[] := (
        If[MatchQ[process, _ProcessObject],
            stop[];
            While[drain[] > 0, Null];
            exit = Replace[Quiet[Check[ProcessInformation[process, "ExitCode"], Missing["NotReported"]]], None -> Missing["NotReported"]]
        ];
        Scan[If[MatchQ[#, _InputStream | _OutputStream], Quiet[Close[#]]] &,
            {stdin, stdout, stderr, out, err}];
    );
    execute[] := (
        If[!MatchQ[{out, err}, {_OutputStream, _OutputStream}], status = "LogFailed"; Return[$Failed]];
        (* Register the process before enabling user interruption. *)
        AbortProtect[
            process = Block[{$Messages = {err}, $MessageList = {}},
                With[{p = Quiet[Check[StartProcess[command, ProcessDirectory -> directory], $Failed], None, All]},
                    messages = ToString[#, InputForm] & /@ $MessageList; p]]
        ];
        If[!MatchQ[process, _ProcessObject], status = "LaunchFailed"; Return[$Failed]];
        processStarted = AbsoluteTime[];
        stdin = ProcessConnection[process, "StandardInput"];
        stdout = ProcessConnection[process, "StandardOutput"];
        stderr = ProcessConnection[process, "StandardError"];
        If[sendInput,
            phase = "Write";
            input = Quiet[Check[Import[FileNameJoin[{directory, "input.json"}], "Text", CharacterEncoding -> "UTF-8"], $Failed]];
            If[!StringQ[input], status = "InputFailed"; Return[$Failed]];
            If[Quiet[Check[SetOptions[stdin, CharacterEncoding -> "UTF-8"]; WriteString[stdin, input <> "\n"]; Close[stdin]; True, False]] =!= True,
                status = "WriteFailed"; Return[$Failed]],
            Close[stdin]
        ];
        stdin = None; input = Null;
        phase = "Wait";
        While[ProcessStatus[process] === "Running",
            drain[];
            If[ioFailed, status = "LogFailed"; Return[$Failed]];
            If[TrueQ[$showProgress],
                processElapsed = AbsoluteTime[] - processStarted;
                If[processElapsed >= nextProgress,
                    Print[backend, " Execute: ",
                        ToString[NumberForm[processElapsed, {12, 2}], OutputForm],
                        " s elapsed (wall clock)."];
                    nextProgress = processElapsed + $progressIntervalSeconds
                ]
            ];
            Pause[0.02]
        ];
        phase = "Read";
        drain[];
        exit = Quiet[Check[ProcessInformation[process, "ExitCode"], Missing["NotReported"]]];
        If[ioFailed, status = "LogFailed", If[exit =!= 0, status = "ProcessFailed"]];
        Null
    );
    WithCleanup[
        out = Quiet[Check[OpenWrite[FileNameJoin[{directory, If[backend =!= "Contractor", "stdout.log", "stdout.json"]}], CharacterEncoding -> "UTF-8"], $Failed]];
        err = Quiet[Check[OpenWrite[FileNameJoin[{directory, "stderr.log"}], CharacterEncoding -> "UTF-8"], $Failed]],
        result = CheckAbort[
            If[limit === Infinity, execute[], TimeConstrained[execute[], limit, status = "TimedOut"; $Failed]],
            status = "Aborted"; $Aborted],
        cleanup[]
    ];
    If[status === "Completed" && ioFailed, status = "LogFailed"];
    elapsed = AbsoluteTime[] - started;
    record = <|"Method" -> backend, "Status" -> status, "Phase" -> phase, "Executable" -> First[command],
        "Command" -> command, "ExitCode" -> If[IntegerQ[exit], exit, Null],
        "ElapsedSeconds" -> elapsed, "Messages" -> messages,
        "ExecutableSHA256" -> Quiet[Check[FileHash[First[command], "SHA256", "HexString"], "Unavailable"]]|>;
    Quiet[Check[Export[FileNameJoin[{directory, "process.json"}], record, "RawJSON"], Null]];
    If[status === "Aborted", Abort[]];
    If[status =!= "Completed", Return[failure[backend <> status,
        backend <> " execution did not complete successfully.",
        Join[record, <|"StandardError" -> diagnostic, "JobDirectory" -> directory|>]]]];
    record
];

(* Private restricted coordinator used by Facade.wl. Resolved ordinary option
   rules arrive from the public OptionsPattern entry point; only "Contractor" and
   "FORMBenchmark" reach here. The coordinator validates shared runtime values,
   dispatches preparation/decoding to the backend-specific operations, owns the
   job-directory retention/cleanup and calls the shared process runner. It
   returns the reconstructed expression or a Failure. *)

simplify[expression_, opts : OptionsPattern[CalcFormConverter`FeynGravSimplify]] := Module[
    {method, executable, workers, limit, keep, progress, timing, data, json, job = None,
     run, result, stage = "Validate", retained = False, commands, cleanup, outputText,
     formExe, formWorkers, prepared},
    If[!AllTrue[First /@ Flatten[{opts}], MemberQ[First /@ Options[CalcFormConverter`FeynGravSimplify], #] &],
        Return[failure["InvalidOption", "Unknown option for FeynGravSimplify."]]];
    method = OptionValue[Method];
    {limit, keep, progress, timing} = {
        OptionValue[TimeConstraint], OptionValue[CalcFormConverter`KeepFiles],
        OptionValue[CalcFormConverter`ShowProgress], OptionValue[CalcFormConverter`ShowTiming]};
    If[!MemberQ[{"Contractor", "FORMBenchmark"}, method],
        Return[failure["UnsupportedMethod", "Method must be \"Contractor\" or \"FORMBenchmark\"."]]];
    If[!(limit === Infinity || (NumberQ[limit] && TrueQ[limit > 0])) || !BooleanQ[keep] ||
       !BooleanQ[progress] || !BooleanQ[timing],
        Return[failure["InvalidOption", "Use a positive TimeConstraint or Infinity, and Boolean KeepFiles, ShowProgress and ShowTiming."]]];
    (* Only the selected backend's executable/worker pair is read and validated.
       The facade does not resolve or forward the unselected pair, so its delayed
       values stay inactive. Immediate rules retain normal argument evaluation
       before the public call. *)
    If[method === "Contractor",
        executable = OptionValue[FeynGrav`ContractorExecutable];
        workers = OptionValue[FeynGrav`ContractorThreads];
        If[!(workers === Automatic || (IntegerQ[workers] && workers > 0)),
            Return[failure["InvalidOption", "Use positive integer ContractorThreads or Automatic."]]],
        formExe = OptionValue[CalcFormConverter`FORMExecutable];
        formWorkers = OptionValue[CalcFormConverter`FORMThreads];
        If[!(formWorkers === Automatic || (IntegerQ[formWorkers] && formWorkers > 0)) ||
            !(formExe === Automatic || (StringQ[formExe] && formExe =!= "")),
            Return[failure["InvalidOption", "Use positive integer FORMThreads or Automatic, and FORMExecutable -> Automatic or a nonempty executable path/name."]]]
    ];
    reportProgress["Validate and encode"];
    data = encode[expression]; If[FailureQ[data], Return[data]];
    reportProgress["Locate executable"];
    prepared = If[method === "Contractor",
        contractorPreparation[executable, workers],
        formPreparation[data, formExe, formWorkers]];
    If[FailureQ[prepared], Return[prepared]];
    executable = prepared["Executable"];
    commands = prepared["Commands"];
    json = Quiet[Check[ExportString[data["Tree"], "RawJSON", "Compact" -> True], $Failed]];
    If[!StringQ[json], Return[failure["EncodingFailed", "Cannot encode the validated expression as JSON."]]];
    cleanup[] := If[StringQ[job],
        If[retained || keep, Message[CalcFormConverter`FeynGravSimplify::files, job],
            If[Quiet[Check[DeleteDirectory[job, DeleteContents -> True]; True, False]] =!= True,
                Message[CalcFormConverter`FeynGravSimplify::cleanup, job]]]];
    CheckAbort[WithCleanup[Null,
        result = CheckAbort[Catch[
            stage = "Prepare"; reportProgress[stage];
            job = Quiet[Check[CreateDirectory[FileNameJoin[{$TemporaryDirectory, "feyngrav-" <> ToLowerCase[method] <> "-" <> CreateUUID[]}]], $Failed]];
            If[!StringQ[job], reject["JobCreationFailed", "Cannot create the simplification job directory."]];
            If[Quiet[Check[Export[FileNameJoin[{job, "input.json"}], json, "Text", CharacterEncoding -> "UTF-8"]; True, False]] =!= True,
                reject["WriteFailed", "Cannot write the Contractor request."]];
            If[Quiet[Check[Export[FileNameJoin[{job, "mapping.json"}],
                Map[Function[entry, <|"Role" -> entry["Role"], "Value" -> ToString[entry["Value"], InputForm, PageWidth -> Infinity]|>], data["Entries"]],
                "RawJSON"]; True, False]] =!= True, reject["WriteFailed", "Cannot write the diagnostic mapping."]];
            If[method =!= "Contractor",
                If[Quiet[Check[Export[FileNameJoin[{job, "input.frm"}], prepared["Program"], "Text", CharacterEncoding -> "UTF-8"];
                    Export[FileNameJoin[{job, "opaque-trees.json"}], prepared["OpaqueTrees"], "RawJSON"]; True, False]] =!= True,
                    reject["WriteFailed", "Cannot write the restricted FORM benchmark program."]]
            ];
            stage = "Execute"; reportProgress[stage];
            run = runJob[commands, job, limit, method, method === "Contractor"];
            If[timing, Print[method, " process: ",
                Lookup[If[FailureQ[run], run[[2]], run], "ElapsedSeconds", Missing["Unavailable"]],
                " s elapsed (wall clock)."]];
            If[FailureQ[run], Throw[run, $failureTag]];
            stage = "Decode"; reportProgress[stage];
            outputText = Quiet[Check[Import[FileNameJoin[{job, If[method =!= "Contractor", "result.txt", "stdout.json"]}], "Text", CharacterEncoding -> "UTF-8"], $Failed]];
            If[!StringQ[outputText], reject["ReadFailed", "Cannot read the backend result."]];
            result = If[method =!= "Contractor", formDecode[outputText, data, prepared["OpaqueTrees"]], decode[outputText, data]];
            If[FailureQ[result], Throw[result, $failureTag]];
            result,
            $failureTag
        ], retained = True; Abort[]];
        If[FailureQ[result], retained = True;
            result = Failure[result[[1]], Join[result[[2]], <|"Stage" -> stage, "Method" -> method, "Executable" -> executable,
                "JobDirectory" -> job|>, If[AssociationQ[run], <|"ExitCode" -> run["ExitCode"]|>, <||>]]]];
        result,
        cleanup[]
    ], retained = True; Abort[]]
];
(* Reporting surrounds the complete adapter call, including early failures and
   cleanup. These are wall-clock measurements, not CPU time or progress estimates. *)

reportProgress[label_] := If[TrueQ[$showProgress], Print[$backendLabel, ": ", label, "."]];
(* The former FeynGrav`FeynGravSimplify dispatcher that followed here is replaced
   by the unified facade in FeynGravSimplify/Facade.wl (API-01): option resolution,
   capability checking and full/restricted dispatch are one owner's definitions. *)

End[];
