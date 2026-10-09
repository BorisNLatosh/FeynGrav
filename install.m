(* FeynGrav installer. Distributed under the repository's GPL-3.0 license. *)
BeginPackage["FeynGravInstaller`"];
InstallFeynGrav::usage = "InstallFeynGrav[] downloads, checks and installs FeynGrav. Restart the kernel afterwards. Existing Git checkouts are never replaced.";
InstallFeynGravTo::usage = "InstallFeynGravTo specifies the absolute destination directory.";
FeynGravReference::usage = "FeynGravReference selects an official GitHub branch, tag or commit; default: main.";
FeynGravArchive::usage = "FeynGravArchive -> Automatic downloads from GitHub; a ZIP filename uses that local archive instead.";
OverwriteFeynGrav::usage = "OverwriteFeynGrav -> Automatic asks before replacing an installation; True consents and False refuses. A complete backup is always retained.";
InstallFeynCalcDependency::usage = "InstallFeynCalcDependency -> Automatic asks before invoking the official FeynCalc installer if needed. True consents; False refuses. Its own prompts remain enabled.";
Options[InstallFeynGrav] = {InstallFeynGravTo -> Automatic, FeynGravReference -> "main", FeynGravArchive -> Automatic,
    OverwriteFeynGrav -> Automatic, InstallFeynCalcDependency -> Automatic};
Begin["`Private`"];

failure[tag_, message_, data_: <||>] := Failure[tag, Join[<|"MessageTemplate" -> message|>, data]];
$failureTag = "FeynGravInstallerFailure";
stop[tag_, message_, data_: <||>] := Throw[failure[tag, message, data], $failureTag];
checked[value_, tag_, message_] := If[value === $Failed || FailureQ[value] || value === $Aborted, stop[tag, message], value];
consent[value_, message_] := Switch[value, True, True, False, False, Automatic,
    If[TrueQ[$Notebooks], TrueQ[ChoiceDialog[message, {"Continue" -> True, "Cancel" -> False}]],
        stop["ConsentRequired", message <> " Run in a notebook or set the corresponding consent option explicitly."]]];
referenceQ[s_] := StringQ[s] && StringLength[s] > 0 &&
    StringMatchQ[s, RegularExpression["[A-Za-z0-9][A-Za-z0-9._/-]*"]] &&
    !StringContainsQ[s, ".." | "//"] && !StringEndsQ[s, "/"];
absolutePathQ[s_] := StringStartsQ[s, "/"] || StringMatchQ[s, RegularExpression["[A-Za-z]:[\\\\/].*"]];
archiveURL[ref_] := "https://github.com/BorisNLatosh/FeynGrav/archive/" <> StringReplace[ref, "/" -> "%2F"] <> ".zip";
gitCheckoutQ[path_] := Module[{p = path, parent, info},
    While[True,
        If[DirectoryQ[p], info = Quiet[FileInformation[p]];
            If[ListQ[info], p = Lookup[Association[info], "AbsoluteFileName", p]]];
        If[FileType[FileNameJoin[{p, ".git"}]] === File || FileExistsQ[FileNameJoin[{p, ".git", "HEAD"}]], Return[True]];
        parent = DirectoryName[p]; If[parent === p || parent === "", Return[False]]; p = parent]];
requireInternet[] := If[TrueQ[$AllowInternet === False],
    stop["InternetDisabled", "Wolfram Internet access is disabled. Enable $AllowInternet in this session, or use a local archive with FeynCalc preinstalled."]];
download[url_, file_] := (requireInternet[]; URLDownload[url, file]);
moveDirectory[from_, to_] := RenameDirectory[from, to];
extract[archive_, to_] := ExtractArchive[archive, to];

(* Validate both ZIP directory and local-header paths before extraction. Reject
   links, special files, encrypted/multidisk/ZIP64 archives and ambiguous names.
   GitHub source archives are ordinary ZIPs. No archive-provided code is run here. *)
safeNameQ[name_] := StringQ[name] && StringLength[name] > 0 &&
    !StringContainsQ[name, "\\" | ":" | "//" | "*" | "?" | "<" | ">" | "|" | "\""] && AllTrue[ToCharacterCode[name], # >= 32 &] && !StringStartsQ[name, "/"] &&
    AllTrue[StringSplit[StringTrim[name, "/"], "/", All],
        # =!= "" && # =!= "." && # =!= ".." && !StringEndsQ[#, " " | "."] &&
        !StringMatchQ[ToUpperCase[First[StringSplit[#, "."]]],
            RegularExpression["CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]"]] &];
validateArchive[file_] := Module[{s, size, tail, positions, e, count, offset, cdSize, h, n, x, c, raw, name,
        mode, local, names = {}, u, extra, fieldsOK, require},
    u[b_] := FromDigits[Reverse[b], 256];
    require[q_] := If[!TrueQ[q], stop["InvalidArchive", "Unsupported or unsafe ZIP archive."]];
    fieldsOK[bytes_] := Module[{i = 1, len, id},
        While[i <= Length[bytes],
            require[i + 3 <= Length[bytes]];
            id = u[bytes[[i ;; i + 1]]]; len = u[bytes[[i + 2 ;; i + 3]]];
            (* Only timestamp/UID metadata. Reject alternate Unicode paths and ZIP64. *)
            require[MemberQ[{10, 21589, 30837}, id] && i + 3 + len <= Length[bytes]];
            i += 4 + len]; True];
    s = checked[OpenRead[file, BinaryFormat -> True], "ArchiveReadFailed", "Cannot read ZIP archive."];
    WithCleanup[Null,
        size = FileByteCount[file]; require[size >= 22];
        SetStreamPosition[s, Max[0, size - 65557]];
        tail = BinaryReadList[s, "Byte"];
        positions = SequencePosition[tail, {80, 75, 5, 6}]; require[positions =!= {}];
        e = First[Last[positions]]; require[e + 21 <= Length[tail]];
        h = Take[tail, {e, e + 21}];
        require[e + 21 + u[h[[21 ;; 22]]] === Length[tail]];
        require[u[h[[5 ;; 8]]] === 0]; count = u[h[[11 ;; 12]]];
        require[0 < count < 65535 && count === u[h[[9 ;; 10]]]];
        cdSize = u[h[[13 ;; 16]]]; offset = u[h[[17 ;; 20]]];
        require[offset + cdSize === size - Length[tail] + e - 1];
        SetStreamPosition[s, offset];
        Do[
            h = BinaryReadList[s, "Byte", 46]; require[Length[h] === 46 && h[[1 ;; 4]] === {80, 75, 1, 2}];
            n = u[h[[29 ;; 30]]]; x = u[h[[31 ;; 32]]]; c = u[h[[33 ;; 34]]];
            require[n > 0 && EvenQ[u[h[[9 ;; 10]]]] && u[h[[35 ;; 36]]] === 0];
            require[FreeQ[{u[h[[21 ;; 24]]], u[h[[25 ;; 28]]], u[h[[43 ;; 46]]]}, 4294967295]];
            mode = BitAnd[BitShiftRight[u[h[[39 ;; 42]]], 16], 61440];
            require[MemberQ[{0, 16384, 32768}, mode]];
            raw = BinaryReadList[s, "Byte", n]; require[Length[raw] === n];
            name = Quiet[Check[FromCharacterCode[raw, "UTF-8"], $Failed]]; require[safeNameQ[name]];
            AppendTo[names, name]; extra = BinaryReadList[s, "Byte", x]; require[Length[extra] === x]; fieldsOK[extra];
            local = StreamPosition[s] + c;
            SetStreamPosition[s, u[h[[43 ;; 46]]]];
            h = BinaryReadList[s, "Byte", 30]; require[Length[h] === 30 && h[[1 ;; 4]] === {80, 75, 3, 4}];
            require[u[h[[27 ;; 28]]] === n && BinaryReadList[s, "Byte", n] === raw];
            x = u[h[[29 ;; 30]]]; extra = BinaryReadList[s, "Byte", x]; require[Length[extra] === x]; fieldsOK[extra];
            SetStreamPosition[s, local], {count}];
        require[StreamPosition[s] === offset + cdSize];
        require[DuplicateFreeQ[ToLowerCase[StringTrim[#, "/"]] & /@ names]];
        names,
        Close[s]]
];

(* A fresh subprocess writes a small JSON report. Never parse its console as
   Wolfram input; no caller definitions or loaded package state are inherited. *)
probe[stage_, work_] := Module[{script, report, code, process = None, output, errors, diagnostics, data, started, timedOut},
    script = FileNameJoin[{work, "probe-" <> CreateUUID[] <> ".wls"}]; report = script <> ".json";
    code = "$FeynCalcStartupMessages=False;$LoadAddOns={};\n" <>
        "$Path=" <> ToString[Select[$Path, StringQ], InputForm] <> ";\n" <>
        "fc=Quiet[Check[FindFile[\"FeynCalc`\"],$Failed]];\n" <>
        "ok=StringQ[fc]&&Quiet[Check[Get[fc];StringQ[FeynCalc`$FeynCalcVersion],False]];\n" <>
        "ver=If[TrueQ[ok],FeynCalc`$FeynCalcVersion,\"unavailable\"];\n" <>
        "parts=StringCases[ver,DigitCharacter..]; nums=If[Length[parts]>=3,FromDigits/@Take[parts,3],{0,0,0}];\n" <>
        "ok=TrueQ[ok]&&OrderedQ[{{10,2,1},nums}];\n" <>
        If[stage === None, "",
            "root=" <> ToString[stage, InputForm] <> ";\n" <>
            "If[ok,calls=0;Block[{StartProcess=Function[Null,calls++;$Failed,HoldAll],RunProcess=Function[Null,calls++;$Failed,HoldAll]},\n" <>
            "loaded=Check[Get[FileNameJoin[{root,\"FeynGrav.wl\"}]],$Failed];\n" <>
            "ok=loaded=!=$Failed&&!FailureQ[loaded]&&TrueQ[FeynGrav`FeynGravInitialized]&&FileNameSplit[ExpandFileName[FeynGrav`Private`packageDirectory]]===FileNameSplit[ExpandFileName[root]];\n" <>
            "ok=ok&&TrueQ[Check[vertices={FeynGrav`GravitonVertex[a,b,p,c,d,q,e,f,r],FeynGrav`GravitonScalarVertex[{a,b},p,q,m],FeynGrav`GravitonFermionVertex[{a,b,k},p,q,m],FeynGrav`GravitonVectorVertex[{a,b,k},c,p,d,q]};\n" <>
            "FreeQ[vertices,_Failure|_FeynGrav`GravitonVertex|_FeynGrav`GravitonScalarVertex|_FeynGrav`GravitonFermionVertex|_FeynGrav`GravitonVectorVertex],False]];];ok=ok&&calls===0];\n"] <>
        "Export[" <> ToString[report, InputForm] <>
        ",<|\"OK\"->TrueQ[ok],\"WolframVersion\"->$Version,\"FeynCalcVersion\"->ver,\"FeynCalcPath\"->If[StringQ[fc],fc,\"\"]|>,\"RawJSON\"];Quit[];";
    checked[Export[script, code, "Text"], "ProbeFailed", "Cannot write verification script."];
    WithCleanup[Null,
        AbortProtect[process = checked[StartProcess[{First[$CommandLine], "-noinit", "-script", script}], "ProbeFailed", "Cannot start a fresh Wolfram kernel."]];
        started = AbsoluteTime[];
        While[ProcessStatus[process] === "Running" && AbsoluteTime[] - started < 180, Pause[0.1]];
        timedOut = ProcessStatus[process] === "Running";
        If[timedOut, KillProcess[process]];
        output = Replace[ReadString[process["StandardOutput"]], Except[_String] -> ""];
        errors = Replace[ReadString[process["StandardError"]], Except[_String] -> ""];
        diagnostics = <|"StandardOutput" -> output, "StandardError" -> errors, "ExitCode" -> ProcessInformation[process, "ExitCode"]|>;
        If[timedOut, stop["ProbeTimeout", "Fresh-kernel verification exceeded 180 seconds.", diagnostics]];
        If[!FileExistsQ[report], stop["ProbeFailed", "Fresh-kernel verification did not produce a report.", diagnostics]];
        data = Quiet[Check[Import[report, "RawJSON"], $Failed]];
        If[!AssociationQ[data], stop["ProbeFailed", "Invalid fresh-kernel report.", diagnostics]];
        If[!TrueQ[data["OK"]] || diagnostics["ExitCode"] =!= 0,
            data = Join[data, <|"OK" -> False, "Diagnostics" -> diagnostics|>]];
        data,
        If[Head[process] === ProcessObject,
            If[ProcessStatus[process] === "Running", KillProcess[process]];
            Quiet[Close[process["StandardOutput"]]]; Quiet[Close[process["StandardError"]]]]]
];
installDependency[] := Module[{loaded},
    requireInternet[];
    loaded = Check[Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"], $Failed];
    If[loaded === $Failed || DownValues[FeynCalcInstaller`InstallFeynCalc] === {},
        stop["DependencyInstallerFailed", "Could not load the official FeynCalc installer."]];
    FeynCalcInstaller`InstallFeynCalc[];
];
requiredFiles = Join[{"FeynGrav.wl", "Rules/Nieuwenhuizen.wl", "CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/FORMRuntime.wl", "CalcFormConverter/Templates/Program.frm.in"},
    Flatten[Table["Libs/" <> family <> "_" <> ToString[n],
        {family, {"GravitonVertex", "GravitonScalarVertex", "GravitonScalarPotentialVertex", "GravitonFermionVertex", "GravitonMassiveVectorVertex", "GravitonVectorVertex", "GravitonVectorGhostVertex"}}, {n, 2}]]];
(* Revision-aware runtime requirements: older compatible revisions did not load
   Conventions or Dirac/colour modules. Require each resource when its owning
   source names it. Read source as text only; never evaluate it for discovery.
   Extend this table when introducing a new runtime module or lazy template. *)
runtimeDependencies = {
    {"FeynGrav.wl", "Conventions.wl"},
    {"Rules/Nieuwenhuizen.wl", "Rules/RuleValidation.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/DiracColour.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/DiracAlgebra.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/ColourAlgebra.wl"},
    {"CalcFormConverter/DiracAlgebra.wl", "CalcFormConverter/Templates/DiracAlgebra.frm.in"},
    {"CalcFormConverter/ColourAlgebra.wl", "CalcFormConverter/Templates/ColourAlgebra.frm.in"}
};
validateLayout[dir_] := Module[{required = requiredFiles, missing, path, source, text},
    path[name_] := FileNameJoin[Prepend[StringSplit[name, "/"], dir]];
    Do[
        source = path[edge[[1]]];
        If[FileType[source] === File,
            text = Quiet[Check[Import[source, "Text"], $Failed]];
            If[!StringQ[text], stop["IncompletePackage", "Cannot read a required runtime source.", <|"UnreadableFile" -> edge[[1]]|>]];
            If[StringContainsQ[text, "\"" <> Last[StringSplit[edge[[2]], "/"]] <> "\""], AppendTo[required, edge[[2]]]]],
        {edge, runtimeDependencies}];
    missing = Select[DeleteDuplicates[required], FileType[path[#]] =!= File &];
    If[missing =!= {}, stop["IncompletePackage", "Archive lacks required runtime files or default vertex libraries.", <|"MissingFiles" -> missing|>]]
];

InstallFeynGrav[opts : OptionsPattern[]] := Module[
    {dest = OptionValue[InstallFeynGravTo], ref = OptionValue[FeynGravReference], archive = OptionValue[FeynGravArchive],
     overwrite = OptionValue[OverwriteFeynGrav], dependency = OptionValue[InstallFeynCalcDependency], work = None,
     backup = None, source, files, root, staged, dep, verified, result, cleanup = {}, retain = False, parent},
    result = Catch[CheckAbort[
        If[$VersionNumber < 12.2, stop["UnsupportedWolfram", "Wolfram Language 12.2 or newer is required."]];
        If[!AllTrue[First /@ {opts}, MemberQ[First /@ Options[InstallFeynGrav], #] &], stop["InvalidOption", "Unknown installer option."]];
        If[!referenceQ[ref] || !MemberQ[{True, False, Automatic}, overwrite] || !MemberQ[{True, False, Automatic}, dependency],
            stop["InvalidOption", "Invalid reference or consent option."]];
        If[dest === Automatic, dest = FileNameJoin[{$UserBaseDirectory, "Applications", "FeynGrav"}]];
        If[!StringQ[dest] || StringLength[dest] === 0 || !absolutePathQ[dest], stop["InvalidDestination", "Destination must be an absolute directory path."]];
        dest = ExpandFileName[dest];
        If[FileNameSplit[dest] === {} || DirectoryName[dest] === "", stop["InvalidDestination", "Destination cannot be a filesystem root."]];
        dest = FileNameJoin[FileNameSplit[dest]]; parent = DirectoryName[dest];
        If[dest === parent || (FileExistsQ[dest] && !DirectoryQ[dest]), stop["InvalidDestination", "Destination must name a package directory."]];
        If[gitCheckoutQ[dest], stop["GitCheckout", "Refusing to replace or install inside a Git working checkout.", <|"Path" -> dest|>]];
        If[archive =!= Automatic && (!StringQ[archive] || FileType[archive] =!= File), stop["InvalidArchive", "Local archive must be an existing ZIP file."]];
        source = If[archive === Automatic, archiveURL[ref], ExpandFileName[archive]];
        If[!DirectoryQ[parent], checked[CreateDirectory[parent, CreateIntermediateDirectories -> True], "CreateDirectoryFailed", "Cannot create destination parent."]];
        work = checked[CreateDirectory[FileNameJoin[{parent, ".feyngrav-install-" <> CreateUUID[]}]], "CreateDirectoryFailed", "Cannot create staging directory."];
        dep = probe[None, work];
        If[!TrueQ[dep["OK"]],
            If[!consent[dependency, "FeynCalc 10.2.1 or newer is required. Run its official installer? It manages its own prompts and changes."],
                stop["DependencyRequired", "Install a compatible FeynCalc first, then retry.", <|"Dependency" -> dep|>]];
            installDependency[]; dep = probe[None, work];
            If[!TrueQ[dep["OK"]], stop["DependencyVerificationFailed", "FeynCalc remains unavailable or incompatible after installation.", <|"Dependency" -> dep|>]]];
        If[archive === Automatic,
            archive = FileNameJoin[{work, "source.zip"}]; Print["Downloading FeynGrav (", ref, ") ..."];
            checked[Quiet[Check[download[source, archive], $Failed]], "DownloadFailed", "Could not download the FeynGrav archive."]];
        files = validateArchive[archive];
        root = DeleteDuplicates[First[StringSplit[#, "/"]] & /@ files];
        If[Length[root] =!= 1 || !MemberQ[files, First[root] <> "/FeynGrav.wl"], stop["InvalidLayout", "ZIP must contain one package root with FeynGrav.wl."]];
        staged = FileNameJoin[{work, "extracted"}]; CreateDirectory[staged];
        checked[Quiet[Check[extract[archive, staged], $Failed]], "ExtractionFailed", "Could not extract the FeynGrav archive."];
        staged = FileNameJoin[{staged, First[root]}]; validateLayout[staged];
        Print["Checking FeynGrav in a fresh kernel ..."];
        verified = probe[staged, work];
        If[!TrueQ[verified["OK"]], stop["PackageVerificationFailed", "Staged FeynGrav failed its loading or vertex checks.", <|"Verification" -> verified|>]];
        If[gitCheckoutQ[dest], stop["GitCheckout", "Destination is now inside a Git checkout; installation cancelled."]];
        If[DirectoryQ[dest] && !consent[overwrite, "Replace FeynGrav at " <> dest <> "? A complete sibling backup will be retained; extra libraries and edits will not be merged."],
            stop["ReplacementDeclined", "Existing installation was left unchanged."]];
        AbortProtect[
            If[DirectoryQ[dest], backup = dest <> ".backup-" <> CreateUUID[];
                checked[Quiet[Check[moveDirectory[dest, backup], $Failed]], "BackupFailed", "Cannot back up the existing installation."]];
            If[Quiet[Check[moveDirectory[staged, dest], $Failed]] === $Failed,
                If[backup =!= None && Quiet[Check[moveDirectory[backup, dest], $Failed]] === $Failed,
                    retain = True; stop["RollbackFailed", "Installation failed and automatic restoration failed. Recover the complete old installation from BackupPath.", <|"BackupPath" -> backup, "StagingPath" -> work|>]];
                stop["InstallFailed", "Could not install the staged directory; any previous installation was restored."]]
        ];
        <|"InstalledPath" -> dest, "Reference" -> If[OptionValue[FeynGravArchive] === Automatic, ref, Null],
          "Source" -> source, "Verification" -> verified, "BackupPath" -> If[backup === None, Null, backup]|>,
        failure["Aborted", "Installation interrupted. Inspect the destination and any reported backup before retrying.", <|"InstalledPath" -> dest, "BackupPath" -> backup|>]], $failureTag];
    If[StringQ[work] && DirectoryQ[work] && !retain,
        If[Quiet[Check[DeleteDirectory[work, DeleteContents -> True], $Failed]] === $Failed, AppendTo[cleanup, work]]];
    If[cleanup =!= {}, Print["Temporary files could not be removed: ", cleanup];
        result = If[FailureQ[result], Failure[result[[1]], Join[result[[2]], <|"RetainedPaths" -> cleanup|>]], Append[result, "RetainedPaths" -> cleanup]]];
    If[AssociationQ[result], Print["FeynGrav installed at ", dest, ". Restart the kernel, then evaluate << FeynGrav`."];
        If[backup =!= None, Print["Previous installation, including extra libraries and edits, retained at ", backup]]];
    result
];
End[];
EndPackage[];
Print["FeynGrav installer loaded. Evaluate InstallFeynGrav[] to install; use ?InstallFeynGrav for help."];
