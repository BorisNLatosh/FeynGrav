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
validateArchive[file_] := Module[
    {stream, archiveSize, archiveTail, endRecordPositions, endRecordStart,
     entryCount, centralOffset, centralSize, header, nameLength, extraLength,
     commentLength, rawName, entryName, fileTypeBits, nextCentralEntry,
     entryNames = {}, littleEndianInteger, extraFields, validateExtraFields, requireArchive},
    (* ZIP integers are unsigned and stored least-significant byte first. *)
    littleEndianInteger[b_] := FromDigits[Reverse[b], 256];
    requireArchive[q_] := If[!TrueQ[q], stop["InvalidArchive", "Unsupported or unsafe ZIP archive."]];
    validateExtraFields[bytes_] := Module[{extraOffset = 1, payloadLength, fieldID},
        While[extraOffset <= Length[bytes],
            requireArchive[extraOffset + 3 <= Length[bytes]];
            fieldID = littleEndianInteger[bytes[[extraOffset ;; extraOffset + 1]]];
            payloadLength = littleEndianInteger[bytes[[extraOffset + 2 ;; extraOffset + 3]]];
            (* Extra-field IDs: 10 = NTFS timestamps, 21589 (0x5455) = extended
               timestamps, 30837 (0x7875) = Unix UID/GID. Preserve this allowlist;
               alternate Unicode paths and ZIP64 metadata are rejected. *)
            requireArchive[MemberQ[{10, 21589, 30837}, fieldID] &&
                extraOffset + 3 + payloadLength <= Length[bytes]];
            extraOffset += 4 + payloadLength];
        True];
    stream = checked[OpenRead[file, BinaryFormat -> True], "ArchiveReadFailed", "Cannot read ZIP archive."];
    WithCleanup[Null,
        archiveSize = FileByteCount[file];
        requireArchive[archiveSize >= 22];
        (* End-of-central-directory record: 22 fixed bytes plus at most
           65535 comment bytes. Its signature is PK followed by bytes 5, 6. *)
        SetStreamPosition[stream, Max[0, archiveSize - 65557]];
        archiveTail = BinaryReadList[stream, "Byte"];
        endRecordPositions = SequencePosition[archiveTail, {80, 75, 5, 6}];
        requireArchive[endRecordPositions =!= {}];
        endRecordStart = First[Last[endRecordPositions]];
        requireArchive[endRecordStart + 21 <= Length[archiveTail]];
        header = Take[archiveTail, {endRecordStart, endRecordStart + 21}];
        requireArchive[endRecordStart + 21 + littleEndianInteger[header[[21 ;; 22]]] === Length[archiveTail]];
        (* Both disk numbers must be zero, and both entry counts must agree.
           A count of 0xFFFF is reserved for ZIP64, which is unsupported. *)
        requireArchive[littleEndianInteger[header[[5 ;; 8]]] === 0];
        entryCount = littleEndianInteger[header[[11 ;; 12]]];
        requireArchive[0 < entryCount < 65535 && entryCount === littleEndianInteger[header[[9 ;; 10]]]];
        centralSize = littleEndianInteger[header[[13 ;; 16]]];
        centralOffset = littleEndianInteger[header[[17 ;; 20]]];
        requireArchive[centralOffset + centralSize === archiveSize - Length[archiveTail] + endRecordStart - 1];
        (* Central-directory entries have 46 fixed bytes. Filename, extra-field
           and comment lengths occupy bytes 29-34 (one-based positions here). *)
        SetStreamPosition[stream, centralOffset];
        Do[
            header = BinaryReadList[stream, "Byte", 46];
            requireArchive[Length[header] === 46 && header[[1 ;; 4]] === {80, 75, 1, 2}];
            nameLength = littleEndianInteger[header[[29 ;; 30]]];
            extraLength = littleEndianInteger[header[[31 ;; 32]]];
            commentLength = littleEndianInteger[header[[33 ;; 34]]];
            (* Flag bit 0 denotes encryption; the starting disk must be zero. *)
            requireArchive[nameLength > 0 && EvenQ[littleEndianInteger[header[[9 ;; 10]]]] &&
                littleEndianInteger[header[[35 ;; 36]]] === 0];
            (* 0xFFFFFFFF in compressed/uncompressed size or local-header offset
               is the ZIP64 sentinel. These fields occupy bytes 21-28 and 43-46. *)
            requireArchive[FreeQ[{littleEndianInteger[header[[21 ;; 24]]],
                littleEndianInteger[header[[25 ;; 28]]], littleEndianInteger[header[[43 ;; 46]]]}, 4294967295]];
            (* Unix file type occupies the upper external-attribute bits:
               mask 0xF000; allow unspecified, directory (0x4000), regular (0x8000). *)
            fileTypeBits = BitAnd[BitShiftRight[littleEndianInteger[header[[39 ;; 42]]], 16], 61440];
            requireArchive[MemberQ[{0, 16384, 32768}, fileTypeBits]];
            rawName = BinaryReadList[stream, "Byte", nameLength];
            requireArchive[Length[rawName] === nameLength];
            entryName = Quiet[Check[FromCharacterCode[rawName, "UTF-8"], $Failed]];
            requireArchive[safeNameQ[entryName]];
            AppendTo[entryNames, entryName];
            extraFields = BinaryReadList[stream, "Byte", extraLength];
            requireArchive[Length[extraFields] === extraLength];
            validateExtraFields[extraFields];
            (* Save the next central entry before visiting the 30-byte local
               header. Its raw filename must agree with the central entry. *)
            nextCentralEntry = StreamPosition[stream] + commentLength;
            SetStreamPosition[stream, littleEndianInteger[header[[43 ;; 46]]]];
            header = BinaryReadList[stream, "Byte", 30];
            requireArchive[Length[header] === 30 && header[[1 ;; 4]] === {80, 75, 3, 4}];
            requireArchive[littleEndianInteger[header[[27 ;; 28]]] === nameLength &&
                BinaryReadList[stream, "Byte", nameLength] === rawName];
            extraLength = littleEndianInteger[header[[29 ;; 30]]];
            extraFields = BinaryReadList[stream, "Byte", extraLength];
            requireArchive[Length[extraFields] === extraLength];
            validateExtraFields[extraFields];
            SetStreamPosition[stream, nextCentralEntry], {entryCount}];
        requireArchive[StreamPosition[stream] === centralOffset + centralSize];
        requireArchive[DuplicateFreeQ[ToLowerCase[StringTrim[#, "/"]] & /@ entryNames]];
        entryNames,
        Close[stream]]
];

(* Keep the child program separate from process ownership and diagnostics.
   InputForm serialises injected paths without changing their Wolfram meaning. *)
probeSource[stage_, report_] :=
    (* Dependency discovery and minimum-version check. *)
    "$FeynCalcStartupMessages=False;$LoadAddOns={};\n" <>
        "$Path=" <> ToString[Select[$Path, StringQ], InputForm] <> ";\n" <>
        "fc=Quiet[Check[FindFile[\"FeynCalc`\"],$Failed]];\n" <>
        "ok=StringQ[fc]&&Quiet[Check[Get[fc];StringQ[FeynCalc`$FeynCalcVersion],False]];\n" <>
        "ver=If[TrueQ[ok],FeynCalc`$FeynCalcVersion,\"unavailable\"];\n" <>
        "parts=StringCases[ver,DigitCharacter..]; nums=If[Length[parts]>=3,FromDigits/@Take[parts,3],{0,0,0}];\n" <>
        "ok=TrueQ[ok]&&OrderedQ[{{10,2,1},nums}];\n" <>
        (* A staged package also has to load and resolve representative vertices
           without launching external processes. *)
        If[stage === None, "",
            "root=" <> ToString[stage, InputForm] <> ";\n" <>
            "If[ok,calls=0;Block[{StartProcess=Function[Null,calls++;$Failed,HoldAll],RunProcess=Function[Null,calls++;$Failed,HoldAll]},\n" <>
            "loaded=Check[Get[FileNameJoin[{root,\"FeynGrav.wl\"}]],$Failed];\n" <>
            "ok=loaded=!=$Failed&&!FailureQ[loaded]&&TrueQ[FeynGrav`FeynGravInitialized]&&FileNameSplit[ExpandFileName[FeynGrav`Private`packageDirectory]]===FileNameSplit[ExpandFileName[root]];\n" <>
            "ok=ok&&TrueQ[Check[vertices={FeynGrav`GravitonVertex[a,b,p,c,d,q,e,f,r],FeynGrav`GravitonScalarVertex[{a,b},p,q,m],FeynGrav`GravitonFermionVertex[{a,b,k},p,q,m],FeynGrav`GravitonVectorVertex[{a,b,k},c,p,d,q]};\n" <>
            "FreeQ[vertices,_Failure|_FeynGrav`GravitonVertex|_FeynGrav`GravitonScalarVertex|_FeynGrav`GravitonFermionVertex|_FeynGrav`GravitonVectorVertex],False]];];ok=ok&&calls===0];\n"] <>
        (* Report through JSON, independently of package console output. *)
        "Export[" <> ToString[report, InputForm] <>
        ",<|\"OK\"->TrueQ[ok],\"WolframVersion\"->$Version,\"FeynCalcVersion\"->ver,\"FeynCalcPath\"->If[StringQ[fc],fc,\"\"]|>,\"RawJSON\"];Quit[];";

(* A fresh subprocess writes a small JSON report. Never parse its console as
   Wolfram input; no caller definitions or loaded package state are inherited. *)
probe[stage_, work_] := Module[{script, report, code, process = None, output, errors, diagnostics, data, started, timedOut},
    script = FileNameJoin[{work, "probe-" <> CreateUUID[] <> ".wls"}]; report = script <> ".json";
    code = probeSource[stage, report];
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
requiredFiles = Join[{"FeynGrav.wl", "Rules/Nieuwenhuizen.wl", "CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/FORMRuntime.wl",
    "CalcFormConverter/Templates/Program.frm.in"},
    Flatten[Table["Libs/" <> family <> "_" <> ToString[n],
        {family, {"GravitonVertex", "GravitonScalarVertex", "GravitonScalarPotentialVertex", "GravitonFermionVertex", "GravitonMassiveVectorVertex", "GravitonVectorVertex", "GravitonVectorGhostVertex"}}, {n, 2}]]];
(* Revision-aware runtime requirements: older compatible revisions did not load
   Conventions or Dirac/colour modules. Require each resource when its owning
   source names it. Read source as text only; never evaluate it for discovery.
   Extend this table when introducing a new runtime module or lazy template.
   Detection below matches the quoted target basename anywhere in source text,
   including comments; dynamically constructed names are not discovered. Keep
   dependency filenames literal and maintain these edges when moving modules.
   Package-installer layout fixtures: FeynGravSimplify/Tests/Loading.wls.
   CalcFormConverter/Tests/Installer.wls tests the separate FORM installer. *)
runtimeDependencies = {
    {"FeynGrav.wl", "Conventions.wl"},
    {"Rules/Nieuwenhuizen.wl", "Rules/RuleValidation.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/MappingEncoding.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/ExportAssembly.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/ImportReconstruction.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/DiracColour.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/DiracAlgebra.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/ColourAlgebra.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/CoefficientParser.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/PropagatorGroups.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/RationalCoefficients.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/DimensionCoefficients.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/PropagatorCancellation.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/StageSymmetry.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/TensorDecomposition.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/LoopCoefficientReduction.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/ScalarMasterReduction.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/LoopDenominatorGroups.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/SharedCoefficientReader.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "CalcFormConverter/FORMRuntime.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "FeynGravSimplify/RestrictedCodec.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "FeynGravSimplify/Contractor.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "FeynGravSimplify/FORM.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "FeynGravSimplify/RestrictedCoordination.wl"},
    {"CalcFormConverter/CalcFormConverter.wl", "FeynGravSimplify/Facade.wl"},
    {"CalcFormConverter/LoopDenominatorGroups.wl", "CalcFormConverter/Templates/LoopDenominatorGroups.frm.in"},
    {"CalcFormConverter/PropagatorGroups.wl", "CalcFormConverter/Templates/PropagatorFactors.frm.in"},
    {"CalcFormConverter/RationalCoefficients.wl", "CalcFormConverter/Templates/RationalCoefficients.frm.in"},
    { "CalcFormConverter/RationalCoefficients.wl", "CalcFormConverter/Templates/MasterRationalCoefficients.frm.in"},
    {"CalcFormConverter/RationalCoefficients.wl", "CalcFormConverter/Templates/PolarisationCoefficients.frm.in"},
    {"CalcFormConverter/RationalCoefficients.wl", "CalcFormConverter/Templates/RationalCoefficientOutput.frm.in"},
    {"CalcFormConverter/DimensionCoefficients.wl", "CalcFormConverter/Templates/DimensionCoefficients.frm.in"},
    {"CalcFormConverter/DimensionCoefficients.wl", "CalcFormConverter/Templates/DimensionCoefficientOutput.frm.in"},
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
     backup = None, source, files, root, staged, dep, verified, result, cleanup = {}, retainWorkForRecovery = False, parent},
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
        (* A sibling staging directory keeps publication on the destination
           filesystem; the replacement below uses directory renames. *)
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
        (* Defer interruption across backup, publication and any restoration.
           The sibling backup is outside work and is never temporary cleanup. *)
        AbortProtect[
            If[DirectoryQ[dest], backup = dest <> ".backup-" <> CreateUUID[];
                checked[Quiet[Check[moveDirectory[dest, backup], $Failed]], "BackupFailed", "Cannot back up the existing installation."]];
            If[Quiet[Check[moveDirectory[staged, dest], $Failed]] === $Failed,
                If[backup =!= None && Quiet[Check[moveDirectory[backup, dest], $Failed]] === $Failed,
                    retainWorkForRecovery = True; stop["RollbackFailed", "Installation failed and automatic restoration failed. Recover the complete old installation from BackupPath.", <|"BackupPath" -> backup, "StagingPath" -> work|>]];
                stop["InstallFailed", "Could not install the staged directory; any previous installation was restored."]]
        ];
        <|"InstalledPath" -> dest, "Reference" -> If[OptionValue[FeynGravArchive] === Automatic, ref, Null],
          "Source" -> source, "Verification" -> verified, "BackupPath" -> If[backup === None, Null, backup]|>,
        failure["Aborted", "Installation interrupted. Inspect the destination and any reported backup before retrying.", <|"InstalledPath" -> dest, "BackupPath" -> backup|>]], $failureTag];
    (* Failed restoration retains work for manual recovery. Successful backups
       remain untouched whether staging cleanup succeeds or fails. *)
    If[StringQ[work] && DirectoryQ[work] && !retainWorkForRecovery,
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
