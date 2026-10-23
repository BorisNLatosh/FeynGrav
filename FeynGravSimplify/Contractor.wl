(* ::Package:: *)
(* Contractor restricted backend: executable discovery and Contractor-specific
   preparation/command construction, plus executable lookup shared with FORM.
   The shared expression codec lives in
   RestrictedCodec.wl and the shared job coordination in
   RestrictedCoordination.wl; this module keeps the donor helper names. It
   defines functions only and never launches programs at load time. *)
Begin["FeynGrav`Simplification`Private`"];

$moduleDirectory = DirectoryName[$InputFileName];

(* Basic executable lookup is shared with the restricted FORM backend.
   Contractor-specific preference and selection remain in resolveExecutable. *)
executableFileQ[path_String] := FileType[path] === File &&
    ($OperatingSystem === "Windows" || With[
        {mode = Quiet[Check[FileInformation[path, "UnixPermissionsCode"], $Failed]]},
        Which[IntegerQ[mode], BitAnd[mode, 73] =!= 0,
            StringQ[mode], BitAnd[FromDigits[mode, 8], 73] =!= 0, True, False]]);
findOnPath[name_String] := Module[{path = Environment["PATH"], names, candidates},
    If[!StringQ[path], Return[Missing["NotFound"]]];
    names = If[$OperatingSystem === "Windows" && FileExtension[name] === "", {name <> ".exe", name}, {name}];
    candidates = Flatten[Table[FileNameJoin[{dir, n}],
        {dir, StringSplit[path, If[$OperatingSystem === "Windows", ";", ":"]]}, {n, names}]];
    SelectFirst[candidates, executableFileQ, Missing["NotFound"]]
];
resolveExecutable[requested_] := Module[{path, sibling},
    If[requested === Automatic,
        sibling = FileNameJoin[{DirectoryName[DirectoryName[$moduleDirectory]], "contractor", "bin",
            If[$OperatingSystem === "Windows", "contractor.exe", "contractor"]}];
        path = If[executableFileQ[sibling], sibling, findOnPath["contractor"]],
        If[!StringQ[requested] || requested === "",
            Return[failure["InvalidOption", "ContractorExecutable must be Automatic or a nonempty executable name/path."]]];
        path = If[StringContainsQ[requested, {"/", "\\"}], ExpandFileName[requested], findOnPath[requested]];
        If[StringQ[path] && !executableFileQ[path], path = Missing["NotExecutable"]]
    ];
    If[MissingQ[path], failure["ContractorNotFound", "Cannot locate an executable Contractor. Supply ContractorExecutable explicitly.",
        <|"RequestedExecutable" -> requested|>], ExpandFileName[path]]
];

(* Contractor-specific preparation: resolve the executable and build the
   backend command. Option-value validation stays in the shared coordinator;
   only the selected backend's resolved values reach this helper. *)
contractorPreparation[executableValue_, workersValue_] := Module[{executable},
    executable = resolveExecutable[executableValue];
    If[FailureQ[executable], Return[executable]];
    <|"Executable" -> executable, "Workers" -> workersValue,
      "Commands" -> Join[{executable},
          If[workersValue === Automatic, {}, {"-threads", ToString[workersValue]}]]|>
];

End[];
