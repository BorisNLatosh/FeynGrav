(* ::Package:: *)

(*
    FeynGravLibrariesGenerator.wl

    This file defines the developer-side library generator for FeynGrav.
    It is responsible for producing the pre-generated interaction-vertex
    libraries stored in the Libs directory.

    Purpose
    -------

    FeynGrav uses pre-generated libraries for large symbolic expressions
    describing gravitational interaction vertices.  This is done for
    performance reasons: generating these expressions every time the user
    calls a vertex function would be prohibitively expensive.

    This package generates such libraries from the rule-generating packages
    stored in ../Rules.  It loads the relevant rule packages, constructs
    symbolic expressions in FeynCalc notation, translates them to FORM input,
    runs FORM to simplify the expressions, and translates the FORM output
    back to FeynCalc notation.

    The generated files are written to the current Libs directory and are
    later imported by the main FeynGrav package.

    Dependencies
    ------------

    This file requires:

        1. Wolfram Mathematica / Wolfram Language;
        2. FeynCalc;
        3. FORM available from the system command line as "form";
        4. the FeynGrav rule packages in ../Rules.

    The package sets the working directory to DirectoryName[$InputFileName],
    so it should be loaded from its file location, for example with

        Get["/path/to/FeynGrav/Libs/FeynGravLibrariesGenerator.wl"]

    or from a Mathematica session where $InputFileName is properly defined.

    Public interface
    ----------------

    The package exposes three groups of commands.

    1. Check commands

       These commands inspect the Libs directory and print which generated
       libraries are already present.  Examples:

           CheckGravitonScalars
           CheckGravitonFermions
           CheckGravitonVectors
           CheckGravitonVertex
           CheckGravitonSUNYM
           CheckHorndeskiG2
           CheckHorndeskiG3
           CheckHorndeskiG4
           CheckHorndeskiG5
           CheckScalarGaussBonnet
           CheckGravitonAxionVector
           CheckQuadraticGravityVertex

    2. Batch-generation commands

       These commands generate all libraries of a given class up to the
       specified perturbative order.  Examples:

           GenerateGravitonScalars[n]
           GenerateGravitonFermions[n]
           GenerateGravitonVectors[n]
           GenerateGravitonVertex[n]
           GenerateGravitonSUNYM[n]
           GenerateGravitonAxionVector[n]
           GenerateScalarGaussBonnet[n]
           GenerateQuadraticGravityVertex[n]

       Horndeski libraries require an additional argument specifying the
       maximal number of scalar fields included in the generation:

           GenerateHorndeskiG2[numberOfScalars, n]
           GenerateHorndeskiG3[numberOfScalars, n]
           GenerateHorndeskiG4[numberOfScalars, n]
           GenerateHorndeskiG5[numberOfScalars, n]

    3. Specific-generation commands

       These commands generate one library or one family member at a fixed
       perturbative order.  They are useful when only a single missing or
       modified library must be regenerated.  Examples:

           GenerateGravitonScalarsSpecific[n]
           GenerateGravitonFermionsSpecific[n]
           GenerateGravitonVectorsSpecific[n]
           GenerateGravitonVertexSpecific[n]
           GenerateGravitonSUNYMSpecific[n]
           GenerateGravitonAxionVectorSpecific[n]
           GenerateScalarGaussBonnetSpecific[n]
           GenerateQuadraticGravityVertexSpecific[n]

       For Horndeski interactions, the specific generators take the powers
       of the scalar field and of the kinetic term explicitly:

           GenerateHorndeskiG2Specific[a, b, n]
           GenerateHorndeskiG3Specific[a, b, n]
           GenerateHorndeskiG4Specific[a, b, n]
           GenerateHorndeskiG5Specific[a, b, n]

    Output files
    ------------

    The generator creates temporary FORM files with extension .frm.  After
    FORM finishes, the temporary .frm file is deleted and the final library
    is stored as a plain file without the .frm extension.  Typical output
    names are

        GravitonScalarVertex_1
        GravitonFermionVertex_2
        GravitonVertex_2
        HorndeskiG2_a_b_n
        ScalarGaussBonnet_n
        QuadraticGravityVertex_n

    Important warning
    -----------------

    Generation commands overwrite pre-existing libraries of the same name.
    Use the corresponding Check* command before regeneration if the current
    library set must be preserved.

    This file is intended for maintainers and developers of FeynGrav.  Regular
    users should normally use the pre-generated libraries distributed with the
    package or downloaded separately.
*)


(*
    Locate this generator file and load all rule-generation packages.

    The generator lives in

        FeynGrav/Libs/FeynGravLibrariesGenerator.wl

    while the rule packages live in

        FeynGrav/Rules/*.wl

    Therefore, the code below first determines the directory containing this
    file, then constructs the path to ../Rules in a platform-independent way.
*)

With[
    {
        (*
            $InputFileName is the name of the file currently being loaded.
            This is reliable when the generator is loaded with Get[...] or <<... .

            In an interactive notebook session, however, $InputFileName is "".
            In that case we cannot reliably infer the location of the Libs
            directory, so we abort with an explicit diagnostic.
        *)
        generatorFileName = $InputFileName,

        (*
            List of rule packages required by the library generator.

            Each entry has the form

                {contextName, fileName}

            where contextName is the package context introduced by the rule file,
            and fileName is the corresponding file in ../Rules.
        *)
        rulePackages = {
            {"GravitonScalarVertex`",        "GravitonScalarVertex.wl"},
            {"GravitonFermionVertex`",       "GravitonFermionVertex.wl"},
            {"GravitonVectorVertex`",        "GravitonVectorVertex.wl"},
            {"GravitonSUNYM`",               "GravitonSUNYM.wl"},
            {"GravitonVertex`",              "GravitonVertex.wl"},
            {"HorndeskiG2`",                 "HorndeskiG2.wl"},
            {"HorndeskiG3`",                 "HorndeskiG3.wl"},
            {"HorndeskiG4`",                 "HorndeskiG4.wl"},
            {"HorndeskiG5`",                 "HorndeskiG5.wl"},
            {"ScalarGaussBonnet`",           "ScalarGaussBonnet.wl"},
            {"GravitonAxionVectorVertex`",   "GravitonAxionVectorVertex.wl"},
            {"QuadraticGravityVertex`",      "QuadraticGravityVertex.wl"}
        }
    },

    (*
        This generator is designed to be loaded as a file.

        If $InputFileName is empty, Mathematica is most likely evaluating this
        code directly in a notebook.  In that mode DirectoryName[$InputFileName]
        does not identify the package location, so relative paths to ../Rules
        cannot be constructed safely.
    *)
    If[!StringQ[generatorFileName] || generatorFileName === "",
        Print[
            "FeynGravLibrariesGenerator.wl must be loaded from a file, ",
            "for example with Get[\"/path/to/FeynGrav/Libs/",
            "FeynGravLibrariesGenerator.wl\"]."
        ];
        Abort[];
    ];

    Module[
        {
            generatorDirectory,
            rulesDirectory,
            missingRuleFiles
        },

        (*
            Directory containing FeynGravLibrariesGenerator.wl.
        *)
        generatorDirectory = ExpandFileName @ DirectoryName[generatorFileName];

        (*
            Directory containing the rule-generation packages.

            FileNameJoin is used instead of string concatenation, because it
            builds paths in the form expected by the current operating system.
        *)
        rulesDirectory =
            ExpandFileName @ FileNameJoin[{generatorDirectory, "..", "Rules"}];

        (*
            Fail early if the expected FeynGrav project layout is not present.
        *)
        If[!DirectoryQ[rulesDirectory],
            Print["Cannot find the FeynGrav Rules directory: ", rulesDirectory];
            Abort[];
        ];

        (*
            Check that all rule files exist before opening the package context.
        *)
        missingRuleFiles =
            Select[
                rulePackages,
                !FileExistsQ[FileNameJoin[{rulesDirectory, #[[2]]}]] &
            ];

        If[missingRuleFiles =!= {},
            Print["Cannot find the following FeynGrav rule files:"];
            Scan[
                Print["  ", FileNameJoin[{rulesDirectory, #[[2]]}]] &,
                missingRuleFiles
            ];
            Abort[];
        ];

        (*
            Set the current working directory to Libs.

            Many later functions in this file use relative paths such as

                FileNames["GravitonScalarVertex_*"]

            so they implicitly assume that the current directory is Libs.
        *)
        SetDirectory[generatorDirectory];

        (*
            Open the public context of the library generator.
        *)
        BeginPackage["FeynGravLibrariesGenerator`", {"FeynCalc`"}];

        (*
            Load every rule package.

            Needs[context, file] loads the file only if the corresponding
            context has not already been loaded.
        *)
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            rulePackages
        ];

        (*
            Restore the current working directory to Libs.

            This is essential.  Some imported rule packages may change the
            current directory as a side effect.  The Check* and Generate*
            procedures below use relative file names, so they must run with
            Libs as the current working directory.

            Without this line, commands such as CheckGravitonScalars may find
            no files and therefore print nothing.
        *)
        SetDirectory[generatorDirectory];
    ];
];


(*
    Print a short startup message after the generator has been loaded.

    The variable $FeynGravLibrariesGeneratorStartupMessage controls whether
    the message is printed.  Users who do not want startup output can evaluate

        $FeynGravLibrariesGeneratorStartupMessage = False;

    before loading this file.
*)

If[!ValueQ[$FeynGravLibrariesGeneratorStartupMessage],
    $FeynGravLibrariesGeneratorStartupMessage = True;
];

FeynGravLibrariesGeneratorPrintStartupMessage[] :=
    Print[
        StringRiffle[
            {
                "",
                "FeynGravLibrariesGenerator",
                "--------------------------",
                "",
                "This package generates precomputed FeynGrav libraries for gravitational interaction vertices.",
                "The generated libraries are stored in the Libs directory and are used by the main FeynGrav package.",
                "",
                "Typical workflow:",
                "  1. Use Check* commands to see which libraries already exist.",
                "     Examples:",
                "       CheckGravitonScalars",
                "       CheckGravitonFermions",
                "       CheckGravitonVertex",
                "",
                "  2. Use Generate* commands to generate missing libraries.",
                "     Examples:",
                "       GenerateGravitonScalars[n]",
                "       GenerateGravitonFermions[n]",
                "       GenerateGravitonVertex[n]",
                "",
                "  3. Use Generate*Specific commands to regenerate a single library.",
                "     Examples:",
                "       GenerateGravitonScalarsSpecific[n]",
                "       GenerateHorndeskiG2Specific[a, b, n]",
                "",
                "Requirements:",
                "  - FeynCalc must be installed and loadable.",
                "  - FORM must be available from the command line as form.",
                "  - The FeynGrav Rules directory must be present next to Libs.",
                "",
                "To suppress this message, evaluate before loading the generator:",
                "  $FeynGravLibrariesGeneratorStartupMessage = False;",
                ""
            },
            "\n"
        ]
    ];

If[TrueQ[$FeynGravLibrariesGeneratorStartupMessage],
    FeynGravLibrariesGeneratorPrintStartupMessage[];
];


(*
    FORM availability check.

    FeynGravLibrariesGenerator uses FORM to simplify large expressions during
    library generation.  The code below checks whether FORM can be executed
    from this Wolfram kernel and tries to extract its version from FORM's
    startup banner.

    The check is non-fatal: if FORM is not found, the package is still loaded.
    This allows users to inspect existing libraries with Check* commands even
    on systems where FORM is not installed.

    Users may configure the executable name/path before loading the package:

        $FeynGravFORMExecutable = "/usr/local/bin/form";

    or suppress this startup check with

        $FeynGravLibrariesGeneratorFORMCheck = False;
*)

If[!ValueQ[$FeynGravFORMExecutable],
    $FeynGravFORMExecutable = "form";
];

If[!ValueQ[$FeynGravLibrariesGeneratorFORMCheck],
    $FeynGravLibrariesGeneratorFORMCheck = True;
];

ClearAll[
    FeynGravLibrariesGeneratorParseFORMVersion,
    FeynGravLibrariesGeneratorFORMInformation,
    FeynGravLibrariesGeneratorPrintFORMStatus
];

FeynGravLibrariesGeneratorParseFORMVersion[output_String] :=
    Module[
        {
            matches
        },

        (*
            Different FORM versions may format the startup banner differently.
            We therefore try several conservative patterns.

            Typical forms that this parser is intended to catch include

                FORM version 4.3.1
                FORM 4.3.1
                ... version 4.3.1 ...

            If no version-like string is found, we return Missing["Unknown"].
        *)
        matches =
            Flatten @ StringCases[
                output,
                {
                    RegularExpression[
                        "(?im)\\bFORM\\s+version\\s+([0-9]+(?:\\.[0-9]+)*(?:[-._A-Za-z0-9]*)?)"
                    ] -> "$1",

                    RegularExpression[
                        "(?im)^\\s*FORM\\s+([0-9]+(?:\\.[0-9]+)*(?:[-._A-Za-z0-9]*)?)"
                    ] -> "$1",

                    RegularExpression[
                        "(?im)\\bversion\\s+([0-9]+(?:\\.[0-9]+)*(?:[-._A-Za-z0-9]*)?)"
                    ] -> "$1"
                }
            ];

        If[matches === {},
            Missing["Unknown"],
            First[matches]
        ]
    ];

FeynGravLibrariesGeneratorFORMInformation[
    formExecutable_String : $FeynGravFORMExecutable
] :=
    Module[
        {
            temporaryDirectory,
            formFile,
            formProgram,
            result,
            output,
            installedQ,
            version
        },

        temporaryDirectory =
            CreateDirectory[
                FileNameJoin[
                    {
                        $TemporaryDirectory,
                        "FeynGravFORMCheck-" <> StringDelete[CreateUUID[], "-"]
                    }
                ]
            ];

        formFile = FileNameJoin[{temporaryDirectory, "form_check.frm"}];

        (*
            Minimal FORM program.

            If FORM is executable and works correctly, it should simplify
            x + x and print an expression containing 2*x.
        *)
        formProgram =
            StringRiffle[
                {
                    "Symbols x;",
                    "Local F = x + x;",
                    "Print;",
                    ".end"
                },
                "\n"
            ];

        Export[formFile, formProgram, "Text"];

        (*
            Run FORM in the temporary directory.

            We use Check[...] because RunProcess emits messages if the executable
            is absent.  TimeConstrained[...] prevents package loading from
            hanging if the external process misbehaves.
        *)
        result =
            TimeConstrained[
                Quiet @ Check[
                    RunProcess[
                        {formExecutable, formFile},
                        All,
                        "",
                        ProcessDirectory -> temporaryDirectory
                    ],
                    $Failed
                ],
                10,
                $Failed
            ];

        output =
            If[AssociationQ[result],
                Lookup[result, "StandardOutput", ""] <>
                    "\n" <>
                    Lookup[result, "StandardError", ""],
                ""
            ];

        installedQ =
            AssociationQ[result]
                && Lookup[result, "ExitCode", -1] === 0
                && StringContainsQ[
                    output,
                    RegularExpression["2\\s*\\*\\s*x"]
                ];

        version =
            If[installedQ,
                FeynGravLibrariesGeneratorParseFORMVersion[output],
                Missing["NotInstalled"]
            ];

        Quiet @ DeleteDirectory[temporaryDirectory, DeleteContents -> True];

        <|
            "Installed" -> installedQ,
            "Executable" -> formExecutable,
            "Version" -> version,
            "Output" -> output
        |>
    ];

FeynGravLibrariesGeneratorPrintFORMStatus[] :=
    Module[
        {
            formInformation,
            version
        },

        formInformation =
            FeynGravLibrariesGeneratorFORMInformation[$FeynGravFORMExecutable];

        If[TrueQ[formInformation["Installed"]],
            version = formInformation["Version"];

            If[MissingQ[version],
                Print[
                    "FORM is installed and available as \"",
                    formInformation["Executable"],
                    "\". Version: unknown."
                ],
                Print[
                    "FORM is installed and available as \"",
                    formInformation["Executable"],
                    "\". Version: ",
                    version,
                    "."
                ]
            ],
            Print[
                "FORM is not installed or is not available as \"",
                formInformation["Executable"],
                "\" from this Wolfram kernel."
            ]
        ];
    ];

If[TrueQ[$FeynGravLibrariesGeneratorFORMCheck],
    FeynGravLibrariesGeneratorPrintFORMStatus[];
];


(* Procedures that verify whether libraries exist. *)


CheckGravitonScalars::usage =
    "CheckGravitonScalars checks which generated libraries for graviton-scalar interaction vertices are present.";

CheckGravitonFermions::usage =
    "CheckGravitonFermions checks which generated libraries for graviton-fermion interaction vertices are present.";

CheckGravitonVectors::usage =
    "CheckGravitonVectors checks which generated libraries for graviton-vector interaction vertices are present.";

CheckGravitonVertex::usage =
    "CheckGravitonVertex checks which generated libraries for pure graviton interaction vertices are present.";

CheckGravitonSUNYM::usage =
    "CheckGravitonSUNYM checks which generated libraries for gravitational interactions of the SU(N) Yang-Mills model are present.";

CheckGravitonAxionVector::usage =
    "CheckGravitonAxionVector checks which generated libraries for graviton-axion-vector interaction vertices are present.";

CheckHorndeskiG2::usage =
    "CheckHorndeskiG2 checks which generated libraries for Horndeski G2 interaction vertices are present.";

CheckHorndeskiG3::usage =
    "CheckHorndeskiG3 checks which generated libraries for Horndeski G3 interaction vertices are present.";

CheckHorndeskiG4::usage =
    "CheckHorndeskiG4 checks which generated libraries for Horndeski G4 interaction vertices are present.";

CheckHorndeskiG5::usage =
    "CheckHorndeskiG5 checks which generated libraries for Horndeski G5 interaction vertices are present.";

CheckScalarGaussBonnet::usage =
    "CheckScalarGaussBonnet checks which generated libraries for scalar-Gauss-Bonnet interaction vertices are present.";

CheckQuadraticGravityVertex::usage =
    "CheckQuadraticGravityVertex checks which generated libraries for quadratic-gravity interaction vertices are present.";


(* Procedures that generate libraries. *)


GenerateGravitonScalars::usage =
    "GenerateGravitonScalars[n] generates libraries for graviton-scalar interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateGravitonFermions::usage =
    "GenerateGravitonFermions[n] generates libraries for graviton-fermion interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateGravitonVectors::usage =
    "GenerateGravitonVectors[n] generates libraries for graviton-vector interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateGravitonVertex::usage =
    "GenerateGravitonVertex[n] generates libraries for pure graviton interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateGravitonSUNYM::usage =
    "GenerateGravitonSUNYM[n] generates libraries for gravitational interactions of the SU(N) Yang-Mills model through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateGravitonAxionVector::usage =
    "GenerateGravitonAxionVector[n] generates libraries for graviton-axion-vector interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateHorndeskiG2::usage =
    "GenerateHorndeskiG2[p, n] generates libraries for Horndeski G2 interaction vertices involving up to p scalar fields through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateHorndeskiG3::usage =
    "GenerateHorndeskiG3[p, n] generates libraries for Horndeski G3 interaction vertices involving up to p scalar fields through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateHorndeskiG4::usage =
    "GenerateHorndeskiG4[p, n] generates libraries for Horndeski G4 interaction vertices involving up to p scalar fields through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateHorndeskiG5::usage =
    "GenerateHorndeskiG5[p, n] generates libraries for Horndeski G5 interaction vertices involving up to p scalar fields through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateScalarGaussBonnet::usage =
    "GenerateScalarGaussBonnet[n] generates libraries for scalar-Gauss-Bonnet interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";

GenerateQuadraticGravityVertex::usage =
    "GenerateQuadraticGravityVertex[n] generates libraries for quadratic-gravity interaction vertices through perturbative order n. Pre-existing libraries of this type are removed.";


(* Procedures that generate specific libraries. *)


GenerateGravitonScalarsSpecific::usage =
    "GenerateGravitonScalarsSpecific[n] generates the library for graviton-scalar interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateGravitonFermionsSpecific::usage =
    "GenerateGravitonFermionsSpecific[n] generates the library for graviton-fermion interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateGravitonVectorsSpecific::usage =
    "GenerateGravitonVectorsSpecific[n] generates the library for graviton-vector interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateGravitonVertexSpecific::usage =
    "GenerateGravitonVertexSpecific[n] generates the library for pure graviton interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateGravitonSUNYMSpecific::usage =
    "GenerateGravitonSUNYMSpecific[n] generates the library for gravitational interactions of the SU(N) Yang-Mills model at perturbative order n. The pre-existing library for this order is removed.";

GenerateGravitonAxionVectorSpecific::usage =
    "GenerateGravitonAxionVectorSpecific[n] generates the library for graviton-axion-vector interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateHorndeskiG2Specific::usage =
    "GenerateHorndeskiG2Specific[a, b, n] generates the library for Horndeski G2 interaction vertices with parameters a and b at perturbative order n. The pre-existing library for these parameters and this order is removed.";

GenerateHorndeskiG3Specific::usage =
    "GenerateHorndeskiG3Specific[a, b, n] generates the library for Horndeski G3 interaction vertices with parameters a and b at perturbative order n. The pre-existing library for these parameters and this order is removed.";

GenerateHorndeskiG4Specific::usage =
    "GenerateHorndeskiG4Specific[a, b, n] generates the library for Horndeski G4 interaction vertices with parameters a and b at perturbative order n. The pre-existing library for these parameters and this order is removed.";

GenerateHorndeskiG5Specific::usage =
    "GenerateHorndeskiG5Specific[a, b, n] generates the library for Horndeski G5 interaction vertices with parameters a and b at perturbative order n. The pre-existing library for these parameters and this order is removed.";

GenerateScalarGaussBonnetSpecific::usage =
    "GenerateScalarGaussBonnetSpecific[n] generates the library for scalar-Gauss-Bonnet interaction vertices at perturbative order n. The pre-existing library for this order is removed.";

GenerateQuadraticGravityVertexSpecific::usage =
    "GenerateQuadraticGravityVertexSpecific[n] generates the library for quadratic-gravity interaction vertices at perturbative order n. The pre-existing library for this order is removed.";


Begin["Private`"];


(*
    Helper functions for generating dummy index and momentum lists.

    These functions are used by the library generator to construct formal
    arguments for FeynGrav rule-generating functions. The generated symbols
    do not carry any physical meaning by themselves. They only serve as
    placeholders for Lorentz indices and momenta.

    Naming convention:
        m1, n1, m2, n2, ...     Lorentz-index pairs of gravitons;
        p1, p2, p3, ...         generic momenta;
        k1, k2, k3, ...         graviton momenta.

    The output order is important. Many rule-generating functions expect
    their graviton data as a flat list, e.g.
        {m1, n1, k1, m2, n2, k2, ...}
    rather than as a nested list of triples.
*)

(*
    Generate n Lorentz-index pairs:
        DummyArray[2] -> {m1, n1, m2, n2}

    This is used for rules where each graviton leg is represented only by
    its two Lorentz indices and no momentum is attached to the leg.
*)
DummyArray[n_Integer?NonNegative] :=
    Flatten[
        Table[
            {
                ToExpression["m" <> ToString[i]],
                ToExpression["n" <> ToString[i]]
            },
            {i, n}
        ]
    ];

(*
    Generate n generic momenta:
        DummyMomenta[3] -> {p1, p2, p3}

    This is used for rules that require only a list of formal external
    momenta and no associated graviton-index pairs.
*)
DummyMomenta[n_Integer?NonNegative] :=
    Table[
        ToExpression["p" <> ToString[i]],
        {i, n}
    ];

(*
    Generate n triples {mi, ni, pi}:
        DummyArrayMomenta[2] -> {m1, n1, p1, m2, n2, p2}

    Each triple represents one graviton leg: two Lorentz indices and one
    associated momentum. This convention is used when the graviton momenta
    are naturally denoted by p1, p2, ...
*)
DummyArrayMomenta[n_Integer?NonNegative] :=
    Flatten[
        Table[
            {
                ToExpression["m" <> ToString[i]],
                ToExpression["n" <> ToString[i]],
                ToExpression["p" <> ToString[i]]
            },
            {i, n}
        ]
    ];

(*
    Generate n triples {mi, ni, ki}:
        DummyArrayMomentaK[2] -> {m1, n1, k1, m2, n2, k2}

*)
DummyArrayMomentaK[n_Integer?NonNegative] :=
    Flatten[
        Table[
            {
                ToExpression["m" <> ToString[i]],
                ToExpression["n" <> ToString[i]],
                ToExpression["k" <> ToString[i]]
            },
            {i, n}
        ]
    ];


(* Procedures that verify whether libraries exist. *)


(* Scalars. *)


CheckGravitonScalars := (
	Scan[ Print["Libraries for the scalar field kinetic term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
	Scan[ Print["Libraries for the scalar field potential term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
);


(* Fermions. *)


CheckGravitonFermions := Scan[ Print["Libraries for Dirac fermion vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFermionVertex_*"] ];


(* Vectors. *)


CheckGravitonVectors := (
	Scan[ Print["Libraries for Proca field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonMassiveVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector-ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorGhostVertex_*"] ];
);


(* Gravitons. *)


CheckGravitonVertex := (
	Scan[ Print["Libraries for graviton vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVertex_*"] ];
);


(* SU(N) Yang-Mills. *)


CheckGravitonSUNYM := (
	Scan[ Print["Libraries for graviton-quark-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonQuarkGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonThreeGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFourGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonYMGhostVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonGhostVertex_*"] ];
);


(* Horndeski. *)


CheckHorndeskiG2 := Scan[ ( Print["Horndeski G2 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG2_*"] ];


CheckHorndeskiG3 := Scan[ ( Print["Horndeski G3 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG3_*"] ];


CheckHorndeskiG4 := Scan[ ( Print["Horndeski G4 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG4_*"] ];


CheckHorndeskiG5 := Scan[ ( Print["Horndeski G5 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG5_*"] ];


CheckScalarGaussBonnet := Scan[ Print["Libraries for Scalar-Gauss-Bonnet vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["ScalarGaussBonnet_*"] ];


(* Graviton-Axion. *)


CheckGravitonAxionVector := Scan[ Print["Libraries for gravitational interaction of a scalar axion coupled to a single vector field exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonAxionVectorVertex_*"] ];


(* Quadratic gravity. *)


CheckQuadraticGravityVertex := (
	Scan[ Print["Libraries for quadratic gravity vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["QuadraticGravityVertex_*"] ];
);


(* Procedures that generate libraries. *)


(* Supplementary functions. *)


(* The function converts FeynCalc output to a FORM-executable file with FeynCalc2FORM and other tools. *)


FORMCodeCleanUp[filePath_, np_, nk_] :=
    Module[
        {
            theDictionary = {
                "\\[Kappa]" -> "Kappa",
                "\\[Alpha]" -> "al",
                "\\[Beta]" -> "be",
                "\\[CapitalTheta]" -> "cthet",
                "\\[GothicM]" -> "gom",
                "(ScriptA)" -> "sca",
                "(ScriptB)" -> "scb",
                "(ScriptM)" -> "scm",
                "(ScriptN)" -> "scn",
                "(ScriptR)" -> "scr",
                "(ScriptS)" -> "scs",
                "(ScriptL)" -> "scl",
                "(ScriptT)" -> "sct",
                "\\[Lambda]" -> "lbd",
                "(Lambda)" -> "lbd",
                "(Tau)" -> "tau",
                "(Omega)" -> "omg",
                "(Epsilon)" -> "eps",
                "(CapitalTheta)" -> "cthet",
                "GaugeFixingEpsilon" -> "gfEPS",
                "(GothicM)" -> "gom"
            },
            theFileIndicesArray,
            theFileMomentaArray,
            theText
        },

        (* Remove Mathematica package contexts that FORM cannot parse. *)
        Export[
            filePath,
            StringReplace[
                Import[filePath, "Text"],
                {
                    "Private`" -> "",
                    "FeynGrav`" -> "",
                    "FeynCalc`FeynCalc2FORM`" -> ""
                }
            ],
            "Text"
        ];

        (* Make the expression into a single line. *)
        Export[
            filePath,
            StringRiffle[
                Join[
                    {First[#]},
                    {StringJoin[Rest[#]]}
                ],
                "\n"
            ] & @ Import[filePath, "Lines"],
            "Text"
        ];

        (* Replace Mathematica-style names by FORM-safe names. *)
        Export[
            filePath,
            StringReplace[
                Import[filePath, "Text"],
                theDictionary
            ],
            "Text"
        ];

        (*
            FeynCalc2FORM may leave Dirac gamma matrices in a Mathematica-like
            intermediate notation. Convert

                diracg(Lorentzi_ndex(mu,D),D)

            to FORM notation

                g_(0,mu)
        *)
        Export[
            filePath,
            StringReplace[
                Import[filePath, "Text"],
                "diracg(Lorentzi_ndex(" ~~ x : (WordCharacter ..) ~~ ",D),D)" :>
                    "g_(0," <> x <> ")"
            ],
            "Text"
        ];

        theText = Import[filePath, "Text"];

        (* Collect Lorentz indices appearing in d_(...), e_(...), and g_(0,...). *)
        theFileIndicesArray =
            Join[
                Flatten[
                    StringCases[
                        theText,
                        "d_(" ~~ x1 : (WordCharacter ..) ~~ "," ~~
                            x2 : (WordCharacter ..) ~~ ")" :> {x1, x2}
                    ]
                ],
                Flatten[
                    StringCases[
                        theText,
                        "e_(" ~~ a : (WordCharacter ..) ~~ "," ~~
                            b : (WordCharacter ..) ~~ "," ~~
                            c : (WordCharacter ..) ~~ "," ~~
                            d : (WordCharacter ..) ~~ ")" :> {a, b, c, d}
                    ]
                ],
                Flatten[
                    StringCases[
                        theText,
                        "g_(0," ~~ x : (WordCharacter ..) ~~ ")" :> x
                    ]
                ]
            ] // DeleteDuplicates;

        (* Build the vector list without a trailing comma. *)
        theFileMomentaArray =
            Join[
                "p" <> ToString[#] & /@ Range[np],
                "k" <> ToString[#] & /@ Range[nk]
            ];

        Export[
            filePath,
            "Indices " <> StringRiffle[theFileIndicesArray, ","] <> ";\n" <>
            If[
                theFileMomentaArray === {},
                "",
                "Vectors " <> StringRiffle[theFileMomentaArray, ","] <> ";\n"
            ] <>
            Import[filePath, "Text"],
            "Text"
        ];

        (* Put the expression in the local variable theResult. *)
        Export[
            filePath,
            MapAt[
                "Local theResult = " <> # <> ";" &,
                Import[filePath, {"Text", "Lines"}],
                4
            ],
            "Lines"
        ];

        (* Add the end of the FORM program. *)
        Export[
            filePath,
            Join[
                Import[filePath, {"Text", "Lines"}],
                {"print theResult;", ".end"}
            ],
            "Lines"
        ];
    ];


(* The function that cleans the FORM output file. *)


FORMOutputCleanUp[filePath_] :=
	Module[{},
		(* Clean the output *)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["theResult =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], "e_(" ~~ a : (WordCharacter ..) ~~ "," ~~ b : (WordCharacter ..) ~~ "," ~~ c : (WordCharacter ..) ~~ "," ~~ d : (WordCharacter ..) ~~ ")" :>  "LeviCivita[" <> a <> "," <> b <> "," <> c <> "," <> d <> "]"], "Text"];
		
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","gfEPS"->"GaugeFixingEpsilon","al"->"\\[Alpha]","be"->"\\[Beta]","lbd"->"\\[Lambda]","cthet"->"\\[CapitalTheta]","gsCoupling"->"SMP[\"g_s\"]","gom"->"\\[GothicM]" }], "Text"];
	];


(* Scalars. *)


GenerateGravitonScalars[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonScalarsSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonScalarsSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
	(* Kinetic term *)
	filePath = "GravitonScalarVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonScalarVertexUncontracted[DummyArray[n],p1,p2,m] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
	
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];

	Print["Scalar field kinetic term vertices is generated for n="<>ToString[n]<>"."];
	
	(* Potential term. *)
	filePath = "GravitonScalarPotentialVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,GravitonScalarPotentialVertexUncontracted[DummyArray[n],Global`\[Lambda]]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	
	Print["Done for the kinetic term for order n="<>ToString[n]<>"."];
];


GenerateGravitonFermions[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonFermionsSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonFermionsSpecific[n_Integer?Positive] := Module[
	{
		libraryFilePath = "GravitonFermionVertex_" <> ToString[n],
		formFilePath = "GravitonFermionVertex_" <> ToString[n]  <> ".frm",
		theTimingVariable,
		formExitCode
	},
	
	(* Remove the old FORM input file if it exists. *)
	If[FileExistsQ[formFilePath],
		DeleteFile[formFilePath]
	];
	
	(* Create a new empty FORM input file. *)
	CreateFile[formFilePath];
	
	(* Remove the old generated library file if it exists. *)
	If[FileExistsQ[libraryFilePath],
		DeleteFile[libraryFilePath]
	];
	
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable =
		Timing[
			FeynCalc2FORM[
				formFilePath,
				GravitonFermionVertexUncontracted[DummyArrayMomentaK[n],p1,p2,m]
			]
		][[1]];
		
	Print[
		"The expression is generated in ",
		theTimingVariable,
		" seconds."
	];
	
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[formFilePath, 2, n];
	
	(* Run FORM and save its output to the library file. *)
	{theTimingVariable, formExitCode} =
		Timing[
			Run[
				"form -q " <> formFilePath <> " > " <> libraryFilePath
			]
		];
		
	If[formExitCode =!= 0,
		Print[
			"FORM failed while processing ",
			formFilePath,
			". Exit code: ",
			formExitCode,
			"."
		];
		Return[$Failed];
	];
	
	If[!FileExistsQ[libraryFilePath],
		Print[
			"FORM did not create the expected library file ",
			libraryFilePath,
			"."
		];
		Return[$Failed];
	];
	
	Print[
		"FORM calculated the expression in ",
		theTimingVariable,
		" seconds."
	];
	
	DeleteFile[formFilePath];
	
	(* Clean the output. *)
	FORMOutputCleanUp[libraryFilePath];
	
	Export[
		libraryFilePath,
		StringReplace[
			Import[libraryFilePath, "Text"],
			{
			"g_(0," ~~ x : (("p" | "k") ~~ DigitCharacter ..) ~~ ")" :>
				"DiracGamma[Momentum[" <> x <> ",D],D]",
				
			"g_(0," ~~ x : (WordCharacter ..) ~~ ")" :>
				"DiracGamma[LorentzIndex[" <> x <> ",D],D]"
			}
		],
		"Text"
	];
	
	Export[
		libraryFilePath,
		StringReplace[
			Import[libraryFilePath, "Text"],
			{
			"GAD[" ~~ x : (("p" | "k") ~~ DigitCharacter ..) ~~ "]" :>
				"DiracGamma[Momentum[" <> x <> ",D],D]",
				
			"GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>
				"DiracGamma[LorentzIndex[" <> x <> ",D],D]"
			}
		],
		"Text"
	];
	
	Export[
		libraryFilePath,
		StringReplace[
			Import[libraryFilePath, "Text"],
				"g_(0," ~~ x : (WordCharacter .. ~~ ("," ~~ WordCharacter ..) ..) ~~ ")" :>
					"(" <>
						StringRiffle[
							(
								StringReplace[
									#,
									{
										y : (("p" | "k") ~~ DigitCharacter ..) :>
											"DiracGamma[Momentum[" <> y <> ",D],D]",
											
										y : (WordCharacter ..) :>
											"DiracGamma[LorentzIndex[" <> y <> ",D],D]"
									}
								] &
							) /@ StringSplit[x, ","],
						"."
					] <>
				")"
			],
		"Text"
	];
	
	Print[
		"Fermion vertex library generated for n = ",
		n,
		"."
	];
];


GenerateGravitonVectors[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonVectorsSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonVectorsSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
(* Proca field *)
	filePath = "GravitonMassiveVectorVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,GravitonMassiveVectorVertexUncontracted[DummyArray[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
		
	Print["Done for the Proca field for order n="<>ToString[n]<>"."];
	
(* Maxwell field *)
	filePath = "GravitonVectorVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,GravitonVectorVertex[DummyArrayMomentaK[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`GaugeFixingEpsilonVector]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
		
	Print["Done for the Maxwell field for order n="<>ToString[n]<>"."];
	
(* Ghost *)
	filePath = "GravitonVectorGhostVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,GravitonVectorGhostVertex[DummyArray[n],Global`p1,Global`p2]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
		
	Print["Done for the Maxwell-ghost for order n="<>ToString[n]<>"."];
];


GenerateGravitonVertex[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonVertexSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonVertexSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
(* Gravitons *)
	filePath = "GravitonVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,GravitonVertexUncontracted[DummyArrayMomenta[2+n]]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2+n,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
				
	Print["Done for the graviton vertex for order n="<>ToString[n]<>"."];
];


(* Procedures that generates rules for SU(N) Yang-Mills model. *)


GenerateGravitonSUNYM[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonSUNYMSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonSUNYMSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
(* Graviton-Quark-Gluon vertex *)
	filePath = "GravitonQuarkGluonVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonQuarkGluonVertexUncontracted[DummyArray[n],{Global`\[Lambda],Global`a}] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SUNIndex[a]","syFC2"->"SUNT[SUNIndex[a]]"}], "Text"];

	Print["Graviton-quark-gluon vertex is generated for n="<>ToString[n]<>"."];

(* Graviton-Gluon-Gluon vertex *)
	filePath = "GravitonGluonVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonGluonVertexUncontracted[DummyArrayMomentaK[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`GaugeFixingEpsilonSUNYM] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SUNDelta[SUNIndex[a1],SUNIndex[a2]]","syFC2"->"SUNIndex[a1]","syFC3"->"SUNIndex[a2]"}], "Text"];

	Print["Graviton-gluon vertex is generated for n="<>ToString[n]<>"."];
		
(* Graviton-Gluon-Gluon-Gluon vertex *)
	
	filePath = "GravitonThreeGluonVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonThreeGluonVertex[DummyArray[n],p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,3,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SMP[\"g_s\"]","syFC2"->"SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]]","syFC3"->"SUNIndex[a1]","syFC4"->"SUNIndex[a2]","syFC5"->"SUNIndex[a3]"}], "Text"];

	Print["Graviton-gluon-gluon-gluon vertex is generated for n="<>ToString[n]<>"."];

(* Graviton-Gluon-Gluon-Gluon-Gluon vertex *)
	filePath = "GravitonFourGluonVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonFourGluonVertexUncontracted[DummyArray[n],p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,4,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SMP[\"g_s\"]","syFC2"->"SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[s]]","syFC3"->"SUNF[SUNIndex[a1],SUNIndex[a3],SUNIndex[s]]","syFC4"->"SUNF[SUNIndex[a1],SUNIndex[a4],SUNIndex[s]]","syFC5"->"SUNF[SUNIndex[a2],SUNIndex[a3],SUNIndex[s]]","syFC6"->"SUNF[SUNIndex[a2],SUNIndex[a4],SUNIndex[s]]","syFC7"->"SUNF[SUNIndex[a3],SUNIndex[a4],SUNIndex[s]]","syFC8"->"SUNIndex[a1]","syFC9"->"SUNIndex[a2]","syFC10"->"SUNIndex[a3]","syFC11"->"SUNIndex[a4]","syFC12"->"SUNIndex[s]"} ], "Text"];

	Print["Graviton-gluon-gluon-gluon-gluon vertex is generated for n="<>ToString[n]<>"."];

(* Gravtion-Yang-Mills-Ghost vertex *)	
	filePath = "GravitonYMGhostVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonYMGhostVertexUncontracted[DummyArray[n],p1,a1,p2,a2] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SUNDelta[SUNIndex[a1],SUNIndex[a2]]","syFC2"->"SUNIndex[a1]","syFC3"->"SUNIndex[a2]"} ], "Text"];

	Print["Graviton-YM ghost vertex is generated for n="<>ToString[n]<>"."];

(* Graviton-Gluon-Ghost vertex *)
	filePath = "GravitonGluonGhostVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, GravitonGluonGhostVertexUncontracted[DummyArray[n],{p1,\[Lambda]1,a1},{p2,\[Lambda]2,a2},{p3,\[Lambda]3,a3}] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,3,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (Except[")"] ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], {"syFC1"->"SMP[\"g_s\"]","syFC2"->"SUNF[SUNIndex[p1],SUNIndex[p2],SUNIndex[p3]]","syFC3"->"SUNIndex[p1]","syFC4"->"SUNIndex[p2]","syFC5"->"SUNIndex[p3]"} ], "Text"];

	Print["Graviton-gluon-YM ghost vertex is generated for n="<>ToString[n]<>"."];

];


(* Procedures that generates rules for Horndeski G2 interaction. *)


GenerateHorndeskiG2[numberOfScalars_,n_] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Apply[GenerateHorndeskiG2Specific , Select[ Tuples[{Range[0,numberOfScalars],Range[1,Ceiling[numberOfScalars/2]],Range[n]}], (3<=#[[1]]+2#[[2]]<=numberOfScalars)&] , 1] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateHorndeskiG2Specific[a_,b_,n_] := Module[{filePath,theTimingVariable},
	filePath = "HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]<>".frm";
	
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath, HorndeskiG2Uncontracted[DummyArray[n],DummyMomenta[a + 2 b ],b] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath , a + 2 b , 0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
					
	Print["Done for the Horndeski G2 vertex with a=",a,", b="<>ToString[b]<>" for order n="<>ToString[n]<>"."];
];


(* Procedures that generates rules for Horndeski G3 interaction. *)


GenerateHorndeskiG3[numberOfScalars_,n_] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Apply[GenerateHorndeskiG3Specific , Select[ Tuples[{Range[0,numberOfScalars],Range[0,Ceiling[numberOfScalars/2]],Range[n]}], (3<=#[[1]]+2#[[2]]+1<=numberOfScalars)&] , 1] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateHorndeskiG3Specific[a_,b_,n_] := Module[{filePath,theTimingVariable},
	filePath = "HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath, HorndeskiG3Uncontracted[DummyArrayMomentaK[n],DummyMomenta[ a + 2 b + 1 ],b] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath, a + 2 b + 1 , n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
						
	Print["Done for the Horndeski G3 vertex with a=",a,", b="<>ToString[b]<>" for order n="<>ToString[n]<>"."];
];


GenerateHorndeskiG4[numberOfScalars_,n_] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Apply[GenerateHorndeskiG4Specific , Select[ Tuples[{Range[0,numberOfScalars],Range[0,Ceiling[numberOfScalars/2]],Range[n]}], (2<=#[[1]]+2#[[2]]<=numberOfScalars)&] , 1] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateHorndeskiG4Specific[a_,b_,n_] := Module[{filePath,theTimingVariable},
	filePath = "HorndeskiG4_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath, HorndeskiG4Uncontracted[DummyArrayMomentaK[n],DummyMomenta[a+2b],b] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,a+2b,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
		
	Print["Done for the Horndeski G4 vertex with a=",a,", b="<>ToString[b]<>" for order n="<>ToString[n]<>"."];
];


GenerateHorndeskiG5[numberOfScalars_,n_] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Apply[GenerateHorndeskiG5Specific , Select[ Tuples[{Range[0,numberOfScalars],Range[0,Ceiling[numberOfScalars/2]],Range[n]}], (3<=#[[1]]+2#[[2]]+1<=numberOfScalars)&] , 1] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateHorndeskiG5Specific[a_,b_,n_] := Module[{filePath,theTimingVariable},
	filePath = "HorndeskiG5_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing [ FeynCalc2FORM[filePath, HorndeskiG5Uncontracted[DummyArrayMomentaK[n],DummyMomenta[a+2b+1],b] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,a+2b+1,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
		
	Print["Done for the Horndeski G5 vertex with a=",a,", b="<>ToString[b]<>" for order n="<>ToString[n]<>"."];
];


(* Scalar-Gauss-Bonnet *)


GenerateScalarGaussBonnet[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateScalarGaussBonnetSpecific , Range[2,n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateScalarGaussBonnetSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
	filePath = "ScalarGaussBonnet_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, ScalarGaussBonnet[DummyArrayMomentaK[n]] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,0,n];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
	Export[filePath, StringReplace[Import[filePath, "Text"], "g_(0," ~~ x : (WordCharacter ..) ~~ ")" :>  "GAD[" <> x <> "]"], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"],{"GAD[p1]"->"DiracGamma[Momentum[p1,D],D]","GAD[p2]"->"DiracGamma[Momentum[p2,D],D]"}], "Text"];
	Export[filePath, StringReplace[Import[filePath, "Text"], "GAD[" ~~ x : (WordCharacter ..) ~~ "]" :>  "DiracGamma[LorentzIndex[" <> x <> ",D],D]"], "Text"];

	Print["Scalar-GaussBonnet vertices is generated for n="<>ToString[n]<>"."];
];


(* Procedures that generates rules for the simplest axion-like interaction. *)


GenerateGravitonAxionVector[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateGravitonAxionVectorSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateGravitonAxionVectorSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
	
	filePath = "GravitonAxionVectorVertex_"<>ToString[n]<>".frm";
		
	(* Check if the FROM code file exists and is empty. *)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
	(* Check if the corresponding library exists and delete it if it does. *)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(* FeynCalc converts the expression to FORM and writes it to the file. *)
	theTimingVariable = Timing[ FeynCalc2FORM[ filePath, (I) GravitonAxionVectorVertexUncontracted[DummyArray[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`\[CapitalTheta]] ] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];

	Print["Done for graviton-axion-like coupling of order n="<>ToString[n]<>"."];
];


GenerateQuadraticGravityVertex[n_Integer?Positive] := Module[{theTimingVariable},
	theTimingVariable = Timing[ Map[GenerateQuadraticGravityVertexSpecific , Range[n]] ][[1]];
	Print["The computational time is ",ToString[theTimingVariable]," seconds."];
];


GenerateQuadraticGravityVertexSpecific[n_Integer?Positive] := Module[{filePath,theTimingVariable},
(* Gravitons *)
	filePath = "QuadraticGravityVertex_"<>ToString[n]<>".frm";
		
	(*Check if the FROM code file is exists and empty.*)
	If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
	(*Check if the corresponding library exists and delete it if it does*)
	If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
	(*Writing the expression of the FORM file*)
	theTimingVariable = Timing[ FeynCalc2FORM[filePath,QuadraticGravityVertex[DummyArrayMomenta[2+n],\[GothicM]0,\[GothicM]2]] ][[1]];
	Print["The expression is generated in ",theTimingVariable," seconds."];
		
	(* I modify the FORM file so that it can be executed. *)
	FORMCodeCleanUp[filePath,2+n,0];
		
	(*Run the FORM*)
	theTimingVariable = Timing[ Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]] ][[1]];
	Print["FORM calculated the expression in ",theTimingVariable," seconds."];
	DeleteFile[filePath];
	filePath = StringDrop[filePath, -4];
		
	(*Clean the output*)
	FORMOutputCleanUp[filePath];
				
	Print["Done for the quadratic gravity vertex for order n="<>ToString[n]<>"."];
];


End[];


EndPackage[];
