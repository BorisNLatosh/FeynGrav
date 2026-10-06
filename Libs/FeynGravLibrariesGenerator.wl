(* ::Package:: *)

(* ::Title:: *)
(*FeynGrav library generation*)

(* Generate extensionless Wolfram-expression libraries through CalcFormConverter.
   Loading preserves Directory[] and starts no external processes. Existing
   libraries are replaced only after calculation and a checked serialisation.
   See Generator.md for options, conventions and recovery instructions. *)

With[{root = DirectoryName[DirectoryName[$InputFileName]]},
    Block[{$ContextPath = $ContextPath},
        Needs["CalcFormConverter`", FileNameJoin[{root, "CalcFormConverter", "CalcFormConverter.wl"}]];
        Scan[Needs[#[[1]], FileNameJoin[{root, "Rules", #[[2]]}]] &,
            {{"GravitonScalarVertex`", "GravitonScalarVertex.wl"},
                {"GravitonFermionVertex`", "GravitonFermionVertex.wl"},
                {"GravitonVectorVertex`", "GravitonVectorVertex.wl"},
                {"GravitonSUNYM`", "GravitonSUNYM.wl"},
                {"GravitonVertex`", "GravitonVertex.wl"},
                {"HorndeskiG2`", "HorndeskiG2.wl"},
                {"HorndeskiG3`", "HorndeskiG3.wl"},
                {"HorndeskiG4`", "HorndeskiG4.wl"},
                {"HorndeskiG5`", "HorndeskiG5.wl"},
                {"ScalarGaussBonnet`", "ScalarGaussBonnet.wl"},
                {"GravitonAxionVectorVertex`", "GravitonAxionVectorVertex.wl"},
                {"QuadraticGravityVertex`", "QuadraticGravityVertex.wl"}}];
    ];
];

BeginPackage["FeynGravLibrariesGenerator`", {"FeynCalc`", "CalcFormConverter`"}];

(* Clear old, more-specific dispatch definitions on reload without touching
   configuration values. Names are strings so legacy Check own-values cannot run. *)
Clear[
    "FeynGravLibrariesGenerator`FeynGravLibrariesGeneratorFORMInformation",
    "FeynGravLibrariesGenerator`FeynGravLibrariesGeneratorPrintFORMStatus",
    "FeynGravLibrariesGenerator`GenerateGravitonScalarsSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonScalars",
    "FeynGravLibrariesGenerator`CheckGravitonScalars",
    "FeynGravLibrariesGenerator`GenerateGravitonFermionsSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonFermions",
    "FeynGravLibrariesGenerator`CheckGravitonFermions",
    "FeynGravLibrariesGenerator`GenerateGravitonVectorsSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonVectors",
    "FeynGravLibrariesGenerator`CheckGravitonVectors",
    "FeynGravLibrariesGenerator`GenerateGravitonVertexSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonVertex",
    "FeynGravLibrariesGenerator`CheckGravitonVertex",
    "FeynGravLibrariesGenerator`GenerateGravitonSUNYMSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonSUNYM",
    "FeynGravLibrariesGenerator`CheckGravitonSUNYM",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG2Specific",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG2",
    "FeynGravLibrariesGenerator`CheckHorndeskiG2",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG3Specific",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG3",
    "FeynGravLibrariesGenerator`CheckHorndeskiG3",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG4Specific",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG4",
    "FeynGravLibrariesGenerator`CheckHorndeskiG4",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG5Specific",
    "FeynGravLibrariesGenerator`GenerateHorndeskiG5",
    "FeynGravLibrariesGenerator`CheckHorndeskiG5",
    "FeynGravLibrariesGenerator`GenerateScalarGaussBonnetSpecific",
    "FeynGravLibrariesGenerator`GenerateScalarGaussBonnet",
    "FeynGravLibrariesGenerator`CheckScalarGaussBonnet",
    "FeynGravLibrariesGenerator`GenerateGravitonAxionVectorSpecific",
    "FeynGravLibrariesGenerator`GenerateGravitonAxionVector",
    "FeynGravLibrariesGenerator`CheckGravitonAxionVector",
    "FeynGravLibrariesGenerator`GenerateQuadraticGravityVertexSpecific",
    "FeynGravLibrariesGenerator`GenerateQuadraticGravityVertex",
    "FeynGravLibrariesGenerator`CheckQuadraticGravityVertex"];
Clear["FeynGravLibrariesGenerator`Private`FORMCodeCleanUp",
    "FeynGravLibrariesGenerator`Private`FORMOutputCleanUp",
    "FeynGravLibrariesGenerator`FeynGravLibrariesGeneratorParseFORMVersion"];

(* ::Section:: *)
(*Public commands and options*)

OutputDirectory::usage =
    "OutputDirectory is an option for Generate* and Generate*Specific. Automatic (the default) writes extensionless libraries to the Libs directory containing this generator, independently of Directory[]. An explicit value must be an existing directory. WorkingDirectory instead controls the parent directory of temporary converter jobs. Check* commands always inspect the generator's Libs directory.";

FeynGravLibrariesGeneratorFORMInformation::usage =
    "FeynGravLibrariesGeneratorFORMInformation[] calls CalcFormCheck and returns its availability and diagnostic association. It honours an existing legacy executable setting; otherwise it uses automatic FORM/TFORM selection. FeynGravLibrariesGeneratorFORMInformation[executable_String] checks that executable with one worker. For worker counts or other check options, call CalcFormCheck directly. No availability check or installation runs when the generator is loaded.";

FeynGravLibrariesGeneratorPrintFORMStatus::usage =
    "FeynGravLibrariesGeneratorPrintFORMStatus[] calls FeynGravLibrariesGeneratorFORMInformation[], prints the availability and diagnostic association, and returns it. This is an explicit check; it does not install software.";

(* Legacy executable and startup settings are looked up by name. Their help
   is in Generator.md; declaring duplicate symbols would cause shadowing. *)
$FeynGravLibrariesGeneratorFORMCheck::usage =
    "Deprecated startup-check setting. Its value is ignored: loading the generator does not check or launch FORM. Use CalcFormCheck[] or FeynGravLibrariesGeneratorFORMInformation[] for an explicit check.";

(* Shared help is private, but its full text is appended to every generation
   command so ?Function remains self-contained. Keep option defaults in step
   with $generationOptions below. *)
FeynGravLibrariesGenerator`Private`$generationUsage =
"\n\nCalculation uses CalcFormCalculate with automatic dimension inference. Options and defaults: OutputDirectory -> Automatic (this generator's Libs directory), FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> Infinity, WorkingDirectory -> Automatic, KeepFiles -> False, ShowTiming -> False, ShowProgress -> False, DiracAlgebra -> Automatic, ColourAlgebra -> True. Automatic workers prefer TFORM with up to eight workers and fall back to serial FORM only when TFORM is absent. TimeConstraint limits FORM execution, not rule construction or total elapsed time. ColourAlgebra -> Automatic is an alias for True; False preserves colour structures. DiracAlgebra -> False disables optional Dirac processing.\n\nHonours SetOptions on the invoked command and individual or nested option lists; the first explicit occurrence wins. Explicit FORMExecutable overrides the command default and legacy setting; an Automatic command default may use that legacy setting. Batches use their own defaults and stop at the first failure.\n\nSuccess returns Null. Invalid arguments, assigned formal placeholders or failed stages return Failure; user definitions are preserved. CompletedFiles lists installed batch members, including a file whose installation succeeded before backup cleanup failed. Existing libraries are replaced only after calculation and a checked read-back. Converter failures retain their job diagnostics. Loading or generating never installs FORM. See Libs/Generator.md for the full workflow.";

GenerateGravitonScalarsSpecific::usage =
    "GenerateGravitonScalarsSpecific[n, opts] generates the scalar kinetic and scalar potential interaction libraries for exactly n external gravitons: GravitonScalarVertex_n and GravitonScalarPotentialVertex_n. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonScalars::usage =
    "GenerateGravitonScalars[n, opts] generates the scalar kinetic and scalar potential interaction libraries for every graviton order from 1 through n, calling the same family builders as GenerateGravitonScalarsSpecific. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonScalars::usage =
    "CheckGravitonScalars (without brackets) prints canonical filenames for the scalar kinetic and scalar potential interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.";

GenerateGravitonFermionsSpecific::usage =
    "GenerateGravitonFermionsSpecific[n, opts] generates the Dirac-fermion interaction libraries for exactly n external gravitons: GravitonFermionVertex_n. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonFermions::usage =
    "GenerateGravitonFermions[n, opts] generates the Dirac-fermion interaction libraries for every graviton order from 1 through n, calling the same family builders as GenerateGravitonFermionsSpecific. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonFermions::usage =
    "CheckGravitonFermions (without brackets) prints canonical filenames for the Dirac-fermion interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.";

GenerateGravitonVectorsSpecific::usage =
    "GenerateGravitonVectorsSpecific[n, opts] generates the massive-vector, massless-vector and vector-ghost interaction libraries for exactly n external gravitons: GravitonMassiveVectorVertex_n, GravitonVectorVertex_n and GravitonVectorGhostVertex_n. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonVectors::usage =
    "GenerateGravitonVectors[n, opts] generates the massive-vector, massless-vector and vector-ghost interaction libraries for every graviton order from 1 through n, calling the same family builders as GenerateGravitonVectorsSpecific. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonVectors::usage =
    "CheckGravitonVectors (without brackets) prints canonical filenames for the massive-vector, massless-vector and vector-ghost interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.";

GenerateGravitonSUNYMSpecific::usage =
    "GenerateGravitonSUNYMSpecific[n, opts] generates the SU(N) Yang-Mills interaction libraries for exactly n external gravitons: GravitonQuarkGluonVertex_n, GravitonGluonVertex_n, GravitonThreeGluonVertex_n, GravitonFourGluonVertex_n, GravitonYMGhostVertex_n and GravitonGluonGhostVertex_n. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonSUNYM::usage =
    "GenerateGravitonSUNYM[n, opts] generates the SU(N) Yang-Mills interaction libraries for every graviton order from 1 through n, calling the same family builders as GenerateGravitonSUNYMSpecific. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonSUNYM::usage =
    "CheckGravitonSUNYM (without brackets) prints canonical filenames for the SU(N) Yang-Mills interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.";

GenerateGravitonAxionVectorSpecific::usage =
    "GenerateGravitonAxionVectorSpecific[n, opts] generates the scalar-axion–vector interaction libraries for exactly n external gravitons: GravitonAxionVectorVertex_n. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonAxionVector::usage =
    "GenerateGravitonAxionVector[n, opts] generates the scalar-axion–vector interaction libraries for every graviton order from 1 through n, calling the same family builders as GenerateGravitonAxionVectorSpecific. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonAxionVector::usage =
    "CheckGravitonAxionVector (without brackets) prints canonical filenames for the scalar-axion–vector interaction libraries in the generator's Libs directory and returns Null. It launches no FORM process and does not validate file contents. Directories, malformed names, staging files and backups are excluded; OutputDirectory does not change this search.";

GenerateGravitonVertexSpecific::usage =
    "GenerateGravitonVertexSpecific[n, opts] generates GravitonVertex_n for the general-relativity graviton vertex with n + 2 external gravitons. n is the library order, not the number of graviton legs, and must be an explicit positive integer. In particular, n = 1 generates the three-graviton vertex." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateGravitonVertex::usage =
    "GenerateGravitonVertex[n, opts] generates GravitonVertex_j for j = 1 through n, corresponding to 3 through n + 2 external gravitons. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckGravitonVertex::usage =
    "CheckGravitonVertex (without brackets) prints canonical GravitonVertex_n filenames in the generator's Libs directory and returns Null. The suffix n corresponds to n + 2 graviton legs. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateQuadraticGravityVertexSpecific::usage =
    "GenerateQuadraticGravityVertexSpecific[n, opts] generates QuadraticGravityVertex_n for the quadratic-gravity graviton vertex with n + 2 external gravitons. n is the library order, not the number of graviton legs, and must be an explicit positive integer. In particular, n = 1 generates the three-graviton vertex." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateQuadraticGravityVertex::usage =
    "GenerateQuadraticGravityVertex[n, opts] generates QuadraticGravityVertex_j for j = 1 through n, corresponding to 3 through n + 2 external gravitons. n must be an explicit positive integer." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckQuadraticGravityVertex::usage =
    "CheckQuadraticGravityVertex (without brackets) prints canonical QuadraticGravityVertex_n filenames in the generator's Libs directory and returns Null. The suffix n corresponds to n + 2 graviton legs. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateHorndeskiG2Specific::usage =
    "GenerateHorndeskiG2Specific[a, b, n, opts] generates HorndeskiG2_a_b_n from the uncontracted G2 rule, with n external gravitons and a + 2 b scalar momentum entries. a and b must be explicit non-negative integers; n must be an explicit positive integer. The underlying rule supplies any additional structural requirements. Unlike the batch command, this specific command does not impose the batch's minimum scalar-count filter." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateHorndeskiG2::usage =
    "GenerateHorndeskiG2[numberOfScalars, n, opts] generates selected G2 libraries for graviton orders 1 through n. numberOfScalars is an explicit non-negative integer upper bound on the number of scalar momentum entries; n is an explicit positive integer. It enumerates a = 0 through numberOfScalars and b = 1 through Ceiling[numberOfScalars/2], retaining only 3 <= a + 2 b <= numberOfScalars. An empty selection returns Null without calculation or files. Use GenerateHorndeskiG2Specific[a, b, n, opts] for one parameter triple." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckHorndeskiG2::usage =
    "CheckHorndeskiG2 (without brackets) prints canonical HorndeskiG2_a_b_n filenames in the generator's Libs directory and returns Null. The scalar momentum count is a + 2 b; n is the number of external gravitons. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateHorndeskiG3Specific::usage =
    "GenerateHorndeskiG3Specific[a, b, n, opts] generates HorndeskiG3_a_b_n from the uncontracted G3 rule, with n external gravitons and a + 2 b + 1 scalar momentum entries. a and b must be explicit non-negative integers; n must be an explicit positive integer. The underlying rule supplies any additional structural requirements. Unlike the batch command, this specific command does not impose the batch's minimum scalar-count filter." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateHorndeskiG3::usage =
    "GenerateHorndeskiG3[numberOfScalars, n, opts] generates selected G3 libraries for graviton orders 1 through n. numberOfScalars is an explicit non-negative integer upper bound on the number of scalar momentum entries; n is an explicit positive integer. It enumerates a = 0 through numberOfScalars and b = 0 through Ceiling[numberOfScalars/2], retaining only 3 <= a + 2 b + 1 <= numberOfScalars. An empty selection returns Null without calculation or files. Use GenerateHorndeskiG3Specific[a, b, n, opts] for one parameter triple." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckHorndeskiG3::usage =
    "CheckHorndeskiG3 (without brackets) prints canonical HorndeskiG3_a_b_n filenames in the generator's Libs directory and returns Null. The scalar momentum count is a + 2 b + 1; n is the number of external gravitons. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateHorndeskiG4Specific::usage =
    "GenerateHorndeskiG4Specific[a, b, n, opts] generates HorndeskiG4_a_b_n from the uncontracted G4 rule, with n external gravitons and a + 2 b scalar momentum entries. a and b must be explicit non-negative integers; n must be an explicit positive integer. The underlying rule supplies any additional structural requirements. Unlike the batch command, this specific command does not impose the batch's minimum scalar-count filter." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateHorndeskiG4::usage =
    "GenerateHorndeskiG4[numberOfScalars, n, opts] generates selected G4 libraries for graviton orders 1 through n. numberOfScalars is an explicit non-negative integer upper bound on the number of scalar momentum entries; n is an explicit positive integer. It enumerates a = 0 through numberOfScalars and b = 0 through Ceiling[numberOfScalars/2], retaining only 2 <= a + 2 b <= numberOfScalars. An empty selection returns Null without calculation or files. Use GenerateHorndeskiG4Specific[a, b, n, opts] for one parameter triple." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckHorndeskiG4::usage =
    "CheckHorndeskiG4 (without brackets) prints canonical HorndeskiG4_a_b_n filenames in the generator's Libs directory and returns Null. The scalar momentum count is a + 2 b; n is the number of external gravitons. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateHorndeskiG5Specific::usage =
    "GenerateHorndeskiG5Specific[a, b, n, opts] generates HorndeskiG5_a_b_n from the uncontracted G5 rule, with n external gravitons and a + 2 b + 1 scalar momentum entries. a and b must be explicit non-negative integers; n must be an explicit positive integer. The underlying rule supplies any additional structural requirements. Unlike the batch command, this specific command does not impose the batch's minimum scalar-count filter." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateHorndeskiG5::usage =
    "GenerateHorndeskiG5[numberOfScalars, n, opts] generates selected G5 libraries for graviton orders 1 through n. numberOfScalars is an explicit non-negative integer upper bound on the number of scalar momentum entries; n is an explicit positive integer. It enumerates a = 0 through numberOfScalars and b = 0 through Ceiling[numberOfScalars/2], retaining only 3 <= a + 2 b + 1 <= numberOfScalars. An empty selection returns Null without calculation or files. Use GenerateHorndeskiG5Specific[a, b, n, opts] for one parameter triple." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckHorndeskiG5::usage =
    "CheckHorndeskiG5 (without brackets) prints canonical HorndeskiG5_a_b_n filenames in the generator's Libs directory and returns Null. The scalar momentum count is a + 2 b + 1; n is the number of external gravitons. It excludes directories and malformed filenames, does not validate file contents and launches no FORM process.";

GenerateScalarGaussBonnetSpecific::usage =
    "GenerateScalarGaussBonnetSpecific[n, opts] generates ScalarGaussBonnet_n for exactly n external gravitons. n must be an explicit integer at least 2. Around flat space the curvature-squared interaction starts at second order, so the one-graviton contribution vanishes. This specific command requires at least two graviton triples and returns Failure for n = 1; it does not create a zero library." <> FeynGravLibrariesGenerator`Private`$generationUsage;

GenerateScalarGaussBonnet::usage =
    "GenerateScalarGaussBonnet[n, opts] generates scalar–Gauss–Bonnet libraries for orders 2 through n. n must be an explicit positive integer. The flat-background curvature-squared interaction has no one-graviton contribution: for n = 1 the batch returns Null without constructing rules, launching FORM or writing libraries. Use GenerateScalarGaussBonnetSpecific[n, opts] for a single order n >= 2." <> FeynGravLibrariesGenerator`Private`$generationUsage;

CheckScalarGaussBonnet::usage =
    "CheckScalarGaussBonnet (without brackets) prints canonical ScalarGaussBonnet_n filenames in the generator's Libs directory and returns Null. Generated interaction libraries start at n = 2. This is a filename inventory, not a check of file contents or the physical order; it launches no FORM process and ignores OutputDirectory.";


Begin["`Private`"];
$libraryDirectory = DirectoryName[$InputFileName];
$ruleContexts = {"GravitonScalarVertex`Private`","GravitonFermionVertex`Private`","GravitonVectorVertex`Private`","GravitonSUNYM`Private`","GravitonVertex`Private`","HorndeskiG2`Private`","HorndeskiG3`Private`","HorndeskiG4`Private`","HorndeskiG5`Private`","ScalarGaussBonnet`Private`","GravitonAxionVectorVertex`Private`","QuadraticGravityVertex`Private`","CETensor`Private`","CTensorGeneral`Private`","ETensor`Private`","GammaTensor`Private`","ITensor`Private`","MTDWrapper`Private`","indexArraySymmetrization`Private`"};

$generationOptions = {
    OutputDirectory -> Automatic, FORMExecutable -> Automatic,
    FORMThreads -> Automatic, TimeConstraint -> Infinity,
    WorkingDirectory -> Automatic, KeepFiles -> False,
    ShowTiming -> False, ShowProgress -> False,
    DiracAlgebra -> Automatic, ColourAlgebra -> True
};
Options[GenerateGravitonScalarsSpecific] = $generationOptions;
Options[GenerateGravitonScalars] = $generationOptions;
Options[GenerateGravitonFermionsSpecific] = $generationOptions;
Options[GenerateGravitonFermions] = $generationOptions;
Options[GenerateGravitonVectorsSpecific] = $generationOptions;
Options[GenerateGravitonVectors] = $generationOptions;
Options[GenerateGravitonVertexSpecific] = $generationOptions;
Options[GenerateGravitonVertex] = $generationOptions;
Options[GenerateGravitonSUNYMSpecific] = $generationOptions;
Options[GenerateGravitonSUNYM] = $generationOptions;
Options[GenerateHorndeskiG2Specific] = $generationOptions;
Options[GenerateHorndeskiG2] = $generationOptions;
Options[GenerateHorndeskiG3Specific] = $generationOptions;
Options[GenerateHorndeskiG3] = $generationOptions;
Options[GenerateHorndeskiG4Specific] = $generationOptions;
Options[GenerateHorndeskiG4] = $generationOptions;
Options[GenerateHorndeskiG5Specific] = $generationOptions;
Options[GenerateHorndeskiG5] = $generationOptions;
Options[GenerateScalarGaussBonnetSpecific] = $generationOptions;
Options[GenerateScalarGaussBonnet] = $generationOptions;
Options[GenerateGravitonAxionVectorSpecific] = $generationOptions;
Options[GenerateGravitonAxionVector] = $generationOptions;
Options[GenerateQuadraticGravityVertexSpecific] = $generationOptions;
Options[GenerateQuadraticGravityVertex] = $generationOptions;

(* ::Section:: *)
(*Library specifications and formal symbols*)

(* These placeholders have a dedicated context. Only declared placeholders and
   the rule coupling kappa are mapped into the library reader's context. *)
parameter[name_String] := Symbol["FeynGravLibrariesGenerator`Parameters`" <> name];
DummyArray[n_] := Flatten[Table[{parameter["m"<>ToString[i]], parameter["n"<>ToString[i]]}, {i,n}]];
DummyMomenta[n_] := Table[parameter["p"<>ToString[i]], {i,n}];
DummyArrayMomenta[n_] := Flatten[Table[{parameter["m"<>ToString[i]], parameter["n"<>ToString[i]], parameter["p"<>ToString[i]]}, {i,n}]];
DummyArrayMomentaK[n_] := Flatten[Table[{parameter["m"<>ToString[i]], parameter["n"<>ToString[i]], parameter["k"<>ToString[i]]}, {i,n}]];

(* A held builder prevents construction until arguments/options are validated. *)
specifications["GenerateGravitonScalarsSpecific", {n_}] := {
    <|"Family" -> "GravitonScalarVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonScalarVertex`GravitonScalarVertexUncontracted[DummyArray[n],parameter["p1"],parameter["p2"],parameter["m"]]]|>,
    <|"Family" -> "GravitonScalarPotentialVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonScalarVertex`GravitonScalarPotentialVertexUncontracted[DummyArray[n],parameter["\[Lambda]"]]]|>
};

specifications["GenerateGravitonFermionsSpecific", {n_}] := {
    <|"Family" -> "GravitonFermionVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonFermionVertex`GravitonFermionVertexUncontracted[DummyArrayMomentaK[n],parameter["p1"],parameter["p2"],parameter["m"]]]|>
};

specifications["GenerateGravitonVectorsSpecific", {n_}] := {
    <|"Family" -> "GravitonMassiveVectorVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonVectorVertex`GravitonMassiveVectorVertexUncontracted[DummyArray[n],parameter["\[Lambda]1"],parameter["p1"],parameter["\[Lambda]2"],parameter["p2"],parameter["m"]]]|>,
    <|"Family" -> "GravitonVectorVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonVectorVertex`GravitonVectorVertex[DummyArrayMomentaK[n],parameter["\[Lambda]1"],parameter["p1"],parameter["\[Lambda]2"],parameter["p2"],parameter["GaugeFixingEpsilonVector"]]]|>,
    <|"Family" -> "GravitonVectorGhostVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonVectorVertex`GravitonVectorGhostVertex[DummyArray[n],parameter["p1"],parameter["p2"]]]|>
};

specifications["GenerateGravitonVertexSpecific", {n_}] := {
    <|"Family" -> "GravitonVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonVertex`GravitonVertexUncontracted[DummyArrayMomenta[2+n]]]|>
};

specifications["GenerateGravitonSUNYMSpecific", {n_}] := {
    <|"Family" -> "GravitonQuarkGluonVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonQuarkGluonVertexUncontracted[DummyArray[n],{parameter["\[Lambda]"],parameter["a"]}]]|>,
    <|"Family" -> "GravitonGluonVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonGluonVertexUncontracted[DummyArrayMomentaK[n],parameter["p1"],parameter["\[Lambda]1"],parameter["a1"],parameter["p2"],parameter["\[Lambda]2"],parameter["a2"],parameter["GaugeFixingEpsilonSUNYM"]]]|>,
    <|"Family" -> "GravitonThreeGluonVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonThreeGluonVertex[DummyArray[n],parameter["p1"],parameter["\[Lambda]1"],parameter["a1"],parameter["p2"],parameter["\[Lambda]2"],parameter["a2"],parameter["p3"],parameter["\[Lambda]3"],parameter["a3"]]]|>,
    <|"Family" -> "GravitonFourGluonVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonFourGluonVertexUncontracted[DummyArray[n],parameter["p1"],parameter["\[Lambda]1"],parameter["a1"],parameter["p2"],parameter["\[Lambda]2"],parameter["a2"],parameter["p3"],parameter["\[Lambda]3"],parameter["a3"],parameter["p4"],parameter["\[Lambda]4"],parameter["a4"]]]|>,
    <|"Family" -> "GravitonYMGhostVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonYMGhostVertexUncontracted[DummyArray[n],parameter["p1"],parameter["a1"],parameter["p2"],parameter["a2"]]]|>,
    <|"Family" -> "GravitonGluonGhostVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonSUNYM`GravitonGluonGhostVertexUncontracted[DummyArray[n],{parameter["p1"],parameter["\[Lambda]1"],parameter["a1"]},{parameter["p2"],parameter["\[Lambda]2"],parameter["a2"]},{parameter["p3"],parameter["\[Lambda]3"],parameter["a3"]}]]|>
};

specifications["GenerateHorndeskiG2Specific", {a_, b_, n_}] := {
    <|"Family" -> "HorndeskiG2", "Parameters" -> {a,b,n}, "Builder" -> HoldComplete[HorndeskiG2`HorndeskiG2Uncontracted[DummyArray[n],DummyMomenta[a + 2 b ],b]]|>
};

specifications["GenerateHorndeskiG3Specific", {a_, b_, n_}] := {
    <|"Family" -> "HorndeskiG3", "Parameters" -> {a,b,n}, "Builder" -> HoldComplete[HorndeskiG3`HorndeskiG3Uncontracted[DummyArrayMomentaK[n],DummyMomenta[ a + 2 b + 1 ],b]]|>
};

specifications["GenerateHorndeskiG4Specific", {a_, b_, n_}] := {
    <|"Family" -> "HorndeskiG4", "Parameters" -> {a,b,n}, "Builder" -> HoldComplete[HorndeskiG4`HorndeskiG4Uncontracted[DummyArrayMomentaK[n],DummyMomenta[a+2b],b]]|>
};

specifications["GenerateHorndeskiG5Specific", {a_, b_, n_}] := {
    <|"Family" -> "HorndeskiG5", "Parameters" -> {a,b,n}, "Builder" -> HoldComplete[HorndeskiG5`HorndeskiG5Uncontracted[DummyArrayMomentaK[n],DummyMomenta[a+2b+1],b]]|>
};

(* Flat-background curvature squared has no one-graviton contribution.
   The rule and batch start at two triples; a batch requested through order one
   is empty and returns Null without evaluating the rule. *)
specifications["GenerateScalarGaussBonnetSpecific", {n_}] := {
    <|"Family" -> "ScalarGaussBonnet", "Parameters" -> {n}, "Builder" -> HoldComplete[ScalarGaussBonnet`ScalarGaussBonnet[DummyArrayMomentaK[n]]]|>
};

specifications["GenerateGravitonAxionVectorSpecific", {n_}] := {
    <|"Family" -> "GravitonAxionVectorVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[GravitonAxionVectorVertex`GravitonAxionVectorVertexUncontracted[DummyArray[n],parameter["\[Lambda]1"],parameter["p1"],parameter["\[Lambda]2"],parameter["p2"],parameter["\[CapitalTheta]"]]]|>
};

specifications["GenerateQuadraticGravityVertexSpecific", {n_}] := {
    <|"Family" -> "QuadraticGravityVertex", "Parameters" -> {n}, "Builder" -> HoldComplete[QuadraticGravityVertex`QuadraticGravityVertex[DummyArrayMomenta[2+n],parameter["\[GothicM]0"],parameter["\[GothicM]2"]]]|>
};

(* Resolve only formal-argument constructors inside HoldComplete; the rule
   itself remains held. This is each specification's symbol contract. *)
formalSymbols[builder_HoldComplete] := DeleteDuplicates[Flatten[
    ReleaseHold /@ Cases[builder,
        x : (_DummyArray | _DummyMomenta | _DummyArrayMomenta | _DummyArrayMomentaK | _parameter) :> HoldComplete[x],
        Infinity]]];

(* ::Section:: *)
(*Validation, selection and diagnostics*)

failure[tag_, message_, data_:<||>] := Failure[tag, Join[
    <|"MessageTemplate" -> message, "Function" -> "FeynGravLibrariesGenerator`" <> $command,
      "Stage" -> $stage|>, data]];
$command = ""; $stage = "Validation";
require[value_] := Module[{bad},
    bad = FirstCase[value, f_Failure :> f, None, {0, Infinity}];
    If[bad =!= None, Throw[bad, $generationTag]];
    If[!FreeQ[value, $Aborted], Abort[]];
    If[!FreeQ[value, $Failed], Throw[failure["RuleEvaluationFailed", "An operation returned $Failed."], $generationTag]];
    value
];

(* Look up legacy Global` settings by name only when they exist. A literal
   Global` symbol in a definition would itself create a shadowing symbol. *)
existingSetting[name_String, fallback_] := If[Names[name] === {}, fallback,
    ToExpression[name, InputForm, Function[s, If[ValueQ[s], s, fallback], HoldAllComplete]]];
legacyExecutable[] := existingSetting["FeynGravLibrariesGenerator`$FeynGravFORMExecutable",
    existingSetting["Global`$FeynGravFORMExecutable", Automatic]];

(* Explicit options win, then the invoked command's current defaults. A legacy
   executable is used only when no executable option is given and that command's
   default is Automatic. Explicit Automatic therefore disables the fallback. *)
resolveOptions[rules_List, command_String:""] := Module[{options, defaults, executable, output},
    If[!AllTrue[rules, MatchQ[#, _Rule | _RuleDelayed] &] ||
       !SubsetQ[First /@ $generationOptions, First /@ rules],
        Return[failure["InvalidOption", "An unknown or malformed generator option was supplied."]]];
    defaults = If[command === "", $generationOptions,
        Options[Symbol["FeynGravLibrariesGenerator`" <> command]]];
    options = Association[Reverse[Join[rules, defaults, $generationOptions]]];
    If[!MemberQ[First /@ rules, FORMExecutable] && options[FORMExecutable] === Automatic,
        options[FORMExecutable] = legacyExecutable[]];
    executable = options[FORMExecutable];
    output = Replace[options[OutputDirectory], Automatic :> $libraryDirectory];
    If[!StringQ[output] || !DirectoryQ[output], Return[failure["InvalidOutputDirectory", "OutputDirectory must identify an existing directory."]]];
    options[OutputDirectory] = ExpandFileName[output];
    If[!(executable === Automatic || (StringQ[executable] && StringLength[executable] > 0)) ||
       !(options[FORMThreads] === Automatic || (IntegerQ[options[FORMThreads]] && options[FORMThreads] > 0)) ||
       !(options[TimeConstraint] === Infinity || (NumberQ[options[TimeConstraint]] && TrueQ[options[TimeConstraint] > 0])) ||
       !AllTrue[{options[KeepFiles], options[ShowTiming], options[ShowProgress]}, MemberQ[{True,False},#]&] ||
       !MemberQ[{Automatic,False},options[DiracAlgebra]] ||
       !MemberQ[{True,Automatic,False},options[ColourAlgebra]],
        Return[failure["InvalidOption", "Invalid executable, worker count, timeout or algebra/runtime switch."]]];
    If[options[WorkingDirectory] =!= Automatic &&
       (!StringQ[options[WorkingDirectory]] || !DirectoryQ[options[WorkingDirectory]]),
        Return[failure["InvalidOption", "WorkingDirectory must be Automatic or an existing directory."]]];
    options
];

(* Gauss-Bonnet starts at two gravitons. Other family ranges, including the
   Horndeski lower bounds on b and scalar count, retain their existing meaning. *)
requestSpecifications[command_, args_List] := Module[{specific, horndeski, count, jobs, minimum, offset, bmin},
    specific = StringEndsQ[command,"Specific"];
    horndeski = StringContainsQ[command,"Horndeski"];
    count = If[horndeski, If[specific,3,2],1];
    If[Length[args] =!= count, Return[failure["InvalidArgumentCount", "Incorrect number of positional arguments.", <|"ExpectedCount"->count,"ActualCount"->Length[args]|>]]];
    If[!AllTrue[args,IntegerQ[#] && # >= 0 &] || Last[args] < 1,
        Return[failure["InvalidParameterValue", "Counts must be non-negative integers and the graviton order must be positive."]]];
    If[specific, Return[specifications[command,args]]];
    If[!horndeski, Return[Flatten[specifications[command<>"Specific",{#}]& /@ Range[If[command === "GenerateScalarGaussBonnet",2,1],First[args]],1]]];
    bmin = If[command === "GenerateHorndeskiG2",1,0];
    minimum = If[command === "GenerateHorndeskiG4",2,3];
    offset = If[MemberQ[{"GenerateHorndeskiG3","GenerateHorndeskiG5"},command],1,0];
    jobs = Select[Tuples[{Range[0,First[args]],Range[bmin,Ceiling[First[args]/2]],Range[Last[args]]}],
        minimum <= #[[1]] + 2 #[[2]] + offset <= First[args] &];
    Flatten[specifications[command<>"Specific",#]& /@ jobs,1]
];

(* ::Section:: *)
(*Library symbol contract and serialisation*)

(* Inspect held constructors as names before creating any symbols. This catches
   assigned source placeholders before ReleaseHold can consume their values. *)
formalNames[builder_HoldComplete] := DeleteDuplicates[Flatten[
    Cases[builder, #, Infinity] & /@ {
    HoldPattern[parameter[name_String]] :> name,
    HoldPattern[DummyArray[n_]] :> Flatten[Table[{"m"<>ToString[i],"n"<>ToString[i]},{i,n}]],
    HoldPattern[DummyMomenta[n_]] :> Table["p"<>ToString[i],{i,n}],
    HoldPattern[DummyArrayMomenta[n_]] :> Flatten[Table[{"m"<>ToString[i],"n"<>ToString[i],"p"<>ToString[i]},{i,n}]],
    HoldPattern[DummyArrayMomentaK[n_]] :> Flatten[Table[{"m"<>ToString[i],"n"<>ToString[i],"k"<>ToString[i]},{i,n}]]
    }]];

symbolHasDefinitions[name_String] := Names[name] =!= {} &&
    ToExpression[name, InputForm, Function[s,
        OwnValues[s] =!= {} || DownValues[s] =!= {} || UpValues[s] =!= {} || SubValues[s] =!= {}, HoldAllComplete]];

validateFormalNames[names_List] := Module[{sources, targets, assigned},
    sources = ("FeynGravLibrariesGenerator`Parameters`" <> # &) /@ names;
    (* Public gauge parameters and kappa are explicitly localised during
       serialisation; ordinary destination placeholders must be unassigned. *)
    targets = ("FeynGrav`Private`" <> # &) /@ Complement[names,
        {"GaugeFixingEpsilonVector","GaugeFixingEpsilonSUNYM"}];
    assigned = Select[Join[sources,targets],symbolHasDefinitions];
    If[assigned === {}, True, failure["AssignedLibrarySymbol",
        "A formal library symbol has definitions. Use unassigned placeholders or a fresh kernel; no definitions were changed.",
        <|"Symbols" -> assigned|>]]
];

librarySymbol[s_Symbol] := Switch[SymbolName[Unevaluated[s]],
    "GaugeFixingEpsilonVector", Unevaluated[FeynGrav`GaugeFixingEpsilonVector],
    "GaugeFixingEpsilonSUNYM", Unevaluated[FeynGrav`GaugeFixingEpsilonSUNYM],
    _, Symbol["FeynGrav`Private`" <> SymbolName[Unevaluated[s]]]
];

(* Symbols are matched by full identity, not by textual substrings. Rule-local
   dummy indices must have disappeared during FORM contraction. Unknown symbols
   in those contexts are rejected rather than silently renamed. *)
libraryExpression[expression_, allowed_:Automatic] := Module[{symbols, mapping, value, leaked},
    symbols = DeleteDuplicates[Cases[expression, s_Symbol /;
        Context[s] === "FeynGravLibrariesGenerator`Parameters`", {0,Infinity}, Heads->True]];
    If[allowed =!= Automatic && !SubsetQ[allowed,symbols],
        Return[failure["LibrarySymbolMismatch", "An undeclared formal symbol remains in the calculated library."]]];
    value = validateFormalNames[SymbolName /@ symbols];
    If[FailureQ[value], Return[value]];
    mapping = (Rule[#,librarySymbol[#]]&) /@ symbols;
    value = expression /. Join[mapping,{Global`\[Kappa] -> FeynGrav`\[Kappa]}];
    leaked = Cases[value, s_Symbol /;
        StringStartsQ[Context[s],"FeynGravLibrariesGenerator`"] || MemberQ[$ruleContexts,Context[s]],
        {0,Infinity}, Heads->True];
    If[leaked =!= {}, Return[failure["LibrarySymbolMismatch", "A rule-local or generator symbol remains in the calculated library.",
        <|"Symbols" -> (ToString[#,InputForm]& /@ DeleteDuplicates[leaked])|>]]];
    value
];

(* Read under the exact context search path used by FeynGrav's importer. *)
readLibrary[path_] := Block[{$Context = "FeynGrav`Private`", $ContextPath = {"FeynGrav`","FeynCalc`","System`"}}, Get[path]];
writeLibrary[path_, expression_] := Block[{$Context = "System`", $ContextPath = {"System`"}}, Put[expression,path]];

(* Replacement is protected from user abort. A backup is kept until the staged
   file has been installed. If recovery fails its path is reported, never erased. *)
publishLibrary[staged_, target_] := AbortProtect[Module[{backup = target<>".backup-"<>CreateUUID[], hadOld, moved, installed, restored},
    hadOld = FileExistsQ[target];
    If[hadOld,
        moved = Check[RenameFile[target,backup],$Failed];
        If[moved === $Failed, Return[failure["LibraryPublicationFailed", "Could not preserve the existing library.", <|"Destination"->target|>]]]
    ];
    installed = Check[RenameFile[staged,target],$Failed];
    If[installed === $Failed,
        restored = If[hadOld,Check[RenameFile[backup,target],$Failed],Null];
        Return[failure["LibraryPublicationFailed", "Could not install the staged library.",
            <|"Destination"->target,"RecoveryFile"->If[hadOld && restored === $Failed,backup,None]|>]]
    ];
    If[hadOld, If[Check[DeleteFile[backup],$Failed] === $Failed,
        Return[failure["LibraryBackupCleanupFailed", "The library was installed but its backup could not be removed.", <|"Destination"->target,"RecoveryFile"->backup,"Published"->True|>]]]];
    target
]];

(* ::Section:: *)
(*One calculation and sequential batch orchestration*)

generateOne[spec_Association, options_Association] := Block[{Global`\[Kappa]}, Module[
    {target, staged = None, expression, calculated, serialised, reread, outcome, constructionTime, calculationTime, started = AbsoluteTime[]},
    target = FileNameJoin[{options[OutputDirectory], spec["Family"] <> "_" <> StringRiffle[ToString /@ spec["Parameters"],"_"]}];
    outcome = CheckAbort[Catch[
        $stage = "Validation";
        require[validateFormalNames[formalNames[spec["Builder"]]]];
        $stage = "Construction";
        (* Rules use Global`kappa. Localise its value so the library remains
           symbolic even when the caller has assigned a numerical coupling. *)
        {constructionTime,expression} = AbsoluteTiming[Block[{Global`\[Kappa]},ReleaseHold[spec["Builder"]]]];
        require[expression];
        $stage = "Calculation";
        {calculationTime,calculated} = AbsoluteTiming[CalcFormCalculate[expression,
            Sequence @@ Normal[KeyDrop[options,{OutputDirectory}]]]];
        require[calculated];
        $stage = "Serialisation";
        Block[{FeynGrav`GaugeFixingEpsilonVector,FeynGrav`GaugeFixingEpsilonSUNYM,FeynGrav`\[Kappa]},
            serialised = require[libraryExpression[calculated,formalSymbols[spec["Builder"]]]];
            staged = target<>".staging-"<>CreateUUID[];
            require[Check[writeLibrary[staged,serialised],$Failed]];
            reread = require[Check[readLibrary[staged],$Failed]];
            If[!SameQ[reread,serialised], require[failure["LibraryReadbackMismatch", "Serialised library differs from the calculated expression."]]];
        ];
        $stage = "Publication";
        require[publishLibrary[staged,target]];
        Print["Generated ",spec["Family"]," ",spec["Parameters"]," in ",ToString[Round[AbsoluteTime[]-started,0.01],InputForm]," s (wall clock)."];
        If[TrueQ[options[ShowTiming]],Print["Construction: ",constructionTime," s; CalcFormCalculate: ",calculationTime," s (wall clock)."]];
        target,
        $generationTag],
        If[StringQ[staged] && FileExistsQ[staged],DeleteFile[staged]]; Abort[]
    ];
    If[StringQ[staged] && FileExistsQ[staged],DeleteFile[staged]];
    If[FailureQ[outcome], failure["LibraryGenerationFailed",
        If[TrueQ[Lookup[outcome[[2]],"Published",False]],
            "The library was installed, but post-publication cleanup failed. See the recovery file.",
            "Library generation failed; inspect the cause and any recovery file."],
        <|"Family"->spec["Family"],"Parameters"->spec["Parameters"],"Destination"->target,
          "Published"->TrueQ[Lookup[outcome[[2]],"Published",False]],"Cause"->outcome|>], outcome]
]];

optionArgumentQ[x_] := MatchQ[x,_Rule|_RuleDelayed] ||
    (ListQ[x] && AllTrue[x,optionArgumentQ]);

dispatch[command_String, arguments_List] := Block[{$command = command,$stage = "Validation",$generationTag = Unique["generation"]},
    Module[{args=arguments,rules={},options,specs,result,completed={},outcome},
        outcome = Catch[
            require[arguments];
            While[args =!= {} && optionArgumentQ[Last[args]],
                rules = Join[Flatten[{Last[args]}],rules]; args = Most[args]];
            options = require[resolveOptions[rules,command]];
            specs = require[requestSpecifications[command,args]];
            Do[
                result = generateOne[spec,options];
                If[FailureQ[result],
                    If[TrueQ[Lookup[result[[2]],"Published",False]], AppendTo[completed,result[[2,"Destination"]]]];
                    Throw[Failure[result[[1]],Join[result[[2]],<|"CompletedFiles"->completed|>]],$generationTag]];
                AppendTo[completed,result],
                {spec,specs}
            ];
            Null,
            $generationTag];
        outcome
    ]
];

(* ::Section:: *)
(*Public entry points and library inventory*)
GenerateGravitonScalarsSpecific[args___] := dispatch["GenerateGravitonScalarsSpecific",{args}];
GenerateGravitonScalars[args___] := dispatch["GenerateGravitonScalars",{args}];
CheckGravitonScalars := inventory[{"GravitonScalarVertex","GravitonScalarPotentialVertex"}];

GenerateGravitonFermionsSpecific[args___] := dispatch["GenerateGravitonFermionsSpecific",{args}];
GenerateGravitonFermions[args___] := dispatch["GenerateGravitonFermions",{args}];
CheckGravitonFermions := inventory[{"GravitonFermionVertex"}];

GenerateGravitonVectorsSpecific[args___] := dispatch["GenerateGravitonVectorsSpecific",{args}];
GenerateGravitonVectors[args___] := dispatch["GenerateGravitonVectors",{args}];
CheckGravitonVectors := inventory[{"GravitonMassiveVectorVertex","GravitonVectorVertex","GravitonVectorGhostVertex"}];

GenerateGravitonVertexSpecific[args___] := dispatch["GenerateGravitonVertexSpecific",{args}];
GenerateGravitonVertex[args___] := dispatch["GenerateGravitonVertex",{args}];
CheckGravitonVertex := inventory[{"GravitonVertex"}];

GenerateGravitonSUNYMSpecific[args___] := dispatch["GenerateGravitonSUNYMSpecific",{args}];
GenerateGravitonSUNYM[args___] := dispatch["GenerateGravitonSUNYM",{args}];
CheckGravitonSUNYM := inventory[{"GravitonQuarkGluonVertex","GravitonGluonVertex","GravitonThreeGluonVertex","GravitonFourGluonVertex","GravitonYMGhostVertex","GravitonGluonGhostVertex"}];

GenerateHorndeskiG2Specific[args___] := dispatch["GenerateHorndeskiG2Specific",{args}];
GenerateHorndeskiG2[args___] := dispatch["GenerateHorndeskiG2",{args}];
CheckHorndeskiG2 := inventory[{"HorndeskiG2"}];

GenerateHorndeskiG3Specific[args___] := dispatch["GenerateHorndeskiG3Specific",{args}];
GenerateHorndeskiG3[args___] := dispatch["GenerateHorndeskiG3",{args}];
CheckHorndeskiG3 := inventory[{"HorndeskiG3"}];

GenerateHorndeskiG4Specific[args___] := dispatch["GenerateHorndeskiG4Specific",{args}];
GenerateHorndeskiG4[args___] := dispatch["GenerateHorndeskiG4",{args}];
CheckHorndeskiG4 := inventory[{"HorndeskiG4"}];

GenerateHorndeskiG5Specific[args___] := dispatch["GenerateHorndeskiG5Specific",{args}];
GenerateHorndeskiG5[args___] := dispatch["GenerateHorndeskiG5",{args}];
CheckHorndeskiG5 := inventory[{"HorndeskiG5"}];

GenerateScalarGaussBonnetSpecific[args___] := dispatch["GenerateScalarGaussBonnetSpecific",{args}];
GenerateScalarGaussBonnet[args___] := dispatch["GenerateScalarGaussBonnet",{args}];
CheckScalarGaussBonnet := inventory[{"ScalarGaussBonnet"}];

GenerateGravitonAxionVectorSpecific[args___] := dispatch["GenerateGravitonAxionVectorSpecific",{args}];
GenerateGravitonAxionVector[args___] := dispatch["GenerateGravitonAxionVector",{args}];
CheckGravitonAxionVector := inventory[{"GravitonAxionVectorVertex"}];

GenerateQuadraticGravityVertexSpecific[args___] := dispatch["GenerateQuadraticGravityVertexSpecific",{args}];
GenerateQuadraticGravityVertex[args___] := dispatch["GenerateQuadraticGravityVertex",{args}];
CheckQuadraticGravityVertex := inventory[{"QuadraticGravityVertex"}];


(* Parse no expressions from filenames. Backups, staging files and FORM jobs
   never count as installed libraries. *)
canonicalLibraryQ[family_String, path_String] := FileType[path] === File &&
    StringMatchQ[FileNameTake[path], RegularExpression[family <>
        If[MemberQ[{"HorndeskiG2","HorndeskiG3","HorndeskiG4","HorndeskiG5"},family],
            "_(0|[1-9][0-9]*)_(0|[1-9][0-9]*)_[1-9][0-9]*", "_[1-9][0-9]*"]]];

inventoryFiles[family_String] := Select[FileNames[family<>"_*",$libraryDirectory],canonicalLibraryQ[family,#]&];
inventory[families_List] := Scan[Function[family,
    Scan[Print[FileNameTake[#]] &, inventoryFiles[family]]],families];

FeynGravLibrariesGeneratorFORMInformation[] := CalcFormCheck[
    FORMExecutable -> legacyExecutable[]];
FeynGravLibrariesGeneratorFORMInformation[executable_String] := CalcFormCheck[FORMExecutable->executable];
FeynGravLibrariesGeneratorPrintFORMStatus[] := Module[{status=FeynGravLibrariesGeneratorFORMInformation[]},Print[status];status];

If[!TrueQ[existingSetting["FeynGravLibrariesGenerator`$FeynGravLibrariesGeneratorStartupMessage",
        existingSetting["Global`$FeynGravLibrariesGeneratorStartupMessage",True]] === False],
    Print[StringRiffle[{
        "FeynGrav library generator — CalcFormConverter",
        "Loading preserves Directory[] and starts no calculation, FORM check or installation.",
        "Use CheckGravitonScalars (without brackets) to list libraries; GenerateGravitonScalarsSpecific[1] generates one order, and GenerateGravitonScalars[n] generates a batch.",
        "Default library destination: " <> $libraryDirectory <> ". Use OutputDirectory -> anExistingDirectory to generate elsewhere. Existing files are replaced only after validation.",
        "Calculation defaults: automatic FORM/TFORM selection, up to eight TFORM workers with serial fallback if TFORM is missing; Dirac and colour processing enabled.",
        "Pure/quadratic gravity: order n means n + 2 graviton legs. Gauss-Bonnet batches start at two; n = 1 returns Null without files.",
        "Use ?function and Options[function] for signatures, defaults and failures. Full guide: " <> FileNameJoin[{$libraryDirectory,"Generator.md"}],
        "To suppress this introduction, set $FeynGravLibrariesGeneratorStartupMessage = False before loading."
    }, "\n"]]];

End[];
EndPackage[];
