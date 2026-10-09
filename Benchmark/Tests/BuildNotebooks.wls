(* Developer utility: regenerate notebooks without evaluating their input cells. *)
root=DirectoryName[DirectoryName[$InputFileName]];
(* Build the formatting call as literal input boxes: ToBoxes would otherwise
   consume StandardForm as a typesetting instruction during generation. *)
input[s_String] := If[StringStartsQ[s,"StandardForm["],
 input[StringDrop[StringDrop[s,13],-1]] /. BoxData[b_] :> BoxData[RowBox[{"StandardForm","[",b,"]"}]],
 Cell[BoxData[RowBox[Riffle[
 (ToExpression[#,InputForm,Function[e,ToBoxes[Unevaluated[e],StandardForm],HoldAllComplete]]& /@ StringSplit[s,"\n"]),"\[IndentingNewLine]"]]],"Input",InitializationCell->False]];
specs={
 {"01_Overview_and_Quick_Check.nb","Overview","Quick check and total calculation time","Does the converter work here, and what overhead does a small calculation have?","Three small contractions have known answers. Package setup, executable checks and complete calculations are reported separately."},
 {"02_Export.nb","Export","Export to FORM","How does export time depend on expression size and repetition?","Inputs vary repeated and distinct scalar products, propagators and symbols, momentum routing, and factorization. Quick uses 100 and 1,000 entries; Full also uses 5,000. Actual evaluated input sizes are recorded. No FORM process is required."},
 {"03_Import.nb","Import","Import from FORM","How does import time depend on output size and structure?","This notebook generates its own fixtures. Export and FORM execution are preparation steps, outside import timing. Repeated imports use the same saved files. Full includes the complete bubble."},
 {"04_FORM_Thread_Scaling.nb","Scaling","FORM and TFORM scaling","Which worker count helps on this machine?","Each workload is exported once. Serial FORM and available TFORM configurations are run sequentially in alternating order. Every successful output is imported and compared outside execution timing. Full includes the complete bubble."},
 {"05_Representative_Calculations.nb","Representative","Representative calculations","Which stage dominates realistic calculations?","Workloads include a vertex trace, a partial bubble and an s-channel tree amplitude with polarization tensors. Full adds the complete scalar-projected bubble. Construction, export, execution, import and complete CalcFormCalculate calls are measured separately. No loop integration is performed."}
};
requested=If[StringQ[Environment["FEYNGRAV_BENCHMARK_NOTEBOOK"]],{Environment["FEYNGRAV_BENCHMARK_NOTEBOOK"]},If[Length[$ScriptCommandLine]>1,Rest[$ScriptCommandLine],{}]];
If[requested=!={},specs=Select[specs,MemberQ[requested,First[#]]&]];
Do[
 cells={Cell[sp[[3]],"Title"],Cell[sp[[4]],"Subtitle"],Cell[sp[[5]],"Text"],
 Cell["1. Setup","Section"],Cell["Use a fresh kernel for reproducible measurements. Mathematica, FeynGrav with FeynCalc, and FORM/TFORM are sufficient. No Python, shell scripts or external fixtures are needed. Opening this notebook does not execute any cells.","Text"],
 input["setupSeconds = First[AbsoluteTiming[Get[FileNameJoin[{NotebookDirectory[], \"Support\", \"Benchmark.wl\"}]];]];"],
 Cell["2. Choose a profile","Section"],
 Cell["Replace None below with \"Quick\" or \"Full\". Until you do, evaluation skips the benchmarks. Quick uses three measured small trials and one measured substantial trial; Full uses five and three, respectively. One warmup is recorded separately. Full workloads can take substantial time and disk space.","Text"],
 input["config = BenchmarkConfiguration[];\nconfig[\"Profile\"] = None;\nconfig[\"FORMExecutable\"] = Automatic;\nconfig[\"TFORMExecutable\"] = Automatic;\nconfig[\"Workers\"] = Automatic;\nconfig[\"Repetitions\"] = Automatic;\nconfig[\"PhysicalRepetitions\"] = Automatic;\nconfig[\"ExecutionTimeout\"] = Automatic;\nconfig[\"OutputDirectory\"] = Automatic;\ncleanupSuccessfulFiles = False;"],
 Cell["Automatic uses PATH discovery, profile trial counts, and a unique directory under the system temporary directory. Supply absolute executable paths if your desktop kernel has a different PATH. Timeouts are 60 seconds per FORM execution in Quick and 600 in Full; they do not bound export, import or the entire notebook. Worker defaults are capped at the reported processor count; you may explicitly choose other counts.","Text"],
 Cell["3. Prepare inputs and check the environment","Section"],
 input["prepared = BenchmarkPrepare[\""<>sp[[2]]<>"\", config];\nIf[AssociationQ[prepared], prepared[\"SetupSeconds\"] = setupSeconds;];"],
 input["StandardForm[If[AssociationQ[prepared], Dataset[KeyTake[prepared, {\"Environment\", \"Checks\", \"Configuration\", \"SetupSeconds\"}]]]]"],
 Cell["Preparation constructs inputs and loads any required libraries. Import fixtures are generated at the start of each case in the following cell, before import measurements. Missing executables are recorded and dependent cases are skipped. No installation is attempted.","Text"],
 Cell["4. Run sequential measurements","Section"],Cell["For a new run, evaluate the preparation cell again first. Each prepared run can be executed once; previous reports are preserved.","Text"],input["report = BenchmarkRun[prepared];"],
 Cell["5. Inspect results and validation","Section"],input["StandardForm[BenchmarkSummary[report]]"],input["BenchmarkPlot[report]"],
 input["StandardForm[If[AssociationQ[report], Dataset[KeyTake[#, {\"Case\", \"Stage\", \"Workers\", \"Phase\", \"Trial\", \"Seconds\", \"Status\", \"Validation\", \"ValidationMethod\", \"FirstBenchmarkImportInKernel\"}] & /@ report[\"Rows\"]]]]"],
 Cell["Only successful, verified measured trials enter the timing summary. A single trial is one observation, not a variability estimate. Known small answers are checked algebraically; larger cases are checked for repeatability and agreement across execution paths, not against an independent physics calculation. Baseline means that a comparison result has been recorded but has not yet been matched. Inconclusive and timed-out comparisons are excluded.","Text"],
 Cell["6. Interpret and retain the report","Section"],
 Cell["All timings are wall-clock seconds. First-import and warmup observations remain separate from measured repetitions. A first benchmark import is not necessarily a cold kernel import if you previously used the converter. Setup, validation, fingerprinting and display are outside stage timers. StageSum adds export, execution and import times; Calculate measures the separate public call including its availability check. Initialization, filesystem caches, background load and temperature can affect timings. These observations do not guarantee a speedup on another machine.","Text"],
 input["If[AssociationQ[report], report[\"Directory\"]]"],
 Cell["The run directory contains report.json, summary.csv and working files. Reports include source hashes and raw observations. The system may remove temporary files later; choose a persistent OutputDirectory before a run if you want to keep results. Cleanup is optional, deletes only successful owned working directories, and retains reports and failure logs.","Text"],
 input["If[TrueQ[cleanupSuccessfulFiles] && AssociationQ[report], BenchmarkCleanup[report]]"]};
 nb=Notebook[cells,StyleDefinitions->"Default.nb",WindowTitle->sp[[3]],WindowSize->{1100,850},CellContext->Notebook,AutoGeneratedPackage->None];
 Export[FileNameJoin[{root,sp[[1]]}],"(* Content-type: application/vnd.wolfram.mathematica *)\n\n(*** Wolfram Notebook File ***)\n(* http://www.wolfram.com/nb *)\n\n"<>StringReplace[ToString[nb,InputForm,PageWidth->100], RegularExpression["(?m)[ \t]+$"] -> ""],"Text"];
 Print["Created ",sp[[1]]],{sp,specs}];
If[requested==={} || MemberQ[requested,"06_Library_Generation.nb"],Get[FileNameJoin[{root,"Tests","BuildGenerationNotebook.wl"}]]];
Quit[];
