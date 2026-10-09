(* ::Package:: *)
(* A single loader expression makes the fresh-kernel guard stop the entire load. *)
With[{core=FileNameJoin[{DirectoryName[$InputFileName],"BenchmarkCore.wl"}]},
 If[!TrueQ[FeynGravBenchmark`Private`$loadingGenerator] &&
   (FeynGravBenchmark`Private`$workflowMode==="Generator" || MemberQ[$Packages,"FeynGravLibrariesGenerator`"]),
 Failure["FreshKernelRequired",<|"MessageTemplate"->"Restart the kernel before switching from generator to main-package benchmarks."|>],Get[core]]]
