(* ::Package:: *)
(* Loading prepares definitions only: no executable checks or benchmarks. *)
With[{root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],support=DirectoryName[$InputFileName]},
 If[MemberQ[$Packages,"FeynGrav`"] || FeynGravBenchmark`Private`$workflowMode==="Main",
 Failure["FreshKernelRequired",<|"MessageTemplate"->"Restart the kernel before loading the generator benchmark."|>],
 Get[FileNameJoin[{root,"Libs","Generator.wl"}]];
 Block[{FeynGravBenchmark`Private`$loadingGenerator=True},Get[FileNameJoin[{support,"Benchmark.wl"}]]];
 Get[FileNameJoin[{support,"GeneratorSupport.wl"}]]]]
