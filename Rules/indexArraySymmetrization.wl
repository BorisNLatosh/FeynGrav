(* ::Package:: *)

BeginPackage["indexArraySymmetrization`"];


indexArraySymmetrization::usage = "indexArraySymmetrization[indexArray]. Returns an array of arrays. It is a list of indices that are symmetric with respect to permutations within each index pair and with respect to permutations of index pairs.";


Begin["Private`"];


indexArraySymmetrization = indexArray |-> If[ Length[indexArray]==0 , {{}} , Partition[Flatten[ Fold[Join[#1,#1/.{#2[[1]]->#2[[2]],#2[[2]]->#2[[1]]}]&,#,Partition[#,2]]&/@(Flatten/@Permutations[Partition[indexArray,2]]) ],Length[indexArray]] ];


End[];


EndPackage[];
