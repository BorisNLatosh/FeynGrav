(* ::Package:: *)

BeginPackage["indexArraySymmetrization`"];


indexArraySymmetrization::usage = "indexArraySymmetrization[indexArray]. Returns an array of arrays. It is a list of indices that are symmetric with respect to permutations within each index pair and with respect to permutations of index pairs.";


indexArraySymmetrization3::usage = "indexArraySymmetrization3[indexArray]. Generalization for triplets with the third component being a momentum. Returns an array of arrays. It is a list of indices that are symmetric with respect to permutations within each index pair and with respect to permutations of index pairs.";


Begin["Private`"];


(* indexArraySymmetrization realization without parallelization *)


ClearAll[indexArraySymmetrization];

indexArraySymmetrization[indexArray_List] := 
	indexArraySymmetrization[indexArray] =
		Module[{pairs, perms},
			If[indexArray === {},
				{{}},
				pairs = Partition[indexArray, 2];
				perms = Permutations[pairs];
				Flatten[
					ParallelMap[
						(Flatten /@ Tuples[Transpose[{#, Reverse /@ #}]]) &,
						perms,
						Method -> "CoarsestGrained"
					],
					1
				]
			]
		];


ClearAll[indexArraySymmetrization3];

indexArraySymmetrization3[indexArray_List] := 
	indexArraySymmetrization3[indexArray] =
		Module[{triplets},
			If[indexArray === {},
				{{}},
				triplets = Partition[indexArray, 3];
				Flatten[
					(Flatten /@ Tuples[Transpose[{#, #[[All, {2, 1, 3}]]}]]) & /@ 
					Permutations[triplets],
					1
				]
			]
		];


End[];


EndPackage[];
