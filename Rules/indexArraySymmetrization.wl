(* ::Package:: *)

BeginPackage["indexArraySymmetrization`"];


indexArraySymmetrization::usage = 
"indexArraySymmetrization[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)] \
returns all index arrays obtained by permuting adjacent index pairs \
and independently swapping indices within each pair.";


indexArraySymmetrization3::usage = 
"indexArraySymmetrization3[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)] \
returns all index arrays obtained by permuting adjacent index triplets and swapping \
the first two entries within each triplet.";


Begin["Private`"];


ClearAll[indexArraySymmetrization];

(*
	The input indexArray is a flat list of the form
	{mu1, nu1, mu2, nu2, ..., mun, nun}.
	
	The function returns all arrays obtained by:
	1. permuting the index pairs,
	2. independently swapping the two indices inside each pair.
	
	Each output is returned in the same flat-list format.
*)

indexArraySymmetrization[indexArray_List] := 
	indexArraySymmetrization[indexArray] =
		Module[{pairs, perms},
			(*
				The empty input has exactly one symmetrized arrangement:
				the empty list itself.
			*)
			If[indexArray === {},
				{{}},
				
				(*
					Split the flat input into adjacent pairs:
					{{mu1, nu1}, {mu2, nu2}, ..., {mun, nun}}.
					The symmetry acts on these pairs as blocks.
				*)
				
				pairs = Partition[indexArray, 2];
				
				(*
					Generate all permutations of the pair list.
					This accounts for the symmetry under permutations of pairs.
				*)
				
				perms = Permutations[pairs];
				
				(*
					For each permutation of pairs:
					- Reverse /@ # swaps the two indices inside every pair,
					- Transpose[{#, Reverse /@ #}] forms, for each pair, 
						the two allowed orientations,
					- Tuples[...] enumerates all independent choices of
						original or swapped orientation for every pair,
					- Flatten /@ ... converts each resulting list of pairs
						back to the flat-list output format.
						
					The outer loop over pair permutations is parallelized.
					Method -> "CoarsestGrained" is used because different
					permutations lead to branches of similar cost.
				*)
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

(*
	The input indexArray is a flat list of the form
	{mu1, nu1, k1, mu2, nu2, k2, ..., mun, nun, kn}.
	
	The function returns all arrays obtained by:
	1. permuting the index triplets,
	2. independently swapping the first two indices inside each triplet.
	
	Each output is returned in the same flat-list format.
*)

indexArraySymmetrization3[indexArray_List] := 
	indexArraySymmetrization3[indexArray] =
		Module[{triplets, perms},
			(*
				The empty input has exactly one symmetrized arrangement:
				the empty list itself.
			*)
			If[indexArray === {},
				{{}},
				(*
					Split the flat input into adjacent triplets:
					{{mu1, nu1, k1}, {mu2, nu2, k2}, ..., {mun, nun, kn}}.
					The symmetry acts on these triplets as blocks.
				*)
				triplets = Partition[indexArray, 3];
				(*
					Generate all permutations of the triplet list.
					This accounts for the symmetry under permutations of triplets.
				*)
				perms = Permutations[triplets];
				(*
					For each permutation of triplets:
					- #[[All,{2,1,3}]] swaps the first two indices in every triplet,
					- Transpose[{#, swapped}] forms, for each triplet,
						the two allowed orientations,
					- Tuples[...] enumerates all independent choices of
						original or swapped orientation for every triplet,
					- Flatten /@ ... converts each resulting list of triplets
						back to the flat-list output format.
						
					The outer loop over triplet permutations is parallelized.
					Method -> "CoarsestGrained" is used because different
					permutations lead to branches of similar cost.
				*)
				Flatten[
					ParallelMap[
						Module[{swapped},
							swapped = #[[All, {2, 1, 3}]];
							Flatten /@ Tuples[Transpose[{#, swapped}]]
						] &,
						perms,
						Method -> "CoarsestGrained"
					],
					1
				]
			]
		];


End[];


EndPackage[];
