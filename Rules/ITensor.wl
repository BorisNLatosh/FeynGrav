(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"MTDWrapper`", "MTDWrapper.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["ITensor`",{"FeynCalc`","MTDWrapper`","indexArraySymmetrization`"}];


ITensor::usage =
"ITensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
The function returns \
MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)] \
symmetrized with respect to each index pair.";


ITensorPlain::usage =
"ITensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
Returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)].";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
ITensor::usage = ITensor::usage <> " Supported signatures: ITensor[indexArray]; argument 1: flat list, block size 2, length 0 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
ITensorPlain::usage = ITensorPlain::usage <> " Supported signatures: ITensorPlain[indexArray]; argument 1: flat list, block size 2, length 0 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[ITensorPlain];

ITensorPlain[indexArray_] :=
    RuleValidation`RuleCall[
        ITensorPlain[indexArray],
        {{1, "Array", 2, 0, Infinity}},
        (
RuleValidation`RuleRequire[MTDWrapper[
			RotateLeft[indexArray, 1]
		]]
        ), True
    ];


Clear[ITensor];

(*
	Empty input corresponds to the empty product of metric tensors.
	By convention, the value of the empty product is 1.
*)

ITensor[{}] = 1;

(*
	The input indexArray is a flat list of the form
	{rho1, sigma1, rho2, sigma2, ..., rhon, sigman}.

	The code converts this flat list into a list of pairs
	{{rho1, sigma1}, {rho2, sigma2}, ..., {rhon, sigman}}
	and performs the symmetrization directly inside ITensor.

	One pair is fixed in the first position. This removes the cyclic
	redundancy of the plain tensor product and reduces the number of
	pair permutations from n! to (n - 1)!.
*)

ITensor[indexArray_] :=
    RuleValidation`RuleCall[
        ITensor[indexArray],
        {{1, "Array", 2, 0, Infinity}},
        (
Module[ {n, pairs, firstPair, restPairs, termFromPairs},

			(* Number of index pairs. *)

			n = Length[indexArray]/2;

			(*
				Split the flat index list into adjacent pairs:
				{{rho1, sigma1}, {rho2, sigma2}, ..., {rhon, sigman}}.
			*)

			pairs = Partition[indexArray, 2];

			(*
				Fix one pair in the first position and permute only the rest.
				This eliminates the redundant cyclic copies of the same
				unsymmetrized tensor monomial.
			*)

			firstPair = First[pairs];
			restPairs = Rest[pairs];

			(*
				For an ordered list of pairs
				{{rho1, sigma1}, ..., {rhon, sigman}},
				construct the cyclic product
				MTD[sigma1, rho2] MTD[sigma2, rho3] ... MTD[sigman, rho1].

				The second entries of the pairs are matched with the first
				entries of the next pairs in the cycle.
			*)

			termFromPairs[pairList_List] :=
				Times @@
				RuleValidation`RuleRequire[MapThread[
					MTD,
					{
						pairList[[All, 2]],
						RotateLeft[pairList[[All, 1]], 1]
					}
				]];

			(*
				For each permutation of the unfixed pairs:
				1. prepend the fixed pair,
				2. generate both orientations of every pair,
				3. build the corresponding tensor monomials,
				4. sum all such monomials,
				5. multiply by the symmetrization factor.

				Reverse /@ Prepend[perm, firstPair] swaps the two indices in
				every pair, while Tuples[...] enumerates all independent choices
				of original or swapped orientation for each pair.

				The normalization factor is
				1/2^n for the 2^n swaps within the pairs,
				1/(n - 1)! for the nonredundant pair permutations
					after fixing one cyclic representative.

				Expand is applied at the end because the desired output is an
				expanded symbolic expression.
			*)
			Expand[
				1/2^n 1/Factorial[n - 1]
				Total[RuleValidation`RuleRequire[
					Flatten[RuleValidation`RuleRequire[
						RuleValidation`RuleRequire[Map[
							Function[perm,
								termFromPairs /@
								Tuples[
									Transpose[
										{
											Prepend[perm, firstPair],
											Reverse /@ Prepend[perm, firstPair]
										}
									]
								]
							],
							Permutations[restPairs]
						]]],
						1
					]
				]]
			]
		]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
ITensor[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[ITensor] <> SymbolName[ITensor], {arguments}, {1}];

ITensorPlain[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[ITensorPlain] <> SymbolName[ITensorPlain], {arguments}, {1}];

End[];


EndPackage[];
