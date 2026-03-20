(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CTensorGeneral`",{"FeynCalc`","ITensor`","indexArraySymmetrization`"}];


CTensorPlainGeneral::usage = "CTensorPlainGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7. The definition does not allow for any symmetry.";


CTensorGeneral::usage = "CTensorGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7.";


Begin["Private`"];


(* C Tensor *)


Clear[CTensorPlain];

(* Base case of the public interface: the value on the empty list is 1. *)
CTensorPlain[{}] = 1;

(*
	Recursive definition with memoization.
	Once CTensorPlain[indexArray] is computed, the value is stored and reused
	on later calls with the same argument.
	
	Internally, the recursion is performed not on explicit tail sublists, but on
	the starting position pos inside the original list indexArray. This avoids
	repeatedly constructing tail slices indexArray[[2 k + 1 ;;]] and improves
	performance.
*)

CTensorPlain[indexArray_List] :=
	CTensorPlain[indexArray] =
		Module[{n, c},
			(* Total number of indices in the current list. *)
			n = Length[indexArray];

			(*
				Internal memoized helper.
				c[pos] represents CTensorPlain applied to the suffix of indexArray
				starting at position pos.
			*)
			c[n + 1] = 1;

			c[pos_Integer] :=
				c[pos] =
					Module[{len, m},
						(* Length of the suffix starting at pos. *)
						len = n - pos + 1;

						(* Number of terms in the sum: integer part of len/2. *)
						m = Quotient[len, 2];

						(*
							Main recurrence relation for the suffix starting at pos.
							For each k = 1, ..., m:
							- take the block of the first 2 k elements of the current suffix,
								i.e. indexArray[[pos ;; pos + 2 k - 1]],
							- apply ITensorPlain to that block,
							- multiply by c[pos + 2 k], which is the value on the remaining tail,
							- include the alternating sign (-1)^(k - 1),
								then sum all contributions and divide by len.
						*)
						1/len *
							Sum[
								(-1)^(k - 1) ITensorPlain[indexArray[[pos ;; pos + 2 k - 1]]] c[pos + 2 k],
								{k, 1, m}
							]
					];

			(* Start from the full list, i.e. from position 1. *)
			c[1]
		];



(*
	CTensor[indexArray] computes the fully symmetrized average of CTensorPlain
	over all possible reorderings of adjacent index pairs and over both possible
	orientations inside each pair.

	If indexArray has length 2 n, then the normalization factor is 1/(2^n n!).
	The code avoids constructing the full symmetrized list explicitly. Instead,
	it generates one configuration at a time, evaluates CTensorPlain on it,
	and accumulates the result directly in sum.
*)

Clear[CTensor];

CTensor[indexArray_List] :=
	CTensor[indexArray] =
		Module[{n, pairs, reversedPairs, used, current, sum = 0, rec},
			(*
				n is the number of adjacent pairs in the input array:
				indexArray = {a1, b1, a2, b2, ..., an, bn}.
			*)
			n = Length[indexArray]/2;

			(* Split the flat index array into adjacent pairs {{a1, b1}, ..., {an, bn}}. *)
			pairs = Partition[indexArray, 2];

			(* For each pair {a, b}, also prepare its reversed orientation {b, a}. *)
			reversedPairs = Reverse /@ pairs;

			(*
				used[[j]] is True once the j-th original pair has already been placed
				into the current partially constructed configuration.
			*)
			used = ConstantArray[False, n];

			(*
				current stores the symmetrized index array currently being built.
				It has the same length as the original input array.
			*)
			current = ConstantArray[0, Length[indexArray]];

			(*
				rec[pos] fills the pos-th pair slot of current.
				When pos > n, the whole configuration has been constructed,
				so we evaluate CTensorPlain on it and add the contribution to sum.
			*)
			rec[pos_Integer] :=
				If[pos > n,
					(* A complete symmetrized configuration has been generated. *)
					sum += CTensorPlain[current],
					(*
						Try every pair that has not yet been used.
						For each such pair, explore both orientations:
						{a, b} and {b, a}.
					*)
					Scan[
						Function[j,
							If[! used[[j]],
								(* Mark the j-th pair as used at this recursion level. *)
								used[[j]] = True;

								(* Place the pair in its original orientation and continue. *)
								current[[2 pos - 1 ;; 2 pos]] = pairs[[j]];
								rec[pos + 1];

								(* Place the same pair in reversed orientation and continue. *)
								current[[2 pos - 1 ;; 2 pos]] = reversedPairs[[j]];
								rec[pos + 1];

								(* Release the pair when backtracking to the previous level. *)
								used[[j]] = False;
							]
						],
						Range[n]
					]
				];

			(* Start the recursive generation from the first pair slot. *)
			rec[1];

			(* Divide by the total number of generated configurations: 2^n n!. *)
			sum/(2^n Factorial[n])
		];


(* C1 Tensor *)


Clear[C1TensorPlain];

C1TensorPlain[args1_, args2_] :=
	C1TensorPlain[args1, args2] =
		If[
			Length[args1] == 0,
			0,
			Total[
				Map[
					Power[-1, #] ITensorPlain[Join[args1, args2[[;; 2 #]]]] CTensorPlain[args2[[2 # + 1 ;;]]] &,
					Range[ 0, Length[args2]/2] ]
			]
		];


Clear[C1Tensor];

C1Tensor[indexArrayExternal_, indexArrayInternal_] :=
	C1Tensor[indexArrayExternal, indexArrayInternal] =
		1/Power[2,Length[indexArrayInternal]/2]1/Factorial[Length[indexArrayInternal]/2]*
		Total[
			C1TensorPlain[indexArrayExternal,#]& /@ indexArraySymmetrization[indexArrayInternal]
		];


(* C2 Tensor *)


Clear[C2TensorPlain];
C2TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C2TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[ Length[indexArrayExternal]!=4, 
			0, 
			Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C1TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 # + 1;;]]] & , Range[0,Length[indexArrayInternal]/2] ]] 
		];


Clear[C2Tensor];

C2Tensor[indexArrayExternal_,indexArrayInternal_] := 
	C2Tensor[indexArrayExternal,indexArrayInternal] = 
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C2TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C3 Tensor *)


Clear[C3TensorPlain];

C3TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C3TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[ Length[indexArrayExternal]!=6 ,
			0,
			Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C2TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 #+1;;]]]& , Range[0,Length[indexArrayInternal]/2] ]] 
		];


Clear[C3Tensor];

C3Tensor[indexArrayExternal_,indexArrayInternal_] := 
	C3Tensor[indexArrayExternal,indexArrayInternal] = 
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C3TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C4 Tensor *)


Clear[C4TensorPlain];

C4TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C4TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[
			Length[indexArrayExternal]!=8,
				0,
				Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C3TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 #+1;;]]]& , Range[0,Length[indexArrayInternal]/2] ]] 
			];


Clear[C4Tensor];

C4Tensor[indexArrayExternal_,indexArrayInternal_] := 
	C4Tensor[indexArrayExternal,indexArrayInternal] = 
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C4TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C5 Tensor *)


Clear[C5TensorPlain];

C5TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C5TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[
			Length[indexArrayExternal]!=2*5,
			0 ,
			Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C4TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 #+1;;]]] &, Range[0,Length[indexArrayInternal]/2] ]] 
		];


Clear[C5Tensor];

C5Tensor[indexArrayExternal_,indexArrayInternal_] := 
	C5Tensor[indexArrayExternal,indexArrayInternal] = 
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C5TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C6 Tensor *)


Clear[C6TensorPlain];

C6TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C6TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[
			Length[indexArrayExternal]!=2*6,
			0,
			Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C5TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 #+1;;]]] & , Range[0,Length[indexArrayInternal]/2] ]] 
		];


Clear[C6Tensor];

C6Tensor[indexArrayExternal_,indexArrayInternal_] := 
	C6Tensor[indexArrayExternal,indexArrayInternal] = 
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C6TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C7 Tensor *)


Clear[C7TensorPlain];

C7TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	C7TensorPlain[indexArrayExternal,indexArrayInternal] = 
		If[
			Length[indexArrayExternal]!=2*7,
			0,
			Total[Map[ Power[-1,#] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 #]]] ] C6TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 #+1;;]]] & , Range[0,Length[indexArrayInternal]/2] ]] 
		];


Clear[C7Tensor];

C7Tensor[indexArrayExternal_,indexArrayInternal_] :=
	C7Tensor[indexArrayExternal,indexArrayInternal] =
		1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[C7TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ;


(* C Tensor General *)


Clear[CTensorPlainGeneral];

CTensorPlainGeneral[indexArrayExternal_,indexArrayInternal_] := 
	CTensorPlainGeneral[indexArrayExternal,indexArrayInternal] = 
		Switch[Length[indexArrayExternal]/2,
			0,CTensorPlain[indexArrayInternal],
			1,C1TensorPlain[indexArrayExternal,indexArrayInternal],
			2,C2TensorPlain[indexArrayExternal,indexArrayInternal],
			3,C3TensorPlain[indexArrayExternal,indexArrayInternal],
			4,C4TensorPlain[indexArrayExternal,indexArrayInternal],
			5,C5TensorPlain[indexArrayExternal,indexArrayInternal],
			6,C6TensorPlain[indexArrayExternal,indexArrayInternal],
			7,C7TensorPlain[indexArrayExternal,indexArrayInternal]
		];


Clear[CTensorGeneral];

CTensorGeneral[indexArrayExternal_,indexArrayInternal_] := 
	CTensorGeneral[indexArrayExternal,indexArrayInternal] = 
		Switch[Length[indexArrayExternal]/2,
			0,CTensor[indexArrayInternal],
			1,C1Tensor[indexArrayExternal,indexArrayInternal],
			2,C2Tensor[indexArrayExternal,indexArrayInternal],
			3,C3Tensor[indexArrayExternal,indexArrayInternal],
			4,C4Tensor[indexArrayExternal,indexArrayInternal],
			5,C5Tensor[indexArrayExternal,indexArrayInternal],
			6,C6Tensor[indexArrayExternal,indexArrayInternal],
			7,C7Tensor[indexArrayExternal,indexArrayInternal]
		];


End[];


EndPackage[];
