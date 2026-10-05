(* ::Package:: *)

(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["MTDWrapper`",{"FeynCalc`"}];




MTDWrapper::usage =
"MTDWrapper[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)}] \
gives the product of D-dimensional metric tensors \
MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\[Nu]1]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)], \
taking the indices in consecutive pairs. The input list must contain \
an even number of elements.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
MTDWrapper::usage = MTDWrapper::usage <> " Supported signatures: MTDWrapper[indexArray]; argument 1: flat list, block size 2, length 0 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[MTDWrapper];

MTDWrapper[indexArray_] :=
    RuleValidation`RuleCall[
        MTDWrapper[indexArray],
        {{1, "Array", 2, 0, Infinity}},
        (
Apply[
			Times,
			RuleValidation`RuleRequire[MapApply[
				MTD,
				Partition[indexArray,2]
			]]
		]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
MTDWrapper[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[MTDWrapper] <> SymbolName[MTDWrapper], {arguments}, {1}];

End[];


EndPackage[];
