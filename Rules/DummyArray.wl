(* ::Package:: *)

(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["DummyArray`"];


DummyArray::usage = "DummyArray[k]. The function returns an array of indices {m1,n1,\[Ellipsis],mk,nk}.";


(* Structural failures are returned as values; callers should use FailureQ. *)
DummyArray::usage = DummyArray::usage <> " Supported signatures: DummyArray[n]; argument 1: explicit integer >= 0." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Clear[DummyArray];

DummyArray[n_] :=
    RuleValidation`RuleCall[
        DummyArray[n],
        {{1, "Integer", 0}},
        (
{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] //Flatten
        ), False
    ];

Clear[DummyArrayK];

DummyArrayK[n_] :=
    RuleValidation`RuleCall[
        DummyArrayK[n],
        {{1, "Integer", 0}},
        (
{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n] //Flatten
        ), False
    ];



(* Unsupported arities fail before any calculation. *)
DummyArray[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray] <> SymbolName[DummyArray], {arguments}, {1}];

DummyArrayK[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArrayK] <> SymbolName[DummyArrayK], {arguments}, {1}];

EndPackage[];
