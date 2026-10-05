(* ::Package:: *)

BeginPackage["RuleValidation`"];

RuleCall::usage = "RuleCall[call, requirements, body, cache] validates a rule call, propagates failures and caches successful results only. This is developer support for Rules.";
RuleRequire::usage = "RuleRequire[result] propagates a failed dependency before further algebra. Lists are checked in input order.";
RuleArityFailure::usage = "RuleArityFailure[function, arguments, counts] reports an unsupported argument count.";
RuleParallelMap::usage = "RuleParallelMap[f, list, options] collects worker failures before combining results.";
RuleBoundary::usage = "RuleBoundary[body] establishes a local failure boundary, including on parallel workers.";

Begin["`Private`"];

$failureTag = None;
$currentFunction = "RuleValidation`RuleRequire";

failure[tag_, message_, details_:<||>] := Failure[tag, Join[
    <|"MessageTemplate" -> message, "Function" -> $currentFunction|>, details]];

(* Only descend through containers, never through large successful tensors. *)
firstFailure[x_Failure] := x;
firstFailure[$Aborted] := Abort[];
firstFailure[$Failed] := failure["RuleEvaluationFailed", "A rule dependency returned $Failed.", <|"Stage" -> "Dependency"|>];
firstFailure[x_List] := SelectFirst[firstFailure /@ x, FailureQ, None];
firstFailure[_] := None;

RuleRequire[result_] := Module[{bad = firstFailure[result]},
    If[FailureQ[bad], If[$failureTag === None, bad, Throw[bad, $failureTag]], result]
];

SetAttributes[RuleBoundary, HoldAll];
RuleBoundary[body_] := Block[{$failureTag = Unique["ruleFailure$"]}, Catch[body, $failureTag]];

(* Requirements: {position, "Array", blockSize, minimum, maximum},
   {position, "Integer", minimum}, {position, "Expression"}, or
   {0, "Supported", condition, explanation}. Checks run in list order. *)
validate[args_, requirements_] := Module[{pos, value, kind, width, minimum, maximum},
    Do[
        pos = requirement[[1]]; kind = requirement[[2]];
        If[kind === "Supported",
            If[!TrueQ[requirement[[3]]], RuleRequire[failure["UnsupportedConfiguration", requirement[[4]], <|"Stage" -> "Validation"|>]]],
            value = args[[pos]];
            Switch[kind,
                "Array",
                    If[!ListQ[value] || AnyTrue[value, ListQ], RuleRequire[failure["InvalidArgumentType",
                        "The argument must be a flat list.", <|"ArgumentPosition" -> pos, "ActualType" -> ToString[Head[value], InputForm]|>]]];
                    {width, minimum, maximum} = requirement[[3;;5]];
                    If[Mod[Length[value], width] != 0 || Length[value] < minimum || Length[value] > maximum,
                        RuleRequire[failure["InvalidIndexArrayLength", "The list length is outside the supported bounds or contains an incomplete block.",
                            <|"ArgumentPosition" -> pos, "InputLength" -> Length[value], "BlockSize" -> width,
                              "MinimumLength" -> minimum, "MaximumLength" -> maximum|>]]],
                "Integer",
                    If[!IntegerQ[value] || value < requirement[[3]], RuleRequire[failure["InvalidParameterValue",
                        "The argument must be an explicit integer at least the stated minimum.",
                        <|"ArgumentPosition" -> pos, "MinimumValue" -> requirement[[3]]|>]]],
                "Expression",
                    If[ListQ[value] || AssociationQ[value], RuleRequire[failure["InvalidArgumentType",
                        "A symbolic expression, rather than a list or association, is required.", <|"ArgumentPosition" -> pos|>]]]
            ]
        ],
        {requirement, requirements}
    ];
    True
];

(* The held call is used only for diagnostics and the final cache assignment.
   A nested failure exits the outer rule boundary before any assignment occurs. *)
SetAttributes[RuleCall, HoldAll];
RuleCall[call_, requirements_, body_, cache_:False] := If[$failureTag === None,
    RuleBoundary[RuleCall[call, requirements, body, cache]],
    Block[{$currentFunction = With[{head = Head[Unevaluated[call]]}, Context[head] <> SymbolName[head]]},
        Module[{args, checked, result},
            args = List @@ Unevaluated[call];
            RuleRequire[args];
            checked = validate[args, requirements];
            RuleRequire[checked];
            result = RuleRequire[body];
            If[TrueQ[cache], call = result];
            result
        ]
    ]
];

RuleArityFailure[name_String, args_List, counts_List] := Module[{bad = firstFailure[args]},
    RuleRequire[If[FailureQ[bad], bad,
        Failure["InvalidArgumentCount", <|"MessageTemplate" -> "The function received an unsupported number of arguments.",
            "Function" -> name, "Stage" -> "Validation", "ArgumentCount" -> Length[args], "ExpectedCounts" -> counts|>]]]
];

RuleParallelMap[f_, items_List, opts___] := Module[{worker, results},
    worker = Function[item, RuleBoundary[f[item]]];
    DistributeDefinitions[worker];
    results = ParallelMap[worker, items, opts];
    RuleRequire[results]
];

End[];
EndPackage[];
