(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"CTensorGeneral`", "CTensorGeneral.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["GravitonAxionVectorVertex`",{"FeynCalc`","CTensorGeneral`"}];


GravitonAxionVectorVertex::usage =
"GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. \
The function returns the gravitational vertex for coupling of scalar axions to the U(1) field. \
Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(q\), \(i\)]\) are momenta of vectors, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector Lorentz indices, and \[Theta] is the coupling.";


GravitonAxionVectorVertexUncontracted::usage =
"GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. \
The function returns the gravitational vertex for coupling of scalar axions to the U(1) field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(q\), \(i\)]\) are momenta of vectors, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector Lorentz indices, and \[Theta] is the coupling.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GravitonAxionVectorVertex::usage = GravitonAxionVectorVertex::usage <> " Supported signatures: GravitonAxionVectorVertex[indexArray, \\[Lambda]1, q1, \\[Lambda]2, q2, \\[Theta]]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonAxionVectorVertexUncontracted::usage = GravitonAxionVectorVertexUncontracted::usage <> " Supported signatures: GravitonAxionVectorVertexUncontracted[indexArray, \\[Lambda]1, q1, \\[Lambda]2, q2, \\[Theta]]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[GravitonAxionVectorVertex];

GravitonAxionVectorVertex[indexArray_, \[Lambda]1_, q1_, \[Lambda]2_, q2_, \[Theta]_] :=
    RuleValidation`RuleCall[
        GravitonAxionVectorVertex[indexArray, \[Lambda]1, q1, \[Lambda]2, q2, \[Theta]],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
- I (Global`\[Kappa])^(Length[indexArray]/2) \[Theta] FeynCalcInternal[RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]]] LC[\[Tau]1,\[Lambda]1,\[Tau]2,\[Lambda]2] FV[q1,\[Tau]1]FV[q2,\[Tau]2] //Contract
        ), True
    ];


Clear[GravitonAxionVectorVertexUncontracted];

GravitonAxionVectorVertexUncontracted[indexArray_, \[Lambda]1_, q1_, \[Lambda]2_, q2_, \[Theta]_] :=
    RuleValidation`RuleCall[
        GravitonAxionVectorVertexUncontracted[indexArray, \[Lambda]1, q1, \[Lambda]2, q2, \[Theta]],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
- I (Global`\[Kappa])^(Length[indexArray]/2) \[Theta] RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]] LC[\[Tau]1,\[Lambda]1,\[Tau]2,\[Lambda]2] FV[q1,\[Tau]1]FV[q2,\[Tau]2]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GravitonAxionVectorVertex[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonAxionVectorVertex] <> SymbolName[GravitonAxionVectorVertex], {arguments}, {6}];

GravitonAxionVectorVertexUncontracted[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonAxionVectorVertexUncontracted] <> SymbolName[GravitonAxionVectorVertexUncontracted], {arguments}, {6}];

End[];


EndPackage[];
