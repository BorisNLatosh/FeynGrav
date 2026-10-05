(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"CTensorGeneral`", "CTensorGeneral.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","CTensorGeneral`","indexArraySymmetrization`"}];


GravitonScalarVertex::usage =
"GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The expression for a vertex describing a coupling of n gravitons to the scalar field kinetic term. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\) are scalar field momenta; m is the scalar field mass.";


GravitonScalarPotentialVertex::usage =
"GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda]]. \
The expression for a vertex describing a coupling of n gravitons to the scalar field power law potential \[Lambda] \!\(\*FractionBox[SuperscriptBox[\(\[Phi]\), \(n\)], \(n!\)]\). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \[Lambda] is the coupling.";


GravitonScalarVertexUncontracted::usage =
"GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The expression for a vertex describing a coupling of n gravitons to the scalar field kinetic term. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\) are scalar field momenta; m is the scalar field mass. No contraction or simplification is carried out.";


GravitonScalarPotentialVertexUncontracted::usage =
"GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda]]. \
The expression for a vertex describing a coupling of n gravitons to the scalar field power law potential \[Lambda] \!\(\*FractionBox[SuperscriptBox[\(\[Phi]\), \(n\)], \(n!\)]\). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \[Lambda] is the coupling. No contraction or simplification is carried out.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GravitonScalarVertex::usage = GravitonScalarVertex::usage <> " Supported signatures: GravitonScalarVertex[indexArray, p1, p2, m]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonScalarPotentialVertex::usage = GravitonScalarPotentialVertex::usage <> " Supported signatures: GravitonScalarPotentialVertex[indexArray, \\[Lambda]]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonScalarVertexUncontracted::usage = GravitonScalarVertexUncontracted::usage <> " Supported signatures: GravitonScalarVertexUncontracted[indexArray, p1, p2, m]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonScalarPotentialVertexUncontracted::usage = GravitonScalarPotentialVertexUncontracted::usage <> " Supported signatures: GravitonScalarPotentialVertexUncontracted[indexArray, \\[Lambda]]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


(* Kinetic term. *)


Clear[GravitonScalarVertexUncontracted];

GravitonScalarVertexUncontracted[indexArray_, p1_, p2_, m_] :=
    RuleValidation`RuleCall[
        GravitonScalarVertexUncontracted[indexArray, p1, p2, m],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) (FVD[p1,Global`\[ScriptM]]FVD[p2,Global`\[ScriptN]]+FVD[p1,Global`\[ScriptN]]FVD[p2,Global`\[ScriptM]])RuleValidation`RuleRequire[CTensorGeneral[{Global`\[ScriptM],Global`\[ScriptN]},indexArray]] - m^2 RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]] )
        ), True
    ];


Clear[GravitonScalarVertex];

GravitonScalarVertex[indexArray_, p1_, p2_, m_] :=
    RuleValidation`RuleCall[
        GravitonScalarVertex[indexArray, p1, p2, m],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) RuleValidation`RuleRequire[Contract[(FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]+FVD[p1,\[ScriptN]]FVD[p2,\[ScriptM]])RuleValidation`RuleRequire[CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray]]]] - m^2 FeynCalcInternal[RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]]] ) //Expand
        ), True
    ];


(* Power law potential. *)


Clear[GravitonScalarPotentialVertexUncontracted];

GravitonScalarPotentialVertexUncontracted[indexArray_, \[Lambda]_] :=
    RuleValidation`RuleCall[
        GravitonScalarPotentialVertexUncontracted[indexArray, \[Lambda]],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]]
        ), True
    ];


Clear[GravitonScalarPotentialVertex];

GravitonScalarPotentialVertex[indexArray_, \[Lambda]_] :=
    RuleValidation`RuleCall[
        GravitonScalarPotentialVertex[indexArray, \[Lambda]],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] FeynCalcInternal[RuleValidation`RuleRequire[CTensorGeneral[{},indexArray]]] //Expand
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GravitonScalarPotentialVertex[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonScalarPotentialVertex] <> SymbolName[GravitonScalarPotentialVertex], {arguments}, {2}];

GravitonScalarPotentialVertexUncontracted[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonScalarPotentialVertexUncontracted] <> SymbolName[GravitonScalarPotentialVertexUncontracted], {arguments}, {2}];

GravitonScalarVertex[arguments___] /; !MemberQ[{4}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonScalarVertex] <> SymbolName[GravitonScalarVertex], {arguments}, {4}];

GravitonScalarVertexUncontracted[arguments___] /; !MemberQ[{4}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonScalarVertexUncontracted] <> SymbolName[GravitonScalarVertexUncontracted], {arguments}, {4}];

End[];


EndPackage[];
