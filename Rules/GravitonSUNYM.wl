(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"CTensorGeneral`", "CTensorGeneral.wl"},
                {"CETensor`", "CETensor.wl"},
                {"GravitonFermionVertex`", "GravitonFermionVertex.wl"},
                {"GravitonVectorVertex`", "GravitonVectorVertex.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["GravitonSUNYM`",{"FeynCalc`","CTensorGeneral`","CETensor`","GravitonFermionVertex`","GravitonVectorVertex`"}];


GravitonQuarkGluonVertex::usage =
"GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\[Lambda],a}]. \
The function returns an expression for the gravitational vertex for quark-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\[Lambda],a} are the quark-gluon vertex parameters.";


GravitonThreeGluonVertex::usage =
"GravitonThreeGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. \
The function returns an expression for the gravitational vertex of three-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";


GravitonFourGluonVertex::usage =
"GravitonFourGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\),\!\(\*SubscriptBox[\(a\), \(4\)]\)]. \
The function returns an expression for the gravitational vertex of four-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";


GravitonGluonVertex::usage =
"GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. \
The function returns an expression for the gravitational vertex for gluon kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(k\), \(i\)]\) are gravitons momenta, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.}";


GravitonYMGhostVertex::usage =
"GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. \
The function returns an expression for the gravitational vertex for ghosts. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are ghost parameters.";


GravitonGluonGhostVertex::usage =
"GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\)},{\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)}]. \
The function returns an expression for the gravitational vertex for gluon-ghost vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluon and ghost parameters.";


GravitonQuarkGluonVertexUncontracted::usage =
"GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\[Lambda],a}]. \
The function returns an expression for the gravitational vertex for quark-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\[Lambda],a} are the quark-gluon vertex parameters.";


GravitonThreeGluonVertexUncontracted::usage =
"GravitonThreeGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. \
The function returns an expression for the gravitational vertex of three-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";


GravitonFourGluonVertexUncontracted::usage =
"GravitonFourGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\),\!\(\*SubscriptBox[\(a\), \(4\)]\)]. \
The function returns an expression for the gravitational vertex of four-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";


GravitonGluonVertexUncontracted::usage =
"GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. \
The function returns an expression for the gravitational vertex for gluon kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(k\), \(i\)]\) are gravitons momenta, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.}";


GravitonYMGhostVertexUncontracted::usage =
"GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. \
The function returns an expression for the gravitational vertex for ghosts. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are ghost parameters.";


GravitonGluonGhostVertexUncontracted::usage =
"GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\)},{\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)}]. \
The function returns an expression for the gravitational vertex for gluon-ghost vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluon and ghost parameters.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GravitonQuarkGluonVertex::usage = GravitonQuarkGluonVertex::usage <> " Supported signatures: GravitonQuarkGluonVertex[indexArray1, indexArray2]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 2 to 2." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonThreeGluonVertex::usage = GravitonThreeGluonVertex::usage <> " Supported signatures: GravitonThreeGluonVertex[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, p3, \\[Lambda]3, a3]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonFourGluonVertex::usage = GravitonFourGluonVertex::usage <> " Supported signatures: GravitonFourGluonVertex[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, p3, \\[Lambda]3, a3, p4, \\[Lambda]4, a4]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association); argument 11: symbolic expression (not a list or association); argument 12: symbolic expression (not a list or association); argument 13: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonGluonVertex::usage = GravitonGluonVertex::usage <> " Supported signatures: GravitonGluonVertex[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, \\[CurlyEpsilon]]; argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonYMGhostVertex::usage = GravitonYMGhostVertex::usage <> " Supported signatures: GravitonYMGhostVertex[indexArray, p1, a, p2, b]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonGluonGhostVertex::usage = GravitonGluonGhostVertex::usage <> " Supported signatures: GravitonGluonGhostVertex[indexArray, array1, array2, array3]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 3 to 3; argument 3: flat list, block size 1, length 3 to 3; argument 4: flat list, block size 1, length 3 to 3." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonQuarkGluonVertexUncontracted::usage = GravitonQuarkGluonVertexUncontracted::usage <> " Supported signatures: GravitonQuarkGluonVertexUncontracted[indexArray1, indexArray2]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 2 to 2." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonThreeGluonVertexUncontracted::usage = GravitonThreeGluonVertexUncontracted::usage <> " Supported signatures: GravitonThreeGluonVertexUncontracted[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, p3, \\[Lambda]3, a3]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonFourGluonVertexUncontracted::usage = GravitonFourGluonVertexUncontracted::usage <> " Supported signatures: GravitonFourGluonVertexUncontracted[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, p3, \\[Lambda]3, a3, p4, \\[Lambda]4, a4]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association); argument 11: symbolic expression (not a list or association); argument 12: symbolic expression (not a list or association); argument 13: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonGluonVertexUncontracted::usage = GravitonGluonVertexUncontracted::usage <> " Supported signatures: GravitonGluonVertexUncontracted[indexArray, p1, \\[Lambda]1, a1, p2, \\[Lambda]2, a2, \\[CurlyEpsilon]]; argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonYMGhostVertexUncontracted::usage = GravitonYMGhostVertexUncontracted::usage <> " Supported signatures: GravitonYMGhostVertexUncontracted[indexArray, p1, a, p2, b]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonGluonGhostVertexUncontracted::usage = GravitonGluonGhostVertexUncontracted::usage <> " Supported signatures: GravitonGluonGhostVertexUncontracted[indexArray, array1, array2, array3]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 3 to 3; argument 3: flat list, block size 1, length 3 to 3; argument 4: flat list, block size 1, length 3 to 3." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


(* Contracted vertex *)


Clear[GravitonQuarkGluonVertex];

GravitonQuarkGluonVertex[indexArray1_, indexArray2_] :=
    RuleValidation`RuleCall[
        GravitonQuarkGluonVertex[indexArray1, indexArray2],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 1, 2, 2}},
        (
RuleValidation`RuleRequire[DiracSimplify[RuleValidation`RuleRequire[Contract[RuleValidation`RuleRequire[FCI[
			RuleValidation`RuleRequire[CETensor[{indexArray2[[1]],\[ScriptM]},indexArray1]] RuleValidation`RuleRequire[Explicit[QuarkGluonVertex[\[ScriptM],indexArray2[[2]]]]]
		]]]]]]
        ), True
    ];


(* Expand momentum differences so each metric-momentum product matches the rule. *)
Clear[GravitonThreeGluonVertex];

GravitonThreeGluonVertex[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, p3_, \[Lambda]3_, a3_] :=
    RuleValidation`RuleCall[
        GravitonThreeGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}},
        (
RuleValidation`RuleRequire[Contract[RuleValidation`RuleRequire[FCI[
			Expand[RuleValidation`RuleRequire[ExpandScalarProduct[RuleValidation`RuleRequire[FCI[GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,Explicit->True]]]]]] /.Pair[LorentzIndex[x_,D],LorentzIndex[y_,D]]Pair[LorentzIndex[z_,D],Momentum[p_,D]] -> RuleValidation`RuleRequire[CTensorGeneral[{x,y,z,s},indexArray]]FVD[p,s]
		]]]]
        ), True
    ];


Clear[GravitonFourGluonVertex];

GravitonFourGluonVertex[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, p3_, \[Lambda]3_, a3_, p4_, \[Lambda]4_, a4_] :=
    RuleValidation`RuleCall[
        GravitonFourGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3, p4, \[Lambda]4, a4],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}, {11, "Expression"}, {12, "Expression"}, {13, "Expression"}},
        (
RuleValidation`RuleRequire[Contract[RuleValidation`RuleRequire[FCI[
			RuleValidation`RuleRequire[FCI[ GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4,Explicit->True] ]] /. Pair[LorentzIndex[\[ScriptX]_, D], LorentzIndex[\[ScriptY]_, D]] Pair[LorentzIndex[\[ScriptA]_, D], LorentzIndex[\[ScriptB]_, D]] -> RuleValidation`RuleRequire[CTensorGeneral[{\[ScriptX], \[ScriptY], \[ScriptA], \[ScriptB]}, indexArray]]
		]]]]
        ), True
    ];


Clear[GravitonGluonVertex]

GravitonGluonVertex[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, \[CurlyEpsilon]_] :=
    RuleValidation`RuleCall[
        GravitonGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, \[CurlyEpsilon]],
        {{1, "Array", 3, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}},
        (
SUNDelta[SUNIndex[a1],SUNIndex[a2]] RuleValidation`RuleRequire[GravitonVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]]]
        ), True
    ];


Clear[GravitonYMGhostVertex];

GravitonYMGhostVertex[indexArray_, p1_, a_, p2_, b_] :=
    RuleValidation`RuleCall[
        GravitonYMGhostVertex[indexArray, p1, a, p2, b],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
SUNDelta[SUNIndex[a],SUNIndex[b]]RuleValidation`RuleRequire[GravitonVectorGhostVertex[indexArray,p1,p2]]
        ), True
    ];


Clear[GravitonGluonGhostVertex];

GravitonGluonGhostVertex[indexArray_, array1_, array2_, array3_] :=
    RuleValidation`RuleCall[
        GravitonGluonGhostVertex[indexArray, array1, array2, array3],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 1, 3, 3}, {3, "Array", 1, 3, 3}, {4, "Array", 1, 3, 3}},
        (
RuleValidation`RuleRequire[Contract[RuleValidation`RuleRequire[FCI[
			Power[Global`\[Kappa],Length[indexArray]/2] RuleValidation`RuleRequire[FCI[GluonGhostVertex[array1,array2,array3,Explicit->True]]]/.Pair[Momentum[p_,D],LorentzIndex[x_,D]] -> RuleValidation`RuleRequire[CTensorGeneral[{x,s},indexArray]]FVD[p,s]
		]]]]
        ), True
    ];


(* Uncontracted vertex *)


Clear[GravitonQuarkGluonVertexUncontracted];

GravitonQuarkGluonVertexUncontracted[indexArray1_, indexArray2_] :=
    RuleValidation`RuleCall[
        GravitonQuarkGluonVertexUncontracted[indexArray1, indexArray2],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 1, 2, 2}},
        (
RuleValidation`RuleRequire[CETensor[{indexArray2[[1]],\[ScriptM]},indexArray1]] QuarkGluonVertex[\[ScriptM],indexArray2[[2]],Explicit->True]
        ), True
    ];


Clear[GravitonGluonVertexUncontracted]

GravitonGluonVertexUncontracted[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, \[CurlyEpsilon]_] :=
    RuleValidation`RuleCall[
        GravitonGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, \[CurlyEpsilon]],
        {{1, "Array", 3, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}},
        (
SUNDelta[SUNIndex[a1],SUNIndex[a2]] RuleValidation`RuleRequire[GravitonVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]]]
        ), True
    ];


Clear[GravitonThreeGluonVertexUncontracted];

GravitonThreeGluonVertexUncontracted[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, p3_, \[Lambda]3_, a3_] :=
    RuleValidation`RuleCall[
        GravitonThreeGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}},
        (
GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,Explicit->True]  /.Pair[LorentzIndex[x_,D],LorentzIndex[y_,D]]Pair[LorentzIndex[z_,D],Momentum[p_,D]] -> RuleValidation`RuleRequire[CTensorGeneral[{x,y,z,s},indexArray]]FVD[Momentum[p,D],LorentzIndex[s,D]]
        ), True
    ];


Clear[GravitonFourGluonVertexUncontracted];

GravitonFourGluonVertexUncontracted[indexArray_, p1_, \[Lambda]1_, a1_, p2_, \[Lambda]2_, a2_, p3_, \[Lambda]3_, a3_, p4_, \[Lambda]4_, a4_] :=
    RuleValidation`RuleCall[
        GravitonFourGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3, p4, \[Lambda]4, a4],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}, {11, "Expression"}, {12, "Expression"}, {13, "Expression"}},
        (
GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4,Explicit->True] /. { Pair[LorentzIndex[\[ScriptX]_, D], LorentzIndex[\[ScriptY]_, D]] Pair[LorentzIndex[\[ScriptA]_, D], LorentzIndex[\[ScriptB]_, D]] -> RuleValidation`RuleRequire[CTensorGeneral[{\[ScriptX], \[ScriptY], \[ScriptA], \[ScriptB]}, indexArray]] , FCGV[_]->s}
        ), True
    ];


Clear[GravitonYMGhostVertexUncontracted];

GravitonYMGhostVertexUncontracted[indexArray_, p1_, a_, p2_, b_] :=
    RuleValidation`RuleCall[
        GravitonYMGhostVertexUncontracted[indexArray, p1, a, p2, b],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
SUNDelta[SUNIndex[a],SUNIndex[b]]RuleValidation`RuleRequire[GravitonVectorGhostVertex[indexArray,p1,p2]]
        ), True
    ];


Clear[GravitonGluonGhostVertexUncontracted];

GravitonGluonGhostVertexUncontracted[indexArray_, array1_, array2_, array3_] :=
    RuleValidation`RuleCall[
        GravitonGluonGhostVertexUncontracted[indexArray, array1, array2, array3],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 1, 3, 3}, {3, "Array", 1, 3, 3}, {4, "Array", 1, 3, 3}},
        (
Power[Global`\[Kappa],Length[indexArray]/2]  GluonGhostVertex[array1,array2,array3,Explicit->True]/.Pair[Momentum[p_,D],LorentzIndex[x_,D]] -> RuleValidation`RuleRequire[CTensorGeneral[{x,s},indexArray]]FVD[p,s]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GravitonFourGluonVertex[arguments___] /; !MemberQ[{13}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonFourGluonVertex] <> SymbolName[GravitonFourGluonVertex], {arguments}, {13}];

GravitonFourGluonVertexUncontracted[arguments___] /; !MemberQ[{13}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonFourGluonVertexUncontracted] <> SymbolName[GravitonFourGluonVertexUncontracted], {arguments}, {13}];

GravitonGluonGhostVertex[arguments___] /; !MemberQ[{4}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonGluonGhostVertex] <> SymbolName[GravitonGluonGhostVertex], {arguments}, {4}];

GravitonGluonGhostVertexUncontracted[arguments___] /; !MemberQ[{4}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonGluonGhostVertexUncontracted] <> SymbolName[GravitonGluonGhostVertexUncontracted], {arguments}, {4}];

GravitonGluonVertex[arguments___] /; !MemberQ[{8}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonGluonVertex] <> SymbolName[GravitonGluonVertex], {arguments}, {8}];

GravitonGluonVertexUncontracted[arguments___] /; !MemberQ[{8}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonGluonVertexUncontracted] <> SymbolName[GravitonGluonVertexUncontracted], {arguments}, {8}];

GravitonQuarkGluonVertex[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonQuarkGluonVertex] <> SymbolName[GravitonQuarkGluonVertex], {arguments}, {2}];

GravitonQuarkGluonVertexUncontracted[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonQuarkGluonVertexUncontracted] <> SymbolName[GravitonQuarkGluonVertexUncontracted], {arguments}, {2}];

GravitonThreeGluonVertex[arguments___] /; !MemberQ[{10}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonThreeGluonVertex] <> SymbolName[GravitonThreeGluonVertex], {arguments}, {10}];

GravitonThreeGluonVertexUncontracted[arguments___] /; !MemberQ[{10}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonThreeGluonVertexUncontracted] <> SymbolName[GravitonThreeGluonVertexUncontracted], {arguments}, {10}];

GravitonYMGhostVertex[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonYMGhostVertex] <> SymbolName[GravitonYMGhostVertex], {arguments}, {5}];

GravitonYMGhostVertexUncontracted[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonYMGhostVertexUncontracted] <> SymbolName[GravitonYMGhostVertexUncontracted], {arguments}, {5}];

End[];


EndPackage[];
