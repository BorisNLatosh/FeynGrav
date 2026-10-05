(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"ITensor`", "ITensor.wl"},
                {"CTensorGeneral`", "CTensorGeneral.wl"},
                {"GammaTensor`", "GammaTensor.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonMassiveVectorVertex::usage =
"GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The expression for a vertex describing a coupling of n gravitons to the Proca field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; m is the Proca field mass.";


GravitonMassiveVectorVertexUncontracted::usage =
"GravitonMassiveVectorVertexUncontracted[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. \
The expression for a vertex describing a coupling of n gravitons to the Proca field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; m is the Proca field mass. No contraction or simplification is carried out.";


GravitonVectorVertex::usage =
"GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. \
The expression for a vertex describing a coupling of n gravitons to a massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(k\), \(i\)]\) is the momentum of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; \[CurlyEpsilon] is the gauge fixing parameter.";


GravitonVectorVertexUncontracted::usage =
"GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. \
The expression for a vertex describing a coupling of n gravitons to a massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(k\), \(i\)]\) is the momentum of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; \[CurlyEpsilon] is the gauge fixing parameter. No contraction or simplification is carried out.";


GravitonVectorGhostVertex::usage =
"GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. \
The expression for a vertex describing a coupling of n gravitons to a ghost for the massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a ghost.";


GravitonVectorGhostVertexUncontracted::usage =
"GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. \
The expression for a vertex describing a coupling of n gravitons to a ghost for the massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a ghost. No contraction or simplification is carried out.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GravitonMassiveVectorVertex::usage = GravitonMassiveVectorVertex::usage <> " Supported signatures: GravitonMassiveVectorVertex[indexArray, \\[Lambda]1, p1, \\[Lambda]2, p2, m]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonMassiveVectorVertexUncontracted::usage = GravitonMassiveVectorVertexUncontracted::usage <> " Supported signatures: GravitonMassiveVectorVertexUncontracted[indexArray, \\[Lambda]1, p1, \\[Lambda]2, p2, m]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonVectorVertex::usage = GravitonVectorVertex::usage <> " Supported signatures: GravitonVectorVertex[indexArray, \\[Lambda]1, p1, \\[Lambda]2, p2, \\[CurlyEpsilon]]; argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonVectorVertexUncontracted::usage = GravitonVectorVertexUncontracted::usage <> " Supported signatures: GravitonVectorVertexUncontracted[indexArray, \\[Lambda]1, p1, \\[Lambda]2, p2, \\[CurlyEpsilon]]; argument 1: flat list, block size 3, length 3 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonVectorGhostVertex::usage = GravitonVectorGhostVertex::usage <> " Supported signatures: GravitonVectorGhostVertex[indexArray, p1, p2]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonVectorGhostVertexUncontracted::usage = GravitonVectorGhostVertexUncontracted::usage <> " Supported signatures: GravitonVectorGhostVertexUncontracted[indexArray, p1, p2]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


(* Supplementary functions. *)


Clear[TakeLorenzIndices];

TakeLorenzIndices[indexArray_] :=
    RuleValidation`RuleCall[
        TakeLorenzIndices[indexArray],
        {{1, "Array", 3, 0, Infinity}},
        (
Flatten[RuleValidation`RuleRequire[(#[[;;2]]&)/@Partition[indexArray,3]]]
        ), False
    ];


Clear[FReduced];

FReduced[\[Mu]_, \[Nu]_, \[Sigma]_, \[Lambda]_] :=
    RuleValidation`RuleCall[
        FReduced[\[Mu], \[Nu], \[Sigma], \[Lambda]],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}},
        (
MTD[\[Mu],\[Sigma]]MTD[\[Nu],\[Lambda]] - MTD[\[Nu],\[Sigma]]MTD[\[Mu],\[Lambda]]
        ), False
    ];


(* Proca field. *)


Clear[GravitonMassiveVectorVertexUncontracted];

GravitonMassiveVectorVertexUncontracted[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_, m_] :=
    RuleValidation`RuleCall[
        GravitonMassiveVectorVertexUncontracted[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, m],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},indexArray]] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]RuleValidation`RuleRequire[FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]]RuleValidation`RuleRequire[FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2]] + m^2 RuleValidation`RuleRequire[CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray]] )
        ), True
    ];


Clear[GravitonMassiveVectorVertex];

GravitonMassiveVectorVertex[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_, m_] :=
    RuleValidation`RuleCall[
        GravitonMassiveVectorVertex[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, m],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray]] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]RuleValidation`RuleRequire[FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]]RuleValidation`RuleRequire[FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2]] + m^2 RuleValidation`RuleRequire[CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray]] ) //Contract
        ), True
    ];


(* Massless vector field. *)


Clear[GravitonVectorVertex1];

GravitonVectorVertex1[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertex1[indexArray, \[Lambda]1, p1, \[Lambda]2, p2],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray]] 1/2 FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]RuleValidation`RuleRequire[FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]]RuleValidation`RuleRequire[FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2]]
        ), True
    ];


Clear[GravitonVectorVertex3];

GravitonVectorVertex3[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertex3[indexArray, \[Lambda]1, p1, \[Lambda]2, p2],
        {{1, "Array", 3, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
(-1) RuleValidation`RuleRequire[CTensorGeneral[{\[Sigma]1,\[Lambda]1,\[Sigma]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]]]] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]
        ), True
    ];


Clear[GravitonVectorVertex4];

GravitonVectorVertex4[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertex4[indexArray, \[Lambda]1, p1, \[Lambda]2, p2],
        {{1, "Array", 3, 3, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
RuleValidation`RuleRequire[Map[
			( RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Nu],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[#[[4;;]]]]]] (RuleValidation`RuleRequire[GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],\[Sigma]]FVD[p2,\[Mu]2] + RuleValidation`RuleRequire[GammaTensor[\[Mu]2,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],\[Sigma]]FVD[p1,\[Mu]1]) )& ,
			Flatten/@Permutations[Partition[indexArray,3]]
		]]//Total
        ), True
    ];


Clear[GravitonVectorVertex5];

GravitonVectorVertex5[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertex5[indexArray, \[Lambda]1, p1, \[Lambda]2, p2],
        {{1, "Array", 3, 6, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
RuleValidation`RuleRequire[Map[
			( (-1/2) RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[#[[7;;]]]]]] (FVD[#[[3]],\[Tau]1]FVD[#[[6]],\[Tau]2]RuleValidation`RuleRequire[GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[1]],#[[2]]]] RuleValidation`RuleRequire[GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[4]],#[[5]]]] + FVD[#[[3]],\[Tau]2]FVD[#[[6]],\[Tau]1]RuleValidation`RuleRequire[GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[4]],#[[5]]]] RuleValidation`RuleRequire[GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[1]],#[[2]]]] ) )&,
			Flatten/@Permutations[Partition[indexArray,3]]
		]]//Total
        ), True
    ];


Clear[GravitonVectorVertex];

GravitonVectorVertex[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_, \[CurlyEpsilon]_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertex[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, \[CurlyEpsilon]],
        {{1, "Array", 3, 0, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
Switch[Length[indexArray]/3,
			0, I (Global`\[Kappa])^(Length[indexArray]/3) ( RuleValidation`RuleRequire[GravitonVectorVertex1[RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]],\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] ) //Contract,
			1, I (Global`\[Kappa])^(Length[indexArray]/3) ( RuleValidation`RuleRequire[GravitonVectorVertex1[RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]],\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] ) //Contract,
			_, I (Global`\[Kappa])^(Length[indexArray]/3) ( RuleValidation`RuleRequire[GravitonVectorVertex1[RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]],\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] + \[CurlyEpsilon] RuleValidation`RuleRequire[GravitonVectorVertex5[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]] ) //Contract
		]
        ), True
    ];


Clear[GravitonVectorVertexUncontracted];

(* The vertex input contains triples; C tensors require only the Lorentz-index pairs. *)
GravitonVectorVertexUncontracted[indexArray_, \[Lambda]1_, p1_, \[Lambda]2_, p2_, \[CurlyEpsilon]_] :=
    RuleValidation`RuleCall[
        GravitonVectorVertexUncontracted[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, \[CurlyEpsilon]],
        {{1, "Array", 3, 3, Infinity}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
Switch[Length[indexArray]/3,
			1, I (Global`\[Kappa])^(Length[indexArray]/3) ( RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]]]] 1/2 FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]RuleValidation`RuleRequire[FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]]RuleValidation`RuleRequire[FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2]] - \[CurlyEpsilon] RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Sigma]1,\[Lambda]1,Global`\[Sigma]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]]]] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2] + \[CurlyEpsilon] Total[RuleValidation`RuleRequire[ RuleValidation`RuleRequire[Map[ ( RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[#[[4;;]]]]]] (RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],Global`\[Sigma]]FVD[p2,Global`\[Mu]2] + RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]2,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],Global`\[Sigma]]FVD[p1,Global`\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ]] ]] ) ,
			_, I (Global`\[Kappa])^(Length[indexArray]/3) ( RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]]]] 1/2 FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]RuleValidation`RuleRequire[FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]]RuleValidation`RuleRequire[FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2]] - \[CurlyEpsilon] RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Sigma]1,\[Lambda]1,Global`\[Sigma]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray]]]] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2] + \[CurlyEpsilon] Total[RuleValidation`RuleRequire[ RuleValidation`RuleRequire[Map[ ( RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[#[[4;;]]]]]] (RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],Global`\[Sigma]]FVD[p2,Global`\[Mu]2] + RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]2,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]]FVD[#[[3]],Global`\[Sigma]]FVD[p1,Global`\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ]] ]] + \[CurlyEpsilon] Total[RuleValidation`RuleRequire[ RuleValidation`RuleRequire[Map[ ( (-1/2) RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Alpha],Global`\[Beta],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},RuleValidation`RuleRequire[TakeLorenzIndices[#[[7;;]]]]]] (FVD[#[[3]],Global`\[Tau]1]FVD[#[[6]],Global`\[Tau]2]RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Tau]1,#[[1]],#[[2]]]] RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]2,Global`\[Alpha],Global`\[Beta],Global`\[Tau]2,#[[4]],#[[5]]]] + FVD[#[[3]],Global`\[Tau]2]FVD[#[[6]],Global`\[Tau]1]RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Tau]1,#[[4]],#[[5]]]] RuleValidation`RuleRequire[GammaTensor[Global`\[Mu]2,Global`\[Alpha],Global`\[Beta],Global`\[Tau]2,#[[1]],#[[2]]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ]] ]] )
		]
        ), True
    ];


(* Faddeev-Popov ghosts for the massless vector field. *)


Clear[GravitonVectorGhostVertex];

GravitonVectorGhostVertex[indexArray_, p1_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorGhostVertex[indexArray, p1, p2],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}},
        (
- I (Global`\[Kappa])^(Length[indexArray]/2) FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]] RuleValidation`RuleRequire[CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray]] //Contract
        ), True
    ];


Clear[GravitonVectorGhostVertexUncontracted];

GravitonVectorGhostVertexUncontracted[indexArray_, p1_, p2_] :=
    RuleValidation`RuleCall[
        GravitonVectorGhostVertexUncontracted[indexArray, p1, p2],
        {{1, "Array", 2, 0, Infinity}, {2, "Expression"}, {3, "Expression"}},
        (
- I (Global`\[Kappa])^(Length[indexArray]/2) FVD[p1,Global`\[Mu]]FVD[p2,Global`\[Nu]] RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Nu]},indexArray]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
FReduced[arguments___] /; !MemberQ[{4}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[FReduced] <> SymbolName[FReduced], {arguments}, {4}];

GravitonMassiveVectorVertex[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonMassiveVectorVertex] <> SymbolName[GravitonMassiveVectorVertex], {arguments}, {6}];

GravitonMassiveVectorVertexUncontracted[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonMassiveVectorVertexUncontracted] <> SymbolName[GravitonMassiveVectorVertexUncontracted], {arguments}, {6}];

GravitonVectorGhostVertex[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorGhostVertex] <> SymbolName[GravitonVectorGhostVertex], {arguments}, {3}];

GravitonVectorGhostVertexUncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorGhostVertexUncontracted] <> SymbolName[GravitonVectorGhostVertexUncontracted], {arguments}, {3}];

GravitonVectorVertex[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertex] <> SymbolName[GravitonVectorVertex], {arguments}, {6}];

GravitonVectorVertex1[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertex1] <> SymbolName[GravitonVectorVertex1], {arguments}, {5}];

GravitonVectorVertex3[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertex3] <> SymbolName[GravitonVectorVertex3], {arguments}, {5}];

GravitonVectorVertex4[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertex4] <> SymbolName[GravitonVectorVertex4], {arguments}, {5}];

GravitonVectorVertex5[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertex5] <> SymbolName[GravitonVectorVertex5], {arguments}, {5}];

GravitonVectorVertexUncontracted[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVectorVertexUncontracted] <> SymbolName[GravitonVectorVertexUncontracted], {arguments}, {6}];

TakeLorenzIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TakeLorenzIndices] <> SymbolName[TakeLorenzIndices], {arguments}, {1}];

End[];


EndPackage[];
