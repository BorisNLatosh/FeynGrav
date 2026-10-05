(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
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

BeginPackage["QuadraticGravityVertex`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


QuadraticGravityVertex::usage =
"QuadraticGravityVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},m0,m2].";


VertexRicciSquare::usage = "";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
QuadraticGravityVertex::usage = QuadraticGravityVertex::usage <> " Supported signatures: QuadraticGravityVertex[gravitonParameters, m0, m2]; argument 1: flat list, block size 3, length 6 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
VertexRicciSquare::usage = VertexRicciSquare::usage <> " Supported signatures: VertexRicciSquare[gravitonParameters]; argument 1: flat list, block size 3, length 0 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

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


(* GR part *)


Clear[GRVertex];

GRVertex[indexArray_] :=
    RuleValidation`RuleCall[
        GRVertex[indexArray],
        {{1, "Array", 3, 6, Infinity}},
        (
Switch[ Length[indexArray]/3,
			0, 0,
			1, 0,
			_, I (Global`\[Kappa])^(Length[indexArray]/3-2) RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},RuleValidation`RuleRequire[TakeLorenzIndices[indexArray[[7;;]]]]]]FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2] (2 RuleValidation`RuleRequire[GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,indexArray[[1]],indexArray[[2]]]]RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,indexArray[[4]],indexArray[[5]]]] - 2 RuleValidation`RuleRequire[GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,indexArray[[1]],indexArray[[2]]]]RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]]] )
		]
        ), True
    ];


(* Curvature square *)


Clear[VertexCurvatureSquare];

VertexCurvatureSquare[gravitonParameters_] :=
    RuleValidation`RuleCall[
        VertexCurvatureSquare[gravitonParameters],
        {{1, "Array", 3, 0, Infinity}},
        (
Switch[ Length[gravitonParameters]/3 ,
			0, 0,
			1, 0,
			2, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]])FVD[gravitonParameters[[-1]],\[ScriptR]1](FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptS]1,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]),
			3, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]])FVD[gravitonParameters[[-1]],\[ScriptR]1](FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptS]1,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + 2 RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[ScriptR]3,\[ScriptS]3}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*3]]]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptM]](FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]]-FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]])( FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptR]2,\[ScriptR]3,\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptS]2,\[ScriptS]3,\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptR]2,\[ScriptS]2,\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptR]3,\[ScriptS]3,\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]]),
			_, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]])FVD[gravitonParameters[[-1]],\[ScriptR]1](FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptS]1,\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + 2 RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[ScriptR]3,\[ScriptS]3}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*3]]]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptM]](FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]]-FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]])( FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptR]2,\[ScriptR]3,\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptS]2,\[ScriptS]3,\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptR]2,\[ScriptS]2,\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptR]3,\[ScriptS]3,\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]]) + RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2,\[ScriptA]3,\[ScriptB]3,\[ScriptA]4,\[ScriptB]4,\[ScriptA]5,\[ScriptB]5,\[ScriptA]6,\[ScriptB]6}, RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*4]]]]]] (FVD[gravitonParameters[[-1-3*3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA]1,\[ScriptA]2,\[ScriptA]3,\[Lambda]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]]FVD[gravitonParameters[[-1-3*2]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptB]1,\[ScriptB]2,\[ScriptB]3,\[Lambda]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] - FVD[gravitonParameters[[-1-3*3]],\[Lambda]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptA]1,\[ScriptA]2,\[ScriptB]2,\[Lambda]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]]FVD[gravitonParameters[[-1-3*2]],\[Lambda]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptB]1,\[ScriptA]3,\[ScriptB]3,\[Lambda]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] )(FVD[gravitonParameters[[-1-3*1]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptA]4,\[ScriptA]5,\[ScriptA]6,\[Lambda]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]4]RuleValidation`RuleRequire[GammaTensor[\[ScriptB]4,\[ScriptB]5,\[ScriptB]6,\[Lambda]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptA]4,\[ScriptA]5,\[ScriptB]5,\[Lambda]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]4]RuleValidation`RuleRequire[GammaTensor[\[ScriptB]4,\[ScriptA]6,\[ScriptB]6,\[Lambda]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] )
		]
        ), True
    ];

(* Ricci square *)


Clear[VertexRicciSquare];

VertexRicciSquare[gravitonParameters_] :=
    RuleValidation`RuleCall[
        VertexRicciSquare[gravitonParameters],
        {{1, "Array", 3, 0, Infinity}},
        (
Switch[ Length[gravitonParameters]/3,
			0, 0,
			1, 0,
			2, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]] ]] ( FVD[gravitonParameters[[-1-3]],\[ScriptR]1] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] - FVD[gravitonParameters[[-1-3]],\[ScriptM]] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptS]1,\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] )( FVD[gravitonParameters[[-1-3*0]],\[ScriptR]2] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]2,\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*0]],\[ScriptA]] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] ),
			3, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]] ]] ( FVD[gravitonParameters[[-1-3]],\[ScriptR]1] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] - FVD[gravitonParameters[[-1-3]],\[ScriptM]] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptS]1,\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] )( FVD[gravitonParameters[[-1-3*0]],\[ScriptR]2] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]2,\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*0]],\[ScriptA]] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] ) + 2 RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[ScriptR]3,\[ScriptS]3} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*3]]]] ]] ( FVD[gravitonParameters[[-1-3*2]],\[ScriptR]1] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] - FVD[gravitonParameters[[-1-3*2]],\[ScriptM]] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptS]1,\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] ) (FVD[gravitonParameters[[-1-3]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]3,\[ScriptR]2,\[ScriptA],\[ScriptL]2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]]FVD[gravitonParameters[[-1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]3,\[ScriptS]2,\[ScriptB],\[ScriptL]3,gravitonParameters[[-3]],gravitonParameters[[-2]]]] - FVD[gravitonParameters[[-1-3]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]3,\[ScriptR]2,\[ScriptS]2,\[ScriptL]2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]]FVD[gravitonParameters[[-1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]3,\[ScriptA],\[ScriptB],\[ScriptL]3,gravitonParameters[[-3]],gravitonParameters[[-2]]]]) ,
			_, RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*2]]]] ]] ( FVD[gravitonParameters[[-1-3]],\[ScriptR]1] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] - FVD[gravitonParameters[[-1-3]],\[ScriptM]] FVD[gravitonParameters[[-1-3]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptS]1,\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] )( FVD[gravitonParameters[[-1-3*0]],\[ScriptR]2] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]2,\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*0]],\[ScriptA]] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptS]2,\[ScriptB],\[ScriptL]2,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] ) + 2 RuleValidation`RuleRequire[CTensorGeneral[ {\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[ScriptR]3,\[ScriptS]3} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*3]]]] ]] ( FVD[gravitonParameters[[-1-3*2]],\[ScriptR]1] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptS]1,\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] - FVD[gravitonParameters[[-1-3*2]],\[ScriptM]] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]1] RuleValidation`RuleRequire[GammaTensor[\[ScriptR]1,\[ScriptS]1,\[ScriptN],\[ScriptL]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] ) (FVD[gravitonParameters[[-1-3]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]3,\[ScriptR]2,\[ScriptA],\[ScriptL]2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]]FVD[gravitonParameters[[-1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]3,\[ScriptS]2,\[ScriptB],\[ScriptL]3,gravitonParameters[[-3]],gravitonParameters[[-2]]]] - FVD[gravitonParameters[[-1-3]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]3,\[ScriptR]2,\[ScriptS]2,\[ScriptL]2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]]FVD[gravitonParameters[[-1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]3,\[ScriptA],\[ScriptB],\[ScriptL]3,gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + RuleValidation`RuleRequire[CTensorGeneral[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR]1,\[ScriptS]1,\[ScriptR]2,\[ScriptS]2,\[ScriptR]3,\[ScriptS]3,\[ScriptR]4,\[ScriptS]4} , RuleValidation`RuleRequire[TakeLorenzIndices[gravitonParameters[[;;-1-3*4]]]]]] (FVD[gravitonParameters[[-1-3*3]],\[ScriptL]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptR]1,\[ScriptM],\[ScriptL]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]2,\[ScriptS]1,\[ScriptN],\[ScriptL]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] - FVD[gravitonParameters[[-1-3*3]],\[ScriptL]1]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]2,\[ScriptR]1,\[ScriptS]1,\[ScriptL]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptL]2]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]2,\[ScriptM],\[ScriptN],\[ScriptL]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] ) (FVD[gravitonParameters[[-1-3*1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]4,\[ScriptR]3,\[ScriptA],\[ScriptL]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]4]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]4,\[ScriptS]3,\[ScriptB],\[ScriptL]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] - FVD[gravitonParameters[[-1-3*1]],\[ScriptL]3]RuleValidation`RuleRequire[GammaTensor[\[ScriptR]4,\[ScriptR]3,\[ScriptS]3,\[ScriptL]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]] FVD[gravitonParameters[[-1-3*0]],\[ScriptL]4]RuleValidation`RuleRequire[GammaTensor[\[ScriptS]4,\[ScriptA],\[ScriptB],\[ScriptL]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]] )
		]
        ), True
    ];


(* The vertex *)


Clear[QuadraticGravityVertexCore];

QuadraticGravityVertexCore[gravitonParameters_, m0_, m2_] :=
    RuleValidation`RuleCall[
        QuadraticGravityVertexCore[gravitonParameters, m0, m2],
        {{1, "Array", 3, 6, Infinity}, {2, "Expression"}, {3, "Expression"}},
        (
RuleValidation`RuleRequire[GRVertex[gravitonParameters]] + I (Global`\[Kappa])^(Length[gravitonParameters]/3) 1/(3 (Global`\[Kappa])^2) (2/m2^2+1/m0^2) RuleValidation`RuleRequire[VertexCurvatureSquare[gravitonParameters]]  - I (Global`\[Kappa])^(Length[gravitonParameters]/3) 2/(Global`\[Kappa])^2 1/m2^2 RuleValidation`RuleRequire[VertexRicciSquare[gravitonParameters]]
        ), True
    ];


Clear[QuadraticGravityVertex];

QuadraticGravityVertex[gravitonParameters_, m0_, m2_] :=
    RuleValidation`RuleCall[
        QuadraticGravityVertex[gravitonParameters, m0, m2],
        {{1, "Array", 3, 6, Infinity}, {2, "Expression"}, {3, "Expression"}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[QuadraticGravityVertexCore[#,m0,m2]]& ,
				Flatten/@Permutations[Partition[gravitonParameters,3]]
			]]
		]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GRVertex[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GRVertex] <> SymbolName[GRVertex], {arguments}, {1}];

QuadraticGravityVertex[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[QuadraticGravityVertex] <> SymbolName[QuadraticGravityVertex], {arguments}, {3}];

QuadraticGravityVertexCore[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[QuadraticGravityVertexCore] <> SymbolName[QuadraticGravityVertexCore], {arguments}, {3}];

TakeLorenzIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TakeLorenzIndices] <> SymbolName[TakeLorenzIndices], {arguments}, {1}];

VertexCurvatureSquare[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[VertexCurvatureSquare] <> SymbolName[VertexCurvatureSquare], {arguments}, {1}];

VertexRicciSquare[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[VertexRicciSquare] <> SymbolName[VertexRicciSquare], {arguments}, {1}];

End[];


EndPackage[];
