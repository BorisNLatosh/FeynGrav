(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"ITensor`", "ITensor.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["GammaTensor`",{"FeynCalc`","ITensor`"}];


GammaTensor::usage =
"GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda],\[Rho],\[Sigma]]. \
The function returns (\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\[Lambda]\[Rho]\[Sigma]\)]\). \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\) = \[Kappa] (-\[ImaginaryI])\!\(\*SubscriptBox[\(p\), \(\[Lambda]\)]\) (\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\[Lambda]\[Rho]\[Sigma]\)]\)\!\(\*SubscriptBox[\(h\), \(\[Rho]\[Sigma]\)]\)(k).";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GammaTensor::usage = GammaTensor::usage <> " Supported signatures: GammaTensor[m, a, b, l, r, s]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[GammaTensor];

GammaTensor[m_, a_, b_, l_, r_, s_] :=
    RuleValidation`RuleCall[
        GammaTensor[m, a, b, l, r, s],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
1/2 ( MTD[l,a]RuleValidation`RuleRequire[ITensor[{b,m,r,s}]] + MTD[l,b]RuleValidation`RuleRequire[ITensor[{a,m,r,s}]] - MTD[l,m] RuleValidation`RuleRequire[ITensor[{a,b,r,s}]] ) //Contract
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GammaTensor[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GammaTensor] <> SymbolName[GammaTensor], {arguments}, {6}];

End[];


EndPackage[];
