(* Small diagnostic model only; no physical one-loop support is implied. *)
$HistoryLength = 0; $LoadFeynArts = True; Get["FeynCalc`"];
FeynArts`$ModelPath = Prepend[FeynArts`$ModelPath, DirectoryName[$InputFileName]];
name = "fgfa_native_tu_ordered_20261012";
If[! TrueQ[FeynArts`InitializeModel[name, FeynArts`GenericModel -> name]], Print["FAIL: init"]; Quit[1]];
top = FeynArts`CreateTopologies[1, 1 -> 1, FeynArts`ExcludeTopologies -> {FeynArts`Tadpoles}];
ins = FeynArts`InsertFields[top, FeynArts`T[1] -> FeynArts`T[1], FeynArts`Model -> name, FeynArts`GenericModel -> name, FeynArts`InsertionLevel -> {FeynArts`Classes}, FeynArts`ExcludeParticles -> {FeynArts`T[1], FeynArts`S[1]}];
If[FreeQ[ins, FeynArts`FeynmanGraph], Print["FAIL: no insertion"]; Quit[2]];
fa = FeynArts`CreateFeynAmp[ins, FeynArts`PreFactor -> 1, FeynArts`Truncated -> True];
control = Block[{FeynArts`$FermionLines = False}, FeynArts`CreateFeynAmp[ins, FeynArts`PreFactor -> 1, FeynArts`Truncated -> True]];
fun[x_] := FCFAConvert[x, IncomingMomenta -> {p}, OutgoingMomenta -> {q}, LoopMomenta -> {ell}, Contract -> False, ChangeDimension -> D, List -> True];
a = fun[fa]; b = fun[control];
Print["CONVERTED_HEADS=", InputForm[Head /@ {a, b}]];
Print["FREE_OF_RESIDUAL_COUPLINGS=", FreeQ[{a, b}, _FeynArts`C | _FeynArts`G | _Global`FAPolarizationTensor]];
(* Dummy-index names may differ. Contract the deliberately simple control only. *)
result = TimeConstrained[Contract[First[a] + First[b], FCI -> True], 15, $TimedOut];
Print["CONTRACTED_SUM=", InputForm[result]];
Print["EXACT_SIGN_CONTROL=", TrueQ[result === 0]];
Print["NOTE: diagnostic loop excludes tadpoles; no complete two-point claim."];
If[result =!= 0, Quit[3]];
Quit[0];
