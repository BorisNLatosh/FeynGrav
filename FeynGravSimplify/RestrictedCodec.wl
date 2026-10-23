(* ::Package:: *)
(* Shared restricted expression codec: validation, expression->JSON trees and
   validated JSON decoding. Moved unchanged from the donor adapter; it owns the
   shared failure/reject helpers used by all restricted modules. It defines
   functions only and never launches programs at load time. *)
Begin["FeynGrav`Simplification`Private`"];

failure[tag_, message_, data_: <||>] := Failure[tag,
    Join[<|"MessageTemplate" -> message|>, data]];
reject[tag_, message_, data_: <||>] := Throw[failure[tag, message, data], $failureTag];

(* The map holds original values, rather than reconstructing symbols from names.
   Roles are distinct, even when a symbol occurs as both a momentum and scalar. *)
encode[expression_] := Catch[Module[
    {entries = <||>, registry = <||>, count = 0, dimensions = {}, register,
     dimension, scalar, sourceQ, slot, pair, walk, denominator, result, index, momentum, routedMomentum},
    register[role_, value_] := Module[{key = HoldComplete[role, value], id},
        If[KeyExistsQ[registry, key], Return[registry[key]]];
        id = "fg" <> ToString[++count];
        AssociateTo[registry, key -> id];
        AssociateTo[entries, id -> <|"Role" -> role, "Value" -> With[{v = value}, HoldComplete[v]]|>];
        id
    ];
    dimension[d_] := If[d === 4 || d === System`D,
        If[!MemberQ[dimensions, d], AppendTo[dimensions, d]]; If[d === 4, {}, {"D"}],
        reject["UnsupportedDimension", "Only uniform four-dimensional or symbolic D-dimensional tensors are supported."]];
    sourceQ[x_] := Which[
        MatchQ[x, _Symbol | _Integer | _Rational], True,
        MemberQ[{Plus, Times, Power}, Head[x]], AllTrue[List @@ x, sourceQ],
        True, False
    ];
    index[x_, d_] := If[Head[x] === Symbol,
        Join[{"LorentzIndex", register["Index", x]}, dimension[d]],
        reject["UnsupportedIndex", "Lorentz indices must be symbols."]];
    momentum[x_, d_] := If[sourceQ[x],
        Join[{"Momentum", register["Momentum", x]}, dimension[d]],
        reject["UnsupportedMomentum", "Momentum sources must be commuting arithmetic in symbols and exact numbers.",
            <|"Stage" -> "Encode", "MomentumSource" -> With[{value = x}, HoldComplete[value]]|>]];
    slot[x_] := Which[
        MatchQ[x, FeynCalc`LorentzIndex[_, ___]] && MemberQ[{1, 2}, Length[x]],
            index[x[[1]], If[Length[x] === 1, 4, x[[2]]]],
        MatchQ[x, FeynCalc`Momentum[_, ___]] && MemberQ[{1, 2}, Length[x]],
            momentum[x[[1]], If[Length[x] === 1, 4, x[[2]]]],
        True, reject["UnsupportedPair", "Pair arguments must be LorentzIndex or Momentum objects."]
    ];
    pair[a_, b_] := <|"Tree" -> {"Pair", a, b},
        "Tensor" -> (First[a] === "LorentzIndex" || First[b] === "LorentzIndex")|>;
    (* An explicit scalar whitelist prevents unknown tensors from being silently
       atomized. Function arguments must themselves be tensor-free. *)
    scalar[x_] := Module[{args, value},
        If[!MemberQ[{Sin, Cos, Tan, Exp, Log, Abs, Conjugate,
            FeynCalc`A0, FeynCalc`A00, FeynCalc`B0, FeynCalc`B1, FeynCalc`B00,
            FeynCalc`B11, FeynCalc`C0, FeynCalc`D0}, Head[x]],
            reject["UnsupportedExpression", "An unsupported expression head was encountered.",
                <|"Head" -> ToString[Head[x], InputForm]|>]];
        args = walk /@ (List @@ x);
        If[AnyTrue[args, TrueQ[#["Tensor"]] &],
            reject["HiddenTensor", "Tensor expressions cannot be hidden inside scalar functions."]];
        (* Rebuild only notation, using the same validated tree decoder. *)
        value = Head[x] @@ Map[Function[arg, decodeTree[arg["Tree"],
            <|"Entries" -> entries, "Dimension" -> If[dimensions === {}, None, First[dimensions]]|>]], args];
        register["Scalar", value]
    ];
    routedMomentum[x_] := Which[
        Head[x] === FeynCalc`Momentum, slot[x]; True,
        MemberQ[{Plus, Times}, Head[x]], AllTrue[List @@ x, routedMomentum],
        sourceQ[x], True,
        True, False
    ];
    denominator[x_] := Module[{parts = List @@ x, p, m, converted},
        If[Head[x] === FeynCalc`FAD,
            If[parts === {}, reject["UnsupportedDenominator", "FAD must contain a propagator."]];
            converted = Map[Function[item,
                {p, m} = Which[!ListQ[item], {item, 0},
                    Length[item] === 2, item,
                    True, reject["UnsupportedDenominator", "Use FAD[p] or FAD[{p,m}] (possibly several propagators)."]];
                momentum[p, System`D];
                If[TrueQ[walk[m]["Tensor"]], reject["HiddenTensor", "A propagator mass must be scalar."]];
                FeynCalc`PropagatorDenominator[FeynCalc`Momentum[p, System`D], m]
            ], parts];
            Return[register["Scalar", FeynCalc`FeynAmpDenominator @@ converted]]
        ];
        If[parts === {}, reject["UnsupportedDenominator", "A propagator product cannot be empty."]];
        Scan[Function[item,
            If[Head[item] =!= FeynCalc`PropagatorDenominator || Length[item] =!= 2 ||
                !TrueQ[routedMomentum[item[[1]]]] || FreeQ[item[[1]], _FeynCalc`Momentum],
                reject["UnsupportedDenominator", "Only ordinary FeynCalc propagator denominators are supported."]];
            If[TrueQ[walk[item[[2]]]["Tensor"]], reject["HiddenTensor", "A propagator mass must be scalar."]]
        ], parts];
        register["Scalar", x]
    ];
    walk[x_] := Module[{h = Head[x], args, tensor, tree, a, b, n},
        Which[
            IntegerQ[x], tree = x; tensor = False,
            h === Rational, tree = {"Rational", Numerator[x], Denominator[x]}; tensor = False,
            h === Complex && FreeQ[x, _Real],
                tree = {"Complex", walk[Re[x]]["Tree"], walk[Im[x]]["Tree"]}; tensor = False,
            h === Symbol && !MemberQ[{True, False, Null, Indeterminate}, x],
                tree = If[x === System`D, "D", register["Scalar", x]]; tensor = False,
            MemberQ[{Plus, Times}, h],
                args = walk /@ (List @@ x); tree = Prepend[Lookup[args, "Tree"], If[h === Plus, "Plus", "Times"]];
                tensor = AnyTrue[args, TrueQ[#["Tensor"]] &],
            h === Power && Length[x] === 2,
                a = walk[x[[1]]]; b = walk[x[[2]]]; n = x[[2]];
                If[TrueQ[b["Tensor"]] || (TrueQ[a["Tensor"]] && !(IntegerQ[n] && 1 <= n <= 2147483647)),
                    reject["UnsupportedPower", "Tensor powers require a positive int32 integer exponent and a scalar exponent."]];
                tree = {"Power", a["Tree"], b["Tree"]}; tensor = a["Tensor"],
            h === FeynCalc`Pair && Length[x] === 2,
                Return[pair[slot[x[[1]]], slot[x[[2]]]]],
            MemberQ[{FeynCalc`MT, FeynCalc`MTD}, h] && Length[x] === 2,
                Return[pair[index[x[[1]], If[h === FeynCalc`MT, 4, System`D]],
                    index[x[[2]], If[h === FeynCalc`MT, 4, System`D]]]],
            MemberQ[{FeynCalc`FV, FeynCalc`FVD}, h] && Length[x] === 2,
                Return[pair[momentum[x[[1]], If[h === FeynCalc`FV, 4, System`D]],
                    index[x[[2]], If[h === FeynCalc`FV, 4, System`D]]]],
            MemberQ[{FeynCalc`SP, FeynCalc`SPD}, h] && MemberQ[{1, 2}, Length[x]],
                Return[pair[momentum[x[[1]], If[h === FeynCalc`SP, 4, System`D]],
                    momentum[x[[-1]], If[h === FeynCalc`SP, 4, System`D]]]],
            MemberQ[{FeynCalc`FAD, FeynCalc`FeynAmpDenominator}, h],
                tree = denominator[x]; tensor = False,
            AtomQ[x] || MemberQ[{List, Equal, Rule, RuleDelayed, Association}, h],
                reject["UnsupportedExpression", "Use an exact commuting scalar/Lorentz expression, not a container or equation."],
            True, tree = scalar[x]; tensor = False
        ];
        <|"Tree" -> tree, "Tensor" -> tensor|>
    ];
    result = walk[expression];
    If[Length[DeleteDuplicates[dimensions]] > 1,
        reject["MixedDimensions", "Mixed four-dimensional and D-dimensional objects are not supported."]];
    <|"Tree" -> result["Tree"], "Entries" -> entries,
        "Dimension" -> If[dimensions === {}, None, First[dimensions]]|>
], $failureTag];

(* Decode in two passes: validate the complete tree before releasing any node. *)
decode[text_String, data_Association] := Module[{tree},
    tree = Quiet[Check[ImportString[text, "RawJSON"], $Failed]];
    If[tree === $Failed, Return[failure["InvalidOutput", "Contractor did not return one valid JSON expression."]]];
    decodeTree[tree, data]
];
decodeTree[tree_, data_Association] := Catch[Module[
    {entries = data["Entries"], validate, build, slotQ, known, numericQ, tensorQ},
    known[id_, role_] := StringQ[id] && KeyExistsQ[entries, id] && entries[id]["Role"] === role;
    numericQ[x_] := IntegerQ[x] || (ListQ[x] && Length[x] === 3 && First[x] === "Rational" &&
        IntegerQ[x[[2]]] && IntegerQ[x[[3]]] && x[[3]] =!= 0);
    slotQ[x_] := ListQ[x] && MemberQ[{2, 3}, Length[x]] &&
        MemberQ[{"LorentzIndex", "Momentum"}, First[x]] &&
        known[x[[2]], If[First[x] === "LorentzIndex", "Index", "Momentum"]] &&
        If[Length[x] === 2, data["Dimension"] === 4,
            x[[3]] === "D" && data["Dimension"] === System`D];
    tensorQ[x_] := !FreeQ[x, {"LorentzIndex", ___}];
    validate[x_] := Which[
        IntegerQ[x], True,
        StringQ[x], x === "D" || known[x, "Scalar"],
        !ListQ[x] || Length[x] < 2 || !StringQ[First[x]], False,
        First[x] === "Pair", Length[x] === 3 && AllTrue[Rest[x], slotQ],
        MemberQ[{"Plus", "Times"}, First[x]], AllTrue[Rest[x], validate],
        First[x] === "Power", Length[x] === 3 && AllTrue[Rest[x], validate] &&
            !tensorQ[x[[3]]] && (!tensorQ[x[[2]]] || (IntegerQ[x[[3]]] && 1 <= x[[3]] <= 2147483647)),
        First[x] === "Rational", numericQ[x],
        First[x] === "Complex", Length[x] === 3 && AllTrue[Rest[x], numericQ],
        True, False
    ];
    If[!TrueQ[validate[tree]], reject["InvalidOutput", "The backend returned an unknown identifier or invalid expression structure."]];
    build[x_Integer] := x;
    build[x_String] := If[x === "D", System`D, ReleaseHold[entries[x]["Value"]]];
    build[x_List] := Switch[First[x],
        "Plus", Plus @@ (build /@ Rest[x]), "Times", Times @@ (build /@ Rest[x]),
        "Power", Power @@ (build /@ Rest[x]), "Rational", Rational @@ Rest[x],
        "Complex", Complex @@ (build /@ Rest[x]),
        "Pair", FeynCalc`Pair @@ (build /@ Rest[x]),
        "LorentzIndex", FeynCalc`LorentzIndex @@ Join[{ReleaseHold[entries[x[[2]]]["Value"]]}, If[Length[x] === 3, {System`D}, {}]],
        "Momentum", FeynCalc`Momentum @@ Join[{ReleaseHold[entries[x[[2]]]["Value"]]}, If[Length[x] === 3, {System`D}, {}]]
    ];
    build[tree]
], $failureTag];

(* Executable discovery is lazy, and an explicit selection never falls back. *)

End[];
