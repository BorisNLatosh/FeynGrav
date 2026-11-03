(* ::Package:: *)
(* ::Title:: *)
(* One-loop tensor decomposition *)

(* ::Text:: *)
(* Loaded in the private converter context. Inspection and coefficient decoding
   live here; the embedded FORM procedure performs all tensor combinatorics.
   No integral is evaluated and no Gram matrix is inverted. *)

(* ::Section:: *)
(* Coefficient vocabulary and non-executable serialisation *)
$tdImport = <||>;
tdCoefficientQ[x_CalcFormConverter`LoopTensorCoefficient] := Module[{a = List @@ x, ids, r, m, powers, dim},
    If[Length[a] =!= 5, Return[False]];
    {ids, r, m, powers, dim} = a;
    ListQ[ids] && ids =!= {} && AllTrue[ids, IntegerQ[#] && # >= 0 &] &&
    (ids === {0} || EvenQ[Count[ids, 0]]) &&
    ListQ[r] && ListQ[m] && ListQ[powers] && Length[r] === Length[m] === Length[powers] &&
    (r === {} || First[r] === 0) && AllTrue[r, # === 0 || physicalMomentumLabelQ[#] &] &&
    AllTrue[m, scalarQ] && AllTrue[powers, IntegerQ[#] && # > 0 &] &&
    Max[ids] <= Max[0, Length[r] - 1] &&
    (Head[dim] === Symbol || (IntegerQ[dim] && dim >= 2)) && dim =!= I
];
tdPaVeDataQ[ids_, inv_, masses_] := ListQ[ids] && ids =!= {} && ListQ[inv] && ListQ[masses] &&
    masses =!= {} && Length[inv] === Length[masses] (Length[masses]-1)/2 &&
    AllTrue[Join[inv, masses], scalarQ] && AllTrue[ids, IntegerQ[#] && 0 <= # < Length[masses] &] &&
    (ids === {0} || EvenQ[Count[ids, 0]]);
tdCoefficientQ[x_PaVe] := Module[{a = List @@ x, plain, opts},
    plain = DeleteCases[a, _Rule]; opts = Cases[a, _Rule];
    Length[plain] >= 3 && tdPaVeDataQ[Drop[plain, -2], plain[[-2]], plain[[-1]]] &&
    DuplicateFreeQ[First /@ opts] &&
    AllTrue[opts, MemberQ[{PaVeAutoOrder, PaVeAutoReduce}, First[#]] && MemberQ[{True, False}, Last[#]] &]
];
(*The existing reversible A0--D0 codec also serves represented-integral
   inputs. Validation remains strict about their scalar arguments and arity.*)
tdCoefficientQ[x : (_A0 | _B0 | _C0 | _D0)] := scalarQ[x];
tdCoefficientQ[_] := False;
AssociateTo[$kindSpecs, "IntegralCoefficient" -> <|"Prefix" -> "cfic", "Declaration" -> "Symbols", "ValidExpression" -> tdCoefficientQ|>];
encode[x_CalcFormConverter`LoopTensorCoefficient] := If[tdCoefficientQ[x],
    {"LoopTensorCoefficient", encode /@ x[[1]], encode /@ x[[2]], encode /@ x[[3]], encode /@ x[[4]], encode[x[[5]]]},
    throwFailure["UnsupportedCoefficient", "Invalid formal loop coefficient."]];
decode[{"LoopTensorCoefficient", i_List, r_List, m_List, p_List, d_}] := Module[{x},
    x = CalcFormConverter`LoopTensorCoefficient[decode /@ i, decode /@ r, decode /@ m, decode /@ p, decode[d]];
    If[tdCoefficientQ[x], x, throwFailure["InvalidMapping", "Invalid formal loop coefficient."]]
];
encode[x_PaVe] := If[tdCoefficientQ[x], {"TensorPaVe", encode /@ DeleteCases[List @@ x, _Rule][[;; -3]],
    encode /@ DeleteCases[List @@ x, _Rule][[-2]], encode /@ DeleteCases[List @@ x, _Rule][[-1]]},
    throwFailure["UnsupportedCoefficient", "Invalid PaVe coefficient."]];
decode[{"TensorPaVe", ids_List, inv_List, masses_List}] := With[{i = decode /@ ids, v = decode /@ inv, m = decode /@ masses},
    If[!tdPaVeDataQ[i, v, m],
        throwFailure["InvalidMapping", "Invalid PaVe coefficient."]];
    PaVe[Sequence @@ i, v, m, PaVeAutoOrder -> False, PaVeAutoReduce -> False]
];

(* ::Section:: *)
(* Export inspection and deterministic denominator families *)
(* Private export-only support sets. A support lists denominator identifiers
   that can coexist, not their powers. Traverse sums/products without expanding
   the numerator. If combinatorics exceed 256 alternatives, use their union:
   extra planning is safe, whereas dropping an alternative would lose rules. *)
tdDenominatorSupports[expression_, name_] := Module[{walk, combine, bounded, all},
    all[x_] := Union[Flatten[(name /@ (List @@ #)) & /@ Cases[x, _FeynAmpDenominator, {0, Infinity}]]];
    bounded[sets_] := If[Length[sets] > 256, {Union @@ sets}, Union[sets]];
    combine[a_, b_] := If[Length[a] Length[b] > 256,
        {Union[Flatten[a], Flatten[b]]},
        bounded[Flatten[Table[Union[x, y], {x, a}, {y, b}], 1]]];
    walk[x_] := Which[
        FreeQ[x, _FeynAmpDenominator], {{}},
        Head[x] === FeynAmpDenominator, {Union[name /@ (List @@ x)]},
        Head[x] === Plus, Fold[bounded[Join[#1, #2]] &, {}, walk /@ (List @@ x)],
        Head[x] === Times, Fold[combine, {{}}, walk /@ (List @@ x)],
        Head[x] === Power && IntegerQ[x[[2]]] && x[[2]] > 0,
            (* Repeated products can mix alternatives: (A+B)^2 includes AB.
               At most the number of distinct denominators is needed to cover
               every support; excess powers do not add denominator slots. *)
            With[{base = walk[x[[1]]]}, Nest[combine[#, base] &, {{}},
                Min[x[[2]], Length[all[x[[1]]]]]]],
        True, {all[x]}
    ];
    walk[expression]
];

(* Derive routing from the authoritative denominator expression, not its
   redundant descriptive fields. Export and import share this normalisation. *)
tdNormaliseDenominators[entries_, q_] := Module[
    {denominators, route, mass, pd, a, r, key, slot, row, keys = {}, rows = {}, aliases = {}},
    denominators = Select[entries, #["Kind"] === "Denominator" &];
    Scan[Function[e,
        pd = decode[e["Expression"]][[1]];
        route = pd[[1]] /. Momentum[x_, ___] :> x; mass = If[Length[pd] === 1, 0, pd[[2]]];
        If[!FreeQ[mass, q], throwFailure["UnsupportedLoopDependence", "A propagator mass depends on the integration variable."]];
        a = Coefficient[route, q];
        If[a =!= 0,
            If[!MatchQ[a, _Integer | _Rational], throwFailure["UnsupportedLoopRouting", "Loop routing requires an exact rational coefficient."]];
            r = Expand[route/a - q];
            key = {r, mass^2/a^2}; slot = FirstPosition[keys, key, Missing["New"]];
            If[MissingQ[slot], AppendTo[keys, key]; slot = {Length[keys]};
                row = <|"Chord" -> encode[r], "MassSquared" -> encode[mass^2/a^2]|>; AppendTo[rows, row]];
            AppendTo[aliases, <|"Name" -> e["Name"], "Slot" -> First[slot], "Scale" -> a|>]
        ]], denominators];
    {rows, aliases}
];

tdPrepare[data_Association, enabled_, drop_] := Module[
    {mapping = data["Mapping"], loops, q, qname, entries, rows, aliases, meta, supports, slots, result = data},
    If[!MemberQ[{True, False}, enabled] || !MemberQ[{True, False}, drop],
        throwFailure["InvalidOption", "TensorDecomposition and DropScaleless must be True or False."]];
    loops = decode /@ mapping["LoopMomenta"];
    If[!enabled || loops === {}, Return[data]];
    If[Length[loops] =!= 1, throwFailure["UnsupportedLoopConfiguration", "Tensor decomposition requires exactly one declared loop momentum."]];
    q = First[loops]; entries = mapping["Entries"];
    qname = First[Select[entries, #["Kind"] === "Vector" && decode[#["Expression"]] === q &]]["Name"];
    (* Loop dependence may never hide in a scalar abbreviation, mass or
       coefficient already representing a different integral. Colour labels
       occupy a distinct index space even when they share the loop symbol. *)
    Scan[Function[e, If[!MemberQ[Join[{"Vector", "Index", "Denominator", "ColourTensor", "ColourWord"}, $caKinds], e["Kind"]] && !FreeQ[decode[e["Expression"]], q],
        throwFailure["UnsupportedLoopDependence", "Loop dependence occurs outside polynomial Lorentz objects."]];
        If[e["Kind"] === "Vector" && !FreeQ[decode[e["Expression"]], q] && decode[e["Expression"]] =!= q,
            throwFailure["UnsupportedLoopDependence", "Loop-dependent polarisation vectors are unsupported."]]], entries];
    {rows, aliases} = tdNormaliseDenominators[entries, q];
    supports = Lookup[data, "DenominatorSupports", Automatic];
    If[ListQ[supports],
        slots = Association[(#["Name"] -> #["Slot"]) & /@ aliases];
        supports = Union[(Union[(Lookup[slots, #, Nothing] &) /@ #]) & /@ supports]];
    AssociateTo[result, "LoopFamilySupports" -> supports];
    meta = <|"LoopMomentum" -> encode[q], "Normalisation" -> "IntegralEqualsIPiSquaredTimesCoefficient",
        "Basis" -> "DistinctSymmetricPlacements", "Families" -> rows, "DropScaleless" -> drop|>;
    AssociateTo[mapping, {"AlgebraVersion" -> mapping["Version"], "Version" -> 6,
        "TensorDecomposition" -> meta, "Processing" -> "OneLoopTensorDecomposition", "ResultLayout" -> "IntegralCoefficientGroups"}];
    AssociateTo[result, "Mapping" -> mapping];
    Join[result, tdProgram[qname, rows, aliases, entries]]
];

(* Only rational linear combinations enter this formatter. Their vector names
   were already allocated and validated by the ordinary exporter. *)
tdVectorText[x_, entries_, index_: None] := Module[{terms, lookup},
    If[x === 0, Return["0"]];
    lookup = Association[(decode[#["Expression"]] -> #["Name"]) & /@ Select[entries, #["Kind"] === "Vector" &]];
    terms = If[Head[x] === Plus, List @@ x, {x}];
    "(" <> StringRiffle[Map[Function[t, Module[{v, c},
        v = First[Select[If[Head[t] === Times, List @@ t, {t}], Head[#] === Symbol &]];
        c = t/v;
        "(" <> ToString[c, InputForm] <> ")*" <> lookup[v] <> If[index === None, "", "(" <> index <> ")"]]], terms], "+"] <> ")"
];

(* ::Section:: *)
(* Embedded FORM recurrence *)
tdProgram[qname_, rows_, aliases_, entries_] := Module[
    {n = Length[rows], vars, pattern, zeros, code, anchor, j, cond, shift, chord, branch,
     common, decl},
    vars = Table["cfcTN" <> IntegerString[k], {k, n}];
    pattern = StringRiffle[(# <> "?") & /@ vars, ","];
    zeros = StringRiffle[ConstantArray["0", n], ","];
    decl = "\nSymbols cfcTPow" <> If[n === 0, "", "," <> StringRiffle[vars, ","]] <> ";\n" <>
        "Indices cfcTX,cfcTY;\nCTensors cfcTensor,cfcRest,cfcNext,cfcMetric;\n" <>
        "CFunctions cfcFamily,cfcLabels,cfcLabel,cfcTC,cfcZeros;\n";
    code = "\n* Tensor decomposition: normalise denominator powers, then shift each family.\n.sort\nMultiply cfcFamily(" <> zeros <> ");\n";
    Scan[Function[e, Module[{rhs = vars, k = e["Slot"], scale},
        rhs[[k]] = rhs[[k]] <> "+cfcTPow";
        scale = "(" <> ToString[e["Scale"]^2, InputForm] <> ")^(-cfcTPow)";
        code = code <> "id " <> e["Name"] <> "^cfcTPow?pos_*cfcFamily(" <> pattern <> ")=" <> scale <> "*cfcFamily(" <> StringRiffle[rhs, ","] <> ");\n"
    ]], aliases];
    common = "ToTensor functions " <> qname <> ",cfcTensor;\nif (count(cfcTensor,1)==0);\nMultiply cfcTensor();\nendif;\nMultiply cfcLabels();\n";
    (* Pick the first surviving denominator as anchor. Conditional recurrence
       includes precisely the active chords. Select metric slots in one native
       distrib_ call and pair them with dd_, rather than recursively generating
       equivalent pairing paths. Symmetric slot sorting makes repeated vector
       arguments adjacent, allowing distrib_ to combine their multiplicities.
       Remaining slots receive active chords through the ordered recurrence. *)
    Do[
        (* An explicit wildcard is needed for every remaining scalar argument. *)
        cond = Table[If[k < anchor, "0", vars[[k]] <> If[k === anchor, "?pos_", "?"]], {k, n}];
        shift = decode[rows[[anchor]]["Chord"]];
        branch = "if (match(cfcFamily(" <> StringRiffle[cond, ","] <> ")));\n" <>
            If[shift === 0, "", "id " <> qname <> "=" <> qname <> "-" <> tdVectorText[shift, entries] <> ";\n"] <> common <>
            "Symmetrize cfcTensor;\n" <>
            "id cfcTensor(?a)*cfcLabels()=sum_(cfcTPow,0,nargs_(?a),2,distrib_(1,cfcTPow,cfcMetric,cfcRest,?a)*cfcZeros(cfcTPow));\n" <>
            "id cfcMetric(?a)=dd_(?a);\nrepeat;\nid cfcZeros(cfcTPow?pos_,?a)=cfcZeros(cfcTPow-1,?a,0);\nendrepeat;\nid cfcZeros(0,?a)=cfcLabels(?a);\nid cfcRest(?a)=cfcTensor(?a);\n" <>
            "repeat;\nid once cfcTensor(cfcTX?,?a)*cfcLabels(?b)=" <>
            "cfcNext(cfcTX)*cfcTensor(?a)*cfcLabels(?b);\n";
        Do[If[j =!= anchor,
            cond = MapIndexed[#1 <> If[First[#2] === j, "?pos_", "?"] &, vars];
            chord = decode[rows[[j]]["Chord"]] - shift;
            If[chord =!= 0, branch = branch <>
                "if (match(cfcFamily(" <> StringRiffle[cond, ","] <> ")));\n" <>
                "id cfcNext(cfcTX?)=cfcNext(cfcTX)+" <> tdVectorText[Expand[chord], entries, "cfcTX"] <> "*cfcLabel(" <> IntegerString[j] <> ");\nendif;\n"]], {j, n}];
        branch = branch <> "id cfcNext(cfcTX?)=0;\nid cfcLabels(?a)*cfcLabel(cfcTPow?)=cfcLabels(?a,cfcTPow);\n" <>
            "endrepeat;\nid cfcTensor()=1;\nendif;\n";
        code = code <> branch,
        {anchor, n}];
    (* Vacuum polynomial terms have no external chords. dd_ gives the exact
       unweighted metric sum and vanishes for an odd number of slots. *)
    code = code <> "if (match(cfcFamily(" <> zeros <> ")));\n" <> common <>
        "id cfcTensor(?a)*cfcLabels()=dd_(?a)*cfcLabels(nargs_(?a));\n" <>
        "id cfcLabels(cfcTPow?)=cfcTC(-1,cfcTPow);\n" <>
        "id cfcTC(-1,cfcTPow?odd_)=0;\nid cfcFamily(" <> zeros <> ")=1;\nendif;\nSymmetrize cfcLabels;\n" <>
        "id cfcFamily(?a)*cfcLabels(?b)=cfcTC(?a,-2,?b);\n.sort\n";
    <|"TensorDeclarations" -> decl <> "#procedure cfcTensorDecompose\n" <> code <> "#endprocedure\n", "TensorProcessing" -> "#call cfcTensorDecompose\n"|>
];

(* ::Section:: *)
(* Version-six validation and coefficient reconstruction *)
tdAlgebraMapping[mapping_] := If[!MemberQ[{6,7,8},mapping["Version"]],
    If[Lookup[mapping, "ResultLayout", None] === "IntegralCoefficientGroups" || KeyExistsQ[mapping,"LoopCoefficientReduction"],
        throwFailure["InvalidMapping", "Integral coefficient groups require version six or seven."]]; mapping,
    If[!MemberQ[Range[5], Lookup[mapping, "AlgebraVersion", None]] ||
        Lookup[mapping, "ResultLayout", None] =!= "IntegralCoefficientGroups" ||
        (Lookup[mapping, "AlgebraVersion", None] === 2 && !KeyExistsQ[mapping, "EpsilonConvention"]),
        throwFailure["InvalidMapping", "Invalid tensor-decomposition format metadata."]];
    Join[mapping, <|"Version" -> mapping["AlgebraVersion"]|>]
];
tdImportSetup[mapping_, dim_] := Module[{meta, rows, q, expected, saved, reduction},
    If[!MemberQ[{6,7,8},mapping["Version"]], Return[Null]];
    rcValidateKinematics[mapping,dim];
    meta = Lookup[mapping, "TensorDecomposition", None];
    If[!AssociationQ[meta] || Lookup[meta, "Normalisation", None] =!= "IntegralEqualsIPiSquaredTimesCoefficient" ||
        Lookup[meta, "Basis", None] =!= "DistinctSymmetricPlacements" ||
        !MemberQ[{True, False}, Lookup[meta, "DropScaleless", None]],
        throwFailure["InvalidMapping", "Invalid tensor coefficient conventions."]];
    saved = Lookup[meta, "Mode", "Integral"] === "ExistingCoefficients";
    If[saved =!= (If[mapping["Version"]===8,mapping["ScalarMasterReduction"]["SourceVersion"],mapping["Version"]] === 7), throwFailure["InvalidMapping","The coefficient interpretation does not match its format version."]];
    If[!MemberQ[{"Integral","ExistingCoefficients"}, Lookup[meta,"Mode","Integral"]], throwFailure["InvalidMapping","Invalid coefficient interpretation."]];
    reduction = Lookup[mapping,"LoopCoefficientReduction",None];
    If[reduction =!= None && (!AssociationQ[reduction] || Lookup[reduction,"Method",None] =!= "PartialFractionsAndFixedDimensionIBP" ||
        Lookup[reduction,"MaximumDistinctChords",None] =!= 4 ||
        !MemberQ[{True,False},Lookup[reduction,"DenominatorGroups",False]] ||
        (TrueQ[Lookup[reduction,"DenominatorGroups",False]] && !TrueQ[Lookup[reduction,"CoefficientBlocks",False]]) ||
        !MemberQ[{True,False},Lookup[reduction,"IncrementalSharedDefinitions",False]] ||
        (TrueQ[Lookup[reduction,"IncrementalSharedDefinitions",False]] &&
            !(TrueQ[Lookup[reduction,"DenominatorGroups",False]] && TrueQ[Lookup[reduction,"SharedCoefficientBlocks",False]])) ||
        !MemberQ[{True,False},Lookup[reduction,"SharedCoefficientBlocks",False]] ||
        (TrueQ[Lookup[reduction,"SharedCoefficientBlocks",False]] && (mapping["AlgebraVersion"] =!= 1 || !TrueQ[Lookup[reduction,"CoefficientBlocks",False]])) ||
        !MemberQ[{True,False},Lookup[reduction,"CoefficientBlocks",False]] || !ListQ[Lookup[reduction,"SingularSubfamilies",None]]),
        throwFailure["InvalidMapping","Invalid loop coefficient reduction metadata."]];
    If[saved && (reduction === None || Lookup[meta,"LoopMomentum",None] =!= Null || mapping["LoopMomenta"] =!= {}),
        throwFailure["InvalidMapping","Invalid saved coefficient mode."]];
    q = If[saved,None,decode[Lookup[meta, "LoopMomentum", None]]]; rows = Lookup[meta, "Families", None];
    If[Head[q] =!= Symbol || !ListQ[rows] || !AllTrue[rows, AssociationQ],
        throwFailure["InvalidMapping", "Invalid integral family dictionary."]];
    expected = Catch[If[saved,{lrSavedRows[mapping["Entries"]],{}},tdNormaliseDenominators[mapping["Entries"], q]], $failureTag];
    If[FailureQ[expected] || rows =!= First[expected], throwFailure["InvalidMapping", "Integral families disagree with the denominator dictionary."]];
    If[reduction =!= None,
        If[!ListQ[Lookup[reduction,"Invariants",None]] ||
            Length[reduction["Invariants"]] =!= Binomial[Length[rows],2] ||
            !AllTrue[reduction["SingularSubfamilies"], ListQ[#] && # =!= {} &&
                AllTrue[#, IntegerQ[#] && 1 <= # <= Length[rows] &] && # === Sort[DeleteDuplicates[#]] &],
            throwFailure["InvalidMapping","Invalid reduction invariant or subfamily dictionary."]];
        If[reduction["Invariants"] =!= lrInvariantSnapshot[rows,dim],
            throwFailure["CoefficientConventionMismatch","Scalar-product definitions changed since coefficient reduction was exported; re-export with the current kinematics."]]];
    rows = Map[Function[r, {decode[Lookup[r, "Chord", None]], decode[Lookup[r, "MassSquared", None]]}], rows];
    If[!DuplicateFreeQ[rows] || !AllTrue[rows, (#[[1]] === 0 || physicalMomentumLabelQ[#[[1]]] || (mapping["Version"]===8 && smChordQ[#[[1]]])) && scalarQ[#[[2]]] && (saved || FreeQ[#, q]) &],
        throwFailure["InvalidMapping", "Invalid integral family routing or mass."]];
    If[!saved && (decode /@ Lookup[mapping, "LoopMomenta", {}]) =!= {q}, throwFailure["InvalidMapping", "Inconsistent integration variable metadata."]];
    $tdImport = <|"Rows" -> rows, "LoopMomentum" -> q, "ExistingCoefficients" -> saved,
        "DenominatorGroups" -> (AssociationQ[reduction] && TrueQ[Lookup[reduction,"DenominatorGroups",False]]),
        "IncrementalSharedDefinitions" -> (AssociationQ[reduction] && TrueQ[Lookup[reduction,"IncrementalSharedDefinitions",False]]),
        "ReductionCoefficientBlocks" -> (AssociationQ[reduction] && TrueQ[Lookup[reduction,"CoefficientBlocks",False]]),
        "DimensionName" -> Lookup[Select[mapping["Entries"],
            #["Kind"] === "Scalar" && #["Expression"] === mapping["Dimension"] &], "Name", {}],
        "LoopDenominators" -> Lookup[Select[mapping["Entries"], #["Kind"] === "Denominator" && !FreeQ[decode[#["Expression"]], q] &], "Name", {}],
        "Dimension" -> dim, "DropScaleless" -> meta["DropScaleless"]|>
];
(* Pre-integration compaction keeps rational dimension coefficients opaque in
   FORM. Reconstruct only exact polynomials in this job's Lorentz dimension;
   tensor arguments, other variables and zero divisors are invalid. No global
   simplification or convention-dependent cache is introduced here. *)
tdDimensionRatio[args_] := Module[{dim, polynomial},
    If[$tdImport === <||> || Length[args] =!= 2,
        throwFailure["InvalidResult", "Invalid dimension coefficient."]];
    dim = $tdImport["Dimension"];
    polynomial[x_] := Which[
        IntegerQ[x] || Head[x] === Rational, True,
        x === dim && Head[dim] === Symbol, True,
        Head[x] === Plus || Head[x] === Times, AllTrue[List @@ x, polynomial],
        Head[x] === Power, IntegerQ[x[[2]]] && x[[2]] >= 0 && polynomial[x[[1]]],
        True, False];
    If[!AllTrue[args, polynomial] || args[[2]] === 0,
        throwFailure["InvalidResult", "Dimension coefficients require exact polynomials and a nonzero denominator."]];
    args[[1]]/args[[2]]
];
(* Native PolyRatFun output is a particularly common scalar leaf. Recognise
   its integer-polynomial subset directly rather than running a recursive
   expression parser for every coefficient. Exact lexical coverage is required;
   all other spellings return to the general parser and its diagnostics. Only
   denominators are cached here, with an import-local bound of 512 entries. *)
tdFastDimensionRatio[text_String] := Module[
    {name, dim, body, comma, numerator, denominator, polynomial, pattern, terms},
    name = $tdImport["DimensionName"]; dim = $tdImport["Dimension"];
    If[Length[name] =!= 1 || Head[dim] =!= Symbol ||
        !StringEndsQ[text, ")"] ||
        !StringFreeQ[text, RegularExpression["[A-Za-z0-9_]\\s+[A-Za-z0-9_]"]], Return[$Failed]];
    name = First[name];
    body = StringReplace[StringTake[text, {11,-2}], WhitespaceCharacter -> ""];
    comma = StringPosition[body, ","];
    If[Length[comma] =!= 1 || !StringFreeQ[body,"("|")"], Return[$Failed]];
    pattern = RegularExpression["([+-]?)(?:([0-9]+)(?:\\*(" <> name <>
        ")(?:\\^([0-9]+))?)?|(" <> name <> ")(?:\\^([0-9]+))?)"];
    polynomial[t_String] := Module[{fields},
        terms = StringCases[t, pattern];
        If[terms === {} || StringJoin[terms] =!= t ||
            !AllTrue[Rest[terms], StringStartsQ[#, "+"|"-"] &], Return[$Failed]];
        fields = StringCases[t, pattern -> {"$1","$2","$3","$4","$5","$6"}];
        Total[Map[Function[f, If[f[[1]] === "-", -1, 1]*
            If[f[[2]] === "", 1, FromDigits[f[[2]]]]*
            dim^If[f[[3]] === "" && f[[5]] === "", 0,
                If[f[[4]] <> f[[6]] === "", 1, FromDigits[f[[4]] <> f[[6]]]]]], fields]]
    ];
    numerator = polynomial[StringTake[body, First[comma][[1]]-1]];
    If[numerator === $Failed, Return[$Failed]];
    body = StringDrop[body, First[comma][[1]]];
    denominator = Lookup[$tdRatioDenominators, body, None];
    If[denominator === None,
        denominator = polynomial[body];
        If[denominator === $Failed || denominator === 0, Return[$Failed]];
        If[Length[$tdRatioDenominators] >= 512, $tdRatioDenominators = <||>];
        If[StringLength[body] <= 512 && ByteCount[denominator] <= 16384,
            AssociateTo[$tdRatioDenominators, body -> denominator]]];
    numerator/denominator
];
tdCall[args_] := Module[{n, powers, labels, active},
    If[$tdImport === <||> || !AllTrue[args, IntegerQ], throwFailure["InvalidResult", "Invalid integral coefficient arguments."]];
    If[MatchQ[args, {-1, _Integer}],
        If[Last[args] < 0 || OddQ[Last[args]], throwFailure["InvalidResult", "Invalid vacuum tensor rank."]]; Return[tdToken @@ args]];
    n = Length[$tdImport["Rows"]];
    If[Length[args] < n + 1 || args[[n + 1]] =!= -2, throwFailure["InvalidResult", "Invalid coefficient family arity."]];
    powers = Take[args, n]; labels = Drop[args, n + 1]; active = Flatten[Position[powers, _?Positive]];
    If[!AllTrue[powers, # >= 0 &] || active === {} || !AllTrue[labels, # === 0 || MemberQ[Rest[active], #] &] ||
        OddQ[Count[labels, 0]] || labels =!= Sort[labels], throwFailure["InvalidResult", "Invalid coefficient powers or tensor labels."]];
    tdToken @@ args
];
(* Cyclic distances give FeynCalc/LoopTools invariant ordering without calling
   any private FeynCalc helper. No Gram determinants enter this construction. *)
tdInvariants[chords_, dim_] := Module[{n = Length[chords], square, pairs},
    square[x_] := lrSquare[x,dim];
    pairs = Flatten[Table[Table[{j + 1, Mod[j + k, n] + 1}, {j, 0, n - 1}], {k, 1, Floor[(n - 1)/2]}], 1];
    If[EvenQ[n], pairs = Join[pairs, Table[{j + 1, j + 1 + n/2}, {j, 0, n/2 - 1}]]];
    (square[chords[[#[[2]]]] - chords[[#[[1]]]]]) & /@ pairs
];
(* Repeated reduced blocks share many identical PaVe coefficients. Cache their
   reconstruction only within this import, with a bounded number of entries. *)
tdReconstruct[args_] := Module[{key = tdToken @@ args, value},
    If[KeyExistsQ[$tdCoefficientCache,key], Return[$tdCoefficientCache[key]]];
    value = tdReconstructUncached[args];
    If[Length[$tdCoefficientCache] < 2048, AssociateTo[$tdCoefficientCache,key -> value]];
    value
];
tdReconstructUncached[args_] := Module[{rows = $tdImport["Rows"], dim = $tdImport["Dimension"], n, powers, ids, active, chords, masses, inv, scaleless,
    norm = If[TrueQ[Lookup[$tdImport,"ExistingCoefficients",False]],1,I Pi^2]},
    If[First[args] === -1, Return[If[TrueQ[$tdImport["DropScaleless"]], 0,
        norm CalcFormConverter`LoopTensorCoefficient[If[Last[args] === 0, {0}, ConstantArray[0, Last[args]]], {}, {}, {}, dim]]]];
    n = Length[rows]; powers = Take[args, n]; ids = Drop[args, n + 1]; active = Flatten[Position[powers, _?Positive]];
    chords = Expand /@ (rows[[active, 1]] - rows[[First[active], 1]]);
    masses = rows[[active, 2]]; inv = tdInvariants[chords, dim];
    ids = ids /. Thread[Rest[active] -> Range[Length[active] - 1]];
    If[ids === {}, ids = {0}];
    scaleless = AllTrue[Join[masses, inv], # === 0 &];
    If[scaleless && TrueQ[$tdImport["DropScaleless"]], Return[0]];
    If[!FreeQ[chords,_smChord] && (scaleless || !AllTrue[powers[[active]],#===1&]),
        throwFailure["InvalidResult","An unresolved invariant-only coefficient cannot be reconstructed as physical momentum routing."]];
    norm If[!scaleless && dim === D && AllTrue[powers[[active]], # === 1 &],
        PaVe[Sequence @@ ids, inv, masses, PaVeAutoOrder -> False, PaVeAutoReduce -> False],
        CalcFormConverter`LoopTensorCoefficient[ids, chords, masses, powers[[active]], dim]]
];

(* Every nonzero additive group must contain exactly one integral token. Check
   before final reconstruction, so malformed output cannot return an integrand
   or conceal a power/product of coefficient tokens in an external factor. *)
tdValidateGroup[parsed_, factors_] := Module[{q = $tdImport["LoopMomentum"]},
    If[parsed === 0 || TrueQ[Lookup[$tdImport,"ExistingCoefficients",False]], Return[Null]];
    If[Count[factors, _tdToken | _?lrFactorQ] =!= 1 ||
        Count[Times @@ Select[factors,!lrFactorQ[#]&], _tdToken | _lrToken, {0, Infinity}] =!= Count[factors,_tdToken] ||
        !FreeQ[parsed, Momentum[v_, ___] /; !FreeQ[v, q]] ||
        AnyTrue[Cases[parsed, pgDen[name_] :> name, {0, Infinity}], MemberQ[$tdImport["LoopDenominators"], #] &],
        throwFailure["InvalidResult", "The result contains undecomposed loop dependence or an invalid integral group."]]
];
