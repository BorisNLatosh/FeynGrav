(* ::Section:: *)
(*Dirac and colour translation: private companion to CalcFormConverter*)

(* ::Text:: *)
(*This file is loaded inside the converter's private context. It launches no
  processes and changes no FeynCalc definitions. Matrix words are typed until
  reconstruction: commuting coefficients must never erase matrix order.*)

(* ::Subsection:: *)
(*Vocabulary and reversible mapping*)

(* ::Input::Initialization:: *)
$diracImportLine = None;
$colourHeads = {SUNF, SUND, SUNDelta, SUNFDelta, SUNTF};
colourIndexQ[x_, h_] := Head[x] === h && Length[x] === 1 && Head[x[[1]]] === Symbol;
colourWordQ[dcColourWord[a_List]] := a =!= {} && AllTrue[a, colourIndexQ[#, SUNIndex] &];
colourWordQ[_] := False;
namedCouplingQ[x_] := MatchQ[x, SMP[_String]];
colourTensorQ[x_] := Switch[Head[x],
    SUNF | SUND, Length[x] === 3 && AllTrue[List @@ x, colourIndexQ[#, SUNIndex] &],
    SUNDelta, Length[x] === 2 && AllTrue[List @@ x, colourIndexQ[#, SUNIndex] &],
    SUNFDelta, Length[x] === 2 && AllTrue[List @@ x, colourIndexQ[#, SUNFIndex] &],
    SUNTF, Length[x] === 3 && ListQ[x[[1]]] &&
        AllTrue[x[[1]], colourIndexQ[#, SUNIndex] &] &&
        AllTrue[Rest[List @@ x], colourIndexQ[#, SUNFIndex] &],
    _, False
];

(* Colour entries contain data only. Separate tags distinguish a complete
   implicit generator word from commuting explicit matrix elements. *)
AssociateTo[$expressionSpecs, {
    "ColourWord" -> <|"Head" -> dcColourWord, "Arity" -> {1, 1}|>,
    "ColourIndexList" -> <|"Head" -> List, "Arity" -> {0, Infinity}|>,
    "SUNIndex" -> <|"Head" -> SUNIndex, "Arity" -> {1, 1}|>,
    "SUNFIndex" -> <|"Head" -> SUNFIndex, "Arity" -> {1, 1}|>,
    "SUNF" -> <|"Head" -> SUNF, "Arity" -> {3, 3}|>,
    "SUND" -> <|"Head" -> SUND, "Arity" -> {3, 3}|>,
    "SUNDelta" -> <|"Head" -> SUNDelta, "Arity" -> {2, 2}|>,
    "SUNFDelta" -> <|"Head" -> SUNFDelta, "Arity" -> {2, 2}|>,
    "SUNTF" -> <|"Head" -> SUNTF, "Arity" -> {3, 3}|>
}];
$expressionHeadByName = Map[#["Head"] &, $expressionSpecs];
$expressionNameByHead = Association[Reverse /@ Normal[$expressionHeadByName]];
AssociateTo[$kindSpecs, {
    "ColourTensor" -> <|"Prefix" -> "cfct", "Declaration" -> "Symbols", "ValidExpression" -> colourTensorQ|>,
    "ColourWord" -> <|"Prefix" -> "cfcw", "Declaration" -> "Symbols", "ValidExpression" -> colourWordQ|>,
    "NamedCoupling" -> <|"Prefix" -> "cfcp", "Declaration" -> "Symbols", "ValidExpression" -> namedCouplingQ|>
}];
encode[x_SMP] := If[namedCouplingQ[x], {"NamedCoupling", x[[1]]},
    throwFailure["UnsupportedScalar", "Only SMP[name_String] couplings are supported."]];
decode[{"NamedCoupling", name_String}] := SMP[name];

(* ::Subsection:: *)
(*Separate matrix spaces without global tensor expansion*)

(* ::Input::Initialization:: *)
(* FreeQ includes heads: literal symbols avoid a separate head-pattern match
   at every node of large bosonic inputs. *)
diracColourQ[x_] := !FreeQ[x, DiracTrace | DiracGamma | SUNTrace | SUNT | SUNTF | SUNF | SUND |
    SUNDelta | SUNFDelta | SMP | SUNN | CA | CF];

(* Counts are maxima across a sum: a Times must not multiply two implicit
   chains in the same space. Such products have no unambiguous spin-line meaning. *)
dcSignature[dcDiracWord[_List]] := {1, 0};
dcSignature[dcColourWord[_List]] := {0, 1};
dcSignature[x_Plus] := Max /@ Transpose[dcSignature /@ List @@ x];
dcSignature[x_Times] := Total[dcSignature /@ List @@ x];
dcSignature[_] := {0, 0};
dcTimes[a_List] := Module[{signature = Total[dcSignature /@ a]},
    If[AnyTrue[signature, # > 1 &],
        throwFailure["AmbiguousMatrixProduct", "Use Dot for matrices in the same implicit matrix space; Times does not identify independent chains."]];
    Times @@ a
];

(* Dot alone requires multilinearity. Each local term stores coefficient,
   ordered Dirac slots and ordered adjoint labels. No outer Plus/Times expands. *)
dcTerms[x_Plus] := Join @@ (dcTerms /@ List @@ x);
dcTerms[x_Times] := Fold[dcTermMultiply, {{1, {}, {}}}, dcTerms /@ List @@ x];
dcTerms[dcDiracWord[a_List]] := {{1, a, {}}};
dcTerms[dcColourWord[a_List]] := {{1, {}, a}};
dcTerms[x_] := {{x, {}, {}}};
dcTermMultiply[a_List, b_List] := Flatten[Table[
    {u[[1]] v[[1]], Join[u[[2]], v[[2]]], Join[u[[3]], v[[3]]]},
    {u, a}, {v, b}], 1];
dcDot[a_List] := Total[(#[[1]] If[#[[2]] === {} && FreeQ[a, _dcDiracWord], 1, dcDiracWord[#[[2]]]]
    If[#[[3]] === {}, 1, dcColourWord[#[[3]]]]) & /@
    Fold[dcTermMultiply, {{1, {}, {}}}, dcTerms /@ a]];

dcNormalize[x_DiracGamma] := Module[{slot, dimension},
    If[!MemberQ[{1, 2}, Length[x]], throwFailure["UnsupportedDirac", "Invalid DiracGamma arity."]];
    slot = x[[1]]; dimension = If[Length[x] === 1, 4, x[[2]]];
    If[!MatchQ[slot, _LorentzIndex | _Momentum],
        throwFailure["UnsupportedDirac", "Only ordinary Lorentz gamma matrices and slashes are supported; gamma-five and projectors are excluded."]];
    If[dimension =!= space[slot], throwFailure["MixedDimensions", "Gamma and slot dimensions must agree."]];
    dcDiracWord[{slot}]
];
dcNormalize[x_SUNT] := Module[{word = dcColourWord[List @@ x]},
    If[!colourWordQ[word], throwFailure["UnsupportedColour", "Invalid implicit colour generator word."]]; word
];
dcNormalize[x_Plus] := Plus @@ (dcNormalize /@ List @@ x);
dcNormalize[x_Times] := dcTimes[dcNormalize /@ List @@ x];
dcNormalize[x_Dot] := dcDot[dcNormalize /@ List @@ x];
dcNormalize[x_Power] := Module[{base = dcNormalize[x[[1]]]},
    If[dcSignature[base] =!= {0, 0}, throwFailure["AmbiguousMatrixProduct", "Matrix powers must be written as ordered Dot products."]];
    base^x[[2]]
];
dcNormalize[x_] := Which[
    MemberQ[$colourHeads, Head[x]], If[colourTensorQ[x], x,
        throwFailure["UnsupportedColour", "Invalid colour tensor arguments."]],
    Head[x] === SMP, If[namedCouplingQ[x], x, throwFailure["UnsupportedScalar", "Invalid named coupling."]],
    !FreeQ[x, _DiracGamma | _SUNT | _Dot],
        throwFailure["UnsupportedDirac", "Matrix objects are nested inside an unsupported head."],
    NonCommQ[x], throwFailure["UnsupportedDirac", "Unsupported noncommutative object."],
    True, x
];

(* ::Subsection:: *)
(*Native gamma serialization and typed reconstruction*)

(* ::Input::Initialization:: *)
(* Install matrix dispatch only for a Dirac/colour export. Legacy bosonic
   traversal retains its original local emitter and per-node dispatch cost. *)
dcConfigureEmitter[emit_Symbol, vector_Symbol, index_Symbol, register_Symbol] := (
    emit[x_dcDiracWord] := daEmitOpen[x, vector, index, emit];
    emit[x_daTrace] := daEmitTrace[x, vector, index, emit];
    emit[x_Power] /; !FreeQ[x[[1]], _daTrace] := daEmitTracePower[x, emit];
    emit[x_dcColourWord] := register["ColourWord", x];
    emit[x_SMP] := register["NamedCoupling", x];
    emit[x : (_SUNF | _SUND | _SUNDelta | _SUNFDelta | _SUNTF)] := register["ColourTensor", x];
);

(* The caller supplies its local vector/index allocators and scalar emitter. *)
dcEmitWord[dcDiracWord[slots_List], vector_, index_, emit_] := Module[{choices, terms},
    If[slots === {}, Return["gi_(1)"]];
    choices = If[Head[#] === LorentzIndex, {{1, index[#]}}, vector[#]] & /@ slots;
    terms = Tuples[choices];
    If[terms === {}, Return["0"]];
    "(" <> StringRiffle[("(" <> emit[Times @@ #[[All, 1]]] <>
        "*g_(1," <> StringRiffle[#[[All, 2]], ","] <> "))") & /@ terms, "+"] <> ")"
];
dcGammaCall[name_, args_List, dim_] := Module[{slots},
    If[args =!= {} && First[args] =!= 1, Return[daImportTrace[name, args, dim]]];
    If[$diracImportLine =!= 1 || args === {} || First[args] =!= 1,
        throwFailure["InvalidResult", "Undeclared Dirac spin line."]];
    If[name === "gi_",
        If[Length[args] =!= 1, throwFailure["InvalidResult", "Invalid Dirac identity."]];
        Return[diracWordToken[{}]]
    ];
    slots = Rest[args];
    If[slots === {} || !AllTrue[slots, MatchQ[#, _indexToken | _vectorToken] &],
        throwFailure["InvalidResult", "Invalid gamma word arguments or special gamma code."]];
    diracWordToken[slots /. {indexToken[x_] :> DiracGamma[LorentzIndex[x, dim], dim],
        vectorToken[x_] :> DiracGamma[Momentum[x, dim], dim]}]
];

dcResultSignature[diracWordToken[_List]] := {1, 0};
dcResultSignature[colourWordToken[_List]] := {0, 1};
dcResultSignature[x_Plus] := Max /@ Transpose[dcResultSignature /@ List @@ x];
dcResultSignature[x_Times] := Total[dcResultSignature /@ List @@ x];
dcResultSignature[_] := {0, 0};
dcResultProduct[a_List] := Module[{signature = Total[dcResultSignature /@ a]},
    If[AnyTrue[signature, # > 1 &], throwFailure["InvalidResult", "Multiple implicit matrix words in a result product."]];
    If[AnyTrue[Values[Merge[daResultLines /@ a, Total]], # > 1 &],
        throwFailure["InvalidResult", "A trace spin line occurs more than once in a result product."]];
    Times @@ a
];
dcResultPower[x_, n_] := If[dcResultSignature[x] =!= {0, 0} || !FreeQ[x, _daTraceToken] || (!FreeQ[x, _caTensor] && n < 0),
    throwFailure["InvalidResult", "A matrix or unprocessed trace word occurs in a scalar power or denominator."], x^n];
dcReconstruct[x_] := x /. {
    diracWordToken[a_List] :> If[a === {}, 1, If[Length[a] === 1, First[a], Dot @@ a]],
    daTraceToken[line_Integer, a_List, norm_] :> DiracTrace[If[a === {}, 1, If[Length[a] === 1, First[a], Dot @@ a]], TraceOfOne -> norm, DiracTraceEvaluate -> False],
    colourWordToken[a_List] :> If[Length[a] === 1, SUNT[First[a]], Dot @@ (SUNT /@ a)]
};
