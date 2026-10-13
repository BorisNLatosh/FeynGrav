(* A bounded, typed spanning basis for one FeynGrav vertex at a time.
   Loading this companion performs no rule import, model initialisation or I/O. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];

ClearAll[faKinematicQ, faTypedPairQ, faBasisError, faBasisPut,
  faBasisMerge, faBasisMultiply, faBasisWalk, faExtractVertex, faAtom];
faKinematicQ[e_] := ! FreeQ[e, FeynCalc`Pair[__]];
faTypedPairQ[e_] := MatchQ[e,
  FeynCalc`Pair[FeynCalc`LorentzIndex[_, D], FeynCalc`LorentzIndex[_, D]] |
  FeynCalc`Pair[FeynCalc`Momentum[_, D], FeynCalc`LorentzIndex[_, D]] |
  FeynCalc`Pair[FeynCalc`LorentzIndex[_, D], FeynCalc`Momentum[_, D]] |
  FeynCalc`Pair[FeynCalc`Momentum[_, D], FeynCalc`Momentum[_, D]]];
faBasisError[reason_] := Throw[Failure["UnsupportedVertexBasis",
  <|"MessageTemplate" -> reason, "Stage" -> "VertexBasis"|>], "FGBasis"];
faBasisPut[out_, key_, val_, cap_] := Module[{v = Lookup[out, key, 0] + val},
  If[! FreeQ[v, FeynCalc`Pair[__]], faBasisError["A class coefficient contains Lorentz kinematics."]];
  If[Length[out] + Boole[! KeyExistsQ[out, key]] > cap,
    faBasisError["The finite vertex basis exceeded its component limit."]];
  Append[out, key -> v]
];
faBasisMerge[acc_, rhs_, cap_] := Module[{out = acc},
  KeyValueMap[Function[{key, value}, out = faBasisPut[out, key, value, cap]], rhs]; out];
faBasisMultiply[a_, b_, cap_] := Module[{out = <||>, key},
  If[Length[a] Length[b] > cap^2, faBasisError["Vertex product exceeded the bounded operation limit."]];
  KeyValueMap[Function[{ka, va}, KeyValueMap[Function[{kb, vb},
    key = HoldComplete @@ Sort[Join[List @@ ka, List @@ kb]];
    out = faBasisPut[out, key, va vb, cap]], b]], a]; out
];
faBasisWalk[e_, cap_] := Which[
  faTypedPairQ[e], <|HoldComplete[e] -> 1|>,
  MatchQ[e, FeynCalc`Pair[__]], faBasisError["Unknown FeynCalc Pair type or mixed Lorentz dimension."],
  ! faKinematicQ[e], <|HoldComplete[] -> e|>,
  Head[e] === Plus, Fold[faBasisMerge[#1, faBasisWalk[#2, cap], cap] &, <||>, List @@ e],
  Head[e] === Times, Fold[faBasisMultiply[#1, faBasisWalk[#2, cap], cap] &,
    <|HoldComplete[] -> 1|>, List @@ e],
  Head[e] === Power && IntegerQ[e[[2]]] && 0 <= e[[2]] <= 4,
    Nest[faBasisMultiply[#, faBasisWalk[e[[1]], cap], cap] &,
      <|HoldComplete[] -> 1|>, e[[2]]],
  True, faBasisError["Unsupported operation enclosing a Lorentz kinematic atom."]
];
faExtractVertex[e_, cap_Integer : 100000] /; 0 < cap <= 100000 := Catch[Module[
  {map, keys, coefficients, monomials},
  map = faBasisWalk[e, cap]; keys = Keys[map]; coefficients = Values[map];
  monomials = Times @@ List @@ # & /@ keys;
  If[! And @@ (FreeQ[#, FeynCalc`Pair[__]] & /@ coefficients),
    faBasisError["A class coefficient contains unclassified Lorentz kinematics."]];
  <|"Monomials" -> monomials, "Coefficients" -> coefficients,
    "TermCount" -> Length[map],
    "Reconstruction" -> Total[MapThread[Times, {monomials, coefficients}]]|>
], "FGBasis"];
faExtractVertex[___] := Failure["InvalidVertexBasisLimit",
  <|"MessageTemplate" -> "Basis limit must be a positive integer not exceeding 100000.", "Stage" -> "VertexBasis"|>];

(* The installed FeynCalc-patched FCPrepareFAAmp recognises these exact heads. *)
faAtom[FeynCalc`Pair[FeynCalc`LorentzIndex[a_, D], FeynCalc`LorentzIndex[b_, D]]] := Global`FAMetricTensor[a, b];
faAtom[FeynCalc`Pair[FeynCalc`Momentum[p_, D], FeynCalc`LorentzIndex[a_, D]]] := Global`FAFourVector[p, a];
faAtom[FeynCalc`Pair[FeynCalc`LorentzIndex[a_, D], FeynCalc`Momentum[p_, D]]] := Global`FAFourVector[p, a];
faAtom[FeynCalc`Pair[FeynCalc`Momentum[p_, D], FeynCalc`Momentum[q_, D]]] := Global`FAScalarProduct[p, q];
faTranslateMonomial[e_] := e /. p : FeynCalc`Pair[__] :> faAtom[p];

End[];
