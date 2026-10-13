(* Disposable RESOURCE-01-bounded, vertex-level research prototype. Never generate amplitudes here. *)
BeginPackage["FGBoundedBasis`"];
ExtractVertex::usage = "ExtractVertex[expr,maxTerms] returns a bounded spanning monomial basis and scalar coefficient vector, or Failure.";
FAAtom::usage = "FAAtom[internalPair] translates one D-dimensional FeynCalc kinematic atom to patched Global`FA... notation.";
EndPackage[];
Begin["`Private`"];
ClearAll[kinQ, typedQ, bad, put, merge, mul, walk, ExtractVertex, FAAtom];
kinQ[e_] := !FreeQ[e, FeynCalc`Pair[__]];
typedQ[e_] := MatchQ[e,
  FeynCalc`Pair[FeynCalc`LorentzIndex[_,D],FeynCalc`LorentzIndex[_,D]] |
  FeynCalc`Pair[FeynCalc`Momentum[_,D],FeynCalc`LorentzIndex[_,D]] |
  FeynCalc`Pair[FeynCalc`LorentzIndex[_,D],FeynCalc`Momentum[_,D]] |
  FeynCalc`Pair[FeynCalc`Momentum[_,D],FeynCalc`Momentum[_,D]]];
bad[reason_] := Throw[Failure["BoundedBasis",<|"Reason"->reason|>],"FGBasis"];
put[out_,key_,val_,cap_] := Module[{v=Lookup[out,key,0]+val},
  If[!FreeQ[v,FeynCalc`Pair[__]],bad["non-scalar coefficient"]];
  If[Length[out]+Boole[!KeyExistsQ[out,key]]>cap,bad["basis term cap exceeded"]];
  Append[out,key->v]];
merge[acc_,rhs_,cap_] := Module[{out=acc},
  KeyValueMap[Function[{key,value},out=put[out,key,value,cap]],rhs];out];
(* Keys hold a canonically sorted list of commutative kinematic atoms. *)
mul[a_,b_,cap_] := Module[{out=<||>, key, aa,bb},
  If[Length[a] Length[b]>8 cap,bad["product operation cap exceeded"]];
  KeyValueMap[Function[{ka,va},KeyValueMap[Function[{kb,vb},
    key=HoldComplete@@Sort[Join[List@@ka,List@@kb]];
    out=put[out,key,va vb,cap]],b]],a];out];
walk[e_,cap_] := Which[
  typedQ[e],<|HoldComplete[e]->1|>,
  MatchQ[e,FeynCalc`Pair[__]],bad["unrecognised FeynCalc pair or dimension"],
  !kinQ[e],<|HoldComplete[]->e|>,
  Head[e]===Plus,Fold[merge[#1,walk[#2,cap],cap]&,<||>,List@@e],
  Head[e]===Times,Fold[mul[#1,walk[#2,cap],cap]&,<|HoldComplete[]->1|>,List@@e],
  Head[e]===Power && IntegerQ[e[[2]]] && 0<=e[[2]]<=4,
    Nest[mul[#,walk[e[[1]],cap],cap]&,<|HoldComplete[]->1|>,e[[2]]],
  True,bad["unsupported head around kinematic atom"]];
ExtractVertex[e_,cap_Integer:4000]/; 0<cap<=20000 := Catch[Module[{m,keys,cs},
  m=walk[e,cap];keys=Keys[m];cs=Values[m];
  If[!And@@(FreeQ[#,FeynCalc`Pair[__]]&/@cs),bad["kinematics in coefficients"]];
  <|"Monomials"->(Times@@List@@#&/@keys),"Coefficients"->cs,
    "TermCount"->Length[m],"Reconstruction"->Total[MapThread[Times,{(Times@@List@@#&/@keys),cs}]]|>],"FGBasis"];
FAAtom[FeynCalc`Pair[FeynCalc`LorentzIndex[a_,D],FeynCalc`LorentzIndex[b_,D]]] := Global`FAMetricTensor[a,b];
FAAtom[FeynCalc`Pair[FeynCalc`Momentum[p_,D],FeynCalc`LorentzIndex[a_,D]]] := Global`FAFourVector[p,a];
FAAtom[FeynCalc`Pair[FeynCalc`LorentzIndex[a_,D],FeynCalc`Momentum[p_,D]]] := Global`FAFourVector[p,a];
FAAtom[FeynCalc`Pair[FeynCalc`Momentum[p_,D],FeynCalc`Momentum[q_,D]]] := Global`FAScalarProduct[p,q];
End[];
