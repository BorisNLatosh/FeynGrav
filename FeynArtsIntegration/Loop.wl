(* Explicit, finite one-loop 1PI pure-GR two-point INTEGRAND. No FORM or integral evaluation. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
ClearAll[faLoopFailure, faLoopNamespaced, faBuildLoop];
faLoopFailure[tag_, text_, data_: <||>] := Failure[tag,
  Join[<|"MessageTemplate" -> text, "Stage" -> "FeynArtsLoopIntegrand"|>, data]];

(* The exact model bytes and source-rule fingerprint have independent tadpole,
   graviton-bubble and oriented ghost-bubble source-product evidence. Any new
   snapshot must pass those gates anew; changing a file never inherits approval. *)
$faLoopSnapshotSHA256 = "5a149c98161e2b2e5e3391f38d7f62dd1ab456a8bb3cdf54d3a1c0999ac50fa2";
$faLoopGenericSHA256 = "9fff5547d9bb9cb9c548f5861699844006fe8dc7cdcc56620004c6deb48f67d9";
$faLoopClassesSHA256 = "536144daf0106bf49c9d7c56e6858a447ba68c8287edb704272836606e5828a0";

(* Make each summand's contracted Lor5+ labels private before forming a total.
   The four external Lor1...Lor4 labels stay shared. This is only an alpha
   renaming of typed Lorentz indices; it cannot affect momentum or tensors. *)
faLoopNamespaced[expr_, ordinal_Integer] := Module[{indices, internal, rules},
  indices = DeleteDuplicates[Cases[expr,
    FeynCalc`LorentzIndex[index_Symbol, D] :> index, Infinity]];
  internal = Select[indices,
    StringMatchQ[SymbolName[#], RegularExpression["Lor([5-9]|[1-9][0-9]+)"]] &];
  rules = Table[FeynCalc`LorentzIndex[internal[[j]], D] ->
      FeynCalc`LorentzIndex[Unique["fgfaLoop" <> ToString[ordinal] <> "$"], D],
    {j, Length[internal]}];
  expr /. rules
];

faBuildLoop[model_, spec_, inMom_List, outMom_List, loopMom_List] := Module[
  {check, verified, manifest, name, directory, top, ins, fa, raw, ids, fc,
    counts, routes, rows, separatelyNamed, loop},
  (* Process, exact external/loop routing, model bytes and verified source
     fingerprint are all checked BEFORE FeynArts topology construction. *)
  check = FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[spec];
  If[FailureQ[check], Return[check]];
  If[spec["LoopOrder"] =!= 1 ||
      Length[inMom] =!= 1 || Length[outMom] =!= 1 || Length[loopMom] =!= 1 ||
      ! MatchQ[inMom[[1]], _Symbol] || outMom[[1]] =!= inMom[[1]] ||
      ! MatchQ[loopMom[[1]], _Symbol] || loopMom[[1]] === inMom[[1]],
    Return[faLoopFailure["InvalidLoopRouting",
      "Supply one symbolic momentum p for both external legs and one distinct symbolic loop momentum ell in a fifth list."]]];
  verified = faValidModel[model]; If[FailureQ[verified], Return[verified]];
  manifest = verified["Manifest"];
  If[Lookup[manifest, "SnapshotSHA256", None] =!= $faLoopSnapshotSHA256 ||
      Lookup[manifest, "FeynArtsVersion", None] =!= "3.12" ||
      Lookup[manifest["ModelFileSHA256"], "Generic", None] =!= $faLoopGenericSHA256 ||
      Lookup[manifest["ModelFileSHA256"], "Classes", None] =!= $faLoopClassesSHA256,
    Return[faLoopFailure["GhostCompletenessGate",
      "The saved model fingerprint lacks independent complete ghost/tadpole/bubble source-product validation. Revalidate a changed snapshot; no partial loop is built."]]];
  name = verified["ModelName"]; directory = verified["Directory"]; loop = First[loopMom];
  raw = CheckAbort[Block[{FeynArts`$ModelPath = Prepend[FeynArts`$ModelPath, directory]},
    If[TimeConstrained[FeynArts`InitializeModel[name, FeynArts`GenericModel -> name,
          FeynArts`Reinitialize -> False], 600, $TimedOut] =!= True,
      Return[faLoopFailure["ModelLoadFailed", "The verified model did not initialise within the bounded time."]]];
    top = TimeConstrained[FeynArts`CreateTopologies[1, 1 -> 1,
      FeynArts`Adjacencies -> {3, 4}, FeynArts`CTOrder -> 0,
      FeynArts`ExcludeTopologies -> {FeynArts`WFCorrections, FeynArts`Reducible}], 25, $TimedOut];
    If[top === $TimedOut || Length[List @@ top] =!= 2,
      Return[faLoopFailure["IncompleteTopologySet", "Expected precisely the tadpole and bubble 1PI topologies; tadpoles are never excluded."]]];
    ins = TimeConstrained[FeynArts`InsertFields[top, spec["Incoming"] -> spec["Outgoing"],
      FeynArts`Model -> name, FeynArts`GenericModel -> name,
      FeynArts`InsertionLevel -> {FeynArts`Classes}], 30, $TimedOut];
    If[ins === $TimedOut || FreeQ[ins, -FeynArts`U[1]],
      Return[faLoopFailure["IncompleteInsertionSet", "The oriented ghost bubble did not appear alongside the graviton graphs."]]];
    fa = TimeConstrained[FeynArts`CreateFeynAmp[ins,
      FeynArts`PreFactor -> 1, FeynArts`Truncated -> True], 75, $TimedOut];
    If[fa === $TimedOut, Return[faLoopFailure["AmplitudeTimeout", "The complete three-graph 1PI construction timed out."]]];
    {top, ins, fa}
  ], $Aborted];
  If[FailureQ[raw], Return[raw]];
  If[raw === $Aborted, Return[faLoopFailure["ModelAbort", "FeynArts aborted 1PI construction."]]];
  {top, ins, fa} = raw;
  ids = Cases[fa, FeynArts`FAFeynAmp[id_, __] :> id, Infinity];
  If[Length[ids] =!= 3 || ! DuplicateFreeQ[ids],
    Return[faLoopFailure["IncompleteDiagramSet", "Require exactly three distinct 1PI tadpole, graviton-bubble and ghost-bubble graphs.",
      <|"Actual" -> Length[ids]|>]]];
  fc = CheckAbort[TimeConstrained[FeynCalc`FCFAConvert[fa,
    FeynCalc`IncomingMomenta -> inMom, FeynCalc`OutgoingMomenta -> outMom,
    FeynCalc`LoopMomenta -> loopMom, FeynCalc`Contract -> False,
    FeynCalc`ChangeDimension -> D, System`List -> True,
    FeynCalc`Prefactor -> 1], 90, $TimedOut], $Aborted];
  If[fc === $TimedOut || fc === $Aborted || ! ListQ[fc] || Length[fc] =!= 3 ||
      AnyTrue[fc, faResidualFA],
    Return[faLoopFailure["ConversionFailed", "The patched converter did not return all three open-index loop integrands."]]];
  counts = (Count[#, _FeynCalc`PropagatorDenominator, Infinity] &) /@ fc;
  routes = Cases[#, FeynCalc`PropagatorDenominator[FeynCalc`Momentum[m_, ___], 0] :> m, Infinity] & /@ fc;
  If[counts =!= {1, 2, 2} ||
      routes =!= {{loop}, {loop, loop - inMom[[1]]}, {loop, loop - inMom[[1]]}},
    Return[faLoopFailure["IncompleteDiagramSet",
      "1PI routes must retain the graviton tadpole and both distinct two-propagator bubbles without scaleless filtering.",
      <|"DenominatorCounts" -> counts, "Routes" -> routes|>]]];
  separatelyNamed = MapIndexed[faLoopNamespaced[#1, First[#2]] &, fc];
  rows = MapIndexed[<|"ID" -> ids[[First[#2]]], "Expression" -> #1,
       "DenominatorCount" -> counts[[First[#2]]], "LoopRouting" -> routes[[First[#2]]],
       "TopologyFamily" -> {"GravitonTadpole", "GravitonBubble", "GhostBubble"}[[First[#2]]],
       "ExpectedGraphSymmetryFactor" -> If[First[#2] === 3, 1, 1/2],
       "ClosedGhostLoopMinusIncluded" -> (First[#2] === 3),
       "SymmetryFactorPolicy" -> "Already in FeynArts amplitude; never reapply"|> &,
    fc];
  <|"Process" -> spec, "Model" -> model, "ModelFingerprint" -> manifest["SnapshotSHA256"],
    "Diagrams" -> rows, "Insertions" -> ins, "RawAmplitude" -> fa,
    "DiagramIntegrity" -> (Hash[{#["ID"], #["Expression"], #["DenominatorCount"]}, "SHA256", "HexString"] & /@ rows),
    "Status" -> "Constructed", "Stage" -> "UnintegratedOneLoop1PI",
    "TotalIntegrand" -> Total[separatelyNamed],
    "TotalDummyIndexPolicy" -> "Private alpha-renamed Lorentz contractions per summand; Lor1...Lor4 external labels shared",
    "LoopMomenta" -> loopMom, "ConservationRelation" -> (First[inMom] - First[outMom] == 0),
    "LoopMeasureAdded" -> False, "ExternalWavefunctionsAdded" -> False,
    "CountertermsIncluded" -> False, "ScalelessTermsRemoved" -> False,
    "ValidationStatus" -> "Exact source factors checked for this pinned snapshot: quartic tadpole, graviton bubble and oriented ghost bubble; no integral evaluation"|>
];
faBuildLoop[___] := faLoopFailure["InvalidArguments", "Use one verified model, one-loop 1PI T[1]->T[1], {p}, {p}, and {ell}."];

FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model_, spec_, inMom_List, outMom_List, loopMom_List] :=
  faBuildLoop[model, spec, inMom, outMom, loopMom];
End[];
