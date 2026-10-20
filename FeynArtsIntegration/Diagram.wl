(* Explicit tree-only graph construction and patched FCFAConvert adapter. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
ClearAll[faDiagramFailure, faValidModel, faBuildTree, faResidualFA,
  faTopologyKey, faRequestedTopologies, faInsertedTopologies, faMissingInsertions];
faDiagramFailure[tag_, text_, data_: <||>] := Failure[tag,
  Join[<|"MessageTemplate" -> text, "Stage" -> "FeynArtsTree"|>, data]];
faValidModel[model_] := Module[{directory, name, manifestPath, manifest, files, hashes, orders, families},
  If[! AssociationQ[model] ||
      ! AllTrue[{"Directory", "ModelName", "Manifest"}, KeyExistsQ[model, #] &],
    Return[faDiagramFailure["InvalidModel", "Supply a model returned by FeynGravBuildModel."]]];
  directory = model["Directory"]; name = model["ModelName"];
  If[! StringQ[directory] || ! StringStartsQ[directory, "/"] ||
      ! StringQ[name] || ! StringMatchQ[name, RegularExpression["[A-Za-z][A-Za-z0-9]{0,63}"]] ||
      ! DirectoryQ[directory],
    Return[faDiagramFailure["InvalidModel", "The expected finite model directory is unavailable."]]];
  manifestPath = FileNameJoin[{directory, "manifest.json"}];
  files = {FileNameJoin[{directory, name <> ".gen"}], FileNameJoin[{directory, name <> ".mod"}]};
  If[model["Manifest"] =!= manifestPath || ! And @@ (FileExistsQ /@ Prepend[files, manifestPath]),
    Return[faDiagramFailure["InvalidModel", "The model manifest or one model file is missing."]]];
  manifest = Quiet[Check[Import[manifestPath, "RawJSON"], $Failed]];
  (* The fixed profile is accepted both at quartic order and with the quintic coupling present. *)
  orders = If[AssociationQ[manifest], Lookup[manifest, "GravitonOrders", None], None];
  families = Which[orders === {1, 2}, {"TTT", "TTTT", "TUU"},
    orders === {1, 2, 3}, {"TTT", "TTTT", "TTTTT", "TUU"}, True, None];
  If[! AssociationQ[manifest] || Lookup[manifest, "Schema", 0] =!= 1 ||
      Lookup[manifest, "Profile", None] =!= "GR" ||
      Lookup[manifest, "FixedGauge", None] =!= 2 ||
      Lookup[manifest, "ModelName", None] =!= name ||
      families === None ||
      Lookup[manifest, "InteractionFamilies", None] =!= families ||
      Lookup[manifest, "FieldClasses", None] =!= {"T[1]", "U[1]", "-U[1]"},
    Return[faDiagramFailure["InvalidModel", "The model manifest does not match the fixed finite pure-GR profile."]]];
  hashes = Lookup[manifest, "ModelFileSHA256", Missing["Unstamped"]];
  If[! AssociationQ[hashes] ||
      Lookup[hashes, "Generic", None] =!= FileHash[First[files], "SHA256", "HexString"] ||
      Lookup[hashes, "Classes", None] =!= FileHash[Last[files], "SHA256", "HexString"],
    Return[faDiagramFailure["ModelChanged", "Model bytes differ from the immutable generation manifest; rebuild in a new directory."]]];
  If[! MemberQ[$Packages, "FeynArts`"] || DownValues[FeynCalc`FCFAConvert] === {},
    Return[faDiagramFailure["MissingDependency", "Load installed FeynCalc-patched FeynArts/FCFAConvert explicitly in a fresh kernel."]]];
  <|"Directory" -> directory, "ModelName" -> name, "Manifest" -> manifest|>
];
faResidualFA[expr_] := untranslatedQ[expr] ||
  ! FreeQ[expr, _Global`FAFourVector | _Global`FAMetricTensor | _Global`FAScalarProduct];
(* FeynArts drops a topology for which the model supplies no vertex, so an
   insertion list shorter than the topology list is a silent truncation, not a
   short process. Topologies are identified by their propagator skeleton and
   compared one by one: equal counts do not prove that the same topologies
   survived, so a count comparison alone is not a completeness criterion. *)
faTopologyKey[topology_] := Sort[Cases[List @@ topology,
  FeynArts`Propagator[type_][from_, to_, ___] :> {type, from, to}, Infinity]];
faRequestedTopologies[top_] := Cases[top, HoldPattern[FeynArts`Topology[_][___]], {1}];
faInsertedTopologies[ins_] := Cases[ins, HoldPattern[Rule[topology_, _]] :> topology, {1}];
faMissingInsertions[top_, ins_] := With[{inserted = faTopologyKey /@ faInsertedTopologies[ins]},
  Select[faRequestedTopologies[top], ! MemberQ[inserted, faTopologyKey[#]] &]];
faBuildTree[model_, spec_, inMom_List, outMom_List] := Module[
  {check, validated, ni, no, n, name, directory, top, ins, fa, fc, raw,
    amount, ids, rows, denominators, missingTopologies, requestedTopologies},
  (* Validate BEFORE model loading or diagram enumeration. *)
  check = FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[spec];
  If[FailureQ[check], Return[check]];
  If[spec["LoopOrder"] =!= 0,
    Return[faDiagramFailure["MissingLoopRouting", "Use the explicit fifth loop-momentum list for a one-loop 1PI request."]]];
  ni = Length[spec["Incoming"]]; no = Length[spec["Outgoing"]]; n = ni + no;
  If[Length[inMom] =!= ni || Length[outMom] =!= no ||
      ! AllTrue[Join[inMom, outMom], MatchQ[#, _Symbol] &] ||
      ! DuplicateFreeQ[Join[inMom, outMom]],
    Return[faDiagramFailure["InvalidMomenta", "Supply a distinct symbolic momentum per incoming and outgoing leg in insertion order."]]];
  validated = faValidModel[model]; If[FailureQ[validated], Return[validated]];
  name = validated["ModelName"]; directory = validated["Directory"];
  raw = CheckAbort[Block[{FeynArts`$ModelPath = Prepend[FeynArts`$ModelPath, directory]},
    If[TimeConstrained[FeynArts`InitializeModel[name, FeynArts`GenericModel -> name,
          FeynArts`Reinitialize -> False], 600, $TimedOut] =!= True,
      Return[faDiagramFailure["ModelLoadFailed", "The saved model did not initialise within the bounded model-load time."]]];
    top = TimeConstrained[FeynArts`CreateTopologies[0, ni -> no,
      FeynArts`Adjacencies -> {3, 4}], 30, $TimedOut];
    If[top === $TimedOut, Return[faDiagramFailure["TopologyTimeout", "Tree topology generation timed out."]]];
    ins = TimeConstrained[FeynArts`InsertFields[top,
      spec["Incoming"] -> spec["Outgoing"], FeynArts`Model -> name,
      FeynArts`GenericModel -> name, FeynArts`InsertionLevel -> {FeynArts`Classes}], 30, $TimedOut];
    If[ins === $TimedOut, Return[faDiagramFailure["InsertionTimeout", "Tree field insertion timed out."]]];
    (* Stop before amplitude construction when field insertion dropped a topology. *)
    requestedTopologies = faRequestedTopologies[top];
    missingTopologies = faMissingInsertions[top, ins];
    If[missingTopologies =!= {},
      Return[faDiagramFailure["IncompleteInsertion",
        "The model supplied no insertion for one or more requested topologies, so the amplitude would be silently incomplete.",
        <|"RequestedTopologies" -> Length[requestedTopologies],
          "InsertedTopologies" -> Length[faInsertedTopologies[ins]],
          "MissingPositions" -> Flatten[Position[requestedTopologies, #] & /@ missingTopologies]|>]]];
    fa = TimeConstrained[FeynArts`CreateFeynAmp[ins,
      FeynArts`PreFactor -> 1, FeynArts`Truncated -> True], 60, $TimedOut];
    If[fa === $TimedOut, Return[faDiagramFailure["AmplitudeTimeout", "Tree amplitude construction timed out."]]];
    {top, ins, fa}
  ], $Aborted];
  If[FailureQ[raw], Return[raw]];
  If[raw === $Aborted, Return[faDiagramFailure["ModelAbort", "FeynArts aborted tree construction."]]];
  {top, ins, fa} = raw;
  amount = If[n === 3, 1, 4];
  ids = Cases[fa, FeynArts`FAFeynAmp[id_, __] :> id, Infinity];
  If[Length[ids] =!= amount,
    Return[faDiagramFailure["IncompleteDiagramSet", "The requested pure-GR tree did not have exactly the expected contact/exchange diagrams.",
      <|"Expected" -> amount, "Actual" -> Length[ids]|>]]];
  fc = TimeConstrained[FeynCalc`FCFAConvert[fa,
    FeynCalc`IncomingMomenta -> inMom, FeynCalc`OutgoingMomenta -> outMom,
    FeynCalc`Contract -> False, FeynCalc`ChangeDimension -> D,
    System`List -> True, FeynCalc`Prefactor -> 1], 60, $TimedOut];
  If[fc === $TimedOut || ! ListQ[fc] || Length[fc] =!= amount ||
      AnyTrue[fc, faResidualFA],
    Return[faDiagramFailure["ConversionFailed", "The installed patched FCFAConvert did not return a complete open-index FeynCalc diagram list."]]];
  denominators = (Count[#, _FeynCalc`PropagatorDenominator, Infinity] &) /@ fc;
  If[n === 4 && Sort[denominators] =!= {0, 1, 1, 1},
    Return[faDiagramFailure["IncompleteDiagramSet", "The four-point tree must contain a single contact and all three single-exchange channels.",
      <|"DenominatorCounts" -> denominators|>]]];
  If[n === 3 && denominators =!= {0},
    Return[faDiagramFailure["IncompleteDiagramSet", "The three-point tree must contain exactly one contact rule."]]];
  rows = MapIndexed[<|"ID" -> ids[[First[#2]]], "Expression" -> #1,
      "DenominatorCount" -> denominators[[First[#2]]],
      "SymmetryFactorPolicy" -> "Already in FeynArts amplitude; never reapply"|> & , fc];
  <|"Process" -> spec, "Model" -> model,
    "Diagrams" -> rows, "Insertions" -> ins, "RawAmplitude" -> fa,
    "DiagramIntegrity" -> (Hash[{#["ID"], #["Expression"], #["DenominatorCount"]}, "SHA256", "HexString"] & /@ rows),
    "Status" -> "Constructed", "ValidationStatus" -> "Complete finite-source tree inventory; consult model fingerprint and external verification evidence",
    "LoopMeasureAdded" -> False, "ExternalWavefunctionsAdded" -> False|>
];
faBuildTree[___] := faDiagramFailure["InvalidArguments", "Use a verified model, allowed process, and separate incoming/outgoing momentum lists."];
FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model_, spec_, inMom_List, outMom_List] :=
  faBuildTree[model, spec, inMom, outMom];
FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[model_, spec_, ___] :=
  With[{check = FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[spec]},
    If[FailureQ[check], check, faDiagramFailure["InvalidMomenta", "Supply distinct symbolic incoming and outgoing momentum lists."]]];
End[];
