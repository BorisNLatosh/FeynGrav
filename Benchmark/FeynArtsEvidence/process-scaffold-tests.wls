(* Exact, bounded, stub-only regression: wolframscript -file process-scaffold-tests.wls *)
Module[{path = FileNameJoin[{DirectoryName[$InputFileName], "process-scaffold.wl"}],
  before, after, tree3, tree4, twoPoint, calls = {}, mark = 0, mock, input, output,
  failures = 0, assert},
  assert[label_, condition_] := If[TrueQ[condition], Print["PASS ", label],
    Print["FAIL ", label]; failures++];
  before = MemberQ[$Packages, "FeynArts`"];
  Get[path];
  after = MemberQ[$Packages, "FeynArts`"];
  assert["load does not load FeynArts", before === after];
  tree3 = <|"Model" -> "PureGR", "Incoming" -> {FeynArts`T[1]},
    "Outgoing" -> {FeynArts`T[1], FeynArts`T[1]}, "LoopOrder" -> 0|>;
  tree4 = Join[tree3, <|"Outgoing" -> {FeynArts`T[1], FeynArts`T[1], FeynArts`T[1]}|>];
  twoPoint = Join[tree3, <|"Outgoing" -> {FeynArts`T[1]}, "LoopOrder" -> 1,
    "OneParticleIrreducible" -> True|>];
  assert["three- and four-leg GR trees allowed",
    And @@ (AssociationQ[FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[#]] & /@ {tree3, tree4})];
  assert["one-loop 1PI two-point closed by ghost gate",
    MatchQ[FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[twoPoint], Failure["GhostCompletenessGate", _]]];
  assert["all other processes rejected", And @@ (FailureQ[FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[#]] & /@
    {Join[tree3, <|"Model" -> "QuadraticGravity"|>], Join[tree3, <|"Outgoing" -> {FeynArts`V[1], FeynArts`T[1]}|>],
     Join[tree4, <|"LoopOrder" -> 1|>], Join[tree4, <|"Outgoing" -> Table[FeynArts`T[1], 4]|>]})];
  assert["unsupported process rejected before action initialization",
    FailureQ[FeynGrav`FeynArtsIntegration`FeynGravWithAllowedProcess[twoPoint, mark++]] && mark === 0];
  assert["valid process releases action once",
    FeynGrav`FeynArtsIntegration`FeynGravWithAllowedProcess[tree3, mark++] === 0 && mark === 1];
  mock[expr_, opts___Rule] := (AppendTo[calls, <|"Expression" -> expr, "Options" -> {opts}|>];
    If[expr === b, Failure["StubFailure", <|"MessageTemplate" -> "stub diagnostic", "RetainedFile" -> "stub.log"|>], 2 expr]);
  input = <|"Process" -> tree4, "Diagrams" -> {
    <|"ID" -> "s", "Expression" -> a, "Topology" -> "exchange", "SymmetryFactor" -> 1|>,
    <|"ID" -> "t", "Expression" -> b, "Topology" -> "exchange", "SymmetryFactor" -> 1/2|>}|>;
  output = FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[input,
    FeynGrav`FeynArtsIntegration`CalculationFunction -> mock,
    CalcFormConverter`FORMThreads -> 1];
  assert["stub called once per diagram in order", Lookup[calls, "Expression"] === {a, b}];
  assert["only explicitly whitelisted options forwarded",
    And @@ (#["Options"] === {CalcFormConverter`FORMThreads -> 1} & /@ calls) &&
      FreeQ[calls, FeynCalc`LoopMomenta]];
  assert["failure preserves result, IDs and metadata; total withheld",
    output["Status"] === "Incomplete" && output["FailedDiagramIDs"] === {"t"} &&
    MatchQ[output["Diagrams"][[2, "Result"]], Failure["StubFailure", a_Association /; Lookup[a, "RetainedFile"] === "stub.log"]] &&
    output["Diagrams"][[2, "Metadata", "SymmetryFactor"]] === 1/2 &&
    MatchQ[output["Total"], Missing["Incomplete", _]]];
  calls = {};
  assert["nonempty loop routing rejected for trees",
    FailureQ[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[input,
      FeynGrav`FeynArtsIntegration`CalculationFunction -> mock,
      FeynCalc`LoopMomenta -> {q}]]];
  assert["residual FeynArts heads are per-diagram failures",
    With[{bad = FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[
        Join[input, <|"Diagrams" -> {<|"ID" -> "untranslated", "Expression" -> FeynArts`T[1]|>}|>],
        FeynGrav`FeynArtsIntegration`CalculationFunction -> mock]},
      bad["Status"] === "Incomplete" && bad["FailedDiagramIDs"] === {"untranslated"} &&
      MatchQ[bad["Diagrams"][[1, "Result"]], Failure["UntranslatedDiagram", _]]]];
  assert["thrown Failure is retained per diagram",
    With[{thrown = FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[
        Join[input, <|"Diagrams" -> {First[input["Diagrams"]]}|>],
        FeynGrav`FeynArtsIntegration`CalculationFunction ->
          Function[{expr}, Throw[Failure["StubThrown", <|"MessageTemplate" -> "thrown diagnostic"|>]]]]},
      thrown["Status"] === "Incomplete" &&
      MatchQ[thrown["Diagrams"][[1, "Result"]], Failure["StubThrown", _]]]];
  calls = {};
  assert["foreign and duplicate options fail before calculation",
    FailureQ[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[input,
      FeynGrav`FeynArtsIntegration`CalculationFunction -> mock, FeynArts`PreFactor -> 1]] &&
    FailureQ[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[input,
      FeynGrav`FeynArtsIntegration`CalculationFunction -> mock,
      CalcFormConverter`FORMThreads -> 1, CalcFormConverter`FORMThreads -> 2]] && calls === {}];
  assert["blocked process never calls calculator",
    FailureQ[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[Join[input, <|"Process" -> twoPoint|>],
      FeynGrav`FeynArtsIntegration`CalculationFunction -> mock]] && calls === {}];
  assert["missing converter returns explicit failure without loading it",
    MatchQ[FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[input],
      Failure["MissingDependency", _]] && calls === {}];
  calls = {};
  output = FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[
    Join[input, <|"Diagrams" -> {First[input["Diagrams"]]}|>],
    FeynGrav`FeynArtsIntegration`CalculationFunction -> mock];
  assert["successful stub result sums with no implicit settings",
    output["Status"] === "Success" && output["Total"] === 2 a && calls[[1, "Options"]] === {}];
  Print["RESULT ", If[failures === 0, "PASS", "FAIL"], " (", failures, " failed)"];
  Exit[If[failures === 0, 0, 1]]
]
