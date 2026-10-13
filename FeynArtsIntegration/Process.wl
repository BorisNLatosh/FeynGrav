(* ::Package:: *)
(* Optional allowlist and sequential converter adapter; loading definitions starts no model or process. *)
BeginPackage["FeynGrav`FeynArtsIntegration`"];

FeynGravValidateProcess::usage = "FeynGravValidateProcess[spec] checks the fixed pure-GR three-/four-graviton tree or one-loop 1PI two-point process allowlist without initializing FeynArts. spec has Model, Incoming, Outgoing, LoopOrder and optional OneParticleIrreducible keys. Actual loop construction also checks the validated finite model fingerprint.";
FeynGravWithAllowedProcess::usage = "FeynGravWithAllowedProcess[spec, action] holds action until spec passes the process allowlist. An action that initializes FeynArts must be supplied unevaluated as the second argument.";
FeynGravCalculateAmplitude::usage = "FeynGravCalculateAmplitude[constructedAmplitude, opts] applies the existing CalcFormCalculate sequentially to each verified FeynGravCreateAmplitude tree diagram. Real algebra requires intact model/diagram provenance; failure or abort preserves diagram IDs, metadata and missing Total. The loop integrand is construction-only; loop FORM algebra/reduction is not offered. CalculationFunction is a test seam only. No calculation occurs on package load.";
CalculationFunction::usage = "CalculationFunction is a test seam for FeynGravCalculateAmplitude; Automatic invokes CalcFormConverter`CalcFormCalculate. Do not use a stub as physical evidence.";

Begin["`Private`"];

failure[tag_, message_, details_: <||>] := Failure[tag, Join[<|"MessageTemplate" -> message, "Stage" -> "FeynArtsIntegration"|>, details]];

(* No fallback to a different tensor species, ghost process, or model. In particular,
   a loop request stays closed even if its caller claims a ghost audit is complete. *)
FeynGravValidateProcess[spec_] := Module[{keys, incoming, outgoing, loop, onePI},
  If[! AssociationQ[spec], Return[failure["InvalidProcess", "Process must be an Association."]]];
  keys = Keys[spec];
  If[! SubsetQ[{"Model", "Incoming", "Outgoing", "LoopOrder", "OneParticleIrreducible"}, keys] ||
     ! SubsetQ[keys, {"Model", "Incoming", "Outgoing", "LoopOrder"}],
    Return[failure["InvalidProcess", "Process keys must be Model, Incoming, Outgoing, LoopOrder and optionally OneParticleIrreducible.", <|"Keys" -> keys|>]]];
  If[spec["Model"] =!= "PureGR", Return[failure["UnsupportedModel", "Only the fixed PureGR profile is allowed."]]];
  incoming = spec["Incoming"]; outgoing = spec["Outgoing"];
  loop = spec["LoopOrder"]; onePI = Lookup[spec, "OneParticleIrreducible", False];
  If[! ListQ[incoming] || ! ListQ[outgoing] ||
     ! AllTrue[Join[incoming, outgoing], # === FeynArts`T[1] &] ||
     ! MemberQ[{True, False}, onePI],
    Return[failure["InvalidProcess", "Incoming and Outgoing must be lists of FeynArts`T[1] with a Boolean OneParticleIrreducible flag."]]];
  If[loop === 1 && Length[incoming] === 1 && Length[outgoing] === 1 && onePI === True,
    Return[<|"Status" -> "Allowed", "Process" -> spec, "Stage" -> "ProcessValidation",
      "Interpretation" -> "Unintegrated amputated 1PI two-point expression only"|>]];
  If[loop =!= 0 || onePI =!= False || Length[incoming] < 1 || Length[outgoing] < 1 ||
     ! MemberQ[{3, 4}, Length[incoming] + Length[outgoing]],
    Return[failure["UnsupportedProcess", "Only three-/four-graviton trees or the one-loop 1PI graviton two-point with one incoming and one outgoing leg are supported.", <|"LoopOrder" -> loop, "ExternalLegCount" -> Length[incoming] + Length[outgoing]|>]]];
  <|"Status" -> "Allowed", "Process" -> spec, "Stage" -> "ProcessValidation", "Interpretation" -> "Amputated integrand algebra only"|>
];

SetAttributes[FeynGravWithAllowedProcess, HoldRest];
FeynGravWithAllowedProcess[spec_, action_] := Module[{check = FeynGravValidateProcess[spec]},
  If[FailureQ[check], check, ReleaseHold[HoldComplete[action]]]
];

(* A translated integrand must not retain FeynArts heads or unconverted external
   spin-2 wavefunction markers. This is a structural guard, not a translator. *)
untranslatedQ[expression_] := Cases[HoldComplete[expression],
    h_Symbol[___] /; Context[Unevaluated[h]] === "FeynArts`", Infinity] =!= {} ||
  ! FreeQ[HoldComplete[expression], _Global`FAPolarizationTensor];

$converterOptions = {
  CalcFormConverter`ColourAlgebra, CalcFormConverter`DiracAlgebra, FeynCalc`Dimension,
  FeynCalc`LoopMomenta, CalcFormConverter`FORMExecutable, System`TimeConstraint,
  CalcFormConverter`WorkingDirectory, CalcFormConverter`KeepFiles,
  CalcFormConverter`ShowTiming, CalcFormConverter`ShowProgress, CalcFormConverter`FORMThreads
};

FeynGravCalculateAmplitude[input_, opts___Rule] := Module[
  {check, diagrams, rules = {opts}, names, converterRules, calculator, rows, failed},
  If[! AssociationQ[input] || ! SubsetQ[Keys[input], {"Diagrams", "Process"}] ||
     ! KeyExistsQ[input, "Process"],
    Return[failure["InvalidAmplitude", "Supply an amplitude Association with Process and Diagrams, preferably from FeynGravCreateAmplitude."]]];
  check = FeynGravValidateProcess[input["Process"]];
  If[FailureQ[check], Return[check]];
  If[input["Process"]["LoopOrder"] === 1,
    Return[failure["LoopAlgebraUnavailable", "The complete one-loop 1PI integrand is returned by FeynGravCreateAmplitude; FORM loop algebra and integration are not validated on this branch. No diagram was calculated."]]];
  names = First /@ rules;
  If[! AllTrue[names, MemberQ[Append[$converterOptions, CalculationFunction], #] &] ||
     ! DuplicateFreeQ[names],
    Return[failure["InvalidOption", "Only distinct, fully qualified converter options and CalculationFunction are allowed.", <|"OptionNames" -> names|>]]];
  If[AnyTrue[rules, First[#] === FeynCalc`LoopMomenta && Last[#] =!= {} &],
    Return[failure["UnsupportedOption", "Nonempty LoopMomenta declarations are unavailable for tree-only integrand algebra."]]];
  diagrams = input["Diagrams"];
  If[! ListQ[diagrams] || diagrams === {} ||
     ! AllTrue[diagrams, AssociationQ[#] && KeyExistsQ[#, "Expression"] && KeyExistsQ[#, "ID"] &] ||
     ! DuplicateFreeQ[Lookup[diagrams, "ID"]],
    Return[failure["InvalidDiagrams", "Supply a nonempty list of diagram Associations with unique ID and Expression keys."]]];
  calculator = Lookup[Association[rules], CalculationFunction, Automatic];
  If[calculator === Automatic, calculator = CalcFormConverter`CalcFormCalculate];
  If[calculator === CalcFormConverter`CalcFormCalculate &&
     DownValues[CalcFormConverter`CalcFormCalculate] === {},
    Return[failure["MissingDependency", "CalcFormCalculate is not loaded; explicitly load the existing converter before requesting a calculation."]]];
  If[! (Head[calculator] === Symbol || Head[calculator] === Function),
    Return[failure["InvalidOption", "CalculationFunction must be Automatic, a symbol or a pure function."]]];
  converterRules = Select[rules, First[#] =!= CalculationFunction &];
  If[calculator === CalcFormConverter`CalcFormCalculate &&
     ! SubsetQ[First /@ Options[CalcFormConverter`CalcFormCalculate], First /@ converterRules],
    Return[failure["UnsupportedConverterOption", "An explicitly requested converter option is absent from the loaded CalcFormCalculate options."]]];
  If[calculator === CalcFormConverter`CalcFormCalculate,
    If[Lookup[input, "Status", None] =!= "Constructed" ||
        ! AllTrue[{"Model", "Insertions", "RawAmplitude", "DiagramIntegrity"}, KeyExistsQ[input, #] &] ||
        DownValues[faValidModel] === {},
      Return[failure["UnverifiedAmplitude", "Real FORM algebra requires a complete FeynGravCreateAmplitude result and the loaded diagram adapter."]]];
    check = faValidModel[input["Model"]]; If[FailureQ[check], Return[check]];
    If[Sort[Lookup[diagrams, "DenominatorCount", Missing["Unverified"]]] =!=
        If[Length[input["Process"]["Incoming"]] + Length[input["Process"]["Outgoing"]] === 3,
          {0}, {0, 1, 1, 1}],
      Return[failure["IncompleteDiagramSet", "The source-rule tree must contain the complete contact/exchange diagram set."]]];
    If[Lookup[input, "DiagramIntegrity"] =!=
        (Hash[{#["ID"], #["Expression"], #["DenominatorCount"]}, "SHA256", "HexString"] & /@ diagrams),
      Return[failure["UnverifiedAmplitude", "A diagram expression, ID or denominator count changed after complete tree construction."]]]
  ];
  rows = MapIndexed[Function[{diagram, position}, Module[{result, identifier = diagram["ID"]},
      result = Which[
        FailureQ[diagram["Expression"]], diagram["Expression"],
        untranslatedQ[diagram["Expression"]],
          failure["UntranslatedDiagram", "Residual FeynArts heads or unsupported external tensor markers remain in the diagram expression.", <|"DiagramID" -> identifier|>],
        True, CheckAbort[
          Catch[Apply[calculator, Prepend[converterRules, diagram["Expression"]]]],
             failure["CalculationAborted", "The per-diagram calculation was aborted.", <|"DiagramID" -> identifier|>]]
      ];
      <|"ID" -> identifier, "Index" -> First[position], "Metadata" -> KeyDrop[diagram, "Expression"],
        "Input" -> diagram["Expression"], "Status" -> If[FailureQ[result], "Failure", "Success"],
        "Result" -> result|>
    ]], diagrams];
  failed = Cases[rows, row_ /; row["Status"] === "Failure" :> row["ID"]];
  <|"Process" -> input["Process"], "Stage" -> "IntegrandAlgebra", "Diagrams" -> rows,
    "Status" -> If[failed === {}, "Success", "Incomplete"], "FailedDiagramIDs" -> failed,
    "Total" -> If[failed === {}, Total[Lookup[rows, "Result"]], Missing["Incomplete", failed]],
    "ConverterOptionsForwarded" -> converterRules|>
];
FeynGravCalculateAmplitude[___] := failure["InvalidArguments", "Use FeynGravCalculateAmplitude[association, allowed rules]."];

End[];
EndPackage[];
