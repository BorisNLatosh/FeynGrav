(* ::Package:: *)
(* Experimental, standalone process/calculate scaffold. No model builder or FeynArts loading. *)
BeginPackage["FeynGrav`FeynArtsIntegration`"];

FeynGravValidateProcess::usage = "FeynGravValidateProcess[spec] checks the exact pure-GR tree process allowlist without initializing FeynArts. spec has Model, Incoming, Outgoing, LoopOrder and optional OneParticleIrreducible keys.";
FeynGravWithAllowedProcess::usage = "FeynGravWithAllowedProcess[spec, action] holds action until spec passes the process allowlist. An action that initializes FeynArts must be supplied unevaluated as the second argument.";
FeynGravCalculateAmplitude::usage = "FeynGravCalculateAmplitude[<|\"Process\"->spec,\"Diagrams\"->{<|\"ID\"->id,\"Expression\"->expr,...|>,...}|>, opts] applies CalcFormCalculate sequentially; results and diagram metadata are retained even if a calculation fails. No calculation is done on package load.";
CalculationFunction::usage = "CalculationFunction is a test seam for FeynGravCalculateAmplitude; Automatic invokes CalcFormConverter`CalcFormCalculate. Do not use a stub as physical evidence.";

Begin["`Private`"];

failure[tag_, message_, details_: <||>] := Failure[tag, Join[<|"MessageTemplate" -> message, "Stage" -> "ProcessScaffold"|>, details]];

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
  If[loop === 1 && Length[incoming] + Length[outgoing] === 2 && onePI === True,
    Return[failure["GhostCompletenessGate", "The one-loop 1PI graviton two-point function remains unavailable until the ghost-action completeness gate passes.", <|"LoopOrder" -> loop|>]]];
  If[loop =!= 0 || onePI =!= False || Length[incoming] < 1 || Length[outgoing] < 1 ||
     ! MemberQ[{3, 4}, Length[incoming] + Length[outgoing]],
    Return[failure["UnsupportedProcess", "Only three- and four-graviton pure-GR trees with incoming and outgoing legs are supported.", <|"LoopOrder" -> loop, "ExternalLegCount" -> Length[incoming] + Length[outgoing]|>]]];
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
  ! FreeQ[HoldComplete[expression],
    FeynCalc`PolarizationTensor[___] | FeynCalc`PolarizationTensorD[___]];

$converterOptions = {
  CalcFormConverter`ColourAlgebra, CalcFormConverter`DiracAlgebra, FeynCalc`Dimension,
  FeynCalc`LoopMomenta, CalcFormConverter`FORMExecutable, System`TimeConstraint,
  CalcFormConverter`WorkingDirectory, CalcFormConverter`KeepFiles,
  CalcFormConverter`ShowTiming, CalcFormConverter`ShowProgress, CalcFormConverter`FORMThreads
};

FeynGravCalculateAmplitude[input_, opts___Rule] := Module[
  {check, diagrams, rules = {opts}, names, converterRules, calculator, rows, failed},
  If[! AssociationQ[input] || Sort[Keys[input]] =!= {"Diagrams", "Process"} ||
     ! KeyExistsQ[input, "Process"],
    Return[failure["InvalidAmplitude", "Supply exactly Process and Diagrams in an amplitude Association."]]];
  check = FeynGravValidateProcess[input["Process"]];
  If[FailureQ[check], Return[check]];
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
  rows = MapIndexed[Function[{diagram, position}, Module[{result, identifier = diagram["ID"]},
      result = Which[
        FailureQ[diagram["Expression"]], diagram["Expression"],
        untranslatedQ[diagram["Expression"]],
          failure["UntranslatedDiagram", "Residual FeynArts heads or unsupported external tensor markers remain in the diagram expression.", <|"DiagramID" -> identifier|>],
        True, CheckAbort[
          Catch[Apply[calculator, Prepend[converterRules, diagram["Expression"]]]], Abort[]]
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
