(* Finite pure-GR model snapshot builder; called explicitly by the optional companion. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
$faSourceRoot = DirectoryName[DirectoryName[$InputFileName]];
$faQuinticTemplate = FileNameJoin[{DirectoryName[$InputFileName], "GRQuinticTensor.gen.in"}];
ClearAll[faBarePairQ, faGenericQuinticSlotsQ, faVerifiedQuinticQ, faQuinticRecord, faModelFailure, faEncode, faPropagator, faBasisRecord, faModelCore,
  faBuildModel, faDependencyCheck, faRoleFreeQ, faGravitonOrderValues,
  faValidGravitonOrderQ, faOrderSpecification, faImportedOrderFailure];
faModelFailure[tag_, text_, data_: <||>] := Failure[tag,
  Join[<|"MessageTemplate" -> text, "Stage" -> "ModelBuild"|>, data]];
(* The encoder exposes ONLY FeynArts kinematic placeholder symbols unqualified;
   Global patched FA tensor heads, FeynGrav couplings and other contexts stay typed. *)
faEncode[e_] := Block[{$Context = "FeynGrav`FeynArtsIntegration`Private`",
    $ContextPath = {"System`", "FeynArts`"}}, ToString[e, InputForm]];
faRoleFreeQ[] := And @@ (OwnValues[#] === {} & /@ Join[
   Table[Symbol["FeynArts`li" <> ToString[i]], {i, 10}],
   Append[Table[Symbol["FeynArts`mom" <> ToString[i]], {i, 5}], FeynArts`mom]]);
faDependencyCheck[] := Module[{path},
  If[! MemberQ[$Packages, "FeynGrav`"],
    Return[faModelFailure["FeynGravNotLoaded", "Load FeynGrav in a fresh kernel before building the model."]]];
  If[OwnValues[D] =!= {} || OwnValues[FeynGrav`\[Kappa]] =!= {} ||
     FeynGrav`GaugeFixingEpsilon =!= 2 || ! faRoleFreeQ[],
    Return[faModelFailure["ContaminatedRuleSettings",
      "Use a fresh kernel with symbolic D and kappa, unset model placeholders and GaugeFixingEpsilon = 2."]]];
  If[! MemberQ[$Packages, "FeynArts`"],
    Return[faModelFailure["FeynArtsNotLoaded", "Load the installed patched FeynArts in a fresh kernel before explicit model building."]]];
  path = ToString[FeynArts`$FeynArtsDir];
  If[! StringContainsQ[path, "FeynCalc/FeynArts"] ||
     ! NumericQ[FeynArts`$FeynArtsVersionNumber] ||
     DownValues[FeynCalc`FCFAConvert] === {},
    Return[faModelFailure["IncompatibleFeynArts", "Use a fresh kernel with the installed FeynCalc-patched FeynArts and FCFAConvert."]]];
  True
];
faPropagator[e_, mom_Symbol] := Module[{den, out},
  den = FeynCalc`FeynAmpDenominator[FeynCalc`PropagatorDenominator[
    FeynCalc`Momentum[mom, D], 0]];
  If[Count[e, den, Infinity] =!= 1,
    Return[faModelFailure["UnsupportedPropagator", "Expected exactly one massless FeynGrav FAD factor."]]];
  out = faTranslateMonomial[e /. den -> FeynArts`FAPropagatorDenominator[mom, 0]];
  If[! FreeQ[out, FeynCalc`Pair | FeynCalc`FeynAmpDenominator | FeynCalc`FAD],
    Return[faModelFailure["UntranslatedPropagator", "The typed propagator map left an unsupported FeynCalc head."]]];
  out
];
faBasisRecord[e_, family_String] := Module[{b, translated},
  If[! FreeQ[e, FeynGrav`GravitonVertex | FeynGrav`GravitonGhostVertex],
    Return[faModelFailure["UnevaluatedSourceRule", "A source vertex remained unevaluated; no self-contained snapshot can be published.", <|"Family" -> family|>]]];
  b = faExtractVertex[e, 100000];
  If[FailureQ[b], Return[b]];
  (* Structural equality is exact for the stored source expressions; not independent physics proof. *)
  If[b["Reconstruction"] =!= e && ! TrueQ[Expand[b["Reconstruction"] - e] === 0],
    Return[faModelFailure["RuleReconstruction", "The extracted basis did not reproduce the source expression exactly.", <|"Family" -> family|>]]];
  translated = faTranslateMonomial /@ b["Monomials"];
  If[! FreeQ[translated, FeynCalc`Pair] ||
     ! And @@ (FreeQ[#, FeynCalc`Pair] & /@ b["Coefficients"]),
    Return[faModelFailure["UntranslatedVertex", "A kinematic atom leaked into a class coefficient or the generic vector.", <|"Family" -> family|>]]];
  <|"Vector" -> translated, "Coefficients" -> b["Coefficients"],
    "TermCount" -> b["TermCount"],
    "RuleSHA256" -> Hash[e, "SHA256", "HexString"]|>
];

(* The verified quintic contains 722 whole-leg permutation orbits. Use the
   prepared seed only for its exact imported source. Changed libraries use the
   general basis extractor, just as before. *)
faBarePairQ[a_, b_] := With[{value = FeynCalc`Pair[a, b]},
  HoldComplete[value] === HoldComplete[FeynCalc`Pair[a, b]] ||
  HoldComplete[value] === HoldComplete[FeynCalc`Pair[b, a]]];
faGenericQuinticSlotsQ[legs_] := Module[{indices, vectors, roles},
  If[!MatchQ[legs, {{_Symbol,_Symbol,_Symbol}..}] || Length[legs] =!= 5 ||
      !DuplicateFreeQ[Flatten[legs]], Return[False]];
  indices = FeynCalc`LorentzIndex[#, D] & /@ Flatten[legs[[All,1;;2]]];
  vectors = FeynCalc`Momentum[#, D] & /@ legs[[All,3]];
  roles = Join[indices,vectors];
  AllTrue[Subsets[roles,{2}], faBarePairQ @@ # &] && AllTrue[vectors, faBarePairQ[#,#] &]
];
faVerifiedQuinticQ[info_Association, indices_, momenta_] := Module[{sources, slots},
  sources = Select[info["Files"], #["Family"] === "GravitonVertex" && #["Order"] === 3 &];
  If[Lookup[sources, "SHA256", None] =!= {"c9deb4b247b58d2c7b5b6ddf9ad7f286c450df073517fc1d06956095f1ccce42"},
    Return[False]];
  (* Literal scalar products set on the source or destination names would
     distinguish the live rule from the generic, off-shell template. *)
  slots = Cases[FeynGrav`Private`DummyArrayMomentaVariables[5], Verbatim[Pattern][x_Symbol,_Blank] :> x];
  faGenericQuinticSlotsQ[Partition[slots,3]] &&
    faGenericQuinticSlotsQ[MapThread[Append,{Partition[indices,2],momenta}]]
];
faQuinticRecord[indices_, momenta_] := Module[{legs, definition, digest},
  digest = "a9d88900d86e791275de54770550e43a08813ba00e835dbc6a20bdffbc49aa43";
  If[!FileExistsQ[$faQuinticTemplate] || FileHash[$faQuinticTemplate, "SHA256", "HexString"] =!= digest,
    Return[faModelFailure["InvalidQuinticTemplate", "The verified quintic kinematic template is missing or has changed."]]];
  definition = Quiet[Check[Import[$faQuinticTemplate, "Text", CharacterEncoding -> "UTF-8"], $Failed]];
  If[!StringQ[definition] || FileHash[$faQuinticTemplate, "SHA256", "HexString"] =!= digest,
    Return[faModelFailure["InvalidQuinticTemplate", "The quintic template could not be read consistently."]]];
  legs = MapThread[Prepend, {Partition[indices, 2], momenta}];
  <|"Vector" -> {FeynGravGR5`Private`tensor[FeynGravGR5`Private`legs @@ legs]},
    "Coefficients" -> {I FeynGrav`\[Kappa]^3}, "TermCount" -> 1,
    "Definition" -> definition,
    "RuleSHA256" -> "af42b95afbade489813f27051a69eba39bbec9f16a6e4af845d762b32ffc29d9"|>
];

faModelCore[name_, gravitonBases_List, gh_, hp_, up_] := Module[
  {tensor, ghost, gravitonGeneric, gravitonClasses, generic, classes, classrows},
  tensor[n_Integer] := "AnalyticalCoupling[" <> StringRiffle[Table[
    "s" <> ToString[i] <> " T[j" <> ToString[i] <> ",mom" <> ToString[i] <>
      ",{li" <> ToString[2 i - 1] <> ",li" <> ToString[2 i] <> "}]", {i, n}], ","] <> "]==G[1][" <>
    StringRiffle[Table["s" <> ToString[i] <> " T[j" <> ToString[i] <> "]", {i, n}], ","] <> "].";
  (* Explicit antighost slot 2, ghost slot 3; source cubic function takes ghost FIRST. *)
  ghost = "AnalyticalCoupling[s1 T[j1,mom1,{li1,li2}],s2 U[j2,mom2,{li3}],s3 U[j3,mom3,{li4}]]==G[1][s1 T[j1],s2 U[j2],s3 U[j3]].";
  classrows[c_] := faEncode[List /@ c];
  (* One generic coupling and one classes row per requested graviton valence. *)
  gravitonGeneric = Table[tensor[3 + i - 1] <> faEncode[gravitonBases[[i]]["Vector"]],
    {i, Length[gravitonBases]}];
  gravitonClasses = Table["C[" <> StringRiffle[ConstantArray["T[1]", 3 + i - 1], ","] <> "]==" <>
    classrows[gravitonBases[[i]]["Coefficients"]], {i, Length[gravitonBases]}];
  generic = "(* Original pure-GR model; FeynGrav rules snapshotted by explicit build. *)\n" <>
    "KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[U]={Lorentz};\n" <>
    "$FermionLines=True; P$NonCommuting=U; P$ChainBuildOrder={U};\n" <>
    "Attributes[FAMetricTensor]={Orderless};\n" <>
    "M$GenericPropagators={" <>
    "AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2]," <>
    "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]==" <> faEncode[hp] <> "," <>
    "AnalyticalPropagator[External][s1 U[j1,mom,{li1}]]==1," <>
    "AnalyticalPropagator[Internal][s1 U[j1,mom,{li1}->{li2}]]==" <> faEncode[up] <> "};\n" <>
    StringJoin[Lookup[gravitonBases,"Definition",""]] <>
    "M$GenericCouplings={" <>
      StringRiffle[Join[gravitonGeneric, {ghost <> faEncode[gh["Vector"]]}], ","] <> "};\n" <>
    "M$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\n" <>
    "M$TruncationRules={_Global`FAPolarizationTensor->1}; M$LastGenericRules={};\n";
  classes = "M$ClassesDescription={" <>
    "T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine}," <>
    "U[1]=={SelfConjugate->False,Mass->0,PropagatorLabel->\"c\",PropagatorType->GhostDash,PropagatorArrow->Forward}};\n" <>
    "M$CouplingMatrices={" <>
      StringRiffle[Join[gravitonClasses,
        {"C[T[1],-U[1],U[1]]==" <> classrows[gh["Coefficients"]]}], ","] <> "};\n" <>
    "M$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices; M$LastModelRules={};\n";
  {generic, classes}
];
(* Order selection. Order n mirrors FeynGravImportGravitons[n] and means graviton
   couplings with up to n + 2 legs: order 2 is the cubic/quartic snapshot that
   the packaged GR model uses, order 3 adds the five-graviton vertex. The
   ghost sector is part of every order. *)
faGravitonOrderValues = {2, 3};
faValidGravitonOrderQ[order_] := MemberQ[faGravitonOrderValues, order];
faOrderSpecification[order_Integer] := <|
  "GravitonOrder" -> order, "ImportOrder" -> order,
  "GravitonOrders" -> Range[order], "LegCounts" -> Range[3, order + 2],
  "Families" -> If[order >= 3, {"TTT", "TTTT", "TTTTT", "TUU"}, {"TTT", "TTTT", "TUU"}],
  "BasisLabels" -> Join[{"ThreeGraviton", "FourGraviton", "FiveGraviton"}[[1 ;; order]], {"GravitonGhost"}]|>;
(* FeynGravImportGravitons[n] caps silently at the highest installed order, so the
   recorded orders are compared with the requested ones and any shortfall fails
   before rule construction instead of publishing a truncated snapshot. *)
faImportedOrderFailure[info_, spec_] := Module[{recorded},
  If[! AssociationQ[info],
    Return[faModelFailure["LibraryInformationUnavailable",
      "The graviton import did not record library information, so the requested order is unverified."]]];
  recorded = Lookup[Lookup[info, "Orders", <||>], "GravitonVertex", {}];
  If[recorded =!= spec["GravitonOrders"],
    Return[faModelFailure["LibraryOrderUnavailable",
      "The requested graviton order is not installed: FeynGravImportGravitons caps at the highest available order, so continuing would publish a snapshot without the requested vertices.",
      <|"Requested" -> spec["GravitonOrders"], "Imported" -> recorded,
        "RequiredLibrary" -> "Libs/GravitonVertex_" <> ToString[spec["GravitonOrder"]]|>]]];
  If[Lookup[info, "ChangedSettings", {"Unverified"}] =!= {},
    Return[faModelFailure["IncompleteLibrary", "Imported library settings do not match the fixed model."]]];
  True
];
faBuildModel[target_String, order_] := Module[
  {dep, parent, stage, imp, info, libFail, spec, gravitonBases, indices, momenta, quintic,
    rules, basis, props, hash, name, contents, gen, mod, manifest, success},
  (* Validate the requested order before any dependency, import or rule work. *)
  If[! faValidGravitonOrderQ[order],
    Return[faModelFailure["InvalidGravitonOrder",
      "Supply GravitonOrder -> 2 (default: cubic and quartic gravitons plus the ghost sector) or GravitonOrder -> 3 (adds the five-graviton vertex).",
      <|"Requested" -> order, "Supported" -> faGravitonOrderValues|>]]];
  If[! StringStartsQ[target, "/"] || ExpandFileName[target] =!= target,
    Return[faModelFailure["InvalidModelPath", "Supply a normalised absolute, new model directory (no relative components)."]]];
  parent = DirectoryName[target];
  If[! DirectoryQ[parent] || FileExistsQ[target],
    Return[faModelFailure["InvalidModelPath", "The parent must exist and the destination must not exist.", <|"Directory" -> target|>]]];
  dep = faDependencyCheck[]; If[FailureQ[dep], Return[dep]];
  If[StringStartsQ[ExpandFileName[target], FileNameJoin[{$faSourceRoot, "Libs"}] <> "/"] ||
      StringStartsQ[ExpandFileName[target], ExpandFileName[ToString[FeynArts`$FeynArtsDir]]],
    Return[faModelFailure["ForbiddenDestination", "Do not build inside shipped libraries or the installed FeynArts package."]]];
  spec = faOrderSpecification[order];
  imp = FeynGrav`FeynGravImportGravitons[spec["ImportOrder"]]; If[FailureQ[imp], Return[imp]];
  imp = FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportGravitons];
  libFail = faImportedOrderFailure[imp, spec]; If[FailureQ[libFail], Return[libFail]];
  (* Slot tables cover the requested order only; the quintic slots exist only
     for GravitonOrder -> 3, so the default build never evaluates a
     five-graviton rule. *)
  indices = Table[Symbol["FeynArts`li" <> ToString[i]], {i, 2 (order + 2)}];
  momenta = Table[Symbol["FeynArts`mom" <> ToString[i]], {i, order + 2}];
  quintic = If[order === 3 && faVerifiedQuinticQ[imp, indices, momenta], faQuinticRecord[indices, momenta], None];
  If[FailureQ[quintic], Return[quintic]];
  (* A source-digest marker avoids evaluating or hashing a second copy of
     the expanded quintic after the requested libraries have been imported. *)
  rules = Join[
    Table[If[legs === 5 && AssociationQ[quintic], {"VerifiedQuintic", quintic["RuleSHA256"]},
      FeynGrav`GravitonVertex @@ Flatten[Transpose[
        {Partition[indices[[1 ;; 2 legs]], 2], momenta[[1 ;; legs]]}]]], {legs, spec["LegCounts"]}],
    {FeynGrav`GravitonGhostVertex[indices[[1]], indices[[2]], momenta[[1]],
      indices[[4]], momenta[[3]], indices[[3]], momenta[[2]]]}];
  If[AnyTrue[rules, FailureQ],
    Return[faModelFailure["MissingRule", "One or more source vertices were not available."]]];
  basis = MapThread[If[#2 === "FiveGraviton" && AssociationQ[quintic], quintic,
    faBasisRecord[#1, #2]] &, {rules, spec["BasisLabels"]}];
  If[AnyTrue[basis, FailureQ], Return[First[Select[basis, FailureQ]]]];
  props = {faPropagator[FeynCalc`FCI[FeynGrav`GravitonPropagator[indices[[1]],indices[[2]],indices[[3]],indices[[4]],FeynArts`mom]],FeynArts`mom],
    faPropagator[FeynCalc`FCI[FeynGrav`GhostVectorPropagator[indices[[1]],indices[[2]],FeynArts`mom]],FeynArts`mom]};
  If[AnyTrue[props, FailureQ], Return[First[Select[props, FailureQ]]]];
  hash = Hash[Join[rules, props], "SHA256", "HexString"];
  (* The model name is a fixed, symbol-safe identifier. The snapshot is pinned by
     "SnapshotSHA256" in the manifest and by the per-file hashes, not by the name. *)
  name = "GR";
  gravitonBases = Most[basis];
  contents = faModelCore[name, gravitonBases, Last[basis], Sequence @@ props];
  stage = CreateDirectory[FileNameJoin[{parent, ".fgfa-stage-" <> CreateUUID[]}]];
  gen = FileNameJoin[{stage, name <> ".gen"}]; mod = FileNameJoin[{stage, name <> ".mod"}];
  manifest = <|"Schema" -> 1, "Profile" -> "GR", "ModelName" -> name,
    "FixedGauge" -> 2, "LorentzDimension" -> "Symbolic D", "FieldClasses" -> {"T[1]", "U[1]", "-U[1]"},
    "InteractionFamilies" -> spec["Families"], "GravitonOrders" -> spec["GravitonOrders"],
    "RuleSHA256" -> (Lookup[#, "RuleSHA256"] & /@ basis), "SnapshotSHA256" -> hash,
    "LibrarySources" -> imp["Files"], "FeynArtsVersion" -> ToString[FeynArts`$FeynArtsVersionNumber]|>;
  If[AssociationQ[quintic], manifest = Append[manifest, "QuinticRepresentation" -> "SymmetricSeed-v1"]];
  success = Quiet[Check[
    Export[gen, contents[[1]], "Text"]; Export[mod, contents[[2]], "Text"];
    manifest = Join[manifest, <|"ModelFileSHA256" -> <|
      "Generic" -> FileHash[gen, "SHA256", "HexString"],
      "Classes" -> FileHash[mod, "SHA256", "HexString"]|>|>];
    Export[FileNameJoin[{stage, "manifest.json"}], manifest, "RawJSON"];
    FileExistsQ[gen] && FileExistsQ[mod] && FileExistsQ[FileNameJoin[{stage, "manifest.json"}]],
    False]];
  If[! TrueQ[success], Return[faModelFailure["ModelWriteFailed", "Staged bundle could not be written; staging retained.", <|"Staging" -> stage|>]]];
  (* Ensure FeynArts genuinely accepts all field and coupling definitions before publication. *)
  success = Quiet[Check[Block[{FeynArts`$ModelPath = Prepend[FeynArts`$ModelPath, stage]},
    TimeConstrained[FeynArts`InitializeModel[name, FeynArts`GenericModel -> name], 600, $TimedOut]], $Failed]];
  If[success =!= True,
    Return[faModelFailure["ModelValidationFailed", "FeynArts rejected or timed out on the staged bundle; staging retained.", <|"Staging" -> stage, "Diagnostic" -> success|>]]];
  If[FileExistsQ[target],
    Return[faModelFailure["DestinationChanged", "The requested target appeared during validation; staging retained.", <|"Staging" -> stage|>]]];
  success = Quiet[Check[RenameDirectory[stage, target], $Failed]];
  If[success === $Failed,
    Return[faModelFailure["ModelPublishFailed", "Could not move the validated bundle; staging retained.", <|"Staging" -> stage|>]]];
  <|"Directory" -> ExpandFileName[target], "ModelName" -> name,
    "GenericModel" -> FileNameJoin[{ExpandFileName[target], name <> ".gen"}],
    "ClassesModel" -> FileNameJoin[{ExpandFileName[target], name <> ".mod"}],
    "Manifest" -> FileNameJoin[{ExpandFileName[target], "manifest.json"}],
    "VertexBasisSizes" -> Lookup[basis, "TermCount"], "GhostLoopEnabled" -> False|>
];
faBuildModel[___] := faModelFailure["InvalidModelPath",
  "Supply one absolute, new model directory and at most a GravitonOrder -> 2 or 3 option."];
(* Public entry point. The default order is the inexpensive cubic/quartic
   snapshot; the five-graviton vertex exists only for an explicit
   GravitonOrder -> 3 request. *)
Options[FeynGrav`FeynArtsIntegration`FeynGravBuildModel] = {
  FeynGrav`FeynArtsIntegration`GravitonOrder -> 2};
FeynGrav`FeynArtsIntegration`FeynGravBuildModel[target_String, opts : (_Rule ...)] := Module[{unknown},
  (* An unrecognised option is reported, never ignored: a mistyped order option
     must not silently fall back to the default. *)
  unknown = Complement[First /@ {opts},
    First /@ Options[FeynGrav`FeynArtsIntegration`FeynGravBuildModel]];
  If[unknown =!= {},
    Return[faModelFailure["InvalidOption",
      "Only GravitonOrder -> 2 or 3 is accepted.",
      <|"UnknownOptions" -> unknown|>]]];
  faBuildModel[target, FeynGrav`FeynArtsIntegration`GravitonOrder /. {opts} /.
    Options[FeynGrav`FeynArtsIntegration`FeynGravBuildModel]]];
FeynGrav`FeynArtsIntegration`FeynGravBuildModel[___] :=
  faModelFailure["InvalidOption",
    "Use FeynGravBuildModel[absoluteNewDirectory] with the optional GravitonOrder -> 2 or 3."];
End[];
