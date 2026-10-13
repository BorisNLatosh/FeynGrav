(* Finite pure-GR model snapshot builder; called explicitly by the optional companion. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
$faSourceRoot = DirectoryName[DirectoryName[$InputFileName]];
ClearAll[faModelFailure, faEncode, faPropagator, faBasisRecord, faModelCore,
  faBuildModel, faDependencyCheck, faRoleFreeQ];
faModelFailure[tag_, text_, data_: <||>] := Failure[tag,
  Join[<|"MessageTemplate" -> text, "Stage" -> "ModelBuild"|>, data]];
(* The encoder exposes ONLY FeynArts kinematic placeholder symbols unqualified;
   Global patched FA tensor heads, FeynGrav couplings and other contexts stay typed. *)
faEncode[e_] := Block[{$Context = "FeynGrav`FeynArtsIntegration`Private`",
    $ContextPath = {"System`", "FeynArts`"}}, ToString[e, InputForm]];
faRoleFreeQ[] := And @@ (OwnValues[#] === {} & /@ Join[
   Table[Symbol["FeynArts`li" <> ToString[i]], {i, 8}],
   Append[Table[Symbol["FeynArts`mom" <> ToString[i]], {i, 4}], FeynArts`mom]]);
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
  b = faExtractVertex[e, 12000];
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
faModelCore[name_, g3_, g4_, gh_, hp_, up_] := Module[{tensor, ghost, generic, classes, classrows},
  tensor[n_Integer] := "AnalyticalCoupling[" <> StringRiffle[Table[
    "s" <> ToString[i] <> " T[j" <> ToString[i] <> ",mom" <> ToString[i] <>
      ",{li" <> ToString[2 i - 1] <> ",li" <> ToString[2 i] <> "}]", {i, n}], ","] <> "]==G[1][" <>
    StringRiffle[Table["s" <> ToString[i] <> " T[j" <> ToString[i] <> "]", {i, n}], ","] <> "].";
  (* Explicit antighost slot 2, ghost slot 3; source cubic function takes ghost FIRST. *)
  ghost = "AnalyticalCoupling[s1 T[j1,mom1,{li1,li2}],s2 U[j2,mom2,{li3}],s3 U[j3,mom3,{li4}]]==G[1][s1 T[j1],s2 U[j2],s3 U[j3]].";
  classrows[c_] := faEncode[List /@ c];
  generic = "(* Original pure-GR model; FeynGrav rules snapshotted by explicit build. *)\n" <>
    "KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[U]={Lorentz};\n" <>
    "$FermionLines=True; P$NonCommuting=U; P$ChainBuildOrder={U};\n" <>
    "Attributes[FAMetricTensor]={Orderless};\n" <>
    "M$GenericPropagators={" <>
    "AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2]," <>
    "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]==" <> faEncode[hp] <> "," <>
    "AnalyticalPropagator[External][s1 U[j1,mom,{li1}]]==1," <>
    "AnalyticalPropagator[Internal][s1 U[j1,mom,{li1}->{li2}]]==" <> faEncode[up] <> "};\n" <>
    "M$GenericCouplings={" <> tensor[3] <> faEncode[g3["Vector"]] <> "," <>
      tensor[4] <> faEncode[g4["Vector"]] <> "," <>
      ghost <> faEncode[gh["Vector"]] <> "};\n" <>
    "M$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\n" <>
    "M$TruncationRules={_Global`FAPolarizationTensor->1}; M$LastGenericRules={};\n";
  classes = "M$ClassesDescription={" <>
    "T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine}," <>
    "U[1]=={SelfConjugate->False,Mass->0,PropagatorLabel->\"c\",PropagatorType->GhostDash,PropagatorArrow->Forward}};\n" <>
    "M$CouplingMatrices={" <>
    "C[T[1],T[1],T[1]]==" <> classrows[g3["Coefficients"]] <> "," <>
    "C[T[1],T[1],T[1],T[1]]==" <> classrows[g4["Coefficients"]] <> "," <>
    "C[T[1],-U[1],U[1]]==" <> classrows[gh["Coefficients"]] <> "};\n" <>
    "M$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices; M$LastModelRules={};\n";
  {generic, classes}
];
faBuildModel[target_String] := Module[
  {dep, parent, stage, imp, indices, momenta, rules, basis, props, hash, name,
    contents, gen, mod, manifest, success},
  If[! StringStartsQ[target, "/"] || ExpandFileName[target] =!= target,
    Return[faModelFailure["InvalidModelPath", "Supply a normalised absolute, new model directory (no relative components)."]]];
  parent = DirectoryName[target];
  If[! DirectoryQ[parent] || FileExistsQ[target],
    Return[faModelFailure["InvalidModelPath", "The parent must exist and the destination must not exist.", <|"Directory" -> target|>]]];
  dep = faDependencyCheck[]; If[FailureQ[dep], Return[dep]];
  If[StringStartsQ[ExpandFileName[target], FileNameJoin[{$faSourceRoot, "Libs"}] <> "/"] ||
      StringStartsQ[ExpandFileName[target], ExpandFileName[ToString[FeynArts`$FeynArtsDir]]],
    Return[faModelFailure["ForbiddenDestination", "Do not build inside shipped libraries or the installed FeynArts package."]]];
  imp = FeynGrav`importGravitons[2]; If[FailureQ[imp], Return[imp]];
  imp = FeynGrav`FeynGravLibraryInformation[FeynGrav`importGravitons];
  If[! AssociationQ[imp] || Lookup[Lookup[imp, "Orders", <||>], "GravitonVertex", {}] =!= {1, 2} ||
      Lookup[imp, "ChangedSettings", {"Unverified"}] =!= {},
    Return[faModelFailure["IncompleteLibrary", "Actual imported graviton orders or settings do not match the fixed model."]]];
  indices = Table[Symbol["FeynArts`li" <> ToString[i]], {i, 8}];
  momenta = Table[Symbol["FeynArts`mom" <> ToString[i]], {i, 4}];
  rules = {
    FeynGrav`GravitonVertex @@ Flatten[Transpose[{Partition[indices[[1 ;; 6]], 2], momenta[[1 ;; 3]]}]],
    FeynGrav`GravitonVertex @@ Flatten[Transpose[{Partition[indices, 2], momenta}]],
    FeynGrav`GravitonGhostVertex[indices[[1]], indices[[2]], momenta[[1]],
      indices[[4]], momenta[[3]], indices[[3]], momenta[[2]]]
  };
  If[AnyTrue[rules, FailureQ],
    Return[faModelFailure["MissingRule", "One or more source vertices were not available."]]];
  basis = MapThread[faBasisRecord, {rules, {"ThreeGraviton", "FourGraviton", "GravitonGhost"}}];
  If[AnyTrue[basis, FailureQ], Return[First[Select[basis, FailureQ]]]];
  props = {faPropagator[FeynCalc`FCI[FeynGrav`GravitonPropagator[indices[[1]],indices[[2]],indices[[3]],indices[[4]],FeynArts`mom]],FeynArts`mom],
    faPropagator[FeynCalc`FCI[FeynGrav`GhostVectorPropagator[indices[[1]],indices[[2]],FeynArts`mom]],FeynArts`mom]};
  If[AnyTrue[props, FailureQ], Return[First[Select[props, FailureQ]]]];
  hash = Hash[Join[rules, props], "SHA256", "HexString"];
  (* The model name is a fixed, symbol-safe identifier. The snapshot is pinned by
     "SnapshotSHA256" in the manifest and by the per-file hashes, not by the name. *)
  name = "PureGR";
  contents = faModelCore[name, Sequence @@ basis, Sequence @@ props];
  stage = CreateDirectory[FileNameJoin[{parent, ".fgfa-stage-" <> CreateUUID[]}]];
  gen = FileNameJoin[{stage, name <> ".gen"}]; mod = FileNameJoin[{stage, name <> ".mod"}];
  manifest = <|"Schema" -> 1, "Profile" -> "PureGR", "ModelName" -> name,
    "FixedGauge" -> 2, "LorentzDimension" -> "Symbolic D", "FieldClasses" -> {"T[1]", "U[1]", "-U[1]"},
    "InteractionFamilies" -> {"TTT", "TTTT", "TUU"}, "GravitonOrders" -> {1, 2},
    "RuleSHA256" -> (Lookup[#, "RuleSHA256"] & /@ basis), "SnapshotSHA256" -> hash,
    "LibrarySources" -> imp["Files"], "FeynArtsVersion" -> ToString[FeynArts`$FeynArtsVersionNumber]|>;
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
    TimeConstrained[FeynArts`InitializeModel[name, FeynArts`GenericModel -> name], 90, $TimedOut]], $Failed]];
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
faBuildModel[___] := faModelFailure["InvalidModelPath", "Supply one absolute, new model directory."];
FeynGrav`FeynArtsIntegration`FeynGravBuildModel[target_String] := faBuildModel[target];
FeynGrav`FeynArtsIntegration`FeynGravBuildModel[___] := faBuildModel[];
End[];
