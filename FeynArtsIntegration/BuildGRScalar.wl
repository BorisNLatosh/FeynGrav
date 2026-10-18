(* ::Package:: *)

(* BuildGRScalar.wl
   Reproducible developer build for the finite GRScalar native FeynArts bundle
   (pure GR plus one real, self-conjugate, minimally coupled massive scalar).

   This is a focused developer helper, NOT a public API and NOT a general model
   framework. It reuses the tested FeynGrav basis extraction and typed
   translation (Basis.wl) and the tested encoder/failure plumbing (ModelBuilder.wl)
   through the optional companion; it does NOT modify the tested PureGR builder.
   The two model files it produces are self-contained and need none of this
   machinery at load time.

   The repository is located from this file's own $InputFileName, so the script
   is portable and contains no machine-specific source hash or package-version
   pin. Exact snapshot/version identity for a run is asserted by the separate
   verification harness, not required here.

   Command line (an explicit target is recommended; a unique temporary
   directory is derived only when none is supplied):
     GRSCALAR_TARGET=/abs/new/dir WolframKernel -noinit -script \
       /abs/repo/FeynArtsIntegration/BuildGRScalar.wl
   or positionally:
     WolframKernel -noinit -script /abs/repo/.../BuildGRScalar.wl /abs/new/dir
   Optional: GRSCALAR_RESULT=/abs/path.json for the machine-readable summary. *)

$HistoryLength = 0;

(* ------------------------------------------------------------------ *)
(* Reporting plumbing (defined before any prerequisite is evaluated)    *)
(* ------------------------------------------------------------------ *)

resultPath = Module[{e = Environment["GRSCALAR_RESULT"]},
  If[StringQ[e] && StringStartsQ[e, "/"], e,
    FileNameJoin[{$TemporaryDirectory,
      "grscalar-build-result-" <> CreateUUID[] <> ".json"}]]];

checks = {};
emit[id_String, label_String, passed_] := Module[{ok = TrueQ[passed]},
  AppendTo[checks, <|"Id" -> id, "Label" -> label, "Passed" -> ok,
    "Detail" -> If[ok, "PASS", ToString[passed, InputForm, PageWidth -> Infinity]]|>];
  Print["CHECK\t", id, "\t", If[ok, "PASS", "FAIL"], "\t", label];
  ok];

hardFail[stage_String, data_] := (
  Print["HARD-FAIL\t", stage, "\t", ToString[data, InputForm, PageWidth -> Infinity]];
  Export[resultPath, <|"Completed" -> False, "Stage" -> stage,
    "Data" -> ToString[data, InputForm, PageWidth -> Infinity], "Checks" -> checks|>, "RawJSON"];
  Exit[1]);

(* A failed prerequisite stops the build immediately: no import, index, model
   or bundle work proceeds after an unmet requirement. *)
require[stage_String, label_String, ok_] := (
  If[! emit[stage, label, ok], hardFail[stage, label]];
  True);

(* ------------------------------------------------------------------ *)
(* Repository location and target resolution (validated early)          *)
(* ------------------------------------------------------------------ *)

scriptPath = ExpandFileName[$InputFileName];
repo = ExpandFileName[DirectoryName[DirectoryName[scriptPath]]];

targetInfo = Module[{env, args, exe, derived},
  exe = If[Length[$ScriptCommandLine] > 0, First[$ScriptCommandLine], ""];
  env = Environment["GRSCALAR_TARGET"];
  Which[
    StringQ[env] && StringStartsQ[env, "/"],
      <|"Path" -> env, "Created" -> False, "Source" -> "GRSCALAR_TARGET"|>,
    (* Drop the script path and the interpreter path explicitly: a bare
       `-script file.wl` invocation must not be mistaken for a target. *)
    (args = Select[Rest[$ScriptCommandLine],
        StringQ[#] && StringStartsQ[#, "/"] &&
          ExpandFileName[#] =!= scriptPath && ExpandFileName[#] =!= ExpandFileName[exe] &]) =!= {},
      <|"Path" -> First[args], "Created" -> False, "Source" -> "script-argument"|>,
    True,
      derived = CreateDirectory[];
      <|"Path" -> derived, "Created" -> True, "Source" -> "derived-temp"|>]];

buildTarget = targetInfo["Path"];

If[! DirectoryQ[repo],
  hardFail["repository", repo]];
If[!StringQ[buildTarget] || ! StringStartsQ[buildTarget, "/"],
  hardFail["invalid-target", buildTarget]];
If[ExpandFileName[buildTarget] =!= buildTarget,
  hardFail["invalid-target-not-normalised", buildTarget]];
If[! TrueQ[targetInfo["Created"]] && FileExistsQ[buildTarget],
  hardFail["target-exists", buildTarget]];
If[! DirectoryQ[DirectoryName[buildTarget]],
  hardFail["target-parent-missing", DirectoryName[buildTarget]]];

(* ------------------------------------------------------------------ *)
(* Load FeynGrav, the patched FeynArts, then the optional companion     *)
(* (documented order: definitions before native model initialisation).  *)
(* ------------------------------------------------------------------ *)

$LoadAddOns = {"FeynArts"};
Get[FileNameJoin[{repo, "FeynGrav.wl"}]];
FeynArts`$FAVerbose = 2;
Get[FileNameJoin[{repo, "FeynArtsIntegration", "FeynArtsIntegration.wl"}]];

(* Explicit wrappers onto the tested private helpers. A context-valued symbol
   (e.g. priv = FeynGrav`...`Private`) does NOT alias a namespace, so the
   helpers are bound through real definitions here. *)
faEncode[e_] := FeynGrav`FeynArtsIntegration`Private`faEncode[e];
faBasisRecord[e_, f_] := FeynGrav`FeynArtsIntegration`Private`faBasisRecord[e, f];
faPropagator[e_, m_] := FeynGrav`FeynArtsIntegration`Private`faPropagator[e, m];
faTranslateMonomial[e_] := FeynGrav`FeynArtsIntegration`Private`faTranslateMonomial[e];
faExtractVertex[e_, c_] := FeynGrav`FeynArtsIntegration`Private`faExtractVertex[e, c];
faModelFailure[args___] := FeynGrav`FeynArtsIntegration`Private`faModelFailure[args];

(* Functional prerequisites only. No exact source hash or package-version pin:
   this run's identity is recorded and asserted by the verification harness. *)
require["B-01", "fresh kernel with symbolic D/kappa, GaugeFixingEpsilon = 2, FCFAConvert present",
  MemberQ[$Packages, "FeynGrav`"] && MemberQ[$Packages, "FeynArts`"] &&
  OwnValues[D] === {} && OwnValues[FeynGrav`\[Kappa]] === {} &&
  FeynGrav`GaugeFixingEpsilon === 2 &&
  DownValues[FeynCalc`FCFAConvert] =!= {}];

buildEnvironment = <|
  "Version" -> $Version,
  "FeynCalc" -> FeynCalc`$FeynCalcVersion,
  "FeynArts" -> ToString[FeynArts`$FeynArtsVersionNumber],
  "FeynGravSHA256" -> FileHash[FileNameJoin[{repo, "FeynGrav.wl"}], "SHA256", "HexString"]|>;

(* The target must not live inside shipped libraries or the installed package. *)
require["B-02", "target is outside shipped libraries and the installed FeynArts",
  ! StringStartsQ[buildTarget, FileNameJoin[{repo, "Libs"}] <> "/"] &&
  ! StringStartsQ[buildTarget, ExpandFileName[ToString[FeynArts`$FeynArtsDir]] <> "/"]];

(* ------------------------------------------------------------------ *)
(* Import the gravity and scalar rule libraries at order 2             *)
(* ------------------------------------------------------------------ *)

impG = FeynGrav`importGravitons[2];
require["B-03", "graviton kinetic libraries import at order 2", ! FailureQ[impG]];
impS = FeynGrav`importScalars[2];
require["B-04", "scalar kinetic libraries import at order 2", ! FailureQ[impS]];

infoG = FeynGrav`FeynGravLibraryInformation[FeynGrav`importGravitons];
infoS = FeynGrav`FeynGravLibraryInformation[FeynGrav`importScalars];
require["B-05", "recorded orders are exactly the requested ones",
  AssociationQ[infoG] && AssociationQ[infoS] &&
  Lookup[Lookup[infoG, "Orders", <||>], "GravitonVertex", {}] === {1, 2} &&
  Lookup[Lookup[infoS, "Orders", <||>], "GravitonScalarVertex", {}] === {1, 2} &&
  Lookup[Lookup[infoS, "Orders", <||>], "GravitonScalarPotentialVertex", {}] === {1, 2}];
require["B-06", "no recorded import setting changed",
  Lookup[infoG, "ChangedSettings", {"Unverified"}] === {} &&
  Lookup[infoS, "ChangedSettings", {"Unverified"}] === {}];

(* ------------------------------------------------------------------ *)
(* Source vertices and the stable scalar-mass symbol                    *)
(* ------------------------------------------------------------------ *)

scalarMass = FeynGrav`ScalarMass;
jSym = Symbol["FeynArts`j1"];
massPlaceholder = FeynArts`Mass[FeynArts`S[jSym]];

indices = Table[Symbol["FeynArts`li" <> ToString[i]], {i, 8}];
momenta = Table[Symbol["FeynArts`mom" <> ToString[i]], {i, 4}];

rules = <|
  "TTT" -> FeynGrav`GravitonVertex @@
    Flatten[Transpose[{Partition[indices[[1 ;; 6]], 2], momenta[[1 ;; 3]]}]],
  "TTTT" -> FeynGrav`GravitonVertex @@
    Flatten[Transpose[{Partition[indices[[1 ;; 8]], 2], momenta[[1 ;; 4]]}]],
  "TUU" -> FeynGrav`GravitonGhostVertex[indices[[1]], indices[[2]], momenta[[1]],
    indices[[4]], momenta[[3]], indices[[3]], momenta[[2]]],
  "TSS" -> FeynGrav`GravitonScalarVertex[{indices[[1]], indices[[2]]},
    momenta[[2]], momenta[[3]], scalarMass],
  "TTSS" -> FeynGrav`GravitonScalarVertex[{indices[[1]], indices[[2]], indices[[3]],
    indices[[4]]}, momenta[[3]], momenta[[4]], scalarMass]|>;

require["B-07", "all five source vertices evaluate to explicit tensor expressions",
  ! AnyTrue[Values[rules], ! FreeQ[#, FeynGrav`GravitonVertex |
    FeynGrav`GravitonGhostVertex | FeynGrav`GravitonScalarVertex |
    FeynGrav`GravitonScalarPotentialVertex] &]];

families = {"TTT", "TTTT", "TUU", "TSS", "TTSS"};
basisList = Map[faBasisRecord[rules[#], #] &, families];
require["B-08", "every source vertex extracts a finite basis with exact reconstruction",
  ! AnyTrue[basisList, FailureQ]];
basis = AssociationThread[families, basisList];

(* Independent reconstruction from faExtractVertex's untranslated monomials and
   coefficients for the two scalar couplings (the source expression is compared
   in FeynCalc space; no translated FA tensor is ever compared with FeynCalc). *)
reconOK[f_String] := Module[{b = faExtractVertex[rules[f], 100000]},
  ! FailureQ[b] &&
  (b["Reconstruction"] === rules[f] || TrueQ[Expand[b["Reconstruction"] - rules[f]] === 0])];
require["B-09", "TSS and TTSS bases reconstruct their source expressions exactly",
  reconOK["TSS"] && reconOK["TTSS"]];

(* The scalar mass must enter only through even powers (m^2), never m^1. *)
dropEvenMassPowers[x_] := x /. Power[scalarMass, k_Integer?EvenQ] :> 1;
oddMassPowerQ[x_] := ! FreeQ[dropEvenMassPowers[x], scalarMass];
require["B-10", "scalar mass enters every TSS/TTSS coefficient only through even powers",
  AllTrue[Flatten[{basis["TSS"]["Coefficients"], basis["TTSS"]["Coefficients"]}],
    ! oddMassPowerQ[#] &]];

(* ------------------------------------------------------------------ *)
(* Propagators: unchanged gravity/ghost, new massive scalar             *)
(* ------------------------------------------------------------------ *)

hp = faPropagator[FeynCalc`FCI[
  FeynGrav`GravitonPropagator[indices[[1]], indices[[2]], indices[[3]],
    indices[[4]], FeynArts`mom]], FeynArts`mom];
require["B-11", "graviton propagator translates without failure", ! FailureQ[hp]];

up = faPropagator[FeynCalc`FCI[
  FeynGrav`GhostVectorPropagator[indices[[1]], indices[[2]], FeynArts`mom]],
  FeynArts`mom];
require["B-12", "ghost propagator translates without failure", ! FailureQ[up]];
(* Structural equality, not an InputForm string match, against the accepted
   -I FAMetricTensor FAD form. *)
require["B-13", "ghost propagator is the accepted (-I) FAMetricTensor FAD form",
  up === -I*Global`FAMetricTensor[indices[[1]], indices[[2]]]*
    FeynArts`FAPropagatorDenominator[FeynArts`mom, 0]];

(* Narrow needed translation: the existing faPropagator refuses a nonzero mass.
   Translate the genuine massive ScalarPropagator source; the FeynArts
   denominator references the class mass Mass[S[j1]] so it acquires the
   physical class mass with no gauge parameter. *)
scalarPropagator[mass_Symbol] := Module[{e, den, out},
  e = FeynCalc`FCI[FeynGrav`ScalarPropagator[FeynArts`mom, mass]];
  den = FeynCalc`FeynAmpDenominator[FeynCalc`PropagatorDenominator[
    FeynCalc`Momentum[FeynArts`mom, D], mass]];
  If[Count[e, den, Infinity] =!= 1,
    Return[faModelFailure["UnsupportedScalarPropagator",
      "Expected exactly one massive FeynGrav scalar FAD factor."]]];
  out = faTranslateMonomial[
    e /. den -> FeynArts`FAPropagatorDenominator[FeynArts`mom, massPlaceholder]];
  If[! FreeQ[out, FeynCalc`Pair | FeynCalc`FeynAmpDenominator | FeynCalc`FAD],
    Return[faModelFailure["UntranslatedScalarPropagator",
      "The typed scalar-propagator map left an unsupported FeynCalc head."]]];
  out];

sp = scalarPropagator[scalarMass];
require["B-14", "scalar propagator translates without failure", ! FailureQ[sp]];
(* Structural equality: I FAPropagatorDenominator[mom, Mass[S[j1]]], with no
   scalar gauge-fixing parameter. *)
require["B-15", "scalar generic propagator is I FAPropagatorDenominator[mom, Mass[S[j1]]]",
  sp === I*FeynArts`FAPropagatorDenominator[FeynArts`mom, massPlaceholder] &&
  FreeQ[sp, FeynArts`FAGaugeXi]];

(* ------------------------------------------------------------------ *)
(* Assemble the model text                                              *)
(* ------------------------------------------------------------------ *)

legLHS[types_List] := Module[{parts = {}, k = 0},
  Do[
    Switch[types[[i]],
      "T", k += 2;
      AppendTo[parts, "s" <> ToString[i] <> " T[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> ",{li" <> ToString[k - 1] <> ",li" <> ToString[k] <> "}]"],
      "U", k += 1;
      AppendTo[parts, "s" <> ToString[i] <> " U[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> ",{li" <> ToString[k] <> "}]"],
      "S", AppendTo[parts, "s" <> ToString[i] <> " S[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> "]"]],
    {i, Length[types]}];
  parts];

legRHS[types_List] := Table[
  "s" <> ToString[i] <> " " <> types[[i]] <> "[j" <> ToString[i] <> "]",
  {i, Length[types]}];

coupling[types_List, vec_] := "AnalyticalCoupling[" <>
  StringRiffle[legLHS[types], ","] <> "]==G[1][" <>
  StringRiffle[legRHS[types], ","] <> "]." <> faEncode[vec];

classRow[classFields_List, coeffs_] := "C[" <> StringRiffle[classFields, ","] <>
  "]==" <> faEncode[List /@ coeffs];

genericCouplings = {
  coupling[{"T", "T", "T"}, basis["TTT"]["Vector"]],
  coupling[{"T", "T", "T", "T"}, basis["TTTT"]["Vector"]],
  coupling[{"T", "U", "U"}, basis["TUU"]["Vector"]],
  coupling[{"T", "S", "S"}, basis["TSS"]["Vector"]],
  coupling[{"T", "T", "S", "S"}, basis["TTSS"]["Vector"]]};

classCouplings = {
  classRow[{"T[1]", "T[1]", "T[1]"}, basis["TTT"]["Coefficients"]],
  classRow[{"T[1]", "T[1]", "T[1]", "T[1]"}, basis["TTTT"]["Coefficients"]],
  classRow[{"T[1]", "-U[1]", "U[1]"}, basis["TUU"]["Coefficients"]],
  classRow[{"T[1]", "S[1]", "S[1]"}, basis["TSS"]["Coefficients"]],
  classRow[{"T[1]", "T[1]", "S[1]", "S[1]"}, basis["TTSS"]["Coefficients"]]};

genericText = "(* GR plus one real massive scalar; FeynGrav rules snapshotted by explicit build. *)\n" <>
  "KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[U]={Lorentz}; KinematicIndices[S]={};\n" <>
  "$FermionLines=True; P$NonCommuting=U; P$ChainBuildOrder={U};\n" <>
  "Attributes[FAMetricTensor]={Orderless};\n" <>
  "M$GenericPropagators={" <>
  "AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2]," <>
  "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]==" <> faEncode[hp] <> "," <>
  "AnalyticalPropagator[External][s1 U[j1,mom,{li1}]]==1," <>
  "AnalyticalPropagator[Internal][s1 U[j1,mom,{li1}->{li2}]]==" <> faEncode[up] <> "," <>
  "AnalyticalPropagator[External][s1 S[j1,mom]]==1," <>
  "AnalyticalPropagator[Internal][s1 S[j1,mom]]==" <> faEncode[sp] <> "};\n" <>
  "M$GenericCouplings={" <> StringRiffle[genericCouplings, ","] <> "};\n" <>
  "M$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\n" <>
  "M$TruncationRules={_Global`FAPolarizationTensor->1}; M$LastGenericRules={};\n";

classesText = "M$ClassesDescription={" <>
  "T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine}," <>
  "U[1]=={SelfConjugate->False,Mass->0,PropagatorLabel->\"c\",PropagatorType->GhostDash,PropagatorArrow->Forward}," <>
  "S[1]=={SelfConjugate->True,Mass->FeynGrav`ScalarMass,PropagatorLabel->\"s\",PropagatorType->ScalarDash,PropagatorArrow->None}};\n" <>
  "M$CouplingMatrices={" <> StringRiffle[classCouplings, ","] <> "};\n" <>
  "M$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices; M$LastModelRules={};\n";

(* ------------------------------------------------------------------ *)
(* Provenance and snapshot identity                                     *)
(* ------------------------------------------------------------------ *)

librarySources = Join[
  infoG["Files"],
  Select[infoS["Files"], #["Family"] === "GravitonScalarVertex" &]];
snapshotHash = Hash[Join[Values[rules], {hp, up, sp}, {scalarMass}],
  "SHA256", "HexString"];

manifest = <|
  "Schema" -> 1, "Profile" -> "GRScalar", "ModelName" -> "GRScalar",
  "FixedGauge" -> 2, "LorentzDimension" -> "Symbolic D",
  "FieldClasses" -> {"T[1]", "U[1]", "-U[1]", "S[1]"},
  "InteractionFamilies" -> families,
  "GravitonOrders" -> {1, 2},
  "ScalarMassSymbol" -> "FeynGrav`ScalarMass",
  "RuleSHA256" -> (Lookup[#, "RuleSHA256"] & /@ basisList),
  "SnapshotSHA256" -> snapshotHash,
  "LibrarySources" -> librarySources,
  "FeynArtsVersion" -> ToString[FeynArts`$FeynArtsVersionNumber],
  "FeynGravSHA256" -> buildEnvironment["FeynGravSHA256"]|>;

(* ------------------------------------------------------------------ *)
(* Write the staged bundle                                              *)
(* ------------------------------------------------------------------ *)

If[! TrueQ[targetInfo["Created"]], CreateDirectory[buildTarget]];
genFile = FileNameJoin[{buildTarget, "GRScalar.gen"}];
modFile = FileNameJoin[{buildTarget, "GRScalar.mod"}];
manifestFile = FileNameJoin[{buildTarget, "manifest.json"}];
Export[genFile, genericText, "Text"];
Export[modFile, classesText, "Text"];
manifest = Join[manifest, <|"ModelFileSHA256" -> <|
  "Generic" -> FileHash[genFile, "SHA256", "HexString"],
  "Classes" -> FileHash[modFile, "SHA256", "HexString"]|>|>];
Export[manifestFile, manifest, "RawJSON"];

require["B-16", "staged bundle written with all three files",
  FileExistsQ[genFile] && FileExistsQ[modFile] && FileExistsQ[manifestFile]];

(* ------------------------------------------------------------------ *)
(* Self-containment of the written bundle                               *)
(* ------------------------------------------------------------------ *)

genText = Import[genFile, "Text"];
modText = Import[modFile, "Text"];
privateLeak = "FeynGrav`FeynArtsIntegration`" | "Private`" | "faBasisRecord" |
  "faEncode" | "faPropagator" | "faExtractVertex" | "faModelFailure";
ioLeak = "Get[" | "Import[" | "Run[" | "Needs[" | "ReadList[" | "Put[" |
  "CreateDirectory[" | "Export[" | "System`Run";
require["B-17", "generic text is self-contained: no IO, no private builder symbols, no repo path",
  ! StringContainsQ[genText, privateLeak] && ! StringContainsQ[genText, ioLeak] &&
  ! StringContainsQ[modText, privateLeak] && ! StringContainsQ[modText, ioLeak] &&
  ! StringContainsQ[genText, repo] && ! StringContainsQ[modText, repo]];

(* Family inventory is checked against the literal heads this script writes,
   not against re-encoded FeynCalc expressions. *)
require["B-18", "exactly the five required interaction families are present",
  And @@ Table[
    Count[StringPosition[modText, "C[" <> fam <> "]"], {_, _}] === 1,
    {fam, {"T[1],T[1],T[1]", "T[1],T[1],T[1],T[1]", "T[1],-U[1],U[1]",
      "T[1],S[1],S[1]", "T[1],T[1],S[1],S[1]"}}] &&
  StringCount[genText, "AnalyticalCoupling["] === 5];
require["B-19", "no separate scalar mass insertion or scalar self-potential coupling is present",
  ! StringContainsQ[modText, "C[S[1],S[1]]"] &&
  ! StringContainsQ[modText, "C[S[1],S[1],S[1]]"] &&
  ! StringContainsQ[modText, "C[S[1],S[1],S[1],S[1]]"] &&
  ! StringContainsQ[genText, "AnalyticalPropagator[Internal][s1 S[j1,mom]]==1"]];

(* ------------------------------------------------------------------ *)
(* FeynArts validation of the staged bundle                             *)
(* ------------------------------------------------------------------ *)

FeynArts`$ModelPath = Prepend[
  DeleteCases[FeynArts`$ModelPath, buildTarget], buildTarget];
(* Messages are deliberately not suppressed: any warning stays in the log. *)
init = Check[
  FeynArts`InitializeModel["GRScalar", FeynArts`GenericModel -> "GRScalar",
    FeynArts`Reinitialize -> True], $Failed];
require["B-20", "FeynArts initialises the staged GRScalar bundle", TrueQ[init]];

require["B-21", "scalar class mass resolves to the documented symbol",
  FeynArts`TheMass[FeynArts`S[1]] === scalarMass];
require["B-22", "scalar class is self-conjugate and has no gauge parameter",
  FeynArts`SelfConjugate[FeynArts`S[1]] === True &&
  FreeQ[sp, FeynArts`FAGaugeXi]];

(* Class-resolved propagator: substitute the resolved class mass for the generic
   class reference Mass[S[j1]], then take the physical m -> 0 limit. TheMass[S[1]]
   and the generic Mass[S[j1]] are different roles; the latter is only the
   placeholder the class machinery fills in. *)
spClassResolved = sp /. FeynArts`Mass[FeynArts`S[jSym]] -> FeynArts`TheMass[FeynArts`S[1]];
require["B-23", "class-resolved scalar propagator carries the class mass and is massless at m = 0",
  spClassResolved === I*FeynArts`FAPropagatorDenominator[FeynArts`mom, scalarMass] &&
  (spClassResolved /. scalarMass -> 0) ===
    I*FeynArts`FAPropagatorDenominator[FeynArts`mom, 0]];

(* ------------------------------------------------------------------ *)
(* Local native smoke: the single TSS contact vertex                    *)
(* ------------------------------------------------------------------ *)

norm[x_] := Expand[FeynCalc`ExpandScalarProduct[
  FeynCalc`MomentumExpand[FeynCalc`FCI[x]]]];

smokeTop = FeynArts`CreateTopologies[0, 1 -> 2, FeynArts`Adjacencies -> {3}];
smokeIns = FeynArts`InsertFields[smokeTop, {FeynArts`T[1]} -> {FeynArts`S[1], FeynArts`S[1]},
  FeynArts`Model -> "GRScalar", FeynArts`GenericModel -> "GRScalar",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
smokeRaw = FeynArts`CreateFeynAmp[smokeIns, FeynArts`PreFactor -> 1,
  FeynArts`GaugeRules -> {}, FeynArts`MomentumConservation -> True,
  FeynArts`Truncated -> True];
smokeFC = Check[FeynCalc`FCFAConvert[smokeRaw, FeynCalc`IncomingMomenta -> {p},
  FeynCalc`OutgoingMomenta -> {q1, q2}, FeynCalc`LorentzIndexNames -> {muA, nuA},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  System`List -> True, FeynCalc`Prefactor -> 1], $Failed];
smokeExpected = FeynGrav`GravitonScalarVertex[{muA, nuA}, -q1, -q2, scalarMass];
smokeResidual = If[ListQ[smokeFC] && Length[smokeFC] === 1,
  norm[smokeFC[[1]] - smokeExpected], $Failed];
require["B-24", "native TSS contact converts and matches the source vertex exactly",
  smokeResidual === 0 || TrueQ[Cancel[Together[smokeResidual]] === 0]];

(* ------------------------------------------------------------------ *)
(* Summary                                                              *)
(* ------------------------------------------------------------------ *)

allPassed = And @@ Lookup[checks, "Passed"];
Export[resultPath, <|
  "Completed" -> True, "Target" -> buildTarget,
  "TargetSource" -> targetInfo["Source"],
  "GenericSHA256" -> manifest["ModelFileSHA256"]["Generic"],
  "ClassesSHA256" -> manifest["ModelFileSHA256"]["Classes"],
  "SnapshotSHA256" -> snapshotHash,
  "ScalarMassSymbol" -> "FeynGrav`ScalarMass",
  "TermCounts" -> AssociationThread[families, Lookup[basisList, "TermCount"]],
  "SmokeResidual" -> ToString[smokeResidual, InputForm, PageWidth -> Infinity],
  "Environment" -> buildEnvironment,
  "Checks" -> checks|>, "RawJSON"];

Print["SUMMARY\tpassed=", Length[checks], "\tfailed=",
  Count[checks, _?(#["Passed"] === False &)]];
If[! TrueQ[allPassed],
  Print["DONE\tFAILED"]; Exit[1]];
Print["DONE\tPASSED\t", buildTarget];
Exit[0];
