(* Private matched-reference switch; batching changes only source-local metadata. *)
$pgBatchMetadata = True;
(* Private reference switch for late batches and empty dependency closures. *)
$pgRegistrationMetadata = True;
(* Matched-reference switch for dependency-ready rational metadata only. *)
$rcBatchRationalMetadata = True;

(* ::Package:: *)

(* ::Title:: *)
(* Bounded rational simplification of propagator coefficients in FORM *)

(* ::Text:: *)
(* Loaded in the converter's private context. This module only describes
   algebra to be performed by FORM. It does not simplify expressions in the
   Wolfram kernel. The kinematic snapshot records supplied exact assignments. Existing identifiers retain their
   meaning, including propagators, which never enter the rational algebra. *)

(* ::Section:: *)
(* Conservative eligibility and structural translation *)

(* These are work-selection limits, not time/memory guarantees. Runtime
   cancellation and TimeConstraint still apply to the complete FORM process.
   A rejected job/coefficient may use dimension-only rational arithmetic, then
   falls back to common-factor extraction and bracketing. *)
$rcMaximumTerms = 1000;
$rcMaximumVariables = 12;

(* Shared coefficient policy. The exporter has already validated dimensions,
   exactness and matrix spaces and expanded supported linear momentum routing.
   This policy reads encoded data only: it never decodes/evaluates user symbols.
   Runtime FORM inspection remains authoritative for complete coefficients and
   shared definitions. Mapping size is only the conservative propagator guard. *)
coefficientRoles[entries_List, pairPrefix_String] := Module[
    {scalars, vectors, pairs, pairNames, polarisation, polarisationNames},
    scalars = Association[Cases[entries, e_ /; e["Kind"] === "Scalar" :>
        (e["Expression"][[2]] -> e["Name"])]];
    vectors = Lookup[Select[entries, #["Kind"] === "Vector" &], "Name", {}];
    pairs = Flatten[Table[{vectors[[i]], vectors[[j]]},
        {i, Length[vectors]}, {j, i, Length[vectors]}], 1];
    pairNames = Table[pairPrefix <> intString[j], {j, Length[pairs]}];
    polarisation = Lookup[Select[entries, #["Kind"] === "Vector" &&
        MatchQ[#["Expression"], {"Polarization", ___}] &], "Name", {}];
    polarisationNames = Pick[pairNames,
        AnyTrue[#, MemberQ[polarisation, #] &] & /@ pairs];
    <|"Scalars" -> scalars, "Pairs" -> pairs, "PairNames" -> pairNames,
      "PolarisationNames" -> polarisationNames,
      "Variables" -> Join[Values[scalars], pairNames]|>
];

(* Freeze only explicit, exact scalar assignments for external momentum pairs.
   Unassigned products and unsupported RHS objects retain their old path. No
   physical assumptions or recursive Pair substitutions are inferred. The JSON
   record is data; its encoded RHS is validated again before import. *)
$rcCanonicalKinematics = True;
$rcCouplingGroups = True;
rcExactScalarDataQ[{}] := False;
rcExactScalarDataQ[e_List] := TrueQ[Quiet[Check[Catch[Switch[First[e],
    "Integer" | "Rational" | "Complex", Quiet[Check[MatchQ[decode[e], _Integer | _Rational | _Complex], False]],
    "Symbol", Length[e] === 2 && StringQ[e[[2]]],
    "Plus" | "Times", Length[e] > 1 && AllTrue[Rest[e], rcExactScalarDataQ],
    "Power", Length[e] === 3 && rcExactScalarDataQ[e[[2]]] &&
        MatchQ[e[[3]], {"Integer", _String}] && Between[integerData[e[[3,2]]], {-32,32}],
    _, False], $failureTag, (False &)], False]]];
rcExactScalarDataQ[_] := False;
rcScalarSymbols[e_] := DeleteDuplicates[Cases[e, {"Symbol", _String}, {0, Infinity}]];
rcScalarText[e_, names_Association] := Switch[First[e],
    "Symbol", Lookup[names, e[[2]]],
    "Integer", e[[2]],
    "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
    "Complex", "(" <> rcScalarText[e[[2]],names] <> "+i_*" <> rcScalarText[e[[3]],names] <> ")",
    "Plus" | "Times", "(" <> StringRiffle[rcScalarText[#,names]& /@ Rest[e], If[First[e]==="Plus","+","*"]] <> ")",
    "Power", "(" <> rcScalarText[e[[2]],names] <> ")^(" <> e[[3,2]] <> ")"];

rcKinematicPlan[initial_List, mapping_Association] := Module[
    {entries=initial, vectors, pairs, records={}, value, encoded, symbols, next, names, rules},
    If[!TrueQ[$rcCanonicalKinematics], Return[<|"Entries"->entries,"Records"->{},"Rules"->""|>]];
    vectors=Select[entries, #["Kind"]==="Vector" && MatchQ[#["Expression"],{"Symbol",_String}] &&
        !MemberQ[mapping["LoopMomenta"],#["Expression"]]&];
    pairs=Flatten[Table[{vectors[[i]],vectors[[j]]},{i,Length[vectors]},{j,i,Length[vectors]}],1];
    Do[
        value=Pair[Momentum[decode[pair[[1,"Expression"]]],decode[mapping["Dimension"]]],
            Momentum[decode[pair[[2,"Expression"]]],decode[mapping["Dimension"]]]];
        If[!FreeQ[value,_Pair] || LeafCount[value]>2048, Continue[]];
        encoded=Catch[encode[value],$failureTag];
        If[FailureQ[encoded] || !rcExactScalarDataQ[encoded], Continue[]];
        AppendTo[records,<|"Vectors"->Lookup[pair,"Name"],"Value"->encoded|>],{pair,pairs}];
    symbols=rcScalarSymbols[Lookup[records,"Value",{}]];
    next=1+Max[Prepend[FromDigits[StringDrop[#["Name"],3]]& /@ Select[entries,#["Kind"]==="Scalar"&],0]];
    Do[If[!AnyTrue[entries,#["Kind"]==="Scalar" && #["Expression"]===symbol&],
        AppendTo[entries,<|"Name"->"cfs"<>intString[next++],"Kind"->"Scalar","Expression"->symbol|>]],{symbol,symbols}];
    names=Association[(#["Expression"][[2]]->#["Name"])& /@ Select[entries,#["Kind"]==="Scalar"&]];
    rules=StringJoin[("id "<>StringRiffle[#["Vectors"],"."]<>"="<>rcScalarText[#["Value"],names]<>";\n")& /@ records];
    <|"Entries"->entries,"Records"->records,"Rules"->rules|>
];

rcValidateKinematics[mapping_, dim_] := Module[{records, vectors, scalars, seen={}, pair, current},
    If[!KeyExistsQ[mapping,"CoefficientKinematics"], Return[Null]];
    records=mapping["CoefficientKinematics"];
    If[!AssociationQ[records] || Sort[Keys[records]] =!= {"Identities","Version"} || records["Version"] =!= 1 ||
        !ListQ[records["Identities"]], throwFailure["InvalidMapping","Invalid coefficient kinematic snapshot."]];
    vectors=Association[(#["Name"]->#["Expression"])& /@ Select[mapping["Entries"],#["Kind"]==="Vector"&]];
    scalars=Lookup[Select[mapping["Entries"],#["Kind"]==="Scalar"&],"Expression",{}];
    Do[
        If[!AssociationQ[row] || Sort[Keys[row]] =!= {"Value","Vectors"} ||
            !MatchQ[row["Vectors"],{_String,_String}] || !AllTrue[row["Vectors"],KeyExistsQ[vectors,#]&] ||
            !rcExactScalarDataQ[row["Value"]] || !AllTrue[rcScalarSymbols[row["Value"]],MemberQ[scalars,#]&], throwFailure["InvalidMapping","Invalid frozen scalar-product identity."]];
        pair=Lookup[vectors,row["Vectors"]];
        If[!AllTrue[pair,MatchQ[#, {"Symbol",_String}]&] || AnyTrue[pair,MemberQ[mapping["LoopMomenta"],#]&] ||
            MemberQ[seen,Sort[row["Vectors"]]], throwFailure["InvalidMapping","Invalid or duplicate external scalar product."]];
        AppendTo[seen,Sort[row["Vectors"]]];
        current=encode[Pair[Momentum[decode[pair[[1]]],dim],Momentum[decode[pair[[2]]],dim]]];
        If[current =!= row["Value"], throwFailure["CoefficientConventionMismatch",
            "Scalar-product definitions changed since export; re-export with the current kinematics."]],{row,records["Identities"]}]
];

(* Spectators cannot occur in dimensions, mass/invariant data or nonmonomial
   denominators. Classification uses encoded structure, never parameter names.
   Polarisation jobs retain their existing structural partition and budgets. *)
rcSpectators[entries_, mapping_, rows_, identities_] := Module[{excluded, unsafe},
    If[!TrueQ[$rcCouplingGroups] || AnyTrue[entries,#["Kind"]==="Vector" && First[#["Expression"]]==="Polarization"&],Return[{}]];
    unsafe=Cases[Lookup[entries,"Expression"], x:{"Power",base_,power_} /;
        !(MatchQ[power,{"Integer",_String}] && (integerData[power[[2]]]>=0 || MatchQ[base,{"Symbol",_String}])) :> x,{0,Infinity}];
    excluded=rcScalarSymbols[{mapping["Dimension"],Lookup[rows,"MassSquared",{}],
        lrInvariantSnapshot[rows,decode[mapping["Dimension"]]],Lookup[identities,"Value",{}],unsafe,
        Lookup[Select[entries,MemberQ[{"IntegralCoefficient","Denominator"},#["Kind"]]&],"Expression",{}]}];
    Lookup[Select[entries,#["Kind"]==="Scalar" && !MemberQ[excluded,#["Expression"]]&],"Name",{}]
];

(* One source for stage allowances. The private legacy controls remain usable
   by focused tests. These profiles deliberately retain their different domains:
   propagator grouping cannot use the final master's shared-definition inspector.
   Limits select optional work; they neither cap memory nor catch runtime errors. *)
(* One job-wide envelope for optional multivariate coefficient work. Legacy
   per-path controls remain local gates; lowering either gate can only skip work.
   The private switch supplies matched references, never a public escape hatch. *)
$rcCoordinateWork = True;
$rcMaximumRationalPasses = 1024;
$rcMaximumFactorisations = 2048;
coefficientLimits["Job"] := <|"RationalPasses" -> Max[0, $rcMaximumRationalPasses],
    "Factorisations" -> Max[0, $rcMaximumFactorisations]|>;
coefficientLimits["Propagator"] := <|"Terms" -> $rcMaximumTerms,
    "Variables" -> $rcMaximumVariables|>;
coefficientLimits["Master"] := <|"Terms" -> $rcMasterMaximumTerms,
    "Variables" -> $rcMaximumVariables, "Degree" -> $rcMasterMaximumDegree|>;
(* Complete coupling groups use their named representation for admission. The
   legacy expansion estimate remains authoritative everywhere else. *)
$rcCouplingGroupMaximumTerms = 1000;
$rcCouplingGroupMaximumDegree = 48;
coefficientLimits["CouplingGroup"] := <|"Terms" -> Max[0, $rcCouplingGroupMaximumTerms],
    "Variables" -> $rcMaximumVariables, "Degree" -> Max[0, $rcCouplingGroupMaximumDegree]|>;
coefficientLimits["Polarisation"] := <|"Structures" -> $rcPolarisationMaximumStructures,
    "Terms" -> $rcPolarisationMaximumTerms,
    "RationalPasses" -> Max[0, $rcPolarisationMaximumRationalPasses]|>;
coefficientLimits["Factor"] := <|"Terms" -> $rcPolarisationFactorMaximumTerms,
    "Variables" -> $rcPolarisationFactorMaximumVariables,
    "Degree" -> $rcPolarisationFactorMaximumDegree|>;
(* Bound retained collector results independently of the arithmetic allowance. *)
$rcCollectionMaximumCacheEntries = 64;
coefficientLimits["Collection"] := <|"CacheEntries" -> Max[0, $rcCollectionMaximumCacheEntries]|>;
coefficientLimits["Dimension"] := <|"PreconditionTerms" -> $dcfPreconditionMinimumTerms|>;
coefficientLimits["Polynomial"] := <|"Power" -> {0,32}, "InversePower" -> {-8,-1}|>;

(* Compile only the existing rational polynomial domain. A supplied scalar
   dictionary defines that domain: all commuting parameters for the general
   path, just the selected dimension for the univariate path. Unknown symbols,
   Pair components, radicals, approximate/complex leaves and opaque functions
   are ineligible here. Exact complex arithmetic remains with the final FORM
   inspector/imaginary-unit restoration; do not turn it into a real variable. *)
coefficientPolynomial[e_, scalars_Association, tag_] := Switch[First[e],
    "Symbol", Lookup[scalars, e[[2]], Throw[$Failed, tag]],
    "Integer", e[[2]],
    "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
    "Plus" | "Times", "(" <> StringRiffle[
        coefficientPolynomial[#, scalars, tag] & /@ Rest[e],
        If[First[e] === "Plus", "+", "*"]] <> ")",
    "Power", With[{n = coefficientInteger[e[[3]], tag]},
        If[!Between[n, coefficientLimits["Polynomial"]["Power"]], Throw[$Failed, tag]];
        "(" <> coefficientPolynomial[e[[2]], scalars, tag] <> ")^" <> intString[n]],
    _, Throw[$Failed, tag]
];
coefficientInteger[{"Integer", s_String}, tag_] := integerData[s];
coefficientInteger[_, tag_] := Throw[$Failed, tag];

(* No successful or failed translation is cached across jobs. Each abbreviation
   is visited once per domain. $Failed means predictable ineligibility only;
   FORM errors/aborts are never handled by this selector. *)
coefficientInverseRules[entries_List, scalars_Association, head_String] :=
    Map[Function[e, Module[{x = e["Expression"], tag = Unique["coefficient"], n},
        Catch[
            If[First[x] =!= "Power", Throw[$Failed, tag]];
            n = coefficientInteger[x[[3]], tag];
            If[!Between[n, coefficientLimits["Polynomial"]["InversePower"]], Throw[$Failed, tag]];
            {e["Name"], "id " <> e["Name"] <> "=" <> head <> "(1,(" <>
                coefficientPolynomial[x[[2]], scalars, tag] <> ")^" <> intString[-n] <> ");"},
            tag]
    ]], Select[entries, #["Kind"] === "Abbreviation" &]];

rcPlan[data_Association, requirePropagators_: True] := Module[
    {roles, pairs, names, variables, inverses, rules, limits = coefficientLimits["Propagator"]},
    If[data["Mapping"]["Version"] =!= 1 ||
        (TrueQ[requirePropagators] && !pgFactorisationQ[data]) || limits["Terms"] <= 0, Return[<||>]];
    roles = coefficientRoles[data["Mapping"]["Entries"], "cfcRX"];
    pairs = roles["Pairs"]; names = roles["PairNames"]; variables = roles["Variables"];
    If[Length[variables] > limits["Variables"], Return[<||>]];
    inverses = coefficientInverseRules[data["Mapping"]["Entries"], roles["Scalars"], "cfcRat"];
    (* All abbreviations must belong to this domain. The dimension-only pass
       instead leaves unrecognised definitions opaque, as before. *)
    If[MemberQ[inverses, $Failed], Return[<||>]];
    rules = Join[If[inverses === {}, {}, inverses[[All,2]]],
        MapThread["id " <> StringRiffle[#1, "."] <> "=" <> #2 <> ";" &, {pairs, names}],
        rcPowerRules[variables, "cfcRat"], {"Multiply cfcRat(1,1);"}];
    <|"Declarations" -> "\nSymbols " <> StringRiffle[
            Join[{"cfcRPower", "cfcRNum", "cfcRDen"}, names], ","] <>
            ";\nCFunction cfcRat;\n",
      "Rules" -> StringRiffle[rules, "\n"],
      "Restore" -> StringRiffle[MapThread[
          "id " <> #1 <> "=" <> StringRiffle[#2, "."] <> ";" &, {names, pairs}], "\n"]|>
];

(* ::Section:: *)
(* Embedded native FORM procedures *)

rcTemplate[name_String] := Module[{text},
    text = Quiet[Check[Import[FileNameJoin[{$moduleDirectory, "Templates", name}], "Text"], $Failed]];
    If[!StringQ[text], throwFailure["MissingTemplate", "Cannot read the rational coefficient template."]];
    text
];
rcWorkBudget[] := With[{limits = coefficientLimits["Job"]},
    StringReplace[rcTemplate["CoefficientWorkLimits.frm.in"], {
        "@ENABLED@" -> If[TrueQ[$rcCoordinateWork], "1", "0"],
        "@RATIONAL@" -> intString[limits["RationalPasses"]],
        "@FACTORS@" -> intString[limits["Factorisations"]]}] <> "\n"];

rcReduction[plan_Association] := If[plan === <||>, "",
    StringReplace[rcTemplate["RationalCoefficients.frm.in"], {
        "@LIMIT@" -> intString[coefficientLimits["Propagator"]["Terms"]], "@RULES@" -> plan["Rules"]}]];
rcWrite[plan_Association, path_String, fallback_String] := If[plan === <||>, fallback,
    "#if `$cfcRDone`cfcSlot'' == 1\n" <>
    StringReplace[rcTemplate["RationalCoefficientOutput.frm.in"], {
        "@RESULT@" -> path, "@RESTORE@" -> plan["Restore"]}] <>
    "\n#else\n" <> fallback <> "\n#endif\n"];

(* The two callers share native rational arithmetic, but select work differently.
   Propagator coefficients keep their historical guards. Final master coefficients
   inspect the actual shared definitions in FORM before exposing their algebra. *)
rcPowerRules[variables_List, head_String] := Flatten[Map[Function[s, {
    "id " <> s <> "^cfcRPower?pos_=" <> head <> "(" <> s <> "^cfcRPower,1);",
    "id " <> s <> "^cfcRPower?neg_=" <> head <> "(1," <> s <> "^(-cfcRPower));"}], variables]];

$rcMasterMaximumTerms = 1000;
$rcMasterMaximumDegree = 24;
rcMasterPlan[entries_List, spectators_List: {}] := Module[{roles, variables, pairs, names, powers, inspect, polNames, polPlan, batchMetadata,
    limits = coefficientLimits["Master"], couplingLimits = coefficientLimits["CouplingGroup"]},
    If[limits["Terms"] <= 0, Return[<||>]];
    roles = coefficientRoles[entries, "cfcMRX"];
    pairs = roles["Pairs"]; names = roles["PairNames"];
    polNames = roles["PolarisationNames"]; variables = roles["Variables"];
    polPlan = rcPolarisationPlan[pairs, names, polNames, variables, spectators];
    batchMetadata = polPlan =!= <||> && TrueQ[$pgBatchMetadata] && TrueQ[$rcBatchRationalMetadata];
    powers = StringRiffle[rcPowerRules[Append[variables,"cfcMRImag"],"cfcMRat"],"\n"];
    inspect = StringJoin[Map[Function[s,
        "id " <> s <> "^cfcRPower?pos_=" <> s <> "*cfcMRDegree^cfcRPower;\n" <>
        "id " <> s <> "^cfcRPower?neg_=" <> s <> "*cfcMRDegree^(-cfcRPower);\n"], variables]];
    <|"Declarations" -> "\nSymbols cfcRPower,cfcMRIndex,cfcMRN,cfcMRD,cfcMRDegree,cfcMRImag" <>
            If[names === {},"",","<>StringRiffle[names,","]] <> ";\nCFunctions cfcMRat,cfcMRDenProbe,cfcCGPart;\nTable sparse cfcCGExpanded(1);\nTable sparse cfcCGExpandedReady(1);\nTable sparse cfcMRMeta(1);\n" <> Lookup[polPlan, "Declarations", ""],
      "Polarisation" -> polPlan, "BatchMetadata" -> batchMetadata, "RegisterDependencies" -> (spectators =!= {}),
      "Procedure" -> StringReplace[rcTemplate["MasterRationalCoefficients.frm.in"] <>
        If[batchMetadata, rcTemplate["RationalMetadataBatch.frm.in"], ""], {
        "@LIMIT@" -> intString[limits["Terms"]], "@DEGREE@" -> intString[limits["Degree"]],
        "@CG_TERMS@" -> intString[couplingLimits["Terms"]], "@CG_DEGREE@" -> intString[couplingLimits["Degree"]],
        "@VARIABLE_LIMIT@" -> intString[limits["Variables"]], "@VARIABLES@" -> StringRiffle[variables,","],
        "@DOTS@" -> StringRiffle[MapThread["id "<>StringRiffle[#1,"."]<>"^cfcRPower?="<>#2<>"^cfcRPower;"&,{pairs,names}],"\n"],
        "@NATIVE_DEN_INSPECT@" -> ("id denom_(?a)=cfcMRDenProbe(?a);\nArgument cfcMRDenProbe;\nDropCoefficient;\nid i_=1;\n" <>
            StringRiffle[MapThread["id "<>StringRiffle[#1,"."]<>"^cfcRPower?="<>#2<>"^cfcRPower;"&,{pairs,names}],"\n"] <>
            "\n" <> inspect <> "EndArgument;\nid cfcMRDenProbe(cfcMRN?)=cfcMRN;\n"),
        "@NATIVE_DEN_RATIONAL@" -> "id denom_(?a)=cfcMRat(1,?a);\n",
        "@INSPECT@" -> inspect, "@POWERS@" -> powers,
        "@RESTORE@" -> StringRiffle[MapThread["id "<>#1<>"="<>StringRiffle[#2,"."]<>";"&,{names,pairs}],"\n"],
        "@FACTOR@" -> If[polPlan === <||>, "", "#call cfcPGFactor\n"]}]|>
];


(* Polarisation roles come from the validated dictionary, never identifier names.
   No grouping data is persisted in mappings: all tables live in one FORM job. *)
$rcPolarisationMaximumStructures = 1024;
$rcPolarisationMaximumTerms = 10000;
(* Total speculative rational passes across all polarisation groups in one job.
   Exhaustion restores the complete original master; it is not a runtime limit. *)
$rcPolarisationMaximumRationalPasses = 1024;
$rcPolarisationFactorMaximumTerms = 250;
$rcPolarisationFactorMaximumVariables = 8;
$rcPolarisationFactorMaximumDegree = 12;
rcPolarisationPlan[pairs_List, names_List, polNames_List, variables_List, spectators_List: {}] := Module[
    {polPairs, plain, structures=Join[polNames,spectators], substitutions, limits = coefficientLimits["Polarisation"],
     factorLimits = coefficientLimits["Factor"]},
    If[structures === {} || limits["Structures"] <= 0, Return[<||>]];
    polPairs = Pick[pairs, MemberQ[polNames, #] & /@ names];
    plain = Complement[variables, structures];
    substitutions = {
        "@POL@" -> StringRiffle[structures, ","],
        "@REGISTRATION_METADATA@" -> If[TrueQ[$pgBatchMetadata] && TrueQ[$pgRegistrationMetadata], "1", "0"],
        "@STRUCTURE_INSPECT@" -> StringJoin[("id "<>#<>"^cfcRPower?neg_="<>If[MemberQ[spectators,#],"cfcPGHas","cfcPGBad"]<>";\nid "<>#<>"^cfcRPower?pos_=cfcPGHas;\n")& /@ structures],
        "@STRUCTURES@" -> intString[limits["Structures"]],
        "@WORK_LIMIT@" -> intString[limits["RationalPasses"]],
        "@TERMS@" -> intString[limits["Terms"]],
        "@FACTOR_TERMS@" -> intString[factorLimits["Terms"]],
        "@FACTOR_VARIABLES@" -> intString[factorLimits["Variables"]],
        "@FACTOR_DEGREE@" -> intString[factorLimits["Degree"]],
        "@POL_DOTS@" -> StringRiffle[MapThread[
            "id " <> StringRiffle[#1, "."] <> "^cfcRPower?=" <> #2 <> "^cfcRPower;" &,
            {polPairs, polNames}], "\n"],
        "@ALL_DOTS@" -> StringRiffle[MapThread[
            "id " <> StringRiffle[#1, "."] <> "^cfcRPower?=" <> #2 <> "^cfcRPower;" &,
            {pairs, names}], "\n"],
        "@POL_RESTORE@" -> StringRiffle[MapThread[
            "id " <> #1 <> "=" <> StringRiffle[#2, "."] <> ";" &,
            {polNames, polPairs}], "\n"],
        "@PLAIN@" -> StringRiffle[plain, ","],
        "@DENOMINATOR_KEYS@" -> StringJoin[("id " <> # <> "^cfcRPower?neg_=cfcPGInverse(" <> # <> ")^(-cfcRPower);\n" &) /@
            Join[Complement[variables, Join[names,spectators]], ("(" <> StringRiffle[#, "."] <> ")" &) /@
                Pick[pairs, !MemberQ[polNames, #] & /@ names]]],
        "@ATOM_COUNT@" -> StringRiffle[("theta_(abs_(count_(" <> # <> ",1))-1)+theta_(abs_(count_(" <> # <> ",1))-2)" &) /@
            variables, "+"] <> "+count_(cfcShare,1,cfcPolyN,1,cfcPolyD,1)",
        "@COMPLETE_GROUP_RETRY@" -> If[spectators === {}, "",
            "#if (`$cfcMROK' == 0) && (`$cfcPGWorkExhausted' == 0)\n" <>
            "#define cfcCGComplete \"1\"\n#call cfcMRFinal(cfcPGCoefficient)\n#undefine cfcCGComplete\n" <>
            "#write \"CFC_CG_DIRECT ok=%$, cost %$, degree %$, variables %$\",$cfcMROK,$cfcMRCost,$cfcMRMax,$cfcMRCount\n#endif\n"]};
    <|"Declarations" -> "\nSymbols cfcPGHas,cfcPGFraction,cfcPGBad,cfcPGIndex;\n" <>
        "CFunctions cfcPGTag,cfcPGRef,cfcPGCollect,cfcPGCollectPart,cfcPGInverse,cfcPGDenProbe,cfcPGReject,cfcPGRejectPart;\n" <>
        "Table sparse cfcPGSize(1);\nTable sparse cfcPGDenOK(1);\nTable sparse cfcPGParts(1);\nTable sparse cfcPGReach(1);\nTable sparse cfcPGTerms(1);\nTable sparse cfcMBTerms(1);\nTable sparse cfcMBAtoms(1);\nTable sparse cfcMBLinks(1);\nTable sparse cfcPGAtoms(1);\nTable sparse cfcPGRejectCost(1);\nTable sparse cfcPGRejectDegree(1);\nTable sparse cfcPGRejectVariables(1);\nTable sparse cfcPGResult(1);\nTable sparse cfcPGResultReady(1);\nTable sparse cfcPGInspectOK(1);\nTable sparse cfcPGInspectMeta(1);\nTable sparse cfcPGAlgebra(2);\nTable sparse cfcPGAlgebraReady(2);\nTable sparse cfcPGDenValue(1);\nTable sparse cfcPGDenReady(1);\n" <>
        "Table sparse cfcCGInspectOK(1);\nTable sparse cfcCGInspectMeta(1);\nTable sparse cfcCGRejectCost(1);\nTable sparse cfcCGRejectDegree(1);\nTable sparse cfcCGRejectVariables(1);\n",
      "Procedure" -> StringReplace[StringReplace[rcTemplate["PolarisationCoefficients.frm.in"], substitutions],
        If[spectators === {}, {}, {"CFC polarisation groups:" -> "CFC coupling groups:", "CFC_PG_" -> "CFC_CG_", "CFC polarisation rational eligible:" -> "CFC coupling rational eligible:", "unsupported_or_no_polarisation" -> "unsupported_or_no_coupling"}]]|>
];
