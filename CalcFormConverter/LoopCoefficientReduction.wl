(* ::Package:: *)
(* ::Title:: *)
(* Reduction of repeated-power one-loop coefficients *)

(* ::Text:: *)
(* Loaded in the private converter context. Wolfram code prepares small scalar
   Cayley matrices and validated mapping entries; the generated standalone FORM
   procedure performs partial fractions and the terminating power recurrence.
   No TID, PaVeReduce, Simplify or numerical integral evaluation is called.
   Singular subfamilies are retained. The power-only option is opt-in; enabled
   scalar-master reduction also uses this planner. Unlike the Gram-free tensor
   decomposition, these reductions may introduce kinematic denominators. *)

(* ::Section:: *)
(* Saved coefficients and mapping contracts *)
(* Reduction coefficients are planned at export. Unlike plain decomposition,
   their scalar-product assumptions must not change before import. *)
lrSquare[x_, dim_] := If[!FreeQ[x,_smChord],smSquare[x],If[x === 0, 0,
    ExpandScalarProduct[Pair[Momentum[x, dim], Momentum[x, dim]]]]];
lrInvariantSnapshot[rows_, dim_] := With[{chords = decode[#["Chord"]] & /@ rows},
    (With[{v=lrSquare[chords[[#[[1]]]] - chords[[#[[2]]]], dim]},If[MissingQ[v],{"UnrelatedFamilies"},encode[v]]] &) /@
        Subsets[Range[Length[chords]], {2}]];

lrSavedEntries[entries_] := Select[entries, #["Kind"] === "IntegralCoefficient" &&
    With[{x = smCoefficient[decode[#["Expression"]]]}, smSavedEligible[decode[#["Expression"]]] && (TrueQ[$smEnabled] || Head[decode[#["Expression"]]]=!=PaVe) && Head[x] === CalcFormConverter`LoopTensorCoefficient &&
        Head[x[[5]]] === Symbol && x[[5]] === D] &];

lrSavedRows[entries_] := Module[{pairs},
    pairs = DeleteDuplicates[Flatten[Map[Function[e, With[{x = smCoefficient[decode[e["Expression"]]]},
        Transpose[{x[[2]], x[[3]]}]]], lrSavedEntries[entries]], 1]];
    (* All zero chords precede nonzero chords, so the selected anchor remains
       zero for every saved coefficient, including equal routes with different masses. *)
    pairs = Join[Select[pairs, First[#] === 0 &], Select[pairs, First[#] =!= 0 &]];
    (<|"Chord" -> encode[#[[1]]], "MassSquared" -> encode[#[[2]]]|> &) /@ pairs
];

(* Saved integral factors retain separate families even in products. Their
   coefficients are reduced independently, never interpreted as one integrand. *)
lrSavedSupports[entries_, rows_] := Module[{pairs},
    pairs = ({decode[#["Chord"]], decode[#["MassSquared"]]} &) /@ rows;
    Union[Function[e, With[{x = smCoefficient[decode[e["Expression"]]]},
        Union[(First[FirstPosition[pairs, #]] &) /@ Transpose[{x[[2]], x[[3]]}]]]]
        /@ lrSavedEntries[entries]]
];

(* Cancellation and the existing recurrences only remove denominator slots or
   change their powers. The downward closure therefore contains every possible
   descendant, including same-route partial fractions. Automatic preserves the
   conservative dictionary-wide planner for callers lacking support metadata. *)
lrReachableFamilies[supports_List, maximum_] := SortBy[
    Union[Flatten[Subsets[#, {1, Min[Length[#], maximum]}] & /@ supports, 1]],
    {Length, Identity}];
lrReductionFamilies[rows_, pairs_, maximum_, supports_] := Module[{groups, families},
    If[ListQ[supports], families = Select[lrReachableFamilies[supports, maximum],
            DuplicateFreeQ[pairs[[#, 1]]] &],
        groups = GatherBy[Range[Length[rows]], pairs[[#, 1]] &];
        families = Flatten[Table[Flatten[(Tuples[groups[[#]]] &) /@
            Subsets[Range[Length[groups]], {k}], 1],
            {k, 1, Min[Length[groups], maximum]}], 1]];
    Select[families, smRowsCompatible[rows, #] &]
];

lrSavedProgram[entries_, rows_, drop_] := Module[{pairs, code},
    pairs = ({decode[#["Chord"]], decode[#["MassSquared"]]} &) /@ rows;
    code = Map[Function[e, Module[{x = smCoefficient[decode[e["Expression"]]], slots, powers, labels, effectivePowers, factor = 1, counts},
        If[x[[2]] === {}, "id " <> e["Name"] <> "=" <> If[drop,"0",
            "cfcTC(-1," <> intString[If[x[[1]] === {0},0,Length[x[[1]]]]] <> ")"] <> ";\n",
        slots = (First[FirstPosition[pairs, #]] &) /@ Transpose[{x[[2]], x[[3]]}];
        effectivePowers = x[[4]];
        labels = If[x[[1]] === {0}, {}, x[[1]]];
        If[!DuplicateFreeQ[slots],
            (* Coalescing equal denominators must not coalesce distinct
               parameter moments. Convert labelled moments before merging
               their powers, retaining each original Pochhammer factor. *)
            counts = Table[Count[labels,j-1],{j,2,Length[slots]}];
            counts = Prepend[counts,0];
            factor = (-2)^Total[counts] Times @@ MapThread[Pochhammer,{effectivePowers,counts}];
            effectivePowers += counts;
            labels = ConstantArray[0,Count[labels,0]+2Total[counts]],
            labels = labels /. Thread[Range[Length[slots]-1] -> Rest[slots]]];
        powers = ConstantArray[0, Length[rows]];
        Do[powers[[slots[[j]]]] += effectivePowers[[j]], {j, Length[slots]}];
        "id " <> e["Name"] <> "=" <> intString[factor] <> "*cfcTC(" <>
            StringRiffle[intString /@ Join[powers, {-2}, Sort[labels]], ","] <> ");\n"]
    ]], lrSavedEntries[entries]];
    "\n* Reduce existing coefficients; do not integrate their external factors.\n" <> StringJoin[code]
];

(* ::Section:: *)
(* Reduce distinct coefficients without carrying amplitude prefactors *)
(* Keep Brackets visits each complete coefficient key once. The original
   amplitude stays hidden while the recurrence acts on a small lookup expression.
   Its indexed brackets associate final coefficient arguments with their
   original amplitude keys. Reassembly combines a final integral's complete
   multiplier, retaining shared blocks for large commuting expressions.
   Products of integral coefficients retain the general recurrence: a product
   is not a single integral, and its factors must not share a lookup key. *)
$lrSharedMinimumTerms = 4096;
(* Scalar-master recurrences can turn a small tensor-coefficient dictionary
   into large rational multipliers. Use the existing shared/factored assembly
   for every nonzero eligible scalar-master job: its polynomial dictionary is
   populated after reduction, when the large factors actually exist. The input
   term threshold remains appropriate for the repeated-power-only path.
   Algebra eligibility and the linear-coefficient guard are unchanged. *)
lrSharedPreparation[plan_] := If[!TrueQ[Lookup[plan,"SharedBlocks",False]], "", StringJoin[
    "#if termsin(cfcResult) > ",intString[If[TrueQ[$smEnabled],0,$lrSharedMinimumTerms]],"\n",
    "Collect cfcShared,cfcSharedPart,50;\nArgToExtraSymbol tonumber,cfcShared,cfcSharedPart;\n.sort\n",
    "id cfcShared(cfcDimNum?)=cfcShare(cfcDimNum);\nid cfcSharedPart(cfcDimNum?)=cfcShare(cfcDimNum);\n",
    "Bracket+ cfcTC;\n.sort\nHide cfcResult;\nLocal cfcShareKeys=cfcResult;\nBracket cfcShare;\n.sort\n",
    "#$cfcShareKeys=0;\nKeep Brackets;\n$cfcShareKeys=$cfcShareKeys+term_;\nModuleOption noparallel;\n.sort\n",
    "Drop cfcShareKeys;\n.sort\nLocal cfcShareKeys=$cfcShareKeys;\n.sort\nHide cfcShareKeys;\nUnhide cfcResult;\nBracket+ cfcTC;\n.sort\n",
    "#define cfcSharedActive \"1\"\n#endif\n"]];
(* Direct output still declares an empty dictionary when the mapping permits
   sharing but the runtime threshold selected no shared amplitude blocks. *)
lrDirectPreamble[plan_, path_] := If[!TrueQ[Lookup[plan, "SharedBlocks", False]], "",
    "#write <" <> path <> "> \"CFC_SHARED 0\"\n" <>
    "#write <" <> path <> "> \"CFC_BODY\"\n"];

(* Collect complete external multipliers before key enumeration/reduction and
   on the direct linear-output path afterwards. Existing shared output owns its
   post-reduction processing. Tensor decomposition has already removed the loop
   momentum from these coefficients; no integrand factor is pulled out here.
   A private switch supplies matched regression references, not a public option. *)
$lrCollectCoefficients = True;
(* Matched-reference switch; reuse is confined to one immutable collection plan. *)
$lrReuseCoefficients = True;
lrCoefficientCollectionPlan[mapping_Association] := Module[{plan, indices, guard, monomials, filtered},
    If[!TrueQ[$lrCollectCoefficients], Return[<||>]];
    monomials = lrMonomialDefinitions[mapping["Entries"]];
    (* Generated recurrence weights include Laurent monomials, not just inverse
       polynomials. The established monomial reader exposes them without sums;
       only the remaining abbreviations need the stricter rational reader. *)
    filtered = Join[mapping, <|"Entries" -> Select[mapping["Entries"],
        !MemberQ[First /@ monomials, #["Name"]] &]|>];
    plan = rcPlan[<|"Mapping" -> tdAlgebraMapping[filtered]|>, False];
    If[plan === <||>, Return[<||>]];
    indices = Lookup[Select[mapping["Entries"], #["Kind"] === "Index" &], "Name", {}];
    guard = "occurs(i_" <> If[indices === {}, "", "," <> StringRiffle[indices, ","]] <>
        ") || match(cfcDimRat(?a)) || match(cfcLRNum(?a)) || match(cfcLRDen(?a))" <>
        " || match(cfA0(?a)) || match(cfB0(?a)) || match(cfC0(?a)) || match(cfD0(?a))" <>
        If[Length[indices] < 2, "", " || match(d_(" <> indices[[1]] <> "?," <> indices[[2]] <> "?))"];
    <|"Declarations" -> plan["Declarations"] <> StringReplace[
        rcTemplate["CoefficientCollection.frm.in"], {
            "@LIMIT@" -> intString[coefficientLimits["Propagator"]["Terms"]],
            "@WORK@" -> intString[coefficientLimits["Polarisation"]["RationalPasses"]],
            "@REUSE@" -> If[TrueQ[$lrReuseCoefficients], "1", "0"],
            "@CACHE@" -> intString[coefficientLimits["Collection"]["CacheEntries"]],
            "@UNSAFE@" -> guard, "@MONOMIALS@" -> lrMonomialRules[mapping["Entries"]],
            "@RULES@" -> plan["Rules"], "@RESTORE@" -> plan["Restore"]}],
      "Before" -> "#call cfcCollectBeforeReduction\n",
      "After" -> "#call cfcCollectScalar(cfcLRPiece)\n"|>
];

lrIsolatedProcessing[] := StringJoin[
    "\n* Isolate the coefficient dictionary before running the recurrence.\n",
    ".sort\nBracket cfcTC;\n.sort\n",
    "#$cfcLRKeys=0;\n#$cfcLRNonlinear=0;\nKeep Brackets;\n",
    "$cfcLRKeys=$cfcLRKeys+term_;\n",
    "if (count(cfcTC,1)>1); $cfcLRNonlinear=1; endif;\n",
    "Bracket+ cfcTC;\nModuleOption noparallel;\n.sort\n",
    "#if `$cfcLRNonlinear' == 0\n",
    Lookup[Lookup[$lrCollectionPlan,"CoefficientCollection",<||>],"Before",""],
    lrSharedPreparation[$lrCollectionPlan],
    "Hide cfcResult;\n.sort\nLocal cfcLRWork=$cfcLRKeys;\n.sort\n",
    If[TrueQ[$smEnabled], "Skip cfcLRWork;\nLocal cfcMSKeys=cfcLRWork;\nif (count(cfcTC,1)==0); Discard; endif;\n.sort\n#$cfcMSRequested=termsin_(cfcMSKeys);\n#write \"CFC_MASTER_REQUESTED %$\",$cfcMSRequested\nDrop cfcMSKeys;\n.sort\nNSkip;\n", ""],
    "if (count(cfcTC,1)==0); Multiply cfcLRKey(); endif;\n",
    "id cfcTC(?a)=cfcLRKey(?a)*cfcTC(?a);\n.sort\n#call cfcLRReduce\n",
    Lookup[$lrCollectionPlan,"CanonicalRules",""],
    "Bracket+ cfcTC;\n.sort\nHide cfcLRWork;\n",
    "Local cfcLRFinalKeys=cfcLRWork;\nBracket cfcTC;\n.sort\n",
    "#$cfcLRKeys=0;\nKeep Brackets;\n$cfcLRKeys=$cfcLRKeys+term_;\n",
    "ModuleOption noparallel;\n.sort\nDrop cfcLRFinalKeys;\n.sort\n",
    "Local cfcLRFinalKeys=$cfcLRKeys;\n.sort\nHide cfcLRFinalKeys;\nUnhide cfcResult;\n.sort\n",
    "#else\n#call cfcLRReduce\n#endif\n#$cfcLRKeys=0;\n"
];

(* The lookup is indexed by the final integral, not the old coefficient.
   Assemble one complete multiplier at a time and simplify it in FORM. This
   bounds simultaneously live amplitude expressions without splitting an
   integral's multiplier across independent result groups. *)
lrOutput[data_Association,path_String] := Module[{plan=data["LoopCollectionPlan"],den},
    den=plan["Denominators"];
    StringJoin[
        den["Preparation"],
        If[TrueQ[plan["SharedBlocks"]],"#ifdef `cfcSharedActive'\n"<>ldgSharedOutput[plan,path]<>"\n#else\n", ""],
        lrDirectPreamble[plan,path],
        "#if `$cfcLRNonlinear' == 0\nHide cfcResult;\n.sort\n#write <",path,"> \"0\"\n",
        "#do cfcLRFinalKey=cfcLRFinalKeys\nLocal cfcLRKeyValue=`cfcLRFinalKey';\nLocal cfcLRPiece=cfcLRWork[`cfcLRFinalKey'];\n",
        "id cfcLRKey()=cfcResult[1];\nid cfcLRKey(?a)=cfcResult[cfcTC(?a)];\n",
        plan["MonomialRules"],".sort\n",
        Lookup[Lookup[plan,"CoefficientCollection",<||>],"After",""],
        If[plan["Dimension"]===<||>,"","PolyRatFun cfcDimRat;\nSkip cfcLRKeyValue;\n"<>plan["Dimension"]["Rules"]<>"\n.sort\nPolyRatFun;\nNSkip;\n"],
        "Skip cfcLRKeyValue;\n",den["Rules"],den["Bracket"],".sort\nNSkip;\n",
        "#write <",path,"> \"+(%E)*(\",cfcLRKeyValue\n#write <",path,"> \"%E\",cfcLRPiece\n#write <",path,"> \")\"\n",
        "Drop cfcLRPiece,cfcLRKeyValue;\n.sort\n#enddo\nUnhide cfcResult;\n.sort\n",
        "#else\n",den["Rules"],den["IntegralBracket"],".sort\n#write <",path,"> \"%E\",cfcResult\n#endif\n",
        If[TrueQ[plan["SharedBlocks"]],"#endif\n", ""],
        "#if `$cfcLRNonlinear' == 0\nUnhide cfcLRWork,cfcLRFinalKeys;\nDrop cfcLRWork,cfcLRFinalKeys;\n.sort\n#endif\n"]
];

(* Expand only mapped Laurent monomials: this joins native mass/momentum
   factors to identical ones introduced by reduction, without distributing
   sums or exposing general inverse polynomials. *)
lrMonomialDefinitions[entries_] := Module[{scalars,vectors,read,integer,rules,tag=Unique["lrMonomial"]},
    scalars=Association[(#["Expression"][[2]]->#["Name"]&) /@
        Select[entries,#["Kind"]==="Scalar"&]];
    vectors=Association[(With[{v=#["Expression"]},HoldComplete[v]]->#["Name"]&) /@
        Select[entries,#["Kind"]==="Vector"&]];
    integer[e_] := If[MatchQ[e,{"Integer",_String}],integerData[e[[2]]],Throw[$Failed,tag]];
    read[e_] := Switch[First[e],
        "Integer",e[[2]],"Rational","("<>e[[2]]<>"/"<>e[[3]]<>")",
        "Symbol",Lookup[scalars,e[[2]],Throw[$Failed,tag]],
        "Times","("<>StringRiffle[read /@ Rest[e],"*"]<>")",
        "Power","("<>read[e[[2]]]<>")^"<>intString[integer[e[[3]]]],
        "Pair",If[!MatchQ[Rest[e],{{"Momentum",_,___},{"Momentum",_,___}}],Throw[$Failed,tag]];
            "("<>StringRiffle[(With[{v=#[[2]]},Lookup[vectors,HoldComplete[v],Throw[$Failed,tag]]]&) /@ Rest[e],"."]<>")",
        _,Throw[$Failed,tag]];
    rules=Map[Function[e,With[{body=Catch[read[e["Expression"]],tag]},
        If[body===$Failed,Nothing,{e["Name"],body}]]],
        Select[entries,#["Kind"]==="Abbreviation"&]];
    rules
];
lrMonomialRules[entries_] := StringJoin[("id "<>#[[1]]<>"="<>#[[2]]<>";\n" &) /@
    lrMonomialDefinitions[entries]];

(* ::Section:: *)
(* Canonical final coefficient identities *)
(* A shifted family can reconstruct the same coefficient as another set of
   global denominator slots. Compare ordered masses and chords for formal
   coefficients, and ordered masses and invariants for ordinary PaVe objects.
   These are identity checks, not permutations or PaVe reduction. Labels are
   relabelled only in their argument range; denominator powers stay intact.
   Family enumeration depends on the denominator dictionary, not tensor rank. *)
lrCanonicalRules[rows_, dim_, drop_:False, supports_:Automatic] := Module[
    {pairs, n=Length[rows], families, seen, code={}, signature, active, target,
     powers, moved, labels, chords, masses, invariants, key, emit},
    pairs=({decode[#["Chord"]],decode[#["MassSquared"]]}&) /@ rows;
    If[DuplicateFreeQ[Last /@ pairs] && !(TrueQ[drop] && MemberQ[Last /@ pairs,0]),Return[""]];
    families=Select[If[ListQ[supports],lrReachableFamilies[supports,n],
        Subsets[Range[n],{1,n}]],smRowsCompatible[rows,#]&];
    emit[active_,target_,unit_] := Module[{left,right,relabel},
        left=Table[If[MemberQ[active,j],If[unit,"1","cfcRN"<>intString[j]<>"?pos_"],"0"],{j,n}];
        right=ConstantArray["0",n];
        Do[right[[target[[k]]]]=If[unit,"1","cfcRN"<>intString[active[[k]]]],{k,Length[active]}];
        relabel=Flatten[Transpose[{Rest[active],Rest[target]}]];
        "id cfcTC("<>StringRiffle[left,","]<> ",-2,?a)=cfcLRCanon("<>StringRiffle[right,","]<> ",-2,?a);\n" <>
        If[relabel === {},"","Transform cfcLRCanon replace("<>intString[n+2]<>",last)=("<>StringRiffle[intString /@ relabel,","]<>");\n"] <>
        "id cfcLRCanon(?a)=cfcLRCanonical(?a);\n"
    ];
    If[TrueQ[drop],Do[
        If[AllTrue[pairs[[active,2]],#===0&] &&
            AllTrue[tdInvariants[pairs[[active,1]],dim],#===0&],
            AppendTo[code,"id cfcTC("<>StringRiffle[
                Table[If[MemberQ[active,j],"cfcRN"<>intString[j]<>"?pos_","0"],{j,n}],","]<>",-2,?a)=0;\n"]],
        {active,families}]];
    Do[seen=<||>;
        Do[chords=Expand /@ (pairs[[active,1]]-pairs[[First[active],1]]);
            masses=pairs[[active,2]];
            invariants=If[unit,tdInvariants[chords,dim],{}];
            If[unit && AllTrue[Join[masses,invariants],#===0&],Continue[]];
            signature={masses,If[unit,invariants,chords]};
            key=With[{v=signature},HoldComplete[v]];
            If[KeyExistsQ[seen,key],target=seen[key];AppendTo[code,emit[active,target,unit]],
                AssociateTo[seen,key->active]],
            {active,families}];
        AppendTo[code,"id cfcLRCanonical(?a)=cfcTC(?a);\n"],
        {unit,{False,True}}];
    StringJoin[code]
];

(* ::Section:: *)
(* Small scalar planning; bounded topology size, unrestricted tensor rank *)
(* A four-chord limit bounds symbolic matrix preparation, not numerator rank.
   Larger active subfamilies remain formal. Partial fractions can first lower
   a family containing many masses on at most four distinct routes. *)
$lrMaximumChords = 4;
lrPlan[rows_, originalEntries_, dimension_, masters_:False, supports_:Automatic] := Module[
    {entries = originalEntries, n = Length[rows], pairs, vars, pattern, names, serial, register, registry = <||>, decl, code,
     call, move, conditions, sameRoute, subsets, mat, det, inverse,
     active, t, base, rhs, changed, coefficient, dimName, skipped = {}, k, masterPlan=<||>},
    pairs = ({decode[#["Chord"]], decode[#["MassSquared"]]} &) /@ rows;
    vars = Table["cfcRN" <> intString[j], {j,n}];
    pattern = (# <> "?") & /@ vars;
    names = Lookup[entries, "Name", {}];
    serial = Max[Prepend[FromDigits[StringDrop[#,3]] & /@
        Select[names, StringMatchQ[#, "cfa" ~~ DigitCharacter..] &], 0]];
    (* Decode existing scalar entries once per job. Keep the first matching
       entry, as the former SelectFirst lookup did, including cross-kind aliases.
       Keys freeze evaluated expressions; lookup adds no algebraic equivalences. *)
    Scan[Function[e, With[{key = With[{value = decode[e["Expression"]]}, HoldComplete[value]]},
        If[!KeyExistsQ[registry, key], AssociateTo[registry, key -> e["Name"]]]]],
        Select[entries, MemberQ[{"Scalar", "Abbreviation"}, #["Kind"]] &]];
    register[x_] := Module[{key, name},
        If[!scalarQ[x], throwFailure["UnsupportedScalar","A coefficient-reduction invariant is not a supported scalar."]];
        If[IntegerQ[x], Return[intString[x]]];
        If[Head[x] === Rational, Return["(" <> ToString[x,InputForm] <> ")"]];
        key = With[{value = x}, HoldComplete[value]];
        If[KeyExistsQ[registry, key], Return[registry[key]]];
        name = "cfa" <> intString[++serial];
        AppendTo[entries, <|"Name"->name,"Kind"->"Abbreviation","Expression"->encode[x]|>];
        AssociateTo[registry, key -> name]; name
    ];
    dimName = register[dimension];
    call[v_, labels_:"?a"] := "cfcIR(" <> StringRiffle[v,","] <> ",-2" <> If[labels === "", "", "," <> labels] <> ")";
    move[v_, j_, amount_] := ReplacePart[v, j -> "(" <> v[[j]] <> If[amount >= 0,"+",""] <> intString[amount] <> ")"];
    decl = "\nSymbols " <> StringRiffle[Join[vars,{"cfcRK"}],","] <> ";\nCFunction cfcIR,cfcLRKey,cfcLRReduced,cfcLRReducedPart,cfcLRCanon,cfcLRCanonical;\n";
    code = "\n* Select only coefficients with repeated powers.\n";
    Do[conditions=ReplacePart[pattern,j->vars[[j]]<>"?{>1}"];
        code=code<>"id cfcTC("<>StringRiffle[conditions,","]<> ",-2,?a)="<>call[vars]<>";\n",{j,n}];
    If[TrueQ[masters], masterPlan=smFamilyRules[rows,register,dimName,vars,supports];
        decl=decl<>"CFunction cfcSMHold;\n";
        code=masterPlan["Protection"]<>code<>"id cfcTC(?a)=cfcIR(?a);\n"];
    (* Parameter moments obey C_{...j}^{nu} = -2 nu_j
       C_{...00}^{nu+e_j}. This preserves normalisation and removes all positive
       labels before scalar IBP. The corresponding scalar dimension is D+2h,
       where 2h is the remaining number of zero labels. *)
    code=code<>"repeat;\n";
    Do[code=code<>"id "<>call[pattern,"?a,"<>intString[j]<>",?b"]<>"=-2*"<>vars[[j]]<>"*"<>
        call[move[vars,j,1],"?a,0,0,?b"]<>";\n",{j,n}];
    (* One lowering step per term and per module permits .sort to combine
       intermediate integrals. A single recursive repeat creates an exponential
       tree before FORM has an opportunity to collect identical branches. *)
    code=code<>"endrepeat;\n.sort\n#do cfcReductionStep=1,1\n#$cfcIRChanged=0;\n";
    If[TrueQ[masters], code=code<>If[TrueQ[$smShareStates],"Bracket+ cfcIR,cfcTC,cfcSMHold;\n.sort\nKeep Brackets;\n",""]<>masterPlan["Code"]];
    (* D_j-D_i = m_i^2-m_j^2. Each branch lowers the sum of powers by one;
       no term is discarded and coincident masses are never divided by zero. *)
    sameRoute=Select[If[ListQ[supports],Select[lrReachableFamilies[supports,2],Length[#]===2&],Subsets[Range[n],{2}]],
        pairs[[#[[1]],1]]===pairs[[#[[2]],1]] &];
    Do[With[{i=ij[[1]],j=ij[[2]],delta=Together[pairs[[ij[[1]],2]]-pairs[[ij[[2]],2]]]},
        If[delta =!= 0,
            conditions=pattern;conditions[[i]]=vars[[i]]<>"?pos_";conditions[[j]]=vars[[j]]<>"?pos_";
            If[TrueQ[masters],masterPlan["Restrictions"]=Append[masterPlan["Restrictions"],delta]];
            coefficient=register[1/delta];
            code=code<>"if (match("<>call[conditions]<>"));\nid once "<>call[conditions]<>"="<>coefficient<>"*("<>
                call[move[vars,j,-1]]<>"-"<>call[move[vars,i,-1]]<>");\n$cfcIRChanged=1;\ngoto cfcReductionEnd;\nendif;\n"]],{ij,sameRoute}];
    subsets=lrReductionFamilies[rows,pairs,$lrMaximumChords,supports];
    Do[
        mat=Table[Together[pairs[[i,2]]+pairs[[j,2]]-
            lrSquare[pairs[[i,1]]-pairs[[j,1]],dimension]],{i,active},{j,active}];
        det=Together[Det[mat]];
        If[det===0,AppendTo[skipped,active];Continue[]];
        If[TrueQ[masters],masterPlan["Restrictions"]=Append[masterPlan["Restrictions"],det]];
        inverse=Map[Together,Inverse[mat],{2}];
        Do[
            conditions=Table[If[!MemberQ[active,j],"0",vars[[j]]<>If[j===t,"?{>1}","?pos_"]],{j,n}];
            base=ReplacePart[Table[If[MemberQ[active,j],vars[[j]],"0"],{j,n}],t->"("<>vars[[t]]<>"-1)"];
            (* Sum_j Y_ij nu_j J(nu+e_j) = (d-|nu|)J(nu)
               - Sum_j nu_j J(nu+e_j-e_i). All RHS integrals have a
               strictly smaller total power than the selected LHS integral. *)
            rhs=Table[With[{i=active[[ii]],weight=inverse[[First[FirstPosition[active,t]],ii]]},
                If[weight===0,"0",register[weight]<>"*( ("<>dimName<>"+nargs_(?a)-("<>
                    StringRiffle[base[[active]],"+"]<>"))*"<>call[base]<>"-"<>
                    StringRiffle[Table[changed=move[move[base,j,1],i,-1];
                        base[[j]]<>"*"<>call[changed],{j,active}],"-"]<>")"]],{ii,Length[active]}];
            code=code<>"if (match("<>call[conditions]<>"));\nid once "<>call[conditions]<>"=("<>StringRiffle[rhs,"+"]<>")/("<>vars[[t]]<>"-1);\n$cfcIRChanged=1;\ngoto cfcReductionEnd;\nendif;\n",
            {t,active}],
        {active,subsets}];
    code=code<>"label cfcReductionEnd;\nModuleOption maximum,$cfcIRChanged;\n.sort\n#if `$cfcIRChanged' == 1\n#redefine cfcReductionStep \"0\"\n#endif\n#enddo\nid cfcIR(?a)=cfcTC(?a);\n.sort\n";
    If[TrueQ[masters],code=code<>"id cfcSMHold(?a)=cfcTC(?a);\n.sort\n";
        masterPlan["Restrictions"]=DeleteDuplicates[masterPlan["Restrictions"]]];
    <|"Entries"->entries,
      "Declarations"->decl<>"#procedure cfcLRReduce\n"<>code<>"#endprocedure\n",
      "Processing"->Block[{$lrCollectionPlan=<||>},lrIsolatedProcessing[]],"SingularSubfamilies"->skipped,"MasterPlan"->masterPlan|>
];

(* Keep a reduced linear combination intact through final amplitude assembly.
   The alternate Collect head splits oversized combinations into additive pieces.
   Only scalars times exactly one validated integral token are accepted. *)
(* Scalar abbreviations can decode to substantial rational expressions. Their
   validity is invariant within an import; do not recursively revalidate the
   same scalar beside every integral token. Keep this cache bounded as well. *)
lrScalarQ[x_] := Module[{key, valid, bytes},
    If[MatchQ[x,_Integer|_Rational|_Complex|_Symbol],Return[scalarQ[x]]];
    If[!TrueQ[$pgNestedParsing], Return[scalarQ[x]]];
    key = With[{value=x},HoldComplete[value]];
    If[KeyExistsQ[$lrScalarCache,key],Return[$lrScalarCache[key]]];
    valid = scalarQ[x];
    bytes=ByteCount[x]+256;
    If[Length[$lrScalarCache]<2048 && bytes<=65536 && $lrScalarCacheBytes+bytes<=8388608,
        AssociateTo[$lrScalarCache,key->valid];$lrScalarCacheBytes+=bytes];
    valid
];
lrLinearCoefficientQ[x_] := Which[
    x === 0, True,
    Head[x] === tdToken, True,
    Head[x] === Plus, AllTrue[List @@ x, lrLinearCoefficientQ],
    Head[x] === Times, With[{f = List @@ x},
        Count[f,_tdToken] === 1 && AllTrue[DeleteCases[f,_tdToken],lrScalarQ]],
    True, False
];
lrReducedCall[args_] := If[TrueQ[Lookup[$tdImport,"ReductionCoefficientBlocks",False]] &&
    Length[args] === 1 && lrLinearCoefficientQ[First[args]], lrToken[First[args]],
    throwFailure["InvalidResult","Invalid linear combination of reduced coefficients."]];
(* Parse large scalar coefficient combinations through the existing validated
   arithmetic-tree reader. Locate matching parentheses in bulk, never with a
   recursive regular expression. The fallback preserves general-parser errors. *)
lrParsePrefix[text_, values_, dim_] := Module[{attempt},
    If[!TrueQ[$importParserActive] || !TrueQ[$pgNestedParsing] ||
        StringContainsQ[text,"cfcLRParsed"], Return[parseGeneralResult[text,values,dim]]];
    attempt = Catch[Module[{starts, replacements = {}, aliases = <||>, tail, positions,
        levels, at, close, body, value, name, serial = 0},
        starts = StringPosition[text,RegularExpression["cfcLRReduced(?:Part)?\\("]];
        If[starts === {}, Return[$Failed]];
        Do[
            tail = StringDrop[text,span[[2]]-1];
            positions = StringPosition[tail,"("|")"];
            levels = Accumulate[StringTake[tail,positions] /. {"("->1,")"->-1}];
            at = FirstPosition[levels,0,None];
            If[at === None, Return[$Failed]];
            close = span[[2]]-1+positions[[First[at],1]];
            body = StringTake[text,{span[[2]]+1,close-1}];
            value = pgParseCoefficientTree[body];
            If[value === $Failed, Return[$Failed]];
            value = lrReducedCall[{value}];
            name = "cfcLRParsed"<>intString[++serial];
            AssociateTo[aliases,name->value];
            AppendTo[replacements,{span[[1]],close}->name],
            {span,starts}];
        parseGeneralResult[StringReplacePart[text,Last /@ replacements,First /@ replacements],
            Join[values,aliases],dim]
    ],$failureTag];
    If[attempt === $Failed || FailureQ[attempt],parseGeneralResult[text,values,dim],attempt]
];

lrFactorQ[x_] := Head[x] === lrToken || (Head[x] === Plus && AllTrue[List @@ x, MatchQ[#,_lrToken]&]);
lrReconstruct[x_] := x /. tdToken[a___] :> tdReconstruct[{a}];

(* ::Section:: *)
(* Export orchestration *)
lrPrepare[data_Association, enabled_, drop_] := Module[
    {result=data,mapping=data["Mapping"],entries,rows,plan,denominatorPlan,collection,meta,supports,saved=False,kinematics,kinematicRules,spectators},
    If[!MemberQ[{True,False},enabled],throwFailure["InvalidOption","ReduceLoopCoefficients must be True or False."]];
    If[!enabled,Return[data]];
    entries=mapping["Entries"];
    If[mapping["Version"]===6,
        (* Reducing directly at an integer dimension can turn a vanishing
           recurrence factor times a UV pole into a false zero. Keep such
           coefficients formal; take dimensional limits only after evaluation. *)
        If[Head[decode[mapping["Dimension"]]] =!= Symbol || decode[mapping["Dimension"]] =!= D, Return[data]];
        rows=mapping["TensorDecomposition"]["Families"],
        If[decode /@ mapping["LoopMomenta"] =!= {},
            throwFailure["UnsupportedLoopConfiguration","Coefficient reduction requires active tensor decomposition or an expression with no declared integration variable."]];
        rows=lrSavedRows[entries];If[lrSavedEntries[entries]==={},Return[data]];
        saved=True;
        If[decode[mapping["Dimension"]] =!= D,throwFailure["UnsupportedDimension","Reduction of saved coefficients requires symbolic D."]];
        meta=<|"Mode"->"ExistingCoefficients","LoopMomentum"->Null,
            "Normalisation"->"IntegralEqualsIPiSquaredTimesCoefficient","Basis"->"DistinctSymmetricPlacements",
            "Families"->rows,"DropScaleless"->drop|>;
        mapping=Join[mapping,<|"AlgebraVersion"->mapping["Version"],"Version"->7,
            "TensorDecomposition"->meta,"Processing"->"ExistingLoopCoefficientReduction","ResultLayout"->"IntegralCoefficientGroups"|>];
        result=Join[result,<|"TensorDeclarations"->"\nCFunction cfcTC;\n",
            "TensorProcessing"->lrSavedProgram[entries,rows,drop]|>]
    ];
    If[rows==={},
        mapping=Join[mapping,<|"LoopCoefficientReduction"-><|"Method"->"PartialFractionsAndFixedDimensionIBP",
            "MaximumDistinctChords"->$lrMaximumChords,"Invariants"->{},"SingularSubfamilies"->{}|>|>];
        Return[Join[result,<|"Mapping"->mapping|>]]];
    supports=If[saved,lrSavedSupports[entries,rows],Lookup[data,"LoopFamilySupports",Automatic]];
    plan=lrPlan[rows,entries,decode[mapping["Dimension"]],TrueQ[$smEnabled],supports];
    denominatorPlan=ldgPlan[plan["Entries"]];
    kinematics=If[tdAlgebraMapping[mapping]["Version"]===1,
        rcKinematicPlan[denominatorPlan["Entries"],mapping],
        <|"Entries"->denominatorPlan["Entries"],"Records"->{},"Rules"->""|>];
    plan["Entries"]=kinematics["Entries"];
    denominatorPlan=ldgPlan[plan["Entries"]];
    kinematicRules=kinematics["Rules"];
    If[kinematicRules =!= "",
        mapping=Join[mapping,<|"CoefficientKinematics"-><|"Version"->1,"Identities"->kinematics["Records"]|>|>];
        kinematicRules="Argument;\n"<>kinematicRules<>"EndArgument;\n"<>kinematicRules;
        denominatorPlan["Preparation"]=StringReplace[denominatorPlan["Preparation"],
            "FactArg cfcLRDen;"->(kinematicRules<>"FactArg cfcLRDen;")];
        denominatorPlan["Rules"]=denominatorPlan["Rules"]<>kinematicRules];
    spectators=rcSpectators[plan["Entries"],mapping,rows,kinematics["Records"]];
    collection=<|"Dimension"->dcfPlan[<|"Mapping"->Join[mapping,<|"Entries"->plan["Entries"]|>]|>,False],"CanonicalRules"->(lrCanonicalRules[rows,decode[mapping["Dimension"]],drop,supports]<>If[TrueQ[$smEnabled],smCanonicalRules[rows,decode[mapping["Dimension"]],supports],""]),"MonomialRules"->lrMonomialRules[plan["Entries"]],"Denominators"->denominatorPlan,"SharedBlocks"->(tdAlgebraMapping[mapping]["Version"]===1)|>;
    collection["Rational"] = If[TrueQ[collection["SharedBlocks"]], rcMasterPlan[plan["Entries"],spectators], <||>];
    collection["CoefficientCollection"] = lrCoefficientCollectionPlan[Join[mapping,<|"Entries"->plan["Entries"]|>]];
    plan["Processing"]=Block[{$lrCollectionPlan=collection},lrIsolatedProcessing[]];
    mapping=Join[mapping,<|"Entries"->plan["Entries"],"LoopCoefficientReduction"->
        <|"Method"->"PartialFractionsAndFixedDimensionIBP","CoefficientBlocks"->True,"DenominatorGroups"->True,"IncrementalSharedDefinitions"->collection["SharedBlocks"],"SharedCoefficientBlocks"->collection["SharedBlocks"],"MaximumDistinctChords"->$lrMaximumChords,
          "Invariants"->lrInvariantSnapshot[rows,decode[mapping["Dimension"]]],
          "SingularSubfamilies"->plan["SingularSubfamilies"]|>|>];
    Join[result,<|"Mapping"->mapping,
        "TensorDeclarations"->result["TensorDeclarations"]<>plan["Declarations"]<>collection["Denominators"]["Declarations"]<>Lookup[collection["Dimension"],"Declarations",""]<>Lookup[collection["Rational"],"Declarations",""]<>Lookup[collection["CoefficientCollection"],"Declarations",""]<>
            If[collection["SharedBlocks"],"\nCFunctions cfcShared,cfcSharedPart,cfcShare;\n", ""],
        "LoopCollectionPlan"->collection,"ScalarMasterPlan"->Lookup[plan,"MasterPlan",<||>],
        "LoopReductionProcessing"->kinematicRules<>If[kinematicRules==="","",".sort\n"]<>plan["Processing"]|>]
];

(* ::Section:: *)
(* Shared scalar coefficient blocks *)
(* Large linear reductions reuse already contracted amplitude blocks. FORM
   writes their definitions once, followed by the final integral groups. These
   are a restricted data format, never Wolfram assignments. Legacy definitions are independent; incremental records may refer only to
   earlier validated definitions. SharedCoefficientReader.wl owns both readers
   and rejects cyclic or forward references and integral-valued definitions.
   The final replacement keeps ordinary factored expressions and introduces no
   public head. Matrix jobs retain their established reader and algebra path. *)
$lrSharedValues = None;
lrSharedCall[args_] := If[AssociationQ[$lrSharedValues] &&
    MatchQ[args,{_Integer}] && KeyExistsQ[$lrSharedValues,First[args]],
    lrSharedToken[First[args]],
    throwFailure["InvalidResult","Unknown or invalid shared coefficient reference."]];
