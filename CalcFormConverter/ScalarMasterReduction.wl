(* ::Package:: *)
(* ::Title:: *)
(* Scalar master reduction in FORM *)

(* ::Text:: *)
(* The normalised all-metric coefficient with 2h zero labels is
   (-1/2)^h Pi^(D/2-2) J^(D+2h), where J uses the
   dimension-normalised scalar measure. The common Pi factor does not shift.
   Positive labels obey C_{...j}^nu = -2 nu_j C_{...00}^{nu+e_j}.
   Equations (15), (17) and (33) of hep-ph/9907327 therefore acquire a
   factor -1/2 for each dimension-lowering step. No expansion at D=4 is used.
   Each emitted step decreases dimension shift, total powers, or topology. *)

(* ::Section:: *)
(* Family recurrences *)
$smShareStates = True;
smFamilyRules[rows_, register_, dim_, vars_, supports_:Automatic] := Module[
 {n=Length[rows], pairs, families, code={}, protect={}, restrictions={}, skipped={}, masses,
  invariants, delta, derivatives, second, gram, active, count, p, base,
  condition, rhs, call, lower, emit, formalMasses, symbolicY, symbolicDelta,
  substitutions, j, k},
 pairs=({decode[#["Chord"]],decode[#["MassSquared"]]}&) /@ rows;
 call[v_,labels_] := "cfcIR("<>StringRiffle[v,","]<>",-2"<>If[labels==="","",","<>labels]<>")";
 lower[v_,i_] := ReplacePart[v,i->"("<>v[[i]]<>"-1)"];
 emit[lhs_,rhs_] := "if (match("<>lhs<>"));\nid once "<>lhs<>"="<>rhs<>";\n$cfcIRChanged=1;\ngoto cfcReductionEnd;\nendif;\n";
 families=lrReductionFamilies[rows,pairs,4,supports];
 Do[
  count=Length[active];
  masses=pairs[[active,2]];
  invariants=Table[lrSquare[pairs[[i,1]]-pairs[[j,1]],D],{i,active},{j,active}];
  If[AllTrue[Flatten[{masses,invariants}],#===0&],
   AppendTo[protect,"id cfcTC("<>StringRiffle[Table[If[MemberQ[active,j],vars[[j]]<>"?pos_","0"],{j,n}],","]<>",-2,?a)=cfcSMHold("<>StringRiffle[Table[If[MemberQ[active,j],vars[[j]],"0"],{j,n}],","]<>",-2,?a);\n"];Continue[]];
  formalMasses=Table[Unique["massSquared"],{count}];
  symbolicY=Table[formalMasses[[j]]+formalMasses[[k]]-invariants[[j,k]],{j,count},{k,count}];
  symbolicDelta=Det[symbolicY];substitutions=Thread[formalMasses->masses];
  delta=Together[symbolicDelta/.substitutions];
  derivatives=Together /@ (Table[D[symbolicDelta,formalMasses[[j]]],{j,count}]/.substitutions);
  second=Map[Together,Table[D[symbolicDelta,formalMasses[[j]],formalMasses[[k]]],{j,count},{k,count}]/.substitutions,{2}];
  gram=Together[-Total[derivatives]];
  p=Table[If[MemberQ[active,j],vars[[j]],"0"],{j,n}];
  condition=Table[If[MemberQ[active,j],vars[[j]]<>"?pos_","0"],{j,n}];
  (* Massive tadpole Gamma-function ratio, with no artificial vacuum pinch. *)
  If[count===1,
   rhs=register[masses[[1]]]<>"/("<>dim<>"+nargs_(?a)+2-2*"<>vars[[First[active]]] <>")*"<>call[p,"?a"];
   AppendTo[code,emit[call[condition,"?a,0,0"],rhs]];Continue[]];
  (* Coincident parameter polynomials at zero momentum combine exactly,
     including repeated powers. Positive labels have already been converted. *)
  If[count===2 && invariants[[1,2]]===0 && masses[[1]]===masses[[2]],
   base=ReplacePart[p,{active[[1]]->"("<>StringRiffle[p[[active]],"+"]<>")",active[[2]]->"0"}];
   AppendTo[code,emit[call[condition,"?a"],call[base,"?a"]]];Continue[]];
  If[gram===0,
   If[delta===0,AppendTo[skipped,active];
    AppendTo[protect,"id cfcTC("<>StringRiffle[condition,","]<>",-2,?a)=cfcSMHold("<>StringRiffle[p,","]<>",-2,?a);\n"];Continue[]];
   AppendTo[restrictions,delta];
   rhs=StringRiffle[Table["("<>register[-derivatives[[k]]/(2delta)]<>")*"<>call[lower[p,active[[k]]],"?a"],{k,count}],"+"];
   AppendTo[code,emit[call[condition,"?a"],rhs]];Continue[]];
  AppendTo[restrictions,gram];
  (* Raised powers: equation (15), lowering both dimension and one power. *)
  Do[
   base=lower[p,active[[j]]];
   rhs="(-1/2)*("<>register[1/gram]<>")/("<>vars[[active[[j]]]]<>"-1)*("<>
    register[derivatives[[j]]]<>"*"<>call[base,"?a"]<>"+"<>
    StringRiffle[Table[register[second[[j,k]]]<>"*"<>call[lower[base,active[[k]]],"?a"],{k,count}],"+"]<>")";
   AppendTo[code,emit[call[ReplacePart[condition,active[[j]]->vars[[active[[j]]]]<>"?{>1}"],"?a,0,0"],rhs]],{j,count}];
  (* Unit powers: equation (17), retaining pinches at the lower dimension. *)
  base=Table[If[MemberQ[active,j],"1","0"],{j,n}];
  rhs="(-1/2)*("<>register[1/gram]<>")/("<>dim<>"+nargs_(?a)-"<>intString[count]<>"+1)*("<>
    register[2delta]<>"*"<>call[base,"?a"]<>"+"<>
    StringRiffle[Table[register[derivatives[[k]]]<>"*"<>call[ReplacePart[base,active[[k]]->"0"],"?a"],{k,count}],"+"]<>")";
  AppendTo[code,emit[call[base,"?a,0,0"],rhs]],
 {active,families}];
 <|"Code"->StringJoin[code],"Protection"->StringJoin[protect],"Restrictions"->DeleteDuplicates[restrictions],"UnresolvedFamilies"->skipped|>
];

(* ::Section:: *)
(* Invariant-only families for saved PaVe coefficients *)
(* These private chord labels are dictionary coordinates, never Lorentz
   vectors. Their pairwise distances come solely from the supplied invariants. *)
smPairs[n_] := Join[Flatten[Table[Table[{j+1,Mod[j+k,n]+1},{j,0,n-1}],{k,1,Floor[(n-1)/2]}],1],
 If[EvenQ[n],Table[{j+1,j+1+n/2},{j,0,n/2-1}],{}]];
smChordQ[smChord[n_Integer,j_Integer,inv_List]] := 1<=j<n && Length[inv]===n(n-1)/2 && AllTrue[inv,scalarQ];
smChordQ[_] := False;
encode[x_smChord] := If[smChordQ[x],{"MasterChord",x[[1]],x[[2]],encode /@ x[[3]]},throwFailure["InvalidMapping","Invalid invariant chord."]];
decode[{"MasterChord",n_Integer,j_Integer,inv_List}] := With[{x=smChord[n,j,decode /@ inv]},
 If[smChordQ[x],x,throwFailure["InvalidMapping","Invalid invariant chord."]]];
smCompatible[chords_] := Module[{c=DeleteCases[chords,0]},
 c==={} || FreeQ[c,_smChord] || (AllTrue[c,smChordQ] && Length[DeleteDuplicates[({#[[1]],#[[3]]}&)/@c]]===1)];
smSquare[x_] := Module[{c=DeleteDuplicates[Cases[x,_smChord,{0,Infinity}]],ij,inv,n},
 If[c==={},Return[If[x===0,0,Missing["UnrelatedFamilies"]]]];
 If[!smCompatible[c] || Length[c]>2,Return[Missing["UnrelatedFamilies"]]];
 n=c[[1,1]];inv=c[[1,3]];
 If[Length[c]===1,If[!MemberQ[{c[[1]],-c[[1]]},x],Return[Missing["UnrelatedFamilies"]]];ij={1,c[[1,2]]+1},
  If[!MemberQ[{c[[1]]-c[[2]],c[[2]]-c[[1]]},x],Return[Missing["UnrelatedFamilies"]]];ij=c[[All,2]]+1];
 First[Pick[inv,(Sort[#]===Sort[ij]&) /@ smPairs[n]]]
];
smCoefficient[x_PaVe] := Module[{a=DeleteCases[List@@x,_Rule],n,inv},n=Length[a[[-1]]];inv=a[[-2]];
 CalcFormConverter`LoopTensorCoefficient[Drop[a,-2],Prepend[Table[smChord[n,j,inv],{j,n-1}],0],a[[-1]],ConstantArray[1,n],D]];
smCoefficient[x_] := x;
smSavedEligible[x_PaVe] := Module[{a=DeleteCases[List@@x,_Rule],key,value},
 If[Drop[a,-2]==={0},Return[Length[a[[-1]]]<=4]];
 key=With[{family=Take[DeleteCases[List@@x,_Rule],-2],drop=TrueQ[$smDropScaleless]},HoldComplete[family,drop]];
 If[AssociationQ[$smEligibilityCache] && KeyExistsQ[$smEligibilityCache,key],Return[$smEligibilityCache[key]]];
 value=smSavedFamilySafe[a[[-2]],a[[-1]],TrueQ[$smDropScaleless]];
 If[AssociationQ[$smEligibilityCache],AssociateTo[$smEligibilityCache,key->value]];value
];
smSavedFamilySafe[inv_,ms_,drop_] := Module[{n=Length[ms],chords,dist,active,m,gram,delta,y},
 If[n>4,Return[False]];If[n===1,Return[True]];
 chords=Prepend[Table[smChord[n,j,inv],{j,n-1}],0];
 Do[
 m=Length[active];dist=Table[If[i===j,0,smSquare[chords[[i]]-chords[[j]]]],{i,active},{j,active}];
 If[AllTrue[Join[ms[[active]],Flatten[dist]],#===0&],If[drop,Continue[],Return[False]]];
 If[m===2 && dist[[1,2]]===0 && ms[[active[[1]]]]===ms[[active[[2]]]],Continue[]];
 gram=Together[-2^m Det[Table[(dist[[1,i]]+dist[[1,j]]-dist[[i,j]])/2,{i,2,m},{j,2,m}]]];
 If[gram===0,y=Table[ms[[active[[i]]]]+ms[[active[[j]]]]-dist[[i,j]],{i,m},{j,m}];
  delta=Together[Det[y]];If[delta===0,Return[False]]],
 {active,Subsets[Range[n],{2,n}]}];True
];
smSavedEligible[_] := True;
smRowsCompatible[rows_,active_] := smCompatible[(decode[#["Chord"]]&) /@ rows[[active]]];

(* Scalar unit-power integrals are invariant under permutation of all lines.
   Compare the complete mass/distance graph, not a guessed momentum ordering.
   Reuse an existing family token, and never apply these rules to tensor labels. *)
smCanonicalRules[rows_,dim_,supports_:Automatic] := Module[
 {pairs,n=Length[rows],families,seen=<||>,code={},active,permutation,signature,key,target,token},
 pairs=({decode[#["Chord"]],decode[#["MassSquared"]]}&)/@rows;
 families=Select[If[ListQ[supports],lrReachableFamilies[supports,4],
  Subsets[Range[n],{1,Min[4,n]}]],smRowsCompatible[rows,#]&];
 token[a_] := "cfcTC("<>StringRiffle[Table[If[MemberQ[a,j],"1","0"],{j,n}],","]<>",-2)";
 Do[
  signature=First[Sort[Table[{pairs[[permutation,2]],tdInvariants[pairs[[permutation,1]],dim]},
    {permutation,Permutations[active]}]]];
  key=With[{value=signature},HoldComplete[value]];
  If[KeyExistsQ[seen,key],target=seen[key];AppendTo[code,"id "<>token[active]<>"="<>token[target]<>";\n"],
   AssociateTo[seen,key->active]],{active,families}];
 StringJoin[code]
];

(* ::Section:: *)
(* Public-option orchestration and format contract *)
CalcFormConverter`CalcFormExport::partialmasters = "Some coefficients are retained without scalar-master reduction: `1`.";
smPrepare[data_, enabled_, powers_, drop_, dimensionOption_] := Block[{$smDropScaleless=drop,$smEligibilityCache=<||>},Module[{result, input=data, oldVersion, retained},
 If[!BooleanQ[enabled],throwFailure["InvalidOption","ReduceToScalarMasters must be True or False."]];
 If[!BooleanQ[powers],throwFailure["InvalidOption","ReduceLoopCoefficients must be True or False."]];
 If[!enabled,Return[Block[{$smEnabled=False},lrPrepare[data,powers,drop]]]];
 If[data["Mapping"]["Version"]<6 && data["Mapping"]["LoopMomenta"]=!={},Return[lrPrepare[data,powers,drop]]];
 (* A stand-alone PaVe has conventional symbolic D, although it carries no
    Lorentz object from which the general exporter could infer a dimension. *)
 If[dimensionOption===Automatic && data["Mapping"]["Version"]<6 &&
    AnyTrue[data["Mapping"]["Entries"],#["Kind"]==="IntegralCoefficient" && Head[decode[#["Expression"]]]===PaVe&] &&
    !AnyTrue[data["Mapping"]["Entries"],MemberQ[{"Vector","Index","Denominator"},#["Kind"]]&],
  input["Mapping"]=Join[input["Mapping"],<|"Dimension"->encode[D]|>]];
 If[decode[input["Mapping"]["Dimension"]]=!=D && AnyTrue[input["Mapping"]["Entries"],#["Kind"]==="IntegralCoefficient"&],
  Message[CalcFormConverter`CalcFormExport::partialmasters,"nonstandard coefficient dimension"];Return[input]];
 retained=AnyTrue[input["Mapping"]["Entries"],#["Kind"]==="IntegralCoefficient" && !smSavedEligible[decode[#["Expression"]]]&];
 result=Block[{$smEnabled=True},lrPrepare[input,True,drop]];
 If[!KeyExistsQ[result["Mapping"],"LoopCoefficientReduction"],
  If[retained,Message[CalcFormConverter`CalcFormExport::partialmasters,"unsupported topology or an exceptional invariant subfamily"]];
  Return[result]];
 oldVersion=result["Mapping"]["Version"];
 result["Mapping"]=Join[result["Mapping"],<|"Version"->8,"ScalarMasterReduction"->
  <|"Method"->"DavydychevTarasov","SourceVersion"->oldVersion,"MaximumPoints"->4,
    "Dimension"->encode[D],"GenericDenominators"->(encode /@ Lookup[Lookup[result,"ScalarMasterPlan",<||>],"Restrictions",{}]),
    "ExceptionalFamilies"->Lookup[Lookup[result,"ScalarMasterPlan",<||>],"UnresolvedFamilies",{}],
    "Normalisation"->"MetricPairEqualsMinusHalfDimensionShift"|>|>];
 result
]];
smValidateMapping[mapping_] := Module[{m=Lookup[mapping,"ScalarMasterReduction",None]},
 If[mapping["Version"]=!=8,
  If[m=!=None,throwFailure["InvalidMapping","Scalar master metadata requires version eight."]];Return[Null]];
 If[!AssociationQ[m] || !AssociationQ[Lookup[mapping,"TensorDecomposition",None]] || !ListQ[Lookup[Lookup[mapping,"TensorDecomposition",<||>],"Families",None]] || Lookup[m,"Method",None]=!="DavydychevTarasov" ||
  !MemberQ[{6,7},Lookup[m,"SourceVersion",None]] || Lookup[m,"MaximumPoints",None]=!=4 ||
  Lookup[m,"Dimension",None]=!=encode[D] || Lookup[m,"Normalisation",None]=!="MetricPairEqualsMinusHalfDimensionShift" ||
  !ListQ[Lookup[m,"GenericDenominators",None]] ||
  !AllTrue[Lookup[m,"GenericDenominators",{}],scalarQ[decode[#]]&] ||
  !ListQ[Lookup[m,"ExceptionalFamilies",None]] ||
  !AllTrue[Lookup[m,"ExceptionalFamilies",{}],ListQ[#] && AllTrue[#,IntegerQ[#] && 1<=#<=Length[mapping["TensorDecomposition"]["Families"]]&]&] ||
  !KeyExistsQ[mapping,"LoopCoefficientReduction"],
  throwFailure["InvalidMapping","Invalid scalar-master conventions."]]
];

(* ::Section:: *)
(* Diagnostics *)
CalcFormConverter`CalcFormImport::partialmasters = "Scalar-master reduction is partial: `1` distinct coefficient(s) remain formal or unreduced (exceptional kinematics, preserved scaleless integrals, unsupported dimension or topology).";
smImportResult[result_,mapping_] := Module[{coefficients,unresolved},
 If[mapping["Version"]=!=8 || FailureQ[result],Return[result]];
 coefficients=DeleteDuplicates[Cases[result,_PaVe|_CalcFormConverter`LoopTensorCoefficient,{0,Infinity}]];
 unresolved=Select[coefficients,Head[#]===CalcFormConverter`LoopTensorCoefficient || Drop[DeleteCases[List@@#,_Rule],-2]=!={0}&];
 If[AssociationQ[$smSummary],$smSummary=<|"Masters"->Length[coefficients]-Length[unresolved],"Unresolved"->Length[unresolved]|>];
 If[unresolved=!={},Message[CalcFormConverter`CalcFormImport::partialmasters,Length[unresolved]]];
 result
];
