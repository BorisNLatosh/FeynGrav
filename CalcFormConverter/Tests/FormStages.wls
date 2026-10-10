Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* Run after Export.wls and before the driver's FORM/import phases. Preserve the
   existing cases and add both staged programs and independent monolithic ones. *)
cases=Get[FileNameJoin[{testDirectory,"cases.wl"}]];
stageCase[label_,expr_,expectedStages_] := Module[{data,reference,paths,rendered,source},
 data=CalcFormConverter`Private`buildExportData[expr,Automatic,{}];
 assert[label<>" stage count",Length[data["Multiplications"]]===expectedStages];
 add[label,expr];
 source=Import[cases[label]["Job"]["InputFile"],"Text"];
 assert[label<>" input echo disabled",StringStartsQ[source,"#-\n"]];
 assert[label<>" complete rendering",!StringContainsQ[source,RegularExpression["@[A-Z]+@"]]];
 assert[label<>" original fingerprint",data["Mapping"]["ExpressionDigest"]===Hash[FCI[expr],"SHA256","HexString"]];
 (* The outer sum takes the original recursive serializer path. Subtract its
    extra unit in FORM, without reconstructing a reference from the stage list. *)
 reference=CalcFormConverter`Private`buildExportData[1+expr,Automatic,{}];
 assert[label<>" monolithic reference",reference["Multiplications"]==={}];
 assert[label<>" mapping traversal preserved",reference["Mapping"]["Entries"]===data["Mapping"]["Entries"]];
 reference["Body"]="("<>reference["Body"]<>")-1";
 paths=CalcFormConverter`Private`exportPaths[FileNameJoin[{testDirectory,label<>"_monolithic.frm"}],False];
 rendered=CalcFormConverter`Private`renderExport[reference,paths["ResultFile"],CalcFormConverter`Private`readProgramTemplate[]];
 CalcFormConverter`Private`writeExport[paths,rendered,False];
 AssociateTo[cases,label<>"_monolithic"-><|"Expected"->FCI[expr],"Job"->paths|>]
];
stageCase["stages_exact",(x+y)(x-y)(z+2)/3,2];
stageCase["stages_opaque",FAD[{l,m}](s+t)(u+v)(m^2+n^2)^(-2),1];
stageCase["stages_free",(MTD[mu,nu]+FVD[p,mu]FVD[p,nu])(s+t),1];
stageCase["stages_contracted",(MTD[mu,nu]+FVD[p,mu]FVD[p,nu])(FVD[q,mu]+FVD[r,mu])(FVD[q,nu]+FVD[r,nu]),2];
stageCase["stages_single_sum",x+y,0];
stageCase["stages_single_power",(x+y)^3,0];
stageCase["stages_one_sum_factor",z(x+y),0];
(* Connected ordering is a planning step after all identifiers are registered. *)
plan[stages_] := CalcFormConverter`Private`connectedStageOrder[FCI /@ stages];
assert["connected planner follows shared indices",
 plan[{MTD[a,b]+FVD[p,a]FVD[p,b], FVD[q,c]+FVD[r,c],
   MTD[b,c]+FVD[p,b]FVD[p,c]}]==={2,3,1}];
assert["scalar stages retain original order",plan[{x+y,z+t,u+v}]==={1,2,3}];
assert["inconsistent sum signature retains original order",
 plan[{FVD[p,a]+x,FVD[q,a]+y}]==={1,2}];
assert["triple index retains original order",
 plan[{FVD[p,a]+FVD[q,a],FVD[r,a]+FVD[s,a],FVD[t,a]+FVD[u,a]}]==={1,2,3}];
assert["internal contraction and external reuse retains original order",
 plan[{FVD[p,a]^2+x FVD[q,a]^2,FVD[r,a]+FVD[s,a]}]==={1,2}];
(* Explore a bounded set of starting points. This graph has a narrower late
   intermediate when contracted from the opposite end. No physical names enter
   the planner; these are only incidence sets. *)
frontierIndices={{a,b,c,d},{e,f,b,c,h,i},{e,j,f},{k,h,i,l},{a,m,n,o,d,k,l},{j,m,n,o}};
frontierSignatures=(AssociationThread[#,ConstantArray[1,Length[#]]]&)/@frontierIndices;
assert["planner prefers narrower late intermediates",
 CalcFormConverter`Private`connectedStageOrder[Range/@{31,1173,31,163,1173,31},frontierSignatures]==={6,5,1,4,2,3}];
assert["large plans preserve bounded greedy fallback",
 CalcFormConverter`Private`connectedStageOrder[Range[9],Table[AssociationThread[{i,i+1},{1,1}],{i,9}]]===Range[9]];
frontierStages=(Times@@(FVD[p,#]&/@#)+Times@@(FVD[q,#]&/@#)&)/@frontierIndices;
stageCase["stages_frontier",Times@@frontierStages,5];
stageCase["stages_chain",(MTD[a,b]+FVD[p,a]FVD[p,b])
 (MTD[b,c]+FVD[q,b]FVD[q,c])(FVD[r,c]+FVD[s,c]),2];
stageCase["stages_loop",(MTD[a,b]+FVD[p,a]FVD[p,b])
 (MTD[b,c]+FVD[q,b]FVD[q,c])(MTD[c,a]+FVD[r,c]FVD[r,a]),2];
stageCase["stages_internal_dummy",(FVD[p,a]^2+FVD[q,a]^2)
 (MTD[b,c]+FVD[r,b]FVD[r,c])(FVD[s,c]+FVD[t,c]) FVD[v,b],2];
stageCase["stages_dummy_signature_fallback",(FVD[p,a]^2+FVD[q,b]^2)
 (FVD[r,c]+FVD[s,c])(FVD[t,c]+FVD[u,c]),2];
stageCase["stages_polarized",(MTD[a,b]+FVD[p,a]FVD[p,b])
 (Pair[LorentzIndex[b,D],Momentum[Polarization[q,I],D]]+
  Pair[LorentzIndex[b,D],Momentum[Polarization[r,-I],D]])
 (MTD[a,c]+FVD[s,a]FVD[s,c]),2];
(* Exercise independent FORM normalization on large stages. The threshold is
   deliberately conservative; it is not a claim of an optimal crossover size. *)
largeTensor = Sum[(x^j+y^j) (MTD[a,b]+j FVD[p-q,a]FVD[p-q,b]), {j,1,40}];
largeChain = largeTensor (MTD[b,c]+FVD[q,b]FVD[q,c]) (FVD[r,c]+FVD[s,c]) FVD[t,a];
largeData = CalcFormConverter`Private`buildExportData[largeChain,Automatic,{}];
assert["large tensor stages are prepared",Length[largeData["Preparations"]]===3];
smallData = CalcFormConverter`Private`buildExportData[
 (MTD[a,b]+FVD[p,a]FVD[p,b])(FVD[q,b]+FVD[r,b]),Automatic,{}];
assert["tiny tensor stages bypass preparation",smallData["Preparations"]==={}];
invalidData = CalcFormConverter`Private`buildExportData[largeChain FVD[u,a],Automatic,{}];
assert["ambiguous repeated index bypasses preparation",invalidData["Preparations"]==={}];
inconsistentData = CalcFormConverter`Private`buildExportData[
 (largeTensor+x) (FVD[q,b]+FVD[r,b]),Automatic,{}];
assert["inconsistent large sum bypasses preparation",inconsistentData["Preparations"]==={}];
stageCase["prepared_routed",largeChain,2];
stageCase["prepared_free",largeTensor (MTD[b,c]+FVD[q,b]FVD[q,c])
 (FVD[r,c]+FVD[s,c]),2];
stageCase["prepared_opaque",FAD[{l,m}] (A0[m^2]+B0[SPD[p],m^2,m^2]) largeChain,3];
stageCase["prepared_polarized",largeChain /. p -> Polarization[k,I],2];
largeInternal = Sum[(x^j+y^j) (FVD[p,mu]^2+j FVD[q,mu]^2), {j,1,40}];
stageCase["prepared_internal",largeInternal (MTD[a,b]+FVD[r,a]FVD[r,b])
 (FVD[s,b]+FVD[t,b]) FVD[u,a],2];
(* External components join their unique tensor neighbour, without changing
   identifier allocation or splitting a factor that connects several sums. *)
attach[factors_, ranges_] := CalcFormConverter`Private`attachStageFactors[FCI /@ factors,ranges];
assert["external factors join their own sums",
 attach[{FVD[p,a], FVD[q,b], MTD[a,c]+FVD[r,a]FVD[r,c],
   MTD[b,d]+FVD[s,b]FVD[s,d]}, {{1,3},{4,4}}] === {{1,3},{2,4}}];
assert["bridging tensor retains original group",
 attach[{MTD[a,b],FVD[p,a]+FVD[q,a],FVD[r,b]+FVD[s,b]},
   {{1,2},{3,3}}] === {{1,2},{3}}];
assert["ambiguous sum prevents attachment",
 attach[{FVD[p,a],x+FVD[q,b],FVD[r,a]+FVD[s,a]},
   {{1,2},{3,3}}] === {{1,2},{3}}];
assert["triple index prevents attachment",
 attach[{FVD[p,a],FVD[q,a]+FVD[r,a],FVD[s,a]+FVD[t,a]},
   {{1,2},{3,3}}] === {{1,2},{3}}];
stageCase["stages_attached",FVD[p,a] FVD[q,b]
 (MTD[a,c]+FVD[r,a] FVD[r,c]) (MTD[b,c]+FVD[s,b] FVD[s,c]),1];
stageCase["stages_attached_metric",MTD[a,b]
 (MTD[a,b]+FVD[p,a]FVD[p,b]) (SPD[p]+SPD[q]),1];
saveCases[];
finish["FORM stages"];
