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
saveCases[];
finish["FORM stages"];
