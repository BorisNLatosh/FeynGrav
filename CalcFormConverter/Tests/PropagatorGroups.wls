Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
root=DirectoryName[$testRoot];
(* Each public calculation exercises export, FORM and incremental import. *)
expr=FAD[{p,m}] (a+b)+FAD[{p,m}]^2 FAD[q] (c+d)+x+y;
Do[
 result=CalcFormCalculate[expr,FORMThreads->workers];
 assert["grouped algebra "<>ToString[workers],!FailureQ[result] && Expand[result-FCI[expr]]===0];
 assert["coefficient stays factored "<>ToString[workers],!FreeQ[result,Times[___,_Plus,___]]],
 {workers,{1,2}}];
job=CalcFormExport[expr,FileNameJoin[{testDirectory,"grouped.frm"}]];
map=Import[job["MappingFile"],"RawJSON"];json=Import[job["MappingFile"],"Text"];
assert["layout marked",map["ResultLayout"]==="PropagatorGroups"];
assert["version unchanged",map["Version"]===1];
assert["FORM grouping",StringContainsQ[Import[job["InputFile"],"Text"],"Bracket+ cfd1,cfd2;"]];
name[v_]:=SelectFirst[map["Entries"],CalcFormConverter`Private`decode[#["Expression"]]===v&]["Name"];
da=name[FCI[FAD[{p,m}]]];db=name[FCI[FAD[q]]];sa=name[a];sb=name[b];
header="CFC1 "<>Hash[json,"SHA256","HexString"]<>"\n";
fixture[body_]:=(Export[job["ResultFile"],header<>body,"Text"];CalcFormImport[job["ResultFile"],job["MappingFile"]]);
body="+"<>da<>"*("<>sa<>" + "<>sb<>")\r\n+"<>da<>"*(2*"<>sa<>")\n+"<>db<>"^(-2)*("<>sb<>"/2)+"<>sa<>"-"<>sb;
expected=FCI[FAD[{p,m}](3 a+b)+FAD[q]^-2 b/2+a-b];
Do[assert["chunk boundary "<>ToString[n],Block[{CalcFormConverter`Private`$pgReadChunkSize=n},Expand[fixture[body]-expected]===0]],{n,{1,2,3,7,31,1048576}}];
assert["zero",fixture["0"]===0];
assert["unit group",fixture[sa<>"+"<>sb]===a+b];
assert["signed spaced exponent",Expand[fixture[da<>"^ - 2*("<>sa<>")"]-FCI[FAD[{p,m}]^-2 a]]===0];
Do[assertFailure["malformed "<>bad,"InvalidResult",fixture[bad]],{bad,{"",da<>"*(",da<>"*("<>sa<>")+",da<>"*()",da<>"*("<>sa<>"+"<>db<>")",da<>"*("<>sa<>"))",da<>"*(1/0)"}}];
assertFailure["unknown identifier","UnknownIdentifier",fixture[da<>"*(unmapped)"]];
oldStreams=Streams[];fixture[da<>"*(unknown)"];assert["failure closes stream",Streams[]===oldStreams];
fixture[da<>"*("<>sa<>")"];
oldStreams=Streams[];
aborted=CheckAbort[Block[{CalcFormConverter`Private`pgReadSummands},CalcFormConverter`Private`pgReadSummands[___]:=Abort[];CalcFormImport[job["ResultFile"],job["MappingFile"]]],$Aborted];
assert["abort propagated",aborted===$Aborted];assert["abort closes stream",Streams[]===oldStreams];
assert["no whole-result Import",Internal`InheritedBlock[{Import},Unprotect[Import];Import[path_,args___]/;path===job["ResultFile"]:=Throw["WholeFileRead"];Catch[CalcFormImport[job["ResultFile"],job["MappingFile"]]]]===FCI[FAD[{p,m}]] a];
Export[job["ResultFile"],"CFC1 wrong\n0","Text"];
assertFailure["identity check","MappingMismatch",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
plain=CalcFormExport[a+b,FileNameJoin[{testDirectory,"plain.frm"}]];
assert["denominator-free layout unchanged",!KeyExistsQ[Import[plain["MappingFile"],"RawJSON"],"ResultLayout"]];
assert["denominator-free no brackets",!StringContainsQ[Import[plain["InputFile"],"Text"],"Bracket "]];
(* Tensor and matrix coefficients keep their own validated algebra. *)
items={MTD[mu,nu],LCD[mu,nu,rho,sigma],GAD[mu].GAD[nu],SUNTrace[SUNT[a,b]],SUNT[a,a]};
Do[ex=FCI[(FAD[{p,m}]+2 FAD[q]) items[[k]]];got=CalcFormCalculate[ex,FORMThreads->1];ref=CalcFormCalculate[items[[k]],FORMThreads->1];
 assert["coefficient kind "<>ToString[k],!FailureQ[got] && Expand[got-FCI[(FAD[{p,m}]+2 FAD[q]) ref]]===0],{k,Length[items]}];
(* One group can require explicit endpoints for all other groups as well. *)
ex=FAD[p] SUNT[a]+FAD[q] (SUNT[b] SUNTF[{b},i,j]+SUNFDelta[i,j]);
got=CalcFormCalculate[ex,FORMThreads->1];
assert["global endpoint promotion",!FailureQ[got] && FreeQ[got,SUNT]];
ends=If[FailureQ[got],{},DeleteDuplicates[Cases[got,SUNFIndex[s_Symbol]/;StringStartsQ[Context[s],"CalcFormConverter`ColourIndices`"]:>s,Infinity]]];
assert["two generated endpoints",Length[Select[ends,MemberQ[{"left","right"},SymbolName[#]]&]]===2];
If[Length[ends]>=2,left=SelectFirst[ends,SymbolName[#]==="left"&];right=SelectFirst[ends,SymbolName[#]==="right"&];
 ref=FCI[FAD[p] SUNTF[{a},left,right]+FAD[q](SUNTF[{b},left,right] SUNTF[{b},i,j]+SUNFDelta[left,right] SUNFDelta[i,j])];
 assert["promoted algebra",SUNSimplify[Expand[FeynAmpDenominatorExplicit[got-ref]],SUNNToCACF->False]===0]];

Do[
 item=LCD[rho,sigma,tau,lambda] (GAD[mu].GAD[nu]+m) SUNTrace[SUNT[a,b]];
 got=CalcFormCalculate[FCI[(FAD[{p,m}]+FAD[q]) item],FORMThreads->1,ColourAlgebra->colour,DiracAlgebra->dirac];
 ref=CalcFormCalculate[item,FORMThreads->1,ColourAlgebra->colour,DiracAlgebra->dirac];
 assert["mixed enabled/disabled "<>ToString[{colour,dirac}],!FailureQ[got] && Expand[got-FCI[(FAD[{p,m}]+FAD[q]) ref]]===0],
 {colour,{True,False}},{dirac,{Automatic,False}}];
epsilonJob=CalcFormExport[LCD[mu,nu,rho,sigma] FAD[p],FileNameJoin[{testDirectory,"epsilon-group.frm"}]];
epsilonJSON=Import[epsilonJob["MappingFile"],"Text"];
Export[epsilonJob["ResultFile"],"CFC2 "<>Hash[epsilonJSON,"SHA256","HexString"]<>"\n0","Text"];
assertFailure["grouped epsilon convention even for zero","EpsilonConventionMismatch",
 Block[{$LeviCivitaSign=-$LeviCivitaSign},CalcFormImport[epsilonJob["ResultFile"],epsilonJob["MappingFile"]]]];
translationJob=CalcFormExport[FAD[p] GAD[mu],FileNameJoin[{testDirectory,"translation-group.frm"}],DiracAlgebra->False];
assert["grouped version three",Import[translationJob["MappingFile"],"RawJSON"]["Version"]===3];
translation=CalcFormCalculate[FAD[p] GAD[mu],FORMThreads->1,DiracAlgebra->False];
assert["translation-only gamma",translation===FCI[FAD[p] GAD[mu]]];

flatUsed=False;
Internal`InheritedBlock[{CalcFormConverter`Private`parseFlatResult},
 DownValues[CalcFormConverter`Private`parseFlatResult]=DownValues[CalcFormConverter`Private`parseFlatResult]/.
  HoldPattern[RuleDelayed[lhs_,rhs_]]:>RuleDelayed[lhs,(flatUsed=True;rhs)];
 Block[{CalcFormConverter`Private`$flatParserMinimumCharacters=0,CalcFormConverter`Private`$flatParserMinimumReusePerDistinctFactor=0},
  fixture[da<>"*("<>sa<>"+"<>sb<>")"]]
];
assert["coefficient reaches existing flat parser",flatUsed];
(* Common-factor extraction must preserve exact algebra and avoid
   re-expanding the resulting powers or products during reconstruction. *)
Do[
 ex=FAD[p](2 a^2 b+2 a b^2)+FAD[q](a+b)+7;
 got=CalcFormCalculate[ex,FORMThreads->workers];
 assert["common factors algebra "<>ToString[workers],!FailureQ[got] && Expand[got-FCI[ex]]===0];
 assert["common factors retained "<>ToString[workers],!FreeQ[got,a+b]],
 {workers,{1,2}}];
got=CalcFormCalculate[FAD[p](MTD[mu,nu]^2-D),FORMThreads->1];
assert["factorised zero output",got===0];
Do[assert["factor chunks "<>ToString[n],Block[{CalcFormConverter`Private`$pgReadChunkSize=n},
 fixture[da<>"*(("<>sa<>"+"<>sb<>")*("<>sa<>"-"<>sb<>"))"]]===FCI[FAD[{p,m}]](a+b)(a-b)],{n,{1,3,1048576}}];
assertFailure["factor unknown","UnknownIdentifier",fixture[da<>"*(("<>sa<>")*(unknown))"]];
assertFailure["factor missing multiply","InvalidResult",fixture[da<>"*(("<>sa<>")("<>sb<>"))"]];
assertFailure["factor empty","InvalidResult",fixture[da<>"*(("<>sa<>")*())"]];
flatCount=0;
Internal`InheritedBlock[{CalcFormConverter`Private`parseFlatResult},
 DownValues[CalcFormConverter`Private`parseFlatResult]=DownValues[CalcFormConverter`Private`parseFlatResult]/.
  HoldPattern[RuleDelayed[lhs_,rhs_]]:>RuleDelayed[lhs,(flatCount++;rhs)];
 Block[{CalcFormConverter`Private`$flatParserMinimumCharacters=0,CalcFormConverter`Private`$flatParserMinimumReusePerDistinctFactor=0},
  fixture[da<>"*(("<>sa<>"+"<>sb<>")*("<>sa<>"-"<>sb<>"))"]]
];
assert["each factor reaches flat parser",flatCount===2];
(* Lower the private template guard only in this test; no public option or
   global package setting is changed. *)
Internal`InheritedBlock[{CalcFormConverter`Private`pgOutput},
 DownValues[CalcFormConverter`Private`pgOutput]=DownValues[CalcFormConverter`Private`pgOutput]/.
  HoldPattern[RuleDelayed[lhs_,rhs_]]:>RuleDelayed[lhs,StringReplace[rhs,"<= 20000"->"<= 1"]];
 got=Block[{CalcFormConverter`Private`$rcMaximumTerms=0},CalcFormCalculate[FAD[p](2 a^2 b+2 a b^2),FORMThreads->1]]
];
assert["oversized coefficient fallback",!FailureQ[got] && Expand[got-FCI[FAD[p](2 a^2 b+2 a b^2)]]===0 && FreeQ[got,a+b]];

Do[
 ex=(FAD[p]+FAD[q]) item;
 got=CalcFormCalculate[ex,FORMThreads->workers];
 assert["factorisation extra vocabulary "<>ToString[{k,workers}],!FailureQ[got] && Expand[got-FCI[ex]]===0],
 {workers,{1,2}},{k,3},{item,{Switch[k,1,I(a^2+2 a b+b^2) SPD[p,q],2,FVD[p,mu](a+b),3,(A0[m^2]+B0[SPD[p,p],0,m^2])^2]}}];

(* More groups than workers exercise full and partial batches, while free
   indices in one coefficient must not disable extraction from its neighbours. *)
ex=Sum[FAD[{p,k m}] (2 a^2 b+2 a b^2),{k,1,17}]+FAD[q] MTD[mu,nu]+FAD[r] FVD[p,mu];
Do[
 got=CalcFormCalculate[ex,FORMThreads->workers];
 assert["parallel multi-batch algebra "<>ToString[workers],!FailureQ[got] && Expand[got-FCI[ex]]===0];
 assert["parallel independent eligibility "<>ToString[workers],!FreeQ[got,a+b]],
 {workers,{1,2,4,12}}];
assert["export embeds whole-expression scheduling",StringContainsQ[Import[job["InputFile"],"Text"],"ModuleOption inparallel;"]];
assert["export uses actual worker count",StringContainsQ[Import[job["InputFile"],"Text"],"NTHREADS_"]];

(* Rational and Laurent content, scalar functions retained in the residual,
   and complex coefficients: polynomial division must not discard terms. *)
Do[
 ex=FAD[p] item;
 got=CalcFormCalculate[ex,FORMThreads->workers];
 assert["common content exact "<>ToString[{workers,k}],!FailureQ[got] && Expand[got-FCI[ex]]===0],
 {workers,{1,2}},{k,5},{item,{Switch[k,
 1,2 a^2/(3 b^2)+4 a/(9 b),
 2,SPD[p,q]^(-2) a+SPD[p,q]^(-1) b,
 3,A0[m^2] B0[SPD[p,p],0,m^2] C0[0,0,0,m^2,m^2,m^2] D0[0,0,0,0,0,0,m^2,m^2,m^2,m^2] (2 a^2 b+2 a b^2),
 4,I (2 a^2 b+2 a b^2),
 5,2 a/3+4 b/9]}}];
assert["common extraction emitted",StringContainsQ[Import[job["InputFile"],"Text"],"content_("]];
assert["bounded coefficient factor search",StringContainsQ[Import[job["InputFile"],"Text"],"Factorize cfcRN"]];
(* Nested momentum grouping preserves the coefficient sums, including signed
   Laurent powers, scalar functions, free tensor factors and several keys. *)
Do[
 ex=FAD[p] (SPD[p,q] (a+b)+SPD[q,q] (c+d))+FAD[q] SPD[p,q]^-2 (a+b);
 got=Block[{CalcFormConverter`Private`$pgNestedBlockCharacters=0},CalcFormCalculate[ex,FORMThreads->workers]];
 assert["nested groups exact "<>ToString[workers],!FailureQ[got] && Expand[got-FCI[ex]]===0];
 assert["nested sums retained "<>ToString[workers],Count[got,_Plus,Infinity]>=3];
 ex=FAD[p] (FVD[p,mu](a+b)+FVD[q,mu](c+d));
 got=Block[{CalcFormConverter`Private`$pgNestedBlockCharacters=0},CalcFormCalculate[ex,FORMThreads->workers]];
 assert["nested free components "<>ToString[workers],!FailureQ[got] && Expand[got-FCI[ex]]===0],
 {workers,{1,2}}];
Do[assert["nested chunks "<>ToString[n],Block[{CalcFormConverter`Private`$pgReadChunkSize=n,CalcFormConverter`Private`$pgNestedBlockCharacters=0},
 fixture[da<>"*((1)*("<>sa<>"*("<>sa<>"+"<>sb<>")+"<>sb<>"*("<>sa<>"-"<>sb<>")))"]]===FCI[FAD[{p,m}]](a(a+b)+b(a-b))],{n,{1,3,1048576}}];
assertFailure["nested unknown identifier","UnknownIdentifier",fixture[da<>"*((1)*("<>sa<>"*("<>sa<>"+unknown)))"]];
assertFailure["nested missing close","InvalidResult",fixture[da<>"*((1)*("<>sa<>"*("<>sa<>"+"<>sb<>"))"]];
assert["momentum dictionary brackets",StringContainsQ[Import[job["InputFile"],"Text"],"Bracket cfv"]];
(* Nested decomposition must preserve the old parser's values and diagnostics. *)
Do[
 old=Block[{CalcFormConverter`Private`pgParseNested},
   CalcFormConverter`Private`pgParseNested[t_,parser_,___]:=parser[t]; fixture[da<>"*((1)*("<>text<>"))"]];
 new=Block[{CalcFormConverter`Private`$pgNestedBlockCharacters=0},fixture[da<>"*((1)*("<>text<>"))"]];
 assert["nested parser agreement "<>text,SameQ[old,new]],
 {text, {sa<>"*("<>sa<>"+"<>sb<>")+"<>sb<>"*("<>sa<>"-"<>sb<>")",
 sa<>"^ -2*("<>sb<>"+1)-"<>sb<>"*("<>sa<>"+2)",
 "("<>sa<>"+1)^2*("<>sb<>"+2)","("<>sa<>"+1)/("<>sb<>"+2)",
 "("<>sa<>"+1)*("<>sb<>"+2)*("<>sa<>"-2)",
 sa<>"*(unknown+1)+other*(2+3)",sa<>"*(1/0)+unknown*(2+3)",
 sa<>"*(1+2)+unknown*(2+@)",sa<>"**(1+2)",sa<>"*(1+)",
 sa<>"*(1+2)+("<>sb<>"+)",sa<>"*(1+2)+"<>sb<>"*(3+)"}}];
(* Observe the largest restricted-parser input, without changing reconstruction. *)
leafLengths={};
Block[{CalcFormConverter`Private`$pgNestedParsing=True, CalcFormConverter`Private`$pgNestedBlockCharacters=0},
 CalcFormConverter`Private`pgParseNested["a*(b+c)+b*(a+c)",
 Function[t,AppendTo[leafLengths,StringLength[t]];1]]];
assert["nested leaves bounded",Max[leafLengths]<=3];
finish["Propagator grouping"];
