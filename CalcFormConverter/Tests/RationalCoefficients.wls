Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
(* Expected results are explicit identities. No Wolfram simplification search
   is used to produce either the calculation or its expected answer. *)
cases={
 {FAD[p](D^2-1)/(D-1), FAD[p](D+1)},
 {FAD[p](a^2-b^2)/(a-b), FAD[p](a+b)},
 {FAD[p](D/(D-1)-1-1/(D-1)), 0},
 {FAD[p](a^2+2 a b+b^2), FAD[p](a+b)^2},
 {FAD[p](a+b)/a^2-FAD[p]b/a^2, FAD[p]/a},
 {FAD[p](SPD[p,p]^2-2 SPD[p,p]SPD[p,q]+SPD[p,q]^2),
  FAD[p](SPD[p,p]-SPD[p,q])^2},
 {(FAD[p]+FAD[{p,m}]^2)(D^2-1)/(D-1),
  (FAD[p]+FAD[{p,m}]^2)(D+1)},
 {FAD[p](a+b)^2+I a+FAD[{p,m}]FVD[p,mu]+FAD[{p,2m}]A0[m^2],
  FAD[p](a+b)^2+I a+FAD[{p,m}]FVD[p,mu]+FAD[{p,2m}]A0[m^2]},
 {FAD[p](D^2-1)/(D-1)+FAD[{p,m}]MTD[mu,nu],
  FAD[p](D+1)+FAD[{p,m}]MTD[mu,nu]},
 {FAD[p]Sqrt[D](a+b), FAD[p]Sqrt[D](a+b)},
 {FAD[p]SPD[p,q]^-1(a+b), FAD[p]SPD[p,q]^-1(a+b)},
 {FAD[p] I (a+b)^2, FAD[p] I (a+b)^2}
};
Do[
 got=CalcFormCalculate[cases[[k,1]],FORMThreads->workers];
 assert["rational identity "<>ToString[{workers,k}],!FailureQ[got] && Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[got-FCI[cases[[k,2]]]]]]===0],
 {workers,{1,2}},{k,Length[cases]}];
result=CalcFormCalculate[FAD[p](D^2-1)/(D-1),FORMThreads->1];
assert["rational result compact",result===FCI[FAD[p]](D+1)];
result=CalcFormCalculate[FAD[p](a^2+2 a b+b^2),FORMThreads->1];
assert["polynomial powers retained",!FreeQ[result,(a+b)^2]];
(* Genuine eligibility limits must retain results, not drop coefficients. *)
result=Block[{CalcFormConverter`Private`$rcMaximumTerms=1},
 CalcFormCalculate[FAD[p](a^2+2 a b+b^2),FORMThreads->2]];
assert["oversized fallback equality",Expand[result-FCI[FAD[p](a+b)^2]]===0];
assert["oversized fallback avoids full factors",FreeQ[result,(a+b)^2]];
result=Block[{CalcFormConverter`Private`$rcMaximumVariables=1},
 CalcFormCalculate[FAD[p](a^2+2 a b+b^2),FORMThreads->1]];
assert["variable budget fallback",Expand[result-FCI[FAD[p](a+b)^2]]===0];
ex=Sum[FAD[{p,k m}](D^2-1)/(D-1),{k,17}]+FAD[q]FVD[p,mu]+I FAD[{p,30m}]a;
expected=Sum[FAD[{p,k m}](D+1),{k,17}]+FAD[q]FVD[p,mu]+I FAD[{p,30m}]a;
Do[got=CalcFormCalculate[ex,FORMThreads->workers];
 assert["mixed multiple batches "<>ToString[workers],!FailureQ[got] && Expand[got-FCI[expected]]===0],{workers,{1,2,4}}];
job=CalcFormExport[FAD[p](D^2-1)/(D-1),FileNameJoin[{testDirectory,"rational-auto.frm"}]];
text=Import[job["InputFile"],"Text"];
assert["automatic FORM procedure",StringContainsQ[text,"PolyRatFun cfcRat;"]];
assert["reciprocal definition exposed",StringContainsQ[text,"id cfa1=cfcRat(1,"]];
assert["fallback directive on own line",StringContainsQ[text,"#$cfcRValue=0;\n#else\n"]];
assert["legacy mapping version",Import[job["MappingFile"],"RawJSON"]["Version"]===1];
plain=CalcFormExport[(D^2-1)/(D-1),FileNameJoin[{testDirectory,"rational-plain.frm"}]];
assert["no denominator unchanged",!StringContainsQ[Import[plain["InputFile"],"Text"],"PolyRatFun"]];
radical=CalcFormExport[FAD[p]Sqrt[D](a+b),FileNameJoin[{testDirectory,"rational-radical.frm"}]];
assert["opaque radical stays opaque",!StringContainsQ[Import[radical["InputFile"],"Text"],"PolyRatFun cfcRat;"]];
finish["Rational coefficients"];
