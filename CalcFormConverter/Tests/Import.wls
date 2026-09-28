Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
cases=Get[FileNameJoin[{testDirectory,"cases.wl"}]];
KeyValueMap[Function[{label,test},Module[{actual,difference},
 actual=CalcFormImport[test["Job"]["ResultFile"],test["Job"]["MappingFile"]];
 If[FailureQ[actual],Print["IMPORT FAILURE ",label,": ",actual];assert[label,False],
 difference=Simplify[Contract[ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[actual-test["Expected"]]]]],Assumptions->D>2];
 If[difference=!=0,Print["DIFFERENCE ",label,": ",InputForm[difference]]];
 assert[label,difference===0]]]],cases];
finish["FORM import"];
