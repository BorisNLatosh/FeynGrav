Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
assert["syntax compilation is lazy", FreeQ[DownValues[CalcFormConverter`Private`pgCoefficientTreeShape], _CompiledFunction]];
values = <|"cfs1" -> a, "cfs2" -> b, "cfs3" -> c,
 "cfv1" -> CalcFormConverter`Private`vectorToken[p],
 "cfv2" -> CalcFormConverter`Private`vectorToken[q],
 "cfi1" -> CalcFormConverter`Private`indexToken[mu],
 "cfi2" -> CalcFormConverter`Private`indexToken[nu]|>;
reference[t_] := Catch[CalcFormConverter`Private`parseGeneralResult[t, values, D],
 CalcFormConverter`Private`$failureTag];
candidate[t_] := Catch[CalcFormConverter`Private`withImportParser[values, D,
 CalcFormConverter`Private`pgParseNestedChecked[t <> StringRepeat[" ", 128],
  Function[s, CalcFormConverter`Private`parseResult[s, values, D]]]],
 CalcFormConverter`Private`$failureTag];
Do[assert["tree differential " <> t, SameQ[candidate[t], reference[t]]],
 {t, {"(0)", "(1)", "(-1)", "(cfs1)", "-(cfs1+cfs2)",
 "cfs1*(cfs1+cfs2)+cfs2*(cfs1-cfs2)", "(cfs1+cfs2)*(cfs2+cfs3)",
 "cfs1/(cfs2+cfs3)", "1/(0)+cfs999", "cfs999+1/(0)",
 "0*(1/0)", "0/(cfs1-cfs1)", "(0)*cfs999", "cfs999*(0)",
 "1/(-cfs1)/cfs2", "-(-(+cfs1))", "cfs1*-cfs2", "cfs1/-cfs2",
 "(cfs1+cfs2)^2", "(cfs1)^(-2)", "cfs1^ -2*(cfs1+cfs2)",
 "cfv1.cfv2*(cfs1+cfs2)", "d_(cfi1,cfi2)*(cfs1-cfs2)",
 "cfv1(cfi1)*(cfs1+cfs2)", "cfv1(cfi1)^2*(cfs1+cfs2)",
 "d_(cfi1,cfv1)*(1+2)", "cfv1*(1+2)", "1/0+cfv1*(1+2)",
 "(cfs1+cfs2", "cfs1+cfs2)", "()", "(1+)cfs999", "(1)2",
 "cfs1**(1+2)", "1 2*(1+2)", "(cfs1) (cfs2)", "(cfs1)@",
 "cfA0(cfs1)*(cfs1+cfs2)", "cfv1.cfv2.cfv1*(1+2)",
 "(cfs1\r\n+\tcfs2)*(cfs3-2)"}}];
(* Prove this exercises the new path, rather than only its fallback. *)
seen = 0;
Internal`InheritedBlock[{CalcFormConverter`Private`pgParseCoefficientTree},
 DownValues[CalcFormConverter`Private`pgParseCoefficientTree] =
  DownValues[CalcFormConverter`Private`pgParseCoefficientTree] /.
   HoldPattern[RuleDelayed[l_, r_]] :> RuleDelayed[l, (seen++; r)];
 assert["tree dispatch value", candidate["(cfs1+cfs2)*(cfs2+cfs3)"] === (a+b)(b+c)]];
assert["tree dispatch used", seen === 1];
BlockRandom[SeedRandom[762];
 Do[
  terms = Table["(" <> StringRiffle[RandomChoice[{"cfs1", "cfs2", "cfs3", "2", "3", "cfv1.cfv2"}, 3],
     RandomChoice[{"+", "-", "*", "/"}]] <> ")", {4}];
  text = StringRiffle[terms, RandomChoice[{"+", "-", "*", "/"}]];
  assert["tree random " <> ToString[k], SameQ[candidate[text], reference[text]]], {k, 200}]];
shape = CalcFormConverter`Private`pgCoefficientTreeShape[];
assert["compiled syntax scanner built", Head[shape] === CompiledFunction];
assert["unbalanced shape rejected", shape[{-5, 1}][[1,1]] === -1];
assert["adjacent atoms rejected", shape[{1,2}][[1,1]] === -1];
assert["empty input rejected", shape[{}][[1,1]] === -1];
assert["excess depth rejected", shape[Join[ConstantArray[-5,65],{1},ConstantArray[-6,65]]][[1,1]] === -1];
FCClearScalarProducts[];
SPD[p,p] = u;
x = candidate["cfv1.cfv1*(cfs1+cfs2)"];
SPD[p,p] = v;
y = candidate["cfv1.cfv1*(cfs1+cfs2)"];
assert["tree scalar-product isolation", x === u(a+b) && y === v(a+b)];
FCClearScalarProducts[];
aborted = CheckAbort[
 CalcFormConverter`Private`withImportParser[values,D,
  Internal`InheritedBlock[{CalcFormConverter`Private`importFactor},
   Clear[CalcFormConverter`Private`importFactor];
   CalcFormConverter`Private`importFactor[_] := Abort[];
   CalcFormConverter`Private`pgParseCoefficientTree["(cfs1+cfs2)"]]], $Aborted];
assert["tree abort releases import state", aborted === $Aborted &&
 !TrueQ[CalcFormConverter`Private`$importParserActive] &&
 !ValueQ[CalcFormConverter`Private`$importFactorCache]];
finish["Coefficient tree parser"];
