Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
assert["syntax compilation is lazy", FreeQ[DownValues[CalcFormConverter`Private`pgCoefficientSyntax], _CompiledFunction]];
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
 {t, {"cfs1++cfs2", "cfs1--cfs2", "cfs1+-cfs2", "cfs1/-cfs2", "(0)", "(1)", "(-1)", "(cfs1)", "-(cfs1+cfs2)",
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
shape = CalcFormConverter`Private`pgCoefficientSyntax[];
assert["compiled syntax scanner built", Head[shape] === CompiledFunction];
assert["unbalanced shape rejected", shape[{-5, 1}] === False];
assert["adjacent atoms rejected", shape[{1,2}] === False];
assert["empty input rejected", shape[{}] === False];
assert["excess depth rejected", shape[Join[ConstantArray[-5,65],{1},ConstantArray[-6,65]]] === False];
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
(* Native reconstruction receives only generated slots/operators. Syntax
   resembling Wolfram code must fail without defining or evaluating anything. *)
Clear[parserSentinel];
Do[assert["source syntax rejected "<>t, FailureQ[candidate[t]]],
 {t,{"(cfs1);parserSentinel=1", "(cfs1)+Run[1]", "(cfs1)+#1",
 "(cfs1)+Evaluate[cfs2]", "(cfs1)\nGet[1]", "(cfs1)//Print"}}];
assert["source did not execute", !ValueQ[parserSentinel]];
assert["valid syntax accepts unary signs", shape[{-2,-2,1,-1,-1,2}] === True];
assert["native path returns its own result",
 Catch[CalcFormConverter`Private`withImportParser[values,D,
   CalcFormConverter`Private`pgParseCoefficientTree["(cfs1+cfs2)/(cfs3+2)"]],
   CalcFormConverter`Private`$failureTag] === (a+b)/(c+2)];

(* Group eligibility has the same balanced-parenthesis requirements as the
   general grammar, including a negative prefix depth with a zero final depth. *)
groupValues=Join[values,<|"cfd1"->den|>];
groupReference[t_]:=Catch[CalcFormConverter`Private`parseGeneralResult[t,groupValues,D],
 CalcFormConverter`Private`$failureTag];
groupCandidate[t_]:=Catch[CalcFormConverter`Private`withImportParser[groupValues,D,
 Block[{CalcFormConverter`Private`$pgNestedParsing=True},
 CalcFormConverter`Private`pgParse[t,groupValues,D]]],CalcFormConverter`Private`$failureTag];
Do[assert["group eligibility differential "<>t,SameQ[groupCandidate[t],groupReference[t]]],
 {t,{"cfd1*(0+cfs1*(cfs2+cfs3))","cfd1*((cfs1+cfs2)*(cfs3+2))",
 "cfd1*(-(cfs1+cfs2))","cfd1*(cfs1)","cfd1*(1)",
 "cfd1*(cfs1)(cfs2)","cfd1*)(cfs1)(","cfd1*(cfs999+1/0)",
 "cfd1*(1/0+cfs999)","cfd1*(0*(1/0))","cfd1*(cfs1\r\n+\t(cfs2))",
 "cfd1*((cfs1+cfs2)","cfd1*(cfs1+cfs2))"}}];
(* Empty grouped bodies retain the existing group-specific diagnostic rather
   than the general parser's differently worded empty-parenthesis message. *)
assertFailure["empty grouped body", "InvalidResult", groupCandidate["cfd1*()"]];
(* Exercise native rejection directly and verify dispatcher selection rather
   than relying on the source length alone. No untrusted text is evaluated. *)
native[t_] := Catch[CalcFormConverter`Private`withImportParser[values,D,
 CalcFormConverter`Private`pgParseCoefficientTree[t]],CalcFormConverter`Private`$failureTag];
hostile = {"(cfs1);parserSentinel=1", "(cfs1)+Run[1]", "(cfs1)+#1",
 "(cfs1)+Evaluate[cfs2]", "(cfs1)\nGet[1]", "(cfs1)//Print"};
Do[
 assert["native hostile rejection "<>t, With[{v=native[t]}, v===$Failed || FailureQ[v]]];
 seen=0;
 Internal`InheritedBlock[{CalcFormConverter`Private`pgParseCoefficientTree},
  DownValues[CalcFormConverter`Private`pgParseCoefficientTree]=
   DownValues[CalcFormConverter`Private`pgParseCoefficientTree]/.
    HoldPattern[RuleDelayed[l_,r_]]:>RuleDelayed[l,(seen++;r)];
  assert["padded hostile failure "<>t,FailureQ[candidate[t<>StringRepeat["+0",70]]]]];
 assert["padded hostile native attempt "<>t,seen===1],{t,hostile}];
assert["hostile sources remain inert",!ValueQ[parserSentinel]];
(* Check detects messages even when the returned Failure happens to agree. *)
Do[
 padded=t<>StringRepeat["+0",70];
 nativeResult=Check[native[padded],unexpectedMessage];
 dispatched=Check[candidate[padded],unexpectedMessage];
 expected=Check[reference[padded],unexpectedMessage];
 assert["nested zero native silent "<>t,nativeResult=!=unexpectedMessage && FailureQ[nativeResult]];
 assert["nested zero dispatcher diagnostics "<>t,dispatched=!=unexpectedMessage && SameQ[dispatched,expected]],
 {t,{"(1/(1/0))","(1/(1/(1/0)))","(0*(1/(1/0)))","(1/(1/(cfs1-cfs1)))"}}];
finish["Coefficient tree parser"];
