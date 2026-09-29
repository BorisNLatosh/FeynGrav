Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* Exercise reconstruction without launching FORM. The mapping is produced by
   the exporter; only the restricted result body is synthesized here. *)
parserJob=CalcFormExport[x+FVD[p,mu]+FVD[q,nu],FileNameJoin[{testDirectory,"parser.frm"}]];
assert["parser fixture exports",AssociationQ[parserJob]];
parserJSON=Import[parserJob["MappingFile"],"Text",CharacterEncoding->"UTF-8"];
parserMapping=ImportString[parserJSON,"RawJSON"];
parserHeader="CFC1 "<>Hash[parserJSON,"SHA256","HexString"]<>"\n";
nameFor[symbol_]:=SelectFirst[parserMapping["Entries"],#["Expression"]==={"Symbol",Context[symbol]<>SymbolName[symbol]}&]["Name"];
xName=nameFor[x]; pName=nameFor[p]; qName=nameFor[q]; muName=nameFor[mu]; nuName=nameFor[nu];
parseBody[body_String]:=(Export[parserJob["ResultFile"],parserHeader<>body,"Text",CharacterEncoding->"UTF-8"];
 CalcFormImport[parserJob["ResultFile"],parserJob["MappingFile"]]);

assert["long sum",parseBody[StringRiffle[Table[ToString[k]<>"*"<>xName<>"^"<>ToString[k],{k,1,1500}],"+"]]===Sum[k x^k,{k,1,1500}]];
assert["long product",parseBody[StringRiffle[ConstantArray["("<>xName<>"+1)",2000],"*"]] === (1+x)^2000];
assert["nested collectors",parseBody["("<>xName<>"+1)*(2+3*"<>xName<>")+4*("<>xName<>"-1)"] === (1+x)(2+3x)+4(x-1)];
assert["division associativity",parseBody["24/3/2"]===4];
assert["signed integer exponents",parseBody["2^(-3)+2^(+3)-2^2+(-2)^2"]===65/8];
assert["nested master arguments",parseBody["cfB0("<>xName<>"+1,2*3,4^2)"]===B0[x+1,6,16]];
assert["repeated scalar products",parseBody[StringRiffle[ConstantArray[pName<>"."<>qName,1500],"+"]]===1500 FCI[SPD[p,q]]];
assert["metric and component reconstruction",parseBody["d_("<>muName<>","<>nuName<>")+"<>pName<>"("<>muName<>")"]===FCI[MTD[mu,nu]+FVD[p,mu]]];

(* Lookahead may inspect EOF, but consuming it must always fail. *)
Scan[Function[body,assertFailure["incomplete syntax rejected: "<>body,"InvalidResult",parseBody[body]]],{
 "+", "(", "(1", "1*", "1/", "1^", "1^(", "1^(-", "1^(2",
 "cfA0(", "cfB0(1,", pName<>".", "1)", "1 2"}];
assertFailure["empty result body rejected","MappingMismatch",parseBody[""]];
assertFailure["literal END is not the sentinel","UnknownIdentifier",parseBody["END"]];
assert["recursive function arguments keep their own state",
 parseBody["cfB0(cfA0(2),cfB0(3,4,5),6)+7"]===B0[A0[2],B0[3,4,5],6]+7];

(* Complete tensor-call tokens share the ordinary parser's validation. *)
assert["component call whitespace",
 parseBody[pName<>" \t(\n "<>muName<>" )"]===FCI[FVD[p,mu]]];
assert["metric call whitespace",
 parseBody["d_ \t( "<>muName<>" ,\n "<>nuName<>" )"]===FCI[MTD[mu,nu]]];
assert["parenthesized component argument falls back",
 parseBody[pName<>"(("<>muName<>"))"]===FCI[FVD[p,mu]]];
assert["repeated complete tensor calls",
 parseBody[pName<>"("<>muName<>")+"<>pName<>"("<>muName<>")"]===2 FCI[FVD[p,mu]]];
assertFailure["index identifier suffix is not swallowed","UnknownIdentifier",
 parseBody[pName<>"("<>muName<>"suffix)"]];
assertFailure["whitespace cannot join index identifier","UnknownIdentifier",
 parseBody[pName<>"(cfi 1)"]];
assertFailure["unknown complete component function","InvalidResult",parseBody["cfv999("<>muName<>")"]];
assertFailure["unknown complete component index","UnknownIdentifier",parseBody[pName<>"(cfi999)"]];
assertFailure["earlier arithmetic failure precedes composite lookup","InvalidResult",
 parseBody["1/0+"<>pName<>"(cfi999)"]];
assertFailure["dot chain rejects before later unknown identifier","InvalidResult",
 parseBody[pName<>"."<>qName<>"."<>pName<>".cfv999"]];
assertFailure["dot right operand call is validated first","UnknownIdentifier",
 parseBody[pName<>"."<>qName<>"(cfi999)"]];

(* Invalid typed tokens must fail even when ordinary arithmetic would hide them. *)
Scan[Function[body,assertFailure["typed syntax rejected: "<>body,"InvalidResult",parseBody[body]]],{
 "0*"<>pName,pName<>"-"<>pName,muName<>"^0","cfA0("<>muName<>")",
 pName<>"*0",pName<>"/"<>pName,pName<>"+0",muName<>"."<>pName,
 pName<>"("<>qName<>")","d_("<>muName<>","<>pName<>")", "cfB0(1,2,3,4)","1+"}];
assertFailure["unknown identifier before cancellation","UnknownIdentifier",parseBody["unknownIdentifier*0"]];

(* Exercise the large flat-result path without executing FORM. Padding also
   keeps malformed differential cases above the size cutoff; repeated valid
   factors make them eligible for caching when their grammar permits it. *)
flatPadding = StringRepeat[" ", 131072];
flatTail = StringRepeat["+" <> xName, 128];
flatValues = CalcFormConverter`Private`decodeEntries[parserMapping["Entries"]];
flatDim = CalcFormConverter`Private`decode[parserMapping["Dimension"]];
assert["large repeated flat factors",
  parseBody[StringRiffle[ConstantArray["2*" <> xName <> "^2", 16000], "+"]] === 32000 x^2];
assert["large flat signed powers and division",
  parseBody[flatPadding <> "-2^2+24/3/2+" <> xName <> "^-2" <> flatTail] === x^-2 + 128 x];
assert["large unique factors use compatible fallback",
  parseBody[StringRiffle[Table[ToString[k] <> "*" <> xName <> "^" <> ToString[k], {k, 12000}], "+"]] ===
    Sum[k x^k, {k, 12000}]];
assert["large nested master syntax uses compatible fallback",
  parseBody[flatPadding <> "cfB0(cfA0(2),3,4)" <> flatTail] === B0[A0[2],3,4] + 128 x];
Scan[Function[body,
  assert["flat/general failure agreement: " <> body,
    Catch[CalcFormConverter`Private`parseResult[flatPadding <> body <> flatTail, flatValues, flatDim],
      CalcFormConverter`Private`$failureTag] ===
    Catch[CalcFormConverter`Private`parseGeneralResult[flatPadding <> body <> flatTail, flatValues, flatDim],
      CalcFormConverter`Private`$failureTag]]
], {"1/0+cfs999", "cfs999/0", "cfs999^0", "0^0+cfs999", "0*" <> pName,
    pName <> "." <> qName <> "." <> pName <> ".cfv999",
    pName <> "." <> qName <> "(cfi999)", "1 2", xName <> "^2^3", "1+"}];
Scan[Function[code,
  assert["flat/general whitespace agreement: " <> ToString[code],
    With[{body = flatPadding <> xName <> FromCharacterCode[code] <> "+1" <> flatTail},
      Catch[CalcFormConverter`Private`parseResult[body, flatValues, flatDim],
        CalcFormConverter`Private`$failureTag] ===
      Catch[CalcFormConverter`Private`parseGeneralResult[body, flatValues, flatDim],
        CalcFormConverter`Private`$failureTag]]]
], {9, 10, 133, 160, 8232}];
Clear[FlatParserTest`x];
flatEvaluationCount = 0;
FlatParserTest`x /: Power[FlatParserTest`x, n_Integer] := (flatEvaluationCount++; n + flatEvaluationCount);
assert["mapped UpValues retain per-occurrence evaluation",
  CalcFormConverter`Private`parseResult[
    flatPadding <> StringRiffle[ConstantArray["cfs1^2", 100], "+"],
    <|"cfs1" -> FlatParserTest`x|>, D] === 5250 && flatEvaluationCount === 100];
Clear[FlatParserTest`x];

(* Validation still visits unused entries, even when the result is zero. *)
badEntries=parserMapping["Entries"];
scalarPosition=First[FirstPosition[Lookup[badEntries,"Name"],xName]];
badEntries[[scalarPosition,"Expression"]]={"Momentum",{"Symbol","Global`p"},{"Symbol","System`D"}};
parserMapping["Entries"]=badEntries;
badJSON=ExportString[parserMapping,"RawJSON"];
Export[parserJob["MappingFile"],badJSON,"Text",CharacterEncoding->"UTF-8"];
Export[parserJob["ResultFile"],"CFC1 "<>Hash[badJSON,"SHA256","HexString"]<>"\n0","Text",CharacterEncoding->"UTF-8"];
assertFailure["unused invalid mapped value","InvalidMapping",CalcFormImport[parserJob["ResultFile"],parserJob["MappingFile"]]];
finish["Parser"];
