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
