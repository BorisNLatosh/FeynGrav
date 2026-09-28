(* Export the complete target without running its potentially large FORM job. *)
$FeynCalcStartupMessages=False;
Get[FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],"FeynGrav.wl"}]];
importQuadraticGravity[1];
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Examples","ScalarBubble.wl"}]];
expr=ScalarBubbleExample[p,l,m0,m2];
{seconds,job}=AbsoluteTiming[CalcFormExport[expr,FileNameJoin[{Environment["CFC_TEST_DIR"],"full-bubble.frm"}],LoopMomenta->{l}]];
If[FailureQ[job],Print[job];Quit[1]];
mapping=Import[job["MappingFile"],"RawJSON"];
entries=mapping["Entries"];
decode=CalcFormConverter`Private`decode;
denominators=Select[entries,#["Kind"]==="Denominator"&];
indices=Select[entries,#["Kind"]==="Index"&];
abbreviations=Select[entries,#["Kind"]==="Abbreviation"&];
sourcePD=DeleteDuplicates[Cases[FCI[expr],_PropagatorDenominator,Infinity]];
mappedPD=(First[decode[#["Expression"]]]& /@ denominators);
sourceIndices=DeleteDuplicates[Cases[FCI[expr],LorentzIndex[x_,___]:>x,Infinity]];
mappedIndices=decode[#["Expression"]]& /@ indices;
If[Sort[sourcePD]=!=Sort[mappedPD] || Sort[sourceIndices]=!=Sort[mappedIndices] ||
 mapping["ExpressionDigest"]=!=Hash[FCI[expr],"SHA256","HexString"] ||
 (decode /@ mapping["LoopMomenta"])=!={l} ||
 !AllTrue[denominators,decode[#["Momentum"]]===First[First[decode[#["Expression"]]]] &&
   decode[#["Mass"]]===Last[First[decode[#["Expression"]]]] && #["Power"]===1 &] ||
 !AllTrue[abbreviations,MemberQ[expr,decode[#["Expression"]],{0,Infinity}]&],
 Print["FAIL: complete bubble metadata preservation"];Quit[1]];
Print["Complete bubble export: ",seconds," seconds; ",FileByteCount[job["InputFile"]],
 " FORM bytes; ",FileByteCount[job["MappingFile"]]," mapping bytes. Metadata checks passed."];
Export[FileNameJoin[{Environment["CFC_TEST_DIR"],"test-completed"}],"BubbleExport","Text"];
Quit[];
