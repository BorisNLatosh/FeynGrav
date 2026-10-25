Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,q,m,n,s,t,u];
pol=Pair[Momentum[p,D],Momentum[Polarization[q,I],D]];
master=PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
other=PaVe[0,{}, {n^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
input=pol master ((s^2-t^2)/(s-t)-s-t)+
 pol^2 master (s^2-u^2)/(s-u)+pol other (1/s+1/t);

(* Lowered limits exercise denial before work, exhaustion inside a group and a
   complete successful pass. The smaller term limit also enters fallback work.
   Instrument only the counter write; the exported algebra is unchanged. *)
checkBudget[limit_,terms_] := Module[
 {label,job,program,process,result,counters,used,recorded,exhausted,output,labels},
 label="budget-"<>ToString[limit]<>"-terms-"<>ToString[terms];
 job=Block[{CalcFormConverter`Private`$rcPolarisationMaximumRationalPasses=limit,
   CalcFormConverter`Private`$rcMasterMaximumTerms=terms},
  CalcFormExport[input,FileNameJoin[{testDirectory,label<>".frm"}]]];
 assert[label<>" export",AssociationQ[job]];
 If[!AssociationQ[job],Return[$Failed]];
 program=Import[job["InputFile"],"Text"];
 assert[label<>" one end",StringCount[program,"\n.end"]===1];
 Export[job["InputFile"],StringReplace[program,"\n.end"->
  "\n#write \"BUDGET used=%$ limit=%$ exhausted=%$\",$cfcPGWorkUsed,$cfcPGWorkLimit,$cfcPGWorkExhausted\n.end"],"Text"];
 process=RunProcess[{"form",job["InputFile"]}];
 Export[FileNameJoin[{testDirectory,label<>".stdout"}],process["StandardOutput"],"Text"];
 Export[FileNameJoin[{testDirectory,label<>".stderr"}],process["StandardError"],"Text"];
 assert[label<>" execution",process["ExitCode"]===0&&process["StandardError"]===""&&
  !AnyTrue[{"Warning","Program terminating","-->"},StringContainsQ[process["StandardOutput"],#]&]];
 If[process["ExitCode"]=!=0,Return[$Failed]];
 result=CalcFormImport[job["ResultFile"],job["MappingFile"]];
 assert[label<>" import",result=!=$Failed&&!FailureQ[result]];
 assert[label<>" exact",Together[result-input]===0];
 assert[label<>" no private objects",FreeQ[result,_Symbol?(StringStartsQ[SymbolName[#],"cfc"]&)]];
 counters=StringCases[process["StandardOutput"],RegularExpression[
  "BUDGET used=([0-9]+) limit=([0-9]+) exhausted=([01])"]->{"$1","$2","$3"}];
 assert[label<>" counter record",Length[counters]===1];
 If[Length[counters]=!=1,Return[$Failed]];
 {used,recorded,exhausted}=FromDigits/@First[counters];
 assert[label<>" bounded work",used<=limit&&recorded===limit];
 output=Import[job["ResultFile"],"Text"];
 labels=FromDigits/@StringCases[output,StartOfLine~~"cfcShare("~~
   id:DigitCharacter..~~")=" :> id];
 assert[label<>" dense output references",labels===Range[Length[labels]]];
 <|"Exhausted"->exhausted,"Restored"->StringContainsQ[process["StandardOutput"],"work_limit_restore"],
   "Definitions"->Length[labels],"Result"->result|>
];
rows=Flatten[Table[checkBudget[limit,terms],{terms,{1000,3}},{limit,{0,1,2,3,4,32}}]];
assert["exhaustion exercised",AnyTrue[rows,AssociationQ[#]&&#["Exhausted"]===1&]];
assert["complete-master restoration exercised",AnyTrue[rows,AssociationQ[#]&&TrueQ[#["Restored"]]&]];
assert["shared output exercised",AnyTrue[rows,AssociationQ[#]&&#["Definitions"]>0&]];
assert["altered coefficient rejected",AssociationQ[Last[rows]]&&Together[Last[rows]["Result"]-input-pol master]=!=0];
finish["Polarisation aggregate work budget"];
