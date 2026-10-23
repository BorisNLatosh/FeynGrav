Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[s,t,u,m,a,b,p,q,l,h];
master=PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
bubble=PaVe[0,{s},{m^2,m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
zero=1/s-1/(s+t)-t/(s (s+t));
(* Switch off only the new final-coefficient pass for a matched reference. *)
SetAttributes[withoutFinalRationals,HoldAll];
withoutFinalRationals[body_] := Block[{CalcFormConverter`Private`$rcMasterMaximumTerms=0},body];
calculate[x_,opts___] := CalcFormCalculate[x,FORMThreads->1,TimeConstraint->30,opts];
cases={
 {"different denominators",zero master,0},
 {"imaginary normalisation",I Pi^2 zero master,0},
 {"complex nonzero",(2+3 I)(1/s+1/t) master,(2+3 I)(s+t)/(s t) master},
 {"complex denominators",(1/(s+I t)-1/(s+I t+u)-u/((s+I t)(s+I t+u)))master,0},
 {"shared polynomial identity",((s+t)^2/(s t)-s/t-t/s-2) master,0},
 {"symbolic dimension",((D-2)/(s (D-1))-(D-2)/((s+t)(D-1))-t (D-2)/(s (s+t)(D-1))) master,0},
 {"complex denominator nonzero",(1/(s+I t)+1/(s-I t))master,2 s/(s^2+t^2)master},
 {"mass dependence",(1/(s+m^2)-1/(t+m^2)+(s-t)/((s+m^2)(t+m^2))) master,0},
 {"vector scalar products",(1/SPD[p,p]-1/(SPD[p,p]+SPD[q,q])-SPD[q,q]/(SPD[p,p](SPD[p,p]+SPD[q,q]))) master,0},
 {"separate masters",zero master+(1/s+1/t) bubble,(s+t)/(s t) bubble},
 {"exact coefficient",(a+b)^3 (1/s+1/t) master,(a+b)^3(s+t)/(s t)master}
};
Do[
 result=calculate[row[[2]]];reference=withoutFinalRationals[calculate[row[[2]]]];
 Put[{row,result,reference},FileNameJoin[{testDirectory,StringReplace[row[[1]]," "->"-"]<>".wl"}]];
 assert[row[[1]]<>" result",!FailureQ[result]];
 assert[row[[1]]<>" exact reference",Together[result-reference]===0];
 assert[row[[1]]<>" expected",Together[result-row[[3]]]===0];
 If[row[[3]]===0,assert[row[[1]]<>" literal zero",result===0]],{row,cases}];
assert["integrated tadpole zero",calculate[zero FAD[{l,m}],LoopMomenta->{l}]===0];
rankTwo=PaVe[0,0,{s},{a,b},PaVeAutoOrder->False,PaVeAutoReduce->False];
assert["tensor reduction unchanged",Together[calculate[rankTwo]-withoutFinalRationals[calculate[rankTwo]]]===0];
assert["serial TFORM zero",CalcFormCalculate[zero master,FORMThreads->2,TimeConstraint->30]===0];
assert["serial TFORM nonzero",Together[CalcFormCalculate[cases[[3,2]],FORMThreads->2,TimeConstraint->30]-cases[[3,3]]]===0];
assert["repeat stable",calculate[calculate[zero master]]===0];
(* Unsupported algebra is retained, not interpreted as a polynomial variable. *)
radical=(1/s+1/t)Sqrt[a+b]master;
assert["opaque radical preserved",Together[calculate[radical]-withoutFinalRationals[calculate[radical]]]===0];
assert["matrix spectator preserved",Together[calculate[zero master GAD[mu]]-withoutFinalRationals[calculate[zero master GAD[mu]]]]===0];
large=(a+b)^30 zero master;
largeResult=calculate[large];
assert["degree fallback retained",largeResult=!=0&&!FailureQ[largeResult]];
assert["degree fallback exact",Together[largeResult]===0];
smallLimit=Block[{CalcFormConverter`Private`$rcMasterMaximumTerms=1},calculate[zero master]];
assert["term fallback retained",smallLimit=!=0&&!FailureQ[smallLimit]];
assert["term fallback exact",Together[smallLimit]===0];
(* Actual per-coefficient variables, not the complete mapping dictionary. *)
many=Sum[zero PaVe[0,{}, {Symbol["mass"<>ToString[j]]^2},PaVeAutoOrder->False,PaVeAutoReduce->False],{j,1,15}];
assert["unrelated mapped variables do not reject",calculate[many]===0];
assert["assumptions are not imported",Block[{$Assumptions=h^2==1},calculate[(h^2-1)master]]=!=0];
(* Manual output keeps the existing dictionary grammar and incremental parser. *)
job=CalcFormExport[cases[[3,2]],FileNameJoin[{testDirectory,"manual.frm"}]];
process=RunProcess[{"form",job["InputFile"]}];
assert["manual FORM execution",process["ExitCode"]===0&&process["StandardError"]===""];
manualResult=Block[{CalcFormConverter`Private`$pgReadChunkSize=7},CalcFormImport[job["ResultFile"],job["MappingFile"]]];
assert["tiny chunk import",Together[manualResult-cases[[3,3]]]===0];
referenceJob=withoutFinalRationals[CalcFormExport[cases[[3,2]],FileNameJoin[{testDirectory,"manual-reference.frm"}]]];
assert["mapping unchanged",Import[job["MappingFile"],"RawJSON"]===Import[referenceJob["MappingFile"],"RawJSON"]];
assert["no temporary rational objects",FreeQ[manualResult, _Symbol?(StringStartsQ[SymbolName[#],"cfcMR"]&)]];
finish["Final master rational coefficients"];
