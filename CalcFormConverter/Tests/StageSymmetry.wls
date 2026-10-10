Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
sym[u_,v_,i_,j_] := FVD[u,i] FVD[v,j]+FVD[v,i] FVD[u,j];
anti[u_,v_,i_,j_] := FVD[u,i] FVD[v,j]-FVD[v,i] FVD[u,j];
left = MTD[a,b]+FVD[p,a] FVD[q,b];
right = MTD[a,b]+sym[r,s,a,b];
bridge = MTD[a,c] MTD[b,d]+MTD[a,d] MTD[b,c];
items = {
 left right, sym[p,q,a,b] right, anti[p,q,a,b] anti[r,s,a,b],
 left (MTD[a,b]+2 FVD[r,a] FVD[s,b]),
 left bridge (MTD[c,d]+sym[r,s,c,d]),
 anti[p,q,a,b] bridge (MTD[c,d]+sym[r,s,c,d]),
 left right FAD[p] SPD[p]/(D-2),
 I left right FVD[p,mu],
 left (MTD[a,b]+sym[r-p,s+q,a,b]),
 (left right)/(D-1)+(left right)/(D-2),
 (left right)/.D->4,
 (left right)/.D->dim,
 (left right)/.{a->StageOne`mu,b->StageTwo`mu},
 (left right)/.a->cfcStageSymI
};
equal[x_,y_] := !FailureQ[x] && !FailureQ[y] &&
 Together[Contract[ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[x-y]]]]]===0;
Do[
 ref=Block[{CalcFormConverter`Private`$ssEnabled=False,
     CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
   CalcFormCalculate[items[[k]],FORMThreads->workers]];
 got=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
   CalcFormCalculate[items[[k]],FORMThreads->workers]];
 assert["symmetry baseline "<>ToString[{workers,k}],equal[got,ref]];
 assert["symmetry independent reference "<>ToString[{workers,k}],equal[got,items[[k]]]];
 If[FailureQ[got],Print[InputForm[got]]],{workers,{1,2}},{k,Length[items]}];
job=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
 CalcFormExport[left right,FileNameJoin[{testDirectory,"verified-symmetry.frm"}]]];
text=Import[job["InputFile"],"Text"];
assert["symmetry checks emitted",StringContainsQ[text,"cfcStageSymCheck"]];
assert["symmetry guarded",StringContainsQ[text,"#if ( `$cfcStageSymOK1' == 0 )"]];
assert["free indices restored",StringContainsQ[text,"d_(cfcStageSymI,"]];
assert["mapping version unchanged",Import[job["MappingFile"],"RawJSON"]["Version"]===1];
assert["internal plan not serialised",!KeyExistsQ[Import[job["MappingFile"],"RawJSON"],"SymmetryLinks"]];
signatures=CalcFormConverter`Private`stageIndexSignatures[FCI/@{left,right}];
assert["one pair planned",Length[CalcFormConverter`Private`ssLinks[signatures,Function[x,ToString[x,InputForm]]]]===1];
assert["stage budget fallback",Block[{CalcFormConverter`Private`$ssMaximumStages=1},
 CalcFormConverter`Private`ssLinks[signatures,Identity]]==={}];
assert["four-index links excluded",CalcFormConverter`Private`ssLinks[
 {AssociationThread[{a,b,c,d},{1,1,1,1}],AssociationThread[{a,b,c,d},{1,1,1,1}]},Identity]==={}];

(* Observe native term reduction: a passing algebra comparison alone would
   also pass if the new strategy were accidentally never selected. *)
job=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
 CalcFormExport[(MTD[a,b]+sym[p,q,a,b]) right,
  FileNameJoin[{testDirectory,"symmetry-observed.frm"}]]];
source=Import[job["InputFile"],"Text"];
source=StringReplace[source,"Multiply cfcStage2;"->
 "#$observed=termsin_(cfcResult);\n#write \"SYMFLAG %$\",$cfcStageSymOK1\n#write \"SYMTERMS %$\",$observed\nMultiply cfcStage2;"];
Export[job["InputFile"],source,"Text"];
probe=RunProcess[{CalcFormCheck[FORMThreads->1]["Executable"],job["InputFile"]},All,
 ProcessDirectory->testDirectory];
assert["native symmetry probe completed",AssociationQ[probe] && probe["ExitCode"]===0];
assert["native symmetry check passes",AssociationQ[probe] && StringContainsQ[probe["StandardOutput"],"SYMFLAG 0"]];
assert["native symmetry reduces three terms to two",AssociationQ[probe] && StringContainsQ[probe["StandardOutput"],"SYMTERMS 2"]];
finish["Verified stage symmetry"];
