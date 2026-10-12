Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[q,p,k,m0,m1,m2,m3,mu,nu,rho,sig];
calc[x_, opts___] := coefficientCalculate[x,LoopMomenta->{q},FORMThreads->1,opts];
den=FAD[{q,m0},{q+p,m1}];
sp=FCI[SPD[p]];
coeff[ids__] := I Pi^2 PaVe[ids,{sp},{m0^2,m1^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
assert["rank zero",calc[den]===coeff[0]];
assert["rank one",calc[FVD[q,mu] den]===FCI[FVD[p,mu]]coeff[1]];
assert["rank two",calc[FVD[q,mu] FVD[q,nu] den]===FCI[MTD[mu,nu]]coeff[0,0]+FCI[FVD[p,mu] FVD[p,nu]]coeff[1,1]];
assert["rank two slash",calc[GSD[q] den]===FCI[GSD[p]]coeff[1]];
assert["scaleless preserved",!FreeQ[calc[FAD[q]],_LoopTensorCoefficient]];
assert["scaleless dropped",calc[FAD[q],DropScaleless->True]===0];
assert["polynomial vacuum scalar",calc[1]===I Pi^2 LoopTensorCoefficient[{0},{},{},{},D]];
assert["polynomial vacuum tensor",calc[FVD[q,mu]FVD[q,nu]]===I Pi^2 FCI[MTD[mu,nu]]LoopTensorCoefficient[{0,0},{},{},{},D]];
assert["odd vacuum zero",calc[FVD[q,mu]]===0];
assert["repeated power",calc[FAD[{q,m0},{q,m0}]]===I Pi^2 LoopTensorCoefficient[{0},{0},{m0^2},{2},D]];
assert["shifted tadpole",calc[FVD[q,mu]FAD[{q+p,m0}]]===-I Pi^2 FCI[FVD[p,mu]]PaVe[0,{},{m0^2},PaVeAutoOrder->False,PaVeAutoReduce->False]];
assert["rational routing",calc[FAD[{2q,m0},{2q,m0}]]===I Pi^2/16 LoopTensorCoefficient[{0},{0},{m0^2/4},{2},D]];
ref=I Pi^2/2 (PaVe[0,{},{m0^2},PaVeAutoOrder->False,PaVeAutoReduce->False]-PaVe[0,{},{m1^2},PaVeAutoOrder->False,PaVeAutoReduce->False])+(-m0^2+m1^2-sp)/2 coeff[0];
assert["scalar product cancellation",Expand[calc[SPD[q,p]den]-ref]===0];
assert["tensor options disabled",coefficientCalculate[SPD[q,p],LoopMomenta->{q},TensorDecomposition->False,FORMThreads->1]===FCI[SPD[q,p]]];
assertFailure["many loops","UnsupportedLoopConfiguration",coefficientExport[1,FileNameJoin[{testDirectory,"bad.frm"}],LoopMomenta->{q,k}]];
assertFailure["hidden loop inverse","UnsupportedLoopDependence",coefficientExport[1/(SPD[q]+1),FileNameJoin[{testDirectory,"hidden.frm"}],LoopMomenta->{q}]];
formal=LoopTensorCoefficient[{0,0},{0,p},{m0^2,m1^2},{2,1},D];
assert["formal round trip",coefficientCalculate[formal,FORMThreads->1]===formal];
(* Independent FeynCalc coefficient-basis comparisons. No reference algebra is
   part of the production implementation. *)
canonical[x_] := PaVeOrder[ToPaVe2[x]] /. PaVe[a___] :> Apply[referenceCoefficient,DeleteCases[{a},_Rule]];
Do[
 expr=Times@@Table[FVD[q,Symbol["tdMu"<>ToString[i]]],{i,rank}] FAD[{q,m0},{q+p,m1}];
 actual=calc[expr];
 reference=TimeConstrained[TID[expr,q,UsePaVeBasis->True,ToPaVe->True,PaVeAutoOrder->False,PaVeAutoReduce->False],30,$Aborted];
 assert["FeynCalc bubble rank "<>ToString[rank],reference=!=$Aborted && Expand[canonical[actual]-canonical[reference]]===0],{rank,0,6}];
Do[
 ds=Take[{FAD[{q,m0}],FAD[{q+p,m1}],FAD[{q+k,m2}],FAD[{q+r,m3}]},legs];
 expr=Times@@ds Times@@Table[FVD[q,Symbol["tdNu"<>ToString[i]]],{i,rank}];
 actual=calc[expr];
 reference=TimeConstrained[TID[expr,q,UsePaVeBasis->True,ToPaVe->True,PaVeAutoOrder->False,PaVeAutoReduce->False],30,$Aborted];
 assert["FeynCalc topology "<>ToString[{legs,rank}],reference=!=$Aborted && Expand[canonical[actual]-canonical[reference]]===0],{legs,{1,3,4}},{rank,0,3}];
Do[
 expr=GSD[q] SUNT[a,b] LCD[mu,nu,rho,sig] den;
 a1=coefficientCalculate[expr,LoopMomenta->{q},FORMThreads->1,DiracAlgebra->da,ColourAlgebra->ca];
 a2=coefficientCalculate[expr,LoopMomenta->{q},FORMThreads->2,DiracAlgebra->da,ColourAlgebra->ca];
 assert["combined engines "<>ToString[{da,ca}],!FailureQ[a1] && a1===a2],{da,{Automatic,False}},{ca,{True,False}}];
(* Differentiation of independently known vacuum moments checks repeated
   powers, signs, normalisation and rank-dependent metric coefficients. *)
vacuumMoment[nu_, zeros_, mass_] := (-1)^(nu-zeros/2) Pi^(D/2-2) Gamma[nu-zeros/2-D/2] mass^(D/2+zeros/2-nu)/(2^(zeros/2) Gamma[nu]);
vacuumValue[x_] := x /. {
 PaVe[inds__, {}, {mass_}, ___Rule] :> vacuumMoment[1,If[{inds}==={0},0,Length[{inds}]],mass],
 LoopTensorCoefficient[ids_,{0},{mass_},{power_},D] :> vacuumMoment[power,If[ids==={0},0,Length[ids]],mass]};
Do[
 numerator=Times@@Table[FVD[q,Symbol["tdVac"<>ToString[i]]],{i,rank}];
 first=vacuumValue[calc[numerator FAD[{q,m0}]]];
 second=vacuumValue[calc[numerator FAD[{q,m0},{q,m0}]]];
 residual=TimeConstrained[FullSimplify[FunctionExpand[second-D[first,m0]/(2m0)]],20,$Aborted];
 assert["mass derivative "<>ToString[rank],residual===0],{rank,{0,2,4}}];
(* Convergent Euclidean radial moment: Wick phase and angular average are
   included explicitly; no numerical integration is used by the converter. *)
Do[
 analytic=N[vacuumMoment[4,2k,4] /. D->3];
 numeric=(-1)^(4-k) (2 Pi^(3/2)/Gamma[3/2])/Pi^2 /
   Product[3+2j,{j,0,k-1}] NIntegrate[t^(2+2k)/(t^2+4)^4,{t,0,Infinity}];
 assert["vacuum parameter moment "<>ToString[k],Abs[analytic-numeric]<10^-9],{k,0,2}];
(* Routing, degeneracies and exceptional kinematics. *)
assert["pinch to formal vacuum",calc[(SPD[q]-m0^2)FAD[{q,m0}]]===I Pi^2 LoopTensorCoefficient[{0},{},{},{},D]];
assert["pinch vacuum dropped",calc[(SPD[q]-m0^2)FAD[{q,m0}],DropScaleless->True]===0];
assert["coalesced rational routing",calc[FAD[{q,m0},{-2q,2m0}]]===I Pi^2/4 LoopTensorCoefficient[{0},{0},{m0^2},{2},D]];
assert["spectator retained",calc[FAD[{q,m0},{k,m1}]]===I Pi^2 FCI[FAD[{k,m1}]] PaVe[0,{},{m0^2},PaVeAutoOrder->False,PaVeAutoReduce->False]];
assert["spectator sums",Expand[calc[FAD[{q,m0}](FAD[{k,m1}]+FAD[{p,m2}])]-I Pi^2 FCI[FAD[{k,m1}]+FAD[{p,m2}]] PaVe[0,{},{m0^2},PaVeAutoOrder->False,PaVeAutoReduce->False]]===0];
assert["explicit dimension preserved",!FreeQ[calc[ChangeDimension[FCI[FVD[q,mu]den],4]],LoopTensorCoefficient[_,_,_,_,4]]];
assert["epsilon loop slot",Contract[calc[LCD[mu,nu,rho][q]den]-FCI[LCD[mu,nu,rho][p]]coeff[1]]===0];
Do[assert["epsilon convention "<>ToString[sign],Block[{$LeviCivitaSign=sign},Contract[calc[LCD[mu,nu,rho][q]den]-FCI[LCD[mu,nu,rho][p]]coeff[1]]===0]],{sign,{-1,1,-I,I}}];
Block[{Pair},
 Pair[Momentum[p,D],Momentum[p,D]]=0;
 assert["null massless kept",!FreeQ[calc[FVD[q,mu]FAD[q,q+p]],_LoopTensorCoefficient]];
 assert["null massless dropped",calc[FVD[q,mu]FAD[q,q+p],DropScaleless->True]===0]
];
assert["unknown scale retained",!FreeQ[calc[FAD[q,q+p],DropScaleless->True],_PaVe]];
Do[assert["invalid option "<>ToString[opt],FailureQ[coefficientExport[1,FileNameJoin[{testDirectory,"bad-option.frm"}],LoopMomenta->{q},opt]]],
 {opt,{TensorDecomposition->Automatic,DropScaleless->Automatic}}];
assert["same symbol colour role",calc[SUNDelta[q,a]den]===FCI[SUNDelta[q,a]]coeff[0]];
assertFailure["boolean option validation","InvalidOption",coefficientCalculate[True,TensorDecomposition->Automatic]];
Do[
 traceResult=calc[DiracTrace[GSD[q].GAD[mu]]den,DiracAlgebra->mode];
 assert["loop-dependent Dirac trace "<>ToString[mode],!FailureQ[traceResult] && Expand[DiracSimplify[traceResult]-4 FCI[FVD[p,mu]]coeff[1]]===0],
 {mode,{Automatic,False}}];
(* Result/mapping pairs are rewritten with a matching digest to exercise
   restricted grammar and typed metadata independently of identity checking. *)
job=coefficientExport[FVD[q,mu] den,FileNameJoin[{testDirectory,"typed-tensor.frm"}],LoopMomenta->{q},OverwriteTarget->True];
map=Import[job["MappingFile"],"RawJSON"];
writeResult[body_,m_:map] := Module[{json=ExportString[m,"RawJSON","Compact"->True]},
 Export[job["MappingFile"],json,"Text"];
 Export[job["ResultFile"],"CFC6 "<>Hash[json,"SHA256","HexString"]<>"\n"<>body,"Text"]];
Do[writeResult[body];assert["reject result "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"1","cfcTC(1,1,-2,3)","cfcTC(1,1,-2,0)","cfcTC(-1,1)","cfcTC(1,1,-2,2)^2","cfcTC(1,1,-2,2)*cfv1(cfi1)","cfcTC(1,1,-2,2)*unknown","cfcTC(1,1,-2,2"}}];
Do[
 broken=map;
 broken["TensorDecomposition"]=Join[map["TensorDecomposition"],bad];
 writeResult["0",broken];
 assert["bad metadata "<>ToString[Keys[bad]],FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {bad,{<|"Normalisation"->"wrong"|>,<|"DropScaleless"->"Automatic"|>,<|"Families"->{<|"Chord"->{"Symbol","Global`q"},"MassSquared"->{"Integer","0"}|>}|>}}];
writeResult["0",KeyDrop[map,"TensorDecomposition"]];
assertFailure["missing conventions","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["cfcTC(1,1,-2,2)*cfv2(cfi1)"];
Do[assert["chunk boundary "<>ToString[size],Block[{CalcFormConverter`Private`$pgReadChunkSize=size},CalcFormImport[job["ResultFile"],job["MappingFile"]]]===FCI[FVD[p,mu]]coeff[1]],{size,{1,2,7,16}}];
assert["closed streams",FreeQ[Streams[],_InputStream?(StringContainsQ[First[#],"typed-tensor"]&)]];
(* Manual and public runtime paths agree. Changed definitions are observed on
   later imports: no coefficient cache escapes the import-local Block. *)
writeResult["cfcTC(1,1,-2,2)*cfv2(cfi1)"];
assert["manual import equals runtime",CalcFormImport[job["ResultFile"],job["MappingFile"]]===calc[FVD[q,mu]den]];
firstImport=CalcFormImport[job["ResultFile"],job["MappingFile"]];
Block[{Pair},Pair[Momentum[p,D],Momentum[p,D]]=s;
 assert["changed scalar product",!FreeQ[CalcFormImport[job["ResultFile"],job["MappingFile"]],s]]];
assert["cache isolation",CalcFormImport[job["ResultFile"],job["MappingFile"]]===firstImport];
writeResult["cfcTC(1,1,-2,2)*cfv2(cfi1)\r\n"];
assert["CRLF",CalcFormImport[job["ResultFile"],job["MappingFile"]]===firstImport];
aborted=CheckAbort[Block[{CalcFormConverter`Private`tdCall},
 CalcFormConverter`Private`tdCall[___]:=Abort[];
 CalcFormImport[job["ResultFile"],job["MappingFile"]]],$Aborted];
assert["abort propagated",aborted===$Aborted];
assert["abort closes stream",FreeQ[Streams[],_InputStream?(StringContainsQ[First[#],"typed-tensor"]&)]];
original=Directory[];
assert["working directory unchanged",calc[1]=!= $Failed && Directory[]===original];
(* Version six must retain pre-integration algebra optimisations. Force stage
   preparation so this is covered by a small, reproducible regression. *)
stagedInput = (MTD[a,b]+FVD[q,a] FVD[p,b])*
    (MTD[a,b]+FVD[p,a] FVD[p,b]) FAD[{q,m0}]/(D-2);
stagedJob = Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
    coefficientExport[stagedInput,FileNameJoin[{testDirectory,"td-stages.frm"}],LoopMomenta->{q}]];
stagedSource = Import[stagedJob["InputFile"],"Text"];
assert["version six retains verified stage symmetry",StringContainsQ[stagedSource,"cfcStageSymCheck"]];
assert["version six retains dimension compaction",StringContainsQ[stagedSource,"PolyRatFun cfcDimRat"]];
assert["version six result format preserved",Import[stagedJob["MappingFile"],"RawJSON"]["Version"]===6];
Do[
 stagedResult=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
    coefficientCalculate[stagedInput,LoopMomenta->{q},FORMThreads->workers]];
 stagedReference=TID[stagedInput,q,UsePaVeBasis->True,ToPaVe->True,PaVeAutoOrder->False,PaVeAutoReduce->False];
 assert["optimised stages integrate correctly "<>ToString[workers],
    !FailureQ[stagedResult] && Together[Contract[canonical[stagedResult]-canonical[stagedReference]]]===0],
 {workers,{1,2}}];
(* Repeated slots exercise distrib_ multiplicities without constructing a
   large independent tensor reference. There are binomial(r,2j)(2j-1)!!
   distinct placements with j metric pairs. *)
Do[
 repeatedActual=calc[SPD[q,k]^rank den];
 repeatedReference=Sum[Binomial[rank,2j] Factorial2[2j-1] FCI[SPD[k]]^j FCI[SPD[p,k]]^(rank-2j)*
    coeff[Sequence@@Join[ConstantArray[0,2j],ConstantArray[1,rank-2j]]],{j,0,rank/2}];
 assert["repeated-slot multiplicity rank "<>ToString[rank],
    !FailureQ[repeatedActual] && Expand[repeatedActual-repeatedReference]===0],{rank,{4,6,8}}];
Do[
 writeResult["cfcTC(1,1,-2)*"<>ratio];
 assert["reject dimension ratio "<>ratio,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {ratio,{"cfcDimRat(1)","cfcDimRat(1,0)","cfcDimRat(cfv2,1)","cfcDimRat(cfv2.cfv2,1)","cfcDimRat(cfi1,1)"}}];
Block[{CalcFormConverter`Private`$tdImport=<|"Dimension"->D,"DimensionName"->{"cfs1"}|>,
       CalcFormConverter`Private`$tdRatioDenominators=<||>},
 Do[
  fastRatio=CalcFormConverter`Private`tdFastDimensionRatio[text];
  generalRatio=CalcFormConverter`Private`parseGeneralResult[text,<|"cfs1"->D|>,D];
  assert["fast dimension polynomial "<>text,fastRatio=!= $Failed && fastRatio===generalRatio],
  {text,{"cfcDimRat(1,3)","cfcDimRat(cfs1^2-1,cfs1-1)",
    "cfcDimRat(-2*cfs1^3+4*cfs1-7,cfs1^2+2)","cfcDimRat( cfs1 ^ 2 - 1 , cfs1 - 1 )"}}];
 Do[assert["dimension polynomial fallback "<>text,
   CalcFormConverter`Private`tdFastDimensionRatio[text] === $Failed],
  {text,{"cfcDimRat(1cfs1,2)","cfcDimRat(cfs1cfs1,2)","cfcDimRat(cfs1 2,3)",
    "cfcDimRat(1,cfs1^-1)","cfcDimRat(1,0)","cfcDimRat()","cfcDimRat(1,2,3)"}}]
];
finish["Tensor decomposition"];
