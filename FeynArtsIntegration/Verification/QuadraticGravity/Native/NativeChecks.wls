(* Native contact tests. All external momenta below are independent and incoming. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;$LoadAddOns={"FeynArts"};
qgRun=Environment["QG_OUTPUT"];
If[!StringQ[qgRun] || !DirectoryQ[qgRun],Print["Supply a fresh QG_OUTPUT directory."];Quit[1]];
qgModel=Environment["QG_MODEL_DIR"];qgSuite=Environment["QG_SUITE"];
qgLog=OpenWrite[FileNameJoin[{qgRun,"diagnostics.log"}]];$Messages={qgLog};
qgCount=0;qgNeed[name_,value_]:=If[TrueQ[value],qgCount++;Print["PASS ",name],Print["STOP ",name];Quit[1]];
Get["FeynCalc`"];
PrependTo[FeynArts`$ModelPath,qgModel];
qgNeed["standalone initialisation",FeynArts`InitializeModel["QuadraticGravity",FeynArts`GenericModel->"QuadraticGravity"]===True];
qgNeed["no package loaded",!MemberQ[$Packages,"FeynGrav`"]];
qgNeed["field inventory and ghost statistics",FeynArts`P$NonCommuting===FeynArts`U&&FeynArts`P$ChainBuildOrder==={FeynArts`U}&&FeynArts`TheMass[FeynArts`T[1]]===0];
qgInsert[in_,out_,adj_]:=FeynArts`InsertFields[FeynArts`CreateTopologies[0,Length[in]->Length[out],FeynArts`Adjacencies->{adj}],in->out,FeynArts`Model->"QuadraticGravity",FeynArts`GenericModel->"QuadraticGravity",FeynArts`InsertionLevel->{FeynArts`Classes}];
qgAmp[top_,adj_]:=FeynArts`CreateFeynAmp[Select[top,Count[#,FeynArts`Vertex[adj][_],Infinity]>0&],FeynArts`Truncated->True,FeynArts`PreFactor->1,FeynArts`MomentumConservation->False];
(* Canonical commuting variables for the unordered pairs of explicit free
   indices and momenta. The reference reader uses the same numbering. *)
qgAtom[a_,b_]:=Symbol["Global`qgz"<>ToString[Min[a,b]]<>"x"<>ToString[Max[a,b]]];
qgIdx[FeynArts`Index[_,i_Integer]]:=If[OddQ[i],Quotient[i-1,2],n+Quotient[i-2,2]];
qgMom[FeynArts`FourMomentum[FeynArts`Incoming,1]]:={1,2n};
qgMom[FeynArts`FourMomentum[FeynArts`Outgoing,i_Integer]]:={-1,2n+i};
qgMom[-v_]:=With[{x=qgMom[v]},{-x[[1]],x[[2]]}];
qgPair[p_,q_]:=With[{a=qgMom[p],b=qgMom[q]},a[[1]]b[[1]]qgAtom[a[[2]],b[[2]]]];
qgVector[p_,i_]:=With[{a=qgMom[p]},a[[1]]qgAtom[a[[2]],qgIdx[i]]];
If[MemberQ[{"Cubic","Quartic"},qgSuite],
 n=If[qgSuite==="Cubic",3,4];
 ins=qgInsert[{FeynArts`T[1]},ConstantArray[FeynArts`T[1],n-1],n];
 raw=qgAmp[ins,n];qgNeed["one native contact",Length[raw]===1];
 tensor=raw[[1,3]];
 qgNeed["all tensor components materialised",FreeQ[tensor,QuadraticGravityModel`Private`tensor|QuadraticGravityModel`Private`legs|FeynArts`G|System`C]];
 poly=tensor/.{Global`FAMetricTensor[a_,b_]:>qgAtom[qgIdx[a],qgIdx[b]],Global`FAFourVector[p_,i_]:>qgVector[p,i],Global`FAScalarProduct[p_,q_]:>qgPair[p,q]};
 qgNeed["all free slots and momenta decoded",FreeQ[poly,_FeynArts`Index|_FeynArts`FourMomentum|_qgMom|_qgIdx|_qgAtom|_qgVector]];
 qgNeed["coupling degree",Expand[(poly/.FeynGrav`\[Kappa]->2)-2^(n-2)(poly/.FeynGrav`\[Kappa]->1)]===0];
 stripped=(poly/.{FeynGrav`\[Kappa]->1,QuadraticGravityModel`M0->1/Sqrt[qgU],QuadraticGravityModel`M2->1/Sqrt[qgV]})/I;
 ref=Get[FileNameJoin[{qgRun,"expected.wl"}]];
 qgNeed["three reference sectors",Length[ref]===3];
 qgNeed["complete native coefficient equality",Expand[stripped-ref[[1]]-qgU ref[[2]]-qgV ref[[3]]]===0];
 qgNeed["wrong sign rejected",Expand[stripped+ref[[1]]+qgU ref[[2]]+qgV ref[[3]]]=!=0];
 (* Compare FCFAConvert output on the same freely specified incoming momenta. *)
 If[Environment["QG_CONVERT"]==="yes",
  names=Table[Symbol["Global`qgExternal"<>ToString[i]],{i,n}];
  converted=FCFAConvert[raw,IncomingMomenta->Take[names,1],OutgoingMomenta->-Rest[names],
    ChangeDimension->D,List->False,SMP->False,Contract->False];
  indexID[LorentzIndex[i_,D]]:={1,qgIdx[FeynArts`Index[Global`Lorentz,FromDigits[StringDrop[SymbolName[i],3]]]]};
  indexID[Momentum[p_,D]]:=Module[{s=If[MatchQ[p,-_Symbol],-1,1],j},
    j=FirstPosition[names,s p];If[!ListQ[j],Print["STOP unknown momentum ",p];Quit[1]];
    {s,2n+First[j]-1}];
  mappedPair[a_,b_]:=With[{ia=indexID[a],ib=indexID[b]},ia[[1]]ib[[1]]qgAtom[ia[[2]],ib[[2]]]];
  fcPoly=converted/.Pair->mappedPair;
  qgNeed["converted result decoded",FreeQ[fcPoly,LorentzIndex|Momentum|Pair|indexID|mappedPair]];
  fcPoly=(fcPoly/.{FeynGrav`\[Kappa]->1,QuadraticGravityModel`M0->1/Sqrt[qgU],QuadraticGravityModel`M2->1/Sqrt[qgV]})/I;
  qgNeed["complete converted result equals reference",Expand[fcPoly-ref[[1]]-qgU ref[[2]]-qgV ref[[3]]]===0];
 ];
];
Close[qgLog];qgNeed["no unexpected messages",FileByteCount[FileNameJoin[{qgRun,"diagnostics.log"}]]===0];
Export[FileNameJoin[{qgRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"Checks"->qgCount|>,"RawJSON"];Quit[0];
