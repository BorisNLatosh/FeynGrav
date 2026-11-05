(* Run one suite per fresh kernel under an external memory and time limit. *)
$HistoryLength=0; $FeynCalcStartupMessages=False;
grRun=Environment["GR5_OUTPUT"]; grModel=Environment["GR5_MODEL"];
grSuite=Environment["GR5_SUITE"]; grRoot=Environment["GR5_PACKAGE"];
If[!StringQ[grRoot],grRoot=DirectoryName[$InputFileName,4]];
If[!MemberQ[{"Contact","Compare","FORM"},grSuite] || !StringQ[grRun] || !DirectoryQ[grRun] ||
  AnyTrue[{"completion.json","diagnostics.log","contact.wxf"},FileExistsQ[FileNameJoin[{grRun,#}]]&],Quit[2]];
grLog=OpenWrite[FileNameJoin[{grRun,"diagnostics.log"}]];$Messages={grLog};
grNeed[label_,ok_]:=If[TrueQ[ok],Print["PASS ",label],Print["FAIL ",label];Quit[1]];
SetAttributes[grTimed,HoldRest]; grTimes=<||>;
grTimed[label_,expr_]:=Module[{sec,value},Print["BEGIN ",label];{sec,value}=AbsoluteTiming[expr];
  AssociateTo[grTimes,label->sec];Print["SECONDS ",label," ",sec];value];

(* Ten distinct free index slots m1..m5,n1..n5, followed by p1..p5.
   Every unordered pair gets its own formal variable. *)
grAtom[a_,b_]:=Symbol["Global`z"<>ToString[Min[a,b]]<>"x"<>ToString[Max[a,b]]];
grIndex[FeynArts`Index[_,i_Integer]]:=If[OddQ[i],Quotient[i-1,2],5+Quotient[i-2,2]];
grMomentum[FeynArts`FourMomentum[FeynArts`Incoming,i_Integer]]:={1,9+i};
grMomentum[FeynArts`FourMomentum[FeynArts`Outgoing,i_Integer]]:={-1,11+i};
grMomentum[-q_]:=With[{a=grMomentum[q]},{-First[a],Last[a]}];
grVector[p_,i_]:=With[{a=grMomentum[p]},First[a] grAtom[Last[a],grIndex[i]]];
grDot[p_,q_]:=With[{a=grMomentum[p],b=grMomentum[q]},First[a] First[b] grAtom[Last[a],Last[b]]];

If[grSuite==="Compare",
  Get["FeynCalc`"];
  grSource=Get[FileNameJoin[{grRoot,"Libs","GravitonVertex_3"}]];
  grNeed["source size",Head[grSource]===Plus&&Length[grSource]===76230];
  grIndices=Join[Table[Symbol["Global`m"<>ToString[i]],{i,5}],Table[Symbol["Global`n"<>ToString[i]],{i,5}]];
  grMomenta=Table[Symbol["Global`p"<>ToString[i]],{i,5}];
  grRoles=Join[FeynCalc`LorentzIndex[#,D]&/@grIndices,FeynCalc`Momentum[#,D]&/@grMomenta];
  grRules=Dispatch[Flatten[Table[FeynCalc`Pair[grRoles[[i]],grRoles[[j]]]->grAtom[i-1,j-1],{i,15},{j,i,15}]]];
  grRef=Expand[-I ((grSource/.grRules)/.Global`\[Kappa]->1)];Clear[grSource];
  grNative=Import[Environment["GR5_NATIVE"],"WXF"];
  grNeed["exact generic contact",Length[grRef]===76230&&grNative===grRef];
  grNeed["sign and coefficient controls",grNative=!=-grRef&&grNative=!=grRef+grRef[[1]]],
  (* Contact loads only FeynCalc and FeynArts. FORM uses the public FeynGrav facade. *)
  $LoadAddOns={"FeynArts"};If[grSuite==="Contact",Get["FeynCalc`"],Needs["FeynGrav`"]];
  PrependTo[FeynArts`$ModelPath,grModel];
  grNeed["standalone initialization",grTimed["initialize",FeynArts`InitializeModel["GR",FeynArts`GenericModel->"GR"]]===True];
  grTop=FeynArts`CreateTopologies[0,2->3,FeynArts`Adjacencies->{3,4,5}];
  grIns=grTimed["insert",FeynArts`InsertFields[grTop,ConstantArray[FeynArts`T[1],2]->ConstantArray[FeynArts`T[1],3],
    FeynArts`Model->"GR",FeynArts`GenericModel->"GR",FeynArts`InsertionLevel->{FeynArts`Classes}]];
  grNeed["26 of 26 insertions",Length[grTop]===26&&Length[grIns]===26];
  grContact=Select[grIns,!FreeQ[#,FeynArts`Vertex[5][_]]&];grNeed["one contact",Length[grContact]===1];
  grRaw=grTimed["contact",FeynArts`CreateFeynAmp[grContact,FeynArts`Truncated->True,FeynArts`PreFactor->1,
    FeynArts`MomentumConservation->False]];
  grNeed["one native amplitude",Length[grRaw]===1&&FreeQ[grRaw,FeynGravGR5`Private`tensor|FeynGravGR5`Private`legs|FeynGravGR5`Private`seed]];
  If[grSuite==="Contact",
    grNeed["no source package loaded",!MemberQ[$Packages,"FeynGrav`"]];
    grPoly=grTimed["free-slot comparison polynomial",Expand[grRaw[[1,3]]/.{
      Global`FAMetricTensor[a_,b_]:>grAtom[grIndex[a],grIndex[b]],Global`FAFourVector[p_,i_]:>grVector[p,i],
      Global`FAScalarProduct[p_,q_]:>grDot[p,q]}]];
    grNeed["all slots decoded",FreeQ[grPoly,FeynArts`Index|FeynArts`FourMomentum|grMomentum|grIndex]];
    grNeed["76230 terms and coupling degree three",Length[grPoly]===76230&&Expand[(grPoly/.FeynGrav`\[Kappa]->2)-8(grPoly/.FeynGrav`\[Kappa]->1)]===0];
    Export[FileNameJoin[{grRun,"contact.wxf"}],Expand[-I(grPoly/.FeynGrav`\[Kappa]->1)],"WXF"],
    grIdx=Table[Symbol["grIndex"<>ToString[i]],{i,10}];grPs=Table[Symbol["grMom"<>ToString[i]],{i,5}];
    grFC=grTimed["convert",FeynCalc`FCFAConvert[grRaw,FeynCalc`IncomingMomenta->Take[grPs,2],FeynCalc`OutgoingMomenta->-Drop[grPs,2],
      FeynCalc`LorentzIndexNames->grIdx,FeynCalc`ChangeDimension->D,List->False,FeynCalc`SMP->False,FeynCalc`Contract->False]];
    Clear[grRaw]; grNeed["ten free converted slots",Union[Cases[grFC,FeynCalc`LorentzIndex[j_,D]:>j,Infinity]]===Sort[grIdx]];
    grProbes=Flatten[ConstantArray[#,2]&/@{grA,grB,grA,grB,grA}];
    grExternal=Times@@MapThread[FeynCalc`FVD,{grProbes,grIdx}];
    grAnswer=grTimed["FeynGravSimplify",CalcFormConverter`FeynGravSimplify[grFC grExternal,
       CalcFormConverter`ShowProgress->True,CalcFormConverter`ShowTiming->True]];
    grNeed["returned scalar",!FailureQ[grAnswer]&&FreeQ[grAnswer,FeynCalc`LorentzIndex|_Failure|ComplexInfinity|Indeterminate]];
    grNative=Import[Environment["GR5_NATIVE"],"WXF"];
    grTag[i_,j_]:=Symbol["grCheckDot"<>ToString[Min[i,j]]<>"x"<>ToString[Max[i,j]]];
    grRole[i_]:=If[i<10,Mod[Mod[i,5],2],i-8];
    grExpected=I Expand[grNative/.Dispatch[Flatten[Table[grAtom[i,j]->grTag[grRole[i],grRole[j]],{i,0,14},{j,i,14}]]]];
    grIDs=AssociationThread[Join[{grA,grB},grPs],Range[0,6]];
    grActual=Expand[(grAnswer/.FeynCalc`Pair[FeynCalc`Momentum[a_,D],FeynCalc`Momentum[b_,D]]:>grTag[grIDs[a],grIDs[b]])/.FeynGrav`\[Kappa]->1];
    grNeed["exact nonzero projection and negative control",grActual=!=0&&Expand[grActual-grExpected]===0&&Expand[grActual+grExpected]=!=0]
  ]
];
Close[grLog];grNeed["clean diagnostics",FileByteCount[FileNameJoin[{grRun,"diagnostics.log"}]]===0];
Export[FileNameJoin[{grRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"Suite"->grSuite,
  "PhaseSeconds"->grTimes,"Terms"->Switch[grSuite,"Contact",Length[grPoly],"Compare",Length[grRef],"FORM",Length[grActual]]|>,"RawJSON"];Quit[0];
