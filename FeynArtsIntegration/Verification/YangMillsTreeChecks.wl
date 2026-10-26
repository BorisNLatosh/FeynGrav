(* Native tree checks at generic massless Mandelstam invariants and symbolic D.
   External polarisation vectors remain independent; no helicity or D=4 limit. *)
Needs["FeynCalc`"];
Get[FileNameJoin[{treeRepo,"CalcFormConverter","CalcFormConverter.wl"}]];
CheckAbort[
AppendTo[FeynArts`$ModelPath,treeCandidate];
treeNeed["standalone initialisation",FeynArts`InitializeModel["GR-SU-N-Yang-Mills",FeynArts`GenericModel->"GR-SU-N-Yang-Mills"]===True];
treeNeed["symbolic dimension",OwnValues[D]==={}&&OwnValues[FeynCalc`SUNN]==={}&&!MemberQ[$Packages,"FeynGrav`"]];
treeTypes=Characters[treeCase];
treeFields=Symbol["FeynArts`"<>#][1]&/@treeTypes;
treeTopologies=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4,5,6}];
treeInsertions=FeynArts`InsertFields[treeTopologies,Take[treeFields,2]->Drop[treeFields,2],FeynArts`Model->"GR-SU-N-Yang-Mills",FeynArts`GenericModel->"GR-SU-N-Yang-Mills",FeynArts`InsertionLevel->{FeynArts`Classes}];
(* Species-labelled channels: both the exchanged field and external partition. *)
treeChannels={};
Do[
 treeTop=First[topRule];treeInt=Cases[treeTop,FeynArts`Propagator[FeynArts`Internal][x_,y_,f_]:>{x,y,f}];
 treeExt=Cases[treeTop,FeynArts`Propagator[FeynArts`Incoming|FeynArts`Outgoing][x_,y_,_]:>{x,y}];
 treeGraphs=Cases[Last[topRule],FeynArts`FeynmanGraph[_,FeynArts`Classes==_][rr__]:>{rr},Infinity];
 Do[If[treeInt==={},AppendTo[treeChannels,"contact"],
  treeLegs=Sort[Table[Sort[Flatten[Cases[Select[treeExt,MemberQ[#,treeInt[[1,j]]]&],FeynArts`Vertex[1][i_]:>i,Infinity]]],{j,2}]];
  treeSpecies=Head[treeInt[[1,3]]/.graph];
  AppendTo[treeChannels,SymbolName[treeSpecies]<>":"<>StringRiffle[StringJoin[ToString/@#]&/@treeLegs,"|"]]],{graph,treeGraphs}],{topRule,List@@treeInsertions}];
treeExpected=Switch[treeCase,"VVVV",{"contact","T:12|34","T:13|24","T:14|23","V:12|34","V:13|24","V:14|23"},"VVTT",{"contact","T:12|34","V:13|24","V:14|23"},"VVVT",{"contact","V:12|34","V:13|24","V:14|23"}];
Print["CHANNELS ",InputForm[treeChannels]];
treeNeed["species-labelled channels",Sort[treeChannels]===Sort[treeExpected]];
treeRaw=FeynArts`CreateFeynAmp[treeInsertions,FeynArts`Truncated->True,FeynArts`MomentumConservation->True,FeynArts`PreFactor->1];
treeConverted=FeynCalc`FCFAConvert[treeRaw,FeynCalc`IncomingMomenta->{p1,p2},FeynCalc`OutgoingMomenta->{p3,p4},FeynCalc`ChangeDimension->D,List->True,FeynCalc`SMP->False,FeynCalc`DropSumOver->False];
treeNeed["amplitude count",Length[treeConverted]===Length[treeExpected]];
treeColour[e_]:=Module[{out=e,markers,index,rest,terms},
 out=out/.FeynArts`SumOver[_,_,FeynArts`External]->1;
 markers=DeleteDuplicates[Cases[out,_FeynArts`SumOver,Infinity]];
 Do[index=First[marker];rest=out/.marker->1;
  terms=If[Head[Expand[rest]]===Plus,List@@Expand[rest],{Expand[rest]}];
  If[!AllTrue[terms,Count[#,index,Infinity]===2&],Put[{marker,rest},FileNameJoin[{treeRun,"sum-failure.wl"}]];treeStop["internal colour sum is not an Einstein pair"]];
  out=rest,{marker,markers}];
 FeynCalc`SUNSimplify[out,FeynCalc`SUNNToCACF->False]];
treeConverted=treeColour/@treeConverted;
treeNeed["typed conversion",FreeQ[treeConverted,FeynArts`SumOver|FeynArts`IndexSum|Global`FASUNF|Global`FAFourVector|FeynArts`FANonCommutative|FeynCalc`DiracGamma]];
treePols=Times@@Flatten[MapIndexed[Function[{type,pos},With[{j=First[pos],offset=Total[(If[#=="T",2,1]&/@Take[treeTypes,First[pos]-1])]},Table[FeynCalc`FVD[Symbol["Global`e"<>ToString[j]],Symbol["Global`Lor"<>ToString[offset+k]]],{k,If[type=="T",2,1]}]]],treeTypes]];
treePolarised=FeynCalc`Contract[# FeynCalc`FCI[treePols]]&/@treeConverted;
treeNeed["all Lorentz indices contracted",FreeQ[treePolarised,FeynCalc`LorentzIndex]];
FeynCalc`FCClearScalarProducts[];
FeynCalc`SetMandelstam[s,t,u,p1,p2,-p3,-p4,0,0,0,0];
treePolarised=FeynCalc`ExpandScalarProduct[FeynCalc`FeynAmpDenominatorExplicit[#]]&/@treePolarised;
treeContact=Total[Pick[treePolarised,FreeQ[#,FeynCalc`FeynAmpDenominator]&/@treeConverted]];
treeAmplitude=Total[treePolarised];
treeNeed["coupling orders",Switch[treeCase,
 "VVVV",Expand[treeAmplitude-(treeAmplitude/.FeynGrav`\[Kappa]->0)-(treeAmplitude/.FeynGrav`YangMillsCoupling->0)]===0,
 "VVTT",Exponent[treeAmplitude,FeynGrav`\[Kappa]]===2&&FreeQ[treeAmplitude,FeynGrav`YangMillsCoupling],
 "VVVT",Exponent[treeAmplitude,FeynGrav`\[Kappa]]===1&&Exponent[treeAmplitude,FeynGrav`YangMillsCoupling]===1]];
treePhysical[e_,omit_:0]:=Module[{out,rules={},ep,pp},
 out=FeynCalc`ExpandScalarProduct[e/.FeynCalc`Momentum[p4,D]->FeynCalc`Momentum[p1+p2-p3,D]];
 Do[ep=FeynCalc`Momentum[Symbol["Global`e"<>ToString[j]],D];pp=FeynCalc`Momentum[Symbol["Global`p"<>ToString[j]],D];
 If[j!=omit,
  AppendTo[rules,If[j==4,FeynCalc`Pair[ep,FeynCalc`Momentum[p3,D]]->FeynCalc`Pair[ep,FeynCalc`Momentum[p1,D]]+FeynCalc`Pair[ep,FeynCalc`Momentum[p2,D]],FeynCalc`Pair[ep,pp]->0]];
  If[treeTypes[[j]]=="T",AppendTo[rules,FeynCalc`Pair[ep,ep]->0]]],{j,4}];
 out=out/.rules/.u->-s-t;
 out=Quiet[CalcFormConverter`FeynGravSimplify[out,Method->"FORM",CalcFormConverter`FORMThreads->1,FeynCalc`LoopMomenta->{},CalcFormConverter`TensorDecomposition->False,CalcFormConverter`KeepFiles->True,CalcFormConverter`WorkingDirectory->treeRun],{CalcFormConverter`CalcFormCalculate::files}];
 If[FailureQ[out],Put[out,FileNameJoin[{treeRun,"FORM-failure.wl"}]];treeStop["FORM failed"]];
 Together[Expand[out/.rules/.u->-s-t]]];
treeWard[e_,j_]:=Module[{ep=Symbol["Global`e"<>ToString[j]],pp=Symbol["Global`p"<>ToString[j]],out},
 If[treeTypes[[j]]=="V",out=e/.FeynCalc`Momentum[ep,D]->FeynCalc`Momentum[pp,D];treePhysical[out],
  out=FeynCalc`ExpandScalarProduct[((e/.FeynCalc`Momentum[ep,D]->FeynCalc`Momentum[ep+pp,D])-(e/.FeynCalc`Momentum[ep,D]->FeynCalc`Momentum[ep-pp,D]))/2];treePhysical[out,j]]];
Put[<|"Converted"->treeConverted,"PolarisedDiagrams"->treePolarised,"Contact"->treeContact,"Amplitude"->treeAmplitude|>,FileNameJoin[{treeRun,"before-ward.wl"}]];
treeResiduals=Table[treeRes=treeWard[treeAmplitude,j];
 Put[treeRes,FileNameJoin[{treeRun,"ward-"<>ToString[j]<>".wl"}]];
 treeNeed["Ward leg "<>ToString[j],treeRes===0];True,{j,4}];
treeNegative=Table[treeWard[treeAmplitude-treeContact,j],{j,4}];
Put[treeNegative,FileNameJoin[{treeRun,"missing-contact-residual.wl"}]];
treeNeed["missing contact fails Ward check",AnyTrue[treeNegative,#=!=0&]];
treeNeed["nonzero physical amplitude",treePhysical[treeAmplitude]=!=0];
Put[<|"Insertions"->treeInsertions,"Raw"->treeRaw,"PolarisedDiagrams"->treePolarised|>,FileNameJoin[{treeRun,"tree-evidence.wl"}]];
Export[FileNameJoin[{treeRun,"tree.json"}],<|"Process"->treeCase,"Channels"->treeChannels,"WardResidualsZero"->treeResiduals,"MissingContactRejected"->True,"Dimension"->"Symbolic D","GravitonWardVector"->"Arbitrary; no transversality imposed on tested gauge parameter"|>,"RawJSON"];
Close[treeDiag];Export[FileNameJoin[{treeRun,"completion.json"}],<|"Status"->"Completed","Passed"->treeCount,"Failed"->0|>,"RawJSON"];Quit[0],treeStop["abort"]];
