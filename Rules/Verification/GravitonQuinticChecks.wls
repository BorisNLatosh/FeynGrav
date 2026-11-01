(* Independent Einstein-Hilbert vertex checks. Test-only, no production load.
   Run each suite in a fresh, externally bounded kernel; this is not a launcher.
   See README.md and the dated verification report for scope and conventions. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;
v02Root=Environment["FEYNGRAV_RULE_AUDIT_ROOT"];
If[!StringQ[v02Root]||v02Root==="",v02Root=DirectoryName[$InputFileName,3]];
v02Run=Environment["FEYNGRAV_RULE_AUDIT_OUTPUT"];
v02Suite=Environment["FEYNGRAV_GR_CHECK_SUITE"];
If[!StringQ[v02Suite]||v02Suite==="",v02Suite="Quintic"];
If[!MemberQ[{"Lower","Quintic","Trace"},v02Suite],Print["Unknown GR check suite"];Quit[2]];
If[!StringQ[v02Run]||!DirectoryQ[v02Run],Quit[2]];
If[AnyTrue[{"diagnostics.log","checks.json","expected-assertions.json","completion.json",
 "lower-results.wxf","quintic-reference.wxf","comparison.json","traces.wxf","trace-summary.json"},
 FileExistsQ[FileNameJoin[{v02Run,#}]]&],Print["Refusing to overwrite existing verification evidence"];Quit[2]];
v02Messages=OpenWrite[FileNameJoin[{v02Run,"diagnostics.log"}]];$Messages={v02Messages};
v02Checks={};v02Expected={};
v02Stop[why_]:=(Print["STOP ",why];Export[FileNameJoin[{v02Run,"checks.json"}],v02Checks,"RawJSON"];
 Export[FileNameJoin[{v02Run,"completion.json"}],<|"Status"->"Failed","Passed"->0,"Failed"->1,"Reason"->ToString[why]|>,"RawJSON"];Quit[2]);
SetAttributes[v02Guard,HoldAll];
v02Guard[x_]:=Module[{out=CheckAbort[Check[x,$Failed],$Aborted]},If[!FreeQ[out,_Failure|$Failed|$Aborted],v02Stop["Unexpected diagnostic or unsuccessful evaluation"]];out];
v02Assert[label_,condition_]:=(AppendTo[v02Checks,<|"Name"->label,"Passed"->TrueQ[condition]|>];
 Export[FileNameJoin[{v02Run,"checks.json"}],v02Checks,"RawJSON"];Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],v02Stop[label]]);
v02Declare[names_List]:=(v02Expected=names;If[!DuplicateFreeQ[names],v02Stop["Duplicate names"]];Export[FileNameJoin[{v02Run,"expected-assertions.json"}],names,"RawJSON"]);
v02Finish[]:=(Close[v02Messages];If[FileByteCount[FileNameJoin[{v02Run,"diagnostics.log"}]]>0,v02Stop["Unexpected messages"]];
 If[Lookup[v02Checks,"Name"]=!=v02Expected,v02Stop["Assertion ledger mismatch"]];
 Export[FileNameJoin[{v02Run,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"RawAssertions"->Length[v02Checks]|>,"RawJSON"];Quit[0]);
v02Guard[Get["FeynCalc`"]];
v02Guard[Get[FileNameJoin[{v02Root,"Rules","Verification","GravitonQuinticReference.wl"}]]];
v02Es[n_]:=Table[Symbol["v02E"<>ToString[j]],{j,n}];
v02Ps[n_]:=Table[Symbol["p"<>ToString[j]],{j,n}];
v02Mu[n_]:=Table[Symbol["m"<>ToString[j]],{j,n}];
v02Nu[n_]:=Table[Symbol["n"<>ToString[j]],{j,n}];
v02Raw[n_]:=Get[FileNameJoin[{v02Root,"Libs","GravitonVertex_"<>ToString[n-2]}]];
v02Project[expr_,n_]:=Expand[expr/.Pair->EHScalarProduct/.
 Join[Thread[(LorentzIndex[#,D]& /@v02Mu[n])->v02Es[n]],Thread[(LorentzIndex[#,D]& /@v02Nu[n])->v02Es[n]],Thread[(Momentum[#,D]& /@v02Ps[n])->v02Ps[n]]]];

Switch[v02Suite,
"Lower",
v02Declare[{"volume-square","inverse-identity","reference-no-production-dependencies","cubic-equality","cubic-nonzero","cubic-wrong-sign","quartic-equality","quartic-nonzero","quartic-wrong-factor","Ricci-bulk-boundary-cubic","symbolic-D-unassigned"}];
v02Guard[
 es=v02Es[3];v=EHVolumeCoefficients[es];
 det=Prepend[Table[ls=Pick[Range[3],Table[BitGet[m,j-1],{j,3}],1];Det[Outer[EHScalarProduct,es[[ls]],es[[ls]]]],{m,1,7}],1];
 v02Assert["volume-square",Expand[EHJetMultiply[v,v]-det]===ConstantArray[0,8]];
 ip=EHInverseCoefficients[v02A,v02B,es]+Total[Table[unit=UnitVector[8,2^(j-1)+1];EHJetMultiply[unit,EHInverseCoefficients[v02A,es[[j]],es]]EHScalarProduct[es[[j]],v02B],{j,3}]];
 v02Assert["inverse-identity",Expand[ip-UnitVector[8,1]EHScalarProduct[v02A,v02B]]===ConstantArray[0,8]];
 v02Assert["reference-no-production-dependencies",!MemberQ[$Packages,"CTensorGeneral`"]&&!MemberQ[$Packages,"GammaTensor`"]&&!MemberQ[$Packages,"GravitonVertex`"]];
 Print["BEGIN cubic ",AbsoluteTime[]];r3=EHVertex[v02Es[3],v02Ps[3]];a3=v02Project[v02Raw[3],3];
 v02Assert["cubic-equality",Expand[a3-r3]===0];
 v02Assert["cubic-nonzero",r3=!=0];v02Assert["cubic-wrong-sign",Expand[a3+r3]=!=0];
 Print["BEGIN quartic ",AbsoluteTime[]];r4=EHVertex[v02Es[4],v02Ps[4]];a4=v02Project[v02Raw[4],4];
 v02Assert["quartic-equality",Expand[a4-r4]===0];v02Assert["quartic-nonzero",r4=!=0];v02Assert["quartic-wrong-factor",Expand[a4-r4/2]=!=0];
 (* The common independent geometry reference constructs Ricci directly, before
    integrating by parts. Only this separate boundary control uses conservation. *)
 Get[FileNameJoin[{v02Root,"Rules","Verification","HorndeskiReference.wl"}]];
 hgSetup[v02Es[3],v02Ps[3]];
 direct=-2 I Global`\[Kappa] Expand[Last[hgMul[hgVolume,hgTrace[hgRicci]]]]/.hgSP->EHScalarProduct;
 delta=Expand[(direct-r3)/.EHScalarProduct->SPD/.p3->-p1-p2];
 v02Assert["Ricci-bulk-boundary-cubic",Expand[ExpandScalarProduct[delta]]===0];
 v02Assert["symbolic-D-unassigned",OwnValues[D]==={}];
 Export[FileNameJoin[{v02Run,"lower-results.wxf"}],<|"Cubic"->r3,"Quartic"->r4,"DirectRicciCubic"->direct|>,"WXF"];
],

"Quintic",
v02Declare[Join[{"quintic-full-equality","quintic-nonzero","quintic-wrong-sign","quintic-wrong-normalisation","quintic-altered-coefficient","no-uncontracted-objects"},Table["stored-pair-symmetry-"<>ToString[j],{j,5}],Table["stored-leg-symmetry-"<>ToString[j],{j,4}],Table["auxiliary-degree-two-"<>ToString[j],{j,5}],{"momentum-degree-two","kappa-degree-three","symbolic-D-unassigned"}]];
v02Guard[
 Print["BEGIN independent quintic ",AbsoluteTime[]];
 ref=EHVertex[v02Es[5],v02Ps[5]];
 Print["BEGIN stored quintic ",AbsoluteTime[]];raw=v02Raw[5];actual=v02Project[raw,5];
 v02Assert["quintic-full-equality",Expand[actual-ref]===0];
 v02Assert["quintic-nonzero",ref=!=0];
 v02Assert["quintic-wrong-sign",Expand[actual+ref]=!=0];
 v02Assert["quintic-wrong-normalisation",Expand[actual-ref/2]=!=0];
 v02Assert["quintic-altered-coefficient",Expand[actual-(ref+First[Expand[ref]])]=!=0];
 v02Assert["no-uncontracted-objects",FreeQ[actual,_Pair|_LorentzIndex|_Momentum]];
 Do[v02Assert["stored-pair-symmetry-"<>ToString[j],(raw/.{v02Mu[5][[j]]->v02Nu[5][[j]],v02Nu[5][[j]]->v02Mu[5][[j]]})===raw],{j,5}];
 Do[per=Range[5];per[[{j,j+1}]]=per[[{j+1,j}]];
   rules=Join[Thread[v02Mu[5]->v02Mu[5][[per]]],Thread[v02Nu[5]->v02Nu[5][[per]]],Thread[v02Ps[5]->v02Ps[5][[per]]]];
   v02Assert["stored-leg-symmetry-"<>ToString[j],(raw/.rules)===raw],{j,4}];
 (* Count vector occurrences in every monomial, not finite-dimensional components. *)
 Do[degree=Exponent[ref/.EHScalarProduct[a_,b_]:>v02Scale^(Count[{a,b},v02Es[5][[j]]])EHScalarProduct[a,b],v02Scale,List];
  v02Assert["auxiliary-degree-two-"<>ToString[j],degree==={2}],{j,5}];
 degree=Exponent[ref/.EHScalarProduct[a_,b_]:>v02Scale^(Count[{a,b},Alternatives@@v02Ps[5]])EHScalarProduct[a,b],v02Scale,List];
 v02Assert["momentum-degree-two",degree==={2}];
 v02Assert["kappa-degree-three",Exponent[ref,Global`\[Kappa],List]==={3}];
 v02Assert["symbolic-D-unassigned",OwnValues[D]==={}];
 Export[FileNameJoin[{v02Run,"quintic-reference.wxf"}],ref,"WXF"];
 Export[FileNameJoin[{v02Run,"comparison.json"}],<|"StoredTerms"->Length[raw],"ProjectedTerms"->Length[actual],"ReferenceTerms"->Length[Expand[ref]],"Residual"->0,"MomentumConservationUsed"->False,"DimensionSpecialised"->False|>,"RawJSON"];
],

"Trace",
v02Declare[{"trace-metric-is-D","trace-square","trace-mixed","quintic-trace-full-equality","quintic-trace-D-dependent","quintic-trace-wrong-D","quintic-double-trace-full-equality","quintic-double-trace-D-dependent","symbolic-D-unassigned","scalar-products-unassigned"}];
(* Two independent trace implementations: identify the two slots in the stored
   tensor; on the reference, use one half of the formal vector Laplacian. *)
v02StoredTrace[x_,z_]:=x/.EHScalarProduct[z,z]->D/.
 Power[EHScalarProduct[z,a_],2]:>EHScalarProduct[a,a]/.
 EHScalarProduct[z,a_] EHScalarProduct[z,b_]:>EHScalarProduct[a,b];
v02ReferenceTrace[x_,z_,vectors_List]:=Module[{xs=Table[Unique["trace"],{Length[vectors]}],q=Unique["norm"],p,out},
 p=x/.Join[{EHScalarProduct[z,z]->q},Thread[(EHScalarProduct[z,#]& /@vectors)->xs]];
 out=D Coefficient[p,q,1]+Sum[EHScalarProduct[vectors[[a]],vectors[[a]]] D[p,{xs[[a]],2}]/2,{a,Length[xs]}]+
  Sum[EHScalarProduct[vectors[[a]],vectors[[b]]]D[p,xs[[a]],xs[[b]]],{a,1,Length[xs]},{b,a+1,Length[xs]}];
 Expand[out/.Thread[Join[xs,{q}]->0]]];
v02Guard[
 v02Assert["trace-metric-is-D",v02StoredTrace[EHScalarProduct[z,z],z]===D];
 v02Assert["trace-square",v02StoredTrace[EHScalarProduct[z,a]^2,z]===EHScalarProduct[a,a]];
 v02Assert["trace-mixed",v02StoredTrace[EHScalarProduct[z,a]EHScalarProduct[z,b],z]===EHScalarProduct[a,b]];
 ref=EHVertex[v02Es[5],v02Ps[5]];raw=v02Raw[5];
 (* Keep the fifth pair unprojected until its indices are identified. *)
 maps=Join[Thread[(LorentzIndex[#,D]& /@v02Mu[4])->v02Es[4]],Thread[(LorentzIndex[#,D]& /@v02Nu[4])->v02Es[4]],Thread[(Momentum[#,D]& /@v02Ps[5])->v02Ps[5]],{LorentzIndex[m5,D]->z,LorentzIndex[n5,D]->z}];
 trActual=Expand[v02StoredTrace[raw/.Pair->EHScalarProduct/.maps,z]];
 trRef=v02ReferenceTrace[ref,Last[v02Es[5]],Join[v02Es[4],v02Ps[5]]];
 v02Assert["quintic-trace-full-equality",Expand[trActual-trRef]===0];
 v02Assert["quintic-trace-D-dependent",!FreeQ[trRef,D]&&PolynomialQ[trRef,D]];
 v02Assert["quintic-trace-wrong-D",Expand[trActual-(trRef/.D->4)]=!=0];
 tr2Actual=Expand[v02StoredTrace[trActual,v02Es[4][[4]]]];
 tr2Ref=v02ReferenceTrace[trRef,v02Es[4][[4]],Join[v02Es[3],v02Ps[5]]];
 v02Assert["quintic-double-trace-full-equality",Expand[tr2Actual-tr2Ref]===0];
 v02Assert["quintic-double-trace-D-dependent",!FreeQ[tr2Ref,D]&&PolynomialQ[tr2Ref,D]];
 v02Assert["symbolic-D-unassigned",OwnValues[D]==={}];
 v02Assert["scalar-products-unassigned",DownValues[EHScalarProduct]==={}&&And@@(OwnValues[#]==={}& /@v02Ps[5])];
 Export[FileNameJoin[{v02Run,"traces.wxf"}],<|"SingleTrace"->trRef,"DoubleTrace"->tr2Ref|>,"WXF"];
 Export[FileNameJoin[{v02Run,"trace-summary.json"}],<|"SingleTraceTerms"->Length[trRef],"DoubleTraceTerms"->Length[tr2Ref],"SingleTraceDegreeD"->Exponent[trRef,D],"DoubleTraceDegreeD"->Exponent[tr2Ref,D],"Residual"->0|>,"RawJSON"];
]
];
v02Finish[];
