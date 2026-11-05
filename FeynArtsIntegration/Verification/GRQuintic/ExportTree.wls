(* In a fresh kernel, record native graph factors before any component arithmetic.
   Keep the two expensive quartic-vertex constructions in separate bounded runs. *)
$HistoryLength=0; $FeynCalcStartupMessages=False; $LoadAddOns={"FeynArts"};
out=Environment["GR5_OUTPUT"]; model=Environment["GR5_MODEL"]; suite=Environment["GR5_EXPORT"];
If[!DirectoryQ[out] || !DirectoryQ[model] || FileNames["*",out]=!={} ||
   !MemberQ[{"Templates","Controls","Contact"},suite], Print["Supply an empty GR5_OUTPUT, GR5_MODEL and GR5_EXPORT."];Quit[2]];
log=OpenWrite[FileNameJoin[{out,"diagnostics.log"}]]; $Messages={log};
need[label_,ok_]:=If[!TrueQ[ok],Print["FAIL ",label];Quit[1]];
Get["FeynCalc`"]; PrependTo[FeynArts`$ModelPath,model];
need["model initialization",FeynArts`InitializeModel["GR",FeynArts`GenericModel->"GR"]===True];
top=FeynArts`CreateTopologies[0,2->3,FeynArts`Adjacencies->{3,4,5}];
ins=FeynArts`InsertFields[top,ConstantArray[FeynArts`T[1],2]->ConstantArray[FeynArts`T[1],3],
  FeynArts`Model->"GR",FeynArts`GenericModel->"GR",FeynArts`InsertionLevel->{FeynArts`Classes}];
need["26 full insertions",Length[top]===26&&Length[ins]===26];
graphs=Table[With[{t=First[ins[[j]]]},<|"Position"->j,"WeightDenominator"->First[Head[t]],
  "Edges"->Cases[t,FeynArts`Propagator[k_][FeynArts`Vertex[a_][b_],FeynArts`Vertex[c_][d_],___]:>{ToString[k],a,b,c,d},Infinity]|>],{j,26}];
need["unit factors",AllTrue[graphs,#["WeightDenominator"]===1&]];
Export[FileNameJoin[{out,"graphs.json"}],graphs,"RawJSON"];

(* The zero-based slot blocks are the first tensor index, second tensor index,
   and incoming momentum on each leg. Store the exact polynomial times 16. *)
slot[FeynArts`Index[_,i_Integer]]:=If[OddQ[i],Quotient[i-1,2],5+Quotient[i-2,2]];
mom[FeynArts`FourMomentum[FeynArts`Incoming,i_Integer]]:={1,9+i};
mom[FeynArts`FourMomentum[FeynArts`Outgoing,i_Integer]]:={-1,11+i};
mom[-q_]:=With[{a=mom[q]},{-First[a],Last[a]}];
tag[i_,j_]:=tag[i,j]=Symbol["Global`grZ"<>ToString[Min[i,j]]<>"x"<>ToString[Max[i,j]]];
Do[pairCode[tag[i,j]]=15i+j,{i,0,14},{j,i,14}];
vector[p_,i_]:=vector[p,i]=With[{a=mom[p]},First[a] tag[Last[a],slot[i]]];
scalar[p_,q_]:=scalar[p,q]=With[{a=mom[p],b=mom[q]},First[a] First[b] tag[Last[a],Last[b]]];
row[term_]:=Module[{f=List@@term,n,ids},n=16 Times@@Select[f,NumberQ];ids=pairCode/@Select[f,!NumberQ[#]&];
  need["complete monomial",Length[ids]===6&&VectorQ[ids,IntegerQ]&&IntegerQ[n]];Prepend[Sort[ids],n]];
If[suite=!="Controls",
  contact=Select[ins,!FreeQ[#,FeynArts`Vertex[5][_]]&];need["one quintic",Length[contact]===1];
  {sec,raw}=AbsoluteTiming[FeynArts`CreateFeynAmp[contact,FeynArts`PreFactor->1,FeynArts`Truncated->True,FeynArts`MomentumConservation->False]];
  Print["Quintic native seconds ",sec];
  (* Keep kappa outside the slot translation, as in the generic source comparison. *)
  {sec,poly}=AbsoluteTiming[Expand[raw[[1,3]]/.{Global`FAMetricTensor[a_,b_]:>tag[slot[a],slot[b]],
    Global`FAFourVector[p_,i_]:>vector[p,i],Global`FAScalarProduct[p_,q_]:>scalar[p,q]}]];
  Print["Quintic slots seconds ",sec];Clear[raw];
  poly=Expand[-I(poly/.FeynGrav`\[Kappa]->1)];need["full contact",Length[poly]===76230];
  (* Evaluate the polynomial once before mapping; never reinsert it into a per-term check. *)
  {sec,rows}=AbsoluteTiming[row/@(List@@poly)];Clear[poly];Print["Quintic coefficient table seconds ",sec];
  Export[FileNameJoin[{out,"contact.csv"}],rows,"CSV"];Clear[rows];
  If[suite==="Contact",Close[log];need["empty diagnostics",FileByteCount[FileNameJoin[{out,"diagnostics.log"}]]===0];Quit[0]]];

(* The factors of an exchange remain separate. Every denominator and relative
   phase comes from FeynArts. Number shared arithmetic nodes after their children. *)
route[FeynArts`FourMomentum[FeynArts`Incoming,j_]]:=UnitVector[5,j];
route[FeynArts`FourMomentum[FeynArts`Outgoing,j_]]:=-UnitVector[5,j+2];
route[s_Plus]:=Total[route/@List@@s];route[n_Integer m_]:=n route[m];
idx[FeynArts`Index[_,j_Integer]]:=j;
atom[Global`FAMetricTensor[i_,j_]]:={"g",idx[i],idx[j]};
atom[Global`FAFourVector[p_,i_]]:={"v",route[p],idx[i]};
atom[Global`FAScalarProduct[p_,q_]]:={"s",route[p],route[q]};
atom[FeynArts`FAPropagatorDenominator[p_,0,k_:1]]:={"d",route[p],0,k};
atom[z_Integer]:={"q",z,1};atom[z_Rational]:={"q",Numerator[z],Denominator[z]};
atom[z_Complex]/;Re[z]===0:={"imag",Numerator[Im[z]],Denominator[Im[z]]};
Do[
  {sec,raw}=AbsoluteTiming[FeynArts`CreateFeynAmp[Take[ins,{id}],FeynArts`Truncated->True,
    FeynArts`PreFactor->1,FeynArts`MomentumConservation->True,FeynArts`GaugeRules->{}]];
  need["one native graph",Length[raw]===1];Clear[node];number=0;
  node[z_]:=node[z]=Module[{op=Head[z],children,record},
    record=Which[MemberQ[{Plus,Times,FeynArts`FAFeynAmpDenominator},op],
      children=node/@List@@z;Prepend[children,If[op===Plus,"add","mul"]],
      op===Power&&IntegerQ[z[[2]]],{"pow",node[z[[1]]],z[[2]]},
      True,With[{a=atom[z]},need["decoded native atom",ListQ[a]&&FreeQ[a,_idx|_route]];a]];
    Sow[record];++number];
  expr=raw[[1,3]]/.{D->4,FeynGrav`\[Kappa]->1};
  {roots,{nodes}}=Reap[node/@If[Head[expr]===Times,List@@expr,{expr}]];
  Export[FileNameJoin[{out,"graph-"<>ToString[id]<>".json"}],
     <|"ID"->id,"Nodes"->nodes,"Factors"->roots,"Seconds"->sec|>,"RawJSON"];
  Print["GRAPH ",id," seconds ",sec];Clear[raw,expr,nodes,roots],
  {id,If[suite==="Templates",{2,14},{3,15}]}];
Close[log];need["empty diagnostics",FileByteCount[FileNameJoin[{out,"diagnostics.log"}]]===0];Quit[0];
