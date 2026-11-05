(* Serialize the four actual native amplitudes. Separate multiplicative factors
   preserve internal index contractions without expanding products of vertices. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;$LoadAddOns={"FeynArts"};
work=Environment["QG_OUTPUT"]; model=Environment["QG_MODEL"];root=Environment["QG_RULE_ROOT"];
If[!MemberQ[{"GR","QuadraticGravity"},model] || !StringQ[work] || !DirectoryQ[work] ||
 AnyTrue[{"diagnostics.log","amplitudes.json","completion.json"},FileExistsQ[FileNameJoin[{work,#}]]&],
 Print["Supply GR or QuadraticGravity and a fresh QG_OUTPUT directory."];Quit[2]];
log=OpenWrite[FileNameJoin[{work,"diagnostics.log"}]];$Messages={log};
stop[msg_]:=(Print["STOP ",msg];Quit[1]);
Needs["FeynCalc`"];
PrependTo[FeynArts`$ModelPath,FileNameJoin[{root,"FeynArts","Models",model}]];
If[FeynArts`InitializeModel[model,FeynArts`GenericModel->model]=!=True,stop["model loading"]];
ins=FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3,4}],{FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},FeynArts`Model->model,FeynArts`GenericModel->model,FeynArts`InsertionLevel->{FeynArts`Classes}];
raw=FeynArts`CreateFeynAmp[ins,FeynArts`Truncated->True,FeynArts`PreFactor->1,FeynArts`MomentumConservation->True,FeynArts`GaugeRules->{}];
If[Length[raw]=!=4,stop["diagram inventory"]];
(* Route coefficients refer to all-incoming physical momenta k1..k4. *)
route[FeynArts`FourMomentum[FeynArts`Incoming,j_]]:=UnitVector[4,j];
route[FeynArts`FourMomentum[FeynArts`Outgoing,j_]]:=-UnitVector[4,j+2];
route[s_Plus]:=Total[route/@List@@s];route[n_Integer m_]:=n route[m];
idx[FeynArts`Index[_,j_Integer]]:=j;
mass[z_]:=Which[z===0,0,Together[z^2-QuadraticGravityModel`M2^2]===0,2,Together[(z^2/.D->4)-QuadraticGravityModel`M0^2]===0,1,True,stop["unknown pole"]];
Clear[atom];
atom[Global`FAMetricTensor[i_,j_]]:={"g",idx[i],idx[j]};
atom[Global`FAFourVector[p_,i_]]:={"v",route[p],idx[i]};
atom[Global`FAScalarProduct[p_,q_]]:={"s",route[p],route[q]};
atom[FeynArts`FAPropagatorDenominator[p_,m_,k_:1]]:={"d",route[p],mass[m],k};
atom[FeynArts`FAGaugeXi[FeynArts`T[1]]]:={"gauge"};
atom[QuadraticGravityModel`M0]:={"m0"};atom[QuadraticGravityModel`M2]:={"m2"};
atom[z_Integer]:={"q",z,1};atom[z_Rational]:={"q",Numerator[z],Denominator[z]};
atom[z_Complex]/;Re[z]===0:={"imag",Numerator[Im[z]],Denominator[Im[z]]};
(* Assign each distinct subexpression one local node. Children are numbered first. *)
node[z_]:=node[z]=Module[{op=Head[z],children,row,a},
 row=Which[MemberQ[{Plus,Times,FeynArts`FAFeynAmpDenominator},op],
   children=node/@List@@z;Prepend[children,If[op===Plus,"add","mul"]],
   op===Power&&IntegerQ[z[[2]]],{"pow",node[z[[1]]],z[[2]]},
   True,a=atom[z];If[!ListQ[a]||!FreeQ[a,_route|_idx],Print["unhandled ",InputForm[z]];stop["unsupported atom"]];a];
 Sow[row];++number];
number=0;
{roots,{nodes}}=Reap[Table[Module[{expr=raw[[j,3]]/.{D->4,FeynGrav`\[Kappa]->1}},
 node/@If[Head[expr]===Times,List@@expr,{expr}]],{j,4}]];
Export[FileNameJoin[{work,"amplitudes.json"}],<|"Model"->model,"Nodes"->nodes,"Factors"->roots,"Convention"->"all incoming, kappa=1, D=4"|>,"RawJSON"];
Print["MODEL ",model," GRAPHS ",Length[roots]," NODES ",number," FACTORS ",Length/@roots];
Close[log];If[FileByteCount[FileNameJoin[{work,"diagnostics.log"}]]!=0,stop["diagnostics"]];
Export[FileNameJoin[{work,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0|>,"RawJSON"];Quit[0];
