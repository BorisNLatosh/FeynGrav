$HistoryLength=0;$FeynCalcStartupMessages=False;$LoadAddOns={"FeynArts"};
(* Run with a fresh FA_QG_OUTPUT directory under the bounded launcher.
   The two vector classes are unit index probes, used only to transmit the four
   free graviton indices through CreateFeynAmp and FCFAConvert. *)
root=DirectoryName[$InputFileName];
run=Environment["FA_QG_OUTPUT"];
If[!StringQ[run] || !DirectoryQ[run],Print["Supply a fresh FA_QG_OUTPUT directory."];Quit[1]];
diag=OpenWrite[FileNameJoin[{run,"diagnostics.log"}]];$Messages={diag};count=0;
stop[s_]:=(Print["FAILED ",s];Quit[1]);
SetAttributes[need,HoldRest];need[s_,v_]:=If[TrueQ[Check[v,False]],count++;Print["PASS ",s],stop[s]];
Needs["FeynCalc`"];
AppendTo[FeynArts`$ModelPath,FileNameJoin[{root,"Fixtures"}]];
need["warning-free native model initialisation",FeynArts`InitializeModel["QGProbe",FeynArts`GenericModel->"QGProbe"]===True];
Print["versions ",$Version," ",FeynCalc`$FeynCalcVersion," ",FeynArts`$FeynArtsVersion];
inds={aa,bb,cc,dd};
wrapped=FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`T[1,p,{aa,bb}->{cc,dd}]];
Put[wrapped,FileNameJoin[{run,"line.wl"}]];
need["instantiated internal tensor line",SymbolName[Head[wrapped]]==="PV"&&Length[wrapped]===1];
Attributes[g]={Orderless};w[i_,j_]:=mom[i]mom[j]/x;t[i_,j_]:=g[i,j]-w[i,j];
native=First[wrapped]/.{FeynArts`FAPropagatorDenominator[p,m_,k_Integer]:>1/(x-m^2)^k,FeynArts`FAPropagatorDenominator[p,m_]:>1/(x-m^2),Global`FAFourVector[p,i_]:>mom[i],Global`FAMetricTensor[i_,j_]:>g[i,j]};
n=D-1;ms=3(D-2)qgM0^2 qgM2^2/((D-4)qgM0^2+2(D-1)qgM2^2);
(* Per free-index monomial: exact rational coefficient, retaining projector denominators. *)
vars=Join[mom/@inds,Flatten[Table[g[inds[[i]],inds[[j]]],{i,4},{j,i,4}]]];
zero[v_]:=AllTrue[Last/@CoefficientRules[Expand[v],vars],Together[#]===0&];
need["native denominator and tensor alphabet",FreeQ[native,_FeynArts`FAPropagatorDenominator|_Global`FAFourVector|_Global`FAMetricTensor]&&!FreeQ[native,qgEpsilon]];
(* Independent inverse quadratic form, normalised scalar coordinates theta/Sqrt[n], omega. *)
aR=(2/qgM2^2+1/qgM0^2)/3;bR=-2/qgM2^2;
k2=x+bR*x^2/2;
k0=-(D-2)x+(2 n aR+D bR/2)x^2;
block=DiagonalMatrix[{k0,0}]+x/qgEpsilon {{n,-Sqrt[n]},{-Sqrt[n],1}};
inv=Map[Together,Inverse[block],{2}];
spin2=(t[aa,cc]t[bb,dd]+t[aa,dd]t[bb,cc])/2-t[aa,bb]t[cc,dd]/n;
spin1=(t[aa,cc]w[bb,dd]+t[aa,dd]w[bb,cc]+t[bb,cc]w[aa,dd]+t[bb,dd]w[aa,cc])/2;
qgActionInverse=I(spin2/k2+spin1*qgEpsilon/(2x)+{t[aa,bb]/Sqrt[n],w[aa,bb]}.inv.{t[cc,dd]/Sqrt[n],w[cc,dd]});
need["inverse of action Hessian, including both longitudinal slots",zero[native-qgActionInverse]];
need["spin-2 and scalar kinetic roots",{Together[k2/.x->qgM2^2],Together[k0/.x->ms]}==={0,0}&&Together[(ms/.D->4)-qgM0^2]===0];
Get[FileNameJoin[{Nest[DirectoryName,root,3],"FeynGrav.wl"}]];
source=Block[{FeynGrav`GaugeFixingEpsilon=qgEpsilon},FCI[FeynGrav`QuadraticGravityPropagator[aa,bb,cc,dd,p,qgM0,qgM2]]];
den[FeynCalc`PropagatorDenominator[FeynCalc`Momentum[p,___],m_]]:=1/(x-m^2);
asPoly[e_]:=e/.{FeynCalc`FeynAmpDenominator[ds__]:>Times@@(den/@{ds}),FeynCalc`Pair[FeynCalc`LorentzIndex[i_,D],FeynCalc`LorentzIndex[j_,D]]:>g[i,j],FeynCalc`Pair[FeynCalc`LorentzIndex[i_,D],FeynCalc`Momentum[p,D]]:>mom[i],FeynCalc`Pair[FeynCalc`Momentum[p,D],FeynCalc`Momentum[p,D]]:>x};
need["actual public FeynGrav rule equals complete native line",zero[native-asPoly[source]]];
(* Laurent coefficients of the free tensor at fixed formal momentum components. *)
coeffAt[c_,0,k_]:=With[{regular=Cancel[x^3 c],j=3+k},Together[(D[regular,{x,j}]/j!)/.x->0]];
coeffAt[c_,pt_,-1]:=Together[Cancel[(x-pt)c]/.x->pt];
laurent[e_,point_,power_]:=Total[(coeffAt[Last[#],point,power] Times@@(vars^First[#]))&/@CoefficientRules[Expand[e],vars]];
spin0=(t[aa,bb]+n w[aa,bb])(t[cc,dd]+n w[cc,dd])/n;
need["massive spin-2 residue",zero[laurent[native,qgM2^2,-1]+I(spin2/.x->qgM2^2)]];
need["massive scalar residue",zero[laurent[native,ms,-1]-I(spin0/.x->ms)/(D-2)]];
Do[need["massless Laurent coefficient "<>ToString[k],zero[laurent[native,0,k]-laurent[qgActionInverse,0,k]]],{k,{-3,-2,-1}}];
need["opposite spin-2 sign rejected",!zero[native-qgActionInverse+2I spin2/(x-qgM2^2)]];
need["incorrect scalar mass rejected",!zero[I spin0/(D-2)(1/(x-ms)-1/(x-qgM0^2))]];
need["scalar mass at D=4",Together[(ms/.D->4)-qgM0^2]===0];
need["gauge choice epsilon=2",zero[(native-asPoly[source])/.qgEpsilon->2]];
(* Test complete FeynArts routing/conversion with two unit VVh probes. *)
ins=FeynArts`InsertFields[FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3}],{FeynArts`V[1],FeynArts`V[1]}->{FeynArts`V[2],FeynArts`V[2]},FeynArts`Model->"QGProbe",FeynArts`GenericModel->"QGProbe",FeynArts`InsertionLevel->{FeynArts`Classes}];
raw=FeynArts`CreateFeynAmp[ins,FeynArts`Truncated->True,FeynArts`MomentumConservation->False,FeynArts`PreFactor->1];Put[raw,FileNameJoin[{run,"tree.wl"}]];
need["single exchange of native tensor",Length[raw]===1];
tree=FCFAConvert[raw,IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},LoopMomenta->{p},ChangeDimension->D,List->False,SMP->False];
Put[tree,FileNameJoin[{run,"converted.wl"}]];
(* MomentumConservation -> False keeps this internal line independent. *)
need["sole independent internal momentum labelled p",DeleteDuplicates[Cases[tree,Momentum[y_,D]:>y,Infinity]]==={p}];
route=tree;
routed=Contract[route];
Put[routed,FileNameJoin[{run,"routed.wl"}]];
ext=SortBy[DeleteDuplicates[Cases[routed,LorentzIndex[y_,D]:>y,Infinity]],SymbolName];
need["four external index slots",(SymbolName/@ext)==={"Lor1","Lor2","Lor3","Lor4"}];
treePoly=asPoly[routed/.Thread[ext->inds]];
need["two unit vertices transmit full tensor through FCFAConvert",zero[treePoly-native]];
Close[diag];need["empty diagnostics",FileByteCount[FileNameJoin[{run,"diagnostics.log"}]]===0];
Export[FileNameJoin[{run,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"Checks"->count|>,"RawJSON"];Quit[0];
