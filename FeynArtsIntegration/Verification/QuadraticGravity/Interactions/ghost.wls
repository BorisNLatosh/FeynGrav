(* Ordered ghost test. e e is the symmetric graviton polarisation; v is the
   ghost and u the antighost. Keep all three incoming momenta independent. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;
qgRoot=Environment["QG_RULE_ROOT"];qgOut=Environment["QG_OUTPUT"];
qgLog=OpenWrite[FileNameJoin[{qgOut,"diagnostics.log"}]];$Messages={qgLog};
Get[FileNameJoin[{qgRoot,"FeynGrav.wl"}]];
qgRequire[title_,v_]:=If[TrueQ[v],Print["PASS ",title],Print["FAIL ",title];Quit[1]];
SetAttributes[qgDot,Orderless];
qgRaw=FCI[FeynGrav`GravitonGhostVertex[mu,nu,k,ig,q,ia,r]];
qgProjected=qgRaw/.Pair->qgDot/.{LorentzIndex[mu,D]->e,LorentzIndex[nu,D]->e,LorentzIndex[ig,D]->v,LorentzIndex[ia,D]->u,Momentum[z_,D]:>z};
(* Contract the linear de Donder operator on L_v h, with the antighost derivative. *)
qgRef=I FeynGrav`\[Kappa] (qgDot[k,v] (qgDot[r,e] qgDot[u,e]-qgDot[r,u] qgDot[e,e]/2)
 +qgDot[e,v] (qgDot[r,q] qgDot[u,e]+qgDot[r,e] qgDot[u,q]-qgDot[r,u] qgDot[q,e]));
qgRequire["ordered cubic vertex",Expand[qgProjected-qgRef]===0];
qgRequire["overall sign control",Expand[qgProjected+qgRef]=!=0];
qgRequire["ghost and antighost exchange control",Expand[qgProjected-(qgRef/.{v->u,u->v,q->r,r->q})]=!=0];
qgTrace=Contract[qgRaw MTD[mu,nu]]/.Pair->qgDot/.{LorentzIndex[ig,D]->v,LorentzIndex[ia,D]->u,Momentum[z_,D]:>z};
qgTraceRef=I FeynGrav`\[Kappa](qgDot[k,v](1-D/2)qgDot[r,u]+qgDot[r,q]qgDot[u,v]+qgDot[r,v]qgDot[u,q]-qgDot[r,u]qgDot[q,v]);
qgRequire["generic D trace",Expand[qgTrace-qgTraceRef]===0&&Coefficient[qgTraceRef,D]=!=0];
qgRequire["free ghost convention",FCI[FeynGrav`GhostVectorPropagator[ig,ia,q]+I MTD[ig,ia] FAD[q]]===0];
Close[qgLog];qgRequire["empty diagnostics",FileByteCount[FileNameJoin[{qgOut,"diagnostics.log"}]]===0];
Export[FileNameJoin[{qgOut,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"Checks"->6|>,"RawJSON"];Quit[0];
