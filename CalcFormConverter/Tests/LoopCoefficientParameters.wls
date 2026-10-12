Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,k,r];
(* Independent Gauss-Legendre quadrature of Feynman-parameter integrals.
   The Jacobi matrix determines nodes/weights; neither the converter's IBP
   recurrence nor FeynCalc tensor reduction participates in the reference.
   Mapping the simplex to a cube makes the massive Euclidean fixtures smooth.
   Compare two grid orders, with a time bound on each reference calculation. *)
gauss[order_] := gauss[order] = Module[{matrix, vals, vecs},
 matrix=Normal[SparseArray[{Band[{1,2}]->Table[j/Sqrt[4j^2-1],{j,order-1}],
     Band[{2,1}]->Table[j/Sqrt[4j^2-1],{j,order-1}]},{order,order}]];
 {vals,vecs}=Eigensystem[N[matrix]];
 {(vals+1)/2,vecs[[All,1]]^2}
];
parameter[ids_,inv_,ms_,pw_,order_] := Module[
 {s,labels,n=Length[ms],total=Total[pw],pref,xx,jac,delta,vars,pairs,t,w,remaining,weight},
 s=If[ids==={0},0,Count[ids,0]/2];labels=If[ids==={0},{},DeleteCases[ids,0]];
 pref=N[Pi^(-1/2)(-1)^(total-s+Length[labels])Gamma[total-s-3/2]/(2^s Times@@(Gamma/@pw))];
 If[n===1,Return[pref ms[[1]]^(3/2+s-total)]];
 {t,w}=gauss[order];vars=Transpose[Tuples[t,n-1]];
 weight=Times@@Transpose[Tuples[w,n-1]];
 remaining=ConstantArray[1.,Length[First[vars]]];xx={};
 Do[AppendTo[xx,remaining vars[[j]]];remaining*=1-vars[[j]];
    weight*=(1-vars[[j]])^(n-1-j),{j,n-1}];
 xx=Prepend[xx,remaining];
 pairs=Switch[n,2,{{1,2}},3,{{1,2},{2,3},{3,1}},4,{{1,2},{2,3},{3,4},{4,1},{1,3},{2,4}}];
 delta=Total[MapThread[Times,{xx,ms}]]-Sum[inv[[j]]xx[[pairs[[j,1]]]]xx[[pairs[[j,2]]]],{j,Length[pairs]}];
 weight*=delta^(3/2+s-total);
 Do[weight*=xx[[j]]^(pw[[j]]-1),{j,n}];
 Do[weight*=xx[[j+1]],{j,labels}];
 pref Total[weight]
];
Internal`InheritedBlock[{Pair},
 Pair[Momentum[p,D],Momentum[p,D]]=-1;Pair[Momentum[k,D],Momentum[k,D]]=-1;Pair[Momentum[r,D],Momentum[r,D]]=-1;
 Pair[Momentum[p,D],Momentum[k,D]]=0;Pair[Momentum[p,D],Momentum[r,D]]=0;Pair[Momentum[k,D],Momentum[r,D]]=0;
 Do[
 chords=Take[{0,p,k,r},n];ms=Take[{2,3,5,7},n];pw=Prepend[ConstantArray[1,n-1],2];
 inv=If[n===3,{-1,-2,-1},{-1,-2,-2,-1,-1,-2}];
 Do[
 input=LoopTensorCoefficient[ids,chords,ms,pw,D];
 result=CalcFormCalculate[input,ReduceLoopCoefficients->True,DropScaleless->True,FORMThreads->1,TimeConstraint->30];
 observations=Table[TimeConstrained[
  {N[result/.D->3/.PaVe[a___]:>With[{z=DeleteCases[{a},_Rule]},
    parameter[Drop[z,-2],z[[-2]],z[[-1]],ConstantArray[1,Length[z[[-1]]]],order]]],
   parameter[ids,inv,ms,pw,order]},30,$Aborted],{order,{20,32}}];
 assert["independent parameter topology "<>ToString[{n,ids}],!FailureQ[result]&&FreeQ[result,_LoopTensorCoefficient]&&
  FreeQ[observations,$Aborted]&&AllTrue[Flatten[observations],NumberQ]&&
  Abs[Subtract@@Last[observations]]<10^-9&&Max[Abs[Subtract@@observations]]<10^-9],
 {ids,{{0},{0,0},{1},{1,2}}}],{n,{3,4}}]
];
finish["Independent triangle/box parameters"];
