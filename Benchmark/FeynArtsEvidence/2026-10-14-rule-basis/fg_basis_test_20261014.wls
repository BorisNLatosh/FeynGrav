$HistoryLength=0;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/tmp/fg_sparse_basis_20261014.wl"];
FeynGrav`importGravitons[1];
ClearAll[check];
check[label_,e_,cap_] := Module[{x,t},
  t=AbsoluteTiming[x=TimeConstrained[FGBoundedBasis`ExtractVertex[e,cap],20,Failure["wall cap"]];][[1]];
  If[FailureQ[x],Print[label," FAILURE=",x," seconds=",t];Return[]];
  Print[label," basis=",x["TermCount"]," seconds=",t,
    " scalar=",And@@(FreeQ[#,FeynCalc`Pair[__]]&/@x["Coefficients"]),
    " exact=",TrueQ[Expand[x["Reconstruction"]-e]===0],
    " FAheads=",Tally[Head/@Cases[x["Monomials"]/.p:FeynCalc`Pair[__]:>FGBoundedBasis`FAAtom[p],
       _Global`FAMetricTensor|_Global`FAFourVector|_Global`FAScalarProduct,Infinity]]];
];
check["cubic",FeynGrav`GravitonVertex[aa,bb,qq,cc,dd,rr,ee,ff,ss],4000];
check["ghost",FeynGrav`GravitonGhostVertex[aa,bb,qq,cc,rr,dd,ss],4000];
