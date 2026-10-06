$FeynCalcStartupMessages=False;
Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
Get[FileNameJoin[{DirectoryName[DirectoryName[$testRoot]], "Rules", "GravitonSUNYM.wl"}]];
$PrePrint=.;
checkExpression[label_,x_]:=Module[{y,residual},
 y=CalcFormCalculate[x,FORMThreads->1,TimeConstraint->60,ColourAlgebra->Automatic];
 residual=If[FailureQ[y],y,TimeConstrained[Simplify[Contract[DiracSimplify[SUNSimplify[Contract[ExpandScalarProduct[DotSimplify[FCI[y-x]]]] /. SUNTrace[z_,___] :> SUNTrace[z],SUNNToCACF->False,Explicit->True,SUNTraceEvaluate->False], DiracOrder -> True]]],60,$TimedOut]];
 assert[label,residual===0];Print[label," : ",If[residual===0,"PASS",InputForm[Short[residual,3]]]]
];
Block[{RuleValidation`RuleParallelMap},RuleValidation`RuleParallelMap[f_,xs_,___]:=RuleValidation`RuleRequire[Map[f,xs]];
Do[
 idx=Take[{aa,bb,cc,dd},2n];legs=Take[{aa,bb,k1,cc,dd,k2},3n];
 fermion=GravitonFermionVertex`GravitonFermionVertexUncontracted[legs,p1,p2,m];
 checkExpression["fermion "<>ToString[n],fermion];
 quark=GravitonSUNYM`GravitonQuarkGluonVertexUncontracted[idx,{mu,ca}];
 If[n>0,assert["dimensional quark "<>ToString[n],AssociationQ[CalcFormExport[quark,FileNameJoin[{testDirectory,"colour-dimensional"<>ToString[n]<>".frm"}]]]]];
 checkExpression["quark dimensional "<>ToString[n],quark];
 checkExpression["two gluons "<>ToString[n],GravitonSUNYM`GravitonGluonVertexUncontracted[legs,p1,mu,ca,p2,nu,cb,eps]];
 checkExpression["three gluons "<>ToString[n],GravitonSUNYM`GravitonThreeGluonVertex[idx,p1,mu,ca,p2,nu,cb,p3,rho,cC]];
 checkExpression["four gluons "<>ToString[n],GravitonSUNYM`GravitonFourGluonVertexUncontracted[idx,p1,mu,ca,p2,nu,cb,p3,rho,cC,p4,sigma,cd]];
 checkExpression["YM ghosts "<>ToString[n],GravitonSUNYM`GravitonYMGhostVertexUncontracted[idx,p1,ca,p2,cb]];
 checkExpression["gluon ghosts "<>ToString[n],GravitonSUNYM`GravitonGluonGhostVertexUncontracted[idx,{p1,mu,ca},{p2,nu,cb},{p3,rho,cC}]];
,{n,0,2}]];
finish["ColourRules"];
