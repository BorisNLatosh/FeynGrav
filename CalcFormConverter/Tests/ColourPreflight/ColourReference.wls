$FeynCalcStartupMessages=False;
Needs["FeynCalc`"];
$PrePrint=.;
cases={
 {"fundamentalLoop",SUNFDelta[u,u]-SUNN},
 {"adjointLoop",SUNDelta[a,a]-(SUNN^2-1)},
 {"fundamentalDelta",SUNFDelta[u,v] SUNFDelta[v,w]-SUNFDelta[u,w]},
 {"adjointDelta",SUNDelta[a,b] SUNDelta[b,c]-SUNDelta[a,c]},
 {"oneTrace",SUNTrace[SUNT[a]]},
 {"twoTrace",SUNTrace[SUNT[a,b]]-SUNDelta[a,b]/2},
 {"casimir",SUNTF[{a,a},u,v]-(SUNN^2-1)/(2 SUNN) SUNFDelta[u,v]},
 {"sandwich",SUNTF[{a,b,a},u,v]+SUNTF[{b},u,v]/(2 SUNN)},
 {"completeness",SUNTF[{a},u,v] SUNTF[{a},w,x]-(SUNFDelta[u,x] SUNFDelta[v,w]-SUNFDelta[u,v] SUNFDelta[w,x]/SUNN)/2},
 {"structureConstants",SUNF[a,c,d] SUNF[b,c,d]-SUNN SUNDelta[a,b]},
 {"symmetricConstants",SUND[a,c,d] SUND[b,c,d]-(SUNN^2-4)/SUNN SUNDelta[a,b]},
 {"mixedConstants",SUNF[a,c,d] SUND[b,c,d]},
 {"traceCommutator",SUNTrace[SUNT[a,b,c]]-SUNTrace[SUNT[b,a,c]]-I SUNF[a,b,c]/2},
 {"chainJoin",SUNTF[{a},u,v] SUNTF[{b},v,w]-SUNTF[{a,b},u,w]},
 {"chainClosure",SUNTF[{a},u,v] SUNTF[{b},v,u]-SUNDelta[a,b]/2},
 {"traceProduct",SUNTrace[SUNT[a,b]] SUNTrace[SUNT[a,b]]-(SUNN^2-1)/4},
 {"tracePower",SUNTrace[SUNT[a,b]]^2-(SUNN^2-1)/4},
 {"cyclicTrace",SUNTrace[SUNT[a,b,c,d]]-SUNTrace[SUNT[c,d,a,b]]}
};
results=Map[Function[test,With[{residual=CheckAbort[TimeConstrained[Simplify[SUNSimplify[FCI[test[[2]]] /. SUNTrace[z_] :> SUNTrace[z,SUNTraceEvaluate->True],SUNNToCACF->False]],60,$TimedOut],$Aborted]},Print[test[[1]],"=",InputForm[residual]];residual===0]],cases];
Print["FeynCalcVersion=",$FeynCalcVersion];
If[And@@results,Exit[0],Exit[1]];
