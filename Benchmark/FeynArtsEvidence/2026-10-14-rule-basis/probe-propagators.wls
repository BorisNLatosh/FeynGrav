$HistoryLength=0; Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Print["G=",InputForm[FeynGrav`GravitonPropagator[mu,nu,al,be,p]]];
Print["U=",InputForm[FeynCalc`FCI[FeynGrav`GhostVectorPropagator[mu,nu,p]]]];
Print["REC=",InputForm[FeynGrav`FeynGravLibraryInformation[FeynGrav`importGravitons]["Orders"]]];
Quit[0];
