Get[FileNameJoin[{DirectoryName[$InputFileName],"process-scaffold.wl"}]];
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
Print["TREE=",InputForm[FeynGrav`FeynArtsIntegration`FeynGravValidateProcess[s]]];
Print["SUBSET_CHECK=",SubsetQ[Keys[s],{"Model","Incoming","Outgoing","LoopOrder","OneParticleIrreducible"}]];
Quit[0];
