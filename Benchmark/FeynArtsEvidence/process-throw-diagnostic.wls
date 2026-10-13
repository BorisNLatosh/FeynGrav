Get[FileNameJoin[{DirectoryName[$InputFileName],"process-scaffold.wl"}]];
s=<|"Model"->"PureGR","Incoming"->{FeynArts`T[1]},"Outgoing"->{FeynArts`T[1],FeynArts`T[1]},"LoopOrder"->0|>;
i=<|"Process"->s,"Diagrams"->{<|"ID"->"a","Expression"->aa|>}|>;
Print["BEFORE"];
r=FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[i,FeynGrav`FeynArtsIntegration`CalculationFunction->Function[{expr},Throw[Failure["StubThrown",<|"MessageTemplate"->"thrown"|>]]]];
Print["AFTER=",InputForm[r]];
Quit[0];
