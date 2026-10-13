x=Global`FAFourVector[FeynArts`mom1,FeynArts`li2];
Print["STRICT=",Block[{$ContextPath={"System`"}},ToString[x,InputForm]]];
Print["ROLED=",Block[{$ContextPath={"System`","FeynArts`"},$Context="FeynGrav`FeynArtsIntegration`Private`"},ToString[x,InputForm]]];
Quit[0];
