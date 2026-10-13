$HistoryLength=0;$LoadFeynArts=True;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynArtsIntegration/Diagram.wl"];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_20261014b";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
r=FeynArts`InitializeModel[name,FeynArts`GenericModel->name];
Print["INIT=",FullForm[r]," PATH=",InputForm[$ContextPath]];
Quit[0];
