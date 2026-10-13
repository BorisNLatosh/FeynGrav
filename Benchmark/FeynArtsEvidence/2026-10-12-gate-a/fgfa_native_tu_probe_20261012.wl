(* Run using timeout 100s WolframKernel -noprompt -script /tmp/fgfa_native_tu_probe_20261012.wl *)
$HistoryLength=0;
$LoadFeynArts=True;
$FAVerbose=0;
Print["START=",DateString[]," kernel=",$Version];
Print["LOAD=",TimeConstrained[Get["FeynCalc`"],40,$TimedOut]];
Print["FApath=",FeynArts`$FeynArtsDir," FCpath=",FindFile["FeynCalc`"]];
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,"/tmp"];
Print["MODEL=",TimeConstrained[FeynArts`InitializeModel["fgfa_native_tu_probe_20261012",FeynArts`GenericModel->"fgfa_native_tu_probe_20261012"],15,$TimedOut]];
Print["KIN=",InputForm[{FeynArts`KinematicIndices[FeynArts`T],FeynArts`KinematicIndices[FeynArts`U],FeynArts`F$Generic,FeynArts`F$Classes}]];
Print["COUPLINGS=",InputForm[FeynArts`M$CouplingMatrices]];
Block[{li1=FeynArts`Index[Global`Lorentz,1],li2=FeynArts`Index[Global`Lorentz,2],li3=FeynArts`Index[Global`Lorentz,3],li4=FeynArts`Index[Global`Lorentz,4]},
 Print["Tinternal=",InputForm[FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`T[1,p,{li1,li2}->{li3,li4}]]]];
 Print["Texternal=",InputForm[FeynArts`AnalyticalPropagator[FeynArts`External][FeynArts`T[1,p,{li1,li2}]]]];
 Print["Uinternal=",InputForm[FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`U[1,p,{li1}->{li2}]]]];
 Print["FCinternalT=",InputForm[TimeConstrained[FCPrepareFAAmp[FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`T[1,p,{li1,li2}->{li3,li4}]]],8,$TimedOut]]];
 Print["FCinternalU=",InputForm[TimeConstrained[FCPrepareFAAmp[FeynArts`AnalyticalPropagator[FeynArts`Internal][FeynArts`U[1,p,{li1}->{li2}]]],8,$TimedOut]]];
 Print["FCexternalT=",InputForm[TimeConstrained[FCPrepareFAAmp[FeynArts`AnalyticalPropagator[FeynArts`External][FeynArts`T[1,p,{li1,li2}]]],8,$TimedOut]]];
];
Print["DONE=",DateString[]];
Quit[];
