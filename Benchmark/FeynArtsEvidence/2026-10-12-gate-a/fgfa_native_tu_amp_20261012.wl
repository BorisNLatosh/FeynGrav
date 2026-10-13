$HistoryLength=0;$LoadFeynArts=True;
Get["FeynCalc`"];
FeynArts`$FAVerbose=0;
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,"/tmp"];
Print["INIT=",FeynArts`InitializeModel["fgfa_native_tu_probe_20261012",FeynArts`GenericModel->"fgfa_native_tu_probe_20261012"]];
short[x_]:=StringTake[ToString[InputForm[x]],UpTo[2500]];
lim[e_]:=TimeConstrained[e,12,$TimedOut];
opts={FeynArts`Model->"fgfa_native_tu_probe_20261012",FeynArts`GenericModel->"fgfa_native_tu_probe_20261012",FeynArts`InsertionLevel->{FeynArts`Classes}};
Print["TREE_BEGIN"];
treeTopo=lim[FeynArts`CreateTopologies[0,1->2]];Print["treeTopo=",short[treeTopo]];
tree=lim[FeynArts`InsertFields[treeTopo,FeynArts`T[1]->{FeynArts`S[1],FeynArts`S[1]},Sequence@@opts]];Print["treeInsert=",short[tree]];
If[tree =!= $TimedOut && !FreeQ[tree,FeynArts`FeynmanGraph],
 treeAmp=lim[FeynArts`CreateFeynAmp[tree,FeynArts`PreFactor->1,FeynArts`Truncated->False]];Print["treeAmp=",short[treeAmp]];
 Print["treeAmpCustomPolar=",Cases[treeAmp,_Global`FAPolarizationTensor,Infinity]];
 If[treeAmp =!= $TimedOut,Print["treeFC=",short[lim[FCFAConvert[treeAmp,IncomingMomenta->{p},OutgoingMomenta->{k1,k2},Contract->False,ChangeDimension->False,List->True]]]]];
 treeAmpTrunc=lim[FeynArts`CreateFeynAmp[tree,FeynArts`PreFactor->1,FeynArts`Truncated->True]];Print["treeTrunc=",short[treeAmpTrunc]];
 If[treeAmpTrunc =!= $TimedOut,Print["treeTruncFC=",short[lim[FCFAConvert[treeAmpTrunc,IncomingMomenta->{p},OutgoingMomenta->{k1,k2},Contract->False,ChangeDimension->False,List->True]]]]];
];
Print["LOOP_BEGIN"];
loopTopo=lim[FeynArts`CreateTopologies[1,1->1,FeynArts`ExcludeTopologies->{FeynArts`Tadpoles}]];Print["loopTopo=",short[loopTopo]];
loop=lim[FeynArts`InsertFields[loopTopo,FeynArts`T[1]->FeynArts`T[1],Sequence@@opts,FeynArts`ExcludeParticles->{FeynArts`T[1],FeynArts`S[1]}]];Print["loopInsert=",short[loop]];
If[loop =!= $TimedOut && !FreeQ[loop,FeynArts`FeynmanGraph],
 loopAmp=lim[FeynArts`CreateFeynAmp[loop,FeynArts`PreFactor->1,FeynArts`Truncated->True]];Print["loopAmp=",short[loopAmp]];
 If[loopAmp =!= $TimedOut,Print["loopFC=",short[lim[FCFAConvert[loopAmp,IncomingMomenta->{p},OutgoingMomenta->{q},LoopMomenta->{ell},Contract->False,ChangeDimension->False,List->True]]]]];
];
Print["DONE"];
Quit[];
