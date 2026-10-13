(* Diagnostic quartic tadpole only; never exclude it from a physical loop API. *)
$HistoryLength=0;$LoadFeynArts=True;
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
If[!StringStartsQ[cpath,"/"] ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import["/sys/fs/cgroup"<>cpath<>"/memory.oom.group","Text"]]=!="1",
 Print["AGGREGATE_CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP_VERIFIED=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
FeynGrav`importGravitons[2];
name="FGPureGR5a149c98161e2b2e";dir="/tmp/fgfa_puregr_finite_public_20261014";
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,dir];
If[FeynArts`InitializeModel[name,FeynArts`GenericModel->name]=!=True,Quit[2]];
top=FeynArts`CreateTopologies[1,1->1,FeynArts`Adjacencies->{4},
 FeynArts`CTOrder->0,FeynArts`ExcludeTopologies->{FeynArts`WFCorrections,FeynArts`Reducible}];
ins=FeynArts`InsertFields[top,FeynArts`T[1]->FeynArts`T[1],
 FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}];
fa=FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`Truncated->True];
fc=TimeConstrained[FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{ell},
 ChangeDimension->D,Contract->False,List->True,Prefactor->1],40,$TimedOut];
If[!ListQ[fc]||Length[fc]=!=1,Print["CONVERSION_FAIL=",InputForm[fc]];Quit[3]];
f=List@@First[fc];If[Length[f]=!=5,Print["BAD_FACTORS"];Quit[4]];
a=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-p,Lor5,Lor6,ell,Lor7,Lor8,-ell];
b=FeynGrav`GravitonVertex[Lor1,Lor2,p,Lor3,Lor4,-p,Lor5,Lor6,-ell,Lor7,Lor8,ell];
Print["STRUCTURAL_MATCH=",InputForm[{f[[5]]===a,f[[5]]===b}],
 " BYTES=",InputForm[ByteCount/@{f[[5]],a,b}]];
dA=TimeConstrained[Expand[FeynCalc`MomentumExpand[f[[5]]]-FeynCalc`MomentumExpand[a]],28,$TimedOut];
Print["FIRST_ROUTE_EXACT=",dA===0," FIRST_TIMEOUT=",dA===$TimedOut,
 " FIRST_RESIDUAL_BYTES=",ByteCount[dA]];
dB=TimeConstrained[Expand[FeynCalc`MomentumExpand[f[[5]]]-FeynCalc`MomentumExpand[b]],28,$TimedOut];
Print["REVERSED_ROUTE_EXACT=",dB===0," REVERSED_TIMEOUT=",dB===$TimedOut,
 " REVERSED_RESIDUAL_BYTES=",ByteCount[dB]];
Print["TADPOLE_DIAGNOSTIC_ONLY=True NO_MEASURE=True"];
Quit[If[dA===0||dB===0,0,5]];
