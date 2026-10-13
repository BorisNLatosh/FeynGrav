$HistoryLength=0;$LoadFeynArts=True;$FAVerbose=0;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/tmp/fg_sparse_basis_20261014.wl"];
FeynGrav`importGravitons[1];
Clear[FeynArts`li1,FeynArts`li2,FeynArts`li3,FeynArts`li4,FeynArts`li5,FeynArts`li6,FeynArts`mom1,FeynArts`mom2,FeynArts`mom3];
src=FeynGrav`GravitonVertex[FeynArts`li1,FeynArts`li2,FeynArts`mom1,FeynArts`li3,FeynArts`li4,FeynArts`mom2,FeynArts`li5,FeynArts`li6,FeynArts`mom3];
rec=FGBoundedBasis`ExtractVertex[src,4000];
If[!AssociationQ[rec],Print["EXTRACTFAIL=",rec];Quit[1]];
basis=rec["Monomials"]/. p:FeynCalc`Pair[__]:>FGBoundedBasis`FAAtom[p];
Print["basis=",Length[basis]," residual=",Count[basis,FeynCalc`Pair[__],Infinity]," exact=",TrueQ[Expand[rec["Reconstruction"]-src]===0]];
enc[e_] := Block[{$ContextPath={"System`"}},ToString[e,InputForm]];
name="fgfa_basis_cubic_v3_20261014";gen="/tmp/"<>name<>".gen";mod="/tmp/"<>name<>".mod";
If[FileExistsQ[gen]||FileExistsQ[mod],Print["ERROR existing prototype, refusing overwrite"];Quit[2]];
core="KinematicIndices[T]={Lorentz,Lorentz};\n$FermionLines=False;\n"<>
 "M$GenericPropagators={AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==1,\n"<>
 "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]==I FAPropagatorDenominator[mom,Mass[T[j1]]] (FAMetricTensor[li1,li3] FAMetricTensor[li2,li4]+FAMetricTensor[li1,li4] FAMetricTensor[li2,li3])/2};\n"<>
 "M$GenericCouplings={AnalyticalCoupling[s1 T[j1,mom1,{li1,li2}],s2 T[j2,mom2,{li3,li4}],s3 T[j3,mom3,{li5,li6}]]== G[1][s1 T[j1],s2 T[j2],s3 T[j3]] . "<>enc[basis]<>"};\n"<>
 "M$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\nM$LastGenericRules={};\n";
cl="M$ClassesDescription={T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine}};\n"<>
 "M$CouplingMatrices={C[T[1],T[1],T[1]]=="<>enc[List/@rec["Coefficients"]]<>"};\n"<>
 "M$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices;\nM$LastModelRules={};\n";
Print["write gen=",Export[gen,core,"Text"]," bytes=",FileByteCount[gen]];
Print["write mod=",Export[mod,cl,"Text"]," bytes=",FileByteCount[mod]];
FeynArts`$ModelPath=Prepend[FeynArts`$ModelPath,"/tmp"];
val=TimeConstrained[FeynArts`InitializeModel[name,FeynArts`GenericModel->name],25,Failure["init wall cap"]];
Print["init=",val," genericBasis=",Length[FeynArts`KinematicVector[FeynArts`T,FeynArts`T,FeynArts`T]]," classes=",Length[FeynArts`M$CouplingMatrices]];
Print["classLength=",Length[Last[First[FeynArts`M$CouplingMatrices]]]];
If[FailureQ[val]||val===$Aborted,Quit[3]];
Print["topologyTime=",AbsoluteTiming[tops=TimeConstrained[FeynArts`CreateTopologies[0,1->2,FeynArts`Adjacencies->{3}],8,Failure["top wall cap"]];][[1]]];
If[FailureQ[tops],Print[tops];Quit[4]];
Print["insertTime=",AbsoluteTiming[ins=TimeConstrained[FeynArts`InsertFields[tops,{FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},FeynArts`Model->name,FeynArts`GenericModel->name,FeynArts`InsertionLevel->{FeynArts`Classes}],10,Failure["insert wall cap"]];][[1]]];
If[FailureQ[ins],Print[ins];Quit[5]];
Print["insertionHead=",Head[ins]," bytes=",ByteCount[ins]];
amp=TimeConstrained[FeynArts`CreateFeynAmp[ins,FeynArts`PreFactor->1,FeynArts`GaugeRules->{},FeynArts`Truncated->True],12,Failure["amp wall cap"]];
Print["ampSuccess=",!FailureQ[amp]," head=",Head[amp]," bytes=",ByteCount[amp]];
Quit[If[FailureQ[amp],6,0]];
