(* Private builder for the fixed-gauge GR + pure SU(N) model through kappa^2.
   Definitions only: explicit developer scripts supply a fresh destination.
   Reuses Basis.wl and ModelBuilder.wl; generated bundles have no build dependency. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
$ymSourceRoot=DirectoryName[DirectoryName[$InputFileName]];
ymRequire[ok_,message_]:=If[!TrueQ[ok],Throw[faModelFailure["YangMillsBuild",message],"YMBuild"]];
ymMetric[a_,b_]:=FeynCalc`MTD[a,b];
ymIdentity[a_,b_,{r_,s_}]:=(ymMetric[a,r] ymMetric[b,s]+ymMetric[a,s] ymMetric[b,r])/2;
ymInverseSecond[a_,b_,h_,k_]:=Module[{z},FeynCalc`Contract[
 ymIdentity[a,z,h] ymIdentity[z,b,k]+ymIdentity[a,z,k] ymIdentity[z,b,h]]];

(* Canonical adjoint-colour representation of the quartic vertex. These are
   labelled derivatives of sqrt(-g) g^ab g^cd, not Taylor coefficients. Every
   nonzero-graviton result is compared exactly with the stored source before use. *)
ymDensity[a_,b_,c_,d_,{}]:=ymMetric[a,b] ymMetric[c,d];
ymDensity[a_,b_,c_,d_,{h_}]:=((ymMetric@@h)/2 ymMetric[a,b] ymMetric[c,d]
 -ymIdentity[a,b,h] ymMetric[c,d]-ymMetric[a,b] ymIdentity[c,d,h]);
ymDensity[a_,b_,c_,d_,{h:{r_,s_},k:{t_,u_}}]:=(
 (ymMetric[r,s] ymMetric[t,u]-ymMetric[r,t] ymMetric[s,u]-ymMetric[r,u] ymMetric[s,t])/4 ymMetric[a,b] ymMetric[c,d]
 -(ymMetric@@h)/2 (ymIdentity[a,b,k] ymMetric[c,d]+ymMetric[a,b] ymIdentity[c,d,k])
 -(ymMetric@@k)/2 (ymIdentity[a,b,h] ymMetric[c,d]+ymMetric[a,b] ymIdentity[c,d,h])
 +ymInverseSecond[a,b,h,k] ymMetric[c,d]+ymMetric[a,b] ymInverseSecond[c,d,h,k]
 +ymIdentity[a,b,h] ymIdentity[c,d,k]+ymIdentity[a,b,k] ymIdentity[c,d,h]);
ymQuartic[h_,{a_,b_,c_,d_},{i_,j_,k_,l_}]:=-I FeynGrav`\[Kappa]^Length[h] FeynCalc`SMP["g_s"]^2 (
 Global`FASUNF[i,j,k,l](ymDensity[a,c,b,d,h]-ymDensity[a,d,b,c,h])+
 Global`FASUNF[i,k,j,l](ymDensity[a,b,c,d,h]-ymDensity[a,d,b,c,h])+
 Global`FASUNF[i,l,j,k](ymDensity[a,b,c,d,h]-ymDensity[a,c,b,d,h]));
ymColourToSource[e_]:=e/.Global`FASUNF[a_,b_,c_,d_]:>Module[{z},FeynCalc`SUNF[a,b,z] FeynCalc`SUNF[c,d,z]];
ymExactZero[e_]:=TrueQ[Expand[FeynCalc`SUNSimplify[
 Expand[FeynCalc`ExpandScalarProduct[FeynCalc`Contract[FeynCalc`FCI[e]]]],
 Explicit->True,FeynCalc`SUNTraceEvaluate->False,FeynCalc`SUNNToCACF->False]]===0];
ymColourEncode[e_]:=e/.{
 FeynCalc`SUNDelta[FeynCalc`SUNIndex[a_],FeynCalc`SUNIndex[b_]]:>FeynArts`IndexDelta[a,b],
 FeynCalc`SUNF[FeynCalc`SUNIndex[a_],FeynCalc`SUNIndex[b_],FeynCalc`SUNIndex[c_]]:>Global`FASUNF[a,b,c],
 FeynCalc`SMP["g_s"]->FeynGrav`YangMillsCoupling};

(* All leg slots are assigned together. The last two slots of a ghost family
   are antighost, ghost; public source routines take ghost, antighost instead. *)
ymSlots[types_]:=Module[{next=0,lor},Table[
 lor=Table[Symbol["FeynArts`li"<>ToString[++next]],{Switch[types[[j]],"T",2,"V"|"F",1,_,0]}];
 <|"Type"->types[[j]],"Momentum"->Symbol["FeynArts`mom"<>ToString[j]],"Lorentz"->lor,
 "Colour"->Symbol["FeynArts`c"<>ToString[j]]|>,{j,Length[types]}]];
ymRule[family_String]:=Module[{types=Characters[family],slots,h,grav,n,vs,gs,raw,canonical},
 slots=ymSlots[types];n=Count[types,"T"];h=(#["Lorentz"]&/@Take[slots,n]);
 grav=Flatten[(Append[#["Lorentz"],#["Momentum"]]&/@Take[slots,n])];
 vs=Select[slots,#["Type"]=="V"&];gs=Take[slots,-2];
 raw=Which[
  family=="TTT"||family=="TTTT",FeynGrav`GravitonVertex@@grav,
  family=="TFF",FeynGrav`GravitonGhostVertex[Sequence@@First[h],slots[[1]]["Momentum"],First[gs[[2]]["Lorentz"]],gs[[2]]["Momentum"],First[gs[[1]]["Lorentz"]],gs[[1]]["Momentum"]],
  StringEndsQ[family,"UU"]&&Length[vs]==0,FeynGrav`GravitonYMGhostVertex[Flatten[h],gs[[2]]["Momentum"],gs[[2]]["Colour"],gs[[1]]["Momentum"],gs[[1]]["Colour"]],
  StringEndsQ[family,"UU"],If[n==0,
    -FeynCalc`SMP["g_s"] FeynCalc`SUNF[vs[[1]]["Colour"],gs[[2]]["Colour"],gs[[1]]["Colour"]] FeynCalc`FVD[gs[[1]]["Momentum"],First[vs[[1]]["Lorentz"]]],
    FeynGrav`GravitonGluonGhostVertex[Flatten[h],First[vs[[1]]["Lorentz"]],vs[[1]]["Colour"],vs[[1]]["Momentum"],0,gs[[2]]["Colour"],gs[[2]]["Momentum"],0,gs[[1]]["Colour"],gs[[1]]["Momentum"]]],
  Length[vs]==4,
    canonical=ymQuartic[h,Flatten[Lookup[vs,"Lorentz"]],Lookup[vs,"Colour"]];
    If[n>0,raw=FeynGrav`GravitonGluonVertex@@Prepend[Flatten[({#["Momentum"],First[#["Lorentz"]],#["Colour"]}&/@vs)],grav];
      ymRequire[ymExactZero[raw-ymColourToSource[canonical]],"Quartic colour reconstruction failed: "<>family]];
    canonical,
  n==0,With[{a=Flatten[Lookup[vs,"Lorentz"]],p=Lookup[vs,"Momentum"],c=Lookup[vs,"Colour"]},
    FeynCalc`SMP["g_s"] (FeynCalc`SUNF@@c)(ymMetric[a[[1]],a[[2]]] FeynCalc`FVD[p[[1]]-p[[2]],a[[3]]]+ymMetric[a[[1]],a[[3]]] FeynCalc`FVD[p[[3]]-p[[1]],a[[2]]]+ymMetric[a[[2]],a[[3]]] FeynCalc`FVD[p[[2]]-p[[3]],a[[1]]])],
  True,FeynGrav`GravitonGluonVertex@@Prepend[Flatten[({#["Momentum"],First[#["Lorentz"]],#["Colour"]}&/@vs)],grav]];
 ymRequire[FreeQ[raw,_Failure|_Missing|ymDensity|ymIdentity|ymInverseSecond|FeynGrav`GravitonGluonVertex|FeynGrav`GravitonYMGhostVertex|FeynGrav`GravitonGluonGhostVertex],"Missing source: "<>family];
 ymRequire[Exponent[raw,FeynGrav`\[Kappa]]===If[MemberQ[{"TTT","TTTT"},family],n-2,n],"Wrong coupling order: "<>family];
 ymColourEncode[FeynCalc`FCI[FeynCalc`ExpandScalarProduct[raw]]]
];
ymGenericRow[family_,vector_]:=Module[{slots=ymSlots[Characters[family]],left,right},
 left=MapIndexed[Function[{s,pos},With[{j=ToString[First[pos]]},"s"<>j<>" "<>s["Type"]<>"[j"<>j<>",mom"<>j<>If[s["Lorentz"]==={},"",","<>faEncode[s["Lorentz"]]]<>"]"]],slots];
 right=MapIndexed[("s"<>ToString[First[#2]]<>" "<>#1["Type"]<>"[j"<>ToString[First[#2]]<>"]")&,slots];
 "AnalyticalCoupling["<>StringRiffle[left,","]<>"]==G[1]["<>StringRiffle[right,","]<>"]."<>faEncode[vector]];
ymClassRow[family_,coefficients_]:=Module[{types=Characters[family],last,fields},last=Length[types];
 fields=MapIndexed[Function[{t,pos},With[{j=First[pos]},If[MemberQ[{"U","F"},t]&&j==last-1,"-",""]<>t<>"[1"<>If[MemberQ[{"V","U"},t],",{c"<>ToString[j]<>"}",""]<>"]"]],types];
 "C["<>StringRiffle[fields,","]<>"]=="<>faEncode[List/@coefficients]];

ymBuild[target_String]:=Catch[Module[{families,rules,bases,props,generic,classes,manifest,files,li},
 ymRequire[StringStartsQ[target,"/"]&&ExpandFileName[target]===target&&!FileExistsQ[target]&&DirectoryQ[DirectoryName[target]],"Supply a fresh absolute output directory with an existing parent."];
 ymRequire[faDependencyCheck[]===True&&FeynGrav`GaugeFixingEpsilonSUNYM===-1&&OwnValues[FeynCalc`SUNN]==={}&&OwnValues[FeynGrav`YangMillsCoupling]==={},"Use a fresh kernel with symbolic N/coupling and the fixed gauges."];
 ymRequire[!FailureQ[FeynGrav`FeynGravImportGravitons[2]]&&!FailureQ[FeynGrav`FeynGravImportSUNYM[2]],"Source import failed."];
 families={"TTT","TTTT","TFF","TVV","TTVV","VVV","TVVV","TTVVV","VVVV","TVVVV","TTVVVV","TUU","TTUU","VUU","TVUU","TTVUU"};
 rules=AssociationMap[ymRule,families];bases=AssociationMap[faBasisRecord[rules[#],#]&,families];
 ymRequire[FreeQ[Values[bases],_Failure],"Lorentz basis reconstruction failed."];
 ymRequire[FreeQ[Values[rules],FeynCalc`SUNF|FeynCalc`SUNDelta|FeynCalc`SUNTrace|FeynCalc`SUNT],"Untranslated colour object."];
 li=Table[Symbol["FeynArts`li"<>ToString[j]],{j,4}];
 props={faPropagator[FeynCalc`FCI[FeynGrav`GravitonPropagator[Sequence@@li,FeynArts`mom]],FeynArts`mom],
 faPropagator[FeynCalc`FCI[FeynGrav`GhostVectorPropagator[li[[1]],li[[2]],FeynArts`mom]],FeynArts`mom]};
 ymRequire[FreeQ[props,_Failure],"Propagator translation failed."];
 generic="(* GR + pure SU(N), fixed gauges; explicit snapshot through kappa^2. *)\n"<>
 "KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[F]={Lorentz}; KinematicIndices[V]={Lorentz}; KinematicIndices[U]={};\n"<>
 "$FermionLines=True; P$NonCommuting=F|U; P$ChainBuildOrder={F,U}; M$FlippingRules={};\nAttributes[FAMetricTensor]={Orderless};\n"<>
 "M$GenericPropagators={AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2],"<>
 "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]=="<>faEncode[props[[1]]]<> ","<>
 "AnalyticalPropagator[External][s1 F[j1,mom,{li1}]]==1,AnalyticalPropagator[Internal][s1 F[j1,mom,{li1}->{li2}]]=="<>faEncode[props[[2]]]<> ","<>
 "AnalyticalPropagator[External][s1 V[j1,mom,{li1}]]==Global`FAPolarizationVector[V[j1],mom,li1],AnalyticalPropagator[Internal][s1 V[j1,mom,{li1}->{li2}]]==-I Global`FAMetricTensor[li1,li2] FAPropagatorDenominator[mom,0],"<>
 "AnalyticalPropagator[External][s1 U[j1,mom]]==1,AnalyticalPropagator[Internal][s1 U[j1,mom]]==I FAPropagatorDenominator[mom,0]};\n"<>
 "M$GenericCouplings={"<>StringRiffle[(ymGenericRow[#,bases[#]["Vector"]]&/@families),","]<>"};\nM$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\n"<>
 "M$TruncationRules={_Global`FAPolarizationTensor->1,_Global`FAPolarizationVector->1};\nM$LastGenericRules={Global`FAPolarizationVector[p_,_. mom:FourMomentum[Outgoing,_],li_]:>Conjugate[Global`FAPolarizationVector][p,mom,li]};\n";
 classes="IndexRange[Index[Gluon]]=NoUnfold[FeynCalc`SUNN^2-1];\nM$ClassesDescription={"<>
 "T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine},"<>
 "F[1]=={SelfConjugate->False,Mass->0,PropagatorLabel->\"cG\",PropagatorType->GhostDash,PropagatorArrow->Forward},"<>
 "V[1]=={SelfConjugate->True,Indices->{Index[Gluon]},Mass->0,PropagatorLabel->\"A\",PropagatorType->Cycles,PropagatorArrow->None},"<>
 "U[1]=={SelfConjugate->False,Indices->{Index[Gluon]},Mass->0,PropagatorLabel->\"cYM\",PropagatorType->GhostDash,PropagatorArrow->Forward}};\n"<>
 "M$CouplingMatrices={"<>StringRiffle[(ymClassRow[#,bases[#]["Coefficients"]]&/@families),","]<>"};\nM$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices; M$LastModelRules={};\n";
 files=Join[{"FeynGrav.wl","FeynArtsIntegration/Basis.wl","FeynArtsIntegration/ModelBuilder.wl","FeynArtsIntegration/YangMillsModelBuilder.wl","FeynArtsIntegration/BuildGRSUNYM.wl","Rules/GravitonSUNYM.wl"},
 Flatten[Table["Libs/"<>f<>"_"<>ToString[n],{f,{"GravitonVertex","GravitonGluonVertex","GravitonThreeGluonVertex","GravitonFourGluonVertex","GravitonYMGhostVertex","GravitonGluonGhostVertex"}},{n,2}]]];
 manifest=<|"Schema"->1,"ModelName"->"GR-SU-N-Yang-Mills","VertexOrderCutoff"->2,"GaugeFixingEpsilon"->2,"GaugeFixingEpsilonSUNYM"->-1,"LorentzDimension"->"Symbolic D","ColourGroup"->"SU(N), N=FeynCalc`SUNN","Parameters"->{"FeynGrav`\[Kappa]","FeynGrav`YangMillsCoupling","FeynCalc`SUNN"},
 "Action"->"-2 R/kappa^2 - F^a_mn F_a^mn/4 - (nabla.A)^2/2 + g^mn partial_m antighost^a (partial_n ghost^a + g_s f^abc A_n^b ghost^c), with sqrt(-g); plus accepted GR gauge fixing and FP determinant",
 "FieldClasses"->{"T[1]","F[1]","-F[1]","V[1]","U[1]","-U[1]"},"InteractionFamilies"->families,
 "GhostRepresentation"->"F: Lorentz-vector gravitational Grassmann ghost; U: adjoint Lorentz-scalar YM Grassmann ghost; no spinor field",
 "Sources"->AssociationMap[FileHash[FileNameJoin[{$ymSourceRoot,#}],"SHA256","HexString"]&,files],"BasisSizes"->AssociationMap[bases[#]["TermCount"]&,families],"RuleSHA256"->AssociationMap[bases[#]["RuleSHA256"]&,families]|>;
 CreateDirectory[target];Export[FileNameJoin[{target,"GR-SU-N-Yang-Mills.gen"}],generic,"Text"];Export[FileNameJoin[{target,"GR-SU-N-Yang-Mills.mod"}],classes,"Text"];
 AppendTo[manifest,"ModelFileSHA256"->AssociationMap[FileHash[FileNameJoin[{target,"GR-SU-N-Yang-Mills"<>#}],"SHA256","HexString"]&,{".gen",".mod"}]];
 Export[FileNameJoin[{target,"manifest.json"}],manifest,"RawJSON"];
 <|"Directory"->target,"Manifest"->manifest,"Rules"->rules|>
],"YMBuild"];
End[];
