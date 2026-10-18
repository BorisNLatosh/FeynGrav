(* Private standalone scalar-model builder. Called only by the explicit developer scripts.
   Shares the existing typed FeynArts encoding; does not load at FeynGrav startup.
   Output must be a new directory. Model files require no builder at run time. *)
$HistoryLength=0;
buildRepo=DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[{"GRScalarPotential","GRJohn","GREsGB"},buildModel],Print["Unknown model"];Exit[2]];
args=Select[$ScriptCommandLine,StringQ[#]&&StringStartsQ[#,"/"]&];
buildTarget=Environment["FG_MODEL_TARGET"];
If[!StringQ[buildTarget],buildTarget=If[Length[args]>=2,Last[args],""]];
If[!StringQ[buildTarget]||!StringStartsQ[buildTarget,"/"]||FileExistsQ[buildTarget]||!DirectoryQ[DirectoryName[buildTarget]],Print["Supply a fresh absolute output directory"];Exit[2]];
$LoadAddOns={"FeynArts"};Get[FileNameJoin[{buildRepo,"FeynGrav.wl"}]];
Get[FileNameJoin[{buildRepo,"FeynArtsIntegration","FeynArtsIntegration.wl"}]];
SetAttributes[need,HoldRest];
need[label_,e_]:=Module[{v=Check[e,$Failed]},If[!TrueQ[v],Print["BUILD-FAIL ",label];Exit[1]]];
need["settings",OwnValues[D]==={}&&OwnValues[FeynGrav`\[Kappa]]==={}&&FeynGrav`GaugeFixingEpsilon===2];
need["imports",!FailureQ[FeynGrav`importGravitons[2]]&&!FailureQ[FeynGrav`importScalars[2]]];
faEncode[e_] := FeynGrav`FeynArtsIntegration`Private`faEncode[e];
faBasisRecord[e_, f_] := FeynGrav`FeynArtsIntegration`Private`faBasisRecord[e, f];
faPropagator[e_, m_] := FeynGrav`FeynArtsIntegration`Private`faPropagator[e, m];
faTranslateMonomial[e_] := FeynGrav`FeynArtsIntegration`Private`faTranslateMonomial[e];
faExtractVertex[e_, c_] := FeynGrav`FeynArtsIntegration`Private`faExtractVertex[e, c];
faModelFailure[args___] := FeynGrav`FeynArtsIntegration`Private`faModelFailure[args];

scalarMass = FeynGrav`ScalarMass;
jSym = Symbol["FeynArts`j1"];
massPlaceholder = FeynArts`Mass[FeynArts`S[jSym]];

indices = Table[Symbol["FeynArts`li" <> ToString[i]], {i, 8}];
momenta = Table[Symbol["FeynArts`mom" <> ToString[i]], {i, 6}];

rules = <|
  "TTT" -> FeynGrav`GravitonVertex @@
    Flatten[Transpose[{Partition[indices[[1 ;; 6]], 2], momenta[[1 ;; 3]]}]],
  "TTTT" -> FeynGrav`GravitonVertex @@
    Flatten[Transpose[{Partition[indices[[1 ;; 8]], 2], momenta[[1 ;; 4]]}]],
  "TUU" -> FeynGrav`GravitonGhostVertex[indices[[1]], indices[[2]], momenta[[1]],
    indices[[4]], momenta[[3]], indices[[3]], momenta[[2]]],
  "TSS" -> FeynGrav`GravitonScalarVertex[{indices[[1]], indices[[2]]},
    momenta[[2]], momenta[[3]], scalarMass],
  "TTSS" -> FeynGrav`GravitonScalarVertex[{indices[[1]], indices[[2]], indices[[3]],
    indices[[4]]}, momenta[[3]], momenta[[4]], scalarMass]|>;


minimalRules=rules;
Switch[buildModel,
 "GRScalarPotential",
 Do[AppendTo[rules,StringJoin[ConstantArray["T",nh],ConstantArray["S",ns]] ->
   If[nh==0,I couplingSymbol,FeynGrav`GravitonScalarPotentialVertex[Take[indices,2 nh],couplingSymbol]]],
   {ns,{3,4}},{couplingSymbol,{If[ns==3,FeynGrav`ScalarLambda3,FeynGrav`ScalarLambda4]}},{nh,0,2}],
 "GRJohn",
 need["G5 import",!FailureQ[FeynGrav`importHorndeskiG5[]]];
 Do[family=If[nh==1,"TSS","TTSS"];
   grav=Flatten[Table[{indices[[2 j-1]],indices[[2 j]],momenta[[j]]},{j,nh}]];
   rules[family]=rules[family]+FeynGrav`HorndeskiG5[grav,{momenta[[nh+1]],momenta[[nh+2]]},0,-FeynGrav`JohnBeta],{nh,1,2}],
 "GREsGB",
 need["GB import",!FailureQ[FeynGrav`importScalarGaussBonnet[2]]];
 grav=Flatten[Table[{indices[[2 j-1]],indices[[2 j]],momenta[[j]]},{j,2}]];
 AppendTo[rules,"TTS"->FeynGrav`ScalarGaussBonnet[grav,FeynGrav`GaussBonnetG1]];
 rules["TTSS"]=rules["TTSS"]+FeynGrav`ScalarGaussBonnet[grav,FeynGrav`GaussBonnetG2]];
need["explicit rules",FreeQ[Values[rules],_Failure|$Failed|FeynGrav`HorndeskiG5|FeynGrav`ScalarGaussBonnet|FeynGrav`GravitonScalarPotentialVertex]];
families=Keys[rules];basisList=Map[faBasisRecord[rules[#],#]&,families];
need["basis reconstruction",FreeQ[basisList,_Failure|$Failed]];
basis=AssociationThread[families,basisList];
hp=faPropagator[FeynCalc`FCI[FeynGrav`GravitonPropagator[indices[[1]],indices[[2]],indices[[3]],indices[[4]],FeynArts`mom]],FeynArts`mom];
up=faPropagator[FeynCalc`FCI[FeynGrav`GhostVectorPropagator[indices[[1]],indices[[2]],FeynArts`mom]],FeynArts`mom];
sp=I FeynArts`FAPropagatorDenominator[FeynArts`mom,massPlaceholder];
need["propagators",FreeQ[{hp,up},_Failure|$Failed]];
legLHS[types_List] := Module[{parts = {}, k = 0},
  Do[
    Switch[types[[i]],
      "T", k += 2;
      AppendTo[parts, "s" <> ToString[i] <> " T[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> ",{li" <> ToString[k - 1] <> ",li" <> ToString[k] <> "}]"],
      "U", k += 1;
      AppendTo[parts, "s" <> ToString[i] <> " U[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> ",{li" <> ToString[k] <> "}]"],
      "S", AppendTo[parts, "s" <> ToString[i] <> " S[j" <> ToString[i] <> ",mom" <>
        ToString[i] <> "]"]],
    {i, Length[types]}];
  parts];

legRHS[types_List] := Table[
  "s" <> ToString[i] <> " " <> types[[i]] <> "[j" <> ToString[i] <> "]",
  {i, Length[types]}];

coupling[types_List, vec_] := "AnalyticalCoupling[" <>
  StringRiffle[legLHS[types], ","] <> "]==G[1][" <>
  StringRiffle[legRHS[types], ","] <> "]." <> faEncode[vec];

classRow[classFields_List, coeffs_] := "C[" <> StringRiffle[classFields, ","] <>
  "]==" <> faEncode[List /@ coeffs];


genericCouplings=Map[coupling[Characters[#],basis[#]["Vector"]]&,families];
classCouplings=Map[classRow[If[#=="TUU",{"T[1]","-U[1]","U[1]"},(#<>"[1]"&/@Characters[#])],basis[#]["Coefficients"]]&,families];
genericText = "(* GR plus one real massive scalar; FeynGrav rules snapshotted by explicit build. *)\n" <>
  "KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[U]={Lorentz}; KinematicIndices[S]={};\n" <>
  "$FermionLines=True; P$NonCommuting=U; P$ChainBuildOrder={U};\n" <>
  "Attributes[FAMetricTensor]={Orderless};\n" <>
  "M$GenericPropagators={" <>
  "AnalyticalPropagator[External][s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2]," <>
  "AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]==" <> faEncode[hp] <> "," <>
  "AnalyticalPropagator[External][s1 U[j1,mom,{li1}]]==1," <>
  "AnalyticalPropagator[Internal][s1 U[j1,mom,{li1}->{li2}]]==" <> faEncode[up] <> "," <>
  "AnalyticalPropagator[External][s1 S[j1,mom]]==1," <>
  "AnalyticalPropagator[Internal][s1 S[j1,mom]]==" <> faEncode[sp] <> "};\n" <>
  "M$GenericCouplings={" <> StringRiffle[genericCouplings, ","] <> "};\n" <>
  "M$GenericCouplings=(If[FreeQ[First[#],T|U],#,CloseKinematicVector[#]]&)/@M$GenericCouplings;\n" <>
  "M$TruncationRules={_Global`FAPolarizationTensor->1}; M$LastGenericRules={};\n";

classesText = "M$ClassesDescription={" <>
  "T[1]=={SelfConjugate->True,Mass->0,PropagatorLabel->\"h\",PropagatorType->Sine}," <>
  "U[1]=={SelfConjugate->False,Mass->0,PropagatorLabel->\"c\",PropagatorType->GhostDash,PropagatorArrow->Forward}," <>
  "S[1]=={SelfConjugate->True,Mass->FeynGrav`ScalarMass,PropagatorLabel->\"s\",PropagatorType->ScalarDash,PropagatorArrow->None}};\n" <>
  "M$CouplingMatrices={" <> StringRiffle[classCouplings, ","] <> "};\n" <>
  "M$CouplingMatrices=(If[FreeQ[First[#],T|U],#,CloseCouplingVector[#]]&)/@M$CouplingMatrices; M$LastModelRules={};\n";


CreateDirectory[buildTarget];
Export[FileNameJoin[{buildTarget,buildModel<>".gen"}],genericText,"Text"];
Export[FileNameJoin[{buildTarget,buildModel<>".mod"}],classesText,"Text"];
sourceImporters={FeynGrav`importGravitons,FeynGrav`importScalars};
If[buildModel=="GRJohn",AppendTo[sourceImporters,FeynGrav`importHorndeskiG5]];
If[buildModel=="GREsGB",AppendTo[sourceImporters,FeynGrav`importScalarGaussBonnet]];
manifest=<|"Schema"->1,"Profile"->buildModel,"ModelName"->buildModel,"FixedGauge"->2,
 "LorentzDimension"->"Symbolic D","FieldClasses"->{"T[1]","U[1]","-U[1]","S[1]"},
 "InteractionFamilies"->families,"GravitonOrders"->{1,2},"VertexOrderCutoff"->2,
 "Parameters"->Switch[buildModel,"GRScalarPotential",{"FeynGrav`ScalarLambda3","FeynGrav`ScalarLambda4"},"GRJohn",{"FeynGrav`JohnBeta"},"GREsGB",{"FeynGrav`GaussBonnetG1","FeynGrav`GaussBonnetG2"}],"ScalarMassSymbol"->"FeynGrav`ScalarMass",
 "LibrarySources"->Flatten[(FeynGrav`FeynGravLibraryInformation[#]["Files"]&/@sourceImporters)],
 "RuleSHA256"->Lookup[basisList,"RuleSHA256"],"FeynArtsVersion"->ToString[FeynArts`$FeynArtsVersionNumber],
 "FeynGravSHA256"->FileHash[FileNameJoin[{buildRepo,"FeynGrav.wl"}],"SHA256","HexString"],
 "ModelFileSHA256"->AssociationThread[{"Generic","Classes"},FileHash[FileNameJoin[{buildTarget,buildModel<>#}],"SHA256","HexString"]&/@{".gen",".mod"}]|>;
Export[FileNameJoin[{buildTarget,"manifest.json"}],manifest,"RawJSON"];
(* Native initialisation is performed by the bounded verification script, which
   also checks independent action normalisation before this candidate is shipped. *)
Print["BUILT ",buildModel," ",buildTarget];
