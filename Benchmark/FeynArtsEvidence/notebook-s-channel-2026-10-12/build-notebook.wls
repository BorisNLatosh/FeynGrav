(* Append readable, unevaluated s-channel cells to the existing initialisation-only notebook.
   Write a candidate first; the validated candidate is installed separately. *)
$HistoryLength=0;
root=DirectoryName[$InputFileName,4];
path=FileNameJoin[{DirectoryName[$InputFileName],"initialisation-only.nb"}];
nb=Get[path];
If[Head[nb]=!=Notebook||Length[Cases[nb,Cell[_,"Input",___],Infinity]]=!=1,
 Print["EXPECTED_INITIALISATION_ONLY_NOTEBOOK"];Quit[2]];
SetAttributes[input,HoldAllComplete];
input[code_]:=Cell[BoxData[ToBoxes[Defer[code],StandardForm]],"Input",InitializationCell->False];
newCells={
 Cell["2. Generate and select the s-channel", "Section"],
 Cell["Generate cubic 2 -> 2 tree topologies and select the one whose two incoming external lines meet at the same vertex. This identifies the s-channel by connectivity, not diagram-list position. FeynArts supplies the two cubic vertices, propagator and graph factor; no additional symmetry factor is applied. All p1,...,p4 below are incoming, so the physical outgoing momenta passed to FCFAConvert are -p3 and -p4.","Text"],
 input[
  treeTopologies=FeynArts`CreateTopologies[0,2->2,FeynArts`Adjacencies->{3}];
  sTopologies=Select[treeTopologies,
   With[{incomingVertices=Cases[#,FeynArts`Propagator[FeynArts`Incoming][_,vertex_]:>vertex,Infinity]},
    Length[incomingVertices]===2&&SameQ@@incomingVertices]&];
  If[Length[sTopologies]=!=1,Print["Expected exactly one s-channel topology."];Abort[]];
  sInsertions=FeynArts`InsertFields[sTopologies,
   {FeynArts`T[1],FeynArts`T[1]}->{FeynArts`T[1],FeynArts`T[1]},
   FeynArts`Model->grModel,FeynArts`GenericModel->grModel,
   FeynArts`InsertionLevel->{FeynArts`Classes}];
 ],
 Cell["Optional drawing: evaluate FeynArts`Paint[sInsertions]. The calculation below does not require a graphical front end.","Text"],
 input[
  sRawAmplitude=FeynArts`CreateFeynAmp[sInsertions,FeynArts`PreFactor->1,
   FeynArts`GaugeRules->{},FeynArts`MomentumConservation->True,FeynArts`Truncated->True];
  sConverted=FeynCalc`FCFAConvert[sRawAmplitude,
   FeynCalc`IncomingMomenta->{p1,p2},FeynCalc`OutgoingMomenta->{-p3,-p4},
   FeynCalc`LorentzIndexNames->{mu1,nu1,mu2,nu2,mu3,nu3,mu4,nu4,alpha1,beta1,alpha2,beta2},
   FeynCalc`ChangeDimension->D,FeynCalc`Contract->False,List->True,FeynCalc`Prefactor->1];
  If[!ListQ[sConverted]||Length[sConverted]=!=1,
   Print["Expected one converted s-channel diagram."];Abort[]];
  sChannelTensor=First[sConverted];
  sChannelRouting=Cases[sChannelTensor,
   FeynCalc`PropagatorDenominator[FeynCalc`Momentum[q_,___],0]:>q,Infinity];
 ],
 input[sChannelRouting],
 Cell["MomentumConservation -> True determines internal routing at each vertex; it does not put external legs on shell. In the all-incoming convention, the exchanged momentum is -(p3+p4) = p1+p2 and its pole is 1/s. The tensor is amputated: external helicity tensors are supplied explicitly below.","Text"],
 Cell["3. Apply the original massless helicity conventions", "Section"],
 Cell["Use the same polarisation scalar products as Graviton_Scattering_Tree_Level.nb. The labels h1,...,h4 take values +/-1 and represent graviton helicities +/-2; I and -I select incoming tensors and conjugated outgoing tensors, not helicity signs. These are four-dimensional helicity kinematics, with symbolic D retained as in the original tensor calculation. SetMandelstam and the Pair assignments change this kernel's kinematics: use a fresh kernel for unrelated examples.","Text"],
 input[
  FeynCalc`SetMandelstam[s,t,u,p1,p2,p3,p4,0,0,0,0];
  momenta={p1,p2,p3,p4};
  polarisationMomenta=MapThread[FeynCalc`Momentum[FeynCalc`Polarization[#1,#2],D]&,
   {momenta,{I,I,-I,-I}}];
  helicityAssumptions=h1^2==1&&h2^2==1&&h3^2==1&&h4^2==1&&
   s+t+u==0&&Element[{h1,h2,h3,h4,s,t,u},Reals]&&s>0&&t<0&&u<0;
 ],
 Cell["The symmetric table below is epsilon_i . epsilon_j. Its zero diagonal imposes traceless factorised helicity tensors. It reproduces the original notebook's six mixed polarisation products.","Text"],
 input[
  polarisationProducts={
   {0,(1+h1 h2)/2,(s+h1 h3 (s+2t))/(2s),h1 h4 (h1 h4 s-t+u)/(2s)},
   {(1+h1 h2)/2,0,h2 h3 (h2 h3 s-t+u)/(2s),(s+h2 h4 (s+2t))/(2s)},
   {(s+h1 h3 (s+2t))/(2s),h2 h3 (h2 h3 s-t+u)/(2s),0,(1+h3 h4)/2},
   {h1 h4 (h1 h4 s-t+u)/(2s),(s+h2 h4 (s+2t))/(2s),(1+h3 h4)/2,0}};
  Do[FeynCalc`Pair[polarisationMomenta[[i]],polarisationMomenta[[j]]]=polarisationProducts[[i,j]],
   {i,4},{j,4}];
 ],
 Cell["Rows in the next table correspond to p1,...,p4; columns to epsilon1,...,epsilon4. Its diagonal gives transversality; its remaining entries fix the same reference-vector convention as the original example.","Text"],
 input[
  momentumPolarisationProducts=Sqrt[t u/(2s)] {
   {0,0,h3,-h4},{0,0,-h3,h4},{-h1,h2,0,0},{h1,-h2,0,0}};
  Do[FeynCalc`Pair[FeynCalc`Momentum[momenta[[i]],D],polarisationMomenta[[j]]]=
   momentumPolarisationProducts[[i,j]],{i,4},{j,4}];
  sChannelPole=FeynCalc`ExpandScalarProduct[FeynCalc`FeynAmpDenominatorExplicit[
   First[Cases[sChannelTensor,_FeynCalc`FeynAmpDenominator,Infinity]]]];
 ],
 input[sChannelPole],
 Cell["4. Contract the s-channel with the external helicity tensors", "Section"],
 Cell["CalcFormCalculate performs the tensor algebra using real FORM, with one worker and a 60-second per-stage time limit. The input stays factored; it is not expanded into a large open-index exchange tensor first. FORM job files are retained under sChannelWorkDirectory. This is one gauge-dependent channel, not the full gauge-invariant scattering amplitude or a cross section. No extra factor of i, graph factor, spin sum or loop measure is inserted.","Text"],
 input[
  sExternalTensors=FeynGrav`PolarizationTensorD[p1,mu1,nu1,I]*
   FeynGrav`PolarizationTensorD[p2,mu2,nu2,I]*
   FeynGrav`PolarizationTensorD[p3,mu3,nu3,-I]*
   FeynGrav`PolarizationTensorD[p4,mu4,nu4,-I];
  sChannelWorkDirectory=CreateDirectory[FileNameJoin[{$TemporaryDirectory,"fgfa-s-channel-"<>CreateUUID[]}]];
  sChannelFORM=CalcFormConverter`CalcFormCalculate[
   FeynCalc`FeynAmpDenominatorExplicit[sChannelTensor]*sExternalTensors,
   FeynCalc`Dimension->D,CalcFormConverter`FORMThreads->1,TimeConstraint->60,
   CalcFormConverter`WorkingDirectory->sChannelWorkDirectory,
   CalcFormConverter`KeepFiles->True,CalcFormConverter`ShowProgress->False];
  If[FailureQ[sChannelFORM]||MemberQ[{$Failed,$Aborted},sChannelFORM]||
   !FreeQ[sChannelFORM,_FeynCalc`LorentzIndex|_FeynCalc`Pair|_CalcFormConverter`CalcFormCalculate],
   Print["The FORM contraction did not return a scalar. Inspect sChannelFORM and sChannelWorkDirectory."];Abort[]];
  AmplitudeSChannel=FeynCalc`TrickMandelstam[
   Simplify[sChannelFORM,Assumptions->helicityAssumptions],{s,t,u,0}];
 ],
 input[AmplitudeSChannel],
 Cell["5. Check against the original notebook", "Section"],
 Cell["The expression below is the saved s-channel reference in the original notebook, including its FeynGrav normalisation. It is used only after the FeynArts/FORM calculation, never as an input or replacement for the calculated amplitude. The residual must vanish; this tests the s-channel only, not a Ward identity for the full amplitude.","Text"],
 input[
  sChannelReference=9 I FeynGrav`\[Kappa]^2 t u (1+h1 h2)(1+h3 h4)/(16s);
  sChannelReferenceResidual=Simplify[AmplitudeSChannel-sChannelReference,
   Assumptions->helicityAssumptions];
  sChannelMatchesReference=TrueQ[sChannelReferenceResidual===0];
 ],
 input[sChannelMatchesReference]
};
nb[[1,2]]=Cell["FeynArts version of Graviton_Scattering_Tree_Level.nb: generate and calculate the massless s-channel four-graviton tree contribution with the same helicity conventions. The other exchange channels and quartic contact are not included yet.","Text"];
nb[[1]]=Join[nb[[1]],newCells];
output=FileNameJoin[{DirectoryName[$InputFileName],"candidate.nb"}];
Put[nb,output];
Print["CANDIDATE=",output," INPUT_CELLS=",Length[Cases[nb,Cell[_,"Input",___],Infinity]]];
Quit[0];
