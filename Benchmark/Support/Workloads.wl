(* ::Package:: *)
(* Loaded inside FeynGravBenchmark`Private`. Inputs are deterministic and all
   workload symbols belong to this context, never to the user's Global context. *)

benchSymbol[prefix_String, i_Integer] := Symbol["FeynGravBenchmark`Data`" <> prefix <> ToString[i]];
case[id_, expression_, reference_: Missing["NotAvailable"], physical_: False, loops_: {}] :=
 <|"ID" -> id, "Expression" -> expression, "Reference" -> reference,
   "Physical" -> physical, "LoopMomenta" -> loops|>;

smallCases[] := {
 case["metric-trace", MTD[mu, nu]^2, D],
 case["momentum-contraction", MTD[mu, nu] FVD[p-l, mu] FVD[p, nu], SPD[p]-SPD[l,p]],
 case["tensor-product", (MTD[mu,nu]+FVD[p,mu] FVD[p,nu]) FVD[q,mu] FVD[q,nu],
   Contract[FCI[(MTD[mu,nu]+FVD[p,mu] FVD[p,nu]) FVD[q,mu] FVD[q,nu]]]]
};

synthetic[id_, n_] := Module[{expr},
 expr = Switch[id,
  "repeated-products", Total[Table[benchSymbol["c",i] SPD[p,q] SPD[p,l],{i,n}]],
  "distinct-products", Total[Table[benchSymbol["c",i] SPD[benchSymbol["v",i],q] SPD[p,l],{i,n}]],
  "repeated-propagators", Total[Table[benchSymbol["c",i] FAD[{l,m}],{i,n}]],
  "distinct-propagators", Total[Table[benchSymbol["c",i] FAD[{l,benchSymbol["m",i]}],{i,n}]],
  "simple-routing", Total[Table[benchSymbol["c",i] FVD[p,mu] FVD[q,mu],{i,n}]],
  "composite-routing", Total[Table[benchSymbol["c",i] FVD[p-i l,mu] FVD[q+l,mu],{i,n}]],
  "repeated-symbols", Total[Table[benchSymbol["c",i] FeynGravBenchmark`Greek`\[Alpha] FeynGravBenchmark`Other`\[Alpha],{i,n}]],
  "distinct-symbols", Total[Table[benchSymbol["c",i] benchSymbol["\[ScriptA]",i] benchSymbol["\[GothicA]",i],{i,n}]],
  "factored", (Total[Table[benchSymbol["c",i],{i,n}]]) (1+SPD[p,q]+SPD[l,p]),
  "expanded", Expand[(Total[Table[benchSymbol["c",i],{i,n}]]) (1+SPD[p,q]+SPD[l,p])]
 ];
 Join[case[id<>"-"<>ToString[n],expr],<|"RequestedEntries"->n|>]
];

(* Library loading is preparation, not part of expression construction timing. *)
loadPhysicalLibraries[] := (importQuadraticGravity[1]; Null);
external[a_, b_] := (Nieuwenhuizen`GaugeProjector[a,b,p]+(D-1) Nieuwenhuizen`GaugeProjectorBar[a,b,p])/Sqrt[(D-1)(D-2)];
partialBubble[] := external[mu,nu] QuadraticGravityVertex[{mu,nu,p,a1,b1,-l,i1,j1,l-p},m0,m2] *
 QuadraticGravityPropagator[a1,b1,a2,b2,l,m0,m2] *
 QuadraticGravityPropagator[i1,j1,i2,j2,p-l,m0,m2];
fullBubble[] := partialBubble[] QuadraticGravityVertex[{al,be,-p,a2,b2,l,i2,j2,p-l},m0,m2] external[al,be];
treeAmplitude[] := GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,al1,be1,-p1-p2] *
 FeynAmpDenominatorExplicit[GravitonPropagator[al1,be1,al2,be2,p1+p2]] *
 GravitonVertex[mu3,nu3,p3,mu4,nu4,p4,al2,be2,p1+p2] *
 PolarizationTensorD[p1,mu1,nu1] PolarizationTensorD[p2,mu2,nu2] *
 ComplexConjugate[PolarizationTensorD[p3,mu3,nu3] PolarizationTensorD[p4,mu4,nu4]];
physicalCase[id_] := case[id, Switch[id,
 "vertex-trace",external[mu,nu] QuadraticGravityVertex[{mu,nu,p,a,b,-l,c,d,l-p},m0,m2] MTD[a,b] MTD[c,d],
 "partial-bubble",partialBubble[], "full-bubble",fullBubble[], "tree-amplitude",treeAmplitude[]],
 Missing["NotAvailable"],True,If[id==="tree-amplitude",{},{l}]];

(* Deferred builders let the driver time construction without including it in
   export/import timings. This is a list of zero-argument Functions. *)
workloadBuilders[suite_, profile_] := Module[{sizes=If[profile==="Full",{100,1000,5000},{100,1000}], ids, builders},
 Switch[suite,
 "Overview", Table[With[{index=i},Function[Null,smallCases[][[index]]]],{i,3}],
 "Export", Flatten[Table[With[{name=id,size=n},Function[Null,synthetic[name,size]]],
  {id,{"repeated-products","distinct-products","repeated-propagators","distinct-propagators","simple-routing","composite-routing","repeated-symbols","distinct-symbols","factored","expanded"}},{n,sizes}]],
 "Import", Join[{Function[Null,First[smallCases[]]],
   Function[Null,case["free-indices",MTD[mu,nu]+FVD[p-l,mu]FVD[q,nu]]],
   Function[Null,case["supported-scalars",FAD[{l,m}]^2/(D-1)+A0[m^2]+B0[s,m^2,m^2]+C0[s,t,u,m^2,m^2,m^2]+D0[s,t,u,s,t,u,m^2,m^2,m^2,m^2],Missing["NotAvailable"],False,{l}]]},
   Flatten[Table[With[{name=id,size=n},Function[Null,synthetic[name,size]]],{id,{"repeated-products","distinct-products"}},{n,sizes}]],
   If[profile==="Full",{Function[Null,physicalCase["full-bubble"]]},{}]],
 "Scaling", Join[{Function[Null,First[smallCases[]]],Function[Null,synthetic["composite-routing",1000]],Function[Null,physicalCase["partial-bubble"]]},
   If[profile==="Full",{Function[Null,physicalCase["full-bubble"]]},{}]],
 "Representative", ids=Join[{"vertex-trace","partial-bubble","tree-amplitude"},If[profile==="Full",{"full-bubble"},{}]];
   Table[With[{name=id},Function[Null,physicalCase[name]]],{id,ids}]
 ]
];
