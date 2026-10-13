(* Regression check: FeynGrav must load and work without FeynArts present,
   and must not create or load the FeynArts context. *)
$HistoryLength=0;
Get["FeynGrav`"];
Print["FEYNARTS_LOADED=",MemberQ[$Packages,"FeynArts`"]];
Print["FEYNARTS_SYMBOLS=",Length[Names["FeynArts`*"]]];
Print["FEYNARTS_ON_CONTEXTPATH=",MemberQ[$ContextPath,"FeynArts`"]];
Print["PACKAGE_INITIALISED=",TrueQ[FeynGrav`FeynGravInitialized]];
vertex=FeynGrav`GravitonVertex[Lor1,Lor2,p1,Lor3,Lor4,p2,Lor5,Lor6,p3,Lor7,Lor8,p4];
Print["QUARTIC_VERTEX_WORKS=",Head[vertex]=!=FeynGrav`GravitonVertex," HEAD=",Head[vertex]];
prop=FeynGrav`GravitonPropagator[Lor1,Lor2,Lor3,Lor4,p];
Print["PROPAGATOR_WORKS=",Head[prop]=!=FeynGrav`GravitonPropagator];
Print["MODELDIRECTORY_SYMBOL_IS_PRIVATE=",Context[FeynGrav`Private`FeynGravModelDirectory]==="FeynGrav`Private`"];
Quit[0];
