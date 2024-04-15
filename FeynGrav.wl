(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];
Print[Style["FeynGrav 2.2",Bold]];
Print["FeynGrav: FeynGravCommands print the list of all supported commands."];
Print["FeynGrav: Examples can be found in FeynGrav_Examples.nb and arXiv:2201.06812."];
Print["Core publications: arXiv:2201.06812, arXiv:2302.14310."];


SetDirectory[DirectoryName[$InputFileName]];
Needs["Nieuwenhuizen`","./Rules/Nieuwenhuizen.wl"];
SetDirectory[DirectoryName[$InputFileName]];


(* Scalar sector *)


GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a scalar field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, m is the scalar field mass.";
GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Expression for gravitational interaction of a scalar field potential energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitational indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\) is the scalar field self-coupling constant.";
ScalarPropagator::usage = "ScalarPropagator[p,m]. Propagator of a scalar field with mass m.";


(* Fermion sector *)


GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a Dirac field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, m is the fermion mass.";


(* Vector sector *)


GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Expression for gravitational interaction of a massless vector field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(k\), \(i\)]\) are graviton momenta, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector fields indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector fields momenta.";
GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a  massive vector field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector fields indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector fields momenta, m is the vector field mass.";
GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Expression for gravitational interaction of the Faddeev-Popov ghost kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are ghosts momenta.";
ProcaPropagator::usage = "ProcaPropagator[\[Mu],\[Nu],p,m]. The Proca field propagator.";


(* SU(N) Yang-Mills sector *)


GravitonGluonVertex::usage = "GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(l\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\),\!\(\*SubscriptBox[\(a\), \(l\)]\)]. The function returns an expression for the gravitational vertex of 2,3, and 4 gluon vertices. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";
GravitonQuarkGluonVertex::usage = "GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda],a]. The function returns an expression for the gravitational vertex for quark-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\[Lambda],a} are the quark-gluon vertex parameters.";
GravitonYMGhostVertex::usage = "GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. The function returns an expression for the gravitational vertex for ghosts. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are ghost parameters.";
GravitonGluonGhostVertex::usage = "GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. The function returns an expression for the gravitational vertex for gluon-ghost vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluon and ghost parameters.";


(* Graviton sector *)


GravitonVertex::usage = "GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons.";
GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons, {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(p\), \(i\)]\)} are ghosts indices and momenta.";


GravitonPropagatorTop::usage = "GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nominator of the graviton propagator.";
GravitonPropagatorTopFAD::usage = "GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nominator of the graviton propagator realized with FAD function.";
GravitonPropagator::usage = "GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator realized with FAD function. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum. ";
GravitonPropagatorAlternative::usage = "GravitonPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator realized without FAD. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum.";


(* Massive gravity *)


GravitonPropagatorMassiveTop::usage = "GravitonPropagatorMassiveTop[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]. Nominator of the massive graviton propagator.";
GravitonPropagatorMassive::usage = "GravitonPropagatorMassive[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]. Massive graviton propagator realized with FAD function. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum, m is the graviton mass.";
GravitonPropagatorMassiveAlternative::usage = "GravitonPropagatorMassiveAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]. Massive graviton propagator realized without FAD function. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum, m is the graviton mass.";


(* Polarisation tensor *)


PolarizationTensor::usage = "PolarizationTensor[\[Mu],\[Nu],p]. Polarization tensor for the graviton in D dimensions. The tensor is constructed from the standard polarization vectors. This definition is neither traceless nor transverse.";
SetPolarizationTensor::usage = "The command makes the graviton polarization tensor being traceless and transverse.";


(* Axion sector *)


GravitonAxionVectorVertex::usage = "GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. The function returns the gravitational vertex for coupling of scalar axions to the U(1) field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(q\), \(i\)]\) are momenta of vectors, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector Lorentz indices, and \[Theta] is the coupling.";


(* Horndeski *)


HorndeskiG2::usage = "HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b,\[Lambda]]. The function returns the Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) interaction vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of scalars, b is the number of kinetic terms, and \[Lambda] is the coupling.";


HorndeskiG3::usage = "HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b,\[Lambda]]. The function returns the Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) interaction vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(k\), \(i\)]\) are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of scalars, b is the number of kinetic terms, and \[Lambda] is the coupling.";


HorndeskiG4::usage = "HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b \)]\)},b,\[Lambda]]. The function returns the Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) interaction vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(k\), \(i\)]\) are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of scalars, b is the number of kinetic terms, and \[Lambda] is the coupling.";


HorndeskiG5::usage = "HorndeskiG5[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1 \)]\)},b,\[Lambda]]. The function returns the Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) interaction vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(k\), \(i\)]\) are graviton momenta, \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of scalars, b is the number of kinetic terms, and \[Lambda] is the coupling.";


(* Quadratic gravity *)


QuadraticGravityVertex::usage = "QuadraticGravityVertex[{\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(m\), \(n\)]\),\!\(\*SubscriptBox[\(n\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\[Alpha],\[Beta],\[Rho]].";


QuadraticGravityPropagator::usage = "QuadraticGravityPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p,\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)].";
QuadraticGravityPropagatorAlternative::usage = "QuadraticGravityPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p,\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)].";


FeynGravCommands := Print["GravitonPropagator","GravitonPropagatorTop","GravitonPropagatorTopFAD","GravitonPropagatorAlternative","GravitonPropagatorMassive","GravitonPropagatorMassiveTop","GravitonPropagatorMassiveAlternative","GravitonVertex","GravitonGhostVertex","PolarizationTensor","SetPolarizationTensor","ScalarPropagator","GravitonScalarVertex","GravitonScalarPotentialVertex","HorndeskiG2","ProcaPropagator","GravitonVectorVertex","GravitonVectorGhostVertex","GravitonMassiveVectorVertex","GravitonAxionVectorVertex","GravitonFermionVertex","GravitonQuarkGluonVertex","GravitonGluonVertex","GravitonGluonGhostVertex","GravitonYMGhostVertex"];


SetDirectory[DirectoryName[$InputFileName]];


(* Symbols *)


FormatValues[FeynGrav`GaugeFixingEpsilon] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilon,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Gravity"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonVector] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonVector,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Vector"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonSUNYM] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonSUNYM,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","SU(N)YM"]} ;


(* Gauge parameters *)


FeynGrav`GaugeFixingEpsilon = 2;
FeynGrav`GaugeFixingEpsilonVector = -1;
FeynGrav`GaugeFixingEpsilonSUNYM = -1;


(* Dummy arrays *)


DummyArray = n |->Flatten[ {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] ];
DummyArrayVariables = n |->Flatten[ {ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"]}&/@Range[n] ];
DummyMomenta = n |-> ToExpression["p"<>ToString[#]]&/@Range[n];
DummyMomentaVariables = n |-> ToExpression["p"<>ToString[#]<>"_"]&/@Range[n];
DummyArrayMomenta = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaVariables = n |-> Flatten[{ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"],ToExpression["p"<>ToString[#]<>"_"]}&/@Range[n]];
DummyArrayMomentaK = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaKVariables = n |-> Flatten[{ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"],ToExpression["k"<>ToString[#]<>"_"]}&/@Range[n]];


(* Graviton sector *)


Block[{cursor,\[Lambda]1,k1,\[Lambda]2,k2},

	cursor = 1;
	
	Clear[GravitonVertex];
	
	While[FileExistsQ["./Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex[Sequence@@DummyArrayMomentaVariables[cursor+2]]] = Get["./Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonGhostVertex];
	
	While[FileExistsQ["./Libs/GravitonGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonGhostVertex[DummyArrayMomentaVariables[cursor],\[Lambda]1_,k1_,\[Lambda]2_,k2_]] = Get["./Libs/GravitonGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Scalar sector *)


Block[{cursor,m,p1,p2},

	cursor = 1;

	Clear[GravitonScalarVertex];
	
	While[FileExistsQ["./Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[ GravitonScalarVertex[DummyArrayVariables[cursor],p1_,p2_,m_ ]] = Get["./Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
]


Block[{cursor,\[Lambda]},

	cursor = 1;
	
	Clear[GravitonScalarPotentialVertex];
	
	While[FileExistsQ["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarPotentialVertex[DummyArrayVariables[cursor],\[Lambda]_]] = Get["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]];
		cursor++;
	];
]


(* Fermion sector *)


Block[{cursor,p1,p2,m},

	cursor = 1;

	Clear[GravitonFermionVertex];
	
	While[FileExistsQ["./Libs/GravitonFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonFermionVertex[DummyArrayVariables[cursor],p1_,p2_,m_]] = Get["./Libs/GravitonFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
]


(* Vector sector *)


Block[{cursor,p1,p2,\[Lambda]1,\[Lambda]2,m},	

	cursor = 1;
	
	Clear[GravitonMassiveVectorVertex];
	
	While[FileExistsQ["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonMassiveVectorVertex[DummyArrayVariables[cursor],\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_]] = Get["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonVectorVertex];
	
	While[FileExistsQ["./Libs/GravitonVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorVertex[DummyArrayMomentaKVariables[cursor],\[Lambda]1_,p1_,\[Lambda]2_,p2_]] = Get["./Libs/GravitonVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonVectorGhostVertex];
	
	While[FileExistsQ["./Libs/GravitonVectorGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorGhostVertex[DummyArrayVariables[cursor],p1_,p2_]] = Get["./Libs/GravitonVectorGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
		
	Print["Graviton-Matter vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* SU(N) Yang-Mills sector *)


Block[{cursor,a1,a2,a3,a4,\[Lambda]1,\[Lambda]2,\[Lambda]3,\[Lambda]4,p1,p2,p3,p4},

	cursor = 1;
	
	Clear[GravitonGluonVertex];
	
	While[FileExistsQ["./Libs/GravitonGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayMomentaK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2}]] = Get["./Libs/GravitonGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	While[FileExistsQ["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayMomentaK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3}]] = Get["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	While[FileExistsQ["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayMomentaK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4}]] = Get["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonQuarkGluonVertex];
	
	While[FileExistsQ["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonQuarkGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda],a} ]] = Get["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonYMGhostVertex];
	
	While[FileExistsQ["./Libs/GravitonYMGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonYMGhostVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,a1,p2,a2} ]] = Get["./Libs/GravitonYMGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	cursor = 1;
	
	Clear[GravitonGluonGhostVertex];
	
	While[FileExistsQ["./Libs/GravitonGluonGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonGhostVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]1,a1,p1,\[Lambda]2,a2,p2,\[Lambda]3,a3,p3}  ]] = Get["./Libs/GravitonGluonGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	Print["Graviton-SU(N) Yang-Mills vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Graviton-Scalar Axion-Single Vector Sector *)


Block[{cursor,\[Lambda]1,p1,\[Lambda]2,p2,\[CapitalTheta]},

	cursor = 1;
	
	Clear[GravitonAxionVectorVertex];
	
	While[FileExistsQ["./Libs/GravitonAxionVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonAxionVectorVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]1,p1,\[Lambda]2,p2,\[CapitalTheta]}  ]  ] = Get["./Libs/GravitonAxionVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	Print["Graviton-Scalar Axion-Single Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Horndeski G2 *)


Block[{cursor,a},

	Clear[HorndeskiG2];
	
	(* b=1 family *)
	For[ a = 1, a <= 4, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[cursor] ],
			HorndeskiG2[DummyArrayVariables[cursor],DummyMomentaVariables[a+2],1,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b=2 family *)
	For[ a = 0, a <= 2, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[cursor] ],
			HorndeskiG2[DummyArrayVariables[cursor],DummyMomentaVariables[a+4],2,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	Print["Horndeski G2 vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Horndeski G3 *)


Block[{cursor,a},

	Clear[HorndeskiG3];
	
	(* b = 0 *)
	For[ a = 2, a <= 5, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[cursor] ],
			HorndeskiG3[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+1],0,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b = 1 *)
	For[ a = 0, a <= 3, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[cursor] ],
			HorndeskiG3[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+2+1],1,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b = 2 *)
	For[ a = 0, a <= 1, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[cursor] ],
			HorndeskiG3[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+2*2+1],2,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	Print["Horndeski G3 vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Horndeski G4 *)


Block[{cursor,a},

	Clear[HorndeskiG4];
	
	(* b = 0 *)
	For[ a = 1, a <= 4, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG4_1_0_"<>ToString[cursor] ],
			HorndeskiG4[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a],0,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG4_1_0_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b = 1 *)
	For[ a = 0, a <= 2, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[cursor] ],
			HorndeskiG4[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+2],1,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b = 2 *)
	For[ a = 0, a <= 0, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG4_"<>ToString[a]<>"_2_"<>ToString[cursor] ],
			HorndeskiG4[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+2*2],2,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG4_"<>ToString[a]<>"_2_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	Print["Horndeski G4 vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Horndeski G5 *)


Block[{cursor,a},

	Clear[HorndeskiG5];
	
	(* b = 0 *)
	For[ a = 1, a <= 4, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[cursor] ],
			HorndeskiG5[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a],0,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	(* b = 1 *)
	For[ a = 1, a <= 2, a++,
		cursor = 1;
		While[FileExistsQ[ "./Libs/HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[cursor] ],
			HorndeskiG5[DummyArrayMomentaKVariables[cursor],DummyMomentaVariables[a+2],1,\[Lambda]_] = \[Lambda] Get[ "./Libs/HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[cursor] ];
			cursor++;
		];
	];
	
	Print["Horndeski G5 vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
]


(* Quadratic gravity *)
(* QuadraticGravityVertex *)


Block[{cursor,\[Lambda]1,k1,\[Lambda]2,k2},

	cursor = 1;
	
	Clear[QuadraticGravityVertex];
	
	While[FileExistsQ["./Libs/QuadraticGravityVertex_"<>ToString[cursor]],
		Evaluate[QuadraticGravityVertex[DummyArrayMomentaVariables[cursor+2],\[Alpha]_,\[Beta]_,\[CurlyEpsilon]_]] = Get["./Libs/QuadraticGravityVertex_"<>ToString[cursor]];
		cursor++;
	];
	
	Print["Quadratic gravity vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	
	Remove[\[Alpha],\[Beta],\[CurlyEpsilon]];
]


(* Cleanup *)


Remove[a,a1,a2,a3,a4,b,cursor,k1,k2,k3,m,m1,m2,m3,m4,m5,n,n1,n2,n3,n4,n5,p1,p2,p3,p4,p5,p6,\[CapitalTheta],\[Lambda],\[Lambda]1,\[Lambda]2,\[Lambda]3,\[Lambda]4];


Remove["DummyArray","DummyArrayMomenta","DummyArrayMomentaK","DummyArrayMomentaKVariables","DummyArrayMomentaVariables","DummyArrayVariables","DummyMomenta","DummyMomentaVariables"];


ResetDirectory[];


Begin["Private`"];


ScalarPropagator[p_,m_] = I FAD[{p,m}];


ProcaPropagator[\[Mu]_,\[Nu]_,p_,m_]=(-I)(MTD[\[Mu],\[Nu]]-FVD[p,\[Mu]]FVD[p,\[Nu]]/m^2)FAD[{p,m}];


GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_]:=(-(1/2) Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p] + Nieuwenhuizen`NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p] -((3 FeynGrav`GaugeFixingEpsilon - 8)/(2 FeynGrav`GaugeFixingEpsilon))Nieuwenhuizen`NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]-1/2 Nieuwenhuizen`NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p])//Calc;
GravitonPropagatorTopFAD[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_]:=(-(1/2) Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator1FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + Nieuwenhuizen`NieuwenhuizenOperator2FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] -((3 FeynGrav`GaugeFixingEpsilon - 8)/(2 FeynGrav`GaugeFixingEpsilon))Nieuwenhuizen`NieuwenhuizenOperator0BarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]-1/2 Nieuwenhuizen`NieuwenhuizenOperator0BarBarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p])//Calc;
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]:= I GravitonPropagatorTopFAD[\[Mu],\[Nu],\[Alpha],\[Beta],k] FAD[k] //Calc;
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]:=I (GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta],k])/SPD[k,k];


GravitonPropagatorMassiveTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:= ( 1/2 ( (MTD[\[Mu],\[Alpha]]-FVD[p,\[Mu]]FVD[p,\[Alpha]]/m^2)(MTD[\[Nu],\[Beta]]-FVD[p,\[Nu]]FVD[p,\[Beta]]/m^2)+(MTD[\[Mu],\[Beta]]-FVD[p,\[Mu]]FVD[p,\[Beta]]/m^2)(MTD[\[Nu],\[Alpha]]-FVD[p,\[Nu]]FVD[p,\[Alpha]]/m^2) ) - 1/(D-1) (MTD[\[Mu],\[Nu]]-FVD[p,\[Mu]]FVD[p,\[Nu]]/m^2)(MTD[\[Alpha],\[Beta]]-FVD[p,\[Alpha]]FVD[p,\[Beta]]/m^2) ) //Calc ;
GravitonPropagatorMassive[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:=(-I)FAD[{p,m}] GravitonPropagatorMassiveTop[\[Mu],\[Nu],\[Alpha],\[Beta],p,m] //Calc;
GravitonPropagatorMassiveAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:=(-I) GravitonPropagatorMassiveTop[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]/(SPD[p,p]-m^2) //Calc;


QuadraticGravityPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m0_,m2_]:= m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]  - m2^2 FAD[p,{p,m2}] Nieuwenhuizen`NieuwenhuizenOperator2FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + ( 4/FeynGrav`GaugeFixingEpsilon FAD[p] + (3 m0^2)/2 FAD[p,{p,m0}] ) Nieuwenhuizen`NieuwenhuizenOperator0BarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0BarBarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] //Calc//FeynAmpDenominatorCombine ;
QuadraticGravityPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m0_,m2_]:= (m0^2/2) 1/(SPD[p,p](SPD[p,p]-m0^2)) Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]  - (m2^2)/( SPD[p,p] (SPD[p,p]-m2^2) ) Nieuwenhuizen`NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p] + ( (4/FeynGrav`GaugeFixingEpsilon) (1/SPD[p,p]) + (3/2 m0^2) 1/(SPD[p,p] (SPD[p,p]-m0^2) ) ) Nieuwenhuizen`NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p] + m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p] //Calc ;


PolarizationTensor={\[Mu],\[Nu],p}|->Pair[Momentum[Polarization[p,I],D],LorentzIndex[\[Mu],D]]Pair[Momentum[Polarization[p,I],D],LorentzIndex[\[Nu],D]];
SetPolarizationTensor := Module[{},
	Pair[Momentum[Polarization[x_,I],D],Momentum[Polarization[x_,I],D]]=0;
	Pair[Momentum[Polarization[x_,I]],Momentum[Polarization[x_,I]]]=0;
	Pair[Momentum[Polarization[x_,I],D],Momentum[x_,D]]=0;
	Pair[Momentum[Polarization[x_,I]],Momentum[x_]]=0;
];


End[];


EndPackage[];
