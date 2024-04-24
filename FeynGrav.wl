(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print[Style["FeynGrav 2.3",Bold]];
Print["FeynGrav: FeynGravCommands prints the list of all supported commands."];
Print["FeynGrav: On initialisation, the package only imports libraries for matter with spin s = 0, 1/2, 1, and 2 with minimal couplings up to the second order. To import additional libraries, use the \"import*\" command."];
Print["FeynGrav: Core publications describing FeynGrav functionality are ",Hyperlink["Class.Quant.Grav. 39 (2022) 16, 165006","https://doi.org/10.1088/1361-6382/ac7e15"],", ",Hyperlink["Comput.Phys.Commun. 292 (2023) 108871","https://doi.org/10.1016/j.cpc.2023.108871"],"."];


Needs["Nieuwenhuizen`",DirectoryName[$InputFileName]<>"Rules/Nieuwenhuizen.wl"];


(* Scalar sector *)


ScalarPropagator::usage = "ScalarPropagator[p,m]. Propagator of a scalar field with mass m.";
GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Interaction vertex for gravity minimally coupled to the scalar field kinetic energy. Here, \!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\) are Lorentz indices of the i-th graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\) and \!\(\*SubscriptBox[\(p\), \(2\)]\) are the scalar field momenta; m is the scalar field mass.";
GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Interaction vertex for gravity minimally coupled to the scalar field potential energy. The vertex corresponds to \!\(\*FractionBox[SubscriptBox[\(\[Lambda]\), \(p\)], \(p!\)]\)\!\(\*SuperscriptBox[\(\[Phi]\), \(p\)]\) scalar field potential. Here, \!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\) are Lorentz indices of the i-th graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\) and \!\(\*SubscriptBox[\(p\), \(2\)]\) are the scalar field momenta; \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\) is the scalar field coiupling.";


(* Fermion sector *)


GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Interaction vertex for gravity minimally coupled to the Dirac fermion. Here, \!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\) are Lorentz indices of the i-th graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\) and \!\(\*SubscriptBox[\(p\), \(2\)]\) are the fermion momenta; m is the fermion mass.";


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


ScalarGaussBonnet::usage = "ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},g]. The function returns Scalar-Gauss-Bonnet vertex for an arbitrary number of scalars and n\[GreaterEqual]2 gravitons. Here, \!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\) are Lorentz indices of a graviton, \!\(\*SubscriptBox[\(k\), \(i\)]\) is a momentum of a graviton, g is the coupling.";


(* Quadratic gravity *)


QuadraticGravityVertex::usage = "QuadraticGravityVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)].";


QuadraticGravityPropagator::usage = "QuadraticGravityPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p,\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)].";
QuadraticGravityPropagatorAlternative::usage = "QuadraticGravityPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p,\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)].";


(* Import procedures *)


importGravitons::usage = "importGravitons[n]. The command imports libraries for graviton vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importScalars::usage = "importScalars[n]. The command imports libraries for the scalar field kinetic and potential term vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importFermions::usage = "importFermions[n]. The command imports libraries for the Dirac fermion vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importVectors::usage = "importVectors[n]. The command imports libraries for vector vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importSUNYM::usage = "importSUNYM[n]. The command imports libraries for SU(N) Yang-Mills theory up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importAxionVectorVertex::usage = "importAxionVectorVertex[n]. The command imports libraries axion-like coupling to a single vector field up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importQuadraticGravity::usage = "importQuadraticGravity[n]. The command imports libraries for quadratic gravity up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG2::usage = "importHorndeskiG2[n]. The command imports libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG3::usage = "importHorndeskiG3[n]. The command imports libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG4::usage = "importHorndeskiG4[n]. The command imports libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG5::usage = "importHorndeskiG5[n]. The command imports libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importScalarGaussBonnet::usage "importScalarGaussBonnet[n]. The command imports libraries for Scalar-Gauss-Bonnet vertices up to order n\[GreaterEqual]2. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";


(* The list of commands *)


FeynGravCommands := Print["GravitonPropagator, GravitonPropagatorAlternative, GravitonPropagatorTop, GravitonPropagatorTopFAD, GravitonPropagatorMassive, GravitonPropagatorMassiveAlternative, GravitonPropagatorMassiveTop,\n","GravitonVertex, GravitonGhostVertex, PolarizationTensor, SetPolarizationTensor,\n","ScalarPropagator, GravitonScalarVertex, GravitonScalarPotentialVertex, GravitonFermionVertex,\n","ProcaPropagator, GravitonMassiveVectorVertex, GravitonVectorVertex, GravitonVectorGhostVertex,\n","GravitonGluonVertex, GravitonGluonGhostVertex, GravitonYMGhostVertex, GravitonQuarkGluonVertex,\n","GravitonAxionVectorVertex,\n","HorndeskiG2, HorndeskiG3, HorndeskiG4, HorndeskiG5,\n","QuadraticGravityPropagator, QuadraticGravityPropagatorAlternative, QuadraticGravityVertex,\n","importGravitons, importScalars, importFermions, importVectors, importSUNYM,\n","importHorndeskiG2, importHorndeskiG3, importHorndeskiG4, importHorndeskiG5,\n","importAxionVectorVertex, importQuadraticGravity."];


(* Symbols *)


FormatValues[FeynGrav`GaugeFixingEpsilon] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilon,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Gravity"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonVector] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonVector,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Vector"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonSUNYM] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonSUNYM,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","SU(N)YM"]} ;


(* Gauge parameters *)


FeynGrav`GaugeFixingEpsilon = 2;
FeynGrav`GaugeFixingEpsilonVector = -1;
FeynGrav`GaugeFixingEpsilonSUNYM = -1;


(* A make sure that the gravitational coupling has the correct context. *)


\[Kappa] =. ;


Begin["Private`"];


(* Dummy arrays *)


DummyArray = n |->Flatten[ {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] ];
DummyArrayVariables = n |->Flatten[ {ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"]}&/@Range[n] ];
DummyMomenta = n |-> ToExpression["p"<>ToString[#]]&/@Range[n];
DummyMomentaVariables = n |-> ToExpression["p"<>ToString[#]<>"_"]&/@Range[n];
DummyArrayMomenta = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaVariables = n |-> Flatten[{ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"],ToExpression["p"<>ToString[#]<>"_"]}&/@Range[n]];
DummyArrayMomentaK = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaKVariables = n |-> Flatten[{ToExpression["m"<>ToString[#]<>"_"],ToExpression["n"<>ToString[#]<>"_"],ToExpression["k"<>ToString[#]<>"_"]}&/@Range[n]];


(* Package directory *)


packageDirectory = DirectoryName[$InputFileName];


(* Graviton sector *)


Options[importGravitons] = { printOutput -> False};

importGravitons[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[nExternal, Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVertex_*", packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGhostVertex_*", packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Graviton vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVertex_*", packageDirectory]]],"."];
		Print["Graviton-ghost vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGhostVertex_*", packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonVertex,GravitonGhostVertex];
	
	Map[
		(Evaluate[GravitonVertex[Sequence@@DummyArrayMomentaVariables[#+2]]] = Get[packageDirectory<>"Libs/GravitonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGhostVertex[DummyArrayMomentaVariables[#],\[Lambda]1_,k1_,\[Lambda]2_,k2_]] = Get[packageDirectory<>"Libs/GravitonGhostVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton vertices imported up to order ",nImport,"."]
	];
];


(* Scalar sector *)


Options[importScalars] = { printOutput -> False};

importScalars[nExternal_ : 2, OptionsPattern[] ] := Block[{nImport},
	
	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonScalarVertex_*", packageDirectory]]],Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonScalarPotentialVertex_*", packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Graviton-scalar vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonScalarVertex_*", packageDirectory]]],"."];
		Print["Graviton-scalar potential vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonScalarPotentialVertex_*", packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonScalarVertex,GravitonScalarPotentialVertex];
	
	Map[
		(Evaluate[ GravitonScalarVertex[DummyArrayVariables[#],p1_,p2_,m_ ]] = Get[packageDirectory<>"Libs/GravitonScalarVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonScalarPotentialVertex[DummyArrayVariables[#],\[Lambda]_]] = Get[packageDirectory<>"Libs/GravitonScalarPotentialVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton-scalar vertices imported up to order ",nImport,"."]
	];
];


(* Fermion sector *)


Options[importFermions] = { printOutput -> False};

importFermions[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonFermionVertex*", packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Graviton-Dirac fermion vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonFermionVertex_*",packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonFermionVertex*",packageDirectory]]]],"."];
	];

	Clear[GravitonFermionVertex];
	
	Map[
		(Evaluate[GravitonFermionVertex[DummyArrayVariables[#],p1_,p2_,m_]] = Get[packageDirectory<>"Libs/GravitonFermionVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton-Dirac fermion vertices imported up to order ",nImport,"."]
	];
];


(* Vector sector *)


Options[importVectors] = { printOutput -> False};

importVectors[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},

	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonMassiveVectorVertex_*",packageDirectory]]],Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVectorVertex_*",packageDirectory]]],Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVectorGhostVertex_*",packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Graviton-massive vector vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonMassiveVectorVertex_*",packageDirectory]]],"."];
		Print["Graviton-massless vector vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVectorVertex_*",packageDirectory]]],"."];
		Print["Graviton-vector ghost vertices vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVectorGhostVertex_*",packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonMassiveVectorVertex,GravitonVectorVertex,GravitonVectorGhostVertex];
	
	Map[
		(Evaluate[GravitonMassiveVectorVertex[DummyArrayVariables[#],\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_]] = Get[packageDirectory<>"Libs/GravitonMassiveVectorVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonVectorVertex[DummyArrayMomentaKVariables[#],\[Lambda]1_,p1_,\[Lambda]2_,p2_]] = Get[packageDirectory<>"Libs/GravitonVectorVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonVectorGhostVertex[DummyArrayVariables[#],p1_,p2_]] = Get[packageDirectory<>"Libs/GravitonVectorGhostVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton-vector vertices imported up to order ",nImport,"."]
	];
];


(* SU(N) Yang-Mills sector *)


Options[importSUNYM] = { printOutput -> False};

importSUNYM[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[ nExternal, Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGluonVertex_*",packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonThreeGluonVertex_*",packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonFourGluonVertex_*",packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonQuarkGluonVertex_*",packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonYMGhostVertex_*",packageDirectory]]], Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGluonGhostVertex_*",packageDirectory]]] ];
	
	If[OptionValue[printOutput], 
		Print["Graviton-gluon-gluon vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGluonVertex_*", packageDirectory]]],"."];
		Print["Graviton-gluon-gluon-gluon vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonThreeGluonVertex_*", packageDirectory]]],"."];
		Print["Graviton-gluon-gluon-gluon-gluon vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonFourGluonVertex_*", packageDirectory]]],"."];
		Print["Graviton-quark-quark-gluon vector vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonQuarkGluonVertex_*", packageDirectory]]],"."];
		Print["Graviton-(Yang-Mills)ghost vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonYMGhostVertex_*", packageDirectory]]],"."];
		Print["Graviton-gluon-(Yang-Mills)ghost vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonGluonGhostVertex_*", packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonGluonVertex,GravitonQuarkGluonVertex,GravitonYMGhostVertex,GravitonGluonGhostVertex];
	
	Map[
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],p1_,\[Lambda]1_,a1_,p2_,\[Lambda]2_,a2_]] = Get[packageDirectory<>"Libs/GravitonGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],p1_,\[Lambda]1_,a1_,p2_,\[Lambda]2_,a2_,p3_,\[Lambda]3_,a3_]] = Get[packageDirectory<>"Libs/GravitonThreeGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],p1_,\[Lambda]1_,a1_,p2_,\[Lambda]2_,a2_,p3_,\[Lambda]3_,a3_,p4_,\[Lambda]4_,a4_]] = Get[packageDirectory<>"Libs/GravitonFourGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonQuarkGluonVertex[DummyArrayVariables[#],\[Lambda]_,a_ ]] = Get[packageDirectory<>"Libs/GravitonQuarkGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonYMGhostVertex[DummyArrayVariables[#],p1_,a1_,p2_,a2_ ]] = Get[packageDirectory<>"Libs/GravitonYMGhostVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonGhostVertex[DummyArrayVariables[#],\[Lambda]1_,a1_,p1_,\[Lambda]2_,a2_,p2_,\[Lambda]3_,a3_,p3_  ]] = Get[packageDirectory<>"Libs/GravitonGluonGhostVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton-SU(N) Yang-Mills vertices imported up to order ",nImport,"."]
	];
];


(* Graviton-Scalar Axion-Single Vector Sector *)


Options[importAxionVectorVertex] = { printOutput -> False};

importAxionVectorVertex[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonAxionVectorVertex_*",packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Axion-Vector vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonAxionVectorVertex_*",packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonAxionVectorVertex];
	
	Map[
		(Evaluate[GravitonAxionVectorVertex[DummyArrayVariables[#],\[Lambda]1_,p1_,\[Lambda]2_,p2_,\[CapitalTheta]_]  ] = Get[packageDirectory<>"Libs/GravitonAxionVectorVertex_"<>ToString[#]];)&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Axion-Vector vertices imported up to order ",nImport,"."]
	];
];


(* Horndeski G2 *)


Options[importHorndeskiG2] = { printOutput -> False};

importHorndeskiG2[OptionsPattern[] ] := Block[{indexArray},

	indexArray =Flatten[ Map[ToExpression ,StringCases[FileNames["Libs/HorndeskiG2_*",packageDirectory], "HorndeskiG2_"~~a_~~"_" ~~b_~~"_"~~n_-> {a,b,n} ], {3}] ,1];
	If[ OptionValue[printOutput],
		Print["Libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) interaction (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\)\!\(\*SuperscriptBox[\(X\), \(b\)]\))=\[ScriptCapitalO](\!\(\*SuperscriptBox[\(\[Kappa]\), \(n\)]\)) exist for :"];
		Map[ Print["a = ",#[[1]],", b = ",#[[2]],", n = ",#[[3]]," ."]&, indexArray];
		Print["Import all of them."];
	];
	
	Clear[HorndeskiG2];
	Map[
		(HorndeskiG2[DummyArrayVariables[#[[3]]],DummyMomentaVariables[#[[1]] + 2 #[[2]]], #[[2]], \[Lambda]_] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG2_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
		indexArray
	];
	If[OptionValue[printOutput],
		Print["Import is done."];
	];
];


(* Horndeski G3 *)


Options[importHorndeskiG3] = { printOutput -> False};

importHorndeskiG3[ OptionsPattern[] ] := Block[{indexArray},

	indexArray =Flatten[ Map[ToExpression ,StringCases[FileNames["Libs/HorndeskiG3_*",packageDirectory], "HorndeskiG3_"~~a_~~"_" ~~b_~~"_"~~n_-> {a,b,n} ], {3}] ,1];
	If[ OptionValue[printOutput],
		Print["Libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) interaction (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\)\!\(\*SuperscriptBox[\(X\), \(b\)] \[Square]\[Phi]\))=\[ScriptCapitalO](\!\(\*SuperscriptBox[\(\[Kappa]\), \(n\)]\)) exist for :"];
		Map[ Print["a = ",#[[1]],", b = ",#[[2]],", n = ",#[[3]]," ."]&, indexArray];
		Print["Import all of them."];
	];
	
	Clear[HorndeskiG3];
	Map[
		(HorndeskiG3[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2*#[[2]]+1],#[[2]],\[Lambda]_] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG3_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
		indexArray
	];
	If[OptionValue[printOutput],
		Print["Import is done."];
	];
];


(* Horndeski G4 *)


Options[importHorndeskiG4] = { printOutput -> False};

importHorndeskiG4[ OptionsPattern[] ] := Block[{indexArray},

	indexArray =Flatten[ Map[ToExpression ,StringCases[FileNames["Libs/HorndeskiG4_*",packageDirectory], "HorndeskiG4_"~~a_~~"_" ~~b_~~"_"~~n_-> {a,b,n} ], {3}] ,1];
	If[ OptionValue[printOutput],
		Print["Libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) interaction ( \!\(\*SqrtBox[\(-g\)]\) R \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\))=\[ScriptCapitalO](\!\(\*SuperscriptBox[\(\[Kappa]\), \(n\)]\)) exist for :"];
		Map[ Print["a = ",#[[1]],", b = ",#[[2]],", n = ",#[[3]]," ."]&, indexArray];
		Print["Import all of them."];
	];
	
	Clear[HorndeskiG4];
	Map[
		(HorndeskiG4[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2*#[[2]]],#[[2]],\[Lambda]_] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG4_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
		indexArray
	];
	If[OptionValue[printOutput],
		Print["Import is done."];
	];
];


(* Horndeski G5 *)


Options[importHorndeskiG5] = { printOutput -> False};

importHorndeskiG5[ OptionsPattern[] ] := Block[{indexArray},

	indexArray =Flatten[ Map[ToExpression ,StringCases[FileNames["Libs/HorndeskiG5_*",packageDirectory], "HorndeskiG5_"~~a_~~"_" ~~b_~~"_"~~n_-> {a,b,n} ], {3}] ,1];
	If[ OptionValue[printOutput],
		Print["Libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) interaction ( \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(G\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Del]\), \(\[Mu]\[Nu]\)]\)\[Phi] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\))=\[ScriptCapitalO](\!\(\*SuperscriptBox[\(\[Kappa]\), \(n\)]\)) exist for :"];
		Map[ Print["a = ",#[[1]],", b = ",#[[2]],", n = ",#[[3]]," ."]&, indexArray];
		Print["Import all of them."];
	];
	
	Clear[HorndeskiG5];
	Map[
		(HorndeskiG5[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2 #[[2]]],#[[2]],\[Lambda]_] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG5_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
		indexArray
	];
	If[OptionValue[printOutput],
		Print["Import is done."];
	];
];


(* Scalar-Gauss-Bonnet *)


Options[importScalarGaussBonnet] = { printOutput -> False};

importScalarGaussBonnet[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/ScalarGaussBonnet_*", packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Scalar-Gauss-Bonnet vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/ScalarGaussBonnet_*",packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/ScalarGaussBonnet*",packageDirectory]]]],"."];
	];

	Clear[ScalarGaussBonnet];
	
	Map[
		(Evaluate[ScalarGaussBonnet[DummyArrayMomentaKVariables[#],g_]] = g Get[packageDirectory<>"Libs/ScalarGaussBonnet_"<>ToString[#]])&,
		Range[2,nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Scalar-Gauss-Bonnet fermion vertices imported up to order ",nImport,"."]
	];
];


(* Quadratic gravity *)


Options[importQuadraticGravity] = { printOutput -> False};

importQuadraticGravity[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},

	nImport = Min[nExternal,Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/QuadraticGravityVertex_*",packageDirectory]]]];
	
	If[OptionValue[printOutput], 
		Print["Quadratic gravity vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/QuadraticGravityVertex_*",packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[QuadraticGravityVertex];
	
	Map[
		(Evaluate[QuadraticGravityVertex[DummyArrayMomentaVariables[#+2],ToExpression["\[GothicM]0_"],ToExpression["\[GothicM]2_"]]] = Get[packageDirectory<>"Libs/QuadraticGravityVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Quadratic gravity vertices imported up to order ",nImport,"."]
	];
];


(* Propagators *)


ScalarPropagator[p_,m_] = I FAD[{p,m}];


ProcaPropagator[\[Mu]_,\[Nu]_,p_,m_] = (-I)(MTD[\[Mu],\[Nu]]-FVD[p,\[Mu]]FVD[p,\[Nu]]/m^2)FAD[{p,m}];


GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_] := (-(1/2) Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p] + Nieuwenhuizen`NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p] -((3 FeynGrav`GaugeFixingEpsilon - 8)/(2 FeynGrav`GaugeFixingEpsilon))Nieuwenhuizen`NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]-1/2 Nieuwenhuizen`NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]) //Expand;
GravitonPropagatorTopFAD[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_] := (-(1/2) Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator1FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + Nieuwenhuizen`NieuwenhuizenOperator2FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] -((3 FeynGrav`GaugeFixingEpsilon - 8)/(2 FeynGrav`GaugeFixingEpsilon))Nieuwenhuizen`NieuwenhuizenOperator0BarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]-1/2 Nieuwenhuizen`NieuwenhuizenOperator0BarBarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]) //FeynAmpDenominatorCombine;
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_] := I GravitonPropagatorTopFAD[\[Mu],\[Nu],\[Alpha],\[Beta],k] FAD[k] //FeynAmpDenominatorCombine ;
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_] := I (GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta],k])/Pair[Momentum[k,D],Momentum[k,D]] //Expand;


GravitonPropagatorMassiveTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_] = ( 1/2 ( (MTD[\[Mu],\[Alpha]]-FVD[p,\[Mu]]FVD[p,\[Alpha]]/m^2)(MTD[\[Nu],\[Beta]]-FVD[p,\[Nu]]FVD[p,\[Beta]]/m^2)+(MTD[\[Mu],\[Beta]]-FVD[p,\[Mu]]FVD[p,\[Beta]]/m^2)(MTD[\[Nu],\[Alpha]]-FVD[p,\[Nu]]FVD[p,\[Alpha]]/m^2) ) - 1/(D-1) (MTD[\[Mu],\[Nu]]-FVD[p,\[Mu]]FVD[p,\[Nu]]/m^2)(MTD[\[Alpha],\[Beta]]-FVD[p,\[Alpha]]FVD[p,\[Beta]]/m^2) )  //FeynCalcInternal//Expand;
GravitonPropagatorMassive[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:=(-I)FAD[{p,m}] GravitonPropagatorMassiveTop[\[Mu],\[Nu],\[Alpha],\[Beta],p,m] //FeynAmpDenominatorCombine;
GravitonPropagatorMassiveAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:=(-I) GravitonPropagatorMassiveTop[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]/(SPD[p,p]-m^2) //FeynCalcInternal//Expand;


QuadraticGravityPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m0_,m2_]:= m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator0FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]  - m2^2 FAD[p,{p,m2}] Nieuwenhuizen`NieuwenhuizenOperator2FAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + ( 4/FeynGrav`GaugeFixingEpsilon FAD[p] + (3 m0^2)/2 FAD[p,{p,m0}] ) Nieuwenhuizen`NieuwenhuizenOperator0BarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p] + m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0BarBarFAD[\[Mu],\[Nu],\[Alpha],\[Beta],p]  //FeynAmpDenominatorCombine;
QuadraticGravityPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m0_,m2_]:= (m0^2/2) 1/(SPD[p,p](SPD[p,p]-m0^2)) Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p] + 2/FeynGrav`GaugeFixingEpsilon Nieuwenhuizen`NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]  - (m2^2)/( SPD[p,p] (SPD[p,p]-m2^2) ) Nieuwenhuizen`NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p] + ( (4/FeynGrav`GaugeFixingEpsilon) (1/SPD[p,p]) + (3/2 m0^2) 1/(SPD[p,p] (SPD[p,p]-m0^2) ) ) Nieuwenhuizen`NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p] + m0^2/2 FAD[p,{p,m0}] Nieuwenhuizen`NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p] //FeynCalcInternal//Expand ;


(* Polarisation tensors *)


PolarizationTensor[\[Mu]_,\[Nu]_,p_] = Pair[Momentum[Polarization[p,I],D],LorentzIndex[\[Mu],D]]Pair[Momentum[Polarization[p,I],D],LorentzIndex[\[Nu],D]];
SetPolarizationTensor := Module[{},
	Pair[Momentum[Polarization[x_,I],D],Momentum[Polarization[x_,I],D]]=0;
	Pair[Momentum[Polarization[x_,I]],Momentum[Polarization[x_,I]]]=0;
	Pair[Momentum[Polarization[x_,I],D],Momentum[x_,D]]=0;
	Pair[Momentum[Polarization[x_,I]],Momentum[x_]]=0;
];


importGravitons[2];
importScalars[2];
importFermions[2];
importVectors[2];


End[];


EndPackage[];
