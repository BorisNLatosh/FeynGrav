(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


(* Function to display initialization messages for FeynGrav package. *)


DisplayInitializationMessages[] := Module[{},
  Print[Style["FeynGrav 3.0", Bold, 16]];
  
  Print[Style["FeynGrav: ", Bold], 
        "Use ", 
        Button[Style["FeynGravCommands", Underlined, Blue], 
               FeynGravCommands[], 
               Appearance -> None], 
        " to print the list of all commands."];

  Print[Style["FeynGrav: ", Bold], 
        "On initialization, the package only imports libraries for matter with spin s = 0, 1/2, 1, and 2 with minimal couplings up to the second order. To import additional libraries, use the \"import*\" command."];

  Print[Style["FeynGrav: Core publications on FeynGrav are ", Bold], 
      Row[{Style[Hyperlink["Class.Quant.Grav. 39 (2022) 16, 165006", 
                          "https://doi.org/10.1088/1361-6382/ac7e15"], Blue, Underlined], 
           ", ", 
           Style[Hyperlink["Comput.Phys.Commun. 292 (2023) 108871", 
                          "https://doi.org/10.1016/j.cpc.2023.108871"], Blue, Underlined], 
           ", ", 
           Style[Hyperlink["Comput.Phys.Commun. 310 (2025) 109508", 
                          "https://doi.org/10.1016/j.cpc.2025.109508"], Blue, Underlined]}]
	];

];


(* Ensure the package is initialized only once. *)


If[!ValueQ[FeynGravInitialized],
  DisplayInitializationMessages[];
  FeynGravInitialized = True;
];


(* The package imports the Nieuwenhuizen operators and gauge projectors. *)


Needs["Nieuwenhuizen`",DirectoryName[$InputFileName]<>"Rules/Nieuwenhuizen.wl"];


(* Description of the functions. *)


(* Scalars. *)


ScalarPropagator::usage = "ScalarPropagator[p,m]. Propagator of a scalar. Here p is the scalar momentum; m is the scalar mass.";

GravitonScalarVertex::usage = 
"GravitonScalarVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for the scalar field kinetic energy coupled to one graviton. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; m is the mass of the scalar.\n\
GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for the scalar field kinetic energy coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; m is the mass of the scalar.\n\
GravitonScalarVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\) }, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for the scalar field kinetic energy coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; m is the mass of the scalar.";

GravitonScalarPotentialVertex::usage = 
"GravitonScalarPotentialVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Vertex for the scalar field potential energy (\[Lambda]/p!) \!\(\*SuperscriptBox[\(\[Phi]\), \(p\)]\) coupled to one graviton. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; \[Lambda] is the coupling constant.\n\
GravitonScalarPotentialVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\) }, \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Vertex for the scalar field potential energy (\[Lambda]/p!) \!\(\*SuperscriptBox[\(\[Phi]\), \(p\)]\) coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \[Lambda] is the coupling constant.\n\
GravitonScalarPotentialVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\) }, \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Vertex for the scalar field potential energy (\[Lambda]/p!) \!\(\*SuperscriptBox[\(\[Phi]\), \(p\)]\) coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \[Lambda] is the coupling constant.";


(* Dirac fermions. *)


(*GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for the Dirac fermion. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of fermions; m is the mass of the fermion.";*)


GravitonFermionVertex::usage = 
"GravitonFermionVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a Dirac fermion coupled to one graviton. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of fermions; m is the fermion mass.\n\
GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a Dirac fermion coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of fermions; m is the fermion mass.\n\
GravitonFermionVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\) }, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a Dirac fermion coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of fermions; m is the fermion mass.";


(* Vectors. *)


ProcaPropagator::usage = "ProcaPropagator[\[Mu],\[Nu],p,m]. Propagator of a massive vector field. Here p is the momenutm of the vector momentum; \[Mu] and \[Nu] are Lorentz inices of the vector; m is the mass of the vector.";

GravitonVectorVertex::usage = 
"GravitonVectorVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for a massless vector field coupled to one graviton. The gauge-fixing parameter is already chosen and enters the expression. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors.\n\
GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)}, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for a massless vector field coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors.\n\
GravitonVectorVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(k\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\), \!\(\*SubscriptBox[\(k\), \(n\)]\)}, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for a massless vector field coupled to n gravitons. The gauge-fixing parameter is already chosen and enters the expression. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors.";

GravitonMassiveVectorVertex::usage = 
"GravitonMassiveVectorVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a massive vector field coupled to one graviton. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors; m is the vector mass.\n\
GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)}, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a massive vector field coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors; m is the vector mass.\n\
GravitonMassiveVectorVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\) }, \!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\), m]. Vertex for a massive vector field coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of vectors; m is the vector mass.";

GravitonVectorGhostVertex::usage = 
"GravitonVectorGhostVertex[{\[Rho], \[Sigma]}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost associated with a massless vector field coupled to one graviton. Here {\[Rho], \[Sigma]} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of ghosts.\n\
GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)}, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost associated with a massless vector field coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of ghosts.\n\
GravitonVectorGhostVertex[{ \!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\), \[Ellipsis], \!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\) }, \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost associated with a massless vector field coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\), \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of ghosts.";


(* SU(N) Yang-Mills theory. *)


GravitonGluonVertex::usage = 
"GravitonGluonVertex[{\[Rho],\[Sigma],k},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(l\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\),\!\(\*SubscriptBox[\(a\), \(l\)]\)]. Vertex for the gluons coupled to one graviton. It describes the interaction with 2,3, and 4 gluons. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; k is the momenutm of a graviton; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of gluonsl; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of gluons.\n 
GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(l\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\),\!\(\*SubscriptBox[\(a\), \(l\)]\)]. Vertex for the gluons coupled to two gravitons. It describes the interaction with 2,3, and 4 gluons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are momenta of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of gluonsl; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of gluons.\n 
GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(l\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\),\!\(\*SubscriptBox[\(a\), \(l\)]\)]. Vertex for the gluons coupled to n gravitons. It describes the interaction with 2,3, and 4 gluons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are momenta of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of gluonsl; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of gluons.";

GravitonQuarkGluonVertex::usage = 
"GravitonQuarkGluonVertex[{\[Rho],\[Sigma]},\[Lambda],a]. Vertex for quark-gluon interaction coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of gravitons; \[Lambda] is the gluon Lorentz index; a is the gluon color index.\n 
GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)},\[Lambda],a]. Vertex for quark-gluon interaction coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \[Lambda] is the gluon Lorentz index; a is the gluon color index.\n 
GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda],a]. Vertex for quark-gluon interaction. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons coupled to n gravitons; \[Lambda] is the gluon Lorentz index; a is the gluon color index.";

GravitonYMGhostVertex::usage =
"GravitonYMGhostVertex[{\[Rho],\[Sigma]},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost for gluons coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts.\n
GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost for gluons coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts.\n
GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost for gluons coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts.";

GravitonGluonGhostVertex::usage =
"GravitonGluonGhostVertex[{\[Rho],\[Sigma]},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost interaction with gluons coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are Lorentz indices of gluons; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts and gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts and gluons. The order of momenta and color indices matches the one used in FeynCalc.\n
GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost interaction with gluons coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are Lorentz indices of gluons; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts and gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts and gluons. The order of momenta and color indices matches the one used in FeynCalc.\n
GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost interaction with gluons coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are Lorentz indices of gluons; \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of ghosts and gluons; \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices of ghosts and gluons. The order of momenta and color indices matches the one used in FeynCalc.";


(* General Relativity. *)


GravitonVertex::usage =
"GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]. Vertex for the 3-graviton interaction in general relativity. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta. The gauge-fixing parameter is already fixed and enters the expression.\n
GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\)]. Vertex for the 4-graviton interaction in general relativity. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta. The gauge-fixing parameter is already fixed and enters the expression.\n
GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)]. Vertex for the n-graviton (n \!\(\*FormBox[\"\\[GreaterEqual]\",TraditionalForm]\) 3) interaction in general relativity. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta. The gauge-fixing parameter is already fixed and enters the expression.";

GravitonGhostVertex::usage =
"GravitonGhostVertex[\[Rho],\[Sigma],k,\[Mu],\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Nu],\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for the Faddeev\[Dash]Popov ghost in general relativity obtained via the BRST formalism, coupled to one graviton. Here {\[Rho],\[Sigma]} are the graviton Lorentz indices and \(k\) is the graviton momentum; \[Mu] and \[Nu] are the ghost and antighost Lorentz indices; \!\(\*SubscriptBox[\(p\), \(1\)]\) and \!\(\*SubscriptBox[\(p\), \(2\)]\) are the ghost and antighost momenta.";


GravitonPropagator::usage = "GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator. The gauge fixing parameter is already fixed and enters the expression. Here \[Mu],\[Nu],\[Alpha], and \[Beta] are Lorentz indices; p is the graviton momentum. The expression uses FAD function from FeynCalc, so it is more suitable for loop calculations.";


(* Massive gravity propagator. *)


GravitonPropagatorMassive::usage = "GravitonPropagatorMassive[\[Mu],\[Nu],\[Alpha],\[Beta],p,m]. Massive graviton propagator. Here \[Mu],\[Nu],\[Alpha], and \[Beta] are Lorentz indices; p is the graviton momentum; m is the graviton mass. The expression uses FAD function from FeynCalc, so it is more suitable for loop calculations.";


(* Cheung-Remmen variables *)


GravitonPropagatorCR::usage = "GravitonPropagatorCR[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Propagator for \[GothicH] perturbations in Cheung-Remmen variables. The gauge fixing parameter is already fixed and enters the expression. Here \[Mu],\[Nu],\[Alpha], and \[Beta] are Lorentz indices; p is the perturbation's momentum. The expression uses FAD function from FeynCalc, so it is more suitable for loop calculations.";
GravitonPropagatorAuxiliaryCR::usage = "GravitonPropagatorAuxiliaryCR[\[Lambda]1,\[Mu]1,\[Nu]1,\[Lambda]2,\[Mu]2,\[Nu]2]. Propagator for the auxliary field B in Cheung-Temmen variables. The field is not a gauge invariant and does not contain a guage fixing parameters. The field is auxiliary, so the propagator has no momenta.";

GravitonVertexCRhhh::usage = "GravitonVertexCRhhh[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)].";
GravitonVertexCRBhh::usage = "GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)].";
GravitonVertexCRBBh::usage = "GravitonVertexCRBBh[\!\(\*SubscriptBox[\(\[Alpha]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\[Mu],\[Nu]].";


(* Polarisation tensors. *)


PolarizationTensor::usage = "PolarizationTensor[\[Mu],\[Nu],p]. Polarization tensor for gravity in D dimensions. The tensor is constructed from the standard polarization vectors. This definition is neither traceless nor transverse.";
SetPolarizationTensor::usage = "The command ensures that the graviton polarization tensor is both traceless and transverse.";


(* Axion-like interaction with a single scalar field. *)


GravitonAxionVectorVertex::usage =
"GravitonAxionVectorVertex[{\[Rho],\[Sigma]},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. Vertex for the axion-like coupling of a scalar field to a single vector field, coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of the graviton; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(q\), \(i\)]\)} are momenta of vectors; \[Theta] is the coupling.\n
GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. Vertex for the axion-like coupling of a scalar field to a single vector field, coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(q\), \(i\)]\)} are momenta of vectors; \[Theta] is the coupling.\n
GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. Vertex for the axion-like coupling of a scalar field to a single vector field, coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)} are Lorentz indices of vectors; {\!\(\*SubscriptBox[\(q\), \(i\)]\)} are momenta of vectors; \[Theta] is the coupling.";


(* Horndeski gravity. *)


HorndeskiG2::usage = 
"HorndeskiG2[{\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(2\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to one graviton. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of a graviton; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(2\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(2\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\)  coupled to n graviton. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are momenta of scalars; b is the number of kinetic terms; \[Lambda] is the coupling.";

HorndeskiG3::usage = 
"HorndeskiG3[{\[Rho],\[Sigma],k},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(3\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; k is the graviton momentum; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(3\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(3\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.";

HorndeskiG4::usage = 
"HorndeskiG4[{\[Rho],\[Sigma],k},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(4\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; k is the graviton momentum; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(4\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(4\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.";

HorndeskiG5::usage = 
"HorndeskiG5[{\[Rho],\[Sigma],k},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(5\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to one graviton. Here {\[Rho],\[Sigma]} are Lorentz indices of a graviton; k is the graviton momentum; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG5[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(5\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to two gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.\n 
HorndeskiG5[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2 b + 1\)]\)},b,\[Lambda]]. Vertex for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) interaction with \!\(\*SubscriptBox[\(G\), \(5\)]\) = \[Lambda] \!\(\*SuperscriptBox[\(\[Phi]\), \(a\)]\) \!\(\*SuperscriptBox[\(X\), \(b\)]\) coupled to n gravitons. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton momenta; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are scalar momenta; b is the number of kinetic terms; \[Lambda] is the coupling.";


(* Scalar-Gauss-Bonnet interaction. *)


ScalarGaussBonnet::usage = 
"ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\)},g]. Vertex for scalar-Gauss-Bonnet interaction coupled to two gravitons (and any number of scalars). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are momenta of gravitons; g is the coupling.\n 
ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(k\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(3\)]\),\!\(\*SubscriptBox[\(k\), \(3\)]\)},g]. Vertex for scalar-Gauss-Bonnet interaction coupled to three gravitons (and any number of scalars). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are momenta of gravitons; g is the coupling.\n 
ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},g]. Vertex for scalar-Gauss-Bonnet interaction coupled to n gravitons (n \!\(\*FormBox[\"\\[GreaterEqual]\",TraditionalForm]\) 2) and any number of scalars. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are momenta of gravitons; g is the coupling.";


(* Quadratic gravity. *)


QuadraticGravityPropagator::usage = "QuadraticGravityPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p,\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)]. Graviton propagator within quadratic gravity. The gauge fixing parameter is already fixed and enters the expression. Here \[Mu], \[Nu], \[Alpha], and \[Beta] are Lorentz indices; p is the momentuml; \!\(\*SubscriptBox[\(m\), \(0\)]\) is the mass of the scalar mode; \!\(\*SubscriptBox[\(m\), \(2\)]\) is the mass of spin-2 ghost mode. The expression uses FAD function from FeynCalc, so it is more suitable for loop calculations.";


QuadraticGravityVertex::usage = 
"QuadraticGravityVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)]. Vertex for the 3-graviton interaction within quadratic gravity. The gauge fixing parameter is already fixed and enters the expression. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta; \!\(\*SubscriptBox[\(m\), \(0\)]\) is the mass of the scalar mode; \!\(\*SubscriptBox[\(m\), \(2\)]\) is the mass of the spin-2 ghost mode.\n 
QuadraticGravityVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)]. Vertex for the 4-graviton interaction within quadratic gravity. The gauge fixing parameter is already fixed and enters the expression. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta; \!\(\*SubscriptBox[\(m\), \(0\)]\) is the mass of the scalar mode; \!\(\*SubscriptBox[\(m\), \(2\)]\) is the mass of the spin-2 ghost mode.\n 
QuadraticGravityVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\),\!\(\*SubscriptBox[\(m\), \(0\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\)]. Vertex for the n-graviton (n \!\(\*FormBox[\"\\[GreaterEqual]\",TraditionalForm]\) 3) interaction within quadratic gravity. The gauge fixing parameter is already fixed and enters the expression. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are Lorentz indices of gravitons; {\!\(\*SubscriptBox[\(p\), \(i\)]\)} are graviton momenta; \!\(\*SubscriptBox[\(m\), \(0\)]\) is the mass of the scalar mode; \!\(\*SubscriptBox[\(m\), \(2\)]\) is the mass of the spin-2 ghost mode.";


(* Procedures that import libraries. *)


importGravitons::usage = "importGravitons[n]. The command imports libraries for graviton vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importScalars::usage = "importScalars[n]. The command imports libraries for the scalar field kinetic and potential term vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importFermions::usage = "importFermions[n]. The command imports libraries for the Dirac fermion vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importVectors::usage = "importVectors[n]. The command imports libraries for vector vertices up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importSUNYM::usage = "importSUNYM[n]. The command imports libraries for SU(N) Yang-Mills theory up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importAxionVectorVertex::usage = "importAxionVectorVertex[n]. The command imports libraries axion-like coupling to a single vector field up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importQuadraticGravity::usage = "importQuadraticGravity[n]. The command imports libraries for quadratic gravity up to order n. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG2::usage = "importHorndeskiG2. The command imports all exisiting libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(2\)]\) vertices. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG3::usage = "importHorndeskiG3. The command imports all exisiting libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(3\)]\) vertices. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG4::usage = "importHorndeskiG4. The command imports all exisiting libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(4\)]\) vertices. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importHorndeskiG5::usage = "importHorndeskiG5. The command imports all exisiting libraries for Horndeski \!\(\*SubscriptBox[\(G\), \(5\)]\) vertices. The command has a single boolean option \"printOutput\", which allows the printing of the output.";
importScalarGaussBonnet::usage "importScalarGaussBonnet[n]. The command imports libraries for Scalar-Gauss-Bonnet vertices up to order n\[GreaterEqual]2. If a library does not exist up to order n, the command imports the maximal existing order. The command has a single boolean option \"printOutput\", which allows the printing of the output.";


(* The list of commands. *)


FeynGravCommands := Print[
  Style[
    StringRiffle[
      {
        "GravitonPropagator", "GravitonPropagatorMassive", "GravitonVertex", "GravitonGhostVertex", "PolarizationTensor", "SetPolarizationTensor",
        "ScalarPropagator", "GravitonScalarVertex", "GravitonScalarPotentialVertex", "GravitonFermionVertex",
        "ProcaPropagator", "GravitonMassiveVectorVertex", "GravitonVectorVertex", "GravitonVectorGhostVertex",
        "GravitonGluonVertex", "GravitonGluonGhostVertex", "GravitonYMGhostVertex", "GravitonQuarkGluonVertex",
        "GravitonAxionVectorVertex",
        "HorndeskiG2", "HorndeskiG3", "HorndeskiG4", "HorndeskiG5", "importScalarGaussBonnet",
        "QuadraticGravityPropagator", "QuadraticGravityVertex",
        "importGravitons", "importScalars", "importFermions", "importVectors", "importSUNYM",
        "importHorndeskiG2", "importHorndeskiG3", "importHorndeskiG4", "importHorndeskiG5",
        "importAxionVectorVertex", "importQuadraticGravity"
      }, 
      ", "
    ],
    FontSize -> 16
  ]
];


(* Gauge fixing parameters. *)


FormatValues[FeynGrav`GaugeFixingEpsilon] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilon,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Gravity"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonCR] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonCR,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","CR"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonVector] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonVector,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","Vector"]} ;
FormatValues[FeynGrav`GaugeFixingEpsilonSUNYM] = {HoldPattern[MakeBoxes[FeynGrav`GaugeFixingEpsilonSUNYM,TraditionalForm]]:>SubscriptBox["\[CurlyEpsilon]","SU(N)YM"]} ;


FeynGrav`GaugeFixingEpsilon = 2;
FeynGrav`GaugeFixingEpsilonCR = -1/2;
FeynGrav`GaugeFixingEpsilonVector = -1;
FeynGrav`GaugeFixingEpsilonSUNYM = -1;


(* The gravitational coupling. The command makes sure that it has the correct context. *)


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


(* Cheung-Remmen variabless *)


GravitonPropagatorCR[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_] := I FAD[p] NieuwenhuizenOperatorInverse[-(1/2) 1/FeynGrav`GaugeFixingEpsilonCR,1,(D-5)/(D-2),-((D-1)/(D-2) +1/FeynGrav`GaugeFixingEpsilonCR),-(1/(D-2)),\[Mu],\[Nu],\[Alpha],\[Beta],p] //FeynAmpDenominatorCombine//Simplify ;


GravitonPropagatorAuxiliaryCR[\[Lambda]1_,\[Mu]1_,\[Nu]1_,\[Lambda]2_,\[Mu]2_,\[Nu]2_] := I \[Kappa]^(2-D) ( MTD[\[Lambda]1,\[Nu]2] MTD[\[Lambda]2,\[Nu]1] MTD[\[Mu]1,\[Mu]2]+MTD[\[Lambda]1,\[Mu]2] MTD[\[Lambda]2,\[Nu]1] MTD[\[Mu]1,\[Nu]2]+MTD[\[Lambda]1,\[Nu]2] MTD[\[Lambda]2,\[Mu]1] MTD[\[Mu]2,\[Nu]1]+MTD[\[Lambda]1,\[Mu]2] MTD[\[Lambda]2,\[Mu]1] MTD[\[Nu]1,\[Nu]2]+(MTD[\[Lambda]1,\[Mu]1] MTD[\[Lambda]2,\[Mu]2] MTD[\[Nu]1,\[Nu]2])/(1-D)+(MTD[\[Lambda]1,\[Mu]1] MTD[\[Lambda]2,\[Nu]2] MTD[\[Mu]2,\[Nu]1])/(1-D)+(MTD[\[Lambda]1,\[Nu]1] MTD[\[Lambda]2,\[Nu]2] MTD[\[Mu]1,\[Mu]2])/(1-D)+(MTD[\[Lambda]1,\[Nu]1] MTD[\[Lambda]2,\[Mu]2] MTD[\[Mu]1,\[Nu]2])/(1-D) )//Calc;


GravitonVertexCRhhh[\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_,\[Mu]3_,\[Nu]3_,p3_] =FVD[p1,\[Mu]2]FVD[p2,\[Mu]1]MTD[\[Mu]3,\[Nu]1]MTD[\[Nu]3,\[Nu]2]+1/2 FVD[p1,\[Mu]2]FVD[p3,\[Nu]2](MTD[\[Mu]1,\[Mu]3]MTD[\[Nu]1,\[Nu]3]-1/(D-2) MTD[\[Mu]1,\[Nu]1]MTD[\[Mu]3,\[Nu]3])-SPD[p1,p2](MTD[\[Nu]1,\[Mu]2]MTD[\[Nu]2,\[Mu]3]MTD[\[Nu]3,\[Mu]1]- 1/(D-2) MTD[\[Mu]1,\[Nu]1]MTD[\[Mu]2,\[Nu]3]MTD[\[Mu]3,\[Nu]2])//Calc;
GravitonVertexCRhhh[\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_,\[Mu]3_,\[Nu]3_,p3_]=1/2 (GravitonVertexCRhhh[\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,p3]+GravitonVertexCRhhh[\[Nu]1,\[Mu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,p3]);
GravitonVertexCRhhh[\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_,\[Mu]3_,\[Nu]3_,p3_]=1/2 (GravitonVertexCRhhh[\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,p3]+GravitonVertexCRhhh[\[Mu]1,\[Nu]1,p1,\[Nu]2,\[Mu]2,p2,\[Mu]3,\[Nu]3,p3]);
GravitonVertexCRhhh[\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_,\[Mu]3_,\[Nu]3_,p3_]=1/2 (GravitonVertexCRhhh[\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,p3]+GravitonVertexCRhhh[\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Nu]3,\[Mu]3,p3]);
GravitonVertexCRhhh[\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_,\[Mu]3_,\[Nu]3_,p3_]=\[Kappa]^(5-D) Total[GravitonVertexCRhhh@@@Flatten/@Permutations[Partition[{\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,p3},3]]]//Calc;


GravitonVertexCRBhh[\[Alpha]_,\[Rho]_,\[Sigma]_,\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_] = -1/4 (FVD[p1,\[Alpha]]MTD[\[Rho],\[Mu]1]MTD[\[Sigma],\[Mu]2]MTD[\[Nu]1,\[Nu]2]- FVD[p1,\[Rho]](MTD[\[Alpha],\[Mu]1]MTD[\[Sigma],\[Mu]2]MTD[\[Nu]1,\[Nu]2]-1/(D-2) MTD[\[Alpha],\[Mu]2]MTD[\[Sigma],\[Nu]2]MTD[\[Mu]1,\[Nu]1])+FVD[p1,\[Mu]2](MTD[\[Alpha],\[Mu]1]MTD[\[Sigma],\[Nu]1]MTD[\[Rho],\[Nu]2]-1/(D-2) MTD[\[Alpha],\[Sigma]]MTD[\[Mu]1,\[Nu]1]MTD[\[Nu]2,\[Rho]]));
GravitonVertexCRBhh[\[Alpha]_,\[Rho]_,\[Sigma]_,\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_]=GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2]+GravitonVertexCRBhh[\[Alpha],\[Sigma],\[Rho],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2];
GravitonVertexCRBhh[\[Alpha]_,\[Rho]_,\[Sigma]_,\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_]=GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2]+GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Nu]1,\[Mu]1,p1,\[Mu]2,\[Nu]2,p2];
GravitonVertexCRBhh[\[Alpha]_,\[Rho]_,\[Sigma]_,\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_]=GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2]+GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Nu]2,\[Mu]2,p2];
GravitonVertexCRBhh[\[Alpha]_,\[Rho]_,\[Sigma]_,\[Mu]1_,\[Nu]1_,p1_,\[Mu]2_,\[Nu]2_,p2_]=-I \[Kappa]^(4-D) (GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2]+GravitonVertexCRBhh[\[Alpha],\[Rho],\[Sigma],\[Mu]2,\[Nu]2,p2,\[Mu]1,\[Nu]1,p1])//Calc;


GravitonVertexCRBBh[\[Alpha]1_,\[Rho]1_,\[Sigma]1_,\[Alpha]2_,\[Rho]2_,\[Sigma]2_,\[Mu]_,\[Nu]_]=1/8 (- (MTD[\[Alpha]1,\[Rho]2]MTD[\[Alpha]2,\[Rho]1]-1/(D-1) MTD[\[Alpha]1,\[Rho]1]MTD[\[Alpha]2,\[Rho]2])MTD[\[Mu],\[Sigma]1]MTD[\[Nu],\[Sigma]2])//Calc;
GravitonVertexCRBBh[\[Alpha]1_,\[Rho]1_,\[Sigma]1_,\[Alpha]2_,\[Rho]2_,\[Sigma]2_,\[Mu]_,\[Nu]_]=GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Mu],\[Nu]]+GravitonVertexCRBBh[\[Alpha]1,\[Sigma]1,\[Rho]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Mu],\[Nu]];
GravitonVertexCRBBh[\[Alpha]1_,\[Rho]1_,\[Sigma]1_,\[Alpha]2_,\[Rho]2_,\[Sigma]2_,\[Mu]_,\[Nu]_]=GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Mu],\[Nu]]+GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Sigma]2,\[Rho]2,\[Mu],\[Nu]];
GravitonVertexCRBBh[\[Alpha]1_,\[Rho]1_,\[Sigma]1_,\[Alpha]2_,\[Rho]2_,\[Sigma]2_,\[Mu]_,\[Nu]_]=GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Mu],\[Nu]]+GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Nu],\[Mu]];
GravitonVertexCRBBh[\[Alpha]1_,\[Rho]1_,\[Sigma]1_,\[Alpha]2_,\[Rho]2_,\[Sigma]2_,\[Mu]_,\[Nu]_]=\[Kappa]^(3-D) (GravitonVertexCRBBh[\[Alpha]1,\[Rho]1,\[Sigma]1,\[Alpha]2,\[Rho]2,\[Sigma]2,\[Mu],\[Nu]]+GravitonVertexCRBBh[\[Alpha]2,\[Rho]2,\[Sigma]2,\[Alpha]1,\[Rho]1,\[Sigma]1,\[Mu],\[Nu]])//Calc;


(* Graviton sector *)


Options[importGravitons] = { printOutput -> False};

importGravitons[nExternal_ : 2, OptionsPattern[] ] := Module[{nImport},
	
	nImport = Min[ nExternal, Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVertex_*", packageDirectory]]] ];
	
	If[OptionValue[printOutput], 
		Print["Graviton vertices exist up to order ",Max[Map[ ToExpression[Last[Characters[#]]] &, FileNames["Libs/GravitonVertex_*", packageDirectory]]],"."];
		Print["Libraries will be imported up to the order ",nImport,"."];
	];

	Clear[GravitonVertex];
	
	Map[
		(Evaluate[GravitonVertex[Sequence@@DummyArrayMomentaVariables[#+2]]] = Get[packageDirectory<>"Libs/GravitonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	
	If[OptionValue[printOutput],
		Print["Graviton vertices imported up to order ",nImport,"."]
	];
];

Clear[GravitonGhostVertex];

GravitonGhostVertex = {\[Rho],\[Sigma],k,\[Mu],p1,\[Nu],p2} |->  I ( (FeynGrav`\[Kappa])/2 ) FVD[p2,\[Lambda]] ( MTD[\[Nu],\[Alpha]]MTD[\[Lambda],\[Beta]] + MTD[\[Nu],\[Beta]]MTD[\[Lambda],\[Alpha]] - MTD[\[Nu],\[Lambda]]MTD[\[Alpha],\[Beta]] ) ( FVD[p1,\[Alpha]](1/2)(MTD[\[Beta],\[Rho]]MTD[\[Mu],\[Sigma]]+MTD[\[Beta],\[Sigma]]MTD[\[Mu],\[Rho]]) + FVD[p1,\[Beta]](1/2)(MTD[\[Alpha],\[Rho]]MTD[\[Mu],\[Sigma]]+MTD[\[Alpha],\[Sigma]]MTD[\[Mu],\[Rho]]) + FVD[k,\[Mu]](1/2)(MTD[\[Alpha],\[Rho]]MTD[\[Beta],\[Sigma]] + MTD[\[Beta],\[Rho]]MTD[\[Alpha],\[Sigma]]) ) //Expand//Calc ;


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
		(Evaluate[ GravitonScalarVertex[DummyArrayVariables[#],ToExpression["p1_"],ToExpression["p2_"],ToExpression["m_"] ]] = Get[packageDirectory<>"Libs/GravitonScalarVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonScalarPotentialVertex[DummyArrayVariables[#],ToExpression["\[Lambda]_"]]] = Get[packageDirectory<>"Libs/GravitonScalarPotentialVertex_"<>ToString[#]])&,
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
		(Evaluate[GravitonFermionVertex[DummyArrayVariables[#],ToExpression["p1_"],ToExpression["p2_"],ToExpression["m_"]]] = Get[packageDirectory<>"Libs/GravitonFermionVertex_"<>ToString[#]])&,
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
		(Evaluate[GravitonMassiveVectorVertex[DummyArrayVariables[#],ToExpression["\[Lambda]1_"],ToExpression["p1_"],ToExpression["\[Lambda]2_"],ToExpression["p2_"],ToExpression["m_"]]] = Get[packageDirectory<>"Libs/GravitonMassiveVectorVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonVectorVertex[DummyArrayMomentaKVariables[#],ToExpression["\[Lambda]1_"],ToExpression["p1_"],ToExpression["\[Lambda]2_"],ToExpression["p2_"]]] = Get[packageDirectory<>"Libs/GravitonVectorVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonVectorGhostVertex[DummyArrayVariables[#],ToExpression["p1_"],ToExpression["p2_"]]] = Get[packageDirectory<>"Libs/GravitonVectorGhostVertex_"<>ToString[#]])&,
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
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],ToExpression["p1_"],ToExpression["\[Lambda]1_"],ToExpression["a1_"],ToExpression["p2_"],ToExpression["\[Lambda]2_"],ToExpression["a2_"]]] = Get[packageDirectory<>"Libs/GravitonGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],ToExpression["p1_"],ToExpression["\[Lambda]1_"],ToExpression["a1_"],ToExpression["p2_"],ToExpression["\[Lambda]2_"],ToExpression["a2_"],ToExpression["p3_"],ToExpression["\[Lambda]3_"],ToExpression["a3_"]]] = Get[packageDirectory<>"Libs/GravitonThreeGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonVertex[DummyArrayMomentaKVariables[#],ToExpression["p1_"],ToExpression["\[Lambda]1_"],ToExpression["a1_"],ToExpression["p2_"],ToExpression["\[Lambda]2_"],ToExpression["a2_"],ToExpression["p3_"],ToExpression["\[Lambda]3_"],ToExpression["a3_"],ToExpression["p4_"],ToExpression["\[Lambda]4_"],ToExpression["a4_"]]] = Get[packageDirectory<>"Libs/GravitonFourGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonQuarkGluonVertex[DummyArrayVariables[#],ToExpression["\[Lambda]_"],a_ ]] = Get[packageDirectory<>"Libs/GravitonQuarkGluonVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonYMGhostVertex[DummyArrayVariables[#],ToExpression["p1_"],ToExpression["a1_"],ToExpression["p2_"],ToExpression["a2_"] ]] = Get[packageDirectory<>"Libs/GravitonYMGhostVertex_"<>ToString[#]])&,
		Range[nImport] 
	];
	Map[
		(Evaluate[GravitonGluonGhostVertex[DummyArrayVariables[#],ToExpression["\[Lambda]1_"],ToExpression["a1_"],ToExpression["p1_"],ToExpression["\[Lambda]2_"],ToExpression["a2_"],ToExpression["p2_"],ToExpression["\[Lambda]3_"],ToExpression["a3_"],ToExpression["p3_"]  ]] = Get[packageDirectory<>"Libs/GravitonGluonGhostVertex_"<>ToString[#]])&,
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
		(Evaluate[GravitonAxionVectorVertex[DummyArrayVariables[#],ToExpression["\[Lambda]1_"],ToExpression["p1_"],ToExpression["\[Lambda]2_"],ToExpression["p2_"],ToExpression["\[CapitalTheta]_"]]  ] = Get[packageDirectory<>"Libs/GravitonAxionVectorVertex_"<>ToString[#]];)&,
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
		(HorndeskiG2[DummyArrayVariables[#[[3]]],DummyMomentaVariables[#[[1]] + 2 #[[2]]], #[[2]], ToExpression["Private`\[Lambda]_"]] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG2_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
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
		(HorndeskiG3[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2*#[[2]]+1],#[[2]],ToExpression["Private`\[Lambda]_"]] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG3_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
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
		(HorndeskiG4[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2*#[[2]]],#[[2]],ToExpression["Private`\[Lambda]_"]] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG4_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
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
		(HorndeskiG5[DummyArrayMomentaKVariables[#[[3]]],DummyMomentaVariables[#[[1]]+2 #[[2]]],#[[2]],ToExpression["Private`\[Lambda]_"]] = \[Lambda] Get[ packageDirectory<>"Libs/HorndeskiG5_"<>ToString[#[[1]]]<>"_"<>ToString[#[[2]]]<>"_"<>ToString[#[[3]]] ])&,
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
		(Evaluate[ScalarGaussBonnet[DummyArrayMomentaKVariables[#],ToExpression["Private`g_"]]] = g Get[packageDirectory<>"Libs/ScalarGaussBonnet_"<>ToString[#]])&,
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


GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_] := I (FeynGrav`GaugeFixingEpsilon/2 NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]+ NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]+(D-5)/(D-2) NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]+((D-1)(FeynGrav`GaugeFixingEpsilon-1)-FeynGrav`GaugeFixingEpsilon)/(D-2) NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]-1/(D-2) NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p])FAD[p] //FeynAmpDenominatorCombine//Simplify ;


GravitonPropagatorMassive[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m_]:=(-I)FAD[{p,m}] ( 1/2 ( (MTD[\[Mu],\[Alpha]]-FVD[p,\[Mu]]FVD[p,\[Alpha]]/m^2)(MTD[\[Nu],\[Beta]]-FVD[p,\[Nu]]FVD[p,\[Beta]]/m^2)+(MTD[\[Mu],\[Beta]]-FVD[p,\[Mu]]FVD[p,\[Beta]]/m^2)(MTD[\[Nu],\[Alpha]]-FVD[p,\[Nu]]FVD[p,\[Alpha]]/m^2) ) - 1/(D-1) (MTD[\[Mu],\[Nu]]-FVD[p,\[Mu]]FVD[p,\[Nu]]/m^2)(MTD[\[Alpha],\[Beta]]-FVD[p,\[Alpha]]FVD[p,\[Beta]]/m^2) ) //FeynAmpDenominatorCombine;


QuadraticGravityPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_,m0_,m2_]:= GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p] - I Collect[ ( NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]+(D-4)/(D-1) NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p])FAD[{p,m2}] ,FeynAmpDenominator[__],Simplify] + I Collect[ 1/(D-2) (3/(D-1) NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]+(D-1)  NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]+  NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p])FAD[{p,Sqrt[(3 (D-2) m0^2 m2^2)/((D-4) m0^2+2 (D-1) m2^2)]}] ,FeynAmpDenominator[__],Simplify] ;


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


(* This line ensures that one shall not specify the Nieuwenhuizen context every time they call for a command from the Nieuwenhuizen package. *)


AppendTo[$ContextPath, "Nieuwenhuizen`"];
