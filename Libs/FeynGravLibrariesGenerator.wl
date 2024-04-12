(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["FeynGravLibrariesGenerator`",{"FeynCalc`"}];


Needs["GravitonScalarVertex`","./../Rules/GravitonScalarVertex.wl"];
Needs["GravitonFermionVertex`","./../Rules/GravitonFermionVertex.wl"];
Needs["GravitonVectorVertex`","./../Rules/GravitonVectorVertex.wl"];
Needs["GravitonSUNYM`","./../Rules/GravitonSUNYM.wl"];
Needs["GravitonVertex`","./../Rules/GravitonVertex.wl"];
Needs["HorndeskiG2`","./../Rules/HorndeskiG2.wl"];
Needs["HorndeskiG3`","./../Rules/HorndeskiG3.wl"];
Needs["HorndeskiG4`","./../Rules/HorndeskiG4.wl"];
Needs["HorndeskiG5`","./../Rules/HorndeskiG5.wl"];
Needs["GravitonAxionVectorVertex`","./../Rules/GravitonAxionVectorVertex.wl"];
SetDirectory[DirectoryName[$InputFileName]];


(* Procedures that verify whether libraries exist. *)


CheckGravitonScalars::usage = "CheckGravitonScalars. This procedure checks what libraries for graviton-scalar interaction are present.";
CheckGravitonFermions::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVectors::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVertex::usage = "CheckGravitonVertex. This procedure checks what libraries for graviton vertices are present.";


CheckGravitonSUNYM::usage = "CheckGravitonSUNYM. This procedure checks what libraries for gravitational interaction for SU(N)YM model are present.";


CheckGravitonAxionVector::usage = "CheckGravitonAxionVector. This procedure checks what libraries for graviton-scalar axion-single vector interaction are present.";


CheckHorndeskiG2::usage = "CheckHorndeskiG2. This procedure checks what libraries for Horndeski G2 interactions are present.";
CheckHorndeskiG3::usage = "CheckHorndeskiG3. This procedure checks what libraries for Horndeski G3 interactions are present.";
CheckHorndeskiG4::usage = "CheckHorndeskiG4. This procedure checks what libraries for Horndeski G4 interactions are present.";
CheckHorndeskiG5::usage = "CheckHorndeskiG5. This procedure checks what libraries for Horndeski G5 interactions are present.";


(* Procedures that generate libraries. *)


GenerateGravitonScalars::usage = "GenerateGravitonScalars[n]. This procedure generates libraries for graviton-scalar interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermions::usage = "GenerateGravitonFermions[n]. This procedure generates libraries for graviton-fermion interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectors::usage = "GenerateGravitonVectors[n]. This procedure generates libraries for graviton-vector interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVertex::usage = "GenerateGravitonVertex[n]. This procedure generates libraries for the gravity sector up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonSUNYM::usage = "GenerateGravitonSUNYM[n]. This procedure generates libraries for gravitational interaction for SU(N)YM model up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonAxionVector::usage = "GenerateGravitonAxionVector[n]. This procedure generates libraries for graviton-scalar axion-single vector interactions up to the order n. Pre-existing libraries will be removed!"


GenerateHorndeskiG2::usage = "GenerateHorndeskiG2[n]. This procedure generates libraries for Horndeski G2 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG3::usage = "GenerateHorndeskiG3[n]. This procedure generates libraries for Horndeski G3 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG4::usage = "GenerateHorndeskiG4[n]. This procedure generates libraries for Horndeski G4 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG5::usage = "GenerateHorndeskiG5[n]. This procedure generates libraries for Horndeski G5 interaction up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonScalarsSpecific::usage = "GenerateGravitonScalarsSpecific[n]. This procedure generates libraries for graviton-scalar interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermionsSpecific::usage = "GenerateGravitonFermionsSpecific[n]. This procedure generates libraries for graviton-fermion interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectorsSpecific::usage = "GenerateGravitonVectorsSpecific[n]. This procedure generates libraries for graviton-vector interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonVertexSpecific::usage = "GenerateGravitonVertexSpecific[n]. This procedure generates libraries for the gravity sector specifically for the order n. Pre-existing libraries will be removed!";


GenerateGravitonSUNYMSpecific::usage = "GenerateGravitonSUNYMSpecific[n]. This procedure generates libraries for SU(N)YM model interactions specifically for the order n. Pre-existing libraries will be removed!";


GenerateHorndeskiG2Specific::usage = "GenerateHorndeskiG2Specific[a,b,n]. This procedure generates libraries for Horndeski G2 for given number of scalar field a, number of scalar field kinetic terms b, and the preturbation order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG3Specific::usage = "GenerateHorndeskiG3Specific[a,b,n]. This procedure generates libraries for Horndeski G3 for given number of scalar field a, number of scalar field kinetic terms b, and the preturbation order n. Pre-existing libraries will be removed!";


Begin["Private`"];


DummyArray = n |->Flatten[ {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] ];
DummyMomenta = n |-> ToExpression["p"<>ToString[#]]&/@Range[n];
DummyArrayMomenta = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaK = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n]];


(* Procedures that verify whether libraries exist. *)


(* Scalars. *)


CheckGravitonScalars := (
	Scan[ Print["Libraries for the scalar field kinetic term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
	Scan[ Print["Libraries for the scalar field potential term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
);


(* Fermions. *)


CheckGravitonFermions := Scan[ Print["Libraries for Dirac fermion vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFermionVertex_*"] ];


(* Vectors. *)


CheckGravitonVectors := (
	Scan[ Print["Libraries for Proca field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonMassiveVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector-ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorGhostVertex_*"] ];
);


(* Gravitons. *)


CheckGravitonVertex := (
	Scan[ Print["Libraries for graviton vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVertex_*"] ];
	Scan[ Print["Libraries for graviton-ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGhostVertex_*"] ];
);


(* SU(N) Yang-Mills. *)


CheckGravitonSUNYM := (
	Scan[ Print["Libraries for graviton-quark-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonQuarkGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonThreeGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFourGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonYMGhostVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonGhostVertex_*"] ];
);


(* Horndeski. *)


CheckHorndeskiG2 := Scan[ ( Print["Horndeski G2 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG2_*"] ];


CheckHorndeskiG3 := Scan[ ( Print["Horndeski G3 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG3_*"] ];


CheckHorndeskiG4 := Scan[ ( Print["Horndeski G4 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG4_*"] ];


CheckHorndeskiG5 := Scan[ ( Print["Horndeski G5 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG5_*"] ];


(* Graviton-Axion. *)


CheckGravitonAxionVector := Scan[ Print["Libraries for gravitational interaction of a scalar axion coupled to a single vector field exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonAxionVectorVertex_*"] ];


(* Procedures that generate libraries. *)


(* Supplementary functions. *)


(* The function converts FeynCalc output to a FORM-executable file with FeynCalc2FORM and other tools. *)


FORMCodeCleanUp[filePath_,np_,nk_] := 
	Module[
		{
			theDictionary = {"\\[Kappa]"->"Kappa","\\[CapitalTheta]"->"cthet","(ScriptA)"->"sca","(ScriptB)"->"scb","(ScriptM)"->"scm","(ScriptN)"->"scn","(ScriptR)"->"scr","(ScriptS)"->"scs","(ScriptL)"->"scl","(ScriptT)"->"sct","\\[Lambda]"->"lbd","(Lambda)"->"lbd","(Tau)"->"tau","(Omega)"->"omg","(Epsilon)"->"eps","(CapitalTheta)"->"cthet"},
			theFileIndicesArray
		},
		(* Remove all Private` contexts. *)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
		(* Make the expression into a single line. *)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		(* FeynCalc2FORM does not convert some symbols correctly. I am fixing this manually. *)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		(* I collect all the Lorentz indices and write them in the head of the FORM file. *)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		theFileIndicesArray = Join[ theFileIndicesArray , Flatten[StringCases[Import[filePath, "Text"], "e_(" ~~ a:(WordCharacter..) ~~ "," ~~ b:(WordCharacter..) ~~ "," ~~ c:(WordCharacter..) ~~ "," ~~ d:(WordCharacter..) ~~ ")":>{a, b, c, d}]] ] // DeleteDuplicates;

		Export[filePath,"Indices "<>StringRiffle[theFileIndicesArray, ","]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[np],","]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[nk],","]<>";\n"<>Import[filePath,"Text"],"Text"];
		(* I put the expression in the local variable theResult. *)
		Export[filePath, MapAt["Local theResult = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
		(* I add the end to the FORM file. *)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print theResult;", ".end"}], "Lines"];
	];


(* The function that cleans the FORM output file. *)


FORMOutputCleanUp[filePath_] :=
	Module[{},
		(* Clean the output *)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["theResult =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], "e_(" ~~ a : (WordCharacter ..) ~~ "," ~~ b : (WordCharacter ..) ~~ "," ~~ c : (WordCharacter ..) ~~ "," ~~ d : (WordCharacter ..) ~~ ")" :>  "LeviCivita[" <> a <> "," <> b <> "," <> c <> "," <> d <> "]"], "Text"];
		(*Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];*)
		
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","lbd"->"\\[Lambda]","cthet"->"\\[CapitalTheta]"}], "Text"];
	];


(* Scalars. *)


GenerateGravitonScalars[n_] := Module[{i,filePath},
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonScalarVertex_"<>ToString[i]<>".frm";
		
		(* Check if the FROM code file exists and is empty. *)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		(* Check if the corresponding library exists and delete it if it does. *)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
		(* FeynCalc converts the expression to FORM and writes it to the file. *)
		FeynCalc2FORM[ filePath, GravitonScalarVertexUncontracted[DummyArray[i],p1,p2,m] ];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,0];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];

		Print["Scalar field kinetic term vertices is generated for n="<>ToString[i]<>"."];
	];
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonScalarPotentialVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonScalarPotentialVertexUncontracted[DummyArray[i],Global`\[Lambda]]];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,0];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
		
		Print["Done for the kinetic term for order n="<>ToString[i]<>"."];
	];

];


GenerateGravitonFermions[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonFermionVertex_"<>ToString[i]], 
		DeleteFile["GravitonFermionVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[ i = 1, i <= n, i++,
		Put[ Evaluate[GravitonFermionVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonFermionVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonVectors[n_] := Module[{i,filePath},
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonMassiveVectorVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonMassiveVectorVertexUncontracted[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,0];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
		
		Print["Done for the Proca field for order n="<>ToString[i]<>"."];
	];
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonVectorVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonVectorVertex[DummyArrayMomentaK[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`GaugeFixingEpsilonVector]];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,i];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
		
		Print["Done for the Maxwell field for order n="<>ToString[i]<>"."];
	];
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonVectorGhostVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonVectorGhostVertex[DummyArray[i],Global`p1,Global`p2]];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,i];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
		
		Print["Done for the Maxwell-ghost for order n="<>ToString[i]<>"."];
	];
];


GenerateGravitonVertex[n_] := Module[{i,filePath},
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonVertexUncontracted[DummyArrayMomenta[2+i],Global`GaugeFixingEpsilon]];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2+i,0];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
				
		Print["Done for the graviton vertex for order n="<>ToString[i]<>"."];
	];
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonGhostVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonGhostVertexUncontracted[DummyArrayMomenta[i],Global`\[Lambda]1,Global`k1,Global`\[Lambda]2,Global`k2]];

		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,i,2];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];
		
		Print["Done for the graviton-ghost vertex for order n="<>ToString[i]<>"."];
	];
];


(* Procedures that generates rules for SU(N) Yang-Mills model. *)


GenerateGravitonSUNYM[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonQuarkGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonQuarkGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonThreeGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonThreeGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonFourGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonFourGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonYMGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonYMGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonGluonGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonGluonVertex[DummyArrayMomentaK[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`GaugeFixingEpsilonSUNYM]] , "GravitonGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonThreeGluonVertex[DummyArray[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3]] , "GravitonThreeGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonFourGluonVertex[DummyArray[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3,Global`p4,Global`\[Lambda]4,Global`a4]] , "GravitonFourGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonQuarkGluonVertex[DummyArray[i],{Global`\[Lambda],Global`a}]] , "GravitonQuarkGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonYMGhostVertex[DummyArray[i],Global`p1,Global`a1,Global`p2,Global`a2]] , "GravitonYMGhostVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGluonGhostVertex[DummyArray[i],{Global`\[Lambda]1,Global`a1,Global`p1},{Global`\[Lambda]2,Global`a2,Global`p2},{Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonGluonGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


(* Procedures that generates rules for Horndeski G2 interaction. *)


GenerateHorndeskiG2[n_] := Module[{a,i,filePath},
	
	(* b = 1 *)
	
	For[ a = 1, a <= 4 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG2Uncontracted[DummyArray[i],DummyMomenta[a+2],1] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,2+a,0];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
					
			Print["Done for the Horndeski G2 vertex with a=",a,", b=1 for order n="<>ToString[i]<>"."];
		]
	];
	
	(* b = 2 *)
	
	For[ a = 0, a <= 2 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG2Uncontracted[DummyArray[i],DummyMomenta[a+4],2] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+4,0];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G2 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


(* Procedures that generates rules for Horndeski G3 interaction. *)


GenerateHorndeskiG3[n_] := Module[{a,i,filePath},
	
	(* b = 0 *)
	
	For[ a = 2, a <= 4 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG3Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+1],0] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+1,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G3 vertex with a=",a,", b=0 for order n="<>ToString[i]<>"."];
		]
	];
	
	(* b = 1 *)
	
	For[ a = 0, a <= 2 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG3Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+2+1],1] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+2+1,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
						
			Print["Done for the Horndeski G3 vertex with a=",a,", b=1 for order n="<>ToString[i]<>"."];
		]
	];
	
	(* b = 2 *)
	
	For[ a = 0, a <= 0 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG3Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+4+1],2] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+4+1,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
					
			Print["Done for the Horndeski G3 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


GenerateHorndeskiG4[n_] := Module[{a,i,filePath},
	
	(* b = 0 *)
	
	For[ a = 1, a <= 1 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG4_"<>ToString[a]<>"_0_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG4Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a],0] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G4 vertex with a=",a,", b=0 for order n="<>ToString[i]<>"."];
		]
	];
	
	(* b = 1 *)
	
	For[ a = 0, a <= 2 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG4Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+2],1] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+2,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G4 vertex with a=",a,", b=1 for order n="<>ToString[i]<>"."];
		]
	];
	
	
	(* b = 2 *)
	
	For[ a = 0, a <= 0 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG4_"<>ToString[a]<>"_2_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG4Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+4],2] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+4,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G4 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


GenerateHorndeskiG5[n_] := Module[{a,i,filePath},
	
	(* b = 0 *)
	
	For[ a = 1, a <= 4 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG5Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a],0] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G5 vertex with a=",a,", b=0 for order n="<>ToString[i]<>"."];
		]
	];
	
	(* b = 1 *)
	
	For[ a = 1, a <= 2 , a ++,
		For[ i = 1, i <= n, i++,
			filePath = "HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[i]<>".frm";
		
			(*Check if the FROM code file is exists and empty.*)
			If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
			(*Check if the corresponding library exists and delete it if it does*)
			If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

			(*Writing the expression of the FORM file*)
			FeynCalc2FORM[filePath, HorndeskiG5Uncontracted[DummyArrayMomentaK[i],DummyMomenta[a+2],1] ];
		
			(* I modify the FORM file so that it can be executed. *)
			FORMCodeCleanUp[filePath,a+2,i];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			FORMOutputCleanUp[filePath];
		
			Print["Done for the Horndeski G5 vertex with a=",a,", b=1 for order n="<>ToString[i]<>"."];
		]
	];
];


(* Procedures that generates rules for the simplest axion-like interaction. *)


GenerateGravitonAxionVector[n_] := Module[{i,filePath},
	
	For[ i = 1, i <= n, i++,
	
		filePath = "GravitonAxionVectorVertex_"<>ToString[i]<>".frm";
		
		(* Check if the FROM code file exists and is empty. *)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		(* Check if the corresponding library exists and delete it if it does. *)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];
		
		(* FeynCalc converts the expression to FORM and writes it to the file. *)
		FeynCalc2FORM[ filePath, (I) GravitonAxionVectorVertexUncontracted[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`\[CapitalTheta]] ];
		
		(* I modify the FORM file so that it can be executed. *)
		FORMCodeCleanUp[filePath,2,0];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		FORMOutputCleanUp[filePath];

		Print["Done for graviton-axion-like coupling of order n="<>ToString[i]<>"."];
	];
];


(* Procedures that generates specific rules for simple models. *)


GenerateGravitonScalarsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonScalarVertex_"<>ToString[n]], DeleteFile["GravitonScalarVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[n]], DeleteFile["GravitonScalarPotentialVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonScalarVertex[DummyArray[n],Global`p1,Global`p2,Global`m]] , "GravitonScalarVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonScalarPotentialVertex[DummyArray[n],Global`\[Lambda]]] , "GravitonScalarPotentialVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonFermionsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonFermionVertex_"<>ToString[n]], DeleteFile["GravitonFermionVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonFermionVertex[DummyArray[n],Global`p1,Global`p2,Global`m]] , "GravitonFermionVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonVectorsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonVectorVertex_"<>ToString[n]], DeleteFile["GravitonVectorVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonMassiveVectorVertex_"<>ToString[n]], DeleteFile["GravitonMassiveVectorVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonVectorGhostVertex_"<>ToString[n]], DeleteFile["GravitonVectorGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonVectorVertex[DummyArrayMomentaK[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`GaugeFixingEpsilonVector]] , "GravitonVectorVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonMassiveVectorVertex[DummyArray[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]] , "GravitonMassiveVectorVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonVectorGhostVertex[DummyArray[n],Global`p1,Global`p2]] , "GravitonVectorGhostVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonVertexSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonVertex_"<>ToString[n]],DeleteFile["GravitonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGhostVertex_"<>ToString[n]],DeleteFile["GravitonGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonVertex[DummyArrayMomenta[2+n],Global`GaugeFixingEpsilon]] , "GravitonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonGhostVertex[DummyArrayMomenta[n],Global`\[Lambda]1,Global`k1,Global`\[Lambda]2,Global`k2]] , "GravitonGhostVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


(* Procedures that generates specific rules for SU(N) Yang-Mills model. *)


GenerateGravitonSUNYMSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonQuarkGluonVertex_"<>ToString[n]], DeleteFile["GravitonQuarkGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonThreeGluonVertex_"<>ToString[n]], DeleteFile["GravitonThreeGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonFourGluonVertex_"<>ToString[n]], DeleteFile["GravitonFourGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGluonVertex_"<>ToString[n]], DeleteFile["GravitonGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonYMGhostVertex_"<>ToString[n]], DeleteFile["GravitonYMGhostVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGluonGhostVertex_"<>ToString[n]], DeleteFile["GravitonGluonGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonGluonVertex[DummyArrayMomentaK[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`GaugeFixingEpsilonSUNYM]] , "GravitonGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonThreeGluonVertex[DummyArray[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3]] , "GravitonThreeGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonFourGluonVertex[DummyArray[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3,Global`p4,Global`\[Lambda]4,Global`a4]] , "GravitonFourGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonQuarkGluonVertex[DummyArray[n],{Global`\[Lambda],Global`a}]] , "GravitonQuarkGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonYMGhostVertex[DummyArray[n],Global`p1,Global`a1,Global`p2,Global`a2]] , "GravitonYMGhostVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonGluonGhostVertex[DummyArray[n],{Global`\[Lambda]1,Global`a1,Global`p1},{Global`\[Lambda]2,Global`a2,Global`p2},{Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonGluonGhostVertex_"<>ToString[n] ];
	Print["Done" ];
];


(* Procedures that generates specific rules for Horndeski G2. *)


GenerateHorndeskiG2Specific[a_,b_,n_] := Module[{},
	If[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]],DeleteFile["HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]]];
	Put[ HorndeskiG2[DummyArray[n],DummyMomenta[a+2b],b] , "HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n] ];
	Print["Done."]
];


(* Procedures that generates specific rules for Horndeski G3. *)


GenerateHorndeskiG3Specific[a_,b_,n_] := Module[{},
	If[ FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]], DeleteFile["HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]] ];
	Put[ HorndeskiG3[DummyArrayMomentaK[n],DummyMomenta[a+2b+1],b] , "HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n] ];
	Print["Done"];
];


End[];


EndPackage[];
