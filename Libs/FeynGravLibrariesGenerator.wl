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


(* Procedures that check if libraries for simple models exist. *)


CheckGravitonScalars := (
	Scan[ Print["Libraries for the scalar field kinetic term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
	Scan[ Print["Libraries for the scalar field potential term vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonScalarVertex_*"] ];
);


CheckGravitonFermions := Scan[ Print["Libraries for Dirac fermion vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFermionVertex_*"] ];


CheckGravitonVectors := (
	Scan[ Print["Libraries for Proca field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonMassiveVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector field vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorVertex_*"] ];
	Scan[ Print["Libraries for a vector-ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVectorGhostVertex_*"] ];
);


CheckGravitonVertex := (
	Scan[ Print["Libraries for graviton vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonVertex_*"] ];
	Scan[ Print["Libraries for graviton-ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGhostVertex_*"] ];
);


(* Procedures that check if libraries for SU(N) Yang-Mills model exist. *)


CheckGravitonSUNYM := (
	Scan[ Print["Libraries for graviton-quark-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonQuarkGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonThreeGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-gluon-gluon-gluon vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonFourGluonVertex_*"] ];
	Scan[ Print["Libraries for graviton-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonYMGhostVertex_*"] ];
	Scan[ Print["Libraries for graviton-gluon-(Yang-Mills) ghost vertices exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonGluonGhostVertex_*"] ];
);


(* Procedures that check if libraries for simple Horndeski G2 interaction exist. *)


CheckHorndeskiG2 := Scan[ ( Print["Horndeski G2 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG2_*"] ];


CheckHorndeskiG3 := Scan[ ( Print["Horndeski G3 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG3_*"] ];


CheckHorndeskiG4 := Scan[ ( Print["Horndeski G4 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG4_*"] ];


CheckHorndeskiG5 := Scan[ ( Print["Horndeski G5 vertex exists for a=",#1,", b=",#2,", n=",#3,"."]&@@ToExpression[StringSplit[#,"_"][[2;;4]]] )&, FileNames["HorndeskiG5_*"] ];


(* Procedures that check if libraries for simple axion-like interaction exist. *)


CheckGravitonAxionVector := Scan[ Print["Libraries for gravitational interaction of a scalar axion coupled to a single vector field exist for n = ",#,"."]& ,StringSplit[#,"_"][[2]]&/@FileNames["GravitonAxionVectorVertex_*"] ];


(* Procedures that generates rules for simple models. *)


GenerateGravitonScalars[n_] := Module[
	{
		i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(ScriptN)"->"scn","(ScriptM)"->"scm","\\[Lambda]"->"lbd","Lambda"->"lbd"}
	},
	
	For[ i = 1, i <= n, i++,
		filePath = "GravitonScalarVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonScalarVertexUncontracted[DummyArray[i],Global`p1,Global`p2,Global`m]];
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors p1,p2;\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
		Print["Done for the kinetic term for order n="<>ToString[i]<>"."];
	];
	
	For[ i = 1, i <= n, i++,
		filePath = "GravitonScalarPotentialVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonScalarPotentialVertexUncontracted[DummyArray[i],Global`\[Lambda]]];
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors p1,p2;\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]", "lbd"->"\\[Lambda]"}], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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


GenerateGravitonVectors[n_] := Module[
	{
		i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(ScriptN)"->"scn","(ScriptM)"->"scm","\\[Lambda]"->"lbd","(Lambda)"->"lbd"}
	},
	
	For[ i = 1, i <= n, i++,
		filePath = "GravitonMassiveVectorVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonMassiveVectorVertexUncontracted[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]];
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors p1,p2,"<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[2+i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","lbd"->"\\[Lambda]"}], "Text"];
		
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
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors p1,p2,"<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","lbd"->"\\[Lambda]"}], "Text"];
		
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
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		theFileIndicesArray =Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]]//DeleteDuplicates;
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors p1,p2,"<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","lbd"->"\\[Lambda]"}], "Text"];
		
		Print["Done for the Maxwell-ghost for order n="<>ToString[i]<>"."];
	];
];


GenerateGravitonVertex[n_] := Module[
	{
		i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(Lambda)"->"lbd","(Tau)"->"tau"}
	},
	
	For[ i = 1, i <= n, i++,
		filePath = "GravitonVertex_"<>ToString[i]<>".frm";
		
		(*Check if the FROM code file is exists and empty.*)
		If[ FileExistsQ[filePath], Close[OpenWrite[filePath]], CreateFile[filePath] ];
		
		(*Check if the corresponding library exists and delete it if it does*)
		If[FileExistsQ[StringDrop[filePath, -4]], DeleteFile[StringDrop[filePath, -4]]];

		
		(*Writing the expression of the FORM file*)
		FeynCalc2FORM[filePath,GravitonVertexUncontracted[DummyArrayMomenta[2+i],Global`GaugeFixingEpsilon]];
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[2+i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
		(*FeynGrav uses many variables with the Private` context. I remove it.*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
		(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
		Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
		(*Apply replacements according to theDictionary*)
		Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
		(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
		Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[2+i],", "]<>",k1,k2;\n"<>Import[filePath,"Text"],"Text"];
	
		(*Place the expresison in a local variable L*)
		Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
		(* Finise the code*)
		Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
		(*Run the FORM*)
		Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
		DeleteFile[filePath];
		filePath = StringDrop[filePath, -4];
		
		(*Clean the output*)
		Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
		(* Bringing the output to the FeynCalc form*)
		Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]","lbd"->"\\[Lambda]"}], "Text"];
		
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


GenerateHorndeskiG2[n_] := Module[
	{
		a,i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(Lambda)"->"lbd","(Tau)"->"tau","(ScriptA)"->"sca","(ScriptB)"->"scb"}
	},
	
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[2+a],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[4+a],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
			Print["Done for the Horndeski G2 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


(* Procedures that generates rules for Horndeski G3 interaction. *)


GenerateHorndeskiG3[n_] := Module[
	{
		a,i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(Lambda)"->"lbd","(Tau)"->"tau","(ScriptN)"->"scn","(ScriptM)"->"scm","(ScriptR)"->"scr","(ScriptL)"->"scl","(ScriptS)"->"scs","(ScriptA)"->"sca","(ScriptB)"->"scb"}
	},
	
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[1+a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[n],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[1+2+a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[n],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
			
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[1+4+a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[n],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
			Print["Done for the Horndeski G3 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


GenerateHorndeskiG4[n_] := Module[
	{
		a,i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(Lambda)"->"lbd","(Tau)"->"tau","(ScriptA)"->"sca","(ScriptB)"->"scb","(ScriptM)"->"scm","(ScriptN)"->"scn","(ScriptR)"->"scr","(ScriptS)"->"scs","(ScriptL)"->"scl","(ScriptT)"->"sct"}
	},
	
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[a+2],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[a+4],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"]
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
			Print["Done for the Horndeski G4 vertex with a=",a,", b=2 for order n="<>ToString[i]<>"."];
		]
	];
];


GenerateHorndeskiG5[n_] := Module[
	{
		a,i,
		filePath,
		theFileIndicesArray,
		theDictionary = {"\\[Kappa]"->"Kappa","(Lambda)"->"lbd","(Tau)"->"tau","(ScriptA)"->"sca","(ScriptB)"->"scb","(ScriptM)"->"scm","(ScriptN)"->"scn","(ScriptR)"->"scr","(ScriptS)"->"scs","(ScriptL)"->"scl","(ScriptT)"->"sct","(Omega)"->"omg","(Epsilon)"->"eps"}
	},
	
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
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
		
			(*FeynGrav uses many variables with the Private` context. I remove it.*)
			Export[filePath, StringReplace[Import[filePath, "Text"], "Private`" -> ""], "Text"];
	
			(*FeynCalc splits the expression in multiple lines. Make sure that it is a single line.*)
			Export[filePath,StringRiffle[Join[{First[#]},{StringJoin[Rest[#]]}],"\n"]&@Import[filePath,"Lines"],"Text"];
		
			(*Apply replacements according to theDictionary*)
			Export[filePath,StringReplace[Import[filePath,"Text"],theDictionary],"Text"];
		
			(*Collect all the Lorentz indices and writhe them in the FORM code heading.*)
			Export[filePath,"Indices "<>StringRiffle[DeleteDuplicates@Flatten[StringCases[Import[filePath,"Text"],"d_("~~x1:(WordCharacter..)~~","~~x2:(WordCharacter..)~~")":>{x1,x2}]],", "]<>";\nVectors "<>StringRiffle[ToString["p"<>ToString[#]]&/@Range[2+a],", "]<>","<>StringRiffle[ToString["k"<>ToString[#]]&/@Range[i],", "]<>";\n"<>Import[filePath,"Text"],"Text"];
	
			(*Place the expresison in a local variable L*)
			Export[filePath, MapAt["Local L = " <> # <> ";" &, Import[filePath, {"Text", "Lines"}], 4], "Lines"];
	
			(* Finise the code*)
			Export[filePath, Join[Import[filePath, {"Text", "Lines"}], {"print L;", ".end"}], "Lines"];
		
			(*Run the FORM*)
			Run["form -q " <> filePath <> " >> "<>StringDrop[filePath, -4]];
			DeleteFile[filePath];
			filePath = StringDrop[filePath, -4];
		
			(*Clean the output*)
			Export[filePath, Import[filePath,"Lines"][[Last[Position[StringContainsQ["L =",#]&/@Import[filePath,"Lines"],True]][[1]]+2;;]],"Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], {" " -> "", "\n" -> "", "\r" -> "", ";" -> ""}], "Text"];
	
			(* Bringing the output to the FeynCalc form*)
			Export[filePath, StringReplace[Import[filePath, "Text"], {"i_" -> "I", "Kappa" -> "\\[Kappa]"}], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], "d_(" ~~ x : (WordCharacter ..) ~~ "," ~~ y : (WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> x <> ", D], LorentzIndex[" <> y <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "(" ~~ (y : WordCharacter ..) ~~ ")" :> "Pair[LorentzIndex[" <> y <> ", D], Momentum[" <> x <> ", D]]"], "Text"];
			Export[filePath, StringReplace[Import[filePath, "Text"], (x : WordCharacter ..) ~~ "." ~~ (y : WordCharacter ..) :>  "Pair[Momentum[" <> x <> ", D], Momentum[" <> y <> ", D]]"], "Text"];
		
			Print["Done for the Horndeski G5 vertex with a=",a,", b=1 for order n="<>ToString[i]<>"."];
		]
	];
];


(* Procedures that generates rules for the simplest axion-like interaction. *)


GenerateGravitonAxionVector[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonAxionVectorVertex_"<>ToString[i]], 
		DeleteFile["GravitonAxionVectorVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[ i = 1, i <= n, i++,
		timeTaken = First[Timing[ Put[ Evaluate[GravitonAxionVectorVertex[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`\[CapitalTheta]]] , "GravitonAxionVectorVertex_"<>ToString[i] ] ]];
		Print["Done for order "<>ToString[i]<>". Time taken: " <> ToString[timeTaken] <> " seconds." ];
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
