(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG4`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`","GammaTensor`","indexArraySymmetrization`"}];


T1::usage = "";
T2::usage = "";
T3::usage = "";
T4::usage = "";
T5::usage = "";
HorndeskiG40Core::usage = "";


Begin["Private`"];


takeIndices = indexArray |-> Flatten[ #[[;;2]]& /@ Partition[indexArray,3] ];
takeIndicesR1 = indexArray |-> Flatten[ #[[;;2]]& /@ Partition[ indexArray[[;;Length[indexArray]-3]], 3] ];
takeMomenta = indexArray |-> Flatten[ #[[3]]& /@ Partition[indexArray,3] ];


DummyMomenta2 = n |-> {ToExpression["\[ScriptA]"<>ToString[#]],ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n] //Flatten;
MomentaWrapper = MomentumArray |-> Times @@ MapThread[FVD,{MomentumArray, DummyMomenta2[Length[MomentumArray]/2] }];


T1 = 


End[];


EndPackage[];
