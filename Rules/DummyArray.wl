(* ::Package:: *)

BeginPackage["DummyArray`"];

SetDirectory[DirectoryName[$InputFileName]];

DummyArray::usage = "DummyArray[k]. The function returns an array of indices {m1,n1,\[Ellipsis],mk,nk}.";

DummyArray = n |-> {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] //Flatten;

DummyArrayK = n |-> {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n] //Flatten;

EndPackage[];
