(* ::Package:: *)

BeginPackage["DummyArray`"];

SetDirectory[DirectoryName[$InputFileName]];

DummyArray::usage = "DummyArray[k]. In returns the following array of variables {m1,n1,\[Ellipsis],mk,nk}.";

DummyArray = Flatten[ ( { ToExpression["m"<>ToString[#]], ToExpression["n"<>ToString[#]]} )& /@ Range[#] ]&;

DummyArrayK = Flatten[ ( { ToExpression["m"<>ToString[#]], ToExpression["n"<>ToString[#]], ToExpression["k"<>ToString[#]]} )& /@ Range[#] ]&;

EndPackage[];
