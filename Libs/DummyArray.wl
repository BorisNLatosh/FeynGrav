(* ::Package:: *)

BeginPackage["DummyArray`"];

SetDirectory[DirectoryName[$InputFileName]];

DummyArray::usage = "DummyArray[k] takes an integer k and returns an array of variables {m1,n1,\[Ellipsis],mk,nk}.";

DummyArray = Flatten[ ( { ToExpression["m"<>ToString[#]], ToExpression["n"<>ToString[#]]} )& /@ Range[#] ]&;

EndPackage[];
