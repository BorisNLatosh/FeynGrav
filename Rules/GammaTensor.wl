(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GammaTensor`",{"FeynCalc`","ITensor`"}];

GammaTensor::usage = "GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda],\[Rho],\[Sigma]]. The function returns (\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\[Lambda]\[Rho]\[Sigma]\)]\). \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\) = \[Kappa] (-\[ImaginaryI])\!\(\*SubscriptBox[\(p\), \(\[Lambda]\)]\) (\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\[Lambda]\[Rho]\[Sigma]\)]\)\!\(\*SubscriptBox[\(h\), \(\[Rho]\[Sigma]\)]\)(k).";

Begin["Private`"];

GammaTensor = {m,a,b,l,r,s}|-> 1/2 ( MTD[l,a]ITensor[{b,m,r,s}] + MTD[l,b]ITensor[{a,m,r,s}] - MTD[l,m] ITensor[{a,b,r,s}] ) //Calc;

End[];

EndPackage[];

