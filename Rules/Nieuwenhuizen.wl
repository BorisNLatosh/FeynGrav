(* ::Package:: *)

BeginPackage["Nieuwenhuizen`",{"FeynCalc`"}];

SetDirectory[DirectoryName[$InputFileName]];

GaugeProjector::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
GaugeProjectorBar::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector \!\(\*SubscriptBox[OverscriptBox[\(\[Theta]\), \(_\)], \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."

GaugeProjectorFAD::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector realized with FAD function \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
GaugeProjectorBarFAD::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector realized with FAD functio \!\(\*SubscriptBox[OverscriptBox[\(\[Theta]\), \(_\)], \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1FAD::usage = "NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator realized with FAD function (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2FAD::usage = "NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator realized with FAD function (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0FAD::usage = "NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator realized with FAD function (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0BarFAD::usage = "NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator realized with FAD function (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBarFAD::usage = "NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator realized with FAD function (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."

Begin["Private`"];

GaugeProjector = {\[Mu],\[Nu],k} |-> MTD[\[Mu],\[Nu]] - FVD[k,\[Mu]] FVD[k,\[Nu]]/SPD[k,k] //Calc;
GaugeProjectorBar = {\[Mu],\[Nu],k} |-> FVD[k,\[Mu]] FVD[k,\[Nu]]/SPD[k,k] //Calc;

NieuwenhuizenOperator1= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjectorBar[\[Nu],\[Beta],k] + GaugeProjector[\[Mu],\[Beta],k]GaugeProjectorBar[\[Nu],\[Alpha],k] + GaugeProjector[\[Nu],\[Alpha],k]GaugeProjectorBar[\[Mu],\[Beta],k] + GaugeProjector[\[Nu],\[Beta],k]GaugeProjectorBar[\[Mu],\[Alpha],k]) //Calc;
NieuwenhuizenOperator2= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjector[\[Nu],\[Beta],k] + GaugeProjector[\[Mu],\[Beta],k]GaugeProjector[\[Nu],\[Alpha],k]) - 1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0Bar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|-> GaugeProjectorBar[\[Mu],\[Nu],k]GaugeProjectorBar[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0BarBar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->GaugeProjector[\[Mu],\[Nu],k]GaugeProjectorBar[\[Alpha],\[Beta],k] + GaugeProjectorBar[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //Calc;

GaugeProjectorFAD = {\[Mu],\[Nu],k} |-> MTD[\[Mu],\[Nu]] - FVD[k,\[Mu]]FVD[k,\[Nu]]FAD[k] // Calc;
GaugeProjectorBarFAD = {\[Mu],\[Nu],k} |-> FVD[k,\[Mu]] FVD[k,\[Nu]]FAD[k] //Calc;

NieuwenhuizenOperator1FAD = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjectorFAD[\[Mu],\[Alpha],k]GaugeProjectorBarFAD[\[Nu],\[Beta],k] + GaugeProjectorFAD[\[Mu],\[Beta],k]GaugeProjectorBarFAD[\[Nu],\[Alpha],k] + GaugeProjectorFAD[\[Nu],\[Alpha],k]GaugeProjectorBarFAD[\[Mu],\[Beta],k] + GaugeProjectorFAD[\[Nu],\[Beta],k]GaugeProjectorBarFAD[\[Mu],\[Alpha],k]) //Calc;
NieuwenhuizenOperator2FAD = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjectorFAD[\[Mu],\[Alpha],k]GaugeProjectorFAD[\[Nu],\[Beta],k] + GaugeProjectorFAD[\[Mu],\[Beta],k]GaugeProjectorFAD[\[Nu],\[Alpha],k]) - 1/3 GaugeProjectorFAD[\[Mu],\[Nu],k]GaugeProjectorFAD[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0FAD = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/3 GaugeProjectorFAD[\[Mu],\[Nu],k]GaugeProjectorFAD[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0BarFAD = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|-> GaugeProjectorBarFAD[\[Mu],\[Nu],k]GaugeProjectorBarFAD[\[Alpha],\[Beta],k] //Calc;
NieuwenhuizenOperator0BarBarFAD = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->GaugeProjectorFAD[\[Mu],\[Nu],k]GaugeProjectorBarFAD[\[Alpha],\[Beta],k] + GaugeProjectorBarFAD[\[Mu],\[Nu],k]GaugeProjectorFAD[\[Alpha],\[Beta],k] //Calc;

End[];

EndPackage[];
