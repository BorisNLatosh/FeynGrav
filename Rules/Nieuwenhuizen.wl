(* ::Package:: *)

BeginPackage["Nieuwenhuizen`",{"FeynCalc`"}];


GaugeProjector::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
GaugeProjectorBar::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projector \!\(\*SubscriptBox[OverscriptBox[\(\[Theta]\), \(_\)], \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."


NieuwenhuizenOperator1::usage = "NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


Begin["Private`"];


GaugeProjector[m_,n_,p_] = Pair[LorentzIndex[m,D],LorentzIndex[n,D]]-FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],0]] Pair[LorentzIndex[m,D],Momentum[p,D]] Pair[LorentzIndex[n,D],Momentum[p,D]];
GaugeProjectorBar[m_,n_,p_] = FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],0]] Pair[LorentzIndex[m,D],Momentum[p,D]] Pair[LorentzIndex[n,D],Momentum[p,D]];


NieuwenhuizenOperator1 = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjectorBar[\[Nu],\[Beta],k] + GaugeProjector[\[Mu],\[Beta],k]GaugeProjectorBar[\[Nu],\[Alpha],k] + GaugeProjector[\[Nu],\[Alpha],k]GaugeProjectorBar[\[Mu],\[Beta],k] + GaugeProjector[\[Nu],\[Beta],k]GaugeProjectorBar[\[Mu],\[Alpha],k]) //FeynAmpDenominatorCombine;
NieuwenhuizenOperator2 = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjector[\[Nu],\[Beta],k] + GaugeProjector[\[Mu],\[Beta],k]GaugeProjector[\[Nu],\[Alpha],k]) - 1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //FeynAmpDenominatorCombine;
NieuwenhuizenOperator0 = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //FeynAmpDenominatorCombine;
NieuwenhuizenOperator0Bar = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|-> GaugeProjectorBar[\[Mu],\[Nu],k]GaugeProjectorBar[\[Alpha],\[Beta],k] //FeynAmpDenominatorCombine;
NieuwenhuizenOperator0BarBar = {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->GaugeProjector[\[Mu],\[Nu],k]GaugeProjectorBar[\[Alpha],\[Beta],k] + GaugeProjectorBar[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k] //FeynAmpDenominatorCombine;


End[];


EndPackage[];
