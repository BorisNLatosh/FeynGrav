(* Private, finite two-fermion basis. A Dirac chain is one ordered kinematic
   factor; only the commuting Pair factors are sorted. No gamma5/colour-chain
   algebra or general tensor framework is provided here. *)
Begin["FeynGrav`FeynArtsIntegration`Private`"];
qfGammaQ[e_]:=MatchQ[e,FeynCalc`DiracGamma[(FeynCalc`LorentzIndex|FeynCalc`Momentum)[_,D],D]];
qfChainQ[e_]:=qfGammaQ[e]||(Head[e]===Dot&&1<=Length[e]<=3&&And@@(qfGammaQ/@List@@e));
qfGamma[FeynCalc`DiracGamma[FeynCalc`LorentzIndex[a_,D],D]]:=Global`FADiracMatrix[a];
qfGamma[FeynCalc`DiracGamma[FeynCalc`Momentum[p_,D],D]]:=Global`FADiracSlash[p];
qfTranslate[e_]:=faTranslateMonomial[e/.{
 c_Dot/;qfChainQ[c]:>FeynArts`FANonCommutative@@(qfGamma/@List@@c),
 g_FeynCalc`DiracGamma/;qfGammaQ[g]:>FeynArts`FANonCommutative[qfGamma[g]]}];
qfBasisRecord[e_,family_String]:=Catch[Module[{terms,map=<||>,factors,kin,coefficient,key,monomials,coefficients,vector,reconstruction},
 terms=With[{x=Expand[e]},If[Head[x]===Plus,List@@x,{x}]];
 If[Length[terms]>10000,faBasisError["Fermion vertex exceeds the finite term limit."]];
 Do[
  factors=If[Head[term]===Times,List@@term,{term}];
  kin=Select[factors,faTypedPairQ[#]||qfChainQ[#]&];
  coefficient=Times@@Select[factors,!(faTypedPairQ[#]||qfChainQ[#])&];
  If[Count[kin,_?qfChainQ]>1||!FreeQ[coefficient,FeynCalc`Pair|FeynCalc`DiracGamma|Dot|FeynCalc`DiracTrace|FeynCalc`Eps],faBasisError["Unsupported ordered fermion kinematics."]];
  key=HoldComplete@@Sort[kin];map=Append[map,key->(Lookup[map,key,0]+coefficient)],{term,terms}];
 monomials=Times@@List@@#&/@Keys[map];coefficients=Values[map];
 reconstruction=Total[MapThread[Times,{monomials,coefficients}]];
 If[Expand[reconstruction-e]=!=0,faBasisError["Fermion basis reconstruction failed."]];
 vector=qfTranslate/@monomials;
 (* Identity is an empty spinor-chain factor; FeynArts removes it when forming a chain. *)
 vector=Map[If[FreeQ[#,FeynArts`FANonCommutative],# FeynArts`FANonCommutative[1],#]&,vector];
 If[!FreeQ[vector,FeynCalc`Pair|FeynCalc`DiracGamma|Dot],faBasisError["Untranslated fermion kinematic object."]];
 <|"Vector"->vector,"Coefficients"->coefficients,"TermCount"->Length[map],"RuleSHA256"->Hash[e,"SHA256","HexString"]|>
],"FGBasis"];

(* The first two slots are the ordered antifermion, fermion. Every remaining
   slot is a boson. The source uses exp(+ikx), whereas the standard FeynArts
   spinors use incoming exp(-ipx). Reverse ALL source momenta: the fermion
   (slot 2), antifermion (slot 1), and every graviton. Reversing only the
   fermion derivative would leave the two-graviton spin connection wrong. *)
qfRule[family_String]:=Module[{slots=ymSlots[Characters[family]],h,v,raw,colour},
 h=Select[slots,#["Type"]=="T"&];v=Select[slots,#["Type"]=="V"&];
 colour=FeynArts`IndexDelta[slots[[1]]["Colour"],slots[[2]]["Colour"]];
 raw=If[v==={},colour FeynGrav`GravitonFermionVertex[Flatten[(Append[#["Lorentz"],-#["Momentum"]]&/@h)],-slots[[2]]["Momentum"],-slots[[1]]["Momentum"],FeynGrav`QuarkMass],
   If[h==={},I FeynCalc`SMP["g_s"] FeynCalc`SUNT[FeynCalc`SUNIndex[v[[1]]["Colour"]]] FeynCalc`GAD[First[v[[1]]["Lorentz"]]],FeynGrav`GravitonQuarkGluonVertex[Flatten[Lookup[h,"Lorentz"]],First[v[[1]]["Lorentz"]],v[[1]]["Colour"]]]/.
    FeynCalc`SUNT[FeynCalc`SUNIndex[a_]]:>Global`FASUNT[a,slots[[1]]["Colour"],slots[[2]]["Colour"]]];
 ymRequire[FreeQ[raw,_Failure|FeynGrav`GravitonFermionVertex|FeynGrav`GravitonQuarkGluonVertex|FeynCalc`SUNT],"Missing quark source: "<>family];
 ymRequire[Exponent[raw,FeynGrav`\[Kappa]]===Length[h],"Wrong quark coupling order: "<>family];
 ymColourEncode[Expand[FeynCalc`DiracSimplify[FeynCalc`Contract[FeynCalc`FCI[raw]],FeynCalc`DiracOrder->True]]]
];
qfClassRow[family_,coefficients_]:=Module[{types=Characters[family],fields},
 fields=MapIndexed[Function[{t,pos},With[{j=First[pos]},If[j==1,"-",""]<>t<>"[1"<>If[MemberQ[{"F","V"},t],",{c"<>ToString[j]<>"}",""]<>"]"]],types];
 "C["<>StringRiffle[fields,","]<>"]=="<>faEncode[List/@coefficients]];
End[];
