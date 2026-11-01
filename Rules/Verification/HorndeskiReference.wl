(* Test-only independent labelled action expansion; never loaded by FeynGrav. *)
(* Independent labelled metric expansion. No production tensor helpers.
   Each auxiliary e_i is unrestricted: h_i(mu,nu)=e_i(mu)e_i(nu).
   Equality as a polynomial in all e_i is a complete test of a separately
   symmetric, multilinear graviton tensor, not a physical polarisation test.
   The bitmask jets keep precisely the coefficient of each labelled field. *)
ClearAll[auditJetMultiply,auditInverse,auditVolume,auditBox,auditReference];
auditJetMultiply[x_List,y_List] := Table[
 Total[Table[If[BitAnd[i,j]===0 && BitOr[i,j]===mask,x[[i+1]] y[[j+1]],0],
   {i,0,Length[x]-1},{j,0,Length[y]-1}],2], {mask,0,Length[x]-1}];

auditInverse[p_,q_,es_List] := Module[{n=Length[es]},
 If[n===1,
  {SPD[p,q],-SPD[p,es[[1]]] SPD[q,es[[1]]]},
  {SPD[p,q],-SPD[p,es[[1]]] SPD[q,es[[1]]],
   -SPD[p,es[[2]]] SPD[q,es[[2]]],
   SPD[es[[1]],es[[2]]] (SPD[p,es[[1]]] SPD[q,es[[2]]] +
                                    SPD[p,es[[2]]] SPD[q,es[[1]]])}]];
auditVolume[es_List] := If[Length[es]===1,
 {1,SPD[es[[1]],es[[1]]]/2},
 {1,SPD[es[[1]],es[[1]]]/2,SPD[es[[2]],es[[2]]]/2,
  SPD[es[[1]],es[[1]]] SPD[es[[2]],es[[2]]]/4-SPD[es[[1]],es[[2]]]^2/2}];

(* From g^{-1}:dd phi - g^{-1}:Gamma.d phi with exp(+i p.x).
   Gamma is derived from the differentiated linear metric, not GammaTensor. *)
auditBox[p_,es_List,ks_List] := Module[{answer=-auditInverse[p,p,es],unit,piece},
 Do[
  unit=UnitVector[2^Length[es],2^(i-1)+1];
  piece=auditJetMultiply[auditInverse[ks[[i]],es[[i]],es],auditInverse[es[[i]],p,es]]-
        auditJetMultiply[auditInverse[es[[i]],es[[i]],es],auditInverse[ks[[i]],p,es]]/2;
  answer=answer+auditJetMultiply[unit,piece],{i,Length[es]}]; answer];

(* Independent functional differentiation in labelled scalar slots. Each
   unordered matching occurs 2^b b! times in X^b, and the remaining phi^a
   contributes a!. The Hessian slot in G3 is chosen separately. This avoids
   recomputing identical products without identifying equal-valued legs. *)
Clear[auditPairings,auditReference];
auditPairings[{}] := {{}};
auditPairings[labels_List] := Flatten[Table[
 Prepend[#, {First[labels],labels[[j]]}] & /@
  auditPairings[Delete[Rest[labels],j-1]],{j,2,Length[labels]}],1];

auditReference[family_,a_,b_,es_List,ks_List,ps_List] := Module[
 {answer=0,jet,remaining,hessianSlots,kineticSlots},
 hessianSlots=If[family===3,Range[Length[ps]],{0}];
 Do[
  remaining=If[hessian===0,Range[Length[ps]],DeleteCases[Range[Length[ps]],hessian]];
  Do[
   Do[
    jet=auditVolume[es];
    Do[jet=auditJetMultiply[jet,-auditInverse[ps[[pair[[1]]]],ps[[pair[[2]]]],es]],{pair,pairing}];
    If[family===3,jet=auditJetMultiply[jet,auditBox[ps[[hessian]],es,ks]]];
    answer+=Last[jet],{pairing,auditPairings[kineticSlots]}],
   {kineticSlots,Subsets[remaining,{2b}]}],
  {hessian,hessianSlots}];
 I Global`\[Kappa]^Length[es] Factorial[a] 2^b Factorial[b] Expand[answer]];


(* Independent labelled action reference. No production C/I/Gamma tensors.
   h_i = e_i e_i with unrestricted auxiliary vectors, not physical helicities.
   All products use nilpotent labels: each external graviton occurs once.
   Covariant rank-two terms are {mask,coefficient,left-vector,right-vector};
   a connection has one upper and two lower vector slots after its coefficient.
   There is no finite-dimensional component basis or Gram inversion. *)
ClearAll[hgSP,hgSetup,hgMul,hgShift,hgCombine,hgInv,hgH,hgTrace,hgTrace2,
 hgTrace3,hgReference,hgPairings,hgKinetic];
SetAttributes[hgSP,Orderless];
hgPairings[{}]={{}};
hgPairings[x_List]:=Flatten[Table[Prepend[#,{First[x],x[[j]]}]& /@
 hgPairings[Delete[Rest[x],j-1]],{j,2,Length[x]}],1];
hgMul[x_List,y_List]:=Table[Total[Table[x[[j+1]]y[[m-j+1]],
 {j,hgSubmasks[[m+1]]}]],{m,0,hgSize-1}];
hgShift[x_List,s_Integer]:=Table[If[BitAnd[m,s]===s,x[[m-s+1]],0],{m,0,hgSize-1}];
hgCombine[terms_List]:=DeleteCases[
 ({First[First[#]],Expand[Total[#[[All,2]]]],Sequence@@Drop[First[#],2]}& /@
 GatherBy[terms,Join[{First[#]},Drop[#,2]]&]),{_,0,___}];

hgSetup[es_List,ks_List]:=Module[{i,m,labels,orders,c,u,v,t,r,k,q,a,b},
 hgEs=es;hgKs=ks;hgN=Length[es];hgSize=2^hgN;
 hgSubmasks=Table[Select[Range[0,m],BitAnd[#,m]===#&],{m,0,hgSize-1}];
 hgUnit=UnitVector[hgSize,1];hgZero=ConstantArray[0,hgSize];
 Clear[hgInv,hgH];
 hgInverseTerms={};
 Do[labels=Pick[Range[hgN],Table[BitGet[m,i-1],{i,hgN}],1];
  Do[c=(-1)^Length[labels] Times@@Table[hgSP[es[[o[[j]]]],es[[o[[j+1]]]]],{j,Length[o]-1}];
   AppendTo[hgInverseTerms,{m,c,es[[First[o]]],es[[Last[o]]]}],
   {o,Permutations[labels]}],{m,1,hgSize-1}];
 hgInv[p_,q_]:=hgInv[p,q]=Module[{out=hgZero},out[[1]]=hgSP[p,q];
  Do[out[[t[[1]]+1]]+=t[[2]] hgSP[p,t[[3]]]hgSP[q,t[[4]]],{t,hgInverseTerms}];out];
 (* det(1 + sum_i t_i e_i e_i) has Gram principal minors as labelled
    coefficients. Solve V.V=det recursively, choosing V[0]=1. This works for
    arbitrary label count, without a fixed-dimensional component basis. *)
 hgVolume=hgUnit;
 Do[labels=Pick[Range[hgN],Table[BitGet[m,i-1],{i,hgN}],1];
  hgVolume[[m+1]]=Expand[(Det[Outer[hgSP,es[[labels]],es[[labels]]]]-
   Total[Table[hgVolume[[j+1]]hgVolume[[m-j+1]],
    {j,DeleteCases[hgSubmasks[[m+1]],0|m]}]])/2],{m,1,hgSize-1}];
 hgGamma={};
 Do[
  (* Identity part of g^{-1} and every nonconstant inverse-metric term. *)
  Do[If[BitAnd[t[[1]],2^(i-1)]===0,
    m=t[[1]]+2^(i-1);c=I t[[2]]/2;
    u=If[t[[1]]===0,es[[i]],t[[3]]];
    a=If[t[[1]]===0,1,hgSP[t[[4]],es[[i]]]];
    AppendTo[hgGamma,{m,c a,u,ks[[i]],es[[i]]}];
    AppendTo[hgGamma,{m,c a,u,es[[i]],ks[[i]]}];
    u=If[t[[1]]===0,ks[[i]],t[[3]]];
    a=If[t[[1]]===0,1,hgSP[t[[4]],ks[[i]]]];
    AppendTo[hgGamma,{m,-c a,u,es[[i]],es[[i]]}]],
   {t,Prepend[hgInverseTerms,{0,1,None,None}]}],{i,hgN}];
 hgGamma=hgCombine[hgGamma];
 hgRicci={};
 Do[Do[If[BitGet[t[[1]],i-1]===1,
   AppendTo[hgRicci,{t[[1]],I t[[2]]hgSP[ks[[i]],t[[3]]],t[[4]],t[[5]]}];
   AppendTo[hgRicci,{t[[1]],-I t[[2]]hgSP[t[[3]],t[[4]]],ks[[i]],t[[5]]}]],
  {i,hgN}],{t,hgGamma}];
 (* R_ab = d_c Gamma^c_ab - d_a Gamma^c_cb
             + Gamma^c_cd Gamma^d_ab - Gamma^c_ad Gamma^d_cb. *)
 Do[If[BitAnd[t[[1]],r[[1]]]===0,
  m=t[[1]]+r[[1]];c=t[[2]]r[[2]];
  AppendTo[hgRicci,{m,c hgSP[t[[3]],t[[4]]]hgSP[t[[5]],r[[3]]],r[[4]],r[[5]]}];
  AppendTo[hgRicci,{m,-c hgSP[t[[5]],r[[3]]]hgSP[t[[3]],r[[4]]],t[[4]],r[[5]]}]],
  {t,hgGamma},{r,hgGamma}];
 hgRicci=hgCombine[hgRicci];
 hgH[p_]:=hgH[p]=hgCombine[Join[{{0,-1,p,p}},
  {#[[1]],-I #[[2]]hgSP[#[[3]],p],#[[4]],#[[5]]}& /@hgGamma]];
 Null];

hgTrace[tensor_List]:=Total[(#[[2]]hgShift[hgInv[#[[3]],#[[4]]],#[[1]]]& /@tensor)];
hgTrace2[x_List,y_List]:=Module[{out=hgZero},
 Do[If[BitAnd[a[[1]],b[[1]]]===0,
  out+=a[[2]]b[[2]]hgShift[hgMul[hgInv[a[[4]],b[[3]]],hgInv[b[[4]],a[[3]]]],a[[1]]+b[[1]]]],
 {a,x},{b,y}];out];
hgTrace3[x_List,y_List,z_List]:=Module[{out=hgZero},
 Do[If[BitAnd[a[[1]],b[[1]]]===0&&BitAnd[a[[1]]+b[[1]],c[[1]]]===0,
  out+=a[[2]]b[[2]]c[[2]]hgShift[hgMul[hgMul[hgInv[a[[4]],b[[3]]],hgInv[b[[4]],c[[3]]]],
       hgInv[c[[4]],a[[3]]]],a[[1]]+b[[1]]+c[[1]]]],{a,x},{b,y},{c,z}];out];
hgKinetic[pairs_List,ps_List]:=Fold[hgMul,hgUnit,(-hgInv[ps[[#[[1]]]],ps[[#[[2]]]]]& /@pairs)];

(* Independent scalar functional differentiation: choose labelled Hessian
   slots, then unordered kinetic matchings; the combinatorial factors account
   for all identical factors, even when momenta have equal values. *)
hgReference[a_Integer,b_Integer,ps_List]:=Module[
 {einstein=hgZero,cubic=hgZero,rest,kinetic,h1,h2,h3,block,weight},
 Do[rest=DeleteCases[Range[Length[ps]],h];
  block=hgTrace2[hgRicci,hgH[ps[[h]]]]-hgMul[hgTrace[hgRicci],hgTrace[hgH[ps[[h]]]]]/2;
  Do[Do[einstein+=hgMul[hgKinetic[pairing,ps],block],{pairing,hgPairings[slots]}],
   {slots,Subsets[rest,{2b}]}],{h,Length[ps]}];
 einstein=Factorial[a]2^b Factorial[b]hgMul[hgVolume,einstein];
 If[b>0,
  Do[rest=Complement[Range[Length[ps]],hs];
   {h1,h2,h3}=hgH[ps[[#]]]& /@hs;
   block=hgMul[hgMul[hgTrace[h1],hgTrace[h2]],hgTrace[h3]]-
       3hgMul[hgTrace[h1],hgTrace2[h2,h3]]+2hgTrace3[h1,h2,h3];
   Do[Do[cubic+=hgMul[hgKinetic[pairing,ps],block],{pairing,hgPairings[slots]}],
    {slots,Subsets[rest,{2(b-1)}]}],{hs,Permutations[Range[Length[ps]],{3}]}];
  cubic=(b/3)Factorial[a]2^(b-1)Factorial[b-1]hgMul[hgVolume,cubic]];
 <|"Einstein"->I Global`\[Kappa]^hgN Expand[Last[einstein]],
   "CubicHessian"->I Global`\[Kappa]^hgN Expand[Last[cubic]]|>];
