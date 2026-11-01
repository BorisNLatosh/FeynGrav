(* Independent test reference for the Einstein-Hilbert bulk action.
   No production C/I/Gamma tensors or stored vertex enter this construction.
   See the verification report for the integration by parts and conventions. *)
BeginPackage["GravitonQuinticReference`"];
EHScalarProduct::usage="EHScalarProduct[p,q] is a formal symmetric scalar product with no kinematic assignments.";
EHVertex::usage="EHVertex[auxiliaryVectors,incomingMomenta] gives the complete Einstein-Hilbert vertex contracted with unrestricted h_i=e_i e_i. It includes i kappa^(n-2).";
EHVolumeCoefficients::usage="EHVolumeCoefficients[es] gives coefficients of sqrt(det(1+sum t_i e_i e_i)) in the ring t_i^2=0, ordered by label bitmask.";
EHInverseCoefficients::usage="EHInverseCoefficients[u,v,es] gives the inverse-metric contraction u.g^-1.v in the same labelled ring.";
EHJetMultiply::usage="EHJetMultiply[x,y] multiplies two labelled coefficient lists, discarding repeated labels.";
SetAttributes[EHScalarProduct,Orderless];
Begin["`Private`"];
submasks[m_Integer]:=Select[Range[0,m],BitAnd[#,m]===#&];
labels[m_Integer,n_Integer]:=Pick[Range[n],Table[BitGet[m,j-1],{j,n}],1];
EHJetMultiply[x_List,y_List]/;Length[x]===Length[y]:=Table[
 Total[Table[x[[j+1]] y[[m-j+1]],{j,submasks[m]}]],{m,0,Length[x]-1}];

(* The determinant lemma gives the principal Gram minors. Solve V^2=det
   with V[0]=1; only already-computed proper subsets enter the recurrence. *)
EHVolumeCoefficients[es_List]:=Module[{v=UnitVector[2^Length[es],1],ls},
 Do[ls=labels[m,Length[es]];
  v[[m+1]]=Expand[(Det[Outer[EHScalarProduct,es[[ls]],es[[ls]]]]-
   Total[Table[v[[j+1]]v[[m-j+1]],{j,DeleteCases[submasks[m],0|m]}]])/2],
  {m,1,Length[v]-1}];v];

(* Neumann inverse: each ordered chain uses its selected metric labels once.
   Endpoints are arbitrary vectors; no finite-dimensional component basis. *)
EHInverseCoefficients[u_,v_,es_List]:=Prepend[Table[
 (-1)^Length[labels[m,Length[es]]] Total[Table[
  EHScalarProduct[u,es[[First[o]]]] EHScalarProduct[es[[Last[o]]],v]
   Times@@Table[EHScalarProduct[es[[o[[j]]]],es[[o[[j+1]]]]],{j,Length[o]-1}],
  {o,Permutations[labels[m,Length[es]]]}]],{m,1,2^Length[es]-1}],EHScalarProduct[u,v]];

(* First-kind connection C_abc=(d_b h_ac+d_c h_ab-d_a h_bc)/2.
   Rows contain {sign, vector_a, vector_b, vector_c}; i/2 is applied below. *)
connection[e_,p_]:={{1,e,p,e},{1,e,e,p},{-1,p,e,e}};
EHVertex[es_List,ps_List]/;Length[es]===Length[ps]&&Length[es]>=2:=Module[
 {n=Length[es],answer=0,rest,volume,inv,first,second,coefficient},
 Do[If[i=!=j,
  rest=es[[Complement[Range[n],{i,j}]]];volume=EHVolumeCoefficients[rest];
  Clear[inv];inv[u_,v_]:=inv[u,v]=EHInverseCoefficients[u,v,rest];
  coefficient=Total[Flatten[Table[
   (* g^mn Gamma^r_ms Gamma^s_nr minus g^mn Gamma^r_mn Gamma^s_rs. *)
   first=EHJetMultiply[EHJetMultiply[inv[a[[2]],b[[4]]],inv[a[[3]],b[[3]]]],inv[a[[4]],b[[2]]]];
   second=EHJetMultiply[EHJetMultiply[inv[a[[2]],b[[3]]],inv[a[[3]],a[[4]]]],inv[b[[2]],b[[4]]]];
   a[[1]]b[[1]](first-second),{a,connection[es[[i]],ps[[i]]]},{b,connection[es[[j]],ps[[j]]]}],1]];
  answer+=Expand[Last[EHJetMultiply[volume,coefficient]]]
 ],{i,n},{j,n}];
 (* i from the vertex, -2 from the action, and (i/2)^2 from connections.
    All n labels occur exactly once; no additional n! is appropriate. *)
 I Global`\[Kappa]^(n-2) Expand[answer]/2];
End[];
EndPackage[];
