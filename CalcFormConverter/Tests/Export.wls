Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
add["exact", -7/13 + 2 I/3];
add["zero", 0];
add["symbols", GreekContext`\[Kappa] OtherContext`\[Kappa] + Global`\[GothicCapitalM]0^2 - GreekContext`\[Kappa]/3];
add["scalar_abbreviations", (D - 1)^(-1/2) (D - 1)^(-1/2) (m^2 + n^2)^(-2) + 1/SPD[p]];
add["trace_D", MTD[mu, nu] MTD[mu, nu]];
add["trace_4", MT[mu, nu] MT[mu, nu]];
add["component", MTD[mu, nu] FVD[p - l, mu] FVD[p, nu], LoopMomenta -> {l}];
add["free_indices", MTD[mu, nu] + FVD[p - 2 l, mu] FVD[p, nu]/s];
add["dot_products", SPD[p - l, p + 2 l] + SPD[-l]];
add["denominators", FAD[l, {l-p, m}, {l-p, m}] (SPD[l] + m^2)];
add["symbolic_mass", FAD[{l, Sqrt[3 (D - 2) m^2 n^2/((D - 4) m^2 + 2 (D - 1) n^2)]}]];
add["masters", A0[m^2] + I B0[s, m^2, n^2] + C0[a,b,c,m^2,n^2,z^2] + D0[a,b,c,d,s,t,m^2,n^2,z^2,y^2]];
add["polarization_norm", (MTD[mu,nu]+(D-2)FVD[p,mu]FVD[p,nu]/SPD[p]) (MTD[mu,nu]+(D-2)FVD[p,mu]FVD[p,nu]/SPD[p])];
add["denominator_product", FAD[{l,m}]^2 FAD[{p-l,n}] MTD[mu,nu]FVD[l,mu]FVD[p,nu]];

(* Polarization identities must remain distinct from physical momenta and from
   their complex conjugates, including explicit transversality options. *)
ep = Momentum[Polarization[p,I],D];
epc = Momentum[Polarization[p,-I],D];
eq = Momentum[Polarization[q,I],D];
add["polarization_components", Pair[LorentzIndex[mu,D],ep] + Pair[LorentzIndex[nu,D],epc]];
add["polarization_contraction", MTD[mu,nu] Pair[LorentzIndex[mu,D],ep] Pair[LorentzIndex[nu,D],epc]];
add["polarization_graviton", (MTD[mu,rho] MTD[nu,sigma]+MTD[mu,sigma] MTD[nu,rho]-MTD[mu,nu] MTD[rho,sigma])
 Pair[LorentzIndex[mu,D],ep] Pair[LorentzIndex[nu,D],ep]
 Pair[LorentzIndex[rho,D],epc] Pair[LorentzIndex[sigma,D],epc]];
add["polarization_linear", Pair[LorentzIndex[mu,D],Momentum[p+2 Polarization[q,I],D]] FVD[p,mu]];
add["polarization_scalar_power", 1/Pair[ep,eq]];
add["polarization_transverse", Pair[LorentzIndex[mu,D],Momentum[Polarization[p,I,Transversality->True],D]] FVD[p,mu]];
add["polarization_transverse_conjugate", Pair[LorentzIndex[mu,D],Momentum[Polarization[p,-I,Transversality->True],D]] FVD[p,mu]];
add["polarization_nontransverse", Pair[LorentzIndex[mu,D],Momentum[Polarization[p,I,Transversality->False],D]] FVD[p,mu]];
add["polarization_four_dimensional", Pair[LorentzIndex[mu],Momentum[Polarization[p,-I]]] FV[q,mu]];

add["polarization_routed_free", FVD[Polarization[-p-q-r,I],mu]];
add["polarization_routed_conjugate", MTD[mu,nu] FVD[Polarization[-p/2-q-r,I],mu] FVD[Polarization[-p/2-q-r,-I],nu]];
add["polarization_routed_transverse", FVD[Polarization[-p-q-r,I,Transversality->True],mu] FVD[-p-q-r,mu]];
add["polarization_routed_nontransverse", FVD[Polarization[-p-q-r,-I,Transversality->False],mu] FVD[p,mu]];
add["polarization_after_momentum_substitution", (MTD[mu,nu] FVD[Polarization[p1,I],mu] FVD[Polarization[p1,-I],nu])/.{p1->-p2-p3-p4,D->4}];

spaceDirectory = CreateDirectory[FileNameJoin[{testDirectory, "directory with spaces"}]];
spaceJob = CalcFormExport[FVD[p,mu]FVD[p,mu],FileNameJoin[{spaceDirectory,"space file.frm"}]];
assert["path containing spaces", AssociationQ[spaceJob]];
If[AssociationQ[spaceJob], AssociateTo[cases,"path_spaces" -> <|"Expected"->FCI[SPD[p]],"Job"->spaceJob|>]];

saveCases[];
finish["FORM export"];
