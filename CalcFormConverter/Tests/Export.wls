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

spaceDirectory = CreateDirectory[FileNameJoin[{testDirectory, "directory with spaces"}]];
spaceJob = CalcFormExport[FVD[p,mu]FVD[p,mu],FileNameJoin[{spaceDirectory,"space file.frm"}]];
assert["path containing spaces", AssociationQ[spaceJob]];
If[AssociationQ[spaceJob], AssociateTo[cases,"path_spaces" -> <|"Expected"->FCI[SPD[p]],"Job"->spaceJob|>]];

saveCases[];
finish["FORM export"];
