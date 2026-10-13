$HistoryLength=0;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
Get["/tmp/fg_sparse_basis_20261014.wl"];
Print["importResult=",FeynGrav`importGravitons[2]];
t=AbsoluteTiming[e=FeynGrav`GravitonVertex[a1,b1,q1,a2,b2,q2,a3,b3,q3,a4,b4,q4];][[1]];
Print["quartic head=",Head[e]," bytes=",ByteCount[e]," topTerms=",Length[If[Head[e]===Plus,List@@e,{e}]]," snapshotSeconds=",t];
t=AbsoluteTiming[x=TimeConstrained[FGBoundedBasis`ExtractVertex[e,12000],16,Failure["time cap"]];][[1]];
Print["quartic extractSeconds=",t," success=",AssociationQ[x]," result=",If[AssociationQ[x],<|"TermCount"->x["TermCount"],"Scalar"->And@@(FreeQ[#,FeynCalc`Pair[__]]&/@x["Coefficients"]),"VectorBytes"->ByteCount[KeyDrop[x,"Reconstruction"]]|>,x]];
(* Intentionally no Expand, no amplitude, no permutation closure. *)
