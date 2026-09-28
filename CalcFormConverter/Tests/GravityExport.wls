Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* Real FeynGrav factors, contracted independently of the full bubble. *)
Get[FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],"FeynGrav.wl"}]];
importQuadraticGravity[1];
external[a_,b_] := (GaugeProjector[a,b,p]+(D-1)GaugeProjectorBar[a,b,p])/Sqrt[(D-1)(D-2)];
add["gravity_vertex_trace", external[mu,nu] QuadraticGravityVertex[{mu,nu,p,a,b,-l,c,d,l-p},m0,m2] MTD[a,b] MTD[c,d]];
add["gravity_propagator_trace", MTD[mu,nu] QuadraticGravityPropagator[mu,nu,a,b,l,m0,m2] MTD[a,b]];

saveCases[];
finish["Gravity export"];
