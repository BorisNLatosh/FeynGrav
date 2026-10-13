(* Independent small ghost factor audit without FeynArts/model initialisation.
   Source snapshot is the 15-KiB converted diagnostic expression, not an integrated amplitude. *)
$HistoryLength=0;$LoadFeynArts=False;
Get["/home/latosh/.Wolfram/Applications/FeynGrav/FeynGrav.wl"];
path=FileNameJoin[{DirectoryName[$InputFileName],"ghost-bubble-fc-20261014.wl"}];
If[!FileExistsQ[path],Print["NO_STAGED_EXPRESSION"];Quit[2]];
Print["FILE_SHA256=",FileHash[path,"SHA256","HexString"],
 " NO_FA_LOADED=",!MemberQ[$Packages,"FeynArts`"]];
x=Get[path];f=List@@x;
If[Head[x]=!=Times||Length[f]=!=5,Print["INCOMPLETE_FACTORS=",Length[f]];Quit[3]];
same[a_,b_]:=TrueQ[TimeConstrained[Expand[FeynCalc`MomentumExpand[a]-FeynCalc`MomentumExpand[b]],4,$TimedOut]===0];
sourceP=FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor5,Lor6,ell]]*
 FeynCalc`FCI[FeynGrav`GhostVectorPropagator[Lor7,Lor8,ell-p]];
routeFA=Cases[f[[1]],_FeynCalc`PropagatorDenominator,Infinity];
routeSrc=Cases[sourceP,_FeynCalc`PropagatorDenominator,Infinity];
route=Sort[FeynCalc`MomentumExpand/@routeFA]===Sort[FeynCalc`MomentumExpand/@routeSrc];
metrics=same[f[[2]] f[[3]],(-sourceP)/._FeynCalc`FeynAmpDenominator->1];
ClearAll[sourceMatches];
sourceMatches[expr_,a_,b_,k_,pairs_,moms_] := Module[{out={},vv},
 Do[vv=FeynGrav`GravitonGhostVertex[a,b,k,inds[[1]],mom,inds[[2]],-k-mom];
   If[same[expr,vv],AppendTo[out,{inds,mom,1}]];
   If[same[expr,-vv],AppendTo[out,{inds,mom,-1}]],
 {inds,pairs},{mom,moms}];out];
v1=sourceMatches[f[[4]],Lor1,Lor2,p,
 {{Lor8,Lor5},{Lor5,Lor8}},{ell-p,-ell}];
v2=sourceMatches[f[[5]],Lor3,Lor4,-p,
 {{Lor7,Lor6},{Lor6,Lor7}},{ell,p-ell}];
Print["PROP_DENOMINATORS_EXACT=",route," CLOSED_MINUS_TIMES_I_SQUARED_EXACT=",metrics,
 " FIRST_SOURCE_MATCHES=",InputForm[v1]," SECOND_SOURCE_MATCHES=",InputForm[v2]];
Print["PRODUCT_SIGN_CANDIDATES=",InputForm[If[v1==={}||v2==={}, {}, Flatten[Outer[Times,v1[[All,3]],v2[[All,3]]]]]]];
Print["UNINTEGRATED=True NO_TADPOLE_EXCLUSION_IN_PRODUCTION=True"];
Quit[If[route&&metrics&&v1=!={}&&v2=!={},0,4]];
