(* Test-only independent G4 action reference.
   Load the sibling HorndeskiReference.wl first, then call
   hgSetup[unrestrictedAuxiliaryVectors, incomingGravitonMomenta].
   This file calls no production rule, C tensor, I tensor or GammaTensor.
   The explicit geometric jets support one through three gravitons. *)

ClearAll[hg4ReferenceBlocks, hg4HessianBilinear];

(* Polarisation of H2 = [H]^2 - [H^2]. The two scalar slots are symmetric. *)
hg4HessianBilinear[left_List, right_List] :=
  hgMul[hgTrace[left], hgTrace[right]] - hgTrace2[left, right];

hg4ReferenceBlocks[a_Integer, b_Integer, scalarMomenta_List] := Module[
  {count = Length[scalarMomenta], curvature, hessian0, hessian1, hessian2,
   ricciScalar, slots, pairing, hs, remaining, kineticSlots, kinetic,
   left0, right0, leftConnection, rightConnection, block0, block1, block2,
   curvatureWeight, hessianWeight, phase},

  If[a < 0 || b < 0 || count =!= a + 2 b ||
      !ValueQ[hgN] || !MemberQ[{1, 2, 3}, hgN],
    Return[Failure["InvalidReferenceInput", <|
      "MessageTemplate" -> "Require a,b >= 0, a+2b labelled scalar momenta, and hgSetup with one to three gravitons."|>]]];

  curvature = hessian0 = hessian1 = hessian2 = hgZero;
  ricciScalar = hgTrace[hgRicci];

  (* Each unordered kinetic matching stands for 2^b b! ordered X slots;
     unused scalar labels fill the phi^a slots in a! different ways. *)
  Do[
    Do[
      curvature += hgMul[hgKinetic[pairing, scalarMomenta], ricciScalar],
      {pairing, hgPairings[slots]}],
    {slots, Subsets[Range[count], {2 b}]}];
  curvatureWeight = Factorial[a] 2^b Factorial[b];
  curvature = curvatureWeight hgMul[hgVolume, curvature];

  If[b > 0,
    (* Choosing an unordered Hessian pair requires its 2! slot weight below.
       Lists contain labelled positions, never distinct momentum values. *)
    Do[
      remaining = Complement[Range[count], hs];
      left0 = {{0, -1, scalarMomenta[[hs[[1]]]], scalarMomenta[[hs[[1]]]]}};
      right0 = {{0, -1, scalarMomenta[[hs[[2]]]], scalarMomenta[[hs[[2]]]]}};
      leftConnection = Select[hgH[scalarMomenta[[hs[[1]]]]], First[#] > 0 &];
      rightConnection = Select[hgH[scalarMomenta[[hs[[2]]]]], First[#] > 0 &];
      block0 = hg4HessianBilinear[left0, right0];
      block1 = hg4HessianBilinear[left0, rightConnection] +
        hg4HessianBilinear[leftConnection, right0];
      block2 = hg4HessianBilinear[leftConnection, rightConnection];
      Do[
        Do[
          kinetic = hgKinetic[pairing, scalarMomenta];
          hessian0 += hgMul[kinetic, block0];
          hessian1 += hgMul[kinetic, block1];
          hessian2 += hgMul[kinetic, block2],
          {pairing, hgPairings[kineticSlots]}],
        {kineticSlots, Subsets[remaining, {2 (b - 1)}]}],
      {hs, Subsets[Range[count], {2}]}];
    hessianWeight = (-2 b) 2 Factorial[a] 2^(b - 1) Factorial[b - 1];
    hessian0 = hessianWeight hgMul[hgVolume, hessian0];
    hessian1 = hessianWeight hgMul[hgVolume, hessian1];
    hessian2 = hessianWeight hgMul[hgVolume, hessian2]
  ];

  (* Derivatives in hgGamma/hgRicci/hgH/hgKinetic already contain I p.
     The sole remaining phase is the action-to-vertex I. *)
  phase = I Global`\[Kappa]^hgN;
  <|
    "Curvature" -> phase Expand[Last[curvature]],
    "Hessian0" -> phase Expand[Last[hessian0]],
    "Hessian1" -> phase Expand[Last[hessian1]],
    "Hessian2" -> phase Expand[Last[hessian2]]
  |>
];
