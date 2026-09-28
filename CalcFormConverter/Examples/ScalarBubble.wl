(* ::Package:: *)

(* Load this example after FeynGrav. Only the cubic quadratic-gravity library
   is needed. Nothing is exported or executed merely by loading this file. *)

ScalarBubbleExample[p_, l_, m0_, m2_] := Module[
  {mu, nu, alpha, beta, a1, b1, a2, b2, i1, j1, i2, j2, external},
  external[a_, b_] := (GaugeProjector[a, b, p] +
    (D - 1) GaugeProjectorBar[a, b, p])/Sqrt[(D - 1) (D - 2)];
  external[mu, nu] *
  QuadraticGravityVertex[{mu, nu, p, a1, b1, -l, i1, j1, -(p - l)}, m0, m2] *
  QuadraticGravityPropagator[a1, b1, a2, b2, l, m0, m2] *
  QuadraticGravityPropagator[i1, j1, i2, j2, p - l, m0, m2] *
  QuadraticGravityVertex[{alpha, beta, -p, a2, b2, l, i2, j2, p - l}, m0, m2] *
  external[alpha, beta]
];

(* Example commands, to evaluate explicitly:

   importQuadraticGravity[1];
   expression = ScalarBubbleExample[p, l, m0, m2];
   job = CalcFormExport[expression, "/absolute/existing/directory/bubble.frm",
     LoopMomenta -> {l}];

   Run form on job["InputFile"] separately. Then:

   result = CalcFormImport[job["ResultFile"], job["MappingFile"]];

   This is the supplied bubble product before its 1/2 symmetry factor and
   loop measure. The converter neither inserts those factors nor integrates.
*)

(* Automated workflow, to evaluate explicitly after loading this example:

   status = CalcFormCheck[];
   (* If status["Status"] is "NotFound", explicitly request CalcFormInstall[]. *)
   importQuadraticGravity[1];
   expression = ScalarBubbleExample[p, l, m0, m2];
   result = CalcFormCalculate[expression, LoopMomenta -> {l},
     WorkingDirectory -> "/tmp", KeepFiles -> True,
     ShowTiming -> True, ShowProgress -> True];

   To request four TFORM workers, add FORMThreads -> 4.
   CalcFormCheck[FORMThreads -> 4] checks that configuration first.
   If TFORM is missing, explicitly call CalcFormInstall[FORMThreads -> 4].

   For an end-to-end elapsed-time measurement, use:
   AbsoluteTiming[
     result = CalcFormCalculate[expression, LoopMomenta -> {l},
       FORMThreads -> 4, ShowTiming -> True, ShowProgress -> True];
   ]
   Timing reports Mathematica kernel CPU time, not total elapsed time.

   This runs the complete FORM algebra and imports its result. It does not
   integrate the loop. The full bubble may require substantial time and disk;
   set TimeConstraint explicitly if a finite FORM runtime is desired.
*)
