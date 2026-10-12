(* Scalar master reduction is enabled by default. Start in a fresh kernel. *)
Needs["FeynGrav`"];

(* New integral: the answer includes the established I Pi^2 normalisation. *)
masters = CalcFormCalculate[
  FVD[q, mu] FVD[q, nu] FAD[{q, m0}, {q + p, m1}],
  LoopMomenta -> {q}];

(* Stop at tensor coefficients, then reduce that saved answer separately. *)
coefficients = CalcFormCalculate[
  FVD[q, mu] FVD[q, nu] FAD[{q, m0}, {q + p, m1}],
  LoopMomenta -> {q}, ReduceToScalarMasters -> False];
savedMasters = CalcFormCalculate[coefficients];

(* Preserve or remove demonstrably scaleless contributions explicitly. *)
formal = LoopTensorCoefficient[{0, 0}, {0, p}, {0, 0}, {1, 1}, D];
kept = CalcFormCalculate[formal, DropScaleless -> False];
dropped = CalcFormCalculate[formal, DropScaleless -> True];

(* Manual export; execute the returned InputFile with FORM separately. *)
(* job = CalcFormExport[coefficients, "/absolute/path/masters.frm"];
   RunProcess[{"form", job["InputFile"]}];
   answer = CalcFormImport[job["ResultFile"], job["MappingFile"]]; *)
