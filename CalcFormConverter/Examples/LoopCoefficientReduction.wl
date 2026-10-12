(* ::Package:: *)
(* ::Title:: *)
(* Reduce an existing one-loop coefficient without recalculating the diagram *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "CalcFormConverter.wl"}]];

coefficient = LoopTensorCoefficient[
    {0, 0}, {0, -p, -p}, {0, 0, M^2}, {1, 2, 1}, D];

(* No LoopMomenta option: this object already represents an integral
   coefficient. Retain the formal scaleless remainder by default. *)
reduced = CalcFormCalculate[coefficient, ReduceLoopCoefficients -> True, ReduceToScalarMasters -> False];

(* Independently choose the dimensional-regularisation convention which
   discards demonstrably scaleless integrals. This does not extract UV poles. *)
withoutScaleless = CalcFormCalculate[coefficient,
    ReduceLoopCoefficients -> True, ReduceToScalarMasters -> False, DropScaleless -> True];

(* In a new calculation the same option follows tensor decomposition. *)
newIntegral = CalcFormCalculate[
    FVD[q, mu] FVD[q, nu] FAD[{q, m, 2}, {q + p, M}],
    LoopMomenta -> {q}, ReduceLoopCoefficients -> True, ReduceToScalarMasters -> False];
