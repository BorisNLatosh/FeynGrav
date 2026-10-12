(* ::Package:: *)
(* ::Title:: *)
(* One-loop coefficient functions: manual and automated workflows *)

(* ::Text:: *)
(* Evaluate individual cells in a fresh kernel. These calculations produce
   coefficient functions, not their values or Laurent expansions. *)

(* ::Input:: *)
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "CalcFormConverter.wl"}]];

(* ::Section:: *)
(* Automated rank-two bubble *)
(* ::Input:: *)
input = FVD[q, mu] FVD[q, nu] FAD[{q, m0}, {q + p, m1}];
result = CalcFormCalculate[input, LoopMomenta -> {q}, ReduceToScalarMasters -> False];

(* ::Section:: *)
(* Manual workflow: run FORM separately, then import *)
(* ::Input:: *)
jobDirectory = CreateDirectory[];
job = CalcFormExport[input, FileNameJoin[{jobDirectory, "bubble.frm"}], LoopMomenta -> {q}, ReduceToScalarMasters -> False];
(* Run: form /absolute/path/to/bubble.frm
   Afterwards evaluate: CalcFormImport[job["ResultFile"], job["MappingFile"]] *)

(* ::Section:: *)
(* Repeated powers and scaleless terms *)
(* ::Input:: *)
repeated = CalcFormCalculate[FAD[{q, m}, {q, m}], LoopMomenta -> {q}, ReduceToScalarMasters -> False];
formalScaleless = CalcFormCalculate[FAD[q], LoopMomenta -> {q}, ReduceToScalarMasters -> False];
droppedScaleless = CalcFormCalculate[FAD[q], LoopMomenta -> {q}, ReduceToScalarMasters -> False, DropScaleless -> True];

(* ::Section:: *)
(* Retain an integrand *)
(* ::Input:: *)
integrand = CalcFormCalculate[input, LoopMomenta -> {q}, ReduceToScalarMasters -> False, TensorDecomposition -> False];
