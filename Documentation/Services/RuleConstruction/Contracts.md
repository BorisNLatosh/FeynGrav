# Interaction rules, validation and failures

[Documentation home](../../README.md)

## Which interface to use

Ordinary calculations use the library-backed commands in the [main-package reference](../../Reference.md). This directory implements rule construction for the [library generator](../LibraryGeneration/Guide.md), plus the Nieuwenhuizen helpers automatically loaded by FeynGrav. Use a separate fresh kernel for rule construction; identical short function names in different package contexts are not interchangeable.

| Rule families | Interaction constructed |
| --- | --- |
| Scalar, fermion and vector packages | Minimal matter couplings and the corresponding vector ghosts |
| GravitonVertex / QuadraticGravityVertex | Gravitational self-interactions in the selected theory |
| GravitonSUNYM | Quark–gluon, multi-gluon and Yang–Mills ghost interactions |
| HorndeskiG2–G5 | Scalar–tensor monomial interactions with explicit scalar momentum lists |
| ScalarGaussBonnet / GravitonAxionVectorVertex | Curvature-squared scalar coupling / axion–vector coupling |
| Nieuwenhuizen | Gauge tensors, operator combinations, inverse and verified decomposition |

An `Uncontracted` routine constructs an expression for subsequent contraction by the generator/converter. A contracted routine also performs the algebra implemented in that rule file. Neither name implies loop integration. Available pairs, signatures and algebra backends differ by family: follow the contracts below rather than mechanically appending `Uncontracted` to a command. Symbolic masses, couplings and composite momenta remain supported where their FeynCalc structures permit them.


The `Rules` packages generate tensors and vertices. Their mathematical definitions and successful return formats are unchanged by structural validation. Load a package by its file path; local dependencies load without changing `Directory[]` or `$Path`.

## Using results

Check `FailureQ[result]` before combining a returned expression with other calculations. A failure is a value, not a printed warning. For example:

```mathematica
result = indexArraySymmetrization[{mu, nu, extra}];
If[FailureQ[result], result, (* use the valid result *) result]
```

New diagnostics identify the fully qualified function, argument position or calculation stage, and relevant bounds. They avoid storing entire large inputs. Dependency failures, including FORM diagnostics and Nieuwenhuizen verification failures, are returned unchanged.

| Tag | Meaning |
|---|---|
| `InvalidArgumentCount` | No signature accepts this number of arguments. |
| `InvalidArgumentType` | A list or symbolic-expression slot has the wrong structural type. |
| `InvalidIndexArrayLength` | A list contains an incomplete block or violates a length bound. |
| `InvalidParameterValue` | An order, count or slice parameter is not an explicit integer in range. |
| `UnsupportedConfiguration` | The requested combination has no implemented branch or exceeds a supported range. |
| `RuleEvaluationFailed` | An evaluated dependency returned `$Failed`. |

Symbolic masses, couplings, indices and composite momenta remain accepted. Structural array slots require concrete lists; symbolic orders are not deferred calculations. Singularity detection, invertibility, physical kinematics and mathematical correctness are not established by these checks. Existing successful empty-list and zero-result cases are preserved. For example, symmetrising `{}` gives `{{}}`, and an empty product of metrics gives `1`.

## Evaluation and caching

Successful results may be memoised. Failed, invalid and aborted calls are not cached. User aborts propagate normally. When a parallel construction fails, the first failure in input order is returned before results are combined. Loading rule packages does not launch workers.

## Function contracts

Positions refer to each displayed signature. `Infinity` means no upper bound. Every listed signature rejects other argument counts; overloads are listed separately. Individual command pages describe the supported public construction interface.

### Public functions

| Function and signature | Structural requirements |
|---|---|
| ``GravitonVectorVertex`GravitonMassiveVectorVertexUncontracted`` — `GravitonMassiveVectorVertexUncontracted[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, m]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`GravitonMassiveVectorVertex`` — `GravitonMassiveVectorVertex[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, m]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`GravitonVectorVertex`` — `GravitonVectorVertex[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, \[CurlyEpsilon]]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`GravitonVectorVertexUncontracted`` — `GravitonVectorVertexUncontracted[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, \[CurlyEpsilon]]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`GravitonVectorGhostVertex`` — `GravitonVectorGhostVertex[indexArray, p1, p2]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`GravitonVectorGhostVertexUncontracted`` — `GravitonVectorGhostVertexUncontracted[indexArray, p1, p2]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``indexArraySymmetrization`indexArraySymmetrization`` — `indexArraySymmetrization[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``indexArraySymmetrization`indexArraySymmetrization3`` — `indexArraySymmetrization3[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonAxionVectorVertex`GravitonAxionVectorVertex`` — `GravitonAxionVectorVertex[indexArray, \[Lambda]1, q1, \[Lambda]2, q2, \[Theta]]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``GravitonAxionVectorVertex`GravitonAxionVectorVertexUncontracted`` — `GravitonAxionVectorVertexUncontracted[indexArray, \[Lambda]1, q1, \[Lambda]2, q2, \[Theta]]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``MTDWrapper`MTDWrapper`` — `MTDWrapper[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``GravitonScalarVertex`GravitonScalarVertexUncontracted`` — `GravitonScalarVertexUncontracted[indexArray, p1, p2, m]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonScalarVertex`GravitonScalarVertex`` — `GravitonScalarVertex[indexArray, p1, p2, m]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonScalarVertex`GravitonScalarPotentialVertexUncontracted`` — `GravitonScalarPotentialVertexUncontracted[indexArray, \[Lambda]]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association) |
| ``GravitonScalarVertex`GravitonScalarPotentialVertex`` — `GravitonScalarPotentialVertex[indexArray, \[Lambda]]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association) |
| ``ITensor`ITensorPlain`` — `ITensorPlain[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``ITensor`ITensor`` — `ITensor[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``DummyArray`DummyArray`` — `DummyArray[n]` | argument 1: explicit integer >= 0 |
| ``DummyArray`DummyArrayK`` — `DummyArrayK[n]` | argument 1: explicit integer >= 0 |
| ``HorndeskiG3`HorndeskiG3`` — `HorndeskiG3[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG3`HorndeskiG3Uncontracted`` — `HorndeskiG3Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``QuadraticGravityVertex`VertexRicciSquare`` — `VertexRicciSquare[gravitonParameters]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``QuadraticGravityVertex`QuadraticGravityVertex`` — `QuadraticGravityVertex[gravitonParameters, m0, m2]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonQuarkGluonVertex`` — `GravitonQuarkGluonVertex[indexArray1, indexArray2]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 2 to 2 |
| ``GravitonSUNYM`GravitonThreeGluonVertex`` — `GravitonThreeGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonFourGluonVertex`` — `GravitonFourGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3, p4, \[Lambda]4, a4]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association); argument 11: symbolic expression (not a list or association); argument 12: symbolic expression (not a list or association); argument 13: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonGluonVertex`` — `GravitonGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, \[CurlyEpsilon]]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonYMGhostVertex`` — `GravitonYMGhostVertex[indexArray, p1, a, p2, b]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonGluonGhostVertex`` — `GravitonGluonGhostVertex[indexArray, array1, array2, array3]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 3 to 3; argument 3: flat list, block size 1, length 3 to 3; argument 4: flat list, block size 1, length 3 to 3 |
| ``GravitonSUNYM`GravitonQuarkGluonVertexUncontracted`` — `GravitonQuarkGluonVertexUncontracted[indexArray1, indexArray2]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 2 to 2 |
| ``GravitonSUNYM`GravitonGluonVertexUncontracted`` — `GravitonGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, \[CurlyEpsilon]]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonThreeGluonVertexUncontracted`` — `GravitonThreeGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonFourGluonVertexUncontracted`` — `GravitonFourGluonVertexUncontracted[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3, p4, \[Lambda]4, a4]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association); argument 11: symbolic expression (not a list or association); argument 12: symbolic expression (not a list or association); argument 13: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonYMGhostVertexUncontracted`` — `GravitonYMGhostVertexUncontracted[indexArray, p1, a, p2, b]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``GravitonSUNYM`GravitonGluonGhostVertexUncontracted`` — `GravitonGluonGhostVertexUncontracted[indexArray, array1, array2, array3]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 1, length 3 to 3; argument 3: flat list, block size 1, length 3 to 3; argument 4: flat list, block size 1, length 3 to 3 |
| ``CTensorGeneral`CTensorPlainGeneral`` — `CTensorPlainGeneral[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity; At most seven external index pairs are implemented. Condition: Length[indexArrayExternal] <= 14. |
| ``CTensorGeneral`CTensorGeneral`` — `CTensorGeneral[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity; At most seven external index pairs are implemented. Condition: Length[indexArrayExternal] <= 14. |
| ``CETensor`CETensorPlain`` — `CETensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CETensor`CETensorPlain`` — `CETensorPlain[indexArrayExternal1, indexArrayExternal2, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 1, length 2 to 2; argument 3: flat list, block size 2, length 0 to Infinity |
| ``CETensor`CETensor`` — `CETensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CETensor`CETensor`` — `CETensor[indexArrayExternal1, indexArrayExternal2, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 1, length 2 to 2; argument 3: flat list, block size 2, length 0 to Infinity |
| ``Nieuwenhuizen`GaugeProjector`` — `GaugeProjector[m, n, p]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`GaugeProjectorBar`` — `GaugeProjectorBar[m, n, p]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator1`` — `NieuwenhuizenOperator1[\[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator2`` — `NieuwenhuizenOperator2[\[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator0`` — `NieuwenhuizenOperator0[\[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator0Bar`` — `NieuwenhuizenOperator0Bar[\[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator0BarBar`` — `NieuwenhuizenOperator0BarBar[\[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperator`` — `NieuwenhuizenOperator[z1, z2, z0, z0b, z0bb, \[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperatorInverse`` — `NieuwenhuizenOperatorInverse[z1, z2, z0, z0b, z0bb, \[Mu], \[Nu], \[Alpha], \[Beta], k]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association) |
| ``Nieuwenhuizen`NieuwenhuizenOperatorExpansion`` — `NieuwenhuizenOperatorExpansion[T, m, n, a, b, p]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``HorndeskiG2`HorndeskiG2`` — `HorndeskiG2[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG2`HorndeskiG2Uncontracted`` — `HorndeskiG2Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``ETensor`ETensorPlain`` — `ETensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 2, length 0 to Infinity |
| ``ETensor`ETensor`` — `ETensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 2, length 0 to Infinity |
| ``GammaTensor`GammaTensor`` — `GammaTensor[m, a, b, l, r, s]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``HorndeskiG5`HorndeskiG5`` — `HorndeskiG5[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; All graviton orders use the complete connection expansion; b <= 4 at n = 1, b <= 3 at n = 2, and b <= 2 at n >= 3 (seven inverse-metric pairs). |
| ``HorndeskiG5`HorndeskiG5Uncontracted`` — `HorndeskiG5Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; All graviton orders use the complete connection expansion; b <= 4 at n = 1, b <= 3 at n = 2, and b <= 2 at n >= 3 (seven inverse-metric pairs). |
| ``ScalarGaussBonnet`ScalarGaussBonnet`` — `ScalarGaussBonnet[gravitonParameters]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``GravitonFermionVertex`GravitonFermionVertexUncontracted`` — `GravitonFermionVertexUncontracted[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonFermionVertex`GravitonFermionVertex`` — `GravitonFermionVertex[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``HorndeskiG4`HorndeskiG4`` — `HorndeskiG4[gravitonParameters, scalarMomenta, b]` | argument 1: flat list of n triples, n >= 1; argument 3: explicit integer b >= 0; argument 2: flat scalar list of length a + 2 b, a >= 0; b <= 5 for n = 1, b <= 4 otherwise. Corrected action and bounded verification are documented below. |
| ``HorndeskiG4`HorndeskiG4Uncontracted`` — `HorndeskiG4Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list of n triples, n >= 1; argument 3: explicit integer b >= 0; argument 2: flat scalar list of length a + 2 b, a >= 0; b <= 5 for n = 1, b <= 4 otherwise. Corrected action and bounded verification are documented below. |
| ``GravitonVertex`GravitonVertex`` — `GravitonVertex[indexArray]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``GravitonVertex`GravitonVertexUncontracted`` — `GravitonVertexUncontracted[indexArray]` | argument 1: flat list, block size 3, length 6 to Infinity |

## Known limitations exposed by validation

- `CTensorGeneral` and `CTensorPlainGeneral` support zero through seven external index pairs.
- G4 uses the corrected $X^b R - 2 b X^{b-1} H_2$ action, labelled permutations and the $b=0$ scalar factorial. Its public domain is $n\geq1$, $a\geq0$, $0\leq b\leq5$ at $n=1$ and $0\leq b\leq4$ at $n\geq2$.
- G5 direct constructors require $n\geq1$, $a\geq0$ and $b\geq0$, with $b\leq4$ at $n=1$, $b\leq3$ at $n=2$ and $b\leq2$ at $n\geq3$. The $b=0$ route remains separate.
- These limits reflect the seven inverse-metric pairs available to the construction helpers. They do not guarantee affordable evaluation at arbitrary graviton order or independent verification of every generated vertex.
- Validation covers construction. The [generator](../LibraryGeneration/Guide.md#Failures%20and%20safe%20replacement) separately handles publication and batch failures.

See [scientific scope](../../ScientificScope.md#Horndeski) for the checked selections and stored-library corrections.

## Dimensional quark and axion conventions

`GravitonQuarkGluonVertexUncontracted` retains the D-dimensional gamma matrices
returned by `QuarkGluonVertex[..., Explicit -> True]` and the named coupling
`SMP["g_s"]`. There is no four-dimensional gamma substitution for FORM transport.

Both quark–gluon source variants include κⁿn! for n graviton pairs: the CE
tensor contains geometric Taylor coefficients only. The order-one/two stored
libraries were regenerated after correcting the missing κⁿ. See the
independent normalisation and public-import checks ([scientific scope](../../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions)).

Both axion–vector rules use the internal tensor

```mathematica
Eps[LorentzIndex[tau1, D], LorentzIndex[lambda1, D],
    LorentzIndex[tau2, D], LorentzIndex[lambda2, D]]
```

with D-dimensional momentum components. It has four slots, including for symbolic
D, and follows FeynCalc's `$LeviCivitaSign` convention. The interaction's physical
`-I` prefactor is retained. CalcFormConverter handles the translation convention;
the library generator adds no compensating factor of `I`.

`ScalarGaussBonnet` requires at least two graviton triples because the
Gauss–Bonnet combination is quadratic in curvature: around a flat background
each curvature starts at first order, so the one-graviton contribution vanishes.
This is not an unimplemented interaction. The current rule validates the
six-entry minimum rather than returning that zero for a one-graviton call.

`GenerateScalarGaussBonnet[n]` generates orders `2` through `n`. For `n = 1`,
the batch returns `Null` without constructing rules or producing files.
`GenerateScalarGaussBonnetSpecific[n]` generates a single order with `n >= 2`;
its one-graviton call still fails the rule's structural validation.


### Gauss–Bonnet construction limits

The order-three construction has been compared with direct FeynCalc contraction, but that comparison does not independently validate the action. The corrected order-two source and higher-order limitations are distinguished below. The four-and-higher-graviton branch has unresolved formula and indexing problems; successful lower-order generation does not validate that branch.

## Scalar-potential differentiation normalisation

The contracted and uncontracted scalar-potential routines include the n-graviton differentiation factor n! multiplying the averaged expansion tensor. This is independent of scalar multiplicity: a potential lambda phi^s/s! has no remaining scalar factorial in its vertex. The order-two stored potential library was corrected on 18 October 2026; kinetic libraries were unchanged. See verification ([scientific scope](../../ScientificScope.md#Scalar%20models)).

## Gauss–Bonnet order-two correction

The two-graviton `ScalarGaussBonnet` rule now contracts complete linearised Riemann, Ricci and scalar curvatures. The previous `TensorT` contraction omitted curvature subtraction terms and was not repairable by an overall factor. Only `Libs/ScalarGaussBonnet_2` was regenerated. Higher-order branches and libraries retain their historical implementation and require a separate mathematical audit. See verification ([scientific scope](../../ScientificScope.md#Scalar%20models)). Restart or reimport after replacing an already loaded old library.

## Graviton differentiation multiplicities

C/CE helpers are averaged Taylor coefficients. Direct interaction terms restore
`n!` for `n` gravitons; terms summing all graviton permutations must not receive
another factorial. See the correction and verification record ([scientific scope](../../ScientificScope.md#Scalar%20models)) for affected families and regeneration commands.

## Release 4.1.0 verification limits

The corrected two-graviton EsGB source is independently verified. Higher-order
legacy contractions and the shipped `ScalarGaussBonnet_3` require a separate
mathematical audit; the native EsGB model uses only the verified order-two
source. The axion epsilon-density/action convention also remains unresolved.
The recent multiplicity checks do not certify every underlying action or higher
order. See the [release notes](../../ReleaseNotes-4.1.0.md) and
scalar correction record ([scientific scope](../../ScientificScope.md#Scalar%20models)).

## Horndeski scientific corrections — 1 November 2026

G2/G3/G5 preserve labelled external-leg multiplicities for coincident values and localise temporary indices. The shared symmetrisation retains $2^n n!$ arrangements, including duplicates required by its averaged-coefficient normalisation. G5 uses the corrected $+b/3$ cubic-Hessian term and symmetric connection placements. Public signatures are unchanged.

The G5 source includes the higher graviton orders within the bounds above. Exact checks include the complete $n=4,a=0,b=1$ vertex and selected sectors at $n=4,b=2$ and $n=5,b=1$. Full five-graviton vertices and loop amplitudes are not certified. No higher-order stored library or native-model order is implied. G4 source and shipped-library checks are separate from the G2/G3/G5 checks; see [Horndeski scope and corrections](../../ScientificScope.md#Horndeski).

<!-- Compatibility anchors for former guide sections. -->
<a id="private-computational-helpers"></a><a id="historical-validation-checks"></a><a id="results-of-the-original-implementation-checks"></a><a id="gaussbonnet-construction-checks-6-october-2026"></a>
