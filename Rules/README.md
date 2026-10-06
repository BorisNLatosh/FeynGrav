# Interaction rules, validation and failures

## Which interface to use

Ordinary calculations use the library-backed commands in the [main-package reference](../Documentation/Reference.md). This directory implements rule construction for the [library generator](../Libs/Generator.md), plus the Nieuwenhuizen helpers automatically loaded by FeynGrav. Use a separate fresh kernel for rule construction; identical short function names in different package contexts are not interchangeable.

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

`RuleValidation.wl` has no FeynCalc dependency and launches no processes on loading. `RuleCall` holds the calculation until validation succeeds. Each public or private computational call has explicit requirements beside its definition. A private tagged failure boundary prevents dependency failures from entering subsequent algebra. `RuleRequire` checks dependency results and list containers; it does not repeatedly traverse successful tensor expressions.

Memoised functions assign a cached result only after successful evaluation. Failed, invalid and aborted calls do not acquire cached values. Previously successful cached entries retain normal Wolfram direct dispatch. User aborts propagate normally.

`RuleParallelMap` establishes a worker boundary, distributes the callback definitions and collects worker results. The first failure in input order is returned before results are combined. It does not launch workers during package loading. Tests using serial substitutes for `ParallelMap` must also substitute `RuleParallelMap` to avoid distributing definitions.

Requirements are written as `{position, "Array", blockSize, minimumLength, maximumLength}`, `{position, "Integer", minimumValue}`, `{position, "Expression"}`, or `{0, "Supported", condition, explanation}`. Checks run in the listed order. Add dependency-result checks before arithmetic, mapping consumers or expensive simplification when extending a formula. Do not cache intermediate failure objects or replace warnings indiscriminately with failures.

## Function contracts

Positions refer to each displayed signature. `Infinity` means no upper bound. Every listed signature rejects other argument counts; overloads are listed separately. Private helpers are included because they also slice arrays and perform calculations. Local functions inside a `Module` are implementation details covered by the surrounding validated call.

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
| ``HorndeskiG5`HorndeskiG5`` — `HorndeskiG5[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; The G5 TII dispatcher is implemented only through three gravitons. Condition: Length[gravitonParameters] <= 9 \|\| b == 0. |
| ``HorndeskiG5`HorndeskiG5Uncontracted`` — `HorndeskiG5Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; The G5 TII dispatcher is implemented only through three gravitons. Condition: Length[gravitonParameters] <= 9 \|\| b == 0. |
| ``ScalarGaussBonnet`ScalarGaussBonnet`` — `ScalarGaussBonnet[gravitonParameters]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``GravitonFermionVertex`GravitonFermionVertexUncontracted`` — `GravitonFermionVertexUncontracted[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonFermionVertex`GravitonFermionVertex`` — `GravitonFermionVertex[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``HorndeskiG4`HorndeskiG4`` — `HorndeskiG4[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity; The contracted multi-graviton G4 branch for b >= 2 is not implemented. Condition: !(Length[gravitonParameters] >= 6 && b >= 2). |
| ``HorndeskiG4`HorndeskiG4Uncontracted`` — `HorndeskiG4Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``GravitonVertex`GravitonVertex`` — `GravitonVertex[indexArray]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``GravitonVertex`GravitonVertexUncontracted`` — `GravitonVertexUncontracted[indexArray]` | argument 1: flat list, block size 3, length 6 to Infinity |

### Private computational helpers

| Function and signature | Structural requirements |
|---|---|
| ``GravitonVectorVertex`Private`TakeLorenzIndices`` — `TakeLorenzIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonVectorVertex`Private`FReduced`` — `FReduced[\[Mu], \[Nu], \[Sigma], \[Lambda]]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`Private`GravitonVectorVertex1`` — `GravitonVectorVertex1[indexArray, \[Lambda]1, p1, \[Lambda]2, p2]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`Private`GravitonVectorVertex3`` — `GravitonVectorVertex3[indexArray, \[Lambda]1, p1, \[Lambda]2, p2]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`Private`GravitonVectorVertex4`` — `GravitonVectorVertex4[indexArray, \[Lambda]1, p1, \[Lambda]2, p2]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``GravitonVectorVertex`Private`GravitonVectorVertex5`` — `GravitonVectorVertex5[indexArray, \[Lambda]1, p1, \[Lambda]2, p2]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association) |
| ``HorndeskiG3`Private`MomentaWrapper`` — `MomentaWrapper[scalarMomenta]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``HorndeskiG3`Private`DummyArray2`` — `DummyArray2[n]` | argument 1: explicit integer >= 0 |
| ``HorndeskiG3`Private`takeIndices`` — `takeIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``HorndeskiG3`Private`HorndeskiG3Core`` — `HorndeskiG3Core[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG3`Private`HorndeskiG3UncontractedCore`` — `HorndeskiG3UncontractedCore[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``QuadraticGravityVertex`Private`TakeLorenzIndices`` — `TakeLorenzIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``QuadraticGravityVertex`Private`GRVertex`` — `GRVertex[indexArray]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``QuadraticGravityVertex`Private`VertexCurvatureSquare`` — `VertexCurvatureSquare[gravitonParameters]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``QuadraticGravityVertex`Private`QuadraticGravityVertexCore`` — `QuadraticGravityVertexCore[gravitonParameters, m0, m2]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association) |
| ``CTensorGeneral`Private`CTensorPlain`` — `CTensorPlain[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`CTensor`` — `CTensor[indexArray]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C1TensorPlain`` — `C1TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C1Tensor`` — `C1Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C2TensorPlain`` — `C2TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C2Tensor`` — `C2Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C3TensorPlain`` — `C3TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C3Tensor`` — `C3Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C4TensorPlain`` — `C4TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C4Tensor`` — `C4Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C5TensorPlain`` — `C5TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C5Tensor`` — `C5Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C6TensorPlain`` — `C6TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C6Tensor`` — `C6Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C7TensorPlain`` — `C7TensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CTensorGeneral`Private`C7Tensor`` — `C7Tensor[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity |
| ``CETensor`Private`CETensorIndexBlock`` — `CETensorIndexBlock[indexArray, firstPair, numberOfPairs]` | argument 1: flat list, block size 2, length 0 to Infinity; argument 2: explicit integer >= 0; argument 3: explicit integer >= 0; The requested pair slice exceeds the input array. Condition: numberOfPairs == 0 \|\| firstPair + numberOfPairs <= Length[indexArray]/2. |
| ``CETensor`Private`EInverseTensorPlain`` — `EInverseTensorPlain[indexArrayExternal, indexArrayInternal]` | argument 1: flat list, block size 1, length 2 to 2; argument 2: flat list, block size 2, length 0 to Infinity |
| ``Nieuwenhuizen`Private`tensorZeroStatus`` — `tensorZeroStatus[expr]` | Arity only; accepts the expression required by the surrounding calculation. |
| ``Nieuwenhuizen`Private`calculateTensor`` — `calculateTensor[expr, stage]` | Arity only; accepts the expression required by the surrounding calculation. |
| ``Nieuwenhuizen`Private`NieuwenhuizenSymmetryCheck`` — `NieuwenhuizenSymmetryCheck[T, m, n, a, b, p]` | argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association) |
| ``HorndeskiG2`Private`MomentaWrapper`` — `MomentaWrapper[scalarMomenta]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``HorndeskiG2`Private`DummyArray2`` — `DummyArray2[n]` | argument 1: explicit integer >= 0 |
| ``HorndeskiG5`Private`MomentaWrapper`` — `MomentaWrapper[scalarMomenta]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``HorndeskiG5`Private`DummyArray2`` — `DummyArray2[n]` | argument 1: explicit integer >= 0 |
| ``HorndeskiG5`Private`takeIndices`` — `takeIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``HorndeskiG5`Private`T1`` — `T1[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T2`` — `T2[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T3`` — `T3[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T4`` — `T4[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 9 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`TI`` — `TI[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T5`` — `T5[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 1; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T6`` — `T6[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 1; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T7`` — `T7[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 6 to Infinity; argument 3: explicit integer >= 1; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`T8`` — `T8[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 9 to Infinity; argument 3: explicit integer >= 1; argument 2: flat list, block size 1, length 2 b + 1 to Infinity |
| ``HorndeskiG5`Private`TII`` — `TII[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 1; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; The G5 TII dispatcher is implemented only through three gravitons. Condition: Length[gravitonParameters] <= 9. |
| ``ScalarGaussBonnet`Private`MomentaWrapper`` — `MomentaWrapper[scalarMomenta]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``ScalarGaussBonnet`Private`DummyArray2`` — `DummyArray2[n]` | argument 1: explicit integer >= 0 |
| ``ScalarGaussBonnet`Private`takeIndices`` — `takeIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``ScalarGaussBonnet`Private`TensorT`` — `TensorT[m, n, a, b, r, s, l, t, secondArray, indexArray]` | argument 9: flat list, block size 2, length 0 to Infinity; argument 10: flat list, block size 2, length 0 to Infinity; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association) |
| ``ScalarGaussBonnet`Private`ScalarGaussBonnetCore`` — `ScalarGaussBonnetCore[gravitonParameters]` | argument 1: flat list, block size 3, length 6 to Infinity |
| ``GravitonFermionVertex`Private`TakeLorentzIndices`` — `TakeLorentzIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonFermionVertex`Private`TakeLorenzIndices`` — `TakeLorenzIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonFermionVertex`Private`TakeMomenta`` — `TakeMomenta[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonFermionVertex`Private`GravitonFermionVertexGravitonLegs`` — `GravitonFermionVertexGravitonLegs[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonFermionVertex`Private`GravitonFermionVertexPermutedIndexArrays`` — `GravitonFermionVertexPermutedIndexArrays[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``GravitonFermionVertex`Private`GravitonFermionVertexKineticMassOrderedUncontracted`` — `GravitonFermionVertexKineticMassOrderedUncontracted[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonFermionVertex`Private`GravitonFermionVertexSpinConnectionOrderedUncontracted`` — `GravitonFermionVertexSpinConnectionOrderedUncontracted[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``GravitonFermionVertex`Private`GravitonFermionVertexOrderedUncontracted`` — `GravitonFermionVertexOrderedUncontracted[indexArray, p1, p2, mass]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association) |
| ``HorndeskiG4`Private`MomentaWrapper`` — `MomentaWrapper[scalarMomenta]` | argument 1: flat list, block size 2, length 0 to Infinity |
| ``HorndeskiG4`Private`DummyArray2`` — `DummyArray2[n]` | argument 1: explicit integer >= 0 |
| ``HorndeskiG4`Private`takeIndices`` — `takeIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
| ``HorndeskiG4`Private`T1`` — `T1[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`T2`` — `T2[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`T3`` — `T3[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`T4`` — `T4[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`T5`` — `T5[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`HorndeskiG4Core`` — `HorndeskiG4Core[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity; The contracted multi-graviton G4 branch for b >= 2 is not implemented. Condition: !(Length[gravitonParameters] >= 6 && b >= 2). |
| ``HorndeskiG4`Private`HorndeskiG4Core1`` — `HorndeskiG4Core1[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity; The contracted multi-graviton G4 branch for b >= 2 is not implemented. Condition: !(Length[gravitonParameters] >= 6 && b >= 2). |
| ``HorndeskiG4`Private`HorndeskiG4CoreUncontracted`` — `HorndeskiG4CoreUncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``HorndeskiG4`Private`HorndeskiG4Core1Uncontracted`` — `HorndeskiG4Core1Uncontracted[gravitonParameters, scalarMomenta, b]` | argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity |
| ``GravitonVertex`Private`TakeLorenzIndices`` — `TakeLorenzIndices[indexArray]` | argument 1: flat list, block size 3, length 0 to Infinity |
## Known limitations exposed by validation

- `CTensorGeneral` and `CTensorPlainGeneral` implement zero through seven external index pairs.
- Contracted G4 with multiple gravitons and `b >= 2` remains unimplemented and returns `UnsupportedConfiguration`.
- G5's `TII` dispatcher supports zero through three gravitons. Its contribution for `b > 0` cannot be requested above that range. The `b = 0` path is separate.
- Rule validation covers construction. Staged publication and batch failure handling are implemented separately by the migrated [generator](../Libs/Generator.md#failures-and-safe-replacement); they are not properties supplied by rule validation alone.

The G5 uncontracted routine now memoises its own successful result, rather than overwriting the contracted routine's cache. This is a cache correction, not a change to either formula.

## Historical validation checks

Verification scripts are kept outside the repository; no top-level `Tests` folder is required. Checks cover all listed signatures, malformed array shapes, invalid counts, injected dependency failures, parallel failure order, cancellation, cache isolation, valid baseline comparisons, loading and representative timings. These are regression checks, not a proof of every generated vertex at arbitrary order.

### Results of the original implementation checks

- 1,076 structural-validation and diagnostic assertions passed without secondary Wolfram messages.
- Of 142 sampled public/private signatures, 141 agreed with the saved pre-change results. The remaining baseline case was a malformed one-graviton uncontracted vector calculation. Its triplet-to-pair conversion has since been corrected in both branches; contraction of the corrected result agrees exactly with `GravitonVectorVertex` for one and two gravitons, with symbolic momenta and gauge parameter.
- All 21 Nieuwenhuizen regression checks and 12 SUNYM comparisons (zero, one and two gravitons) passed. Nieuwenhuizen reconstruction was also checked with real FORM.
- Ten failure-propagation/cache/cancellation checks and fourteen boundary/compatibility checks passed. Real parallel workers preserved failure order and successful symmetrisation.
- The validation helper loaded without FeynCalc. Loading all rule files preserved the working directory and search path. Main-package and generator loading passed.
- A small local timing sample of 40 fresh `CTensorGeneral` calls took 0.003728 s before and 0.025334 s after validation. Repeated batches of 1,000 cached calls took about 0.000363 s and 0.000349 s respectively. These observations show the cost of validation on tiny fresh calls; they are not portable performance guarantees.

## Dimensional quark and axion conventions

`GravitonQuarkGluonVertexUncontracted` retains the D-dimensional gamma matrices
returned by `QuarkGluonVertex[..., Explicit -> True]` and the named coupling
`SMP["g_s"]`. There is no four-dimensional gamma substitution for FORM transport.

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
