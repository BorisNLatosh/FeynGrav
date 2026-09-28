# Version-one compatibility fixture

`v1.map.json` and `v1.out` are fixed artifacts created by the original exporter and FORM 4.3 before its maintainability refactor. They preserve a D-dimensional metric trace, a context-qualified Greek coupling, a repeated massive propagator, an exact imaginary coefficient and A0.

The input was:

```mathematica
MTD[mu, nu] MTD[mu, nu] + GreekContext`\[Kappa] FAD[{l, m}]^2 + I/3 + A0[m^2]
```

with `LoopMomenta -> {l}`. All other input symbols belong to Global`, except System`D and FeynCalc heads.

Do not rewrite or regenerate these artifacts in ordinary test runs. `Core.wls` checks their exact restored expression. See `../../FORMAT.md` for the compatibility policy and mapping hash definition.
