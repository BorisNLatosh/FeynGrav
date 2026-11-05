# \[Kappa]

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`\[Kappa]``. **Baseline:** 4.2.0.

Gravitational expansion coupling κ, with $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$ and $\kappa^2=32\pi G_N$. Ordinary GR r-graviton rules carry $\kappa^{r-2}$; minimally coupled matter rules with n gravitons carry $\kappa^n$. Couplings in non-minimal actions may themselves contain κ.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`\[Kappa] -> value
```

`expression` is a calculated vertex or amplitude and `value` is your chosen gravitational coupling. For example, use `Sqrt[32 Pi GN]` to express κ through an unassigned Newton constant `GN`; apply the replacement to the completed expression rather than assigning κ before import.

## Related documentation

- [Model or conventions](../../Models/GeneralRelativity/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
