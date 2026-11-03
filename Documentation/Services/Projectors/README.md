# Projectors and operator decomposition

[Documentation index](../../README.md) · [Complete symbol index](../../Symbols.md)

These public helpers are loaded from `Nieuwenhuizen`. All tensor objects are D-dimensional; the package retains the fixed `1/3` coefficients in [`NieuwenhuizenOperator0`](./NieuwenhuizenOperator0.md) and [`NieuwenhuizenOperator2`](./NieuwenhuizenOperator2.md). Do not replace them by `1/(D-1)` when interpreting this interface: the implemented basis and inverse use these conventions.

The transverse and longitudinal tensors are respectively `theta = eta - p p/p^2` and `omega = p p/p^2`. Their implementation uses FeynCalc propagator notation for inverse momentum squares. No singularity check at `p^2 = 0` is performed.

| Command | Returned object |
| --- | --- |
| [`GaugeProjector[mu, nu, p]`](GaugeProjector.md) | `theta(mu,nu)`. |
| [`GaugeProjectorBar[mu, nu, p]`](GaugeProjectorBar.md) | `omega(mu,nu)`. |
| [`NieuwenhuizenOperator1[mu, nu, alpha, beta, p]`](NieuwenhuizenOperator1.md) | Half the sum of the four mixed `theta omega` terms. |
| [`NieuwenhuizenOperator2[mu, nu, alpha, beta, p]`](NieuwenhuizenOperator2.md) | Symmetrised transverse identity minus `theta(mu,nu) theta(alpha,beta)/3`. |
| [`NieuwenhuizenOperator0[mu, nu, alpha, beta, p]`](NieuwenhuizenOperator0.md) | `theta(mu,nu) theta(alpha,beta)/3`. |
| [`NieuwenhuizenOperator0Bar[mu, nu, alpha, beta, p]`](NieuwenhuizenOperator0Bar.md) | `omega(mu,nu) omega(alpha,beta)`. |
| [`NieuwenhuizenOperator0BarBar[mu, nu, alpha, beta, p]`](NieuwenhuizenOperator0BarBar.md) | `theta(mu,nu) omega(alpha,beta) + omega(mu,nu) theta(alpha,beta)`, without an extra square-root normalisation. |
| [`NieuwenhuizenOperator[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]`](NieuwenhuizenOperator.md) | Linear combination in exactly this coefficient order: `1, 2, 0, 0Bar, 0BarBar`. |
| [`NieuwenhuizenOperatorInverse[z1, z2, z0, zb, zbb, mu, nu, alpha, beta, p]`](NieuwenhuizenOperatorInverse.md) | The implemented D-dependent inverse in the same basis, assuming its scalar denominators are nonzero. Singular cases are not diagnosed automatically. |
| ``Nieuwenhuizen`Private`NieuwenhuizenSymmetryCheck[T, mu, nu, alpha, beta, p]`` | Internal helper, not an exported command. Checks both within-pair swaps and exchange of the two pairs. Returns `True`, `False`, or a diagnostic `Failure`; requires usable FORM. |
| [`NieuwenhuizenOperatorExpansion[T, mu, nu, alpha, beta, p]`](NieuwenhuizenOperatorExpansion.md) | Returns `{z1,z2,z0,zb,zbb}` only after symmetry checks, coefficient extraction and complete reconstruction verification; requires usable FORM. |

There are no `LongForm` options. Projector constructors accept symbolic expressions rather than lists or associations for each argument. Structural errors return `Failure`, and incoming dependency failures propagate. Expansion is limited to tensors representable by this five-element basis with scalar coefficients. Unsupported residual tensor structures, inconclusive verification and calculation failures have distinct diagnostics. Check `FailureQ` before using coefficients; a failed FORM call is not a failed physical symmetry identity.

## Individual pages

| Symbol | Kind | Audience |
| --- | --- | --- |
| [``Nieuwenhuizen`GaugeProjector``](./GaugeProjector.md) | command | user |
| [``Nieuwenhuizen`GaugeProjectorBar``](./GaugeProjectorBar.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator``](./NieuwenhuizenOperator.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator0``](./NieuwenhuizenOperator0.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator0Bar``](./NieuwenhuizenOperator0Bar.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator0BarBar``](./NieuwenhuizenOperator0BarBar.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator1``](./NieuwenhuizenOperator1.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperator2``](./NieuwenhuizenOperator2.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperatorExpansion``](./NieuwenhuizenOperatorExpansion.md) | command | user |
| [``Nieuwenhuizen`NieuwenhuizenOperatorInverse``](./NieuwenhuizenOperatorInverse.md) | command | user |
