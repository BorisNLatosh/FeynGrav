# Native quadratic-gravity model

Use `"QuadraticGravity"` for both the FeynArts model and the generic model. The bundle provides the conventional linear de Donder gauge. Its fields are one self-conjugate symmetric tensor `T[1]` and a Grassmann-odd Faddeev–Popov vector `U[1]` with antighost `-U[1]`. The tensor line carries the massless, massive spin-2 and scalar poles. The massive spin-2 contribution has negative residue.

The interactions are the cubic and quartic graviton contacts and the cubic graviton–antighost–ghost contact. Their orders are κ, κ² and κ respectively. The graviton vertices use the Einstein term and both curvature-squared terms of the [quadratic-gravity action](./README.md). The field order in the classes ghost coupling is tensor, antighost, ghost.

## Generate an off-shell contact

In a fresh kernel with FeynCalc-patched FeynArts, evaluate

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
FeynArts`InitializeModel["QuadraticGravity",
  FeynArts`GenericModel -> "QuadraticGravity"];

insertions = FeynArts`InsertFields[
  FeynArts`CreateTopologies[0, 1 -> 2, FeynArts`Adjacencies -> {3}],
  {FeynArts`T[1]} -> {FeynArts`T[1], FeynArts`T[1]},
  FeynArts`Model -> "QuadraticGravity",
  FeynArts`GenericModel -> "QuadraticGravity",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[insertions, FeynArts`Truncated -> True,
  FeynArts`PreFactor -> 1, FeynArts`MomentumConservation -> False];
vertex = FCFAConvert[raw, IncomingMomenta -> {p1},
  OutgoingMomenta -> {-p2, -p3}, ChangeDimension -> D, List -> False];
```

The minus signs in `OutgoingMomenta` give the three-incoming convention of the stored vertex. For standalone loading, add the bundle directory to ``FeynArts`$ModelPath`` as described in the [model catalogue](../FeynArts.md). The `.gen` and `.mod` files contain their own tensor and coefficient definitions.

[Example 14](../../Examples/README.md#example-14-four-gravitons-in-quadratic-gravity) follows the scattering calculation in example 04 and compares three massless four-graviton helicity amplitudes with its GR expressions.

## Parameters and scope

The mass parameters are the symbols ``QuadraticGravityModel`M0`` and ``QuadraticGravityModel`M2``. With generic nondegenerate parameters the scalar pole is

$$
M_s^2(D)=\frac{3(D-2) M_0^2 M_2^2}{(D-4)M_0^2+2(D-1)M_2^2}.
$$

It reduces to $M_0^2$ at $D=4$. The spin-2 pole is at $M_2^2$. The full internal numerator includes longitudinal projector denominators. The gauge parameter is $\varepsilon=2\,\mathrm{FAGaugeXi}(T[1])$. The default FeynArts gauge rules set ε to 2. Use ``FeynArts`GaugeRules -> {}`` in `CreateFeynAmp` to keep the gauge parameter symbolic. The coupling is ``FeynGrav`\[Kappa]``.

## External states

Use the massless, positive-residue pole of `T[1]` for external gravitons. For a four-dimensional physical tree amplitude take $p^2=0$, $p^\mu e_{\mu\nu}=0$, $e^\mu{}_\mu=0$, with $e_{\mu\nu}$ symmetric. The two helicities are $+2$ and $-2$. For orthonormal spacelike transverse vectors $a,b$ one may take $e^{\pm2}=\epsilon^\pm\otimes\epsilon^\pm$, where $\epsilon^\pm=(a\pm i b)/\sqrt{2}$. Use the conjugate outgoing polarisation in a physical scattering convention and cross the outgoing momenta consistently into the all-incoming convention. `Truncated -> True` supplies the amputated tensor to contract with these states.

The massive scalar and negative-residue spin-2 pole propagate internally. External massive states would require separate external wavefunctions and an explicit asymptotic-state prescription. The Grassmann vector is a Faddeev–Popov ghost. With four external gravitons the tree diagrams are the quartic contact and the $s$, $t$ and $u$ tensor exchanges.

## Einstein limit and complete tree checks

Keep $\kappa$, the conventional gauge and the external kinematics fixed. Set $M_0=\Lambda\bar M_0$ and $M_2=\Lambda\bar M_2$ with a fixed nondegenerate positive mass ratio, then take $\Lambda\to\infty$. At fixed channel invariant away from exceptional poles, $M_s^2(D)$ scales as $\Lambda^2$. The curvature-squared vertex coefficients and the two heavy internal pole contributions fall as $\Lambda^{-2}$. The surviving contact and exchange factors give the Einstein tree in the same gauge. At four dimensions $M_s^2=M_0^2$.

At $(s,t,u)=(100,-10,-90)$ and $(676,-208,-468)$, all 16 physical polarisation combinations at each point reproduced the independently generated GR tree even at finite masses. Individual diagrams differed. In the decoupling limit each diagram separately reproduced its GR counterpart. This finite-mass cancellation for massless external gravitons is also derived in [Donà et al., *Scattering amplitudes in super-renormalizable gravity*](https://arxiv.org/abs/1506.04589).

For each leg the complete tree obeyed the Ward identity under $e_{\mu\nu}\to p_\mu\xi_\nu+p_\nu\xi_\mu$, with the other three legs physical. Four independent $\xi$ directions and both real transverse polarisations on each remaining leg gave 256 exact zero residuals across the two kinematic points. Each omitted-diagram control produced a nonzero residual. The local vertices and propagator have separate symbolic-$D$ off-shell checks. The physical scattering checks cover the specified four-dimensional massless tree process.
