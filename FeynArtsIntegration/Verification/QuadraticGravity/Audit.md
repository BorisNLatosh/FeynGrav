# Quadratic gravity internal propagator

This is a developer verification fixture. The two vector species in QGProbe are distinguishable index probes with unit symmetric-tensor vertices. The pole parameters belong to one internal tensor field. Its class mass is zero for the external massless branch. The external tensor placeholder has no physical wavefunction interpretation.

## Action, gauge and orders

With signature (+---), the package curvature convention, g = η + κh, and fixed independent m₀ and m₂, the invariant action is

$$
S=\frac{1}{\kappa^2}\int d^Dx\,\sqrt{-g}
\left[-2R+a R^2+b R_{\mu\nu}R^{\mu\nu}\right],
\qquad a=\frac{2/m_2^2+1/m_0^2}{3},\quad b=-\frac{2}{m_2^2}.
$$

The source is the sum in `Rules/QuadraticGravityVertex.wl`. Each field in the metric expansion contributes one κ, so an r-field term in this action carries κ to power r−2. The kinetic operator has order zero. The cubic and quartic vertices exhaust the pure-graviton inventory through κ² per vertex. The stored cubic and quartic files are `Libs/QuadraticGravityVertex_1` and `Libs/QuadraticGravityVertex_2`. A text inventory found the corresponding powers in all 1,818 and 60,054 summands respectively. Independent tensor checks of those interactions remain future work.

The conventional flat-background gauge adds $(2/\varepsilon)\int F_\mu F^\mu$, where $F_\mu=\partial^\nu h_{\mu\nu}-\partial_\mu h/2$. The source sets ε to 2 by default. This linear gauge gives a quadratic gauge-fixing term. The associated Faddeev–Popov vector has source propagator $-i\eta_{\mu\nu}/p^2$ and a cubic ghost–antighost–graviton interaction of order κ. Derivation and native verification of that interaction remain outstanding. The massive spin-2 tensor pole has negative residue and bosonic statistics.

## Inverting the quadratic action

Write $x=p^2$, $n=D-1$, $\omega_{\mu\nu}=p_\mu p_\nu/x$, $\theta=\eta-\omega$. Use the transverse traceless and spin-1 projectors $P_2,P_1$ in D dimensions and the orthonormal scalar slots $(\theta/\sqrt n,\omega)$. For a transverse traceless field, the square of the linear Ricci tensor supplies $x^2 h^2/4$ after factoring out κ². For a unit transverse scalar the squares of linear $R$ and $R_{\mu\nu}$ supply $nx^2$ and $Dx^2/4$ respectively. The pure longitudinal scalar is a linear diffeomorphism. Together with the Einstein kinetic term these give

$$
 K_2=x+bx^2/2,\qquad K_s=-(D-2)x+(2na+Db/2)x^2.
$$

The gauge adds $2x/\varepsilon$ in spin 1 and $(x/\varepsilon)(\sqrt n,-1)^T(\sqrt n,-1)$ to the scalar block $\mathrm{diag}(K_s,0)$. Inverting this two-by-two block keeps both scalar mixing terms. The test compares $iK^{-1}$ coefficient by coefficient with the native line and the public FeynGrav source. This also checks the conversion from the source's fixed one-third tensor basis to the D-dimensional traceless projector.

For noncoincident poles the scalar position is

$$
 M_s^2=\frac{3(D-2)m_0^2m_2^2}{(D-4)m_0^2+2(D-1)m_2^2}.
$$

In the moving projector basis, set $U=(\theta+n\omega)\otimes(\theta+n\omega)/n$. The residues including the propagator's $i$ are

| Pole | Residue in this basis |
| --- | --- |
| $0$ | $i(P_2+\varepsilon P_1/2-U/(D-2)+\varepsilon\,\omega\otimes\omega)$ |
| $m_2^2$ | $-iP_2$ |
| $M_s^2$ | $iU/(D-2)$ |

Projectors contain inverse powers of $x$. For free tensor components the test also compares the $x^{-3},x^{-2},x^{-1}$ Laurent coefficients at zero, holding the formal momentum components fixed. This distinguishes longitudinal kinematic singularities from additional propagating states. At $D=4$ the scalar mass squared reduces to $m_0^2$. The comparisons assume generic parameters and exclude singular dimensions, coincident poles and vanishing masses.

## Native test and scope

`Propagator.wls` loads the fixture with patched FeynArts, extracts the actual internal-line expression and tests the action inverse, the public rule, poles, residues and sign/mass negative controls. It also generates the single tree exchange between distinct pairs of vector probes, names its independent internal momentum, and compares the converted, contracted result with all four external slots free. The unit vertices and the chosen amplitude prefactor fix the overall normalisation. Run in a fresh bounded Wolfram kernel with `FA_QG_OUTPUT` set to a fresh directory. The fixture is self-contained and is deliberately outside the deliverable model directory.

The single-field internal representation passed in the tested versions. [Interaction checks](./Interactions/Audit.md) verify the local vertices, and the [native bundle tests](./Native/README.md) cover generated and converted rules. The massless external sector, Einstein limit and complete four-graviton Ward checks are documented in the [physical tree test](./Ward/README.md).
