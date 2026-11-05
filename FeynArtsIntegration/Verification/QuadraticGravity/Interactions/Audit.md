# Cubic, quartic and ghost interaction checks

This verification covers conventional quadratic gravity in the FeynGrav mass parametrisation, with the linear de Donder gauge and all momenta incoming. The gravitational vertices are separated into the Einstein, inverse scalar-mass-squared and inverse spin-2-mass-squared sectors. The starting action is given in the [propagator audit](../Audit.md).

## Graviton derivation

Insert one labelled plane wave for each external field,
$h_{\mu\nu}(x)=\sum_j t_j e_{j\mu}e_{j\nu}\exp(i p_j x)$, with $t_j^2=0$ in the coefficient calculation. The square-free coefficient of all labels is the multilinear functional derivative, including its permutations. The $e_j$ are unrestricted. Polarising each $e_je_j$ recovers two independently chosen slots of a symmetric field. The stored tensor is separately checked for symmetry within every pair and under adjacent whole-leg permutations. These facts make the polynomial comparison a test of the complete tensor.

Expand the inverse metric with its Neumann series and the volume with $\exp[\operatorname{tr}\log(1+\kappa h)/2]$. Construct the Levi-Civita connection from one differentiated labelled field and the inverse metric. In $\partial_\rho\Gamma^\rho_{\mu\nu}$ the derivative acts on the whole connection, giving the sum of its labels' momenta. Use the derivative and connection-product terms of the Ricci tensor separately, then contract the inverse metrics in $R$, $R^2$ and $R_{\mu\nu}R^{\mu\nu}$. If $[L]_r$ denotes the labelled coefficient of $\sqrt{-g} L$ with the common metric-expansion powers stripped, the vertex is

$$
 i\kappa^{r-2}\left[-2[R]_r+
 \frac{[R^2]_r}{3m_0^2}+
 \frac{1}{m_2^2}\left(\frac{2}{3}[R^2]_r-2[R_{\mu\nu}R^{\mu\nu}]_r\right)\right].
$$

For the Einstein term the stored representative uses the bulk connection-product form of the action. A second calculation expands this form directly, including the sign from the two Fourier derivatives. The covariant and bulk derivations give the same integrated vertex. Imposing $\sum_j p_j=0$ implements the vertex delta function and leaves every independent momentum off shell.

All coefficients are exact rationals multiplying formal Gram monomials. There are no finite-dimensional Gram identities, transverse conditions or mass-shell substitutions. For an additional dimension-sensitive comparison, trace the first graviton of the stored tensor by joining its slots, and trace the derived polynomial with half the auxiliary-vector Laplacian. A closed metric trace gives symbolic $D$.

| Order | Stored input terms | Einstein bulk monomials | $1/m_0^2$ monomials | $1/m_2^2$ monomials |
| --- | ---: | ---: | ---: | ---: |
| Cubic | 1,818 | 57 | 84 | 342 |
| Quartic | 60,054 | 534 | 1,344 | 6,102 |

The bulk Einstein and both curvature-squared sectors have zero residual even with every incoming momentum independent. When the unintegrated Einstein curvature density is used instead, the residual has 120 cubic or 946 quartic monomials before momentum conservation and zero afterwards. Each symbolic trace agrees, the stored symmetries hold, and reversing the sign in any sector is detected.

## Faddeev–Popov derivation

For the ghost interaction, vary $F_\mu=\partial^\nu h_{\mu\nu}-\partial_\mu h/2$ by a diffeomorphism. Give the ghost momentum $q$ and auxiliary vector $v$, the antighost momentum $r$ and auxiliary vector $u$, and the graviton momentum $k$ and polarisation $ee$. The nonlinear metric variation supplies

$$
 B_{\mu\nu}=(k\cdot v)e_\mu e_\nu+
 (e\cdot v)(q_\mu e_\nu+q_\nu e_\mu).
$$

After moving the gauge derivative onto the antighost, the ordered vertex is $i\kappa(r^\mu u^\nu B_{\mu\nu}-(r\cdot u)B^\mu{}_{\mu}/2)$. This equals the public `GravitonGhostVertex` exactly for independent $k,q,r$. Its graviton trace agrees at generic $D$. Both an overall sign reversal and swapping ghost/antighost slots change the result. The free ordered source propagator agrees with $-i\eta_{\mu\nu}/q^2$. This gauge function is linear and the diffeomorphism of the metric is at most linear in $h$, so there is no quartic $hh\bar c c$ interaction.

## Reproduction and scope

Set `QG_RULE_ROOT` to the package root and `QG_OUTPUT` to a fresh output directory for each invocation. In separate bounded scopes, run `check_vertices.py 3`, `check_vertices.py 4` and `check_einstein_bulk.py` with Python 3, and `ghost.wls` with a fresh Wolfram kernel. The Python calculation reads the flat Pair-product grammar of the two extensionless libraries. The ghost check loads the public Wolfram rule. The tests compare local FeynGrav tensors; native model couplings and physical tree amplitudes will need their own checks.
