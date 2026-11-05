# Four-graviton tree check

The process is elastic scattering of four external positive-residue massless gravitons in four dimensions. The two native models are QuadraticGravity and the independently shipped GR model. Each exports one amputated contact and three exchange diagrams, using the native graph and statistics factors, `PreFactor -> 1`, exact routing, symbolic gauge choice and κ set to one. The comparison strips the overall factor of i. External tensor indices are kept through the export. Shared arithmetic nodes are serialized once; the evaluator keeps the native multiplicative factors separate so that the cubic currents can be contracted before sewing the internal tensor line. Internal Lorentz sums use the (+---) metric.

## Reproduce

Use a separate fresh, externally bounded Wolfram kernel for each model (1 GiB for the whole process tree, zero swap, 120 seconds plus at most 10 seconds for cleanup, and at least 2 GiB available host memory). Set `QG_RULE_ROOT` to the package directory, `QG_MODEL` to `QuadraticGravity` or `GR`, and `QG_OUTPUT` to a fresh output directory. Run `ExportTree.wls` with `WolframKernel -noinit -script`. The generated `amplitudes.json` contains the complete class amplitudes, including the propagator denominators carried by the projectors.

For each fixture, run this command in a bounded process, supplying a fresh existing directory and frame number 0 or 1:

```sh
python3 ward.py QG/amplitudes.json GR/amplitudes.json OUTPUT FRAME
```

## External data and checks

Write all momenta as incoming. In the centre-of-mass frame take
$k_1=(E,0,0,E)$, $k_2=(E,0,0,-E)$,
$k_3=(-E,-S,0,-C)$ and $k_4=(-E,S,0,C)$, with $S^2+C^2=E^2$.
On each leg choose two spacelike transverse vectors $a,b$ of equal norm, respectively in and out of the scattering plane. The real symmetric polarisations used here are $a\odot b$ and $(a+b)\odot(a-b)=aa-bb$, where $u\odot v=(uv+vu)/2$. They span the physical helicity space; their unnormalised scale is kept in the ledgers. Symmetry within each index pair permits evaluating a single ordered dyad.

| Frame | $(E,S,C)$ | $(s,t,u)$ | $(M_0,M_2,\varepsilon)$ |
| --- | --- | --- | --- |
| 0 | $(5,3,4)$ | $(100,-10,-90)$ | $(7,11,2)$ |
| 1 | $(13,12,5)$ | $(676,-208,-468)$ | $(17,31,5)$ |

Per frame the test evaluates all 16 physical tensor products and checks the full finite-mass amplitude against GR. It also compares the Einstein limit of each diagram to its GR counterpart. For the Ward identity it replaces each external tensor by $p\odot\xi$, testing four independent directions for $\xi$ and eight polarisations of the other legs. Each of the 128 exact residuals vanishes. The ledger retains an explicit nonzero residual for dropping each diagram. The two frames establish these exact identities at the stated kinematics. Earlier local comparisons cover symbolic D and generic off-shell momenta.
