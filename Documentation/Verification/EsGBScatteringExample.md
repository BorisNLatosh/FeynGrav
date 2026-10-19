# EsGB scalar annihilation example: historical longitudinal failure

**Resolved:** the missing minimal scalar contact multiplicity was corrected and
both generic graviton Ward identities now pass in the rebuilt model. See
[the correction record](ScalarWardCorrection.md). The report below preserves
the earlier failed snapshot; current notebook edits and saved outputs were not
re-evaluated by the correction check.

Checked on 19 October 2026. Example 11 constructs the native tree process
$\phi(p_1)\phi(p_2)\to h(p_3)h(p_4)$ with the installed
`Einstein-scalar-Gauss-Bonnet` bundle. No model or source formula was changed.

## What was executed

All ten computational notebook cells executed in a fresh kernel. The example
loads FeynGrav and the model, displays six diagrams, assigns on-shell kinematics,
contracts the outgoing factorised polarisations, and presents the calculated
amplitude as minimal, $g_2$ and $g_1^2$ contributions. No FORM execution is needed.

The diagnostic confirmed the contact, the massless s-channel exchange, the
massive scalar t/u exchanges and the additional massless t/u exchanges. The
coupling polynomial contains all three contributions with orders $\kappa^2$,
$\kappa^2g_2$ and $\kappa^4g_1^2$. No free Lorentz indices, propagator heads or
unconverted model parameters remained. These are construction checks, not
acceptance of the physical amplitude.

## Exact residuals

The vectors $e_3,e_4$ denote conjugated outgoing polarisations, satisfying
$e_i^2=e_i\cdot p_i=0$, with momentum conservation imposed in their scalar
products. They have no chosen helicity or normalisation. With
$s+t+u=2m^2$, the calculated minimal contribution gives

$$\left.\mathcal M_{\mathrm{minimal}}\right|_{e_3\to p_3}
=\frac{\kappa^2}{8}(t-u)(p_3\cdot e_4)
\left[2(p_1\cdot e_4)-p_3\cdot e_4\right],$$

$$\left.\mathcal M_{\mathrm{minimal}}\right|_{e_4\to p_4}
=-\frac{\kappa^2}{8}(t-u)(p_4\cdot e_3)
\left[2(p_1\cdot e_3)-p_4\cdot e_3\right].$$

Both are generically nonzero. The $g_2$ and $g_1^2$ contributions separately give
zero under each replacement. The displayed minimal residuals were also checked
by an independent SymPy rational simplification of the saved amplitude. That
cross-check confirms the algebra of the saved expression, not its derivation.

**Historical status at this run: unresolved.** The root cause has not been established. Do not infer a
specific source-rule defect from these residuals alone. The prior native
factor-by-factor agreement with source rules remains a different, narrower
check. No general physical Ward-identity claim is made for this example.

## Runs and retained evidence

The initial run completed in 8.398 seconds and stopped on the failed embedded
longitudinal checks. A diagnostic run completed in 8.092 seconds and retained
separate residuals and native diagram tensors. It passed its first nine checks,
including reproduction of the failure; its tenth, an auxiliary polarisation
rescaling comparator, returned false and stopped the run. The eleventh check was
not executed. Neither run is recorded as a passing validation suite.

The saved amplitude is polynomial and homogeneous of degree two in each
polarisation, as checked separately in its scalar-product polynomial. The
kernel rescaling comparator remains unvalidated and is not part of the notebook.

Both launches used a task-owned whole-process-tree cap of 1 GiB, zero swap,
120 seconds plus ten seconds termination grace, and fresh host headroom above
2 GiB. No timeout or memory termination was observed. Existing kernels were
not touched. Existing example notebooks were preserved byte-for-byte. The new
notebook has no saved output cells; its front-end layout has not been visually
reviewed in Mathematica.

Retained local evidence directories under
`/home/latosh/Documents/Codex/FeynGrav-evidence/`:

- `2026-10-19-esgb-scattering-example/`: initial notebook and check, saved amplitude,
  logs and protected-input snapshots.
- `2026-10-19-esgb-scattering-example-diagnostic/`: diagnostic script, native diagram
  tensors, separate longitudinal residuals and resource records.

The delivered notebook adds an explicit validation-status notice to the tested
computational cells. The next task is to isolate the minimal-sector residual
against an independent amplitude derivation, checking external-index assignment,
conversion conventions and the relevant source factors before changing a rule.
