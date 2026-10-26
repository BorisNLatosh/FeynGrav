# Yang–Mills construction and focused verification

`../BuildGRSUNYM.wl` builds a new candidate into an explicit fresh absolute
directory. It reuses the common private Lorentz basis/encoding helpers, verifies
the quartic colour reconstruction and records sources/model hashes. Run from
the repository root, under an external resource limiter:

```sh
FG_MODEL_TARGET=/absolute/new/candidate \
     WolframKernel -noinit -script FeynArtsIntegration/BuildGRSUNYM.wl
```

A launcher can set the same explicit `FG_MODEL_TARGET`. The builder does
not install or certify the model. Its source libraries must already be generated
from the corrected rules; it must not compensate for old source normalisation.
Source generation uses the existing library generator, not model loading.

The portable native tree check uses the installed repository bundle with
FeynCalc/patched FeynArts and serial FORM through `FeynGravSimplify`. Select exactly one process per fresh kernel:

```sh
FG_YM_PROCESS=VVTT FG_YM_EVIDENCE=/absolute/new/evidence \
  WolframKernel -noinit -script FeynArtsIntegration/Verification/YangMillsTrees.wls
```

Choices: `VVVV` (AA → AA), `VVVT` (AA → Ah), `VVTT` (AA → hh). Each has 13
assertions: loading, symbolic parameters, species-labelled channels, conversion,
index/coupling checks, four exact Ward identities, a missing-contact negative
control and a nonzero physical amplitude. `completion.json`, raw diagnostics,
converted amplitudes and residuals are retained. A zero exit does not permit
ignoring a nonempty diagnostic log. These scripts do not enforce resource limits.

For the development checks, use sequential task-owned fresh kernels with a
whole-process-tree 1 GiB cap, zero swap, 120 seconds plus at most 10 seconds
termination grace, and fresh available host memory of at least 2 GiB. Stop and
review failures/warnings/resource events; never affect existing user kernels.
A command timeout or per-process RSS monitor alone does not enforce this policy.

The [delivery report](../../Documentation/Verification/GRSUNYM-2026-10-26.md)
records independent source, ghost-sign, all-contact and tree evidence, including
historical harness failures. The portable tree checks are a subset, not the full
source-action audit. The existing bridge's loop gates remain unchanged.

The quark extension adds a private ordered-Dirac companion,
`../FermionModelBasis.wl`, and five families to the same builder. Its finite
one/three-gamma basis preserves chain order and rejects unsupported objects.
The gravitational vector ghost now uses `S`, leaving `F` for quarks. See the
[quark verification report](../../Documentation/Verification/GRSUNYMQuarks-2026-10-26.md)
for independent action checks, all five native contacts, selected massive trees,
identical-quark signs and the evaluated FORM example.

The graviton Ward check now uses the direct symmetric variation
`(A[e+p] - A[e-p])/2`. Extracting a formal coefficient without first expanding
all momentum dependence can produce a vacuous zero. The corrected check leaves
the tested gauge vector arbitrary. FORM handles the tensor algebra; FeynCalc
handles notation, routing and the final commuting rational comparison.
Successful FORM job files are retained in the evidence directory. External
resource controls still apply; the FA-YM-02 follow-up used the maintainer's
explicit 600-second override, with the same memory/swap/grace limits.
