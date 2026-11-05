# FeynGravConventions

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynGravConventions``. **Baseline:** 4.2.0.

```mathematica
FeynGravConventions[]
```

Prints a fully visible, static report and returns `Null`. The command takes no arguments or options; invalid calls return `Failure`. It is listed by `FeynGravCommands[]` and has `?` help. Mathematical formulas appear in notebook notation, with tables protected by `StandardForm`; kernels without a front end receive plain text.

The six sections cover:

1. The curvature definition, positive Fourier phase, its D-dimensional extension, metric expansion and gravitational coupling normalisation. Cheung–Remmen variables are a separate parametrisation.
2. Lorentz dimensions, incoming vertex momenta and the current assignment to `D`.
3. Initial and current gauge parameters, actual loaded orders, and differences between recorded import settings and the current session.
4. Momentum-first polarisation syntax, conjugation labels, current transversality defaults and limitations of the tensor product shortcuts.
5. Rank-four epsilon conventions, trace normalisation, the current FeynCalc Dirac scheme, supported Dirac operations and fundamental SU(N) normalisation. The independent defaults of `CalcFormExport`, `CalcFormCalculate`, `FeynGravFORMExport` and `FeynGravSimplify` are shown separately, together with the configured `Method` and the distinction between the full pipeline and restricted methods.
6. Factors already supplied by interaction rules and factors the converter does not add, together with the limits of ordinary `FAD` prescription information.

Fixed formulas are inert display data: assignments to symbols do not change them. Historical import settings remain held when displayed. The report reads the existing [library import records](../LibraryManagement/FeynGravLibraryInformation.md); it does not import libraries, calculate tensors, launch FORM, write files or change settings. It is not printed automatically on loading.

“Not recorded” means that provenance is unavailable. Records describe the last successful import, not historical generation settings, manually redefined vertices or previously calculated user expressions. Reimport a library when a gauge value was substituted into its vertices. Other setting differences need not affect every expression. Use `FeynGravLibraryInformation[]` for detailed diagnostic data, including paths and hashes.

FeynCalc's Cartesian metric setting is reported separately from the package's spacetime convention; changing it does not transform stored interactions. Likewise, displaying a FeynCalc Dirac scheme does not imply converter support for all operations in that scheme. Unavailable dependency settings are labelled explicitly. The effective omitted transversality setting for all four polarisation shortcuts is read from `Options[Polarization]`; wrapper option declarations do not override it. Delayed option defaults (`:>`), including `Method`, are displayed as held expressions labelled “Delayed default”, without evaluating them or choosing explanatory text from their values.

Representative curvature and momentum-space checks and the display checks are recorded in [Conventions verification](../../Conventions.md). They do not constitute a proof for every interaction or perturbative order.
