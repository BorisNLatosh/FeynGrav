# Optional dependencies

[Documentation index](./README.md) · [Installation guide](./UserGuide.md#Requirements) · [Example notebooks](./Examples/README.md)

FeynCalc is required by FeynGrav. The tools below are needed only for the workflows listed. Installing FeynGrav does not install FORM, Contractor or FeynHelpers. Follow the upstream installation instructions for your operating system, then restart the Mathematica kernel. The checks below are small setup checks, not verification of a complete example or physical result.

| Workflow | Additional tools |
| --- | --- |
| Retrieve stored vertices and propagators | None beyond FeynCalc |
| Full `FeynGravSimplify` pipeline or library generation | FORM; TFORM for multiple workers |
| Native diagram construction (examples 04 and 08–13) | FeynCalc-patched FeynArts |
| Analytic integral and pole evaluation in example 08 | FeynHelpers, including its Package-X component |
| Explicit `Method -> "Contractor"` | A compatible Contractor executable; not needed by the numbered examples |

Example 08 combines FeynArts, FORM and FeynHelpers. Other notebooks' requirements are listed in the [notebook catalogue](./Examples/README.md). Use a fresh kernel for each independent setup below.

<a name="FORM and TFORM"></a>

## FORM and TFORM

Obtain FORM/TFORM from the [official FORM project](https://github.com/form-dev/form) or your operating system's packages. The FeynGrav converter has been checked with version 4.3. Its explicit [installation helper](./Services/Calculation/CalcFormInstall.md) supports Debian/Ubuntu; other systems need their own installation procedure. Serial FORM is sufficient to start.

After loading FeynGrav, check the serial executable:

```mathematica
<< FeynGrav`
status = FeynGravFORMCheck[FORMThreads -> 1];
AssociationQ[status] && TrueQ[status["Available"]]
```

The last line should return `True`. Otherwise inspect `status`. If the executable works in a terminal but is not found here, supply its absolute path with `FORMExecutable` in both the check and subsequent calculation calls. A path supplied to one call does not change later defaults. For example, use `FORMExecutable -> "/absolute/path/to/form"` with `FORMThreads -> 1`.

To check TFORM, use `FeynGravFORMCheck[FORMThreads -> 2]`; an explicit path must point to `tform`. More than one worker requires TFORM. Automatic selection and fallback rules depend on the method; see [executable policies](./Services/Calculation/Methods.md).

<a name="FeynArts"></a>

## FeynArts

Use the version patched for FeynCalc. FeynCalc's installer offers to download and patch FeynArts; an existing installation can also be updated using the [official FeynArts integration instructions](https://feyncalc.github.io/FeynCalcBook/Extra/FeynArts.html). Patching prevents name clashes between the two packages. Do not load an unpatched FeynArts installation into the same kernel.

After installation, start a fresh kernel and request the add-on before loading FeynGrav:

```mathematica
$LoadAddOns = {"FeynArts"};
<< FeynGrav`
FeynArts`InitializeModel["GR", FeynArts`GenericModel -> "GR"];
```

FeynArts should load and initialise the supplied GR model without missing-package, missing-model or symbol-shadowing errors. This is a loading check, not an amplitude calculation. If the model cannot be found, follow the [model-path registration instructions](./Models/FeynArts.md#Common%20loading%20workflow). If FeynCalc was already loaded before setting `$LoadAddOns`, restart the kernel and repeat the loading sequence.

<a name="FeynHelpers and Package-X"></a>

## FeynHelpers and Package-X

Follow the [official FeynHelpers installation guide](https://feyncalc.github.io/FeynHelpersBookDev/Extra/Install.html). The stable installation uses:

```mathematica
Import["https://raw.githubusercontent.com/FeynCalc/feynhelpers/master/install.m"];
InstallFeynHelpers[]
```

For a development FeynCalc installation, follow that guide's development-version instructions instead. FeynHelpers supplies the Package-X `OneLoop.m` component used by its analytic one-loop interface; **no separate Package-X installation is needed for that interface**. Installing FeynHelpers does not require installing all of its other external tools.

Restart the kernel. For example 08, request both add-ons before FeynGrav:

```mathematica
$LoadAddOns = {"FeynArts", "FeynHelpers"};
<< FeynGrav`
```

To check the Package-X interface separately, with unassigned `q` and `m`, evaluate a simple massive tadpole:

```mathematica
probe = PaXEvaluate[FAD[{q, m}], q];
probe
```

A working interface produces an analytic expression rather than leaving `PaXEvaluate` unevaluated or reporting a missing tool. This uses the interface's default integral normalisation; see the [upstream PaXEvaluate examples](https://feyncalc.github.io/FeynHelpersBookDev/PaXEvaluate.html) for explicit prefactors. Passing this setup check does not validate example 08's downstream results; its [recomputation limits](./Examples/README.md#Verification%20and%20saved-output%20status) still apply.

<a name="Contractor"></a>

## Contractor

Contractor is optional and is used only when explicitly selecting `Method -> "Contractor"`. Start from the [Contractor source repository](https://github.com/izokina/contractor). The adapter's recorded compatibility target is version 0.1.0 at revision `641febdaeb6b30835a5f7f846b9a26198ac20313`, as stated in the [method guide](./Services/Calculation/Methods.md). A newer checkout is not automatically covered by that verification.

For that target, the source declares Go 1.25.3 and supplies a Makefile. With a suitable Go toolchain and `make` installed, select the revision in your own Contractor checkout, then run `make build` there. The Makefile enables `GOEXPERIMENT=jsonv2` and builds `bin/contractor`. FeynGrav does not build or download this program. If the repository is unavailable to you, obtain access or a compatible build from its maintainer; the normal FORM workflow remains available without Contractor.

Keep the executable in your chosen location and supply its absolute path. In a fresh kernel:

```mathematica
<< FeynGrav`
contractorPath = "/absolute/path/to/contractor/bin/contractor";
result = FeynGravSimplify[MTD[mu, nu] FVD[p, mu] FVD[q, nu],
  Method -> "Contractor", ContractorExecutable -> contractorPath,
  ContractorThreads -> 1, TimeConstraint -> 30];
If[FailureQ[result], result, result === FCI[SPD[p, q]]]
```

Use unassigned indices and momenta. The last line should return `True`; otherwise inspect the result or failure. This checks the selected executable and one supported contraction. It does not establish full algebra compatibility. `ContractorExecutable -> Automatic` searches the sibling `contractor/bin/contractor` location and then the kernel PATH. An explicit path avoids depending on that layout. See the [restricted input domain and limits](./Services/Calculation/Methods.md) before adapting calculations.
