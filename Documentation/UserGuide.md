# FeynGrav 4.2.0

[Directory overview](./README.md) · [Getting started](./GettingStarted.md) · [Common conventions](./Conventions.md)

**Development version; 4.2.0 is not yet released.** See the
[4.2.0 draft release notes](./ReleaseNotes-4.2.0.md). Installation from `main`
continues to select that branch, not this local development checkout.

FeynGrav is a Wolfram Mathematica package that implements gravitational Feynman rules in the FeynCalc framework. It provides propagators, interaction vertices, polarisation tensors, and tools for working with the Nieuwenhuizen operators.

Supported models include general relativity, minimally coupled scalar, fermion and vector fields, SU(N) Yang–Mills theory, Horndeski interactions, axion-like couplings, and quadratic gravity. The package also includes a massive-gravity propagator and Cheung–Remmen variables for general relativity. The CalcFormConverter module sends supported Lorentz tensors, rank-four epsilon tensors, ordinary Dirac chains and traces, and fundamental SU(N) colour expressions to FORM, then reconstructs the results in FeynCalc notation.

## Contents

- [Documentation index](./README.md) and [function reference](./Reference.md)
- [Requirements](#Requirements)
- [Installation and help](#Installation%20and%20help)
- [Optional dependency setup](./OptionalDependencies.md)
- [Interaction libraries](#Interaction%20libraries)
- [CalcFormConverter](#CalcFormConverter)
- [Examples](#Examples)
- [Benchmarks](#Benchmarks)
- [Package structure](#Package%20structure)
- [Troubleshooting and support](#Troubleshooting%20and%20support)
- [Version history](#Version%20history)
- [Citations and licence](#Citations%20and%20licence)

<a name="Requirements"></a>

## Requirements

- **FeynCalc 10.2.1 or newer.** Version 10.2.1 moved legacy functions, including the native `Calc`, out of the core package. FeynGrav does not require `Calc` or FeynCalcLegacy. See the [FeynCalc 10.2.1 release](https://github.com/FeynCalc/feyncalc/releases/tag/Release-10_2_1) and [installation instructions](https://feyncalc.github.io/).
- **Wolfram Mathematica / Wolfram Language 12.2 or newer.** The current branch uses language features introduced in 12.2, including [WithCleanup](https://reference.wolfram.com/language/ref/WithCleanup.html). Use a Wolfram version also supported by your chosen FeynCalc release. Development verification was performed with Wolfram 15.0.1 and FeynCalc 10.2.1; the full range of older Wolfram versions has not been tested.
- **FORM is optional for ordinary FeynGrav use.** Loading the package and exporting or importing FORM files do not require a FORM installation. Executing those files requires FORM; parallel execution with `FORMThreads > 1` requires TFORM. The converter has been tested with FORM/TFORM 4.3. The library generator uses the same converter runtime; compatibility with earlier FORM versions has not been established by the current verification. Obtain executables from the [FORM project](https://github.com/form-dev/form) or your distribution's packages.

For workflows using FeynArts, FeynHelpers/Package-X, FORM/TFORM or Contractor, follow [optional dependency setup and checks](./OptionalDependencies.md). Install only the tools required by your chosen workflow.

<a name="Installation and help"></a>

## Installation and help

### Automatic installation

The installer is available on public `main`. The commands below install the current contents of that branch, which may include development changes beyond the latest tagged release; they do not automatically select the latest GitHub Release. To test a local checkout, load its `install.m` with `Get["/absolute/path/to/FeynGrav/install.m"]` and select a local archive with `FeynGravArchive` and a temporary destination with `InstallFeynGravTo`; loading a local installer alone does not select the local package. The installer refuses to replace Git working checkouts.

```mathematica
Import["https://raw.githubusercontent.com/BorisNLatosh/FeynGrav/main/install.m"];
InstallFeynGrav[]
```

Importing the script only defines the installer and prints instructions. `InstallFeynGrav[]` downloads GitHub `main`, checks the archive and bundled default libraries, verifies FeynCalc and the staged package in fresh kernels, and installs into `$UserBaseDirectory/Applications/FeynGrav`. No Git, Python, FORM or administrator access is required for this installation. If Mathematica reports `InternetDisabled`, enable `$AllowInternet = True` in the current session or use a local archive with FeynCalc preinstalled; the installer does not override your Internet setting. Internet access to GitHub and permission to launch a fresh Wolfram kernel are required; an inability to launch a kernel or a 180-second verification timeout produces a failure.

<details>
<summary>Optional: install a specific published version</summary>

The default remains installation from `main`; ordinary installation does not require choosing a tag.

To select a particular published release, supply its tag; for example:

```mathematica
InstallFeynGrav[FeynGravReference -> "v4.1.0"]
```

This selects the package archive at that tag; the preceding `Import` still loads the installer from `main`. For a fully pinned workflow, load `install.m` from the same tag by replacing `main` in its URL with `v4.1.0`. The planned `v4.2.0` tag is not available from this version update; use it only after publication. A commit hash may also be supplied through `FeynGravReference`.

</details>

A compatible FeynCalc installation is retained. If FeynCalc is missing or older than 10.2.1, the installer asks before invoking the [official FeynCalc installer](https://feyncalc.github.io/), whose own prompts remain enabled. Its changes are separate from FeynGrav installation and are not rolled back by this installer. FORM/TFORM and additional library datasets are not installed. The FeynGrav installer does not change front-end preferences; the official FeynCalc installer may offer its own preference settings.

When FeynGrav already exists, replacement is requested only after the new package passes verification. The complete old directory is retained beside it under a unique `FeynGrav.backup-...` name. Additional libraries, edited notebooks and configuration remain in that backup; they are not merged into the new installation automatically. If the replacement move fails, the installer attempts to restore the old directory and reports recovery paths if restoration fails.

After installation, **restart the kernel**, then load the package with `` << FeynGrav` ``. A successful call returns an association with `InstalledPath`, `Reference`, `Source`, `Verification` and `BackupPath`; failures return a `Failure` with an explanation. Incomplete archives report `MissingFiles`; failed fresh-kernel checks include stdout/stderr and exit status under `Verification["Diagnostics"]` (or directly in a probe failure). These diagnostics remain available after temporary files are cleaned up. `Reference` is `Null` for a local archive, whose provenance the installer cannot infer. A custom destination does not automatically change `$Path`; load its `FeynGrav.wl` by absolute path, or add its parent directory to `$Path` yourself.

| Option | Default | Behaviour |
| --- | --- | --- |
| `InstallFeynGravTo` | `Automatic` | User `Applications/FeynGrav` directory; otherwise an absolute package-directory path. Git checkouts and destinations inside them are refused. |
| `FeynGravReference` | `"main"` | Official GitHub branch, tag or commit. Pin a commit for reproducible installation. |
| `FeynGravArchive` | `Automatic` | Download from GitHub, or use a local ZIP containing a single package root. A local archive takes precedence over the reference. Use only trusted archives: verification loads their Wolfram code. |
| `OverwriteFeynGrav` | `Automatic` | Ask before replacing an existing installation. `True` consents with a retained backup; `False` refuses. |
| `InstallFeynCalcDependency` | `Automatic` | Ask before invoking FeynCalc's installer when needed. `True` consents; `False` requires compatible FeynCalc to be installed already. |

Notebook sessions display consent dialogs. Without a notebook front end, a required `Automatic` consent returns `Failure["ConsentRequired", ...]`; set the relevant option explicitly. Consenting to FeynCalc installation does not suppress its own prompts, so unattended use should preinstall FeynCalc and set `InstallFeynCalcDependency -> False`.

For example, test a downloaded archive without touching the ordinary installation:

```mathematica
InstallFeynGrav[
  FeynGravArchive -> "/absolute/path/to/FeynGrav.zip",
  InstallFeynGravTo -> FileNameJoin[{$TemporaryDirectory, "FeynGrav-install-test"}],
  InstallFeynCalcDependency -> False,
  OverwriteFeynGrav -> False
]
```

The installer accepts ordinary ZIP archives; encrypted, split, ZIP64 and link-containing archives are rejected. Interrupted or failed operations may report retained temporary paths for manual inspection. Successful backups are never deleted automatically.

### Manual installation and help

1. Install FeynCalc and verify that it loads in a fresh kernel.
2. Evaluate `$UserBaseDirectory` in Mathematica. Place the complete `FeynGrav` directory inside its `Applications` subdirectory, so the main file is at `Applications/FeynGrav/FeynGrav.wl`. Retain all package subdirectories, including `Rules`, `Libs`, `CalcFormConverter` and `FeynGravSimplify`.
3. Load FeynGrav:

   ```mathematica
   << FeynGrav`
   ```

FeynGrav loads FeynCalc if necessary, along with its own rules, default interaction libraries, and CalcFormConverter. Loading does not probe or install FORM or launch a FORM process. Restart the kernel after updating package files.

Use `FeynGravCommands[]` to list available commands. Mathematica's `?FunctionName` syntax displays usage information, including `?GravitonVertex`, `?FeynGravImportGravitons`, and `?CalcFormCalculate`. Calculation workflows are provided in the [examples](#Examples).

<a name="Interaction libraries"></a>

## Interaction libraries

At initialisation, FeynGrav calls `FeynGravImportGravitons[2]`, `FeynGravImportScalars[2]`, `FeynGravImportFermions[2]`, and `FeynGravImportVectors[2]`. Additional sectors and higher orders must be loaded explicitly.

| Commands | Meaning of the requested order `n` |
| --- | --- |
| `FeynGravImportGravitons[n]`, `FeynGravImportQuadraticGravity[n]` | Load vertex libraries through order `n`; order `n` supplies a vertex with `n + 2` graviton legs. |
| `FeynGravImportScalars[n]`, `FeynGravImportFermions[n]`, `FeynGravImportVectors[n]` | Load matter vertices with up to `n` graviton legs. |
| `FeynGravImportSUNYM[n]`, `FeynGravImportAxionVectorVertex[n]` | Load the corresponding matter interactions with up to `n` graviton legs. |
| `FeynGravImportScalarGaussBonnet[n]` | Load vertices with 2 through `n` graviton legs; the flat-background interaction begins at two. |
| `FeynGravImportHorndeskiG2[]` through `FeynGravImportHorndeskiG5[]` | Load all available libraries for the selected Horndeski sector. |

The order-limited import commands load up to the available order when the requested maximum is higher than the installed libraries. They read local files; they do not download missing libraries. Imports are silent by default (`Verbose -> False`). Use `Verbose -> True` to report available and imported orders. Consult each command's usage message for its interface.

Use `FeynGravConventions[]` for a readable report of geometry, Fourier, dimension, gauge, polarisation, algebra and diagram conventions. It distinguishes fixed definitions from current settings and shows loaded orders and settings changed since import. The command prints static mathematical output (plain text in a kernel without a front end), returns `Null`, and changes no settings. See the [conventions report](./Reference.md#Conventions%20report).

Use `FeynGravLibraryInformation[]` to inspect the last successful imports: source paths and hashes, actual orders, import times and relevant settings. `FeynGravLibraryInformation[FeynGravImportVectors]` selects one importer and identifies settings changed since that import. See [library import records](./Reference.md#Library%20import%20records) for the scope and limitations.

Older precomputed rules are available in the [FeynGrav Libraries dataset, version 2, published 14 May 2024](https://data.mendeley.com/datasets/9xrw2jjrbr/2). This dataset predates version 4 and is historical material, not a compatibility-certified extension of this checkout. **Keep the libraries shipped with your package version; do not overwrite them with files from the dataset or an older installation.** A matching filename or successful import does not establish correct action conventions, coupling powers or normalisation.

Later releases corrected matter-vertex multiplicities and mixed Yang–Mills coupling powers, among other scientific changes. Older package installations may also contain superseded Horndeski and Gauss–Bonnet rules. Retaining the current ghost rules alone is therefore insufficient. Consult the [4.1.0 scientific corrections](./ReleaseNotes-4.1.0.md), [4.2.0 draft release notes](./ReleaseNotes-4.2.0.md) and [current scientific scope](./ScientificScope.md) before reusing older files. A universal rescaling cannot repair all affected rules or saved amplitudes.

If an additional order is needed:

1. Extract candidate files into a separate directory and retain their dataset/package version and file identities. Do not copy an entire older library collection into `Libs`.
2. Establish compatibility for the exact candidate against the current action, conventions and documented corrections, using an existing independent verification for that file or a new mathematical comparison. This manual supplies no per-file compatibility approval list for the historical dataset. A file without that evidence remains unverified. Alternatively, use the [current library generator](./Services/LibraryGeneration/Guide.md) with a separate `OutputDirectory`; successful generation alone does not independently validate a rule, and the documented source bounds and unresolved sectors still apply.
3. Test validated additions in a separate package copy and a fresh kernel before adopting them. Add only the required missing files to that copy's `Libs`, preserving canonical filenames and the shipped baseline. Use the relevant importer with `Verbose -> True`, then inspect `FeynGravLibraryInformation[]` for the actual loaded orders, source paths and hashes. These records identify what was loaded; they do not certify its physics.

Most users can start with the shipped libraries. Additional stored orders are not guaranteed to exist for every sector. Restart the kernel and recompute affected expressions after changing a library set; previously saved results do not update themselves.

## FeynArts models

`FeynArts/Models` contains one folder per model, each with matching `.gen`, `.mod`, `manifest.json` and `README.md` files. Loading FeynArts together with FeynGrav registers these model folders on FeynArts' `$ModelPath`, so the supplied models are available by name:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;

InitializeModel["GR", GenericModel -> "GR"];
```

`GenericModel` is required because the classes file does not name its generic model and the FeynArts default is the stock `"Lorentz"` file. The snapshot covers the cubic and quartic graviton interactions and the oriented ghost coupling at gauge parameter 2 with symbolic `D`. The ordinary ghost line is `-I MTD[mu,nu] FAD[p]`. This release is ready for three-/four-graviton trees; loop compatibility checks are deferred and the optional bridge deliberately rejects the new snapshot's loop requests. Start a fresh kernel after updating the model. See the [GR guide](./Models/GeneralRelativity/NativeModel.md) for details and hashes, and `Examples/04. Graviton_Scattering_Tree_Level_FeynArts.nb` for a worked off-shell four-graviton tree calculation.

A separate [Single-Scalar-Tensor-Minimal-Coupling model](./Models/Scalar/NativeModel.md) adds one real minimally coupled scalar of arbitrary symbolic mass, including `TSS` and `TTSS` through $\kappa^2$. It is supplied ready for the tested native tree workflows; no rebuild is needed. Use `InitializeModel["Single-Scalar-Tensor-Minimal-Coupling", GenericModel -> "Single-Scalar-Tensor-Minimal-Coupling"]` with the same loading workflow; its guide also shows explicit path registration without loading FeynGrav. The optional pure-GR bridge is unchanged; independent positive cubic and quartic scalar self-interactions are included. Setting both couplings to zero gives the minimal massive scalar sector. Non-minimal couplings and loop validation are outside this model.

The [GR + SU(N) Yang–Mills model](./Models/YangMills/NativeModel.md)
uses `"GR-SU-N-Yang-Mills"` for both model names. It includes all 21 families
through κ² per vertex, with symbolic D/N, `YangMillsCoupling`, both ghost species
and five/six-leg contacts. It includes one massive fundamental quark flavour (`QuarkMass`), with `F[1]`
for quarks, `S[1]` for vector GR ghosts and `U[1]` for YM ghosts. Selected native
tree Ward checks passed; native loop validation remains deferred.

<a name="CalcFormConverter"></a>

## CalcFormConverter

CalcFormConverter loads automatically with FeynGrav and can also be loaded independently. It supports exact Lorentz tensors and polarisation vectors, rank-four single-space epsilon tensors, ordinary Dirac chains and traces, fundamental SU(N) colour tensors and traces, ordinary quadratic propagators, and scalar `A0` through `D0` notation. Supported Dirac and colour processing are enabled by default. External spinors, gamma-five, mixed Lorentz spaces and general Lie groups remain outside this interface.

| Command | Purpose |
| --- | --- |
| `CalcFormExport[expr, file, options]` | Write a FORM program and its JSON mapping; return the input, mapping, and expected result paths. |
| `CalcFormImport[resultFile, mappingFile]` | Reconstruct the dedicated FORM result in FeynCalc internal notation. |
| `CalcFormCheck[options]` | Locate the selected FORM/TFORM executable and verify it with a small calculation. |
| `CalcFormInstall[options]` | Check availability and explicitly attempt installation on supported Debian/Ubuntu systems. |
| `CalcFormCalculate[expr, options]` | Export, execute FORM/TFORM, import, and return the FeynCalc expression. |

`FORMThreads -> Automatic` is the default for checking, installation, and calculation: it prefers up to eight TFORM workers, capped by the processor count, and falls back to serial FORM when TFORM is missing. Explicit worker counts do not fall back; an explicit executable with automatic threads uses one worker. `ShowTiming` reports FORM execution wall time; `ShowProgress` reports stages and elapsed execution time. Use `AbsoluteTiming` when measuring the entire Mathematica call. More workers do not guarantee a faster calculation.

The converter performs algebra and Lorentz contractions. It automatically groups complete propagator products, applies guarded numerator cancellation, and simplifies eligible scalar coefficients in FORM. Prepared tensor stages and incremental, factor-preserving import reduce intermediate work; these are automatic implementation choices, not new public options. With `LoopMomenta -> {q}`, default `TensorDecomposition -> True` expresses one-loop integrals through tensor coefficient functions using FeynCalc's `I Pi^2` conversion normalisation. Empty loop metadata or `TensorDecomposition -> False` retains integrand algebra. `DropScaleless -> False` preserves formal scaleless terms. Optional `ReduceLoopCoefficients -> True` lowers repeated propagator powers in FORM and can also process previously computed coefficients without declaring a new integration variable. Singular and unsupported families remain formal. The default `ReduceToScalarMasters -> True` further reduces supported coefficients through boxes to scalar PaVe masters, including necessary repeated-power reduction. Set it to False to stop at coefficient functions. Exceptional and preserved scaleless cases may remain formal, with a partial-reduction diagnostic. The converter does not analytically evaluate masters or add diagram symmetry factors, integration-measure factors or renormalisation prescriptions. Checking and calculating never install software implicitly.

See the [user guide](./Services/Calculation/Guide.md) for workflows, supported expressions, options, installation details, and performance guidance; the [format specification](./Services/Calculation/SavedFiles.md) for saved-file compatibility.

### Simplified front end (development, unreleased)

The five-command facade planned for 4.2.0 is described in the converter guide,
[4.2.0 draft release notes](./ReleaseNotes-4.2.0.md) and
[Calculation interface](./Services/Calculation/FeynGravSimplify.md): ``CalcFormConverter`FeynGravSimplify[expr, opts]``
selects the full pipeline (`Method -> Automatic` or `"FORM"`) or the restricted
external adapter (`"Contractor"`, `"FORMBenchmark"`) without fallback, and
`FeynGravFORMExport`, `FeynGravFORMImport`, `FeynGravFORMCheck` and
`FeynGravFORMInstall` wrap the corresponding `CalcForm*` commands. Restricted
methods accept only the fixed capability baseline. These commands are additions;
`CalcFormCalculate` and all `CalcForm*` commands above remain the published
interface and are unchanged.

`FeynGravSimplify` defaults to `PaVeToABCD -> True`: supported final coefficients
become named integral functions such as `A0`, `B0`, `C0` and `D0`. Use `False`
to preserve the prior PaVe output. Unsupported coefficients remain. This option
is independent of tensor/master reduction and is a Boolean no-op on restricted
methods. See the [full contract](./Services/Calculation/Guide.md#Final%20PaVe%20conversion%20%28%60FeynGravSimplify%60%20only%29).

<a name="Examples"></a>

## Examples

The [examples guide](./Examples/README.md) lists the numbered notebooks in their suggested reading order, with requirements and scope. Run each notebook in a fresh kernel and evaluate its setup cells before its calculation cells.

Examples 07, 09 and 11 have repaired input cells. Follow the [rerunning guidance](./Examples/README.md#Syntax%20and%20interface%20audit) and [saved-output limitations](./ScientificScope.md#Examples%20and%20saved%20outputs). Saved results do not establish a fresh complete loop calculation.

Use the [complete notebook catalogue](./Examples/README.md) for all fourteen examples, including massive quark graviton emission in example 13 and four-graviton scattering in quadratic gravity in example 14. That page maintains the reading order, dependency list and [per-notebook verification and saved-output status](./Examples/README.md#Verification%20and%20saved-output%20status).

The selected scalar-model checks are described in [Scientific scope](./ScientificScope.md#Scalar%20models). For examples 12 and 13, see the [Yang–Mills and fermion scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions). These model checks and saved notebook results have distinct verification limits.

The converter's `CalcFormConverter/Examples/ScalarBubble.wl` demonstrates manual and automated FORM workflows for a scalar-projected quadratic-gravity bubble. It requires the cubic quadratic-gravity library, loaded with `FeynGravImportQuadraticGravity[1]`, and FORM/TFORM for execution.

<a name="Benchmarks"></a>

## Benchmarks

The [Benchmark notebooks](./Benchmarks/README.md) measure export, FORM execution, import, complete CalcFormConverter calls and library generation. Select **Quick** or **Full** before running. Inputs and import fixtures are generated locally; no extra tools beyond Mathematica, the package dependencies, and FORM/TFORM are required. Reports include raw timings, validation status and source hashes.

<a name="Package structure"></a>

## Package structure

| Location | Contents |
| --- | --- |
| `FeynGrav.wl` | Main package, public commands, and library loading. |
| `Conventions.wl` | Read-only conventions report and static formatting. |
| `Rules` | Interaction-rule construction, structural validation and projector utilities. |
| `Libs` | Precomputed vertex libraries and the [library generator](./Services/LibraryGeneration/Guide.md), which uses CalcFormConverter. |
| `Examples` | Calculation notebooks. |
| [FeynArts/Models](./Models/FeynArts.md) | Six native gravity, scalar and Yang–Mills model bundles and tree-use guides. |
| [Documentation](./README.md) | Model descriptions, command reference and usage guides. |
| [Benchmark](./Benchmarks/README.md) | Reproducible converter and library-generation measurements. |
| `CalcFormConverter` | Converter, FORM runtime, templates, documentation, examples, and tests. |

<a name="Troubleshooting and support"></a>

## Troubleshooting and support

- **The package cannot be found:** check that `FeynGrav.wl` is directly inside `$UserBaseDirectory/Applications/FeynGrav`, and that FeynCalc loads in the same kernel.
- **A vertex remains unevaluated or a library cannot be loaded:** check the argument syntax, required sector, and installed library order. Use `Verbose -> True` on the relevant import command to inspect what is available.
- **FORM works in a terminal but cannot be found in Mathematica:** the kernel may have a different `PATH`. Supply the absolute executable path using `FORMExecutable` when checking or calculating. Multiple workers require a working TFORM executable.
- **A FORM calculation returns `Failure`:** inspect the complete failure, including its nested cause, stage, job directory, and log paths. Failed jobs retain diagnostic files. See the [converter troubleshooting guide](./Services/Calculation/Guide.md#Troubleshooting).
- **Definitions appear inconsistent after an update:** restart the kernel and rerun the notebook's setup cells.

Report problems through [GitHub issues](https://github.com/BorisNLatosh/FeynGrav/issues) or contact Dr Boris Latosh at [latosh.boris@gmail.com](mailto:latosh.boris@gmail.com). Include your operating system, Wolfram and FeynCalc versions, FeynGrav revision, and a minimal reproducible expression. For converter issues, include the FORM/TFORM version, options, full failure details, and relevant logs.

<a name="Version history"></a>

## Version history

### Version 4.2.0 — unreleased

The next release is a minor version because it adds compatible public commands
and calculation capabilities. It introduces `FeynGravSimplify` and four
`FeynGravFORM*` wrappers, one-loop coefficient/master reduction, shared output
and bounded coefficient simplification, plus reliability and example repairs.
The existing `CalcForm*` commands remain available. See the
[4.2.0 draft release notes](./ReleaseNotes-4.2.0.md) for verification and migration
limits. No release date or tag is available yet.

<a id="version-410--unreleased"></a>

### Version 4.1.0 — 20 October 2026

Release tag: `v4.1.0`. See the [4.1.0 release notes](./ReleaseNotes-4.1.0.md) for migration instructions, scientific corrections and known limitations.

From this release onward, versions follow MAJOR.MINOR.PATCH: incompatible public-interface changes increment MAJOR, compatible additions MINOR, and compatible corrections PATCH. Version 4.1.0 is the explicitly chosen transition release and includes the incompatible changes listed below.

- Added a two-command installer with dependency checks, package verification, and backups when replacing an existing installation.
- Added CalcFormConverter for exchanging supported FeynCalc expressions with FORM, including Lorentz contractions, rank-four epsilon tensors, ordinary Dirac algebra, and fundamental SU(N) colour algebra. Automated FORM/TFORM execution includes timing and progress reporting.
- Added automatic propagator grouping, guarded numerator cancellation, coefficient simplification, verified tensor-stage symmetries and incremental result import. See the [current converter guide](./Services/Calculation/Guide.md) for scope and memory limits.
- Added D-dimensional polarisation vectors and factorised polarisation tensors in four and D dimensions.
- Migrated interaction-library generation to CalcFormConverter and added reproducible benchmarks for conversion and library generation.
- Improved rule-input validation and library loading, preserving existing definitions when an import fails.
- Added `FeynGravConventions[]` and `FeynGravLibraryInformation[]` to inspect mathematical conventions, current settings, and the provenance of loaded libraries.
- Expanded the function reference and numbered the calculation examples, including native scalar, John and EsGB scattering.
- Added four standalone native FeynArts models through κ² per vertex: GR, a massive scalar with cubic/quartic potential, John and Einstein–scalar–Gauss–Bonnet. Loop validation is deferred.
- Corrected the ordinary gravitational ghost sign, scalar potential/EsGB source normalisations and missing graviton differentiation multiplicities; rebuilt affected libraries and scalar models. See the release notes for scope and evidence.
- Renamed public library importers to `FeynGravImport*` and their diagnostic option to `Verbose`, defaulting to `False`; old names are not aliases.
- Added `FeynGravHowToCite[]` and refreshed the startup message.

**Compatibility:** requires FeynCalc 10.2.1 or newer and Wolfram Language 12.2 or newer, subject to the requirements of the selected FeynCalc version. The local `Calc` implementation has been removed, and the library generator is now loaded from `Libs/Generator.wl`. FORM remains optional for loading FeynGrav; it is required for converter execution and library generation.

### Version 4.0

- Revised the BRST treatment of general relativity and quadratic gravity to use a finite set of graviton–Faddeev–Popov ghost interaction rules.
- Added higher-derivative gauge fixing for quadratic gravity, with the corresponding graviton and ghost propagators and ghost–graviton interaction.
- Implemented the pure-gravity Cheung–Remmen formulation, including its auxiliary field, propagators, and finite set of interaction vertices.
- Added commands to construct, invert, and decompose linear combinations of Nieuwenhuizen operators, and updated command descriptions. See [FeynGrav 4.0](https://arxiv.org/html/2510.17320v3#S4).

### Version 3.0

- Added Horndeski interactions, scalar–Gauss–Bonnet interactions, axion-like coupling to a vector field, and quadratic gravity.
- Added a massive-graviton propagator.
- Introduced recursive tensor-construction algorithms, memoisation, and FORM-assisted interaction-library generation.
- Added selective loading of interaction libraries and standardised propagator interfaces around FeynCalc’s `FAD` notation. See [FeynGrav 3.0](https://arxiv.org/html/2406.14872#S7).

### Version 2.0

- Extended minimally coupled scalar, Dirac, and vector fields to arbitrary masses.
- Added gravitational interactions for SU(N) Yang–Mills theory, including the associated ghost sector.
- Extended gravitational gauge fixing to an arbitrary gauge parameter and included the corresponding ghost interactions. See [FeynGrav 2.0](https://arxiv.org/abs/2302.14310).

### Version 1.0

- Introduced FeynGrav as a FeynCalc extension for gravitational Feynman rules.
- Supported general relativity and minimally coupled massless fields of spin 0, 1/2, and 1.
- Provided examples of tree-level graviton scattering, one-loop self-energies, and scalar–gravity interactions. See [the original FeynGrav publication](https://arxiv.org/abs/2201.06812).

<a name="Citations and licence"></a>

## Citations and licence

Evaluate `FeynGravHowToCite[]` to print the reference list and links in your session. Like `FeynCalcHowToCite[]`, it prints guidance and returns `Null`; it does not fetch information online.

When using FeynGrav in published work, cite the publications relevant to the version and features used:

- Boris Latosh, [FeynGrav](https://doi.org/10.1088/1361-6382/ac7e15), *Classical and Quantum Gravity* **39** (2022), 165006.
- Boris Latosh, [FeynGrav 2.0](https://doi.org/10.1016/j.cpc.2023.108871), *Computer Physics Communications* **292** (2023), 108871.
- Boris Latosh, [FeynGrav 3.0](https://doi.org/10.1016/j.cpc.2025.109508), *Computer Physics Communications* **310** (2025), 109508.
- Boris Latosh, [FeynGrav 4.0](https://arxiv.org/abs/2510.17320), arXiv:2510.17320.

Follow the citation guidance of FeynCalc and FORM when using those packages as well.

FeynGrav is distributed under the GNU General Public Licence version 3; see Licence (`LICENSE`). Separately downloaded datasets carry the licence stated on their distribution page.

## Native scalar extensions — 18 October 2026

`Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet` are supplied alongside `GR`, with matching generic/classes files in individual model folders. Use a fresh kernel and the same name for `Model` and `GenericModel`. The optional pure-GR bridge remains restricted to its existing processes. See [model index](./Models/FeynArts.md) for actions, native examples and verification limits. The two-graviton EsGB source and library have been corrected against an independent curvature derivation; higher-order EsGB sources require a separate audit. Vertex order is limited to kappa squared; potential contacts still require up to six legs. Loops are not validated.
