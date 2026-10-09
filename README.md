# FeynGrav 4.1

FeynGrav is a Wolfram Mathematica package that implements gravitational Feynman rules in the FeynCalc framework. It provides propagators, interaction vertices, polarisation tensors, and tools for working with the Nieuwenhuizen operators.

Supported models include general relativity, minimally coupled scalar, fermion and vector fields, SU(N) Yang–Mills theory, Horndeski interactions, axion-like couplings, and quadratic gravity. The package also includes a massive-gravity propagator and Cheung–Remmen variables for general relativity. The CalcFormConverter module sends supported Lorentz tensors, rank-four epsilon tensors, ordinary Dirac chains and traces, and fundamental SU(N) colour expressions to FORM, then reconstructs the results in FeynCalc notation.

## Contents

- [Documentation index](Documentation/README.md) and [function reference](Documentation/Reference.md)
- [Requirements](#requirements)
- [Installation and help](#installation-and-help)
- [Interaction libraries](#interaction-libraries)
- [CalcFormConverter](#calcformconverter)
- [Examples](#examples)
- [Benchmarks](#benchmarks)
- [Package structure](#package-structure)
- [Troubleshooting and support](#troubleshooting-and-support)
- [Version history](#version-history)
- [Citations and licence](#citations-and-licence)

## Requirements

- **FeynCalc 10.2.1 or newer.** Version 10.2.1 moved legacy functions, including the native `Calc`, out of the core package. FeynGrav does not require `Calc` or FeynCalcLegacy. See the [FeynCalc 10.2.1 release](https://github.com/FeynCalc/feyncalc/releases/tag/Release-10_2_1) and [installation instructions](https://feyncalc.github.io/).
- **Wolfram Mathematica / Wolfram Language 12.2 or newer.** The current branch uses language features introduced in 12.2, including [WithCleanup](https://reference.wolfram.com/language/ref/WithCleanup.html). Use a Wolfram version also supported by your chosen FeynCalc release. Development verification was performed with Wolfram 15.0.1 and FeynCalc 10.2.1; the full range of older Wolfram versions has not been tested.
- **FORM is optional for ordinary FeynGrav use.** Loading the package and exporting or importing FORM files do not require a FORM installation. Executing those files requires FORM; parallel execution with `FORMThreads > 1` requires TFORM. The converter has been tested with FORM/TFORM 4.3. The library generator uses the same converter runtime; compatibility with earlier FORM versions has not been established by the current verification. Obtain executables from the [FORM project](https://github.com/form-dev/form) or your distribution's packages.

## Installation and help

### Automatic installation

**Publication pending:** the public commands below become available after `install.m` is published on `main`. While testing this branch, load its local `install.m` with `Get["/absolute/path/to/FeynGrav/install.m"]` and use a temporary destination. The installer refuses to replace Git working checkouts.

```mathematica
Import["https://raw.githubusercontent.com/BorisNLatosh/FeynGrav/main/install.m"];
InstallFeynGrav[]
```

Importing the script only defines the installer and prints instructions. `InstallFeynGrav[]` downloads GitHub `main`, checks the archive and bundled default libraries, verifies FeynCalc and the staged package in fresh kernels, and installs into `$UserBaseDirectory/Applications/FeynGrav`. No Git, Python, FORM or administrator access is required for this installation. If Mathematica reports `InternetDisabled`, enable `$AllowInternet = True` in the current session or use a local archive with FeynCalc preinstalled; the installer does not override your Internet setting. Internet access to GitHub and permission to launch a fresh Wolfram kernel are required; an inability to launch a kernel or a 180-second verification timeout produces a failure.

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
2. Evaluate `$UserBaseDirectory` in Mathematica. Place the complete `FeynGrav` directory inside its `Applications` subdirectory, so the main file is at `Applications/FeynGrav/FeynGrav.wl`. Retain the `Rules`, `Libs`, and `CalcFormConverter` subdirectories.
3. Load FeynGrav:

   ```mathematica
   << FeynGrav`
   ```

FeynGrav loads FeynCalc if necessary, along with its own rules, default interaction libraries, and CalcFormConverter. Loading does not probe or install FORM or launch a FORM process. Restart the kernel after updating package files.

Use `FeynGravCommands[]` to list available commands. Mathematica's `?FunctionName` syntax displays usage information, including `?GravitonVertex`, `?importGravitons`, and `?CalcFormCalculate`. Calculation workflows are provided in the [examples](#examples).

## Interaction libraries

At initialisation, FeynGrav calls `importGravitons[2]`, `importScalars[2]`, `importFermions[2]`, and `importVectors[2]`. Additional sectors and higher orders must be loaded explicitly.

| Commands | Meaning of the requested order `n` |
| --- | --- |
| `importGravitons[n]`, `importQuadraticGravity[n]` | Load vertex libraries through order `n`; order `n` supplies a vertex with `n + 2` graviton legs. |
| `importScalars[n]`, `importFermions[n]`, `importVectors[n]` | Load matter vertices with up to `n` graviton legs. |
| `importSUNYM[n]`, `importAxionVectorVertex[n]` | Load the corresponding matter interactions with up to `n` graviton legs. |
| `importScalarGaussBonnet[n]` | Load vertices with 2 through `n` graviton legs; the flat-background interaction begins at two. |
| `importHorndeskiG2[]` through `importHorndeskiG5[]` | Load all available libraries for the selected Horndeski sector. |

The order-limited import commands load up to the available order when the requested maximum is higher than the installed libraries. They read local files; they do not download missing libraries. Use their `printOutput -> True` option to report available and imported orders. Consult each command's usage message for its interface.

Use `FeynGravConventions[]` for a readable report of geometry, Fourier, dimension, gauge, polarisation, algebra and diagram conventions. It distinguishes fixed definitions from current settings and shows loaded orders and settings changed since import. The command prints static mathematical output (plain text in a kernel without a front end), returns `Null`, and changes no settings. See the [conventions report](Documentation/Reference.md#conventions-report).

Use `FeynGravLibraryInformation[]` to inspect the last successful imports: source paths and hashes, actual orders, import times and relevant settings. `FeynGravLibraryInformation[importVectors]` selects one importer and identifies settings changed since that import. See [library import records](Documentation/Reference.md#library-import-records) for the scope and limitations.

Additional precomputed libraries are distributed in the [FeynGrav Libraries dataset](https://data.mendeley.com/datasets/9xrw2jjrbr/2). Download the required files and place them directly in `FeynGrav/Libs`, preserving names such as `GravitonVertex_3`, then invoke the appropriate import command. The dataset predates version 4: use compatible interaction libraries and retain the current package's ghost rules, which were revised in version 4. Availability varies by sector; the dataset is not a guarantee that every requested order exists.

The [library generator](Libs/Generator.wl) is a separate developer tool for producing vertex libraries. Most users can work with the supplied or downloaded libraries.

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

The converter performs algebra and Lorentz contractions. It does **not** perform loop integration or integral reduction, or supply symmetry factors, integration measures, or normalisation conventions. Supporting scalar integral notation does not mean that it evaluates those integrals. Checking and calculating never install software implicitly.

See the [user guide](CalcFormConverter/README.md) for workflows, supported expressions, options, installation details, and performance guidance; the [format specification](CalcFormConverter/FORMAT.md) for saved-file compatibility; and the [developer guide](CalcFormConverter/DEVELOPER.md) for architecture, tests, and measurements.

## Examples

The [Examples directory](Examples) contains complete notebooks. Run their setup cells before the calculation cells; these establish libraries, kinematics, and conventions.

| Notebook | Topic |
| --- | --- |
| [Scalars_Gravitational_Scattering_Tree_Level.nb](Examples/Scalars_Gravitational_Scattering_Tree_Level.nb) | Tree-level scalar scattering through gravity. |
| [Graviton_Scattering_Tree_Level.nb](Examples/Graviton_Scattering_Tree_Level.nb) | Tree-level graviton scattering and polarisation contractions; FORM calculation cells require FORM/TFORM. |
| [Nieuwenhuizen_Operators.nb](Examples/Nieuwenhuizen_Operators.nb) | Algebra of the Nieuwenhuizen operators. |
| [Graviton_Self_Energy.nb](Examples/Graviton_Self_Energy.nb) | Graviton contributions to the graviton self-energy. |
| [Graviton_Self_Energy_Matter_Contribution.nb](Examples/Graviton_Self_Energy_Matter_Contribution.nb) | Matter contributions to the graviton self-energy; setup includes `importSUNYM[]`. |
| [Matter_Self_Energy_Graviton_Contribution.nb](Examples/Matter_Self_Energy_Graviton_Contribution.nb) | Gravitational contributions to matter self-energies; setup includes `importSUNYM[]`. |
| [Graviton_Scalar_Vertex_at_First_Loop.nb](Examples/Graviton_Scalar_Vertex_at_First_Loop.nb) | A one-loop graviton–scalar vertex calculation. |

The converter's [ScalarBubble.wl](CalcFormConverter/Examples/ScalarBubble.wl) demonstrates manual and automated FORM workflows for a scalar-projected quadratic-gravity bubble. It requires the cubic quadratic-gravity library, loaded with `importQuadraticGravity[1]`, and FORM/TFORM for execution.

## Benchmarks

The [Benchmark notebooks](Benchmark/README.md) measure export, FORM execution, import, complete CalcFormConverter calls and library generation. Select **Quick** or **Full** before running. Inputs and import fixtures are generated locally; no extra tools beyond Mathematica, the package dependencies, and FORM/TFORM are required. Reports include raw timings, validation status and source hashes.

## Package structure

| Location | Contents |
| --- | --- |
| [FeynGrav.wl](FeynGrav.wl) | Main package, public commands, and library loading. |
| [Conventions.wl](Conventions.wl) | Read-only conventions report and static formatting. |
| [Rules](Rules) | Interaction-rule construction, structural validation and projector utilities. |
| [Libs](Libs) | Precomputed vertex libraries and the [library generator](Libs/Generator.md), which uses CalcFormConverter. |
| [Examples](Examples) | Calculation notebooks. |
| [Documentation](Documentation/README.md) | Main-package reference, documentation routes and verification records. |
| [Benchmark](Benchmark/README.md) | Reproducible converter and library-generation measurements. |
| [CalcFormConverter](CalcFormConverter) | Converter, FORM runtime, templates, documentation, examples, and tests. |

## Troubleshooting and support

- **The package cannot be found:** check that `FeynGrav.wl` is directly inside `$UserBaseDirectory/Applications/FeynGrav`, and that FeynCalc loads in the same kernel.
- **A vertex remains unevaluated or a library cannot be loaded:** check the argument syntax, required sector, and installed library order. Enable `printOutput -> True` on the relevant import command to inspect what is available.
- **FORM works in a terminal but cannot be found in Mathematica:** the kernel may have a different `PATH`. Supply the absolute executable path using `FORMExecutable` when checking or calculating. Multiple workers require a working TFORM executable.
- **A FORM calculation returns `Failure`:** inspect the complete failure, including its nested cause, stage, job directory, and log paths. Failed jobs retain diagnostic files. See the [converter troubleshooting guide](CalcFormConverter/README.md#troubleshooting).
- **Definitions appear inconsistent after an update:** restart the kernel and rerun the notebook's setup cells.

Report problems through [GitHub issues](https://github.com/BorisNLatosh/FeynGrav/issues) or contact Dr Boris Latosh at [latosh.boris@gmail.com](mailto:latosh.boris@gmail.com). Include your operating system, Wolfram and FeynCalc versions, FeynGrav revision, and a minimal reproducible expression. For converter issues, include the FORM/TFORM version, options, full failure details, and relevant logs.

## Version history

### Version 4.1 — unreleased

Planned release tag: `v4.1.0`.

- Added a two-command installer with dependency checks, package verification, and backups when replacing an existing installation.
- Added CalcFormConverter for exchanging supported FeynCalc expressions with FORM, including Lorentz contractions, rank-four epsilon tensors, ordinary Dirac algebra, and fundamental SU(N) colour algebra. Automated FORM/TFORM execution includes timing and progress reporting.
- Added D-dimensional polarisation vectors and factorised polarisation tensors in four and D dimensions.
- Migrated interaction-library generation to CalcFormConverter and added reproducible benchmarks for conversion and library generation.
- Improved rule-input validation and library loading, preserving existing definitions when an import fails.
- Added `FeynGravConventions[]` and `FeynGravLibraryInformation[]` to inspect mathematical conventions, current settings, and the provenance of loaded libraries.
- Expanded the function reference and updated calculation examples for the FORM workflow.

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

## Citations and licence

When using FeynGrav in published work, cite the publications relevant to the version and features used:

- Boris Latosh, [FeynGrav](https://doi.org/10.1088/1361-6382/ac7e15), *Classical and Quantum Gravity* **39** (2022), 165006.
- Boris Latosh, [FeynGrav 2.0](https://doi.org/10.1016/j.cpc.2023.108871), *Computer Physics Communications* **292** (2023), 108871.
- Boris Latosh, [FeynGrav 3.0](https://doi.org/10.1016/j.cpc.2025.109508), *Computer Physics Communications* **310** (2025), 109508.
- Boris Latosh, [FeynGrav 4.0](https://arxiv.org/abs/2510.17320), arXiv:2510.17320.

Follow the citation guidance of FeynCalc and FORM when using those packages as well.

FeynGrav is distributed under the GNU General Public Licence version 3; see [Licence](LICENSE). Separately downloaded datasets carry the licence stated on their distribution page.
