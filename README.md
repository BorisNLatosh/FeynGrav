# FeynGrav

FeynGrav is a Wolfram Mathematica package that implements gravitational Feynman rules in the FeynCalc framework. It provides propagators, interaction vertices, polarization tensors, and tools for working with the Nieuwenhuizen operators.

Supported models include general relativity, minimally coupled scalar, fermion and vector fields, SU(N) Yang–Mills theory, Horndeski interactions, axion-like couplings, and quadratic gravity. The package also includes a massive-gravity propagator and Cheung–Remmen variables for general relativity. The CalcFormConverter module sends supported bosonic tensor expressions to FORM for algebra and Lorentz contractions, then reconstructs the results in FeynCalc notation.

## Contents

- [Requirements](#requirements)
- [Installation and help](#installation-and-help)
- [Interaction libraries](#interaction-libraries)
- [CalcFormConverter](#calcformconverter)
- [Examples](#examples)
- [Benchmarks](#benchmarks)
- [Package structure](#package-structure)
- [Troubleshooting and support](#troubleshooting-and-support)
- [Version history](#version-history)
- [Citations and license](#citations-and-license)

## Requirements

- **FeynCalc 10.2.1 or newer.** Version 10.2.1 moved legacy functions, including the native `Calc`, out of the core package. FeynGrav supplies its own implementation in [Rules/Calc.wl](Rules/Calc.wl); FeynCalcLegacy is not required. See the [FeynCalc 10.2.1 release](https://github.com/FeynCalc/feyncalc/releases/tag/Release-10_2_1) and [installation instructions](https://feyncalc.github.io/).
- **Wolfram Mathematica / Wolfram Language 12.2 or newer.** The current branch uses language features introduced in 12.2, including [WithCleanup](https://reference.wolfram.com/language/ref/WithCleanup.html). Use a Wolfram version also supported by your chosen FeynCalc release. Development verification was performed with Wolfram 15.0.1 and FeynCalc 10.2.1; the full range of older Wolfram versions has not been tested.
- **FORM is optional for ordinary FeynGrav use.** Loading the package and exporting or importing FORM files do not require a FORM installation. Executing those files requires FORM; parallel execution with `FORMThreads > 1` requires TFORM. The converter has been tested with FORM/TFORM 4.3. The separate library generator requires FORM 4.2 or newer. Obtain executables from the [FORM project](https://github.com/form-dev/form) or your distribution's packages.

## Installation and help

1. Install FeynCalc and verify that it loads in a fresh kernel.
2. Evaluate `$UserBaseDirectory` in Mathematica. Place the complete `FeynGrav` directory inside its `Applications` subdirectory, so the main file is at `Applications/FeynGrav/FeynGrav.wl`. Retain the `Rules`, `Libs`, and `CalcFormConverter` subdirectories.
3. Load FeynGrav:

   ```mathematica
   << FeynGrav`
   ```

FeynGrav loads FeynCalc if necessary, along with its own rules, default interaction libraries, and CalcFormConverter. Loading does not probe or install FORM or launch a FORM process. Restart the kernel after updating package files.

Use `FeynGravCommands[]` to list available commands. Mathematica's `?FunctionName` syntax displays usage information, including `?GravitonVertex`, `?importGravitons`, and `?CalcFormCalculate`. Calculation workflows are provided in the [examples](#examples).

## Interaction libraries

At initialization, FeynGrav calls `importGravitons[2]`, `importScalars[2]`, `importFermions[2]`, and `importVectors[2]`. Additional sectors and higher orders must be loaded explicitly.

| Commands | Meaning of the requested order `n` |
| --- | --- |
| `importGravitons[n]`, `importQuadraticGravity[n]` | Load vertex libraries through order `n`; order `n` supplies a vertex with `n + 2` graviton legs. |
| `importScalars[n]`, `importFermions[n]`, `importVectors[n]` | Load matter vertices with up to `n` graviton legs. |
| `importSUNYM[n]`, `importAxionVectorVertex[n]` | Load the corresponding matter interactions with up to `n` graviton legs. |
| `importHorndeskiG2[]` through `importHorndeskiG5[]` | Load all available libraries for the selected Horndeski sector. |

The order-limited import commands load up to the available order when the requested maximum is higher than the installed libraries. They read local files; they do not download missing libraries. Use their `printOutput -> True` option to report available and imported orders. Consult each command's usage message for its interface.

Additional precomputed libraries are distributed in the [FeynGrav Libraries dataset](https://data.mendeley.com/datasets/9xrw2jjrbr/2). Download the required files and place them directly in `FeynGrav/Libs`, preserving names such as `GravitonVertex_3`, then invoke the appropriate import command. The dataset predates version 4: use compatible interaction libraries and retain the current package's ghost rules, which were revised in version 4. Availability varies by sector; the dataset is not a guarantee that every requested order exists.

The [library generator](Libs/FeynGravLibrariesGenerator.wl) is a separate developer tool for producing vertex libraries. Most users can work with the supplied or downloaded libraries.

## CalcFormConverter

CalcFormConverter loads automatically with FeynGrav and can also be loaded independently. It supports exact bosonic tensor algebra, supported polarization vectors, ordinary quadratic propagators, and scalar `A0` through `D0` functions.

| Command | Purpose |
| --- | --- |
| `CalcFormExport[expr, file, options]` | Write a FORM program and its JSON mapping; return the input, mapping, and expected result paths. |
| `CalcFormImport[resultFile, mappingFile]` | Reconstruct the dedicated FORM result in FeynCalc internal notation. |
| `CalcFormCheck[options]` | Locate the selected FORM/TFORM executable and verify it with a small calculation. |
| `CalcFormInstall[options]` | Check availability and explicitly attempt installation on supported Debian/Ubuntu systems. |
| `CalcFormCalculate[expr, options]` | Export, execute FORM/TFORM, import, and return the FeynCalc expression. |

`FORMThreads` selects the worker count for checking, installation, and calculation. `ShowTiming` reports FORM execution wall time; `ShowProgress` reports stages and elapsed execution time. Use `AbsoluteTiming` when measuring the entire Mathematica call. More workers do not guarantee a faster calculation.

The converter performs algebra and Lorentz contractions. It does **not** perform loop integration or integral reduction, or supply symmetry factors, integration measures, or normalization conventions. Supporting scalar integral notation does not mean that it evaluates those integrals. Checking and calculating never install software implicitly.

See the [user guide](CalcFormConverter/README.md) for workflows, supported expressions, options, installation details, and performance guidance; the [format specification](CalcFormConverter/FORMAT.md) for saved-file compatibility; and the [developer guide](CalcFormConverter/DEVELOPER.md) for architecture, tests, and measurements.

## Examples

The [Examples directory](Examples) contains complete notebooks. Run their setup cells before the calculation cells; these establish libraries, kinematics, and conventions.

| Notebook | Topic |
| --- | --- |
| [Scalars_Gravitational_Scattering_Tree_Level.nb](Examples/Scalars_Gravitational_Scattering_Tree_Level.nb) | Tree-level scalar scattering through gravity. |
| [Graviton_Scattering_Tree_Level.nb](Examples/Graviton_Scattering_Tree_Level.nb) | Tree-level graviton scattering and polarization contractions; FORM calculation cells require FORM/TFORM. |
| [Nieuwenhuizen_Operators.nb](Examples/Nieuwenhuizen_Operators.nb) | Algebra of the Nieuwenhuizen operators. |
| [Graviton_Self_Energy.nb](Examples/Graviton_Self_Energy.nb) | Graviton contributions to the graviton self-energy. |
| [Graviton_Self_Energy_Matter_Contribution.nb](Examples/Graviton_Self_Energy_Matter_Contribution.nb) | Matter contributions to the graviton self-energy; setup includes `importSUNYM[]`. |
| [Matter_Self_Energy_Graviton_Contribution.nb](Examples/Matter_Self_Energy_Graviton_Contribution.nb) | Gravitational contributions to matter self-energies; setup includes `importSUNYM[]`. |
| [Graviton_Scalar_Vertex_at_First_Loop.nb](Examples/Graviton_Scalar_Vertex_at_First_Loop.nb) | A one-loop graviton–scalar vertex calculation. |

The converter's [ScalarBubble.wl](CalcFormConverter/Examples/ScalarBubble.wl) demonstrates manual and automated FORM workflows for a scalar-projected quadratic-gravity bubble. It requires the cubic quadratic-gravity library, loaded with `importQuadraticGravity[1]`, and FORM/TFORM for execution.

## Benchmarks

The [Benchmark notebooks](Benchmark/README.md) measure export, FORM execution, import and complete CalcFormConverter calls. Select **Quick** or **Full** before running. Inputs and import fixtures are generated locally; no extra tools beyond Mathematica, the package dependencies, and FORM/TFORM are required. Reports include raw timings, validation status and source hashes.

## Package structure

| Location | Contents |
| --- | --- |
| [FeynGrav.wl](FeynGrav.wl) | Main package, public commands, and library loading. |
| [Rules](Rules) | Interaction rules and algebra utilities, including the local `Calc` implementation. |
| [Libs](Libs) | Precomputed vertex libraries and the separate library generator. |
| [Examples](Examples) | Calculation notebooks. |
| [CalcFormConverter](CalcFormConverter) | Converter, FORM runtime, templates, documentation, examples, and tests. |

## Troubleshooting and support

- **The package cannot be found:** check that `FeynGrav.wl` is directly inside `$UserBaseDirectory/Applications/FeynGrav`, and that FeynCalc loads in the same kernel.
- **A vertex remains unevaluated or a library cannot be loaded:** check the argument syntax, required sector, and installed library order. Enable `printOutput -> True` on the relevant import command to inspect what is available.
- **FORM works in a terminal but cannot be found in Mathematica:** the kernel may have a different `PATH`. Supply the absolute executable path using `FORMExecutable` when checking or calculating. Multiple workers require a working TFORM executable.
- **A FORM calculation returns `Failure`:** inspect the complete failure, including its nested cause, stage, job directory, and log paths. Failed jobs retain diagnostic files. See the [converter troubleshooting guide](CalcFormConverter/README.md#troubleshooting).
- **Definitions appear inconsistent after an update:** restart the kernel and rerun the notebook's setup cells.

Report problems through [GitHub issues](https://github.com/BorisNLatosh/FeynGrav/issues) or contact Dr Boris Latosh at [latosh.boris@gmail.com](mailto:latosh.boris@gmail.com). Include your operating system, Wolfram and FeynCalc versions, FeynGrav revision, and a minimal reproducible expression. For converter issues, include the FORM/TFORM version, options, full failure details, and relevant logs.

## Version history

### Unreleased — FORM-converter branch

- Added CalcFormConverter for exporting bosonic FeynCalc expressions to FORM and importing results with preserved symbol mappings.
- Added FORM/TFORM availability checks, explicit installation support, automated calculation, timing, and progress reporting.
- Improved conversion performance and support for graviton polarization workflows.
- Updated the graviton-scattering notebook to use the converter and group vertex arguments by external leg.
- Documented the FeynCalc 10.2.1 dependency and use of FeynGrav's local `Calc` implementation.

### Version 4

- Implemented a finite set of graviton–Faddeev–Popov ghost vertices.
- Added a higher-derivative gauge-fixing term for quadratic gravity.
- Implemented Cheung–Remmen variables for general relativity.
- Added functions for working with the Nieuwenhuizen operators.

### Version 3

- Added a massive-gravity propagator, Horndeski models, axion-like coupling, and quadratic gravity.

### Version 2

- Added arbitrary masses for spin-0, spin-1/2, and spin-1 fields.
- Extended general relativity with advanced gauge fixing and implemented SU(N) Yang–Mills theory.

### Version 1

- Supported massless spin-0, spin-1/2, and spin-1 fields, together with gravity.

## Citations and license

When using FeynGrav in published work, cite the publications relevant to the version and features used:

- Boris Latosh, [FeynGrav](https://doi.org/10.1088/1361-6382/ac7e15), *Classical and Quantum Gravity* **39** (2022), 165006.
- Boris Latosh, [FeynGrav 2.0](https://doi.org/10.1016/j.cpc.2023.108871), *Computer Physics Communications* **292** (2023), 108871.
- Boris Latosh, [FeynGrav 3.0](https://doi.org/10.1016/j.cpc.2025.109508), *Computer Physics Communications* **310** (2025), 109508.
- Boris Latosh, [FeynGrav 4.0](https://arxiv.org/abs/2510.17320), arXiv:2510.17320.

Follow the citation guidance of FeynCalc and FORM when using those packages as well.

FeynGrav is distributed under the GNU General Public License version 3; see [LICENSE](LICENSE). Separately downloaded datasets carry the license stated on their distribution page.
