# Colour procedure preflight

> Historical implementation record. Forward-looking statements describe that stage. For current capabilities use the [converter guide](../../README.md). The original observations and limitations below are retained.

These are standalone checks of the proposed SU(N) backend. They check the unchanged upstream algorithm separately from converter integration.
The unchanged source is now bundled in `../../ThirdParty/FORMColour/SUn.prc`;
see its attribution notes for the GPL basis. These checks perform no downloads
or installation.

## Reproduce

Check the bundled `SUn.prc` against the member SHA-256 in `Upstream.json`.
Use a separate working directory for these commands, substituting absolute paths:

```sh
form -q -D CFCSUNFILE=/path/to/SUn.prc /path/to/ColourProcedure.frm
tform -w2 -q -D CFCSUNFILE=/path/to/SUn.prc /path/to/ColourProcedure.frm
WolframKernel -noinit -script /path/to/ColourReference.wls
```

Each FORM run checks 18 exact zero residuals and writes `ColourProcedure.out`
in its working directory. Compare that file with `Expected.out` before running
the second engine, which replaces the output. The Wolfram script independently
checks the same identities with FeynCalc; successful completion prints all 18
zero residuals and the version, then exits with code zero. An interrupted script
without the final version line must not be counted as a completed check.

The three observation rows are deliberately not zero-residual assertions:
empty traces require adapter handling, while three- and four-generator traces
remain in the upstream trace basis. The three-generator output basis conversion
is separate future work. Distinct cyclic orderings are not identified with
reversed orderings.

## Integration findings

- Declare fundamental and adjoint dimensions separately.
- Set generator normalisation to `a = 1/2` and flavour multiplicity to `nf = 1`.
  Explicitly substitute `a^-1` and `nf^-1` as well: the corresponding positive
  power substitutions alone leave inverse parameters in some results.
- Reserve upstream temporary indices `i1` through `i4` and `j1` through `j3`.
  User indices in this fixture have a separate prefix. The future adaptation
  must namespace all internal objects before accepting arbitrary user names.
- FeynCalc trace evaluation is explicit in the independent reference script.
  Production import must not call `SUNSimplify`.

These checks cover delta contractions, loops, generator completeness, open
Casimirs, sandwiches, chain joining and closure, f/f, d/d and f/d contractions,
trace commutators, products, powers and cyclicity. They do not verify converter
translation, generated endpoint promotion, restricted parsing or saved formats.
