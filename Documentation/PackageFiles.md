# Opening notebooks and package files

This manual can be read by opening `Documentation/` as an Obsidian vault. Its Markdown pages stay inside that folder. Reading the manual does not require Mathematica or a complete package installation.

## Find the package folder

To run an example, use the complete FeynGrav package matching this documentation. Paths such as `Examples/01. Nieuwenhuizen_Operators.nb` start at the FeynGrav folder containing `FeynGrav.wl`; they are not paths inside this vault.

In Mathematica, locate the installed package with:

```wl
DirectoryName[FindFile["FeynGrav`"]]
```

If FeynGrav is not installed, follow [Getting started](./GettingStarted.md). If you are reading a downloaded checkout, use that checkout's folder and follow its installation instructions so the examples and loaded package match.

<a name="Open a notebook"></a>

## Open a notebook

Use Mathematica's **File → Open**, navigate to the package's `Examples/` or `Benchmark/` folder, and select the named `.nb` file. Start a fresh kernel and follow the notebook's setup instructions before evaluating cells. Opening a notebook does not run it. A standalone copy of this manual does not contain the notebook files.

The [example guide](./Examples/README.md) explains the numbered notebooks and their dependencies. The [benchmark guide](./Benchmarks/README.md) describes the timing notebooks and their explicit run settings.

## Other package files

Source scripts, saved libraries and native FeynArts models also remain at their package-relative paths. Follow the relevant guide's loading commands; do not load source files merely to read them. The manual displays these paths as code rather than as Obsidian note links. Keep model `.gen`/`.mod` files and their matching manifest together.
