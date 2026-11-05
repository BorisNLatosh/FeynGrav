# InstallFeynGrav

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravInstaller`InstallFeynGrav``. **Baseline:** 4.2.0.

Downloads, validates and installs a FeynGrav archive. Load the repository's `install.m` to define the installer; loading it alone does not install the package.

## Syntax

```mathematica
InstallFeynGrav[opts]
```

No positional arguments are accepted. The default reference is the current GitHub `main` branch, not the latest release. `FeynGravReference -> "v4.2.0"` selects version 4.2.0 once its tag is published.

## Options

| Option | Default | Purpose |
| --- | --- | --- |
| [InstallFeynGravTo](../Options/InstallFeynGravTo.md) | `Automatic` | User applications directory, or an explicit absolute destination. |
| [FeynGravReference](../Options/FeynGravReference.md) | `"main"` | Branch, tag or commit of the official repository. |
| [FeynGravArchive](../Options/FeynGravArchive.md) | `Automatic` | Download the archive, or use an existing local ZIP. |
| [OverwriteFeynGrav](../Options/OverwriteFeynGrav.md) | `Automatic` | Ask before replacement; explicit True consents and False refuses. |
| [InstallFeynCalcDependency](../Options/InstallFeynCalcDependency.md) | `Automatic` | Ask before invoking the official dependency installer if needed. |

## Result and side effects

Success returns an association with `InstalledPath`, `Reference`, `Source`, `Verification` and `BackupPath`. Failures are diagnostic `Failure` values. The archive layout is checked and the staged package is probed in a fresh kernel before replacement. Git working checkouts are never replaced. A complete sibling backup of an existing installation is retained; local edits and extra libraries are not merged into the new installation.

With a local ZIP and a compatible preinstalled FeynCalc, the package archive need not be downloaded. A dependency installation can still require network access if explicitly requested. Without a notebook front end, an unanswered automatic consent step returns `ConsentRequired`.

After installation, restart the kernel and recompute affected expressions. Replacing files does not update loaded definitions or saved notebook output.

See the [installation workflow](../../UserGuide.md#Installation%20and%20help) and [Scientific scope](../../ScientificScope.md#Installation%20and%20compatibility).
