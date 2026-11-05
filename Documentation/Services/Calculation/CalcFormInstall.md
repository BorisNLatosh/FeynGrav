# CalcFormInstall

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`CalcFormInstall``. **Baseline:** 4.2.0.

Explicitly attempts to install missing FORM or TFORM on supported Debian/Ubuntu systems, with system authorisation, then checks the requested configuration.

## Syntax and arguments

```mathematica
CalcFormInstall[opts]
```

Returns availability information or a diagnostic failure. It is never invoked automatically. Only `FORMThreads` is accepted; executable paths and calculation timeouts are not installation options. Normal package loading does not install software.

## Options

- [FORMThreads](../Options/FORMThreads.md)

Defaults are per command; inspect `Options[CalcFormInstall]` for the current session. An option passed in one call does not modify later defaults.

## Explicit installation

Invoke `CalcFormInstall[]` only when intending to install software. Use [CalcFormCheck](./CalcFormCheck.md) for an availability query.

## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
