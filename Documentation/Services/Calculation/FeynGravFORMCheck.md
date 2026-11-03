# FeynGravFORMCheck

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`FeynGravFORMCheck``. **Baseline:** 4.2.0 development.

Preferred facade wrapper of [CalcFormCheck](./CalcFormCheck.md). Probes FORM/TFORM on demand and reports availability, version and diagnostics.

## Syntax and arguments

```mathematica
FeynGravFORMCheck[opts]
```

Returns an association on a completed availability check; inspect `TrueQ[status["Available"]]`. A missing executable is reported as unavailable, not as a valid algebra result. Invalid calls may return `Failure`. The default check timeout is 10 seconds. It does not install software. Its option store is independent of the underlying command; `SetOptions` on one does not change the other.

## Options

- [FORMExecutable](../Options/FORMExecutable.md)
- [FORMThreads](../Options/FORMThreads.md)
- [TimeConstraint](../Options/TimeConstraint.md)

Defaults are per command; inspect `Options[FeynGravFORMCheck]` for the current session. An option passed in one call does not modify later defaults.

## Example

In a fresh kernel with symbolic indices and momenta:

```mathematica
<< FeynGrav`
status = FeynGravFORMCheck[];
available = AssociationQ[status] && TrueQ[status["Available"]];
available
```

Expected result: `True` when the requested FORM/TFORM configuration passes its availability check. If it is `False`, display `status` to see the missing executable, failed probe or invalid option. An availability check does not install software or run your physical calculation.



## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
