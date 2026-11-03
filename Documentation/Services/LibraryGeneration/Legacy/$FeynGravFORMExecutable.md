# $FeynGravFORMExecutable

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`$FeynGravFORMExecutable``. **Baseline:** 4.2.0 development.

Legacy executable selection looked up by name in the generator context or as ``Global`$FeynGravFORMExecutable``. It is a compatibility setting, not a function and not necessarily newly declared by loading the generator.

An explicit per-call `FORMExecutable` takes precedence over command defaults and this legacy setting. An automatic stored default may use the legacy setting. Prefer [FORMExecutable](../../Options/FORMExecutable.md) on the invoked generator command for new code. This setting does not install or probe FORM by itself.
