# Omitting `Transversality -> False`

Recorded 12 October 2026. No shipped code changed; this directory documents a style correction and re-validates the example notebook as it now stands.

## The correction

Writing `Transversality -> False` is bad practice. `False` is the default, so a non-transverse polarisation is requested simply by **not** giving the option:

```wl
PolarizationTensorD[p1, mu1, nu1, I]      (* non-transverse, the default *)
```

The package documents the same behaviour in `Documentation/Reference.md`: "Omitting it leaves the internal `Polarization` object without an explicit transversality option; an explicit option is forwarded." An explicit `Transversality -> False` therefore produces a *different* object from the omitted form, even though both are physically non-transverse. That difference is real for anything comparing expressions, and it is why a harness that spells the option out can stop matching a notebook that omits it.

The correction applies to the example notebook. The user removed the option from the external-tensor cell directly, so no rewrite was needed here; an earlier candidate rewrite built for this task was deleted as unnecessary.

## Re-validation of the notebook as it stands

`before.nb` is the notebook as edited, byte-identical to the installed `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb` (SHA-256 `536eef35ebdbc46785e76dd7071a3e6ea2e7bf114719fffbb25c16c6c758312d`). `check-notebook.wls` is a copy of the previous harness with two changes: it reads `before.nb`, and it no longer writes the option itself.

`check-notebook.log`: **19 checks passed**, exit 0, peak memory 318,296,064 bytes under a verified 1-GiB cgroup with zero swap and zero cap/OOM events. The new check `the notebook imposes no transversality option` confirms the input cells contain no `Transversality` string at all. The physics checks are unchanged: four diagrams, denominator counts `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, four scalar FORM results, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, contact equal to the source, and retained off-shell virtuality, longitudinal and trace terms.

That last group passing is the useful evidence: with the harness omitting the option too, the polarisation objects it constructs match the notebook's exactly.

## Retained harnesses are stale for this notebook

The harnesses under `notebook-four-tree-2026-10-12`, `notebook-no-contexts-2026-10-12`, `notebook-s-channel-offshell-2026-10-12`, `notebook-readability-2026-10-12` and `model-relocation-2026-10-12` all build polarisation tensors with an explicit `Transversality -> False`. Their logs are records of runs made when the notebook also spelled the option out, so they are left untouched. They are **not** suitable for re-running against the current notebook; use `check-notebook.wls` here instead.
