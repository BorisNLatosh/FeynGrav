# FeynGrav benchmarks

Select a notebook, then explicitly choose `"Quick"` or `"Full"`. The default
profile is `None`; opening a notebook does not run measurements.

| Notebook | Measures |
| --- | --- |
| [01 — Quick check](01_Overview_and_Quick_Check.nb) | Known contractions, package setup, availability checks and complete public calls |
| [02 — Export](02_Export.nb) | Public export with repeated/distinct objects, routed momenta, and factored/expanded inputs |
| [03 — Import](03_Import.nb) | Public import of locally generated, unchanged result/mapping pairs |
| [04 — Thread scaling](04_FORM_Thread_Scaling.nb) | Serial FORM and TFORM execution with alternating worker configurations |
| [05 — Representative calculations](05_Representative_Calculations.nb) | Construction, export, execution, import and total time for package expressions |
| [06 — Library generation](06_Library_Generation.nb) | Public generator calls, isolated generation stages and supplementary tensors |

See the [benchmark guide](UserGuide.md) for settings, timing boundaries,
reports, validation and cleanup. Use a fresh kernel; notebook 06 loads the
generator rather than the main-package workflow.

<!-- Compatibility anchors for links to the former detailed README. -->
<a id="configuration"></a><a id="what-the-timings-mean"></a><a id="validation-and-reports"></a><a id="maintenance"></a><a id="library-generation-notebook-06"></a><a id="developer-verification-and-notebook-generation"></a><a id="generator-timing-presentation"></a>
Detailed sections: [UserGuide](UserGuide.md).
