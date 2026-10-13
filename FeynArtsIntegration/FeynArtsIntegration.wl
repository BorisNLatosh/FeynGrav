(* ::Package:: *)
(* Explicit optional companion: ordinary FeynGrav loading does not load it.
   Loading THIS file only defines functions; it does not initialise a model,
   write an artefact, execute FeynArts/FORM or launch a process. *)
BeginPackage["FeynGrav`FeynArtsIntegration`"];
FeynGravBuildModel::usage = "FeynGravBuildModel[absoluteNewDirectory] snapshots the fixed-gauge conventional pure-GR graviton/ghost rules into a finite FeynArts generic/classes model, after explicitly loading FeynGrav and patched FeynArts in a fresh kernel. The default GravitonOrder -> 2 includes the cubic and quartic graviton vertices and the validated gravitational ghost sector, which is inexpensive: the resulting generic model is about 0.7 MB and FeynArts initialises it in roughly 1.6 s at a 0.3 GB peak. GravitonOrder -> 3 additionally derives the five-graviton vertex and is expensive: its basis has 76,230 components against 231 cubic and 4,158 quartic components, the generic model grows to about 15.4 MB, and measured FeynArts initialisation took 486 s at a 6.07 GB peak on this machine (single measurements, not a portable expectation). The requested order is validated before any rule work and a missing requested library fails rather than truncating the snapshot silently. The builder never overwrites an existing directory. A rebuilt model does not inherit the one-loop 1PI verification: that integrand additionally requires the exact independently validated source/model fingerprint.";
GravitonOrder::usage = "GravitonOrder is the FeynGravBuildModel option selecting the graviton vertex valence: 2 (default) builds the cubic and quartic vertices with the ghost sector, 3 additionally builds the five-graviton vertex. Values other than 2 or 3 are rejected before any import or rule construction.";
FeynGravCreateAmplitude::usage = "FeynGravCreateAmplitude[model, process, incomingMomenta, outgoingMomenta] constructs complete three-/four-graviton trees. With an explicit fifth {loopMomentum} list and the pinned verified one-loop 1PI T[1]->T[1] process, it returns tadpole, graviton/ghost bubbles and a dummy-index-separated unintegrated total. Both modes retain per-diagram open-index FeynCalc expressions, raw FeynArts amplitudes and drawable insertions. No on-shell condition, loop measure, integral evaluation, FORM loop reduction or external wavefunction is inserted.";
EndPackage[];

With[{directory = DirectoryName[$InputFileName]},
  Get[FileNameJoin[{directory, "Process.wl"}]];
  Get[FileNameJoin[{directory, "Basis.wl"}]];
  Get[FileNameJoin[{directory, "ModelBuilder.wl"}]];
  Get[FileNameJoin[{directory, "Diagram.wl"}]];
  Get[FileNameJoin[{directory, "Loop.wl"}]];
];
