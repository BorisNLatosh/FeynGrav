(* ::Package:: *)
(* Explicit optional companion: ordinary FeynGrav loading does not load it.
   Loading THIS file only defines functions; it does not initialise a model,
   write an artefact, execute FeynArts/FORM or launch a process. *)
BeginPackage["FeynGrav`FeynArtsIntegration`"];
FeynGravBuildModel::usage = "FeynGravBuildModel[absoluteNewDirectory] snapshots the fixed-gauge conventional pure-GR graviton/ghost rules into a finite FeynArts generic/classes model, after explicitly loading FeynGrav and patched FeynArts in a fresh kernel. It never overwrites an existing directory. The one-loop 1PI integrand additionally requires the exact independently validated source/model fingerprint.";
FeynGravCreateAmplitude::usage = "FeynGravCreateAmplitude[model, process, incomingMomenta, outgoingMomenta] constructs complete three-/four-graviton trees. With an explicit fifth {loopMomentum} list and the pinned verified one-loop 1PI T[1]->T[1] process, it returns tadpole, graviton/ghost bubbles and a dummy-index-separated unintegrated total. Both modes retain per-diagram open-index FeynCalc expressions, raw FeynArts amplitudes and drawable insertions. No on-shell condition, loop measure, integral evaluation, FORM loop reduction or external wavefunction is inserted.";
EndPackage[];

With[{directory = DirectoryName[$InputFileName]},
  Get[FileNameJoin[{directory, "Process.wl"}]];
  Get[FileNameJoin[{directory, "Basis.wl"}]];
  Get[FileNameJoin[{directory, "ModelBuilder.wl"}]];
  Get[FileNameJoin[{directory, "Diagram.wl"}]];
  Get[FileNameJoin[{directory, "Loop.wl"}]];
];
