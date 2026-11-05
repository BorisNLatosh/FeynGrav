(* ::Package:: *)
(* Explicit optional companion: ordinary FeynGrav loading does not load it.
   Loading THIS file only defines functions; it does not initialise a model,
   write an artefact, execute FeynArts/FORM or launch a process. *)
BeginPackage["FeynGrav`FeynArtsIntegration`"];
FeynGravBuildModel::usage = "FeynGravBuildModel[absoluteNewDirectory] snapshots the fixed-gauge conventional pure-GR graviton/ghost rules into a finite FeynArts generic/classes model, after explicitly loading FeynGrav and patched FeynArts in a fresh kernel. The default GravitonOrder -> 2 includes the cubic and quartic graviton vertices and the tested gravitational ghost sector. The supplied model is already built and ready for tree construction. Load this companion before native model initialisation in a fresh kernel. GravitonOrder -> 3 adds the five-graviton contact. The verified quintic is stored as one symmetric FeynArts coupling component and expands at concrete diagram momenta. The isolated native contact has a complete off-shell comparison; five-point amplitudes are outside the optional bridge. The requested order is validated before any rule work and a missing requested library fails rather than truncating the snapshot silently. The builder never overwrites an existing directory. A rebuilt model does not inherit the one-loop 1PI verification: that integrand additionally requires the exact independently validated source/model fingerprint.";
GravitonOrder::usage = "GravitonOrder is the FeynGravBuildModel option selecting the graviton vertex valence: 2 (default) builds the cubic and quartic vertices with the ghost sector, 3 additionally builds the five-graviton vertex. Values other than 2 or 3 are rejected before any import or rule construction.";
FeynGravCreateAmplitude::usage = "FeynGravCreateAmplitude[model, process, incomingMomenta, outgoingMomenta] constructs complete three-/four-graviton trees. The fifth {loopMomentum} interface is restricted to its historical verified fingerprint. The current corrected GR model has not undergone loop compatibility checks and returns GhostCompletenessGate before loop construction. This release supports trees; loop checks are deferred. Tree results retain per-diagram open-index FeynCalc expressions, raw FeynArts amplitudes and drawable insertions. No on-shell condition, loop measure, integral evaluation, FORM loop reduction or external wavefunction is inserted.";
EndPackage[];

With[{directory = DirectoryName[$InputFileName]},
  Get[FileNameJoin[{directory, "Process.wl"}]];
  Get[FileNameJoin[{directory, "Basis.wl"}]];
  Get[FileNameJoin[{directory, "ModelBuilder.wl"}]];
  Get[FileNameJoin[{directory, "Diagram.wl"}]];
  Get[FileNameJoin[{directory, "Loop.wl"}]];
];
