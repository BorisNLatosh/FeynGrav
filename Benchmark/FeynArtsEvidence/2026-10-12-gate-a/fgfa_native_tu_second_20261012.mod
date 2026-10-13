(* Deliberately distinct coefficient for a bounded FeynArts model-cache check. *)
M$ClassesDescription = {
 T[1] == {SelfConjugate -> True, Mass -> 0, PropagatorLabel -> "h", PropagatorType -> Sine},
 U[1] == {SelfConjugate -> False, Mass -> 0, PropagatorLabel -> "c", PropagatorType -> GhostDash, PropagatorArrow -> Forward},
 S[1] == {SelfConjugate -> True, Mass -> 0, PropagatorLabel -> "s", PropagatorType -> ScalarDash}
};
M$CouplingMatrices = {
 C[T[1],S[1],S[1]] == {{2 I gg}, {0}},
 C[T[1],-U[1],U[1]] == {{I gh}}
};
M$LastModelRules = {};
