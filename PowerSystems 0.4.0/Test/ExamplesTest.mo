within PowerSystems.Test;
package ExamplesTest "Test variants of the examples"
  model NetworkLoop_d = PowerSystems.Examples.Network.NetworkLoop (
    redeclare replaceable package PhaseSystem =
          PowerSystems.PhaseSystems.ThreePhase_d);
  model NetworkLoop_dq =PowerSystems.Examples.Network.NetworkLoop (
    redeclare replaceable package PhaseSystem =
          PowerSystems.PhaseSystems.ThreePhase_dq);
  model NetworkLoop_dq0=PowerSystems.Examples.Network.NetworkLoop (
    redeclare replaceable package PhaseSystem =
          PowerSystems.PhaseSystems.ThreePhase_dq0);
end ExamplesTest;
