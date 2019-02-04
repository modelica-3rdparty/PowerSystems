within PowerSystems;
package AC3ph "AC three phase components from Spot ACdq0"
  extends Modelica.Icons.VariantsPackage;

  replaceable package PackagePhaseSystem = PhaseSystems.ThreePhase_dq0
    "Default phase system for AC3ph"
    annotation (choicesAllMatching=true);

end AC3ph;
