within PowerSystems.Test;
package ImpedancesTest
  model InductorTest

    PowerSystems.Generic.FixedVoltageSource
                               source(redeclare package PhaseSystem =
          PowerSystems.PhaseSystems.ThreePhase_dq0)
      annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                           rotation=0)));
    AC3ph.Impedances.Inductor
      load(r=1)
      annotation (Placement(transformation(extent={{-20,0},{0,20}}, rotation=0)));
    PowerSystems.Generic.Ground
      ground(redeclare package PhaseSystem =
          PowerSystems.PhaseSystems.ThreePhase_dq0)
      annotation (Placement(transformation(extent={{40,0},{60,20}}, rotation=0)));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
  equation
    connect(source.terminal, load.term_p) annotation (Line(
        points={{-60,10},{-20,10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(load.term_n, ground.terminal) annotation (Line(
        points={{0,10},{40,10}},
        color={0,120,120},
        smooth=Smooth.None));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=true,  extent={{-100,
              -100},{100,100}}),
                        graphics),
                experiment(StopTime=1));
  end InductorTest;
end ImpedancesTest;
