within PowerSystems;
package Test 


  package SimpleComponentsTest
  "See: Oeding, Oswald: Elektrische Kraftwerke und Netze, section 14: Leistungsfluss im Drehstromnetz."
    model ImpedanceTest
    import PowerSystems;

      PowerSystems.Generic.FixedVoltageSource
                                 source
        annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                             rotation=0)));
      PowerSystems.Generic.Impedance
        load(R=30, L=10/314)
        annotation (Placement(transformation(extent={{-20,0},{0,20}}, rotation=0)));
      PowerSystems.Generic.Ground
        ground
        annotation (Placement(transformation(extent={{40,0},{60,20}}, rotation=0)));
    equation
      connect(source.terminal, load.terminal_p)
        annotation (Line(points={{-60,10},{-20,10}}, color={0,120,120}));
      connect(load.terminal_n, ground.terminal)
        annotation (Line(points={{0,10},{40,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end ImpedanceTest;

    model AdmittanceTest
    import PowerSystems;

      PowerSystems.Generic.FixedVoltageSource
                                 source
        annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                             rotation=0)));
      PowerSystems.Generic.Admittance
        load(G=30/(30*30 + 10*10), C=10/(30*30 + 10*10)/314)     annotation (Placement(transformation(extent={{-20,0},{0,20}}, rotation=0)));
      PowerSystems.Generic.Ground
        ground
        annotation (Placement(transformation(extent={{40,0},{60,20}}, rotation=0)));
    equation
      connect(source.terminal, load.terminal_p)
        annotation (Line(points={{-60,10},{-20,10}}, color={0,120,120}));
      connect(load.terminal_n, ground.terminal)
        annotation (Line(points={{0,10},{40,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end AdmittanceTest;

    model InductiveLoadTest
    import PowerSystems;

      PowerSystems.Generic.FixedVoltageSource
                                 source
        annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                             rotation=0)));
      PowerSystems.Generic.Impedance
        line(R=1.2, L=1.6/314)
        annotation (Placement(transformation(extent={{-40,0},{-20,20}}, rotation=0)));
      PowerSystems.Generic.Impedance
        load(R=30, L=10/314)
        annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
      PowerSystems.Generic.Ground
        ground
        annotation (Placement(transformation(extent={{40,
                                                      0},{60,20}}, rotation=0)));
    equation
      connect(source.terminal, line.terminal_p)
        annotation (Line(points={{-60,10},{-40,10}}, color={0,120,120}));
      connect(load.terminal_n, ground.terminal)
        annotation (Line(points={{20,10},{40,10}}, color={0,120,120}));
      connect(line.terminal_n, load.terminal_p)
        annotation (Line(points={{-20,10},{0,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end InductiveLoadTest;

    model FixedCurrentTest
    import PowerSystems;

      PowerSystems.Generic.FixedVoltageSource source
        annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                             rotation=0)));
      PowerSystems.Generic.Impedance
        line(R=1.2, L=1.6/314)
        annotation (Placement(transformation(extent={{-40,0},{-20,20}}, rotation=0)));
      PowerSystems.Generic.FixedCurrent
                           load(I=173.448, phi=-0.356)                                         annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
    equation
      connect(line.terminal_n, load.terminal)
        annotation (Line(points={{-20,10},{0,10}}, color={0,120,120}));
      connect(source.terminal, line.terminal_p)
        annotation (Line(points={{-60,10},{-40,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end FixedCurrentTest;

    model FixedLoadTest
    import PowerSystems;

      PowerSystems.Generic.FixedVoltageSource
                                 source
        annotation (Placement(transformation(extent={{-80,0},{-60,20}},
                                             rotation=0)));
      PowerSystems.Generic.Impedance
        line(R=1.2, L=1.6/314)
        annotation (Placement(transformation(extent={{-40,0},{-20,20}}, rotation=0)));
      PowerSystems.Generic.FixedLoad
                        load(                       P=2.7076e6, phi=atan(1000/
          3000))                                                                                                     annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
    equation
      connect(source.terminal, line.terminal_p)
        annotation (Line(points={{-60,10},{-40,10}}, color={0,120,120}));
      connect(line.terminal_n, load.terminal)
        annotation (Line(points={{-20,10},{0,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end FixedLoadTest;

    model GeneratorTest
    import PowerSystems;

      PowerSystems.Generic.Generator generator annotation (Placement(
          transformation(extent={{-20,0},{0,20}}, rotation=0)));
      Modelica.Mechanics.Rotational.Components.Inertia inertia(J=1e3, w(start=2*pi*50))
        annotation (Placement(transformation(extent={{-50,0},{-30,20}},
                                             rotation=0)));
      Modelica.Mechanics.Rotational.Sources.ConstantTorque constantTorque(useSupport=false, tau_constant=3e6/50/2/pi)
        annotation (Placement(transformation(extent={{-90,0},{-70,
                                                              20}}, rotation=0)));
      PowerSystems.Generic.FixedLoad
                        fixedLoad(P=3e6,
                                  phi=0.3)
        annotation (Placement(transformation(extent={{20,0},{40,20}}, rotation=
                                             0)));
    equation
    connect(inertia.flange_b, generator.flange)
      annotation (Line(points={{-30,10},{-20,10}}, color={0,0,0}));
      connect(constantTorque.flange, inertia.flange_a) annotation (Line(points=
                                                                        {{-70,10},{-50,10}}, color={0,0,0}));
    connect(generator.terminal, fixedLoad.terminal)
      annotation (Line(points={{0,10},{20,10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end GeneratorTest;

    model GeneratorTest2
    import PowerSystems;

      PowerSystems.Generic.Generator generator annotation (Placement(
          transformation(extent={{-20,0},{0,20}}, rotation=0)));
      Modelica.Mechanics.Rotational.Components.Inertia inertia(J=1e3, w(start=50*2*pi))
        annotation (Placement(transformation(extent={{-50,0},{-30,20}},
                                             rotation=0)));
      Modelica.Mechanics.Rotational.Sources.ConstantTorque constantTorque(useSupport=false, tau_constant=3e6/50/2/2/pi)
        annotation (Placement(transformation(extent={{-90,0},{-70,
                                                              20}}, rotation=0)));
      PowerSystems.Generic.FixedLoad
                        fixedLoad(P=3e6,
                                  phi=0.3)
        annotation (Placement(transformation(extent={{60,-20},{80,0}}, rotation=
                                             0)));
      PowerSystems.Generic.Generator generator1(synchronous=false) annotation (
        Placement(transformation(extent={{-20,-40},{0,-20}}, rotation=0)));
      Modelica.Mechanics.Rotational.Components.Inertia inertia1(J=1e3, w(start=50*2*pi))
        annotation (Placement(transformation(extent={{-50,-40},{-30,-20}},
                                             rotation=0)));
      Modelica.Mechanics.Rotational.Sources.ConstantTorque constantTorque1(useSupport=false, tau_constant=3e6/50/2/pi)
        annotation (Placement(transformation(extent={{-90,-40},{-70,
                                                                -20}}, rotation=0)));
      PowerSystems.Generic.Impedance
        impedance
        annotation (Placement(transformation(extent={{20,0},{40,20}}, rotation=0)));
      PowerSystems.Generic.Impedance
        impedance1
        annotation (Placement(transformation(extent={{20,-40},{40,-20}}, rotation=0)));
    equation
    connect(inertia.flange_b, generator.flange)
      annotation (Line(points={{-30,10},{-20,10}}, color={0,0,0}));
      connect(constantTorque.flange, inertia.flange_a) annotation (Line(points=
                                                                        {{-70,10},{-50,10}}, color={0,0,0}));
    connect(inertia1.flange_b, generator1.flange)
      annotation (Line(points={{-30,-30},{-20,-30}}, color={0,0,0}));
      connect(constantTorque1.flange, inertia1.flange_a)
        annotation (Line(points=
                         {{-70,-30},{-50,-30}}, color={0,0,0}));
    connect(generator.terminal, impedance.terminal_p)
      annotation (Line(points={{0,10},{20,10}}, color={0,120,120}));
      connect(impedance.terminal_n, fixedLoad.terminal) annotation (Line(points=
                                                                         {{40,10},{50,10},{50,-10},{60,-10}}, color={0,120,120}));
    connect(generator1.terminal, impedance1.terminal_p)
      annotation (Line(points={{0,-30},{20,-30}}, color={0,120,120}));
      connect(impedance1.terminal_n, fixedLoad.terminal) annotation (Line(points={{40,-30},{50,-30},{50,-10},{60,-10}}, color={0,120,120}));
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                                                                               -100},{100,100}}),
                          graphics),
                  experiment(StopTime=1));
    end GeneratorTest2;

  end SimpleComponentsTest;

end Test;
