within PowerSystems.Examples.Generic;
package Network "Text book example and variants of it"
  extends Modelica.Icons.ExamplesPackage;

  model NetworkLoop
    "Simple textbook example for a steady-state power flow calculation"
    extends Modelica.Icons.Example;

    replaceable package PhaseSystem =
        PhaseSystems.ThreePhase_dq "Default phase system"
      annotation (choicesAllMatching=true);

    PowerSystems.Generic.FixedVoltageSource
      fixedVoltageSource1(V=10e3, redeclare package PhaseSystem=PhaseSystem)
      annotation (Placement(transformation(
          origin={0,70},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance1(R=2, L=0,
    redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(
          origin={-50,-10},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance2(R=4, L=0,
    redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(
          origin={-50,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance3(R=2, L=0,
    redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(extent={{-10,-90},{10,-70}})));
    PowerSystems.Generic.Impedance impedance4(L=0, R=1,
    redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(
          origin={50,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance5(L=0, R=3,
    redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(
          origin={50,-10},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.FixedCurrent fixedCurrent3(I=50,
      redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(extent={{70,-90},{90,-70}})));
    PowerSystems.Generic.FixedCurrent fixedCurrent1(I=55,
      redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(extent={{-70,-40},{-90,-20}})));
    PowerSystems.Generic.FixedCurrent fixedCurrent2(I=45,
      redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(extent={{-70,-90},{-90,-70}})));
    PowerSystems.Generic.FixedCurrent fixedCurrent4(I=60,
      redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(extent={{70,-40},{90,-20}})));
    PowerSystems.Generic.Transformer transformer1(ratio=10/10.4156,
      redeclare package PhaseSystem = PhaseSystem)
      annotation (Placement(transformation(
          origin={-50,30},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Transformer transformer2(ratio=10/10, redeclare
        package PhaseSystem=PhaseSystem)
      annotation (Placement(transformation(
          origin={50,30},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    inner System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
  equation
    connect(impedance1.terminal_n, impedance2.terminal_p)
      annotation (Line(points={{-50,-20},{-50,-40}}, color={0,120,120}));
    connect(impedance2.terminal_n, impedance3.terminal_p) annotation (Line(
          points={{-50,-60},{-50,-80},{-10,-80}}, color={0,120,120}));
    connect(impedance4.terminal_p, impedance5.terminal_n)
      annotation (Line(points={{50,-40},{50,-20}}, color={0,120,120}));
    connect(fixedCurrent1.terminal, impedance1.terminal_n) annotation (Line(
          points={{-70,-30},{-50,-30},{-50,-20}}, color={0,120,120}));
    connect(fixedCurrent2.terminal, impedance3.terminal_p)
      annotation (Line(points={{-70,-80},{-10,-80}}, color={0,120,120}));
    connect(fixedCurrent4.terminal, impedance5.terminal_n) annotation (Line(
          points={{70,-30},{50,-30},{50,-20}}, color={0,120,120}));
    connect(fixedVoltageSource1.terminal, transformer1.terminal_p) annotation (Line(
          points={{-1.83697e-015,60},{-1.83697e-015,50},{-50,50},{-50,40}},
          color={0,120,120}));
    connect(transformer1.terminal_n, impedance1.terminal_p)
      annotation (Line(points={{-50,20},{-50,0}}, color={0,120,120}));
    connect(transformer2.terminal_n, impedance5.terminal_p)
      annotation (Line(points={{50,20},{50,0}}, color={0,120,120}));
    connect(transformer2.terminal_p, fixedVoltageSource1.terminal) annotation (Line(
          points={{50,40},{50,50},{-1.83697e-015,50},{-1.83697e-015,60}},
                                                                        color=
           {0,120,120}));
    connect(impedance3.terminal_n, fixedCurrent3.terminal) annotation (Line(
        points={{10,-80},{70,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(impedance4.terminal_n, impedance3.terminal_n) annotation (Line(
        points={{50,-60},{50,-80},{10,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    annotation (Documentation(info="<html>
  <p>This textbook example demonstrates a basic power flow calculation.</p>
  <p>See Oeding, Oswald: Elektrische Kraftwerke und Netze, section 14.2.6: Leistungsfluss in Ringnetzen.</p>
</html>"),                         experiment(StopTime=1));
  end NetworkLoop;

  model NetworkLoop_d "NetworkLoop example with phase system ThreePhase_d"
    extends NetworkLoop(
      redeclare replaceable package PhaseSystem =
        PowerSystems.PhaseSystems.ThreePhase_d);
    annotation (experiment(StopTime=1));
  end NetworkLoop_d;

  model NetworkLoop_dq "NetworkLoop example with phase system ThreePhase_dq"
    extends NetworkLoop(
      redeclare replaceable package PhaseSystem =
        PowerSystems.PhaseSystems.ThreePhase_dq);
    annotation (experiment(StopTime=1));
  end NetworkLoop_dq;

  model NetworkLoop_dq0 "NetworkLoop example with phase system ThreePhase_dq0"
    extends NetworkLoop(
      redeclare replaceable package PhaseSystem =
        PowerSystems.PhaseSystems.ThreePhase_dq0);
    annotation (experiment(StopTime=1));
  end NetworkLoop_dq0;

  model NetworkOpened
    "Steady-state power flow calculation with two voltage sources"
    extends Modelica.Icons.Example;

    PowerSystems.Generic.FixedVoltageSource fixedVoltageSource1(V=10e3)
      annotation (Placement(transformation(
            origin={-50,70},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.FixedVoltageSource fixedVoltageSource2(V=10e3)
      annotation (Placement(transformation(
            origin={50,70},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.Impedance impedance1(R=2, L=0)
      annotation (Placement(transformation(
            origin={-50,-10},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.Impedance impedance2(L=0, R=4)
      annotation (Placement(transformation(
            origin={-50,-50},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.Impedance impedance3(R=2, L=0)
      annotation (Placement(transformation(extent={{-10,-90},{10,-70}})));
    PowerSystems.Generic.Impedance impedance4(L=0, R=1)
      annotation (Placement(transformation(
            origin={50,-50},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.Impedance impedance5(L=0, R=3)
      annotation (Placement(transformation(
            origin={50,-10},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent3(I=50) annotation (Placement(
            transformation(extent={{70,-90},{90,-70}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent1(I=55) annotation (Placement(
            transformation(extent={{-70,-40},{-90,-20}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent2(I=45)
      annotation (Placement(transformation(extent={{-70,-90},{-90,-70}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent4(I=60) annotation (Placement(
            transformation(extent={{70,-40},{90,-20}})));
    PowerSystems.Generic.Transformer transformer1(ratio=10/10.4156)
      annotation (Placement(transformation(
            origin={-50,30},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    PowerSystems.Generic.Transformer transformer2(ratio=10/10)
      annotation (Placement(transformation(
            origin={50,30},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    inner System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
  equation
    connect(impedance1.terminal_n, impedance2.terminal_p)
      annotation (Line(points={{-50,-20},{-50,-40}}, color={0,120,120}));
    connect(impedance2.terminal_n, impedance3.terminal_p) annotation (Line(
            points={{-50,-60},{-50,-80},{-10,-80}}, color={0,120,120}));
    connect(impedance3.terminal_n, impedance4.terminal_n) annotation (Line(
            points={{10,-80},{50,-80},{50,-60}}, color={0,120,120}));
    connect(impedance4.terminal_p, impedance5.terminal_n)
      annotation (Line(points={{50,-40},{50,-20}}, color={0,120,120}));
    connect(impedance3.terminal_n, fixedCurrent3.terminal)
      annotation (Line(points={{10,-80},{70,-80}}, color={0,120,120}));
    connect(fixedCurrent1.terminal, impedance1.terminal_n) annotation (Line(
            points={{-70,-30},{-50,-30},{-50,-20}}, color={0,120,120}));
    connect(fixedCurrent2.terminal, impedance3.terminal_p)
      annotation (Line(points={{-70,-80},{-10,-80}}, color={0,120,120}));
    connect(fixedCurrent4.terminal, impedance5.terminal_n) annotation (Line(
            points={{70,-30},{50,-30},{50,-20}}, color={0,120,120}));
    connect(fixedVoltageSource1.terminal, transformer1.terminal_p)
      annotation (Line(points={{-50,60},{-50,40}}, color={0,120,120}));
    connect(transformer1.terminal_n, impedance1.terminal_p)
      annotation (Line(points={{-50,20},{-50,0}}, color={0,120,120}));
    connect(fixedVoltageSource2.terminal, transformer2.terminal_p)
      annotation (Line(points={{50,60},{50,40}}, color={0,120,120}));
    connect(transformer2.terminal_n, impedance5.terminal_p)
      annotation (Line(points={{50,20},{50,0}}, color={0,120,120}));
    annotation (Documentation(info="<html>
  <p>The loop of the NetworkLoop example has been opened and a second voltage source was added.</p>
</html>"),                           experiment(StopTime=1));
  end NetworkOpened;

  model NetworkControlled "Dynamic power flow calculation with two generators"
    extends Modelica.Icons.Example;

    PowerSystems.Generic.Impedance impedance1(R=2, L=0)
      annotation (Placement(transformation(
          origin={-50,-10},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance2(L=0, R=4)
      annotation (Placement(transformation(
          origin={-50,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance3(R=2, L=0)
      annotation (Placement(transformation(extent={{-10,-90},{10,-70}})));
    PowerSystems.Generic.Impedance impedance4(L=0, R=1)
      annotation (Placement(transformation(
          origin={50,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Impedance impedance5(L=0, R=3)
      annotation (Placement(transformation(
          origin={50,-10},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent3(I=50) annotation (Placement(
          transformation(extent={{70,-90},{90,-70}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent1(I=55) annotation (Placement(
          transformation(extent={{-70,-40},{-90,-20}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent2(I=45)
      annotation (Placement(transformation(extent={{-70,-90},{-90,-70}})));
    PowerSystems.Generic.FixedCurrent
                         fixedCurrent4(I=60) annotation (Placement(
          transformation(extent={{70,-40},{90,-20}})));
    PowerSystems.Generic.Transformer transformer1(ratio=10/10.4156)
      annotation (Placement(transformation(
          origin={-50,30},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Transformer transformer2(ratio=10/10)
      annotation (Placement(transformation(
          origin={50,30},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Generic.Generator generator1 annotation (Placement(
          transformation(extent={{-30,70},{-10,90}})));
    Modelica.Mechanics.Rotational.Components.Inertia inertia1(
      J=1e3,
      w(start=system.w_nom/generator1.pp),
      a(start=0))               annotation (Placement(transformation(extent={{-60,
              70},{-40,90}})));
    PowerSystems.Generic.Generator generator2(pp=8) annotation (Placement(
          transformation(extent={{70,70},{90,90}})));
    Modelica.Mechanics.Rotational.Components.Inertia inertia2(
                                                   J=1e3,
      w(start=system.w_nom/generator2.pp),
      a(start=0))
      annotation (Placement(transformation(extent={{40,70},{60,90}})));
    Modelica.Mechanics.Rotational.Sources.Torque turbine1(useSupport=false)
      annotation (Placement(transformation(extent={{-90,70},{-70,90}})));
    Modelica.Mechanics.Rotational.Sensors.SpeedSensor angularVelocity
      annotation (Placement(transformation(extent={{-50,110},{-70,130}})));
    Modelica.Mechanics.Rotational.Sources.Torque turbine2(useSupport=false)
      annotation (Placement(transformation(extent={{10,70},{30,90}})));
    Modelica.Blocks.Sources.Trapezoid disturbance(
      width=30,
      rising=0,
      falling=0,
      period=60,
      amplitude=2e3,
      offset=2e3)
                 annotation (Placement(transformation(extent={{30,110},{10,
              130}})));
    Modelica.Blocks.Sources.Constant const(k=system.f_nom)
      annotation (Placement(transformation(extent={{-160,70},{-140,90}})));
    Modelica.Blocks.Continuous.LimPID frequencyPowerControl(
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      Td=0,
      yMax=1e5,
      k=1e6/50,
      Ti=10)
      annotation (Placement(transformation(extent={{-120,90},{-100,70}})));
    Modelica.Blocks.Math.Gain frequency(k=1/(2*pi))
      annotation (Placement(transformation(extent={{-80,110},{-100,130}})));
    inner System system(fType=PowerSystems.Types.SystemFrequency.Average)
      annotation (Placement(transformation(extent={{-170,110},{-150,130}})));
    Interfaces.Sender sender1(H=0.5*inertia1.J*inertia1.w^2/1e6, w=generator1.w)
      annotation (Placement(transformation(extent={{-26,100},{-14,112}})));
    Interfaces.Sender sender2(H=0.5*inertia2.J*inertia2.w^2/1e6, w=generator2.w)
      annotation (Placement(transformation(extent={{74,100},{86,112}})));
  initial equation
    if system.dynType == Types.Dynamics.SteadyInitial then
      inertia1.a = 0;
    else
      inertia1.w = system.omega/generator1.pp;
    end if;
    inertia1.phi = system.theta/generator1.pp;

  equation
    connect(impedance1.terminal_n, impedance2.terminal_p)
      annotation (Line(points={{-50,-20},{-50,-40}}, color={0,120,120}));
    connect(impedance2.terminal_n, impedance3.terminal_p) annotation (Line(
          points={{-50,-60},{-50,-80},{-10,-80}}, color={0,120,120}));
    connect(impedance3.terminal_n, impedance4.terminal_n) annotation (Line(
          points={{10,-80},{50,-80},{50,-60}}, color={0,120,120}));
    connect(impedance4.terminal_p, impedance5.terminal_n)
      annotation (Line(points={{50,-40},{50,-20}}, color={0,120,120}));
    connect(impedance3.terminal_n, fixedCurrent3.terminal)
      annotation (Line(points={{10,-80},{70,-80}}, color={0,120,120}));
    connect(fixedCurrent1.terminal, impedance1.terminal_n) annotation (Line(
          points={{-70,-30},{-50,-30},{-50,-20}}, color={0,120,120}));
    connect(fixedCurrent2.terminal, impedance3.terminal_p)
      annotation (Line(points={{-70,-80},{-10,-80}}, color={0,120,120}));
    connect(fixedCurrent4.terminal, impedance5.terminal_n) annotation (Line(
          points={{70,-30},{50,-30},{50,-20}}, color={0,120,120}));
    connect(transformer1.terminal_n, impedance1.terminal_p)
      annotation (Line(points={{-50,20},{-50,0}}, color={0,120,120}));
    connect(transformer2.terminal_n, impedance5.terminal_p)
      annotation (Line(points={{50,20},{50,0}}, color={0,120,120}));
    connect(inertia1.flange_b, generator1.flange)
      annotation (Line(points={{-40,80},{-30,80}}, color={0,0,0}));
    connect(generator1.terminal, transformer1.terminal_p) annotation (Line(points=
           {{-10,80},{-10,60},{-50,60},{-50,40}}, color={0,120,120}));
    connect(inertia2.flange_b, generator2.flange)
      annotation (Line(points={{60,80},{70,80}}, color={0,0,0}));
    connect(generator2.terminal, transformer2.terminal_p) annotation (Line(points=
           {{90,80},{90,60},{50,60},{50,40}}, color={0,120,120}));
    connect(turbine1.flange, inertia1.flange_a)
      annotation (Line(points={{-70,80},{-60,80}}, color={0,0,0}));
    connect(turbine2.flange, inertia2.flange_a)
      annotation (Line(points={{30,80},{40,80}}, color={0,0,0}));
    connect(disturbance.y, turbine2.tau)
      annotation (Line(points={{9,120},{0,120},{0,80},{8,80}}, color={0,0,127}));
    connect(angularVelocity.flange, inertia1.flange_b)
      annotation (Line(points={{-50,120},{-40,120},{-40,80}}, color={0,0,0}));
    connect(frequencyPowerControl.y, turbine1.tau)
      annotation (Line(points={{-99,80},{-92,80}}, color={0,0,127}));
    connect(const.y, frequencyPowerControl.u_s) annotation (Line(
        points={{-139,80},{-122,80}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(angularVelocity.w, frequency.u) annotation (Line(
        points={{-71,120},{-78,120}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(frequency.y, frequencyPowerControl.u_m) annotation (Line(
        points={{-101,120},{-110,120},{-110,92}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(system.receiveFreq, sender1.sendFreq) annotation (Line(
        points={{-168,128},{-168,138},{-20,138},{-20,104.08}},
        color={120,0,120},
        smooth=Smooth.None));
    connect(system.receiveFreq, sender2.sendFreq) annotation (Line(
        points={{-168,128},{-168,138},{80,138},{80,104.08}},
        color={120,0,120},
        smooth=Smooth.None));
    annotation (Documentation(info="<html>
  <p>The fixed voltage sources of NetworkOpened have been replaced with generators.
  Generator1 provides for primary frequency control, while generator2 introduces fluctuations.</p>
  <p>Note the computation of the average sytem frequency in the global system model, basing on senders for
  each generator. This is needed for initialization (see initial equations in
  the text view). The initialization of the simulation corresponds to a black start in the real world.</p>
  <p>The remainder of the PowerFlow library hides this mechanism in the composed generator or plant models.</p>
</html>"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-180,-100},{100,140}}), graphics),
      experiment(StopTime=120));
  end NetworkControlled;
  annotation(Documentation(info="<html><p>The Network examples demonstrate the evolution of
  a simple powerflow calculation from a textbook to a dynamic simulation model with power/frequency control.
  </p></html>"));
end Network;
