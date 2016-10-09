within PowerSystems.Examples;
package Wind "Different wind turbine concepts"
  extends Modelica.Icons.ExamplesPackage;


  model WindTurbine_IG
  "Wind turbine with Induction Generator (asynchronous) -- fixed speed"
     extends Modelica.Icons.Example;
    AC3ph.Machines.Asynchron generator(redeclare record Data =
        Spot.Data.Machines.Asynchron400V_30kVA (
        V_nom=690,
        S_nom=3e6,
        pp=4))
      annotation (Placement(transformation(extent={{-50,-50},{-70,-30}})));
    Common.Thermal.BdCondV bdCond(m=2)
        annotation (Placement(transformation(extent={{-70,-24},{-50,-4}},rotation=
               0)));
    inner System system annotation (Placement(transformation(extent={{-100,80},
              {-80,100}},  rotation=0)));
    AC3ph.Nodes.BusBar busbar        annotation (Placement(
            transformation(extent={{-70,-90},{-50,-70}},
                                                      rotation=0)));
    AC3ph.Sensors.PVImeter meter(phasor=true,
      V_nom(displayUnit="kV") = 1000,
      S_nom(displayUnit="MVA") = 1000000) annotation (Placement(
          transformation(extent={{-50,-90},{-30,-70}}, rotation=0)));
    AC3ph.Lines.PIline line(len=1000, redeclare record Data =
        AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{20,-90},{40,-70}},
                                                                    rotation=0)));
    AC3ph.Sources.InfBus infBus(V_nom=15e3)
      annotation (Placement(transformation(extent={{80,-90},{60,-70}},
                                                                    rotation=0)));
    AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
              extent={{80,-90},{100,-70}},
                                        rotation=0)));
    Modelica.Blocks.Sources.Ramp windSpeed(
      duration=80,
      startTime=10,
    offset=3,
    height=12)
      annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
    Components.Rotor rotor(R=40)
      annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
    PowerSystems.Mechanics.Rotational.Rotor inertia(
      J=300, w_start=314/generator.par.pp)
      annotation (Placement(transformation(extent={{10,40},{30,60}})));
    Modelica.Mechanics.Rotational.Components.IdealGear gear(ratio=generator.par.pp
          /200)
      annotation (Placement(transformation(extent={{-20,40},{0,60}})));
    AC3ph.Transformers.TrafoIdeal trafo(redeclare record Data =
        AC3ph.Transformers.Parameters.TrafoIdeal (V_nom={690,15e3}, S_nom=3e6))
      annotation (Placement(transformation(extent={{-20,-90},{0,-70}})));
  equation
    connect(generator.heat,bdCond. heat) annotation (Line(
        points={{-60,-30},{-60,-24}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(busbar.term, meter.term_p)
      annotation (Line(points={{-60,-80},{-50,-80}}, color={0,110,110}));
    connect(line.term_n,infBus. term) annotation (Line(points={{40,-80},{60,-80}},
            color={0,110,110}));
    connect(infBus.neutral,grd. term)
        annotation (Line(points={{80,-80},{80,-80}}, color={0,0,255}));
    connect(generator.term, busbar.term) annotation (Line(
        points={{-50,-40},{-40,-40},{-40,-60},{-80,-60},{-80,-80},{-60,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(windSpeed.y, rotor.v) annotation (Line(
        points={{-69,50},{-51,50}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(rotor.flange, gear.flange_a) annotation (Line(
        points={{-30,50},{-20,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(gear.flange_b,inertia.flange_a) annotation (Line(
        points={{0,50},{10,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(inertia.flange_b, generator.airgap) annotation (Line(
        points={{30,50},{40,50},{40,30},{-80,30},{-80,-34},{-60,-34}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(meter.term_n, trafo.term_p) annotation (Line(
        points={{-30,-80},{-20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(trafo.term_n, line.term_p) annotation (Line(
        points={{0,-80},{20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    annotation (preferredView="diagram", Diagram(graphics),
      experiment(StopTime=100));
  end WindTurbine_IG;


  model WindTurbine_DFIG
  "Wind turbine with Doubly Fed Induction Generator (asynchronous) -- variable speed"
     extends Modelica.Icons.Example;
  AC3ph.Machines.Asynchron_dfig generator(redeclare record Data =
        Spot.Data.Machines.Asynchron400V_30kVA (
        V_nom=690,
        S_nom=3e6,
        pp=4))
    annotation (Placement(transformation(extent={{-50,-50},{-70,-30}})));
    Common.Thermal.BdCondV bdCond(m=2)
        annotation (Placement(transformation(extent={{-70,-18},{-50,2}}, rotation=
               0)));
    inner System system annotation (Placement(transformation(extent={{-100,80},
              {-80,100}},  rotation=0)));
    AC3ph.Nodes.BusBar busbar        annotation (Placement(
            transformation(extent={{-70,-90},{-50,-70}},
                                                      rotation=0)));
    AC3ph.Sensors.PVImeter meter(phasor=true,
      V_nom(displayUnit="kV") = 1000,
      S_nom(displayUnit="MVA") = 1000000) annotation (Placement(
          transformation(extent={{-50,-90},{-30,-70}}, rotation=0)));
    AC3ph.Lines.PIline line(len=1000, redeclare record Data =
        AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{20,-90},{40,-70}},
                                                                    rotation=0)));
    AC3ph.Sources.InfBus infBus(V_nom=15e3)
      annotation (Placement(transformation(extent={{80,-90},{60,-70}},
                                                                    rotation=0)));
    AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
              extent={{80,-90},{100,-70}},
                                        rotation=0)));
    Modelica.Blocks.Sources.Ramp windSpeed(
      duration=80,
      startTime=10,
    height=12,
    offset=3)
      annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
    Components.Rotor rotor(R=40)
      annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
    PowerSystems.Mechanics.Rotational.Rotor inertia(J=300, w_start=314/generator.par.pp)
      annotation (Placement(transformation(extent={{10,40},{30,60}})));
    Modelica.Mechanics.Rotational.Components.IdealGear gear(ratio=generator.par.pp
        /200)
      annotation (Placement(transformation(extent={{-20,40},{0,60}})));
    AC3ph.Inverters.InverterAverage inverter1
      annotation (Placement(transformation(extent={{-10,-44},{-30,-24}})));
    Common.Thermal.BdCondV bdCond1(m=1)
      annotation (Placement(transformation(extent={{-30,-18},{-10,2}}, rotation=
               0)));
    AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{0,-44},{20,-24}},
                                                                    rotation=0)));
    AC3ph.Inverters.InverterAverage inverter2
      annotation (Placement(transformation(extent={{30,-44},{50,-24}})));
    AC3ph.Inverters.Select select2
      annotation (Placement(transformation(extent={{30,-2},{50,18}})));
    Common.Thermal.BdCondV bdCond2(m=1)
      annotation (Placement(transformation(extent={{30,-18},{50,2}},   rotation=
               0)));
    Modelica.Blocks.Sources.RealExpression vPhasor_set[2](y={0.5 - min(
        windSpeed.y, 15)/20,0.25*windSpeed.y/15})
      annotation (Placement(transformation(extent={{-10,-4},{-30,16}})));
    AC3ph.Transformers.TrafoIdeal trafo(redeclare record Data =
        AC3ph.Transformers.Parameters.TrafoIdeal (V_nom={690,15e3}, S_nom=3e6))
      annotation (Placement(transformation(extent={{-20,-90},{0,-70}})));
  equation
    connect(generator.heat,bdCond. heat) annotation (Line(
        points={{-60,-30},{-60,-18}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(busbar.term, meter.term_p)
      annotation (Line(points={{-60,-80},{-50,-80}}, color={0,110,110}));
    connect(line.term_n,infBus. term) annotation (Line(points={{40,-80},{60,-80}},
            color={0,110,110}));
    connect(infBus.neutral,grd. term)
        annotation (Line(points={{80,-80},{80,-80}},
                                                   color={0,0,255}));
    connect(generator.term, busbar.term) annotation (Line(
        points={{-50,-40},{-40,-40},{-40,-60},{-80,-60},{-80,-80},{-60,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(windSpeed.y, rotor.v) annotation (Line(
        points={{-69,50},{-51,50}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(rotor.flange, gear.flange_a) annotation (Line(
        points={{-30,50},{-20,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(gear.flange_b, inertia.flange_a) annotation (Line(
        points={{0,50},{10,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(inertia.flange_b, generator.airgap) annotation (Line(
        points={{30,50},{40,50},{40,30},{-80,30},{-80,-34},{-60,-34}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(inverter1.heat, bdCond1.heat) annotation (Line(
        points={{-20,-24},{-20,-18}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(inverter1.AC, generator.term_r) annotation (Line(
        points={{-30,-34},{-50,-34}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(generator.phiRotor, inverter1.theta) annotation (Line(
        points={{-70,-30},{-74,-30},{-74,20},{-6,20},{-6,-20},{-14,-20},{-14,
          -24}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(bdCond2.heat,inverter2. heat) annotation (Line(
        points={{40,-18},{40,-24}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(select2.vPhasor_out,inverter2. vPhasor) annotation (Line(
        points={{46,-2},{46,-6},{50,-6},{50,-20},{46,-20},{46,-24}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(select2.theta_out,inverter2. theta) annotation (Line(
        points={{34,-2},{34,-6},{30,-6},{30,-20},{34,-20},{34,-24}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(inverter2.AC, busbar.term) annotation (Line(
        points={{50,-34},{60,-34},{60,-60},{-80,-60},{-80,-80},{-60,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(inverter1.DC, meterDC.term_p) annotation (Line(
        points={{-10,-34},{0,-34}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(meterDC.term_n, inverter2.DC) annotation (Line(
        points={{20,-34},{30,-34}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(meter.term_n, trafo.term_p) annotation (Line(
        points={{-30,-80},{-20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(trafo.term_n, line.term_p) annotation (Line(
        points={{0,-80},{20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(vPhasor_set.y, inverter1.vPhasor) annotation (Line(points={{-31,6},{
            -34,6},{-34,-20},{-26,-20},{-26,-24}}, color={0,0,127}));
    annotation (preferredView="diagram",
      experiment(StopTime=100));
  end WindTurbine_DFIG;


  model WindTurbine_PSGR
  "Wind turbine with Permanent magnet Synchronous Generator and generator side Rectifier"
    extends Modelica.Icons.Example;
    Modelica.Blocks.Sources.Ramp windSpeed(
      duration=80,
      startTime=10,
    height=12,
    offset=3)
      annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
    Components.Rotor rotor(R=40)
      annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
    inner System system annotation (Placement(transformation(extent={{-100,80},
              {-80,100}},  rotation=0)));
  PowerSystems.Mechanics.Rotational.Rotor inertia(J=300, w_start=314/generator.par.pp)
    annotation (Placement(transformation(extent={{10,40},{30,60}})));
    Modelica.Mechanics.Rotational.Components.IdealGear gear(ratio=generator.par.pp
        /200)
      annotation (Placement(transformation(extent={{-20,40},{0,60}})));
    AC3ph.Machines.Synchron_pm generator(redeclare record Data =
        Spot.Data.Machines.Synchron_pm400V_30kVA (
        V_nom=690,
        S_nom=3e6,
        pp=4))
      annotation (Placement(transformation(extent={{-50,-50},{-70,-30}})));
    Common.Thermal.BdCondV bdCond(m=2)
        annotation (Placement(transformation(extent={{-70,-24},{-50,-4}},rotation=
               0)));
    AC3ph.Inverters.RectifierAverage rectifier1(redeclare record Data =
          Semiconductors.Ideal.SCparameter)
      annotation (Placement(transformation(extent={{-12,-50},{-32,-30}})));
    Common.Thermal.BdCondV bdCond1(m=1)
      annotation (Placement(transformation(extent={{-32,-24},{-12,-4}},rotation=
               0)));
    AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{0,-50},{20,-30}},
                                                                    rotation=0)));
    AC3ph.Inverters.InverterAverage inverter2
      annotation (Placement(transformation(extent={{30,-50},{50,-30}})));
    AC3ph.Inverters.Select select2
      annotation (Placement(transformation(extent={{30,-8},{50,12}})));
    Common.Thermal.BdCondV bdCond2(m=1)
      annotation (Placement(transformation(extent={{30,-24},{50,-4}},  rotation=
               0)));
    AC3ph.Sensors.PVImeter meter(phasor=true,
      V_nom(displayUnit="kV") = 1000,
      S_nom(displayUnit="MVA") = 1000000) annotation (Placement(
          transformation(extent={{-50,-90},{-30,-70}}, rotation=0)));
    AC3ph.Lines.PIline line(len=1000, redeclare record Data =
          AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{20,-90},{40,-70}},
                                                                    rotation=0)));
    AC3ph.Sources.InfBus infBus(V_nom=15e3)
      annotation (Placement(transformation(extent={{80,-90},{60,-70}},
                                                                    rotation=0)));
    AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
              extent={{80,-90},{100,-70}},
                                        rotation=0)));
    AC3ph.Nodes.BusBar busbar        annotation (Placement(
            transformation(extent={{-70,-90},{-50,-70}},
                                                      rotation=0)));
    AC3ph.Transformers.TrafoIdeal trafo(redeclare record Data =
        AC3ph.Transformers.Parameters.TrafoIdeal(V_nom={690,15e3}, S_nom=3e6))
      annotation (Placement(transformation(extent={{-20,-90},{0,-70}})));
    AC3ph.Nodes.DefReference reference
      annotation (Placement(transformation(extent={{-54,-50},{-34,-30}})));
  equation
    connect(windSpeed.y, rotor.v) annotation (Line(
        points={{-69,50},{-51,50}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(rotor.flange, gear.flange_a) annotation (Line(
        points={{-30,50},{-20,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(gear.flange_b,inertia.flange_a) annotation (Line(
        points={{0,50},{10,50}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(generator.heat,bdCond.heat) annotation (Line(
        points={{-60,-30},{-60,-24}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(rectifier1.heat, bdCond1.heat) annotation (Line(
        points={{-22,-30},{-22,-24}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(inertia.flange_b, generator.airgap) annotation (Line(
        points={{30,50},{40,50},{40,30},{-80,30},{-80,-34},{-60,-34}},
        color={0,0,0},
        smooth=Smooth.None));
    connect(rectifier1.DC, meterDC.term_p) annotation (Line(
        points={{-12,-40},{0,-40}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(meterDC.term_n, inverter2.DC) annotation (Line(
        points={{20,-40},{30,-40}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(bdCond2.heat, inverter2.heat) annotation (Line(
        points={{40,-24},{40,-30}},
        color={176,0,0},
        smooth=Smooth.None));
    connect(select2.vPhasor_out, inverter2.vPhasor) annotation (Line(
        points={{46,-8},{46,-12},{50,-12},{50,-26},{46,-26},{46,-30}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(select2.theta_out, inverter2.theta) annotation (Line(
        points={{34,-8},{34,-12},{30,-12},{30,-26},{34,-26},{34,-30}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(line.term_n,infBus. term) annotation (Line(points={{40,-80},{60,-80}},
            color={0,110,110}));
    connect(infBus.neutral,grd. term)
        annotation (Line(points={{80,-80},{80,-80}}, color={0,0,255}));
    connect(busbar.term, meter.term_p) annotation (Line(
        points={{-60,-80},{-50,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(inverter2.AC, busbar.term) annotation (Line(
        points={{50,-40},{60,-40},{60,-60},{-80,-60},{-80,-80},{-60,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(meter.term_n, trafo.term_p) annotation (Line(
        points={{-30,-80},{-20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(trafo.term_n, line.term_p) annotation (Line(
        points={{0,-80},{20,-80}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(generator.term, reference.term) annotation (Line(
      points={{-50,-40},{-44,-40}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(reference.term, rectifier1.AC) annotation (Line(
      points={{-44,-40},{-32,-40}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(generator.phiRotor, reference.theta) annotation (Line(
      points={{-70,-30},{-72,-30},{-72,-26},{-44,-26},{-44,-30}},
      color={0,0,127},
      smooth=Smooth.None));
    annotation (preferredView="diagram", Diagram(coordinateSystem(
          preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
                                                 graphics),
      experiment(StopTime=100));
  end WindTurbine_PSGR;


  model WindTurbine_PSGI
    "Wind turbine with Permanent magnet Synchronous Generator and generator side Inverter"
    extends Modelica.Icons.Example;
    Modelica.Blocks.Sources.Ramp windSpeed(
      duration=80,
      startTime=10,
    height=12,
    offset=3)
      annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
    Components.Rotor rotor(R=40)
      annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
    inner System system annotation (Placement(transformation(extent={{-100,80},
            {-80,100}},  rotation=0)));
    PowerSystems.Mechanics.Rotational.Rotor inertia(J=300, w_start=314/generator.par.pp)
      annotation (Placement(transformation(extent={{10,40},{30,60}})));
    Modelica.Mechanics.Rotational.Components.IdealGear gear(ratio=generator.par.pp
        /200)
      annotation (Placement(transformation(extent={{-20,40},{0,60}})));
    AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{0,-50},{20,-30}},
                                                                  rotation=0)));
    AC3ph.Machines.Synchron_pm_ctrl generator(redeclare record Data =
        Spot.Data.Machines.Synchron_pm400V_30kVA (
        V_nom=690,
        S_nom=3e6,
        pp=4))
      annotation (Placement(transformation(extent={{-50,-50},{-70,-30}})));
    Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-70,-24},{-50,-4}},rotation=
             0)));
    AC3ph.Inverters.InverterAverage inverter1
      annotation (Placement(transformation(extent={{-12,-50},{-32,-30}})));
    Common.Thermal.BdCondV bdCond1(m=1)
      annotation (Placement(transformation(extent={{-32,-24},{-12,-4}},rotation=
             0)));
    AC3ph.Inverters.InverterAverage inverter2
      annotation (Placement(transformation(extent={{30,-50},{50,-30}})));
    AC3ph.Inverters.Select select2
      annotation (Placement(transformation(extent={{30,-8},{50,12}})));
    Common.Thermal.BdCondV bdCond2(m=1)
      annotation (Placement(transformation(extent={{30,-24},{50,-4}},  rotation=
             0)));
    AC3ph.Sensors.PVImeter meter(
      phasor=true,
      V_nom(displayUnit="kV") = 1000,
      S_nom(displayUnit="MVA") = 1000000) annotation (Placement(transformation(
        extent={{-50,-90},{-30,-70}}, rotation=0)));
    AC3ph.Lines.PIline line(len=1000, redeclare record Data =
        AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{20,-90},{40,-70}},
                                                                  rotation=0)));
    AC3ph.Sources.InfBus infBus(V_nom=15e3)
      annotation (Placement(transformation(extent={{80,-90},{60,-70}},
                                                                  rotation=0)));
    AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{80,-90},{100,-70}}, rotation=0)));
    AC3ph.Nodes.BusBar busbar  annotation (Placement(
          transformation(extent={{-70,-90},{-50,-70}}, rotation=0)));
    AC3ph.Transformers.TrafoIdeal trafo(redeclare record Data =
        AC3ph.Transformers.Parameters.TrafoIdeal(V_nom={690,15e3}, S_nom=3e6))
      annotation (Placement(transformation(extent={{-20,-90},{0,-70}})));
    Modelica.Blocks.Continuous.PI PI(T=0.5, initType=Modelica.Blocks.Types.Init.SteadyState)
      annotation (Placement(transformation(extent={{-44,-6},{-56,6}})));
    Modelica.Blocks.Sources.RealExpression i_q_err(y=(0.75 + min(windSpeed.y,
        15)/12) - inertia.w*generator.par.pp/200)
      annotation (Placement(transformation(extent={{-10,-10},{-30,10}})));
    Modelica.Blocks.Sources.RealExpression i_d_set(y=0)
      annotation (Placement(transformation(extent={{-10,4},{-30,24}})));
  equation
    connect(windSpeed.y, rotor.v) annotation (Line(
      points={{-69,50},{-51,50}},
      color={0,0,127},
      smooth=Smooth.None));
    connect(rotor.flange, gear.flange_a) annotation (Line(
      points={{-30,50},{-20,50}},
      color={0,0,0},
      smooth=Smooth.None));
    connect(gear.flange_b, inertia.flange_a) annotation (Line(
      points={{0,50},{10,50}},
      color={0,0,0},
      smooth=Smooth.None));
    connect(generator.heat,bdCond.heat) annotation (Line(
      points={{-60,-30},{-60,-24}},
      color={176,0,0},
      smooth=Smooth.None));
    connect(generator.term, inverter1.AC) annotation (Line(
      points={{-50,-40},{-32,-40}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(inverter1.heat, bdCond1.heat) annotation (Line(
      points={{-22,-30},{-22,-24}},
      color={176,0,0},
      smooth=Smooth.None));
    connect(inverter1.DC, meterDC.term_p) annotation (Line(
      points={{-12,-40},{0,-40}},
      color={0,0,255},
      smooth=Smooth.None));
    connect(inertia.flange_b, generator.airgap) annotation (Line(
      points={{30,50},{40,50},{40,30},{-80,30},{-80,-34},{-60,-34}},
      color={0,0,0},
      smooth=Smooth.None));
    connect(bdCond2.heat,inverter2. heat) annotation (Line(
      points={{40,-24},{40,-30}},
      color={176,0,0},
      smooth=Smooth.None));
    connect(select2.vPhasor_out,inverter2. vPhasor) annotation (Line(
      points={{46,-8},{46,-12},{50,-12},{50,-26},{46,-26},{46,-30}},
      color={0,0,127},
      smooth=Smooth.None));
    connect(select2.theta_out,inverter2. theta) annotation (Line(
      points={{34,-8},{34,-12},{30,-12},{30,-26},{34,-26},{34,-30}},
      color={0,0,127},
      smooth=Smooth.None));
    connect(line.term_n,infBus. term) annotation (Line(
      points={{40,-80},{60,-80}},
      color={0,110,110}));
    connect(infBus.neutral,grd. term) annotation (Line(
      points={{80,-80},{80,-80}},
      color={0,0,255}));
    connect(meterDC.term_n,inverter2. DC) annotation (Line(
      points={{20,-40},{30,-40}},
      color={0,0,255},
      smooth=Smooth.None));
    connect(busbar.term, meter.term_p) annotation (Line(
      points={{-60,-80},{-50,-80}},
      color={0,120,120},
    smooth=Smooth.None));
    connect(inverter2.AC, busbar.term) annotation (Line(
      points={{50,-40},{60,-40},{60,-60},{-80,-60},{-80,-80},{-60,-80}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(meter.term_n, trafo.term_p) annotation (Line(
      points={{-30,-80},{-20,-80}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(trafo.term_n, line.term_p) annotation (Line(
      points={{0,-80},{20,-80}},
      color={0,120,120},
      smooth=Smooth.None));
    connect(generator.vPhasor, inverter1.vPhasor) annotation (Line(
      points={{-50,-30},{-39,-30},{-28,-30}}, color={0,0,127}));
    connect(generator.phiRotor, inverter1.theta) annotation (Line(points={{-70,-30},
          {-76,-30},{-76,26},{-6,26},{-6,-30},{-16,-30}},   color={0,0,127}));
    connect(PI.y, generator.i_act[2]) annotation (Line(
      points={{-56.6,0},{-70,0},{-70,-26},{-66,-26},{-66,-30.5}},
      color={0,0,127}));
    connect(i_d_set.y, generator.i_act[1]) annotation (Line(
      points={{-31,14},{-50,14},{-70,14},{-70,-26},{-66,-26},{-66,-29.5}},
      color={0,0,127}));
  connect(i_q_err.y, PI.u)
    annotation (Line(points={{-31,0},{-42.8,0}}, color={0,0,127}));
    annotation (preferredView="diagram",
      experiment(StopTime=100));
  end WindTurbine_PSGI;



  model WindFarm "Multiple wind turbines connected together"
    extends Modelica.Icons.Example;
    inner System system annotation (Placement(transformation(extent={{-100,80},
              {-80,100}},  rotation=0)));
    AC3ph.Lines.PIline line(len=500, redeclare record Data =
        AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=15e6))
      annotation (Placement(transformation(extent={{-10,-10},{10,10}},
                                                                    rotation=-90,
          origin={60,-40})));
    AC3ph.Sources.InfBus infBus(V_nom=15e3)
      annotation (Placement(transformation(extent={{80,-80},{60,-60}},
                                                                    rotation=0)));
    AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
              extent={{80,-80},{100,-60}},
                                        rotation=0)));
    Modelica.Blocks.Sources.Ramp windSpeed(
      duration=80,
      startTime=10,
    height=12,
    offset=3)
      annotation (Placement(transformation(extent={{-100,30},{-80,50}})));
    Components.WindTurbine wt1
      annotation (Placement(transformation(extent={{-40,70},{-20,90}})));
    Components.WindTurbine wt2
      annotation (Placement(transformation(extent={{-40,30},{-20,50}})));
    Components.WindTurbine wt3
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    AC3ph.Sensors.PVImeter meter(phasor=true,
      V_nom(displayUnit="kV") = 1000,
      S_nom(displayUnit="MVA") = 1000000) annotation (Placement(
          transformation(extent={{30,-20},{50,0}}, rotation=0)));
    AC3ph.Nodes.BusBar busbar annotation (Placement(
            transformation(extent={{10,-20},{30,0}},  rotation=0)));
    AC3ph.Lines.PIline line1(
    ne=1,
    len=100,
    redeclare record Data = AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{-6,-6},{6,6}},  rotation=0,
          origin={-10,70})));
    AC3ph.Lines.PIline line2(
    ne=1,
    len=100,
    redeclare record Data = AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{-6,-6},{6,6}},  rotation=0,
          origin={-10,30})));
    AC3ph.Lines.PIline line3(
    ne=1,
    len=100,
    redeclare record Data = AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{-6,-6},{6,6}},  rotation=0,
          origin={-10,-10})));
    Components.WindTurbine wt4
      annotation (Placement(transformation(extent={{-40,-50},{-20,-30}})));
    AC3ph.Lines.PIline line4(
    ne=1,
    len=100,
    redeclare record Data = AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{-6,-6},{6,6}},  rotation=0,
          origin={-10,-50})));
    Components.WindTurbine wt5
      annotation (Placement(transformation(extent={{-40,-90},{-20,-70}})));
    AC3ph.Lines.PIline line5(
    ne=1,
    len=100,
    redeclare record Data = AC3ph.Lines.Parameters.Line (V_nom=15e3, S_nom=3e6))
      annotation (Placement(transformation(extent={{-6,-6},{6,6}},  rotation=0,
          origin={-10,-90})));
  equation
    connect(line.term_n,infBus. term) annotation (Line(
      points={{60,-50},{60,-70}},
      color={0,110,110}));
    connect(infBus.neutral,grd. term)
      annotation (Line(points={{80,-70},{80,-70}},color={0,0,255}));
  connect(windSpeed.y, wt1.windSpeed) annotation (Line(
        points={{-79,40},{-60,40},{-60,80},{-40,80}},
        color={0,0,127},
        smooth=Smooth.None));
  connect(windSpeed.y, wt2.windSpeed) annotation (Line(
        points={{-79,40},{-40,40}},
        color={0,0,127},
        smooth=Smooth.None));
  connect(windSpeed.y, wt3.windSpeed) annotation (Line(
        points={{-79,40},{-60,40},{-60,0},{-40,0}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(busbar.term, meter.term_p) annotation (Line(
        points={{20,-10},{30,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(meter.term_n, line.term_p) annotation (Line(
        points={{50,-10},{60,-10},{60,-30}},
        color={0,120,120},
        smooth=Smooth.None));
  connect(wt2.term, line2.term_p) annotation (Line(
        points={{-29,30},{-16,30}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(line2.term_n, busbar.term) annotation (Line(
        points={{-4,30},{20,30},{20,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(line1.term_n, busbar.term) annotation (Line(
        points={{-4,70},{20,70},{20,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(line3.term_n, busbar.term) annotation (Line(
        points={{-4,-10},{20,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(wt3.term, line3.term_p) annotation (Line(
        points={{-29,-10},{-16,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(wt1.term, line1.term_p) annotation (Line(
        points={{-29,70},{-16,70}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(wt4.term, line4.term_p) annotation (Line(
        points={{-29,-50},{-16,-50}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(wt5.term, line5.term_p) annotation (Line(
        points={{-29,-90},{-16,-90}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(windSpeed.y, wt4.windSpeed) annotation (Line(
        points={{-79,40},{-60,40},{-60,-40},{-40,-40}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(windSpeed.y, wt5.windSpeed) annotation (Line(
        points={{-79,40},{-60,40},{-60,-80},{-40,-80}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(line4.term_n, busbar.term) annotation (Line(
        points={{-4,-50},{20,-50},{20,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(line5.term_n, busbar.term) annotation (Line(
        points={{-4,-90},{20,-90},{20,-10}},
        color={0,120,120},
        smooth=Smooth.None));
    annotation (preferredView="diagram",
      experiment(StopTime=100));
  end WindFarm;


  package Components
    extends Modelica.Icons.VariantsPackage;

    model Rotor "Transformation of wind to mechanical power. See S. Heier, Grid Integration of Wind Energy Conversion Systems, John Wiley & Sons Ltd.,
1998"
      parameter Real c1=0.5176;
      parameter Real c2=116;
      parameter Real c3=0.4;
      parameter Real c4=5;
      parameter Real c5=21;
      parameter Real c6=0.0068;
      parameter SI.Angle beta = 0 "Pitch angle of blades (0..20deg)";
      parameter SI.Density rho = 1.29 "Density of air";
      parameter SI.Radius R = 17 "Rotor length (radius)";
      parameter SI.Power P_nom = 1e6 "Nominal power"
        annotation(Dialog(group="Nominal"));
      SI.Area A = pi*R^2 "Area swept by rotor";
      Real Cp "power coefficient";
      Real lambda "tip speed ratio";
      Real lambdai;
      SI.AngularVelocity w = der(flange.phi);
      SIpu.Power P(start = 1);

      Modelica.Mechanics.Rotational.Interfaces.Flange_b flange(tau(start=-P_nom/10))
        annotation (Placement(transformation(extent={{90,-10},{110,10}})));
      Modelica.Blocks.Interfaces.RealInput v(unit="m/s") "wind speed"
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
    equation
      0 = P*P_nom + w*flange.tau;
      P*P_nom = 0.5*rho*A*v^3*Cp;
      Cp=c1*(c2*lambdai-c3*beta-c4)*exp(-c5*lambdai)+c6*lambda;
      lambdai = 1/(lambda+0.08*beta) - 0.035/(beta^3 + 1);
      lambda = w*R/v;
      annotation (Diagram(graphics), Icon(graphics={
            Polygon(
              points={{-6,-6},{-86,-16},{-8,8},{14,86},{6,6},{62,-58},{-6,-6}},
              lineColor={85,85,255},
              smooth=Smooth.None,
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-8,0},{8,-100}},
              lineColor={85,85,255},
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid)}));
    end Rotor;

    model WindTurbine
      inner outer System system;

      Modelica.Blocks.Interfaces.RealInput windSpeed
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    AC3ph.Machines.Asynchron_dfig generator(redeclare record Data =
          Spot.Data.Machines.Asynchron400V_30kVA (
          V_nom=690,
          S_nom=3e6,
          pp=4))
      annotation (Placement(transformation(extent={{-50,-50},{-70,-30}})));
      Common.Thermal.BdCondV bdCond(m=2)
          annotation (Placement(transformation(extent={{-70,-18},{-50,2}}, rotation=
                 0)));
      AC3ph.Nodes.BusBar busbar
        annotation (Placement(
              transformation(extent={{-80,-90},{-60,-70}}, rotation=0)));
      AC3ph.Sensors.Psensor sensor
        annotation (Placement(transformation(extent={{-60,-90},{-40,-70}},
                                                                       rotation=0)));
      Rotor rotor(R=40)
        annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
      PowerSystems.Mechanics.Rotational.Rotor inertia(J=300, w_start=314/generator.par.pp)
        annotation (Placement(transformation(extent={{10,40},{30,60}})));
      Modelica.Mechanics.Rotational.Components.IdealGear gear(ratio=generator.par.pp
          /200)
        annotation (Placement(transformation(extent={{-20,40},{0,60}})));
      AC3ph.Inverters.InverterAverage inverter1
        annotation (Placement(transformation(extent={{-10,-44},{-30,-24}})));
      Common.Thermal.BdCondV bdCond1(m=1)
          annotation (Placement(transformation(extent={{-30,-18},{-10,2}}, rotation=
                 0)));
      AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
        annotation (Placement(transformation(extent={{0,-44},{20,-24}},
                                                                      rotation=0)));
      AC3ph.Inverters.InverterAverage inverter2
        annotation (Placement(transformation(extent={{30,-44},{50,-24}})));
      AC3ph.Inverters.Select select2
        annotation (Placement(transformation(extent={{30,-2},{50,18}})));
      Common.Thermal.BdCondV bdCond2(m=1)
          annotation (Placement(transformation(extent={{30,-18},{50,2}},   rotation=
                 0)));
      AC3ph.Transformers.TrafoIdeal trafo(redeclare record Data =
          AC3ph.Transformers.Parameters.TrafoIdeal(V_nom={690,15e3}, S_nom=3e6))
        annotation (Placement(transformation(extent={{-30,-90},{-10,-70}})));
      AC3ph.Ports.ACdq0_p term
        annotation (Placement(transformation(extent={{0,-110},{20,-90}})));
      Modelica.Blocks.Sources.RealExpression vPhasor_set[2](y={0.5 - min(
          windSpeed, 15)/20,0.25*windSpeed/15})
      annotation (Placement(transformation(extent={{-10,-4},{-30,16}})));
    equation
      connect(generator.heat,bdCond.heat) annotation (Line(
          points={{-60,-30},{-60,-18}},
          color={176,0,0},
          smooth=Smooth.None));
      connect(busbar.term,sensor.term_p)  annotation (Line(points={{-70,-80},{-60,-80}},
                      color={0,110,110}));
      connect(generator.term,busbar.term) annotation (Line(
          points={{-50,-40},{-40,-40},{-40,-60},{-80,-60},{-80,-80},{-70,-80}},
          color={0,120,120},
          smooth=Smooth.None));
      connect(rotor.flange,gear.flange_a) annotation (Line(
          points={{-30,50},{-20,50}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(gear.flange_b,inertia.flange_a) annotation (Line(
          points={{0,50},{10,50}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(inertia.flange_b,generator.airgap) annotation (Line(
          points={{30,50},{40,50},{40,30},{-80,30},{-80,-34},{-60,-34}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(inverter1.heat,bdCond1.heat) annotation (Line(
          points={{-20,-24},{-20,-18}},
          color={176,0,0},
          smooth=Smooth.None));
      connect(inverter1.AC,generator.term_r) annotation (Line(
          points={{-30,-34},{-50,-34}},
          color={0,120,120},
          smooth=Smooth.None));
      connect(generator.phiRotor,inverter1.theta) annotation (Line(
          points={{-70,-30},{-74,-30},{-74,20},{-6,20},{-6,-20},{-14,-20},{-14,-24}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(bdCond2.heat,inverter2.heat) annotation (Line(
          points={{40,-18},{40,-24}},
          color={176,0,0},
          smooth=Smooth.None));
      connect(select2.vPhasor_out,inverter2.vPhasor) annotation (Line(
          points={{46,-2},{46,-6},{50,-6},{50,-20},{46,-20},{46,-24}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(select2.theta_out,inverter2.theta) annotation (Line(
          points={{34,-2},{34,-6},{30,-6},{30,-20},{34,-20},{34,-24}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(inverter2.AC,busbar.term) annotation (Line(
          points={{50,-34},{60,-34},{60,-60},{-80,-60},{-80,-80},{-70,-80}},
          color={0,120,120},
          smooth=Smooth.None));
      connect(inverter1.DC,meterDC.term_p) annotation (Line(
          points={{-10,-34},{0,-34}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(meterDC.term_n,inverter2.DC) annotation (Line(
          points={{20,-34},{30,-34}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(sensor.term_n,trafo.term_p) annotation (Line(
          points={{-40,-80},{-30,-80}},
          color={0,120,120},
          smooth=Smooth.None));
      connect(windSpeed, rotor.v) annotation (Line(
          points={{-100,0},{-88,0},{-88,50},{-51,50}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(trafo.term_n, term) annotation (Line(
          points={{-10,-80},{10,-80},{10,-100}},
          color={0,120,120},
          smooth=Smooth.None));
      connect(vPhasor_set.y, inverter1.vPhasor) annotation (Line(points={{-31,6},
            {-34,6},{-34,-20},{-26,-20},{-26,-24}}, color={0,0,127}));
      annotation (                   Icon(graphics={
            Text(
                extent={{-100,90},{100,130}},
                lineColor={0,0,0},
                textString="%name"),
            Polygon(
              points={{-12,-6},{-60,-18},{-14,8},{-6,92},{0,6},{36,-76},{-12,-6}},
              lineColor={85,85,255},
              smooth=Smooth.None,
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-14,0},{2,-100}},
              lineColor={85,85,255},
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{0,-16},{-10,-4},{0,16},{28,24},{44,6},{34,-4},{0,-16}},
              lineColor={85,85,255},
              smooth=Smooth.None,
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid)}));
    end WindTurbine;

  end Components;


  package Test
    model RotorTest

      Components.Rotor rotor(R=17)
        annotation (Placement(transformation(extent={{-10,0},{10,20}})));
      Modelica.Blocks.Sources.Ramp ramp(
        duration=80,
        startTime=10,
        height=15,
        offset=10)
        annotation (Placement(transformation(extent={{-70,0},{-50,20}})));
      Modelica.Mechanics.Rotational.Sources.ConstantSpeed constantSpeed(w_fixed(
            displayUnit="rpm") = 6.2831853071796, phi(start=0, fixed=true))
        annotation (Placement(transformation(extent={{70,0},{50,20}})));
    equation
      connect(ramp.y, rotor.v) annotation (Line(
          points={{-49,10},{-11,10}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(rotor.flange, constantSpeed.flange) annotation (Line(
          points={{10,10},{50,10}},
          color={0,0,0},
          smooth=Smooth.None));
      annotation (
        Diagram(graphics),
        experiment(StopTime=100));
    end RotorTest;

  end Test;


annotation (preferredView="info", Documentation(info="<html>
<p>This collection of examples was developed within MODRIO WP8.7. It demonstrates different electrical drive train concepts for wind power generation.</p>
<p>A challenge originates from the non-linear relationship between wind speed, rotational speed of the rotor and extracted mechanical power &ndash;
see Components.Rotor whose characteristics is visualized in the following figure: </p>
<p><img src=\"modelica://PowerSystems/Examples/Wind/Resources/Rotor.png\" width=\"500\"/></p>
<p>The rotor speed is typically controlled with power electronics at the Maximum Power Point (MPP) such that the extracted mechanical power is maximized for varying wind speed. See the red dots in the figure. The examples cover different basic concepts. </p>
<p>One approach is to use a synchonous machine together with a frequency converter to control the rotor speed and to decouple the variable rotor speed from the constant grid frequency, see e.g. WindTurbine_PSGI (Permanent magnet Synchronous Generator with Inverter). </p>
<p>Alternatively an asynchronous machine can be connected directly to the grid,
and a frequency converter is used to provide a bidirectional second feed to the rotor windings. See WindTurbine_DFIG (Doubly Fed Induction Generator).
This way the power electronics can be smaller as the second feed only amounts to 20-30&percnt; of the overall turbine power. </p>
<p><img src=\"modelica://PowerSystems/Examples/Wind/Resources/WindTurbine_DFIG.png\"/></p>
</html>"));
end Wind;
