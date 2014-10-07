within PowerSystems.Examples.Spot;
package GenerationAC3ph "AC power generation, dqo"
  extends Modelica.Icons.ExamplesPackage;

model Vsource "Power transfer from voltage source to slack bus"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Sources.VsourceRX Vsource(
    V_nom=20e3,
    v_ini=1.02,
    S_nom=500e6,
    pq_ini={1,0.428},
      alpha_ini=0.037545522868902)
    annotation (Placement(transformation(extent={{-70,0},{-50,20}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{-30,0},{-10,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(
    len=40e3, par(V_nom=20e3, S_nom=500e6),
      stIni_en=false)
    annotation (Placement(transformation(extent={{10,0},{30,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
    annotation (Placement(transformation(extent={{70,0},{50,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-70,0},{-90,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{70,0},{90,20}}, rotation=0)));

equation
  connect(Vsource.term, sensor.term_p)   annotation (Line(points={{-50,10},{-30,
            10}}, color={0,110,110}));
  connect(sensor.term_n, line.term_p)   annotation (Line(points={{-10,10},{10,
            10}}, color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{30,10},{50,10}},
          color={0,110,110}));
  connect(grd1.term, Vsource.neutral)
      annotation (Line(points={{-70,10},{-70,10}}, color={0,0,255}));
  connect(infBus.neutral, grd2.term)
      annotation (Line(points={{70,10},{70,10}}, color={0,0,255}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>Amplitude and phase of voltage are given in both nodes.<br>
The powerflow depends essentially on the phase difference between the nodes and also on the voltage amplitudes.</p>
<p><i>See for example:</i>
<pre>
 sensor.p[1:2]    active and reactive power
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
end Vsource;

model PVsource "Power transfer from power-voltage source to slack bus"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Sources.PVsource PVsource(
    V_nom=20e3,
    S_nom=500e6,
    v0=1.02,
    p0=1)
    annotation (Placement(transformation(extent={{-70,0},{-50,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(par(
    V_nom=20e3,
    S_nom=500e6),
    len=40e3)
    annotation (Placement(transformation(extent={{10,0},{30,20}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{-30,0},{-10,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
    annotation (Placement(transformation(extent={{70,0},{50,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-70,0},{-90,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{70,0},{90,20}}, rotation=0)));

equation
  connect(PVsource.term, sensor.term_p)   annotation (Line(points={{-50,10},{
            -30,10}}, color={0,110,110}));
  connect(sensor.term_n, line.term_p)   annotation (Line(points={{-10,10},{10,
            10}}, color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{30,10},{50,10}},
          color={0,110,110}));
  connect(grd1.term, PVsource.neutral)
      annotation (Line(points={{-70,10},{-70,10}}, color={0,0,255}));
  connect(infBus.neutral, grd2.term)
      annotation (Line(points={{70,10},{70,10}}, color={0,0,255}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>Power and voltage-amplitude are given in the source node, whereas the slackBus is identical to the previous example.<br>
The active powerflow is now directly determined through a parameter instead of indirectly depending on the voltage phase-angle.</p>
<p><i>See for example:</i>
<pre>
 sensor.p[1:2]    active and reactive power
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
end PVsource;

model PQsource "Power transfer from power source to slack bus"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Sources.PQsource PQsource(
    S_nom=500e6,
    V_nom=20e3,
    pq0={1,0.428})
    annotation (Placement(transformation(extent={{-70,0},{-50,20}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{-30,0},{-10,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(par(
    V_nom=20e3,
    S_nom=500e6),
    len=40e3)
    annotation (Placement(transformation(extent={{10,0},{30,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
    annotation (Placement(transformation(extent={{70,0},{50,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-70,0},{-90,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{70,0},{90,20}}, rotation=0)));

equation
  connect(PQsource.term, sensor.term_p)   annotation (Line(points={{-50,10},{
            -30,10}}, color={0,110,110}));
  connect(sensor.term_n, line.term_p)   annotation (Line(points={{-10,10},{10,
            10}}, color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{30,10},{50,10}},
          color={0,110,110}));
  connect(grd1.term, PQsource.neutral)
      annotation (Line(points={{-70,10},{-70,10}}, color={0,0,255}));
  connect(infBus.neutral, grd2.term)
      annotation (Line(points={{70,10},{70,10}}, color={0,0,255}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>Active and reactive power are given in the source node, whereas the slackBus is identical to the previous example.<br>
Both active and reactive powerflow are now directly determined through a parameter.</p>
<p><i>See for example:</i>
<pre>
 sensor.p[1:2]    active and reactive power
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
end PQsource;

  model PowerAngle "Generator at fixed power angle"

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}}, rotation=0)));
    PowerSystems.AC3ph.Machines.Control.PowerAngle powerAngle(delta=
          0.78539816339745)
          annotation (Placement(transformation(extent={{-80,0},{-60,20}},
            rotation=0)));
    PowerSystems.Control.Exciters.ExciterSimple exciter(v_f=2.258)
               annotation (Placement(transformation(extent={{-30,70},{-50,90}},
            rotation=0)));
    PowerSystems.AC3ph.Machines.Control.Excitation excitation(V_nom=20e3, Vf_nom=20e3)
                                            annotation (Placement(
          transformation(extent={{-30,40},{-50,60}}, rotation=0)));
    PowerSystems.AC3ph.Machines.Synchron3rd_ee generator(par=synData)
         annotation (Placement(transformation(extent={{-30,0},{-50,20}},
            rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor
      annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,0},{70,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.Voltage Vgen1(
      V_nom=20e3,
      v0=2.258,
      alpha0=0.78539816339745)
      annotation (Placement(transformation(extent={{-80,-80},{-60,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Impedances.Inductor RLgen1(
      V_nom=20e3,
      S_nom=500e6,
      r=0.01,
      x_m=-0.5,
      x_s=1.1)
         annotation (Placement(transformation(extent={{-50,-80},{-30,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor1
      annotation (Placement(transformation(extent={{0,-80},{20,-60}}, rotation=
              0)));
    PowerSystems.AC3ph.Sources.Voltage Vbus1(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,-80},{70,-60}}, rotation=
             0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-80},{-100,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-80},{110,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd3
                                   annotation (Placement(transformation(extent=
              {{90,0},{110,20}}, rotation=0)));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-50,20},{-30,40}}, rotation=
             0)));
    PowerSystems.Examples.Spot.Data.Machines.SynchronIso20kV_500MVA synData
                                          annotation (Placement(transformation(
            extent={{40,80},{80,100}}, rotation=0)));

  equation
    connect(Vgen1.term, RLgen1.term_p) annotation (Line(points={{-60,-70},{-50,
            -70}}, color={0,110,110}));
    connect(RLgen1.term_n, sensor1.term_p) annotation (Line(points={{-30,-70},{
            0,-70}}, color={0,110,110}));
    connect(sensor1.term_n, Vbus1.term) annotation (Line(points={{20,-70},{70,
            -70}}, color={0,110,110}));
    connect(exciter.fieldVoltage, excitation.fieldVoltage) annotation (Line(
          points={{-46,70},{-46,60}}, color={0,0,127}));
    connect(excitation.termVoltage, exciter.termVoltage) annotation (Line(
          points={{-34,60},{-34,70}}, color={0,0,127}));
    connect(generator.term, excitation.term)
                                       annotation (Line(points={{-30,10},{-20,
            10},{-20,50},{-30,50}}, color={0,110,110}));
    connect(excitation.field, generator.field)
      annotation (Line(points={{-30,46},{-24,46},{-24,6},{-30,6}}, color={0,0,
            255}));
    connect(generator.term, powerAngle.term)
                                       annotation (Line(points={{-30,10},{-20,
            10},{-20,-10},{-80,-10},{-80,10}}, color={0,110,110}));
    connect(powerAngle.airgap, generator.airgap)
      annotation (Line(points={{-60,16},{-40,16}}, color={0,0,0}));
    connect(generator.term, sensor.term_p)
                                     annotation (Line(points={{-30,10},{0,10}},
          color={0,110,110}));
    connect(sensor.term_n, infBus.term) annotation (Line(points={{20,10},{70,10}},
          color={0,110,110}));
    connect(grd1.term, Vgen1.neutral) annotation (Line(points={{-80,-70},{-80,
            -70}}, color={0,0,255}));
    connect(Vbus1.neutral, grd2.term)
      annotation (Line(points={{90,-70},{90,-70}}, color={0,0,255}));
    connect(infBus.neutral, grd3.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
    connect(generator.heat, bdCond.heat)   annotation (Line(points={{-40,20},{
            -40,20}}, color={176,0,0}));
    annotation (
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Documentation(
              info="<html>
<p>This example is a first step on the way 'from voltage source to generator model'.</p>
<p>Lower part of example (for comparison with and interpretation of upper part):<br>
simplest generator description as a \"voltage behind reactance\".</p>
<p>Upper part: 3-winding generator model, isotrope with x_d = x_q.<br>
The power-angle is artificially fixed. The correspondence is:
<pre>
  V_gen1.alpha0 ~ powerAngle.delta
  V_gen1.v0     ~ exciter.v_f
</pre></p>
<p>In both cases the terminal voltage is fixed by the bus voltage. The results should coincide.</p>
<p><i>See for example:</i>
<pre>
 sensor.p[1:2]    active and reactive power
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end PowerAngle;

  model TurbineGenerator "Turbine with generator"

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}}, rotation=0)));
    parameter SIpu.AngularVelocity speed_ini(unit="1")=1 "initial speed";
    PowerSystems.Mechanics.TurboGroups.SingleMassTG turboGrp(
      final w_ini=speed_ini*2*pi*generator.par.f_nom/generator.par.pp,
      H=20,
      w_nom=2*pi*generator.par.f_nom,
      P_nom=generator.par.S_nom)
                         annotation (Placement(transformation(extent={{-80,0},{
              -60,20}}, rotation=0)));
    PowerSystems.AC3ph.Machines.Synchron3rd_ee generator(par=synData)
      annotation (Placement(transformation(extent={{-30,0},{-50,20}}, rotation=
              0)));
    PowerSystems.Control.Governors.GovernorSimple governor(p=1.0116)
      annotation (Placement(transformation(extent={{-80,30},{-60,50}}, rotation=
             0)));
    PowerSystems.Control.Exciters.ExciterSimple exciter(v_f=2.258)
      annotation (Placement(transformation(extent={{-30,70},{-50,90}}, rotation=
             0)));
    PowerSystems.AC3ph.Machines.Control.Excitation excitation(V_nom=20e3, Vf_nom=20e3)
                                            annotation (Placement(
          transformation(extent={{-30,40},{-50,60}}, rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor
      annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,0},{70,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.Voltage Vgen1(
      V_nom=20e3,
      v0=2.258,
      alpha0=0.78539816339745)
      annotation (Placement(transformation(extent={{-80,-80},{-60,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Impedances.Inductor RLgen1(
      V_nom=20e3,
      S_nom=500e6,
      x_s=1.1,
      x_m=-0.5,
      r=0.01)
         annotation (Placement(transformation(extent={{-50,-80},{-30,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor1
      annotation (Placement(transformation(extent={{0,-80},{20,-60}}, rotation=
              0)));
    PowerSystems.AC3ph.Sources.Voltage VBus1(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,-80},{70,-60}}, rotation=
             0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-80},{-100,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-80},{110,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd3
                                   annotation (Placement(transformation(extent=
              {{90,0},{110,20}}, rotation=0)));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-50,20},{-30,40}}, rotation=
             0)));
    PowerSystems.Examples.Spot.Data.Machines.SynchronIso20kV_500MVA synData
                                          annotation (Placement(transformation(
            extent={{40,80},{80,100}}, rotation=0)));

  equation
    connect(Vgen1.term, RLgen1.term_p) annotation (Line(points={{-60,-70},{-50,
            -70}}, color={0,110,110}));
    connect(RLgen1.term_n, sensor1.term_p) annotation (Line(points={{-30,-70},{
            0,-70}}, color={0,110,110}));
    connect(sensor1.term_n, VBus1.term) annotation (Line(points={{20,-70},{70,
            -70}}, color={0,110,110}));
    connect(exciter.fieldVoltage, excitation.fieldVoltage) annotation (Line(
          points={{-46,70},{-46,60}}, color={0,0,127}));
    connect(excitation.termVoltage, exciter.termVoltage) annotation (Line(
          points={{-34,60},{-34,70}}, color={0,0,127}));
    connect(turboGrp.airgap, generator.airgap)
      annotation (Line(points={{-60,16},{-40,16}}, color={0,0,0}));
    connect(generator.term, excitation.term)
                                       annotation (Line(points={{-30,10},{-20,
            10},{-20,50},{-30,50}}, color={0,110,110}));
    connect(excitation.field, generator.field)
      annotation (Line(points={{-30,46},{-24,46},{-24,6},{-30,6}}, color={0,0,
            255}));
    connect(generator.term, sensor.term_p)
                                     annotation (Line(points={{-30,10},{0,10}},
          color={0,110,110}));
    connect(sensor.term_n, infBus.term) annotation (Line(points={{20,10},{70,10}},
          color={0,110,110}));
    connect(turboGrp.speed, governor.speed) annotation (Line(points={{-76,20},{
            -76,30}}, color={0,0,127}));
    connect(governor.power, turboGrp.power) annotation (Line(points={{-64,30},{
            -64,20}}, color={0,0,127}));
    connect(grd1.term, Vgen1.neutral) annotation (Line(points={{-80,-70},{-80,
            -70}}, color={0,0,255}));
    connect(VBus1.neutral, grd2.term)
      annotation (Line(points={{90,-70},{90,-70}}, color={0,0,255}));
    connect(infBus.neutral, grd3.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
    connect(generator.heat, bdCond.heat)   annotation (Line(points={{-40,20},{
            -40,20}}, color={176,0,0}));
    annotation (
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>Second example 'from voltage source to generator model' with additional turbine.</p>
<p>Lower part of example (for comparison with and interpretation of upper part):<br>
simplest generator description as a \"voltage behind reactance\".</p>
<p>Upper part: 3-winding generator model, isotrope with x_d = x_q.<br>
Instead of a fixed power-angle as in the previous example, a turbine delivers the appropriate power. The correspondence is still:
<pre>
  V_gen1.alpha0 ~ generator.powerAngle (mod 2pi)
  V_gen1.v0     ~ exciter.v_f
</pre></p>
<p>In both cases the terminal voltage is fixed by the bus voltage. The results should coincide.</p>
<p><i>See and compare for example:</i>
<pre>
 sensor.p[1:2]      active and reactive power
 V_gen1.alpha0 with gen.powerAngle.
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
  end TurbineGenerator;

  model TurbineGeneratorLine "Turbine with generator and line"

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}}, rotation=0)));
    parameter SIpu.AngularVelocity speed_ini(unit="1")=1 "initial speed";
    PowerSystems.Mechanics.TurboGroups.SingleMassTG turboGrp(
      final w_ini=speed_ini*2*pi*generator.par.f_nom/generator.par.pp,
      H=20,
      w_nom=2*pi*generator.par.f_nom,
      P_nom=generator.par.S_nom)
                         annotation (Placement(transformation(extent={{-80,0},{
              -60,20}}, rotation=0)));
    PowerSystems.AC3ph.Machines.Synchron3rd_ee generator(par=synData)
      annotation (Placement(transformation(extent={{-30,0},{-50,20}}, rotation=
              0)));
    PowerSystems.Control.Governors.GovernorSimple governor(p=1.0094)
      annotation (Placement(transformation(extent={{-80,30},{-60,50}}, rotation=
             0)));
    PowerSystems.Control.Exciters.ExciterSimple exciter(v_f=2.245)
      annotation (Placement(transformation(extent={{-30,80},{-50,100}},
            rotation=0)));
    PowerSystems.AC3ph.Machines.Control.Excitation excitation(V_nom=20e3, Vf_nom=20e3)
                                            annotation (Placement(
          transformation(extent={{-30,50},{-50,70}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.BusBar busbar        annotation (Placement(
          transformation(extent={{-20,0},{0,20}}, rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor
      annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
    PowerSystems.AC3ph.Lines.RXline line(
              par(
        V_nom=20e3,
        r=0.02e-3,
        x=0.2e-3),
      stIni_en=false,
      len=400e3)
      annotation (Placement(transformation(extent={{30,0},{50,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,0},{70,20}}, rotation=0)));
    PowerSystems.AC3ph.Sources.Voltage Vgen1(
      V_nom=20e3,
      v0=2.245,
      alpha0=1.0285225282003)
      annotation (Placement(transformation(extent={{-80,-80},{-60,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Impedances.Inductor RLgen1(
      V_nom=20e3,
      S_nom=500e6,
      x_s=1.1,
      x_m=-0.5,
      r=0.01)
         annotation (Placement(transformation(extent={{-50,-80},{-30,-60}},
            rotation=0)));
    PowerSystems.AC3ph.Nodes.BusBar busbar1        annotation (Placement(
          transformation(extent={{-20,-80},{0,-60}}, rotation=0)));
    PowerSystems.AC3ph.Sensors.Psensor sensor1
      annotation (Placement(transformation(extent={{0,-80},{20,-60}}, rotation=
              0)));
    PowerSystems.AC3ph.Lines.RXline line1(
              par(
        V_nom=20e3,
        r=0.02e-3,
        x=0.2e-3),
      stIni_en=false,
      len=400e3)                            annotation (Placement(transformation(
            extent={{30,-80},{50,-60}}, rotation=0)));
    PowerSystems.AC3ph.Sources.Voltage VBus1(V_nom=20e3)
      annotation (Placement(transformation(extent={{90,-80},{70,-60}}, rotation=
             0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-80},{-100,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-80},{110,-60}}, rotation=0)));
    PowerSystems.AC3ph.Nodes.GroundOne grd3
                                   annotation (Placement(transformation(extent=
              {{90,0},{110,20}}, rotation=0)));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-50,20},{-30,40}}, rotation=
             0)));
    PowerSystems.Examples.Spot.Data.Machines.SynchronIso20kV_500MVA synData
                                          annotation (Placement(transformation(
            extent={{40,80},{80,100}}, rotation=0)));

  equation
    connect(Vgen1.term, RLgen1.term_p) annotation (Line(points={{-60,-70},{-50,
            -70}}, color={0,110,110}));
    connect(RLgen1.term_n, busbar1.term)
                                        annotation (Line(points={{-30,-70},{-10,
            -70}}, color={0,110,110}));
    connect(busbar1.term, sensor1.term_p)
                                         annotation (Line(points={{-10,-70},{0,
            -70}}, color={0,110,110}));
    connect(sensor1.term_n, line1.term_p) annotation (Line(points={{20,-70},{30,
            -70}}, color={0,110,110}));
    connect(line1.term_n, VBus1.term) annotation (Line(points={{50,-70},{70,-70}},
          color={0,110,110}));
    connect(exciter.fieldVoltage, excitation.fieldVoltage) annotation (Line(
          points={{-46,80},{-46,70}}, color={0,0,127}));
    connect(excitation.termVoltage, exciter.termVoltage) annotation (Line(
          points={{-34,70},{-34,80}}, color={0,0,127}));
    connect(turboGrp.airgap, generator.airgap)
      annotation (Line(points={{-60,16},{-40,16}}, color={0,0,0}));
    connect(generator.term, excitation.term)
                                       annotation (Line(points={{-30,10},{-20,
            10},{-20,60},{-30,60}}, color={0,110,110}));
    connect(excitation.field, generator.field)
      annotation (Line(points={{-30,56},{-24,56},{-24,6},{-30,6}}, color={0,0,
            255}));
    connect(generator.term, busbar.term)
                                  annotation (Line(points={{-30,10},{-10,10}},
          color={0,110,110}));
    connect(busbar.term, sensor.term_p)
                                       annotation (Line(points={{-10,10},{0,10}},
          color={0,110,110}));
    connect(sensor.term_n, line.term_p) annotation (Line(points={{20,10},{30,10}},
          color={0,110,110}));
    connect(line.term_n, infBus.term) annotation (Line(points={{50,10},{70,10}},
          color={0,110,110}));
    connect(turboGrp.speed, governor.speed) annotation (Line(points={{-76,20},{
            -76,30}}, color={0,0,127}));
    connect(governor.power, turboGrp.power) annotation (Line(points={{-64,30},{
            -64,20}}, color={0,0,127}));
    connect(grd1.term, Vgen1.neutral) annotation (Line(points={{-80,-70},{-80,
            -70}}, color={0,0,255}));
    connect(VBus1.neutral, grd2.term)
      annotation (Line(points={{90,-70},{90,-70}}, color={0,0,255}));
    connect(infBus.neutral, grd3.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
    connect(generator.heat, bdCond.heat)   annotation (Line(points={{-40,20},{
            -40,20}}, color={176,0,0}));
    annotation (
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The third example is obtained from the previous by adding a line between generator and infinite bus.</p>
<p>The terminal voltage depends on the line-properties. Now the correspondence is:
<pre>
  V_gen1.alpha0 - bus1.alpha_v ~ generator.powerAngle
  V_gen1.v0                    ~ exciter.v_f
</pre></p>
<p><i>See and compare for example:</i>
<pre>
 sensor.p[1:2]      active and reactive power
 V_gen1.alpha0 and busbar1.alpha_v with gen.powerAngle.
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
  end TurbineGeneratorLine;

model TurboGeneratorLine "Turbo-generator with line to infinite bus"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen(
    H=20,
      generator(par=synData),
      v_ini=1.12762,
      p_ini=1,
      q_ini=0.42729,
      alpha_ini=0.346481,
      redeclare Control.Exciters.ExciterConst exciter "constant")
    annotation (Placement(transformation(extent={{-60,0},{-40,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.BusBar busbar        annotation (Placement(
          transformation(extent={{-20,0},{0,20}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{0,0},{20,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(
            par(
      V_nom=20e3,
      r=0.02e-3,
      x=0.2e-3),
      stIni_en=false,
      len=400e3)
    annotation (Placement(transformation(extent={{30,0},{50,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
    annotation (Placement(transformation(extent={{90,0},{70,20}}, rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts
                                     annotation (Placement(transformation(
            extent={{-80,0},{-60,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.Voltage Vgen1(
    V_nom=20e3,
    v0=2.245,
      alpha0=1.0285225282003)
    annotation (Placement(transformation(extent={{-80,-60},{-60,-40}}, rotation=
             0)));
  PowerSystems.AC3ph.Impedances.Inductor RLgen1(
    V_nom=20e3,
    S_nom=500e6,
    x_s=1.1,
    x_m=-0.5,
    r=0.01)
       annotation (Placement(transformation(extent={{-50,-60},{-30,-40}},
            rotation=0)));
  PowerSystems.AC3ph.Nodes.BusBar busbar1        annotation (Placement(
          transformation(extent={{-20,-60},{0,-40}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor1
    annotation (Placement(transformation(extent={{0,-60},{20,-40}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line1(
            par(
      V_nom=20e3,
      r=0.02e-3,
      x=0.2e-3),
      stIni_en=false,
      len=400e3)                          annotation (Placement(transformation(
            extent={{30,-60},{50,-40}}, rotation=0)));
  PowerSystems.AC3ph.Sources.Voltage VBus1(V_nom=20e3)
    annotation (Placement(transformation(extent={{90,-60},{70,-40}}, rotation=0)));
  PowerSystems.Examples.Spot.Data.Machines.SynchronIso20kV_500MVA synData
                                          annotation (Placement(transformation(
            extent={{40,80},{80,100}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-60},{-100,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-60},{110,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{90,0},{110,20}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-60,20},{-40,40}}, rotation=
             0)));

equation
  connect(setpts.setpts, turboGen.setpts)   annotation (Line(points={{-60,10},{
            -60,10}}, color={0,0,127}));
  connect(Vgen1.term, RLgen1.term_p)   annotation (Line(points={{-60,-50},{-50,
            -50}}, color={0,110,110}));
  connect(RLgen1.term_n, busbar1.term)  annotation (Line(points={{-30,-50},{-10,
            -50}}, color={0,110,110}));
  connect(busbar1.term, sensor1.term_p)  annotation (Line(points={{-10,-50},{0,
            -50}}, color={0,110,110}));
  connect(sensor1.term_n, line1.term_p)   annotation (Line(points={{20,-50},{30,
            -50}}, color={0,110,110}));
  connect(line1.term_n, VBus1.term)   annotation (Line(points={{50,-50},{70,-50}},
          color={0,110,110}));
  connect(turboGen.term, busbar.term)  annotation (Line(points={{-40,10},{-10,
            10}}, color={0,110,110}));
  connect(busbar.term, sensor.term_p)  annotation (Line(points={{-10,10},{0,10}},
          color={0,110,110}));
  connect(sensor.term_n, line.term_p)   annotation (Line(points={{20,10},{30,10}},
          color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{50,10},{70,10}},
          color={0,110,110}));
  connect(grd1.term, Vgen1.neutral) annotation (Line(points={{-80,-50},{-80,-50}},
          color={0,0,255}));
  connect(VBus1.neutral, grd2.term)
      annotation (Line(points={{90,-50},{90,-50}}, color={0,0,255}));
  connect(infBus.neutral, grd.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
  connect(turboGen.heat, bdCond.heat)   annotation (Line(points={{-50,20},{-50,
            20}}, color={176,0,0}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>This example is the last step on the way 'from voltage source to generator model'.</p>
<p>Turbine and generator are packed into one single model. The terminal voltage depends on the line-properties. The correspondence is:
<pre>
  V_gen1.alpha0 - bus1.alpha_v ~ turboGen.generator.powerAngle
  V_gen1.v0                    ~ exciter.v_f
</pre></p>
<p><i>See and compare for example:</i>
<pre>
 sensor.p[1:2]      active and reactive power
 V_gen1.alpha0 and busbar1.alpha_v with gen.powerAngle.
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment,
      experimentSetupOutput);
end TurboGeneratorLine;

model GenOrder3and7 "Generator-models of different order"

  inner PowerSystems.System system(f_nom=60)
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen1(
    dispPA=true,
    v_ini=1.03,
    p_ini=0.97,
    H=20,
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
      redeclare PowerSystems.AC3ph.Machines.Synchron3rd_ee generator(par=
            syn3rd60Hz26k_720M) "3rd order",
      alpha_ini=0.5235987755983)
    annotation (Placement(transformation(extent={{-80,40},{-60,60}}, rotation=0)));
  PowerSystems.AC3ph.Breakers.Switch switchGrd1(V_nom=26e3,I_nom=30e6)
    annotation (Placement(transformation(extent={{-20,30},{0,10}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.Ground grd1f
    annotation (Placement(transformation(extent={{20,10},{40,30}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line1(par(
    V_nom=26e3,
    f_nom=60,
    r=0.02e-3,
    x=0.3e-3), len=250e3)
            annotation (Placement(transformation(extent={{20,40},{40,60}},
            rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=26e3)
    annotation (Placement(transformation(extent={{80,40},{60,60}}, rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen2(
    dispPA=true,
    v_ini=1.03,
    p_ini=0.97,
    H=20,
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
      redeclare PowerSystems.AC3ph.Machines.Synchron_ee generator(par=syn60Hz26k_720M)
        "nth order",
      alpha_ini=0.5235987755983)
    annotation (Placement(transformation(extent={{-80,-60},{-60,-40}}, rotation=
             0)));
  PowerSystems.AC3ph.Breakers.Switch switchGrd2(V_nom=26e3,I_nom=30e6)
    annotation (Placement(transformation(extent={{-20,-90},{0,-70}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.Ground grd2f
    annotation (Placement(transformation(extent={{20,-90},{40,-70}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line2(par(
    V_nom=26e3,
    f_nom=60,
    r=0.02e-3,
    x=0.3e-3), len=250e3)
            annotation (Placement(transformation(extent={{20,-60},{40,-40}},
            rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=26e3)
    annotation (Placement(transformation(extent={{80,-60},{60,-40}}, rotation=0)));
  PowerSystems.Control.Relays.SwitchRelay relayGrd(
    ini_state=false,
    t_switch={0.1,0.3})
              annotation (Placement(transformation(extent={{20,-20},{0,0}},
            rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts1
                                     annotation (Placement(transformation(
            extent={{-100,40},{-80,60}}, rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts2
                                     annotation (Placement(transformation(
            extent={{-100,-60},{-80,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{80,-60},{100,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{80,40},{100,60}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond1(m=2)
      annotation (Placement(transformation(extent={{-80,60},{-60,80}}, rotation=
             0)));
  PowerSystems.Common.Thermal.BdCondV bdCond2(m=2)
      annotation (Placement(transformation(extent={{-80,-40},{-60,-20}},
            rotation=0)));
  PowerSystems.Examples.Spot.Data.Machines.Synchron3rd_ee60Hz_26kV_720MVA syn3rd60Hz26k_720M
      annotation (Placement(transformation(extent={{-40,80},{0,100}}, rotation=
              0)));
  PowerSystems.Examples.Spot.Data.Machines.Synchron_ee60Hz_26kV_720MVA syn60Hz26k_720M
      annotation (Placement(transformation(extent={{20,80},{60,100}}, rotation=
              0)));

equation
  connect(setpts1.setpts, turboGen1.setpts)   annotation (Line(points={{-80,50},
            {-80,50}}, color={0,0,127}));
  connect(setpts2.setpts, turboGen2.setpts)   annotation (Line(points={{-80,-50},
            {-80,-50}}, color={0,0,127}));
  connect(relayGrd.y, switchGrd1.control) annotation (Line(points={{0,-10},{-10,
            -10},{-10,10}}, color={255,0,255}));
  connect(relayGrd.y, switchGrd2.control) annotation (Line(points={{0,-10},{-10,
            -10},{-10,-70}}, color={255,0,255}));
  connect(turboGen1.term, line1.term_p)   annotation (Line(points={{-60,50},{20,
            50}}, color={0,110,110}));
  connect(line1.term_n, infBus1.term)   annotation (Line(points={{40,50},{60,50}},
          color={0,110,110}));
  connect(turboGen1.term, switchGrd1.term_p)   annotation (Line(points={{-60,50},
            {-40,50},{-40,20},{-20,20}}, color={0,110,110}));
  connect(switchGrd1.term_n, grd1f.term)
      annotation (Line(points={{0,20},{20,20}}, color={0,110,110}));
  connect(turboGen2.term, line2.term_p)   annotation (Line(points={{-60,-50},{
            20,-50}}, color={0,110,110}));
  connect(line2.term_n, infBus2.term)   annotation (Line(points={{40,-50},{60,
            -50}}, color={0,110,110}));
  connect(turboGen2.term, switchGrd2.term_p)   annotation (Line(points={{-60,
            -50},{-40,-50},{-40,-80},{-20,-80}}, color={0,110,110}));
  connect(switchGrd2.term_n, grd2f.term)  annotation (Line(points={{0,-80},{20,
            -80}}, color={0,110,110}));
  connect(infBus1.neutral, grd1.term)
      annotation (Line(points={{80,50},{80,50}}, color={0,0,255}));
  connect(infBus2.neutral, grd2.term)
      annotation (Line(points={{80,-50},{80,-50}}, color={0,0,255}));
  connect(turboGen1.heat, bdCond1.heat)   annotation (Line(points={{-70,60},{
            -70,60}}, color={176,0,0}));
  connect(turboGen2.heat, bdCond2.heat)   annotation (Line(points={{-70,-40},{
            -70,-40}}, color={176,0,0}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>The example illustrates the difference in dynamic behaviour between low- and high-order generator models.<br>
A common 3-phase short circuit occurs at 100 msec, cleared after 300 ms.</p>
<p><i>Compare for example:</i>
<pre>
  .generator.i        current
  .generator.tau      torque
</pre>
of <tt>turbGen1</tt> and <tt>turbGen2</tt>.<br>
The high order model exhibits fast damping of torque-oscillations due to the damper windings. See also damper currents i_rd, i_rq (protected).</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment(StopTime=0.5, NumberOfIntervals=1000),
      experimentSetupOutput(derivatives=false));
end GenOrder3and7;

model TurboGroupGenerator
    "Turbogroup with generator, electro-mechanical interaction"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts
                                     annotation (Placement(transformation(
            extent={{-100,0},{-80,20}}, rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGrpGenerator turboGrpGen(
    v_ini=1.0156,
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
      turboGroup(par=turboGrp1200M),
      redeclare PowerSystems.AC3ph.Machines.Synchron_ee generator(par=syn20k_1200M)
        "nth order")
    annotation (Placement(transformation(extent={{-80,0},{-60,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(
    len=40e3, par(
      V_nom=20e3,
      r=0.02e-3,
      x=0.2e-3))
            annotation (Placement(transformation(extent={{20,0},{40,20}},
            rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=20e3)
    annotation (Placement(transformation(extent={{80,0},{60,20}}, rotation=0)));
  PowerSystems.AC3ph.Breakers.ForcedSwitch switchGrd(V_nom=20e3,I_nom=60e3)
    annotation (Placement(transformation(extent={{-20,-30},{0,-10}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.Ground grdf
    annotation (Placement(transformation(extent={{20,-30},{40,-10}}, rotation=0)));
  PowerSystems.Control.Relays.SwitchRelay relayGrd(
    ini_state=false,
    t_switch={0.1,0.3},
    n=1)      annotation (Placement(transformation(extent={{40,40},{20,60}},
            rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{80,0},{100,20}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-80,20},{-60,40}}, rotation=
             0)));
  PowerSystems.Examples.Spot.Data.Machines.Synchron_ee20kV_1200MVA syn20k_1200M
      annotation (Placement(transformation(extent={{20,80},{60,100}}, rotation=
              0)));
  PowerSystems.Examples.Spot.Data.Turbines.SteamTurboGroup1200MW turboGrp1200M
      annotation (Placement(transformation(extent={{-40,80},{0,100}}, rotation=
              0)));

equation
  connect(setpts.setpts, turboGrpGen.setpts) annotation (Line(points={{-80,10},
            {-80,10}}, color={0,0,127}));
  connect(relayGrd.y[1], switchGrd.control[1])   annotation (Line(points={{20,
            50},{-10,50},{-10,-10}}, color={255,0,255}));
  connect(turboGrpGen.term, line.term_p)   annotation (Line(points={{-60,10},{
            20,10}}, color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{40,10},{60,10}},
          color={0,110,110}));
  connect(turboGrpGen.term, switchGrd.term_p)   annotation (Line(points={{-60,
            10},{-40,10},{-40,-20},{-20,-20}}, color={0,110,110}));
  connect(switchGrd.term_n, grdf.term)  annotation (Line(points={{0,-20},{20,
            -20}}, color={0,110,110}));
  connect(infBus.neutral, grd.term)
      annotation (Line(points={{80,10},{80,10}}, color={0,0,255}));
  connect(turboGrpGen.heat, bdCond.heat)   annotation (Line(points={{-70,20},{
            -70,20}}, color={176,0,0}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>The example illustrates the influence of an electric shock on the mechanical behaviour of the turbogroup.<br>
A common 3-phase short circuit occurs at 0.1 sec, cleared after 200 ms.</p>
<p><i>See for example:</i>
<pre>
  turboGrpGen.generator.tau      torque (electric frequency)
  turboGrpGen.turboGroup.delta   relative angles between single turbines (frequencies typical 16 to 23 Hz)
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment(
        StopTime=0.5,
        NumberOfIntervals=1000,
        fixedstepsize=0.001,
        Algorithm=""),
      experimentSetupOutput(derivatives=false, inputs=false));
end TurboGroupGenerator;

model TieLine "Generators and power-oscillations"

  inner PowerSystems.System system(f_nom=60, fType_par=false,
      f_lim={55,65})
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen1(
    dispPA=true,
    alpha_ini=0.316177 + system.alpha0,
    v_ini=1.05,
    H=20,
    redeclare PowerSystems.AC3ph.Machines.Synchron_ee generator(par=syn60Hz26k_720M)
        "nth order",
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
    redeclare PowerSystems.Control.Governors.Governor1st governor "1st order",
      p_ini=0.9,
      q_ini=0.24022361,
      iniType=PowerSystems.Basic.Types.IniType.p_q)
    annotation (Placement(transformation(extent={{-100,-20},{-80,0}}, rotation=
              0)));
  PowerSystems.AC3ph.Nodes.BusBar bus1           annotation (Placement(
          transformation(extent={{-80,-20},{-60,0}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor1(term_p(v(start={20e3,0,0})))
    annotation (Placement(transformation(extent={{-60,-20},{-40,0}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line1(par(
    V_nom=26e3,
    f_nom=60,
    r=0.02e-3,
    x=0.3e-3), len=200e3,
    stIni_en=false)
            annotation (Placement(transformation(extent={{-30,-20},{-10,0}},
            rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen2(
    dispPA=true,
    alpha_ini=0.0144536 + system.alpha0,
    v_ini=1.04,
    H=20,
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
    redeclare PowerSystems.AC3ph.Machines.Synchron_ee generator(par=syn60Hz26k_720M)
        "nth order",
    redeclare PowerSystems.Control.Governors.Governor1st governor "1st order",
      p_ini=0.9,
      q_ini=0.5092972,
      iniType=PowerSystems.Basic.Types.IniType.p_q)
    annotation (Placement(transformation(extent={{100,-20},{80,0}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.BusBar bus2           annotation (Placement(
          transformation(extent={{60,-20},{80,0}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor2(term_p(v(start={20e3,0,0})))
    annotation (Placement(transformation(extent={{60,-20},{40,0}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line2(par(
    V_nom=26e3,
    f_nom=60,
    r=0.02e-3,
    x=0.3e-3), len=40e3,
    stIni_en=false)
            annotation (Placement(transformation(extent={{10,-20},{30,0}},
            rotation=0)));
  PowerSystems.AC3ph.Generation.TurboGenerator turboGen3(
    dispPA=true,
    v_ini=1.03,
    H=20,
    redeclare PowerSystems.Control.Exciters.Exciter1st exciter "1st order",
    redeclare PowerSystems.Control.Governors.Governor1st governor "1st order",
      redeclare PowerSystems.AC3ph.Machines.Synchron_ee generator(par=syn60Hz26k_720M,
          stIni_en=false) "nth order",
      q_ini=0.10292306,
      p_ini=0.27353)
    annotation (Placement(transformation(
          origin={0,70},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.AC3ph.Sensors.Psensor sensor3(term_p(v(start={20e3,0,0})))
    annotation (Placement(transformation(
          origin={0,40},
          extent={{10,-10},{-10,10}},
          rotation=90)));
  PowerSystems.AC3ph.Lines.RXline line3(par(
    V_nom=26e3,
    f_nom=60,
    r=0.02e-3,
    x=0.3e-3), len=150e3,
    stIni_en=false)
    annotation (Placement(transformation(
          origin={0,10},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  PowerSystems.AC3ph.Sensors.Psensor sensorLoad
    annotation (Placement(transformation(
          origin={0,-50},
          extent={{10,-10},{-10,10}},
          rotation=90)));
  PowerSystems.AC3ph.Loads.Zload load(
    p0_set={0.95,0.2},
    V_nom=26e3,
    S_nom=1500e6,
      scType_par=false)
    annotation (Placement(transformation(
          origin={0,-90},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.Blocks.Signals.Transient[2] pq_change(
    each t_duration=0.1,
    s_ini={0.95,0.2},
    s_fin={0.2,0.1},
    each t_change=2)
               annotation (Placement(transformation(extent={{-40,-100},{-20,-80}},
            rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts1
                                     annotation (Placement(transformation(
            extent={{-120,-20},{-100,0}}, rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts2
                                     annotation (Placement(transformation(
            extent={{120,-20},{100,0}}, rotation=0)));
  PowerSystems.Control.Setpoints.Set_w_p_v setpts3
                                     annotation (Placement(transformation(
          origin={0,90},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.Common.Thermal.BdCondV bdCond1(m=2)
      annotation (Placement(transformation(extent={{-100,0},{-80,20}}, rotation=
             0)));
  PowerSystems.Common.Thermal.BdCondV bdCond2(m=2)
      annotation (Placement(transformation(extent={{80,0},{100,20}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond3(m=2)
      annotation (Placement(transformation(
          origin={-20,70},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  PowerSystems.Examples.Spot.Data.Machines.Synchron_ee60Hz_26kV_720MVA syn60Hz26k_720M
      annotation (Placement(transformation(extent={{-60,80},{-20,100}},
            rotation=0)));

equation
  connect(pq_change.y,load. p_set)   annotation (Line(points={{-20,-90},{-10,
            -90}}, color={0,0,127}));
  connect(setpts1.setpts,turboGen1. setpts)   annotation (Line(points={{-100,
            -10},{-100,-10}}, color={0,0,127}));
  connect(setpts2.setpts,turboGen2. setpts)   annotation (Line(points={{100,-10},
            {100,-10}}, color={0,0,127}));
  connect(sensorLoad.term_p,line3. term_p)   annotation (Line(points={{
            -6.12303e-016,-40},{-6.12303e-016,-30},{-6.12303e-016,-30},{
            -6.12303e-016,-20},{-6.12303e-016,0},{-6.12303e-016,0}}, color={0,
            110,110}));
  connect(sensorLoad.term_n,load. term)   annotation (Line(points={{
            6.12303e-016,-60},{1.76911e-022,-70},{6.12303e-016,-70},{
            6.12303e-016,-80}}, color={0,110,110}));
  connect(turboGen1.term,bus1. term)  annotation (Line(points={{-80,-10},{-70,
            -10}}, color={0,110,110}));
  connect(bus1.term,sensor1. term_p)  annotation (Line(points={{-70,-10},{-60,
            -10}}, color={0,110,110}));
  connect(sensor1.term_n,line1. term_p)   annotation (Line(points={{-40,-10},{
            -30,-10}}, color={0,110,110}));
  connect(turboGen2.term,bus2. term)  annotation (Line(points={{80,-10},{70,-10}},
          color={0,110,110}));
  connect(bus2.term,sensor2. term_p)  annotation (Line(points={{70,-10},{60,-10}},
          color={0,110,110}));
  connect(sensor2.term_n,line2. term_n)   annotation (Line(points={{40,-10},{30,
            -10}}, color={0,110,110}));
  connect(line1.term_n,sensorLoad. term_p)   annotation (Line(points={{-10,-10},
            {-6,-10},{-6,-40},{-6.12303e-016,-40}}, color={0,110,110}));
  connect(line2.term_p,sensorLoad. term_p)   annotation (Line(points={{10,-10},
            {6,-10},{6,-40},{-6.12303e-016,-40}}, color={0,110,110}));
  connect(setpts3.setpts,turboGen3. setpts) annotation (Line(points={{
            -6.12303e-016,80},{6.12303e-016,80}}, color={0,0,127}));
  connect(turboGen3.term, sensor3.term_p)   annotation (Line(points={{
            -6.12303e-016,60},{-1.76911e-022,56},{-6.12303e-016,56},{
            -6.12303e-016,50}}, color={0,120,120}));
  connect(sensor3.term_n, line3.term_n)   annotation (Line(points={{
            6.12303e-016,30},{6.12303e-016,27.5},{6.12303e-016,27.5},{
            6.12303e-016,25},{6.12303e-016,20},{6.12303e-016,20}}, color={0,120,
            120}));
  connect(turboGen1.heat, bdCond1.heat)
      annotation (Line(points={{-90,0},{-90,0}}, color={176,0,0}));
  connect(turboGen2.heat, bdCond2.heat)
      annotation (Line(points={{90,0},{90,0}}, color={176,0,0}));
  connect(turboGen3.heat, bdCond3.heat)   annotation (Line(points={{-10,70},{
            -10,70}}, color={176,0,0}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Documentation(
            info="<html>
<p>Tie line with three generators (third as infinite slack bus), feeding a variable load. The load changes abruptly at <tt>t=2</tt> and induces power oscillations between the nodes.<br>
The oscillations are slowly damped (depending on control), but may also lead to system instability.<br>
After the load decreases, system frequency starts to increase from 60 to 62 Hz with an intermediate maximum of 63 Hz. For narrow frequency-bounds 'system.f_lim', for example {58, 62} simulation stops when the limit is reached (after 5 sec).</p>
<p><i>See for example:</i>
<pre>
 sensor's.p[1:2]    active and reactive power
 system.omega       system frequency
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment(StopTime=30),
      experimentSetupOutput);
end TieLine;

model WindGeneratorLine "Asynchronous generator"

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}}, rotation=0)));
  PowerSystems.AC3ph.Generation.WindGenerator windGen(
    WT(par=windTurb), generator(par=asyn400_30k))
    annotation (Placement(transformation(extent={{-60,0},{-40,20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.BusBar busbar        annotation (Placement(
          transformation(extent={{-30,0},{-10,20}}, rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{-10,0},{10,20}}, rotation=0)));
  PowerSystems.AC3ph.Lines.RXline line(par(
    V_nom=400,
    S_nom=30e3,
    r=0.05), len=1.5e3,
      stIni_en=false)
    annotation (Placement(transformation(extent={{30,0},{50,20}}, rotation=0)));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=400)
    annotation (Placement(transformation(extent={{90,0},{70,20}}, rotation=0)));
  PowerSystems.Blocks.Signals.Transient trsSignal1(
    t_change=25,
    t_duration=50,
      s_ini=5,
      s_fin=15)     annotation (Placement(transformation(extent={{-100,0},{-80,
              20}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{90,0},{110,20}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
      annotation (Placement(transformation(extent={{-60,20},{-40,40}}, rotation=
             0)));
  PowerSystems.Examples.Spot.Data.Turbines.WindTurbineGear windTurb
                                           annotation (Placement(transformation(
            extent={{-40,80},{0,100}}, rotation=0)));
  PowerSystems.Examples.Spot.Data.Machines.Asynchron400V_30kVA asyn400_30k
      annotation (Placement(transformation(extent={{20,80},{60,100}}, rotation=
              0)));

equation
  connect(windGen.term, busbar.term)  annotation (Line(points={{-40,10},{-20,10}},
          color={0,110,110}));
  connect(busbar.term, sensor.term_p)  annotation (Line(points={{-20,10},{-10,
            10}}, color={0,110,110}));
  connect(sensor.term_n, line.term_p)   annotation (Line(points={{10,10},{30,10}},
          color={0,110,110}));
  connect(line.term_n, infBus.term)   annotation (Line(points={{50,10},{70,10}},
          color={0,110,110}));
  connect(infBus.neutral, grd.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
  connect(windGen.heat, bdCond.heat)   annotation (Line(points={{-50,20},{-50,
            20}}, color={176,0,0}));
  connect(trsSignal1.y, windGen.windSpeed)   annotation (Line(points={{-80,10},
            {-60,10}}, color={0,0,127}));
  annotation (
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
<p>This example shows an asynchron generator directly coupled to the grid.<br>
The wind-speed is increased from 5 to 15 m/s. The machine remains stable.</p>
<p><i>See for example:</i>
<pre>
  sensor.p[1:2]    active and reactive power
  windGen.generator.slip
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>

"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment(StopTime=60, NumberOfIntervals=1000),
      experimentSetupOutput);
end WindGeneratorLine;

model Islanding "AC synchronous generator in islanding configuration"

  inner PowerSystems.System system
annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=0)));
  parameter SI.Voltage vAC_nom=560 "AC nominal voltage";
  parameter SI.Voltage vDC_nom=750 "DC nominal voltage";
  PowerSystems.Control.Setpoints.Set_w_p setpts   annotation (Placement(
          transformation(extent={{-100,-60},{-80,-40}}, rotation=0)));
  PowerSystems.AC3ph.Generation.TurboPMgenerator turboGen(
                   redeclare PowerSystems.Control.Governors.Governor1st governor
        "1st order",
                   redeclare PowerSystems.AC3ph.Machines.Synchron_pm generator(
      par=synpm560_100k) "nth order")
    annotation (Placement(transformation(extent={{-80,-60},{-60,-40}}, rotation=
             0)));
  PowerSystems.AC3ph.Nodes.DefReference reference
      annotation (Placement(transformation(extent={{-60,-60},{-40,-40}},
            rotation=0)));
  PowerSystems.AC3ph.Sensors.Psensor ACpower  annotation (Placement(transformation(
            extent={{-40,-60},{-20,-40}}, rotation=0)));
  PowerSystems.AC3ph.Inverters.RectifierAverage rectifier(par=idealSC1k_100)
                                          annotation (Placement(transformation(
            extent={{10,-60},{-10,-40}}, rotation=0)));
  PowerSystems.AC1ph_DC.Impedances.CapacitorSym capSym(
    G=1e-4,
    C=0.020,
      Vstart=vDC_nom)                   annotation (Placement(transformation(
            extent={{20,-60},{40,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd    annotation (Placement(transformation(
          origin={30,-80},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.AC1ph_DC.Sensors.VdiffSensor DCvoltage
                                    annotation (Placement(transformation(extent=
             {{20,-20},{40,0}}, rotation=0)));
  PowerSystems.AC1ph_DC.Sensors.Psensor DCpower  annotation (Placement(transformation(
            extent={{50,-60},{70,-40}}, rotation=0)));
  PowerSystems.AC1ph_DC.Loads.ZloadDC zLoadDC(S_nom=100e3,
      scType_par=false,
    V_nom=vDC_nom)
    annotation (Placement(transformation(extent={{80,-60},{100,-40}}, rotation=
              0)));
  PowerSystems.Blocks.Signals.Transient transSig(
    t_duration=2.5,
    s_ini=1,
    s_fin=0.7,
      t_change=10)                       annotation (Placement(transformation(
          origin={90,-20},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  Modelica.Blocks.Continuous.LimPID limPID_DC(
    yMin=0,
    yMax=2,
    xi_start=1,
    Td=0.1,
    Ti=0.5,
    k=1/vDC_nom,
    initType=Modelica.Blocks.Types.InitPID.SteadyState)
         annotation (Placement(transformation(extent={{40,20},{20,40}},
            rotation=0)));
  Modelica.Blocks.Sources.Constant set_vDC(k=vDC_nom)
      annotation (Placement(transformation(extent={{70,20},{50,40}}, rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond1(m=2)
      annotation (Placement(transformation(extent={{-80,-40},{-60,-20}},
            rotation=0)));
  PowerSystems.Common.Thermal.BdCondV bdCond2(m=1)
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}},
            rotation=0)));
  PowerSystems.Examples.Spot.Data.Machines.Synchron_pm560V_100kVA synpm560_100k
      annotation (Placement(transformation(extent={{-60,80},{-20,100}},
            rotation=0)));
  PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC1kV_100A idealSC1k_100
    annotation (Placement(transformation(extent={{0,80},{40,100}}, rotation=0)));

equation
  connect(ACpower.term_n, rectifier.AC) annotation (Line(points={{-20,-50},{-10,
            -50}}, color={0,120,120}));
  connect(capSym.term_n, DCpower.term_p)
      annotation (Line(points={{40,-50},{50,-50}}, color={0,0,255}));
  connect(DCpower.term_n, zLoadDC.term)
      annotation (Line(points={{70,-50},{80,-50}}, color={0,0,255}));
  connect(setpts.setpts, turboGen.setpts) annotation (Line(points={{-80,-50},{
            -80,-50}}, color={0,0,127}));
  connect(set_vDC.y, limPID_DC.u_s)
      annotation (Line(points={{49,30},{42,30}}, color={0,0,127}));
  connect(limPID_DC.y, setpts.setpt_p) annotation (Line(points={{19,30},{-100,
            30},{-100,-50},{-92,-50}}, color={0,0,127}));
  connect(capSym.term_p, DCvoltage.term)
      annotation (Line(points={{20,-50},{20,-10}}, color={0,0,255}));
  connect(rectifier.DC, capSym.term_p)
      annotation (Line(points={{10,-50},{20,-50}}, color={0,0,255}));
  connect(turboGen.heat, bdCond1.heat)   annotation (Line(points={{-70,-40},{
            -70,-40}}, color={176,0,0}));
  connect(rectifier.heat, bdCond2.heat)   annotation (Line(points={{0,-40},{0,
            -40}}, color={176,0,0}));
  connect(transSig.y, zLoadDC.p_set)   annotation (Line(points={{90,-30},{90,
            -40}}, color={0,0,127}));
  connect(capSym.neutral, grd.term)
      annotation (Line(points={{30,-60},{30,-70}}, color={0,0,255}));
  connect(DCvoltage.v, limPID_DC.u_m)
      annotation (Line(points={{30,0},{30,18}}, color={0,0,127}));
  connect(turboGen.term, reference.term)   annotation (Line(points={{-60,-50},{
            -50,-50}}, color={0,120,120}));
  connect(reference.term, ACpower.term_p)   annotation (Line(points={{-50,-50},
            {-40,-50}}, color={0,120,120}));
  connect(turboGen.phiRotor, reference.theta)   annotation (Line(points={{-60,
            -40},{-50,-40}}, color={0,0,127}));
  annotation (
Window(
    x=0.45,
    y=0.01,
    width=0.44,
    height=0.65),
Documentation(
        info="<html>
<p>Permanent magnet excited synchron generator, rotor defines reference system.<br>
The generator is directly coupled to a (passive) rectifier. If an average-version of the rectifier is tolerable, no transforms at all are necessary. The simulation (for linear generator models) is fast. This is of particular importance for high speed machines, because the high frequency drastically slows down
integration in inertial abc-system.</p>
<p><i>See for example:</i>
<pre>
  ACpower.p
  DCpower.p
  DCvoltage.v
</pre>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
experiment(StopTime=30),
experimentSetupOutput);
end Islanding;

public
model LocalGeneration
    "AC torque controlled synchronous machine as local generator"

  inner PowerSystems.System system(f_nom=60)
annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=0)));
  parameter SI.Voltage vDC_nom=780 "DC nominal voltage";
  final parameter SI.Current I_nom=syn3rdpm560_100k.S_nom/syn3rdpm560_100k.V_nom
      "generator nominal current";
  PowerSystems.Blocks.Signals.Transient speedSignal(
    t_duration=5,
    t_change=20,
      s_ini=2*pi*180,
      s_fin=2*pi*150)
    annotation (Placement(transformation(extent={{-100,-60},{-80,-40}},
            rotation=0)));
  PowerSystems.Mechanics.Rotation.Speed speed(scType_par=false, w0=2*pi*
          180)                        annotation (Placement(transformation(
            extent={{-70,-60},{-50,-40}}, rotation=0)));
  PowerSystems.AC3ph.Generation.PMgenerator PMgen(generator(par=syn3rdpm560_100k),
    inverter(par=idealSC1k_100),
      w_ini=1130.9733552923)
    annotation (Placement(transformation(extent={{-40,-60},{-20,-40}}, rotation=
             0)));
  PowerSystems.AC1ph_DC.Impedances.CapacitorSym capSym(
    G=1e-4,
      Vstart=vDC_nom,
    C=0.020)                            annotation (Placement(transformation(
            extent={{0,-60},{20,-40}}, rotation=0)));
  PowerSystems.AC3ph.Nodes.GroundOne grd    annotation (Placement(transformation(
          origin={10,-80},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.AC1ph_DC.Sensors.VdiffSensor DCvoltage
                                    annotation (Placement(transformation(extent=
             {{0,-30},{20,-10}}, rotation=0)));
  PowerSystems.AC1ph_DC.Sensors.Psensor DCpower  annotation (Placement(transformation(
            extent={{30,-60},{50,-40}}, rotation=0)));
  PowerSystems.AC1ph_DC.Loads.ZloadDC zLoadDC(
      scType_par=false,
    V_nom=vDC_nom,
    S_nom=100e3)
    annotation (Placement(transformation(extent={{60,-60},{80,-40}}, rotation=0)));
  PowerSystems.Blocks.Signals.Transient transSig(
    t_duration=2.5,
      t_change=10,
    s_ini=0.5,
    s_fin=0.9)                           annotation (Placement(transformation(
          origin={70,-20},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  PowerSystems.Common.Thermal.BdCondV bdCond(m=3)
      annotation (Placement(transformation(extent={{-40,-40},{-20,-20}},
            rotation=0)));
  PowerSystems.Blocks.Signals.Transient i_d(
    t_duration=5,
    s_fin=0,
    t_change=20,
      s_ini=-0.1)
    annotation (Placement(transformation(extent={{-100,-20},{-80,0}}, rotation=
              0)));
  Modelica.Blocks.Sources.Constant vDC_set(k=vDC_nom)
      annotation (Placement(transformation(extent={{-100,20},{-80,40}},
            rotation=0)));
  Modelica.Blocks.Continuous.LimPID PI_vDC(
      Td=0.05,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      initType=Modelica.Blocks.Types.InitPID.SteadyState,
    Ti=0.1,
      k=0.3*I_nom/vDC_nom,
      yMax=1.4*I_nom)
         annotation (Placement(transformation(extent={{-70,20},{-50,40}},
            rotation=0)));
  Modelica.Blocks.Math.Gain gain(k=-1) "generator: negative torque tau_act"
                                       annotation (Placement(transformation(
            extent={{-40,20},{-20,40}}, rotation=0)));
  parameter PowerSystems.Examples.Spot.Data.Machines.Synchron3rd_pm560V_100kVA syn3rdpm560_100k(
                                                           neu_iso=true)
      annotation (Placement(transformation(extent={{-60,80},{-20,100}},
            rotation=0)));
  parameter PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC1kV_100A idealSC1k_100(
                                                    Vf=0)
    annotation (Placement(transformation(extent={{0,80},{40,100}}, rotation=0)));

equation
  connect(PMgen.heat, bdCond.heat) annotation (Line(points={{-30,-40},{-30,-40}},
          color={176,0,0}));
  connect(PMgen.term, capSym.term_p)
      annotation (Line(points={{-20,-50},{0,-50}}, color={0,0,255}));
  connect(DCpower.term_n, zLoadDC.term)
      annotation (Line(points={{50,-50},{60,-50}}, color={0,0,255}));
  connect(capSym.term_p, DCvoltage.term)
      annotation (Line(points={{0,-50},{0,-20}}, color={0,0,255}));
  connect(transSig.y, zLoadDC.p_set) annotation (Line(points={{70,-30},{70,-40}},
          color={0,0,127}));
  connect(DCvoltage.v, PI_vDC.u_m) annotation (Line(points={{10,-10},{10,0},{
            -60,0},{-60,18}}, color={0,0,127}));
  connect(vDC_set.y, PI_vDC.u_s) annotation (Line(points={{-79,30},{-72,30}},
          color={0,0,127}));
  connect(PI_vDC.y, gain.u) annotation (Line(points={{-49,30},{-42,30}}, color=
            {0,0,127}));
  connect(gain.y, PMgen.i_act[2]) annotation (Line(points={{-19,30},{-10,30},{-10,
          10},{-36,10},{-36,-39.5}},       color={0,0,127}));
  connect(speed.flange, PMgen.flange)
    annotation (Line(points={{-50,-50},{-40,-50}}, color={0,0,0}));
  connect(capSym.term_n, DCpower.term_p)
      annotation (Line(points={{20,-50},{30,-50}}, color={0,0,255}));
  connect(capSym.neutral, grd.term)
      annotation (Line(points={{10,-60},{10,-70}}, color={0,0,255}));
  connect(speedSignal.y, speed.w) annotation (Line(points={{-80,-50},{-70,-50}},
          color={0,0,127}));
  connect(i_d.y, PMgen.i_act[1]) annotation (Line(points={{-80,-10},{-36,-10},{-36,
          -40.5}},       color={0,0,127}));
  annotation (
Window(
    x=0.45,
    y=0.01,
    width=0.44,
    height=0.65),
Documentation(
        info="<html>
<p>Permanent magnet excited synchron generator, defining reference system.<br>
The generator is directly coupled to a (passive) rectifier. If an average-version of the rectifier is tolerable, no transforms at all are necessary.</p>
<p><i>See for example:</i>
<pre>
  DCpower.p
  DCvoltage.v
</pre>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
experiment(StopTime=30),
experimentSetupOutput);
end LocalGeneration;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.42,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Power sources and generation, a set of examples mainly for understanding the synchronous machine.</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end GenerationAC3ph;
