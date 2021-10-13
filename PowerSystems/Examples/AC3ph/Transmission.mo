within PowerSystems.Examples.AC3ph;
package Transmission "AC transmission, dq0"
  extends Modelica.Icons.ExamplesPackage;

model PowerTransfer "Power transfer between two nodes"
  extends Modelica.Icons.Example;

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}})));
  PowerSystems.Blocks.Signals.TransientPhasor transPh(
      t_change=30,
      t_duration=60,
      ph_end=2*pi,
      ph_start=0)
               annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
  PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=130e3, use_vPhasor_in=true)
    annotation (Placement(transformation(extent={{-60,0},{-40,20}})));
  PowerSystems.AC3ph.Lines.RXline line(redeclare record Data =
    PowerSystems.AC3ph.Lines.Parameters.RXline (
    V_nom=130e3,
    S_nom=100e6),
    len=100e3)                            annotation (Placement(transformation(
            extent={{20,0},{40,20}})));
  PowerSystems.AC3ph.Sensors.Psensor sensor
    annotation (Placement(transformation(extent={{-20,0},{0,20}})));
  PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=130e3)
    annotation (Placement(transformation(extent={{80,0},{60,20}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-60,0},{-80,20}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{80,0},{100,20}})));

equation
  connect(transPh.y,infBus1.vPhasor_in)
                                 annotation (Line(points={{-60,30},{-44,30},{
            -44,20}}, color={0,0,127}));
  connect(infBus1.term, sensor.term_p)  annotation (Line(points={{-40,10},{-20,
            10}}, color={0,110,110}));
  connect(sensor.term_n, line.term_p)
      annotation (Line(points={{0,10},{20,10}}, color={0,110,110}));
    connect(line.term_n, infBus2.term)
                                      annotation (Line(points={{40,10},{60,10}},
          color={0,110,110}));
  connect(grd1.term,infBus1. neutral)
      annotation (Line(points={{-60,10},{-60,10}}, color={0,0,255}));
    connect(grd2.term, infBus2.neutral)
      annotation (Line(points={{80,10},{80,10}}, color={0,0,255}));
  annotation (
    Documentation(
            info="<html>
<p>Shows the influence of phase-difference on power flow.<br>
Alternatively one can look at a variation of amplitude ratios.</p>
<p><i>See for example:</i>
<pre>
  sensor.p[1]     active power
  sensor.p[2]     reactive power.
</pre>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=60));
end PowerTransfer;

model VoltageStability "Voltage stability"
  extends Modelica.Icons.Example;

  inner PowerSystems.System system
                      annotation (Placement(transformation(extent={{-100,80},{
              -80,100}})));
  PowerSystems.AC3ph.Sources.InfBus Vsource0(V_nom=400e3, alpha0=0)
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
  PowerSystems.AC3ph.Sources.InfBus infBus(V_nom=400e3, use_vPhasor_in=true)
    annotation (Placement(transformation(extent={{60,0},{40,20}})));

  PowerSystems.AC3ph.Sensors.Vmeter Vmeter(V_nom=400e3)
      annotation (Placement(transformation(extent={{40,40},{60,60}})));
  PowerSystems.AC3ph.Sensors.PVImeter meter0(S_nom=100e6)
      annotation (Placement(transformation(extent={{0,40},{20,60}})));
  PowerSystems.AC3ph.Lines.RXline line0(redeclare record Data =
    PowerSystems.AC3ph.Lines.Parameters.RXline (
        V_nom=400e3,
        r=2e-3,
        x=0.25e-3), len=500e3)            annotation (Placement(transformation(
            extent={{-40,40},{-20,60}})));
  PowerSystems.AC3ph.Sources.InfBus Vsource1(V_nom=400e3, alpha0=
          0.087266462599716)
    annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
  PowerSystems.AC3ph.Sensors.PVImeter meter1(S_nom=100e6)
      annotation (Placement(transformation(extent={{0,0},{20,20}})));
  PowerSystems.AC3ph.Lines.RXline line1(redeclare record Data =
    PowerSystems.AC3ph.Lines.Parameters.RXline (
        V_nom=400e3,
        r=2e-3,
        x=0.25e-3), len=500e3)            annotation (Placement(transformation(
            extent={{-40,0},{-20,20}})));
  PowerSystems.AC3ph.Sources.InfBus Vsource2(V_nom=400e3, alpha0=-0.087266462599716)
    annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
  PowerSystems.AC3ph.Sensors.PVImeter meter2(S_nom=100e6)
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
  PowerSystems.AC3ph.Lines.RXline line2(redeclare record Data =
    PowerSystems.AC3ph.Lines.Parameters.RXline (
        V_nom=400e3,
        r=2e-3,
        x=0.25e-3), len=500e3)            annotation (Placement(transformation(
            extent={{-40,-40},{-20,-20}})));
  PowerSystems.Blocks.Signals.TransientPhasor transPh(
      t_change=90,
      t_duration=120,
      a_start=1,
      a_end=0) annotation (Placement(transformation(extent={{90,20},{70,40}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,40},{-100,60}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd4 annotation (Placement(transformation(
            extent={{60,0},{80,20}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{-80,0},{-100,20}})));
  PowerSystems.AC3ph.Nodes.GroundOne grd3 annotation (Placement(transformation(
            extent={{-80,-40},{-100,-20}})));

equation
  connect(transPh.y, infBus.vPhasor_in)
      annotation (Line(points={{70,30},{44,30},{44,20}}, color={0,0,127}));
  connect(Vsource0.term, line0.term_p)   annotation (Line(points={{-60,50},{-40,
            50}}, color={0,110,110}));
  connect(line0.term_n, meter0.term_p)   annotation (Line(points={{-20,50},{0,
            50}}, color={0,110,110}));
  connect(meter0.term_n, infBus.term)   annotation (Line(points={{20,50},{30,50},
            {30,10},{40,10}}, color={0,110,110}));
  connect(Vsource1.term, line1.term_p)   annotation (Line(points={{-60,10},{-40,
            10}}, color={0,110,110}));
  connect(line1.term_n, meter1.term_p)   annotation (Line(points={{-20,10},{0,
            10}}, color={0,110,110}));
  connect(meter1.term_n, infBus.term)   annotation (Line(points={{20,10},{40,10}},
          color={0,110,110}));
  connect(Vsource2.term, line2.term_p)   annotation (Line(points={{-60,-30},{
            -40,-30}}, color={0,110,110}));
  connect(line2.term_n, meter2.term_p)   annotation (Line(points={{-20,-30},{0,
            -30}}, color={0,110,110}));
  connect(meter2.term_n, infBus.term)   annotation (Line(points={{20,-30},{30,
            -30},{30,10},{40,10}}, color={0,110,110}));
  connect(infBus.term, Vmeter.term)   annotation (Line(points={{40,10},{40,50}},
          color={0,110,110}));
  connect(grd1.term, Vsource0.neutral)
      annotation (Line(points={{-80,50},{-80,50}}, color={0,0,255}));
  connect(grd2.term, Vsource1.neutral)
      annotation (Line(points={{-80,10},{-80,10}}, color={0,0,255}));
  connect(grd3.term, Vsource2.neutral) annotation (Line(points={{-80,-30},{-80,
            -30}}, color={0,0,255}));
  connect(infBus.neutral, grd4.term)
      annotation (Line(points={{60,10},{60,10}}, color={0,0,255}));
  annotation (
    Documentation(
            info="<html>
<p>Power flow between source and infinite bus. The bus-voltage decreases from 1 to 0.
<pre>
  stable:     voltage above extremal point (maximum p[1])
  instable:   voltage below extremal point (maximum p[1])
</pre></p>
<p><i>See for example:</i>
<pre>
  meter0/1/2.v_norm and plot it against
  meter0/1/2.p[1] as independent variable.
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>"),
      experiment(StopTime=180, Interval=180e-3));
end VoltageStability;

  model RXline "Single lumped line"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=400e3,
      v0=1.04,
      alpha0=0.5235987755983)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.AC3ph.Lines.RXline line(redeclare record Data =
      PowerSystems.AC3ph.Lines.Parameters.RXline (
        V_nom=400e3,
        x=0.25e-3,
        r=0.02e-3), len=400e3)
      annotation (Placement(transformation(extent={{10,-20},{30,0}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{40,-20},{60,0}})));
    PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=400e3)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(
      t_switch={0.2,0.65})
        annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(
      t_switch={0.3,0.7})
        annotation (Placement(transformation(extent={{80,20},{60,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter(V_nom=400e3, S_nom=1000e6)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));

  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-20,30},{-10,
            30},{-10,0}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{60,30},{50,30},
            {50,0}}, color={255,0,255}));
    connect(infBus1.term, meter.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,110,110}));
    connect(meter.term_n, switch1.term_p) annotation (Line(points={{-30,-10},{
            -20,-10}}, color={0,110,110}));
    connect(switch1.term_n, line.term_p) annotation (Line(points={{0,-10},{10,
            -10}}, color={0,110,110}));
    connect(line.term_n, switch2.term_p) annotation (Line(points={{30,-10},{40,
            -10}}, color={0,110,110}));
    connect(switch2.term_n, infBus2.term)
                                         annotation (Line(points={{60,-10},{70,
            -10}}, color={0,110,110}));
    connect(grd1.term,infBus1. neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(infBus2.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Short-time line switched off.<br>
Compare with PIline.</p>
<p><i>See for example:</i>
<pre>  meter.p[1:2]     active and reactive power</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=1));
  end RXline;

  model Tline "Single PI-line"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=400e3,
      v0=1.04,
      alpha0=0.5235987755983)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Breakers.Breaker breaker1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.AC3ph.Lines.Tline line(len=400e3, redeclare record Data =
          PowerSystems.AC3ph.Lines.Parameters.Line (
          V_nom=400e3,
          x=0.25e-3,
          r=0.02e-3))
      annotation (Placement(transformation(extent={{10,-20},{30,0}})));
    PowerSystems.AC3ph.Breakers.Breaker breaker2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{40,-20},{60,0}})));
    PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=400e3)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(
      t_switch={0.2,0.65})
        annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(
      t_switch={0.3,0.7})
        annotation (Placement(transformation(extent={{80,20},{60,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter(V_nom=400e3, S_nom=1000e6)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));

  equation
    connect(relay1.y, breaker1.control) annotation (Line(points={{-20,30},{-10,
            30},{-10,0}}, color={255,0,255}));
    connect(relay2.y, breaker2.control) annotation (Line(points={{60,30},{50,30},
            {50,0}}, color={255,0,255}));
    connect(infBus1.term, meter.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,110,110}));
    connect(meter.term_n, breaker1.term_p) annotation (Line(points={{-30,-10},{
            -20,-10}}, color={0,110,110}));
    connect(breaker1.term_n, line.term_p) annotation (Line(points={{0,-10},{10,
            -10}}, color={0,110,110}));
    connect(line.term_n, breaker2.term_p) annotation (Line(points={{30,-10},{40,
            -10}}, color={0,110,110}));
    connect(breaker2.term_n, infBus2.term)
                                          annotation (Line(points={{60,-10},{70,
            -10}}, color={0,110,110}));
    connect(grd1.term,infBus1. neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(infBus2.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Short-time line switched off.<br>
Compare with RXline.</p>
<p><i>See for example:</i>
<pre>
  meter.p[1:2]     active and reactive power
  line.v           line voltage, oscillations due to switching
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=1, Interval=2.5e-5));
  end Tline;

  model FaultRXline "Faulted lumped line"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=400e3,
      v0=1.04,
      alpha0=0.5235987755983)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.AC3ph.Lines.FaultRXline line(redeclare record Data =
      PowerSystems.AC3ph.Lines.Parameters.RXline (
        V_nom=400e3,
        x=0.25e-3,
        r=0.02e-3), len=400e3)
      annotation (Placement(transformation(extent={{10,-20},{30,0}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{40,-20},{60,0}})));
    PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=400e3)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(
      t_switch={0.2,0.65})
        annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(
      t_switch={0.3,0.7})
        annotation (Placement(transformation(extent={{80,20},{60,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter(V_nom=400e3, S_nom=1000e6)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc
         annotation (Placement(transformation(extent={{10,20},{30,40}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));

  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-20,30},{-10,
            30},{-10,0}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{60,30},{50,30},
            {50,0}}, color={255,0,255}));
    connect(infBus1.term, meter.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,110,110}));
    connect(meter.term_n, switch1.term_p) annotation (Line(points={{-30,-10},{
            -20,-10}}, color={0,110,110}));
    connect(switch1.term_n, line.term_p) annotation (Line(points={{0,-10},{10,
            -10}}, color={0,110,110}));
    connect(line.term_n, switch2.term_p) annotation (Line(points={{30,-10},{40,
            -10}}, color={0,110,110}));
    connect(switch2.term_n, infBus2.term)
                                         annotation (Line(points={{60,-10},{70,
            -10}}, color={0,110,110}));
    connect(line.term_f, abc.term)
      annotation (Line(points={{20,0},{20,20}}, color={0,120,120}));
    connect(grd1.term,infBus1. neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(infBus2.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with FaultPIline.</p>
<p><i>See for example:</i>
<pre>
  meter.p[1:2]     active and reactive power
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=1));
  end FaultRXline;

  model FaultTline "Faulted PI-line"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=400e3,
      v0=1.04,
      alpha0=0.5235987755983)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Breakers.Breaker breaker1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.AC3ph.Lines.FaultTline line(len=400e3, redeclare record Data =
          PowerSystems.AC3ph.Lines.Parameters.Line (
          V_nom=400e3,
          x=0.25e-3,
          r=0.02e-3))
      annotation (Placement(transformation(extent={{10,-20},{30,0}})));
    PowerSystems.AC3ph.Breakers.Breaker breaker2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{40,-20},{60,0}})));
    PowerSystems.AC3ph.Sources.InfBus infBus2(V_nom=400e3)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(
      t_switch={0.2,0.65})
        annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(
      t_switch={0.3,0.7})
        annotation (Placement(transformation(extent={{80,20},{60,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter(V_nom=400e3, S_nom=1000e6)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc
         annotation (Placement(transformation(extent={{10,20},{30,40}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));

  equation
    connect(relay1.y, breaker1.control) annotation (Line(points={{-20,30},{-10,
            30},{-10,0}}, color={255,0,255}));
    connect(relay2.y, breaker2.control) annotation (Line(points={{60,30},{50,30},
            {50,0}}, color={255,0,255}));
    connect(infBus1.term, meter.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,110,110}));
    connect(meter.term_n, breaker1.term_p) annotation (Line(points={{-30,-10},{
            -20,-10}}, color={0,110,110}));
    connect(breaker1.term_n, line.term_p) annotation (Line(points={{0,-10},{10,
            -10}}, color={0,110,110}));
    connect(line.term_n, breaker2.term_p) annotation (Line(points={{30,-10},{40,
            -10}}, color={0,110,110}));
    connect(breaker2.term_n, infBus2.term)
                                          annotation (Line(points={{60,-10},{70,
            -10}}, color={0,110,110}));
    connect(line.term_f, abc.term)
      annotation (Line(points={{20,0},{20,20}}, color={0,120,120}));
    connect(grd1.term,infBus1. neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(infBus2.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with FaultRXline.</p>
<p><i>See for example:</i>
<pre>
  meter.p[1:2]     active and reactive power
  line.v           line voltage, oscillations due to switching
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=1, Interval=2.5e-5));
  end FaultTline;

  model DoubleRXline "Parallel lumped lines, one faulted"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=20e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo(
      redeclare record Data =
          PowerSystems.Examples.Data.Transformers.TrafoStray,
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Y)
              annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Lines.RXline line(len=480000, redeclare record Data =
          PowerSystems.Examples.Data.Lines.OHline400kV)
      annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
    PowerSystems.AC3ph.Lines.FaultRXline lineF(
      redeclare record Data = PowerSystems.Examples.Data.Lines.OHline400kV,
        len=430000)
         annotation (Placement(transformation(extent={{20,0},{40,20}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{50,0},{70,20}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc
         annotation (Placement(transformation(extent={{20,40},{40,60}})));
    PowerSystems.AC3ph.Sources.InfBus InfBus2(V_nom=400e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(t_switch={0.15,0.2})
      annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(t_switch={0.153,0.21})
             annotation (Placement(transformation(extent={{90,40},{70,60}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterL(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterF(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,0},{10,20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-90,-20},{-110,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));
  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-40,50},{-30,
            50},{-30,20}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{70,50},{60,50},
            {60,20}}, color={255,0,255}));
    connect(trafo.term_n, meterL.term_p) annotation (Line(points={{-40,-10},{
            -40,-30},{-10,-30}}, color={0,110,110}));
    connect(meterL.term_n, line.term_p) annotation (Line(points={{10,-30},{20,
            -30}}, color={0,110,110}));
    connect(line.term_n, InfBus2.term)
                                      annotation (Line(points={{40,-30},{70,-30},
            {70,-10}}, color={0,110,110}));
    connect(trafo.term_n, switch1.term_p) annotation (Line(points={{-40,-10},{
            -40,10}}, color={0,110,110}));
    connect(switch1.term_n, meterF.term_p) annotation (Line(points={{-20,10},{
            -10,10}}, color={0,110,110}));
    connect(meterF.term_n, lineF.term_p) annotation (Line(points={{10,10},{20,
            10}}, color={0,110,110}));
    connect(lineF.term_n, switch2.term_p) annotation (Line(points={{40,10},{50,
            10}}, color={0,110,110}));
    connect(switch2.term_n, InfBus2.term)
                                         annotation (Line(points={{70,10},{70,
            -10}}, color={0,110,110}));
    connect(lineF.term_f, abc.term) annotation (Line(points={{30,20},{30,40}},
          color={0,120,120}));
    connect(infBus1.term, trafo.term_p)  annotation (Line(points={{-70,-10},{
            -60,-10}}, color={0,120,120}));
    connect(grd1.term,infBus1. neutral)  annotation (Line(points={{-90,-10},{
            -90,-10}}, color={0,0,255}));
    connect(InfBus2.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with DoublePIline.</p>
<p><i>See for example:</i>
<pre>
  meterL.p[1:2]    active and reactive power
  meterF.p[1:2]    active and reactive power fault line
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=0.5, Interval=2.5e-5));
  end DoubleRXline;

  model DoubleLine "Parallel lines, one faulted"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Sources.InfBus infBus1(V_nom=20e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo(
      redeclare record Data =
        PowerSystems.Examples.Data.Transformers.TrafoStray,
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Y)
              annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Lines.PIline line(
                                        len=480000, redeclare record Data =
          PowerSystems.Examples.Data.Lines.OHline_400kV)
      annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
    PowerSystems.AC3ph.Lines.FaultTline lineF(
      redeclare record Data =
          PowerSystems.Examples.Data.Lines.OHline_400kV, len=430000)
      annotation (Placement(transformation(extent={{20,0},{40,20}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{50,0},{70,20}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc
         annotation (Placement(transformation(extent={{20,40},{40,60}})));
    PowerSystems.AC3ph.Sources.InfBus InfBus2(V_nom=400e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(t_switch={0.15,0.2})
      annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(t_switch={0.153,0.21})
             annotation (Placement(transformation(extent={{90,40},{70,60}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterL(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterF(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,0},{10,20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-90,-20},{-110,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));

  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-40,50},{-30,
            50},{-30,20}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{70,50},{60,50},
            {60,20}}, color={255,0,255}));
    connect(trafo.term_n, meterL.term_p) annotation (Line(points={{-40,-10},{
            -40,-30},{-10,-30}}, color={0,110,110}));
    connect(meterL.term_n, line.term_p) annotation (Line(points={{10,-30},{20,
            -30}}, color={0,110,110}));
    connect(line.term_n, InfBus2.term)
                                      annotation (Line(points={{40,-30},{70,-30},
            {70,-10}}, color={0,110,110}));
    connect(trafo.term_n, switch1.term_p) annotation (Line(points={{-40,-10},{
            -40,10}}, color={0,110,110}));
    connect(switch1.term_n, meterF.term_p) annotation (Line(points={{-20,10},{
            -10,10}}, color={0,110,110}));
    connect(meterF.term_n, lineF.term_p) annotation (Line(points={{10,10},{20,
            10}}, color={0,110,110}));
    connect(lineF.term_n, switch2.term_p) annotation (Line(points={{40,10},{50,
            10}}, color={0,110,110}));
    connect(switch2.term_n, InfBus2.term)
                                         annotation (Line(points={{70,10},{70,
            -10}}, color={0,110,110}));
    connect(lineF.term_f, abc.term) annotation (Line(points={{30,20},{30,40}},
          color={0,110,110}));
    connect(infBus1.term, trafo.term_p)  annotation (Line(points={{-70,-10},{
            -60,-10}}, color={0,120,120}));
    connect(grd1.term, infBus1.neutral)  annotation (Line(points={{-90,-10},{
            -90,-10}}, color={0,0,255}));
    connect(grd2.term, InfBus2.neutral)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with DoublePIline.</p>
<p><i>See for example:</i>
<pre>
  meterL.p[1:2]    active and reactive power
  meterF.p[1:2]    active and reactive power fault line
  line.v           line voltage, oscillations due to switching
  lineF.v          fault line voltage
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>"),
      experiment(StopTime=0.5, Interval=2.5e-5));
  end DoubleLine;

  model DoubleRXlineTG
    "Parallel lumped lines with turbo generator, one line faulted"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Generation.TurboGenerator turbGen(
      p_start=0.762922,
      redeclare model Generator = PowerSystems.AC3ph.Machines.Synchron_ee (
            redeclare replaceable record Data =
              PowerSystems.AC3ph.Machines.Parameters.Synchron_ee (
              V_nom=20e3,
              S_nom=1000e6,
              If_nom=2000), dynType=PowerSystems.Types.Dynamics.FreeInitial) "nth order",
      initType=PowerSystems.Types.Init.v_alpha,
      alpha_start=0.5235987755983)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo(
      redeclare record Data =
        PowerSystems.Examples.Data.Transformers.TrafoStray,
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Y)
              annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Lines.RXline line(
      len=480000,
      redeclare record Data = PowerSystems.Examples.Data.Lines.OHline400kV)
      annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
    PowerSystems.AC3ph.Lines.FaultRXline lineF(
      redeclare record Data = PowerSystems.Examples.Data.Lines.OHline400kV,
        len=430000)
         annotation (Placement(transformation(extent={{20,0},{40,20}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{50,0},{70,20}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc(epsG=1e-5)
         annotation (Placement(transformation(extent={{20,40},{40,60}})));
    PowerSystems.AC3ph.Sources.InfBus InfBus(V_nom=400e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(t_switch={0.15,0.2})
      annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(t_switch={0.153,0.21})
             annotation (Placement(transformation(extent={{90,40},{70,60}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterL(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterF(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,0},{10,20}})));
    PowerSystems.Control.Setpoints.Set_w_p_v cst_set
                                       annotation (Placement(transformation(
            extent={{-110,-20},{-90,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));
    PowerSystems.Common.Thermal.BoundaryV boundary(m=2)
      annotation (Placement(transformation(extent={{-90,0},{-70,20}})));
  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-40,50},{-30,
            50},{-30,20}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{70,50},{60,50},
            {60,20}}, color={255,0,255}));
    connect(cst_set.setpts, turbGen.setpts) annotation (Line(points={{-90,-10},
            {-90,-10}}, color={0,0,127}));
    connect(turbGen.term, trafo.term_p) annotation (Line(points={{-70,-10},{-60,
            -10}}, color={0,110,110}));
    connect(trafo.term_n, meterL.term_p) annotation (Line(points={{-40,-10},{
            -40,-30},{-10,-30}}, color={0,110,110}));
    connect(meterL.term_n, line.term_p) annotation (Line(points={{10,-30},{20,
            -30}}, color={0,110,110}));
    connect(line.term_n, InfBus.term) annotation (Line(points={{40,-30},{70,-30},
            {70,-10}}, color={0,110,110}));
    connect(trafo.term_n, switch1.term_p) annotation (Line(points={{-40,-10},{
            -40,10}}, color={0,110,110}));
    connect(switch1.term_n, meterF.term_p) annotation (Line(points={{-20,10},{
            -10,10}}, color={0,110,110}));
    connect(meterF.term_n, lineF.term_p) annotation (Line(points={{10,10},{20,
            10}}, color={0,110,110}));
    connect(lineF.term_n, switch2.term_p) annotation (Line(points={{40,10},{50,
            10}}, color={0,110,110}));
    connect(switch2.term_n, InfBus.term) annotation (Line(points={{70,10},{70,
            -10}}, color={0,110,110}));
    connect(lineF.term_f, abc.term) annotation (Line(points={{30,20},{30,40}},
          color={0,110,110}));
    connect(InfBus.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    connect(turbGen.heat, boundary.heat)
      annotation (Line(points={{-80,0},{-80,0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with DoublePIline.</p>
<p><i>See for example:</i>
<pre>
  meterL.p[1:2]    active and reactive power
  meterF.p[1:2]    active and reactive power fault line
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>
"),      experiment(StopTime=0.5));
  end DoubleRXlineTG;

  model DoubleLineTG "Parallel lines with turbo generator, one line faulted"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system
                        annotation (Placement(transformation(extent={{-100,80},
              {-80,100}})));
    PowerSystems.AC3ph.Generation.TurboGenerator turbGen(
      p_start=0.761825,
      redeclare model Generator = PowerSystems.AC3ph.Machines.Synchron_ee (
        redeclare replaceable record Data =
          PowerSystems.AC3ph.Machines.Parameters.Synchron_ee (
          V_nom=20e3,
          S_nom=1000e6,
          If_nom=2000), dynType=PowerSystems.Types.Dynamics.FreeInitial) "nth order",
      alpha_start=0.5235987755983)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo(
      redeclare record Data =
        PowerSystems.Examples.Data.Transformers.TrafoStray,
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Y)
              annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Lines.PIline line(
                                        len=480e3, redeclare record Data =
          PowerSystems.Examples.Data.Lines.OHline_400kV)
      annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
    PowerSystems.AC3ph.Breakers.Switch switch1(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
    PowerSystems.AC3ph.Lines.FaultTline lineF(
      redeclare record Data =
          PowerSystems.Examples.Data.Lines.OHline_400kV, len=430000)
      annotation (Placement(transformation(extent={{20,0},{40,20}})));
    PowerSystems.AC3ph.Breakers.Switch switch2(V_nom=400e3, I_nom=2500)
      annotation (Placement(transformation(extent={{50,0},{70,20}})));
    PowerSystems.AC3ph.Faults.Fault_abc abc(epsG=1e-5)
         annotation (Placement(transformation(extent={{20,40},{40,60}})));
    PowerSystems.AC3ph.Sources.InfBus InfBus(V_nom=400e3, alpha0=
          0.5235987755983)
      annotation (Placement(transformation(extent={{90,-20},{70,0}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(t_switch={0.15,0.2})
      annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(t_switch={0.153,0.21})
             annotation (Placement(transformation(extent={{90,40},{70,60}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterL(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterF(S_nom=1000e6, V_nom=400e3)
      annotation (Placement(transformation(extent={{-10,0},{10,20}})));
    PowerSystems.Control.Setpoints.Set_w_p_v cst_set
                                       annotation (Placement(transformation(
            extent={{-110,-20},{-90,0}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-20},{110,0}})));
    PowerSystems.Common.Thermal.BoundaryV boundary(m=2)
      annotation (Placement(transformation(extent={{-90,0},{-70,20}})));
  equation
    connect(relay1.y, switch1.control) annotation (Line(points={{-40,50},{-30,
            50},{-30,20}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{70,50},{60,50},
            {60,20}}, color={255,0,255}));
    connect(cst_set.setpts, turbGen.setpts) annotation (Line(points={{-90,-10},
            {-90,-10}}, color={0,0,127}));
    connect(turbGen.term, trafo.term_p) annotation (Line(points={{-70,-10},{-60,
            -10}}, color={0,110,110}));
    connect(trafo.term_n, meterL.term_p) annotation (Line(points={{-40,-10},{
            -40,-30},{-10,-30}}, color={0,110,110}));
    connect(meterL.term_n, line.term_p) annotation (Line(points={{10,-30},{20,
            -30}}, color={0,110,110}));
    connect(line.term_n, InfBus.term) annotation (Line(points={{40,-30},{70,-30},
            {70,-10}}, color={0,110,110}));
    connect(trafo.term_n, switch1.term_p) annotation (Line(points={{-40,-10},{
            -40,10}}, color={0,110,110}));
    connect(switch1.term_n, meterF.term_p) annotation (Line(points={{-20,10},{
            -10,10}}, color={0,110,110}));
    connect(meterF.term_n, lineF.term_p) annotation (Line(points={{10,10},{20,
            10}}, color={0,110,110}));
    connect(lineF.term_n, switch2.term_p) annotation (Line(points={{40,10},{50,
            10}}, color={0,110,110}));
    connect(switch2.term_n, InfBus.term) annotation (Line(points={{70,10},{70,
            -10}}, color={0,110,110}));
    connect(lineF.term_f, abc.term) annotation (Line(points={{30,20},{30,40}},
          color={0,110,110}));
    connect(InfBus.neutral, grd2.term)
      annotation (Line(points={{90,-10},{90,-10}}, color={0,0,255}));
    connect(turbGen.heat, boundary.heat)
      annotation (Line(points={{-80,0},{-80,0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Fault clearance by short-time line switched off.<br>
Compare with DoublePIline.</p>
<p><i>See for example:</i>
<pre>
  meterL.p[1:2]    active and reactive power
  meterF.p[1:2]    active and reactive power fault line
  line.v           line voltage, oscillations due to switching
  lineF.v          fault line voltage
  abc.i_abc        fault currents
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Transmission\">up users guide</a></p>
</html>"),
      experiment(StopTime=0.5, Interval=1.5e-4));
  end DoubleLineTG;
  annotation (preferredView="info",
Documentation(info="<html>
<p>Transmission line models and faults.</p>
<p><a href=\"modelica://PowerSystems.Examples\">up users guide</a></p>
</html>"));
end Transmission;