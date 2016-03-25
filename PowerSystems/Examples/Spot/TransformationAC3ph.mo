within PowerSystems.Examples.Spot;
package TransformationAC3ph "Transformation dq0"
  extends Modelica.Icons.ExamplesPackage;

  model PhaseShifts "Phase shift primary-secondary"

    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100.5,80},{-80.5,100}})));

    PowerSystems.AC3ph.Sources.Voltage voltage
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter1(phasor=true)
      annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
    PowerSystems.AC3ph.Nodes.BusBar bus      annotation (Placement(transformation(
            extent={{-30,-10},{-10,10}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo1(
      redeclare record Data =
      PowerSystems.AC3ph.Transformers.Parameters.TrafoStray(V_nom={1,10}),
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Y,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Y)
      annotation (Placement(transformation(extent={{0,20},{20,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter12( V_nom=10, phasor=true)
      annotation (Placement(transformation(extent={{40,20},{60,40}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res12(V_nom=10, r=1000)
                                           annotation (Placement(transformation(
            extent={{80,20},{100,40}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo2(
      redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Y,
      redeclare model Topology_n = PowerSystems.AC3ph.Ports.Topology.Delta,
      redeclare record Data =
        PowerSystems.AC3ph.Transformers.Parameters.TrafoStray(V_nom={1,10}))
      annotation (Placement(transformation(
            extent={{0,-40},{20,-20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter22(phasor=true, V_nom=10)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res22(V_nom=10, r=1000)
                                           annotation (Placement(transformation(
            extent={{80,-40},{100,-20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-90,-10},{-110,10}})));

  equation
    connect(voltage.term,meter1. term_p) annotation (Line(points={{-70,0},{-60,
            0}}, color={0,110,110}));
    connect(meter1.term_n,bus. term)
                                    annotation (Line(points={{-40,0},{-20,0}},
          color={0,110,110}));
    connect(bus.term, trafo1.term_p) annotation (Line(points={{-20,0},{-10,0},{
            -10,30},{0,30}}, color={0,110,110}));
    connect(trafo1.term_n, meter12.term_p) annotation (Line(points={{20,30},{40,
            30}}, color={0,110,110}));
    connect(meter12.term_n, res12.term) annotation (Line(points={{60,30},{80,30}},
          color={0,110,110}));
    connect(bus.term, trafo2.term_p) annotation (Line(points={{-20,0},{-10,0},{
            -10,-30},{0,-30}}, color={0,110,110}));
    connect(trafo2.term_n, meter22.term_p) annotation (Line(points={{20,-30},{
            40,-30}}, color={0,110,110}));
    connect(meter22.term_n, res22.term) annotation (Line(points={{60,-30},{80,
            -30}}, color={0,110,110}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-90,0},{-90,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Primary and secondary signals show a topology dependent phase shift.</p>
<p>
Y-Y and Delta-Delta configuration:<br>
&nbsp; &nbsp;  no phase shift.</p>
<p>
Y-Delta configuration:<br>
&nbsp; &nbsp; shift secondary vs primary voltage is -30deg.</p>
<p>
Delta_Y configuration:<br>
&nbsp; &nbsp; shift secondary vs primary voltage is +30deg.</p>
<p>
<i>Compare:</i>
<pre>
  meter12.alpha_v     voltage phase secondary Y-Y topology
  meter22.alpha_v     voltage phase secondary Y_Delta topology
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.TransformationAC3ph\">up users guide</a></p>
</html>
"),      experiment(StopTime=1));
  end PhaseShifts;

  model TapChanger "Tap changing primary and secondary"

    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100.5,80},{-80.5,100}})));
    PowerSystems.AC3ph.Sources.Voltage voltage
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter1(phasor=true)
      annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
    PowerSystems.AC3ph.Nodes.BusBar bus      annotation (Placement(transformation(
            extent={{-30,-10},{-10,10}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo1(
      redeclare record Data =
      PowerSystems.AC3ph.Transformers.Parameters.TrafoStray (
      V_nom={1,10},
      v_tc1={0.9,1.0,1.1},
      v_tc2={0.9,1.0,1.1}),
      use_tap_p=true,
      use_tap_n=true)
      annotation (Placement(transformation(extent={{0,20},{20,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter12(V_nom=10, phasor=true)
      annotation (Placement(transformation(extent={{40,20},{60,40}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res12(V_nom=10, r=1000)
      annotation (Placement(transformation(extent={{80,20},{100,40}})));
    PowerSystems.AC3ph.Transformers.TrafoStray trafo2(
      redeclare record Data =
      PowerSystems.AC3ph.Transformers.Parameters.TrafoStray (
      V_nom={1,10},
      v_tc1={0.9,1.0,1.1},
      v_tc2={0.9,1.0,1.1}),
      use_tap_p=true,
      use_tap_n=true)
      annotation (Placement(transformation(extent={{0,-20},{20,-40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter22(phasor=true, V_nom=10)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res22(V_nom=10, r=1000)
                                           annotation (Placement(transformation(
            extent={{80,-40},{100,-20}})));
    PowerSystems.Control.Relays.TapChangerRelay TapRelay2(
      preset_1={0,0},
      preset_2={0,1,2,3},
      t_switch_2={1,2,3})
      annotation (Placement(transformation(
          origin={10,-70},
          extent={{10,-10},{-10,10}},
          rotation=270)));
    PowerSystems.Control.Relays.TapChangerRelay TapRelay1(
      preset_1={0,1,2,3},
      preset_2={0,0},
      t_switch_1={1,2,3})
      annotation (Placement(transformation(
          origin={10,70},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-90,-10},{-110,10}})));

  equation
    connect(voltage.term, meter1.term_p) annotation (Line(points={{-70,0},{-60,
            0}}, color={0,110,110}));
    connect(meter1.term_n, bus.term)
                                    annotation (Line(points={{-40,0},{-20,0}},
          color={0,110,110}));
    connect(bus.term, trafo1.term_p)
                                    annotation (Line(points={{-20,0},{-10,0},{
            -10,30},{0,30}}, color={0,110,110}));
    connect(trafo1.term_n, meter12.term_p) annotation (Line(points={{20,30},{40,
            30}}, color={0,110,110}));
    connect(meter12.term_n, res12.term) annotation (Line(points={{60,30},{80,30}},
          color={0,110,110}));
    connect(bus.term, trafo2.term_p)
                                    annotation (Line(points={{-20,0},{-10,0},{
            -10,-30},{0,-30}}, color={0,110,110}));
    connect(trafo2.term_n, meter22.term_p) annotation (Line(points={{20,-30},{
            40,-30}}, color={0,110,110}));
    connect(meter22.term_n, res22.term) annotation (Line(points={{60,-30},{80,
            -30}}, color={0,110,110}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-90,0},{-90,0}}, color={0,0,255}));
    connect(TapRelay1.tap_p, trafo1.tap_p)
      annotation (Line(points={{6,60},{6,40}}, color={255,127,0}));
    connect(TapRelay1.tap_n, trafo1.tap_n) annotation (Line(points={{14,60},{14,
            40}}, color={255,127,0}));
    connect(TapRelay2.tap_p, trafo2.tap_p) annotation (Line(points={{6,-60},{6,
            -40}}, color={255,127,0}));
    connect(TapRelay2.tap_n, trafo2.tap_n) annotation (Line(points={{14,-60},{
            14,-40}}, color={255,127,0}));
    annotation (
      Documentation(
              info="<html>
<p>The transformers change either primary or secondary voltage level at times (1,2,3).
<pre>
  trafo1   primary voltage levels (1, 0.9, 1, 1.1)*V_nom_prim
  trafo2 secondary voltage levels (1, 0.9, 1, 1.1)*V_nom_prim
</pre>
Note that the primary voltage source is fixed.</p>
<p>
<i>See for example:</i>
<pre>  meter 12.v_norm     voltage norm secondary</pre>
if primary side is changed at fixed source.
<pre>  meter 22.v_norm     voltage norm secondary</pre>
</pre>
if secondary side is changed at fixed source.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.TransformationAC3ph\">up users guide</a></p>
</html>
"),      experiment(StopTime=4));
  end TapChanger;

  model TreeWinding "Tree winding transformer"

    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100.5,80},{-80.5,100}})));

    PowerSystems.AC3ph.Sources.Voltage voltage
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter1(phasor=true)
      annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
    PowerSystems.AC3ph.Nodes.BusBar bus      annotation (Placement(transformation(
            extent={{-30,-10},{-10,10}})));
    PowerSystems.AC3ph.Transformers.Trafo3Stray trafo3Stray
      annotation (Placement(transformation(extent={{0,-10},{20,10}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter12( V_nom=10, phasor=true)
      annotation (Placement(transformation(extent={{40,20},{60,40}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res12(V_nom=10, r=1000)
                                           annotation (Placement(transformation(
            extent={{80,20},{100,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meter22(phasor=true, V_nom=10)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.AC3ph.ImpedancesYD.Resistor res22(V_nom=10, r=1000)
                                           annotation (Placement(transformation(
            extent={{80,-40},{100,-20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-90,-10},{-110,10}})));

  equation
    connect(voltage.term,meter1. term_p) annotation (Line(points={{-70,0},{-60,
            0}}, color={0,110,110}));
    connect(meter1.term_n,bus. term)
                                    annotation (Line(points={{-40,0},{-20,0}},
          color={0,110,110}));
    connect(meter12.term_n, res12.term) annotation (Line(points={{60,30},{80,30}},
          color={0,110,110}));
    connect(meter22.term_n, res22.term) annotation (Line(points={{60,-30},{80,
            -30}}, color={0,110,110}));
    connect(bus.term, trafo3Stray.term_p)
      annotation (Line(points={{-20,0},{0,0}}, color={0,120,120}));
    connect(trafo3Stray.term_na, meter12.term_p) annotation (Line(points={{20,4},
            {32,4},{32,30},{40,30}}, color={0,120,120}));
    connect(trafo3Stray.term_nb, meter22.term_p) annotation (Line(points={{20,
            -4},{30,-4},{30,-30},{40,-30}}, color={0,120,120}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-90,0},{-90,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
<p>Primary and secondary signals show a topology dependent phase shift.</p>
<p>
Y-Y and Delta-Delta configuration:<br>
&nbsp; &nbsp;  no phase shift.</p>
<p>
Y-Delta configuration:<br>
&nbsp; &nbsp; shift secondary vs primary voltage is -30deg.</p>
<p>
Delta_Y configuration:<br>
&nbsp; &nbsp; shift secondary vs primary voltage is +30deg.</p>
<p>
<i>Compare:</i>
<pre>
  meter12.alpha_v     voltage phase secondary Y-Y topology
  meter22.alpha_v     voltage phase secondary Y_Delta topology
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.TransformationAC3ph\">up users guide</a></p>
</html>
"),      experiment(StopTime=1));
  end TreeWinding;
  annotation (preferredView="info",
Documentation(info="<html>
<p>Transformers three-phase and tap changer control.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot\">up users guide</a></p>
</html>"));
end TransformationAC3ph;
