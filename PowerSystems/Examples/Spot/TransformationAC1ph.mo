within PowerSystems.Examples.Spot;
package TransformationAC1ph "Transformation 1 phase"
  extends Modelica.Icons.ExamplesPackage;

  model OnePhase "One phase transformer"

    inner PowerSystems.System system(ref="inertial")
      annotation (Placement(transformation(extent={{-100.5,80},{-80.5,100}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(pol=-1)
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter1
      annotation (Placement(transformation(extent={{-60,-10},{-40,10}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.BusBar bus   annotation (Placement(transformation(
            extent={{-30,-10},{-10,10}}, rotation=0)));
    PowerSystems.AC1ph_DC.Transformers.TrafoStray trafo(par(V_nom={1,10}))
      annotation (Placement(transformation(extent={{0,-10},{20,10}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter2(V_nom=10)
      annotation (Placement(transformation(extent={{40,-10},{60,10}}, rotation=
              0)));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Resistor res(V_nom=10, r=1000)
      annotation (Placement(transformation(extent={{80,-10},{100,10}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.PolarityGround polGrd(pol=0)
      annotation (Placement(transformation(extent={{80,-40},{100,-20}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-90,-10},{-110,10}}, rotation=0)));

  equation
    connect(voltage.term, meter1.term_p)
      annotation (Line(points={{-70,0},{-60,0}}, color={0,0,255}));
    connect(meter1.term_n, bus.term)
      annotation (Line(points={{-40,0},{-20,0}}, color={0,0,255}));
    connect(bus.term, trafo.term_p)
      annotation (Line(points={{-20,0},{0,0}}, color={0,0,255}));
    connect(trafo.term_n, meter2.term_p)
      annotation (Line(points={{20,0},{40,0}}, color={0,0,255}));
    connect(meter2.term_n, res.term)
      annotation (Line(points={{60,0},{80,0}}, color={0,0,255}));
    connect(res.term, polGrd.term)
      annotation (Line(points={{80,0},{80,-30}}, color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-90,0},{-90,0}}, color={0,0,255}));
    annotation (
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The one-phase transformer has fluctuating potential primary and secondary side.<br>
Both sides have to choose a grounding scheme. In this example grounding is performed<br>
- for primary side: integrated in the voltage source<br>
- for secondary side: explicitly using the component 'PolarityGround'.</p>
<p>
<i>Compare:</i>
<pre>
  meter1.v     voltage phase secondary Y-Y topology
  meter2.v     voltage phase secondary Y_Delta topology
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
      experiment(NumberOfIntervals=1234),
      experimentSetupOutput);
  end OnePhase;

  model TapChanger "One phase tap changing primary and secondary"

    inner PowerSystems.System system(ref="inertial")
      annotation (Placement(transformation(extent={{-100.5,80},{-80.5,100}},
            rotation=0)));
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
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(pol=-1)
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter1
      annotation (Placement(transformation(extent={{-60,-10},{-40,10}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.BusBar bus   annotation (Placement(transformation(
            extent={{-30,-10},{-10,10}}, rotation=0)));
    PowerSystems.AC1ph_DC.Transformers.TrafoStray trafo1(par(
      v_tc1={0.9,1.0,1.1},
      v_tc2={0.9,1.0,1.1},
      V_nom={1,10}))
                    annotation (Placement(transformation(extent={{0,20},{20,40}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter12(V_nom=10)
      annotation (Placement(transformation(extent={{40,20},{60,40}}, rotation=0)));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Resistor res12(V_nom=10, r=1000)
      annotation (Placement(transformation(extent={{80,20},{100,40}}, rotation=
              0)));
    PowerSystems.AC1ph_DC.Transformers.TrafoStray trafo2(par(
      v_tc1={0.9,1.0,1.1},
      v_tc2={0.9,1.0,1.1},
      V_nom={1,10}))
                    annotation (Placement(transformation(extent={{0,-20},{20,
              -40}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter22(V_nom=10)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Resistor res22(V_nom=10, r=1000)
      annotation (Placement(transformation(extent={{80,-40},{100,-20}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.PolarityGround polGrd1(pol=0)
      annotation (Placement(transformation(extent={{80,0},{100,20}}, rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.PolarityGround polGrd2(pol=0)
      annotation (Placement(transformation(extent={{80,-60},{100,-40}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-90,-10},{-110,10}}, rotation=0)));

  equation
    connect(voltage.term, meter1.term_p)
      annotation (Line(points={{-70,0},{-60,0}}, color={0,0,255}));
    connect(meter1.term_n, bus.term)
      annotation (Line(points={{-40,0},{-20,0}}, color={0,0,255}));
    connect(bus.term, trafo1.term_p) annotation (Line(points={{-20,0},{-10,0},{
            -10,30},{0,30}}, color={0,0,255}));
    connect(trafo1.term_n, meter12.term_p)
      annotation (Line(points={{20,30},{40,30}}, color={0,0,255}));
    connect(meter12.term_n, res12.term)
      annotation (Line(points={{60,30},{80,30}}, color={0,0,255}));
    connect(bus.term, trafo2.term_p) annotation (Line(points={{-20,0},{-10,0},{
            -10,-30},{0,-30}}, color={0,0,255}));
    connect(trafo2.term_n, meter22.term_p)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,255}));
    connect(meter22.term_n, res22.term)
      annotation (Line(points={{60,-30},{80,-30}}, color={0,0,255}));
    connect(res12.term, polGrd1.term)
      annotation (Line(points={{80,30},{80,10}}, color={0,0,255}));
    connect(res22.term, polGrd2.term)
      annotation (Line(points={{80,-30},{80,-50}}, color={0,0,255}));
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
<p>The transformers change either primary or secondary voltage level at times (1,2,3).
<pre>
  trafo1   primary voltage levels (1, 0.9, 1, 1.1)*V_nom_prim
  trafo2 secondary voltage levels (1, 0.9, 1, 1.1)*V_nom_prim
</pre>
Note that the primary voltage source is fixed.</p>
<p>
<i>See for example:</i>
<pre>
  meter 12.v     voltage secondary, if primary is changed at fixed source.
  meter 22.v     voltage secondary, if secondary is changed at fixed source.
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      experiment(StopTime=4, NumberOfIntervals=4567),
      experimentSetupOutput);
  end TapChanger;
  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.42,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Transformers one-phase and tap changer control.</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end TransformationAC1ph;
