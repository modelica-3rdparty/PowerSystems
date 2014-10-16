within PowerSystems.Semiconductors;
package PhaseModules "Phase modules"
  extends Modelica.Icons.VariantsPackage;

model DiodeModule "Diode module"
  extends Partials.AC1ph_DC_base;

/*replaceable package SCpackage=Semiconductors.Ideal "SC package"
  annotation (choices(
  choice(redeclare package SCpackage=Semiconductors.Ideal "ideal"),
  choice(redeclare package SCpackage=Semiconductors.Custom "custom")));*/
  package SCpackage=Semiconductors.Ideal "SC package";
  parameter SCpackage.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  PowerSystems.AC1ph_DC.Nodes.Electric_pn_p_n pn_p_n
                             annotation (Placement(transformation(extent={{-60,
              -10},{-40,10}}, rotation=0)));
  SCpackage.Diode diode1(final par=par) "diode DC_p"
    annotation (Placement(transformation(
          origin={0,30},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  SCpackage.Diode diode2(final par=par) "diode DC_n"
    annotation (Placement(transformation(
          origin={0,-30},
          extent={{-10,-10},{10,10}},
          rotation=90)));

equation
  connect(AC,diode1. term_p) annotation (Line(points={{100,0},{-6.12303e-016,0},
            {-6.12303e-016,20}}, color={0,0,255}));
  connect(diode1.term_n, pn_p_n.term_p) annotation (Line(points={{6.12303e-016,
            40},{0,40},{0,50},{-40,50},{-40,4},{-44,4}}, color={0,0,255}));
  connect(pn_p_n.term_n,diode2. term_p) annotation (Line(points={{-44,-4},{-40,
            -4},{-40,-50},{0,-50},{0,-40},{-6.12303e-016,-40}}, color={0,0,255}));
  connect(diode2.term_n, AC) annotation (Line(points={{6.12303e-016,-20},{
            6.12303e-016,0},{100,0}}, color={0,0,255}));
  connect(pn_p_n.term_pn, DC)
      annotation (Line(points={{-56,0},{-100,0}}, color={0,0,255}));
  connect(diode1.heat, heat)   annotation (Line(points={{-10,30},{-20,30},{-20,
            80},{0,80},{0,100}}, color={176,0,0}));
  connect(diode2.heat, heat)   annotation (Line(points={{-10,-30},{-20,-30},{
            -20,80},{0,80},{0,100}}, color={176,0,0}));
  annotation (defaultComponentName = "diodeMod1",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Documentation(
          info="<html>
<p>Diode-module for passive AC-DC rectifiers.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{-10,-38},{0,-18},{10,-38},{-10,-38}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-10,18},{0,38},{10,18},{-10,18}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,-18},{0,18}}, color={0,0,255}),
          Line(points={{-10,-18},{10,-18}}, color={0,0,255}),
          Line(points={{-10,38},{10,38}}, color={0,0,255}),
          Line(points={{0,0},{80,0}}, color={0,0,255}),
          Line(points={{-80,10},{-60,10},{-60,50},{0,50},{0,38}}, color={0,0,
                255}),
          Line(points={{-80,-10},{-60,-10},{-60,-50},{0,-50},{0,-40}}, color={0,
                0,255})}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end DiodeModule;

model SwitchModule "Switch module"
  extends Partials.AC1ph_DC_base;

/*replaceable package SCpackage=Semiconductors.Ideal "SC package"
  annotation (choices(
  choice(redeclare package SCpackage=PowerSystems.Semiconductors.Ideal "ideal"),
  choice(redeclare package SCpackage=PowerSystems.Semiconductors.Custom "custom")));*/
  package SCpackage=Semiconductors.Ideal "SC package";
  parameter SCpackage.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  Modelica.Blocks.Interfaces.BooleanInput[2] gates "gate-pair p and n"
    annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  Blocks.Multiplex.Gate2demux gate2demux(final n=1)
    annotation (Placement(transformation(extent={{-70,60},{-50,80}}, rotation=0)));
  PowerSystems.AC1ph_DC.Nodes.Electric_pn_p_n pn_p_n
    annotation (Placement(transformation(extent={{-80,-10},{-60,10}}, rotation=
              0)));
  SCpackage.SCswitch_Diode switch_D1(final par=par)
      "switch + reverse diode DC_p"
    annotation (Placement(transformation(
          origin={0,30},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  SCpackage.SCswitch_Diode switch_D2(final par=par)
      "switch + reverse diode DC_n"
    annotation (Placement(transformation(
          origin={0,-30},
          extent={{-10,10},{10,-10}},
          rotation=270)));

equation
  connect(AC, switch_D1.term_n) annotation (Line(points={{100,0},{-6.12303e-016,
            0},{-6.12303e-016,20}}, color={0,0,255}));
  connect(switch_D1.term_p, pn_p_n.term_p) annotation (Line(points={{
            6.12303e-016,40},{0,40},{0,50},{-40,50},{-40,4},{-64,4}}, color={0,
            0,255}));
  connect(AC, switch_D2.term_p) annotation (Line(points={{100,0},{6.12303e-016,
            0},{6.12303e-016,-20}}, color={0,0,255}));
  connect(switch_D2.term_n, pn_p_n.term_n) annotation (Line(points={{
            -6.12303e-016,-40},{0,-40},{0,-50},{-40,-50},{-40,-4},{-64,-4}},
          color={0,0,255}));
  connect(gates, gate2demux.gates) annotation (Line(points={{-60,100},{-60,80}},
          color={255,0,255}));
  connect(gate2demux.gates_1[1], switch_D1.gate) annotation (Line(points={{-64,
            60},{-64,24},{-10,24}}, color={255,0,255}));
  connect(gate2demux.gates_2[1], switch_D2.gate) annotation (Line(points={{-56,
            60},{-56,-36},{-10,-36}}, color={255,0,255}));
  connect(pn_p_n.term_pn, DC)
      annotation (Line(points={{-76,0},{-100,0}}, color={0,0,255}));
  connect(switch_D1.heat, heat)   annotation (Line(points={{-10,30},{-20,30},{
            -20,80},{0,80},{0,100}}, color={176,0,0}));
  connect(switch_D2.heat, heat)   annotation (Line(points={{-10,-30},{-20,-30},
            {-20,80},{0,80},{0,100}}, color={176,0,0}));
  annotation (defaultComponentName = "switchMod1",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Documentation(
          info="<html>
<p>Switch-module for controlled DC-AC inverters.</p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
<p>In order to avoid short circuit of the DC-side, the gate control of this module has to guarantee
<pre>
  (gates[1] and gates[2]) = false
</pre>
for all times.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{30,46},{30,-48}}, color={0,0,255}),
          Line(points={{30,-34},{16,-46}}, color={255,0,255}),
          Line(points={{-30,-48},{-30,46}}, color={0,0,255}),
          Line(points={{-40,-14},{-20,-14}}, color={0,0,255}),
          Line(points={{-40,34},{-20,34}}, color={0,0,255}),
          Line(points={{20,14},{40,14}}, color={0,0,255}),
          Line(points={{20,-34},{40,-34}}, color={0,0,255}),
          Line(points={{-30,0},{80,0}}, color={0,0,255}),
          Polygon(
            points={{20,34},{30,14},{40,34},{20,34}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-40,14},{-30,34},{-20,14},{-40,14}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{20,-14},{30,-34},{40,-14},{20,-14}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-40,-34},{-30,-14},{-20,-34},{-40,-34}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{30,14},{16,2}}, color={255,0,255}),
          Line(points={{-80,10},{-60,10},{-60,46},{30,46}}, color={0,0,255}),
          Line(points={{-80,-10},{-60,-10},{-60,-48},{30,-48}}, color={0,0,255})}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end SwitchModule;
annotation (preferedView="info",
    Window(
x=0.05,
y=0.41,
width=0.4,
height=0.32,
library=1,
autolayout=1),
    Documentation(info="<html>
<p>Phase modules for passive and active devices.<br>
</html>
"),
  Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end PhaseModules;
