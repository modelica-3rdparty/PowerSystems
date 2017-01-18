within PowerSystems.AC1ph_DC;
package ImpedancesSingle "Simple mpedance and admittance two terminal"
  extends Modelica.Icons.VariantsPackage;

  model Resistor "Resistor, 1-phase"
    extends Partials.ImpedBase;

    parameter SI.Resistance R=1;

  equation
    R*i = v;
    annotation (defaultComponentName="res1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,20},{80,-20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Resistor;

  model Conductor "Conductor, 1-phase"
    extends Partials.ImpedBase;

    parameter SI.Conductance G=1;

  equation
    G*v = i;
    annotation (defaultComponentName="cond1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,20},{80,-20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Conductor;

  model Inductor "Inductor with series resistor, 1-phase"
    extends Partials.ImpedBase;

    parameter SI.Resistance R=0;
    parameter SI.Inductance L=1e-3;

  equation
    L*der(i) + R*i = v;
    annotation (defaultComponentName="ind1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,20},{-40,-20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-40,20},{80,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-60,10},{-40,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-40,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}));
  end Inductor;

  model Capacitor "Capacitor with parallel conductor, 1-phase"
    extends Partials.ImpedBase;

    parameter SI.Conductance G=0;
    parameter SI.Capacitance C=1e-6;

  equation
    C*der(v) + G*v = i;
    annotation (defaultComponentName="cap1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,0},{-16,0}}, color={0,0,255}),
          Line(points={{90,0},{16,0}}, color={0,0,255}),
          Rectangle(
            extent={{-10,50},{10,-50}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-18,50},{-10,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{10,50},{18,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,20},{-2,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,20},{4,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-6},{10,-14}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,10},{-60,10},{-60,-10},{-10,-10}}, color={0,0,255}),
          Line(points={{4,10},{60,10},{60,-10},{10,-10}}, color={0,0,255})}));
  end Capacitor;

  model ResistorSym "Symmetrical capacitor with neutral access, 1-phase"
    extends Ports.PortBase;

    parameter SI.Resistance R=1;
    Interfaces.Electric_p term_p annotation (Placement(transformation(extent={{
              -110,-10},{-90,10}})));
    Interfaces.Electric_n term_n annotation (Placement(transformation(extent={{
              90,-10},{110,10}})));
    Interfaces.Electric_n neutral "symmetrical point"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Resistor res1(final R=R/2)               annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    Resistor res2(final R=R/2)               annotation (Placement(
          transformation(extent={{20,-10},{40,10}})));

  equation
    connect(term_p, res1.term_p)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,0,255}));
    connect(res1.term_n, neutral) annotation (Line(points={{-20,0},{0,0},{0,
            -100}}, color={0,0,255}));
    connect(neutral, res2.term_p) annotation (Line(points={{0,-100},{0,0},{20,0}},
          color={0,0,255}));
    connect(res2.term_n, term_n)
      annotation (Line(points={{40,0},{100,0}}, color={0,0,255}));
    annotation (defaultComponentName="resSym",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,0},{-70,0}}, color={0,0,255}),
          Line(points={{90,0},{70,0}}, color={0,0,255}),
          Line(points={{-10,0},{10,0}}, color={0,0,255}),
          Line(points={{0,0},{0,-90}}, color={0,0,255}),
          Text(
            extent={{-100,130},{100,90}},
            lineColor={0,0,0},
            textString=
             "%name"),
          Rectangle(
            extent={{-70,10},{-10,-10}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{10,10},{70,-10}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end ResistorSym;

  model CapacitorSym "Symmetrical capacitor with neutral access, 1-phase"
    extends Ports.PortBase;

    parameter SI.Conductance G=0;
    parameter SI.Capacitance C=1e-6;
    parameter PS.Voltage V_start=0 "start voltage";
    Interfaces.Electric_p term_p annotation (Placement(transformation(extent={{
              -110,-10},{-90,10}})));
    Interfaces.Electric_n term_n annotation (Placement(transformation(extent={{
              90,-10},{110,10}})));
    Interfaces.Electric_n neutral "symmetrical point"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Capacitor cap1(final G=2*G, final C=2*C, v(start=V_start/2))
                                             annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    Capacitor cap2(final G=2*G, final C=2*C, v(start=V_start/2))
                                             annotation (Placement(
          transformation(extent={{20,-10},{40,10}})));

  equation
    connect(term_p, cap1.term_p)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,0,255}));
    connect(cap1.term_n, neutral) annotation (Line(points={{-20,0},{0,0},{0,
            -100}}, color={0,0,255}));
    connect(neutral, cap2.term_p) annotation (Line(points={{0,-100},{0,0},{20,0}},
          color={0,0,255}));
    connect(cap2.term_n, term_n)
      annotation (Line(points={{40,0},{100,0}}, color={0,0,255}));
    annotation (defaultComponentName="capSym",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.ImpedancesSingle.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,0},{-68,0}}, color={0,0,255}),
          Line(points={{90,0},{68,0}}, color={0,0,255}),
          Rectangle(
            extent={{-60,50},{-40,-50}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-68,50},{-60,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,50},{-32,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{40,50},{60,-50}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{32,50},{40,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{60,50},{68,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-32,0},{32,0}}, color={0,0,255}),
          Line(points={{0,0},{0,-90}}, color={0,0,255}),
          Text(
            extent={{-100,130},{100,90}},
            lineColor={0,0,0},
            textString=
             "%name")}));
  end CapacitorSym;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model ImpedBase "Impedance base, 1-phase"
      extends Ports.PortBase;

      parameter PS.Voltage v_start = 0 "start value of voltage drop"
        annotation(Dialog(tab="Initialization"));
      parameter PS.Current i_start = 0 "start value of current"
        annotation(Dialog(tab="Initialization"));
      PS.Voltage v(start = v_start);
      PS.Current i(start = i_start);
      Interfaces.Electric_p term_p annotation (Placement(transformation(extent=
                {{-110,-10},{-90,10}})));
      Interfaces.Electric_n term_n annotation (Placement(transformation(extent=
                {{90,-10},{110,10}})));
    equation
      term_p.i + term_n.i = 0;
      v = term_p.v - term_n.v;
      i = term_p.i;
    annotation (
      Documentation(
            info="<html>
</html>
"),   Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
               "%name")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(points={{-90,0},{-60,0}}, color={0,0,
                  255}), Line(points={{60,0},{90,0}}, color={0,0,255})}));
    end ImpedBase;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>One-conductor models <b>without</b> choice of units and base-values, using directly the parameters</p>
<pre>
  R      resistance
  L      inductance
  G      conductance
  C      capacitance
</pre>
<p>in SI-units.</p>
</html>       "));
end ImpedancesSingle;
