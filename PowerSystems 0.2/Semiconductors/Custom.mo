within PowerSystems.Semiconductors;
package Custom "Custom models"
  extends Modelica.Icons.VariantsPackage;

record SCparameter "Custom semiconductor parameters"
  extends Basic.Nominal.NominalDataVI;

  annotation (
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Documentation(
          info="<html>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end SCparameter;

model Diode "Diode"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}}, rotation=0)));

  protected
  constant Real unitAmperePerVolt(unit="A/V") = 1    annotation(HideResult=true);

equation
  i = v*unitAmperePerVolt; // replace!
  annotation (defaultComponentName = "diode1",
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{40,0},{-40,40},{-40,-40},{40,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{-40,0}}, color={0,0,255}),
          Line(points={{40,0},{90,0}}, color={0,0,255}),
          Line(points={{40,40},{40,-40}}, color={0,0,255}),
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Diode;

model SCswitch "IGBT"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}}, rotation=0)));
  Modelica.Blocks.Interfaces.BooleanInput gate "true:on, false: off"
    annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  protected
  constant Real unitAmperePerVolt(unit="A/V") = 1    annotation(HideResult=true);
equation
  i = v*unitAmperePerVolt; // replace!
  annotation (defaultComponentName = "IGBT1",
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{80,0},{30,40},{16,10},{80,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,40},{24,26}}, color={0,0,255}),
          Line(points={{90,0},{80,0}}, color={0,0,255}),
          Line(points={{60,60},{60,90}}, color={255,0,255}),
          Line(points={{-70,40},{70,40}}, color={0,0,255}),
          Line(points={{-70,60},{70,60}}, color={0,0,255}),
          Line(points={{-90,0},{-80,0},{-20,40}}, color={0,0,255}),
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end SCswitch;

model Thyristor "Thyristor"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}}, rotation=0)));
  Modelica.Blocks.Interfaces.BooleanInput gate "true:on, false: off"
    annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  protected
  constant Real unitAmperePerVolt(unit="A/V") = 1    annotation(HideResult=true);
equation
  i = v*unitAmperePerVolt; // replace!
  annotation (defaultComponentName = "thyristor1",
    Window(
x=0.45,
y=0.01,
width=0.44,
height=0.65),
    Documentation(
            info="<html>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{20,0},{-60,40},{-60,-40},{20,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{-60,0}}, color={0,0,255}),
          Line(points={{20,0},{90,0}}, color={0,0,255}),
          Line(points={{20,40},{20,-40}}, color={0,0,255}),
          Line(
            points={{20,0},{60,40},{60,90}},
            color={255,0,255},
            pattern=LinePattern.Dot),
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Thyristor;

annotation (preferedView="info",
    Window(
x=0.05,
y=0.41,
width=0.4,
height=0.32,
library=1,
autolayout=1),
    Documentation(info="<html>
<p>Intended for custom semiconductor models, replacing ideal components.<br>
</html>
"),
  Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Custom;
