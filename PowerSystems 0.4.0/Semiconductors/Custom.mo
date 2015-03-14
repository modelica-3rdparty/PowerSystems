within PowerSystems.Semiconductors;
package Custom "Custom models"
  extends Modelica.Icons.VariantsPackage;

record SCparameter "Custom semiconductor parameters"
  extends Basic.Nominal.NominalDataVI;

  annotation (
    Documentation(
          info="<html>
</html>"));
end SCparameter;

model Diode "Diode"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}})));

  protected
  constant Real unitAmperePerVolt(unit="A/V") = 1    annotation(HideResult=true);

equation
  i = v*unitAmperePerVolt; // replace!
  annotation (defaultComponentName = "diode1",
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
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}));
end Diode;

model SCswitch "IGBT"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}})));
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
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}));
end SCswitch;

model Thyristor "Thyristor"
  extends Partials.ComponentBase;

  parameter SCparameter par "parameters" annotation (Placement(transformation(
            extent={{-80,-80},{-60,-60}})));
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
          Line(points={{-100,-100},{100,100}}, color={255,0,0})}));
end Thyristor;

annotation (preferredView="info",
    Documentation(info="<html>
<p>Intended for custom semiconductor models, replacing ideal components.<br>
</html>
"));
end Custom;
