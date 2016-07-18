within PowerSystems.AC1ph_DC;
package Shunts
  extends Modelica.Icons.VariantsPackage;

model ReactiveShunt "Shunt reactor with parallel conductor, 1-phase"
  extends Partials.ShuntBase;

  parameter SIpu.Conductance g=0 "conductance (parallel)";
  parameter SIpu.Resistance r=0 "resistance (serial)";
  parameter SIpu.Reactance x_s=1 "self reactance";
  parameter SIpu.Reactance x_m=0 "mutual reactance, -x_s < x_m < x_s";
  protected
  final parameter SI.Resistance[2] RL_base=Basic.Precalculation.baseRL(puUnits, V_nom, S_nom, 2*pi*f_nom);
  final parameter SI.Conductance G=g/RL_base[1];
  final parameter SI.Resistance R=r*RL_base[1];
  final parameter SI.Inductance[2,2] L=[x_s,x_m;x_m,x_s]*RL_base[2];
  PS.Current[2] i_x;

equation
  i_x = i - G*v;
  L*der(i_x) + R*i_x = v;
annotation (defaultComponentName = "xShunt",
  Documentation(
          info="<html>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{72,20},{80,-20}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-80,20},{-40,-20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,20},{72,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,32},{60,22}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,18},{60,8}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,4},{60,-4}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-8},{60,-18}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-22},{60,-32}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-60,8},{-60,32}}, color={0,0,255}),
          Line(points={{-60,-32},{-60,-8}}, color={0,0,255}),
          Line(points={{60,8},{60,32}}, color={0,0,255}),
          Line(points={{60,-32},{60,-8}}, color={0,0,255})}));
end ReactiveShunt;

model CapacitiveShunt "Shunt capacitor with parallel conductor, 1-phase, pp pg"
  extends Partials.ShuntBase;

  parameter SIpu.Conductance g_pg=0 "conductance ph-grd";
  parameter SIpu.Conductance g_pp=0 "conductance ph_ph";
  parameter SIpu.Susceptance b_pg=0.5 "susceptance ph-grd";
  parameter SIpu.Susceptance b_pp=0.5 "susceptance ph-ph";
  protected
  final parameter SI.Resistance[2] GC_base=Basic.Precalculation.baseGC(puUnits, V_nom, S_nom, 2*pi*f_nom);
  final parameter SI.Conductance[2,2] G=[g_pg+g_pp,-g_pp;-g_pp,g_pg+g_pp]*GC_base[1];
  final parameter SI.Capacitance[2,2] C=[b_pg+b_pp,-b_pp;-b_pp,b_pg+b_pp]*GC_base[2];

equation
  C*der(v) + G*v = i;
annotation (defaultComponentName = "cShunt",
  Documentation(
          info="<html>
<p>Terminology.<br>
&nbsp;  _pg denotes phase-to-ground<br>
&nbsp;  _pp denotes phase-to-phase</p>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-12,60},{12,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{-20,0}}, color={0,0,255}),
          Rectangle(
            extent={{-20,60},{-12,-60}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{36,40},{38,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{42,40},{44,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{36,0},{38,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{42,0},{44,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,14},{50,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,-26},{50,-34}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{36,30},{20,30},{20,10},{30,10}}, color={0,0,255}),
          Line(points={{36,-10},{20,-10},{20,-30},{30,-30}}, color={0,0,255}),
          Line(points={{44,30},{60,30},{60,10},{50,10}}, color={0,0,255}),
          Line(points={{44,-10},{60,-10},{60,-30},{50,-30}}, color={0,0,255}),
          Rectangle(
            extent={{-60,4},{-40,2}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-2},{-40,-4}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-34,10},{-26,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-50,4},{-50,16},{-30,16},{-30,10}}, color={0,0,255}),
          Line(points={{-50,-4},{-50,-16},{-30,-16},{-30,-10}}, color={0,0,255}),
          Line(points={{-60,20},{20,20}}, color={0,0,255}),
          Line(points={{-60,-20},{20,-20}}, color={0,0,255}),
          Line(points={{-40,20},{-40,16}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-16}}, color={0,0,255}),
          Text(
            extent={{0,-40},{80,-50}},
            lineColor={0,0,255},
            lineThickness=0.5,
            textString=
                 "b_pg"),
          Text(
            extent={{0,-50},{80,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            textString=
                 "g_pg"),
          Text(
            extent={{-80,-30},{0,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            textString=
                 "b_pp, g_pp")}));
end CapacitiveShunt;

package Partials "Partial models"
  extends Modelica.Icons.BasesPackage;

  partial model ShuntBase "Load base, 1-phase"
    extends Ports.Port_p;
    extends Basic.Nominal.NominalAC;

    PS.Voltage[2] v "voltage";
    PS.Current[2] i "current";

  equation
    v = term.v;
    i = term.i;
  annotation (
    Documentation(
  info="<html>
</html>
"), Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{60,20},{80,20}}, color={0,0,255}),
            Rectangle(
              extent={{80,30},{84,10}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid),
            Line(points={{60,-20},{80,-20}}, color={0,0,255}),
            Rectangle(
              extent={{80,-10},{84,-30}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,20},{-60,20}}, color={0,0,255}),
            Line(points={{-80,-20},{-60,-20}}, color={0,0,255})}));
  end ShuntBase;

end Partials;

  annotation (preferredView="info", Documentation(info="<html>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>
"));
end Shunts;
