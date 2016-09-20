within PowerSystems.AC1ph_DC;
package Sensors "Sensors n-phase or DC"
  extends Modelica.Icons.SensorsPackage;

  model VdiffSensor "Voltage difference sensor, 1-phase"
    extends Partials.Sensor1Base;

    Modelica.Blocks.Interfaces.RealOutput v
      "difference voltage 'plus' - 'minus'"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    v = term.v[1] - term.v[2];
    annotation (defaultComponentName = "Vsensor1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}),
      Documentation(
              info="<html>
</html>"));
  end VdiffSensor;

  model IdiffSensor "Current difference sensor, 1-phase"
    extends Partials.Sensor2Base;

    Modelica.Blocks.Interfaces.RealOutput i
      "current ('plus' - 'minus')/2, term_p to term_n"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    i = 0.5*(term_p.i[1] - term_p.i[2]);
    annotation (defaultComponentName = "Isensor1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(extent={{-20,20},{20,-20}}, lineColor=
                {135,135,135}), Line(points={{0,20},{0,90}}, color={135,135,135})}));
  end IdiffSensor;

  model Vsensor "Voltage sensor, 1-phase"
    extends Partials.Sensor1Base;

    Modelica.Blocks.Interfaces.RealOutput[2] v
      "voltage 'plus' and 'minus'-to-ground"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    v = term.v;
    annotation (defaultComponentName = "Vsensor1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,50},{20,70}}, color={135,135,135}),
          Line(points={{-20,40},{20,60}}, color={135,135,135})}),
      Documentation(
              info="<html>
</html>"));
  end Vsensor;

  model Isensor "Current sensor, 1-phase"
    extends Partials.Sensor2Base;

    Modelica.Blocks.Interfaces.RealOutput[2] i
      "current 'plus' and 'minus', term_p to term_n"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    i = term_p.i;
    annotation (defaultComponentName = "Isensor1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135}),
          Line(points={{0,20},{0,90}}, color={135,135,135}),
          Line(points={{-20,50},{20,70}}, color={135,135,135}),
          Line(points={{-20,40},{20,60}}, color={135,135,135})}));
  end Isensor;

  model Psensor "Power sensor, 1-phase"
    extends Partials.Sensor2Base;

    Modelica.Blocks.Interfaces.RealOutput p "power, term_p to term_n"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    p = term_p.v*term_p.i;
   annotation (defaultComponentName = "Psensor1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid), Line(points={{0,0},{20,0}}, color={
                0,0,255})}));
  end Psensor;

  model Vmeter "Voltage meter, 1-phase"
    extends Partials.Meter1Base(final S_nom=1);

    output SIpu.Voltage v(stateSelect=StateSelect.never);
    output SIpu.Voltage v0(stateSelect=StateSelect.never);
  protected
    SIpu.Voltage[2] v_ab(each stateSelect=StateSelect.never);
    final parameter PS.Voltage V_base=Utilities.Precalculation.baseV(
                                                                 puUnits, V_nom);

  equation
    v_ab = term.v/V_base;

    v = v_ab[1] - v_ab[2];
    v0 = (v_ab[1] + v_ab[2])/2;
    annotation (defaultComponentName = "Vmeter1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-15,45},{15,59}}, color={135,135,135}),
          Line(points={{-15,35},{15,49}}, color={135,135,135})}),
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals both in SI-units or in 'pu'.
Use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables:</p>
<pre>
  v      difference voltage 'plus' - 'minus'
  v0     average voltage ('plus' + 'minus')/2
</pre>
</html>
"));
  end Vmeter;

  model Imeter "Current meter, 1-phase"
    extends Partials.Meter2Base;

    output SIpu.Current i(stateSelect=StateSelect.never);
    output SIpu.Current i0(stateSelect=StateSelect.never);
  protected
    SIpu.Current[2] i_ab(each stateSelect=StateSelect.never);
    final parameter PS.Current I_base=Utilities.Precalculation.baseI(
                                                                 puUnits, V_nom, S_nom);

  equation
    i_ab = term_p.i/I_base;

    i = (i_ab[1] - i_ab[2])/2;
    i0 = (i_ab[1] + i_ab[2]);
    annotation (defaultComponentName = "Imeter1",
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals both in SI-units or in 'pu'.
Use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables:</p>
<pre>
  i     current term_p to term_n, ('plus' - 'minus')/2</pre>
  i0    sum current term_p to term_n, 'plus' + 'minus'
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135}),
          Line(points={{-15,45},{15,59}}, color={135,135,135}),
          Line(points={{-15,35},{15,49}}, color={135,135,135})}));
  end Imeter;

  model Pmeter "Power meter, 1-phase"

    parameter Boolean av=false "time average power"
                                   annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
                                                    annotation(Evaluate=true, Dialog(group="Options",enable=av));
    extends Partials.Meter2Base(final V_nom=1);

    output SIpu.Power p(stateSelect=StateSelect.never);
    output SIpu.Power p_av=pav if av;
  protected
    outer System system;
    final parameter SI.ApparentPower S_base=Utilities.Precalculation.baseS(
        puUnits, S_nom);
    SIpu.Power pav;

  initial equation
    if av then
      pav = p;
    end if;

  equation
    p = (term_p.v*term_p.i)/S_base;
    if av then
      der(pav) = (p - pav)/tcst;
    else
      pav = 0;
    end if;
    annotation (defaultComponentName = "Pmeter1",
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals both in SI-units or in 'pu'.
Use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variable:</p>
<pre>  p     power term_p to term_n</pre>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid), Line(points={{0,0},{20,0}}, color={
                0,0,255})}));
  end Pmeter;

  model PVImeter "Power-voltage-current meter, 1-phase"

    parameter Boolean av=false "time average power"
                                   annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
                                                    annotation(Evaluate=true, Dialog(group="Options",enable=av));
    extends Partials.Meter2Base;
    output SIpu.Power p(stateSelect=StateSelect.never);
    output SIpu.Power p_av=pav if av;
    output SIpu.Voltage v(stateSelect=StateSelect.never);
    output SIpu.Voltage v0(stateSelect=StateSelect.never);
    output SIpu.Current i(stateSelect=StateSelect.never);
    output SIpu.Current i0(stateSelect=StateSelect.never);
  protected
    outer System system;
    final parameter PS.Voltage V_base=Utilities.Precalculation.baseV(
                                                                 puUnits, V_nom);
    final parameter PS.Current I_base=Utilities.Precalculation.baseI(
                                                                 puUnits, V_nom, S_nom);
    SIpu.Power pav;
    SIpu.Voltage[2] v_ab;
    SIpu.Current[2] i_ab;

  initial equation
    if av then
      pav = p;
    end if;

  equation
    v_ab = term_p.v/V_base;
    i_ab = term_p.i/I_base;
    v = v_ab[1] - v_ab[2];
    v0 = (v_ab[1] + v_ab[2])/2;
    i = (i_ab[1] - i_ab[2])/2;
    i0 = (i_ab[1] + i_ab[2]);
    p = v_ab*i_ab;
    if av then
      der(pav) = (p - pav)/tcst;
    else
      pav = 0;
    end if;
    annotation (defaultComponentName = "PVImeter1",
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals both in SI-units or in 'pu'.
Use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables:</p>
<pre>
  p     power term_p to term_n
  v     difference voltage 'plus' - 'minus'
  v0    average voltage ('plus' + 'minus')/2
  i     current term_p to term_n, ('plus' - 'minus')/2</pre>
  i0    sum current term_p to term_n, 'plus' + 'minus'
</pre>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135}),
          Ellipse(
            extent={{-8,8},{8,-8}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{0,0},{20,0}}, color={0,0,255}),
          Line(points={{-15,45},{15,59}}, color={135,135,135}),
          Line(points={{-15,35},{15,49}}, color={135,135,135})}));
  end PVImeter;

  model Efficiency "Power sensor, 3-phase dq0"
    extends Partials.Sensor2Base;

    Interfaces.ThermalV_p heat(     m=m) "vector heat port"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Boolean dir_in=true "direction" annotation(Evaluate=true, choices(
      choice=true "points into the component",
      choice=false "point out of the component"));
    parameter Integer m(final min=1)=1 "dimension of heat port";
    parameter Boolean av=false "time average efficiency" annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
      annotation(Evaluate=true, Dialog(group="Options",enable=av));
    parameter SI.Temperature T_amb=300 "ambient temperature";
    output Real eta "efficiency";
  protected
    SI.Power p "total el power, term_p to term_n";
    SI.HeatFlowRate q "total heat flow 'in'";
    SI.Power pav(start=1);
    SI.HeatFlowRate qav(start=1);

  initial equation
    if av then
      pav = p;
      qav = q;
    end if;

  equation
    heat.ports.T = fill(T_amb, heat.m);
    p = term_p.v*term_p.i;
    q = sum(heat.ports.Q_flow);

    if av then
      der(pav) = (p - pav)/tcst;
      der(qav) = (q - qav)/tcst;
    else
      pav = p;
      qav = q;
    end if;

    if qav < abs(pav) then
      if dir_in then
        eta = if pav > 0 then 100*(pav - qav)/pav else -100*pav/(pav - qav);
      else
        eta = if pav > 0 then 100*pav/(pav + qav) else -100*(pav + qav)/pav;
      end if;
    else
      eta = 0;
    end if;
    annotation (
      defaultComponentName="efficiency",
  Documentation(
          info="<html>
<p>Measures the electric power <tt>p</tt> flowing from 'term_p' to 'term_n' and the total heat inflow <tt>q</tt> at term 'heat'. The efficiency eta in % is then defined by
<pre>
  eta = 100*(p - q)/p     if arrow points into the measured component and q &lt  abs(p)
  eta = 100*p/(p + q)     if arrow points out of the measured component and q &lt  abs(p)
  eta = 0                 else
</pre>
Positive values of eta indicate powerflow in direction of arrow,
negative values of eta indicate powerflow against direction of arrow.</p>
<p>Note: Take care about the above definitions if approximations are used in measured components.<br>
In problematic cases use power sensors electrical and mechanical.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid), Line(points={{0,0},{20,0}}, color={
                0,0,255})}));
  end Efficiency;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model Sensor1Base "Sensor Base, 1-phase"
      extends Ports.Port_p;

    equation
      term.i = zeros(2);
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,70},{70,-70}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-88,0},{40,0}}, color={0,0,255}),
            Line(points={{0,20},{0,90}}, color={135,135,135})}));
    end Sensor1Base;

    partial model Sensor2Base "Sensor Base, 1-phase"
      extends Ports.Port_pn;

    equation
      term_p.v = term_n.v;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,70},{70,-70}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{0,20},{0,90}}, color={135,135,135}),
            Line(points={{-88,0},{-20,0}}, color={0,0,255}),
            Line(points={{0,0},{88,0}}, color={0,0,255}),
            Line(points={{30,20},{70,0},{30,-20}}, color={0,0,255})}));
    end Sensor2Base;

    partial model Meter1Base "Meter base 1 terminal, 1-phase"
      extends Sensor1Base;
      extends Common.Nominal.Nominal;

      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(extent={{-70,70},{70,-70}},
                lineColor={135,135,135})}));
    end Meter1Base;

    partial model Meter2Base "Meter base 2 terminal, 1-phase"
      extends Sensor2Base;
      extends Common.Nominal.Nominal;

      annotation (Icon(graphics={Ellipse(extent={{-70,70},{70,-70}}, lineColor=
                  {135,135,135})}));
    end Meter2Base;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Sensors directly output terminal signals (voltage, current, power).</p>
<p>Meters allow choosing base-units for output variables.</p>
</html>
"));
end Sensors;
