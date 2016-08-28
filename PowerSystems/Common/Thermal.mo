within PowerSystems.Common;
package Thermal "Thermal boundary and adaptors"
  extends Modelica.Icons.VariantsPackage;

  model BdCond "Default (Neumann) boundary condition, scalar port"
    extends Partials.BdCondBase;

    Interfaces.Thermal_p heat "heat port"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    heat.T = T_amb;
    annotation (defaultComponentName="bdCond",
      Documentation(info="<html>
<p>Deault thermal boundary condition for applications where the thermal output of heat-producing components is not needed.<br>
Boundary has fixed temperature T = 0.</p>
</html>

"));
  end BdCond;

  model BdCondV "Default (Neumann) boundary condition, vector port"

    parameter Integer m(final min=1)=1 "dimension of heat port";
    extends Partials.BdCondBase;
    PowerSystems.Interfaces.ThermalV_p heat(
                                    final m=m) "vector heat port"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    heat.ports.T = fill(T_amb, heat.m);
    annotation (defaultComponentName="bdCond",
      Documentation(info="<html>
<p>Deault thermal boundary condition for applications where the thermal output of heat-producing components is not needed.<br>
Boundary has fixed temperature T = 0.</p>
</html>

"));
  end BdCondV;

  model Boundary "Boundary model, scalar port"
    extends Partials.BoundaryBase;

    output SI.HeatFlowRate Q_flow;
    output SI.HeatFlowRate Qav_flow=q if av;
    PowerSystems.Interfaces.Thermal_p heat "heat port"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    SI.HeatFlowRate q;

  equation
    heat.T = T;
    Q_flow = heat.Q_flow;

    if ideal then
      T = T_amb;
    else
      C*der(T) = Q_flow - G*(T - T_amb);
    end if;

    if av then
      der(q) = (Q_flow - q)/tcst;
    else
      q = 0;
    end if;
    annotation (defaultComponentName="boundary",
      Documentation(info="<html>
<p>Ideal cooling (ideal=true):<br>
Boundary has fixed temperature T_amb.</p>
<p>Cooling by linear heat transition (ideal=false):<br>
Boundary has one common variable temperature T.<br>
T is determined by the difference between heat inflow at the heat-port and outflow=G*(T-T_amb) towards ambient, according to a given heat capacity C.</p>
<p>The time-average equation
<pre>  der(q) = (Q_flow - q)/tcst</pre>
is equivalent to the heat equation
<pre>  C*der(T) = Q_flow - G*(T - T_amb)</pre>
at constant ambient temperature. The correspondence is
<pre>
  tcst = C/G
  q = G*(T - T_amb)
</pre></p>
</html>
"));
  end Boundary;

  model BoundaryV "Boundary model, vector port"

    parameter Boolean add_up=true "add up Q_flow at equal T";
    parameter Integer m(final min=1)=1 "dimension of heat port";
    extends Partials.BoundaryBase;
    output SI.HeatFlowRate[if add_up then 1 else m] Q_flow;
    output SI.HeatFlowRate[if add_up then 1 else m] Qav_flow=q if av;
    PowerSystems.Interfaces.ThermalV_p heat(
                                    final m=m) "vector heat port"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    SI.HeatFlowRate[if add_up then 1 else m] q;

  equation
    heat.ports.T = fill(T, heat.m);

    if add_up then
       Q_flow = {sum(heat.ports.Q_flow)};
    else
       Q_flow[1:m] = heat.ports.Q_flow;
    end if;

    if ideal then
      T = T_amb;
    else
      C*der(T) = sum(Q_flow) - G*(T - T_amb);
    end if;

    if av then
      der(q) = (Q_flow - q)/tcst;
    else
      q = zeros(if add_up then 1 else m);
    end if;
    annotation (defaultComponentName="boundary",
      Documentation(info="<html>
<p>Ideal cooling (ideal=true):<br>
Boundary has fixed temperature T_amb.</p>
<p>Cooling by linear heat transition (ideal=false):<br>
Boundary has one common variable temperature T.<br>
T is determined by the difference between heat inflow at the heat-port and outflow=G*(T-T_amb) towards ambient, according to a given heat capacity C.</p>
<p>The time-average equation
<pre>  der(q) = (Q_flow - q)/tcst</pre>
is equivalent to the heat equation
<pre>  C*der(T) = Q_flow - G*(T - T_amb)</pre>
at constant ambient temperature. The correspondence is
<pre>
  tcst = C/G
  q = G*(T - T_amb)
</pre></p>
</html>
"));
  end BoundaryV;

  model Heat_a_b_ab "Adaptor 2 x Thermal (scalar) to ThermalV (vector)"

    PowerSystems.Interfaces.Thermal_p port_a "scalar port a"
      annotation (Placement(transformation(
          origin={-40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.Thermal_p port_b "scalar port b"
      annotation (Placement(transformation(
          origin={40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_n port_ab(
                                       final m=2) "vector port {a,b}"
      annotation (Placement(transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    {port_a.T, port_b.T} = port_ab.ports.T;
    {port_a.Q_flow, port_b.Q_flow} + port_ab.ports.Q_flow = zeros(2);
    annotation (defaultComponentName="heat_adapt",
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={Line(points={{-40,-40},{-40,0},{-5,0},{-5,
                40}}, color={176,0,0}), Line(points={{40,-40},{40,0},{5,0},{5,
                40}}, color={176,0,0})}),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Rectangle(
            extent={{-60,40},{60,-40}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-38},{-40,0},{-10,0},{-10,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{10,0},{10,40}}, color={176,0,0}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Text(
            extent={{-100,-60},{-60,-100}},
            lineColor={176,0,0},
            textString="a"),
          Text(
            extent={{60,-60},{100,-100}},
            lineColor={176,0,0},
            textString="b")}),
      Documentation(info="<html>
</html>"));
  end Heat_a_b_ab;

  model Heat_a_b_c_abc "Adaptor 3 x Thermal (scalar) to ThermalV (vector)"

    PowerSystems.Interfaces.Thermal_p port_a "scalar port a"
      annotation (Placement(transformation(
          origin={-40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.Thermal_p port_b "scalar port b"
      annotation (Placement(transformation(
          origin={0,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.Thermal_p port_c "scalar port c"
      annotation (Placement(transformation(
          origin={40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_n port_abc(
                                        final m=3) "vector port {a,b,c}"
                                                 annotation (Placement(
          transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    {port_a.T, port_b.T, port_c.T} = port_abc.ports.T;
    {port_a.Q_flow, port_b.Q_flow, port_c.Q_flow} + port_abc.ports.Q_flow = zeros(3);
    annotation (defaultComponentName="heat_adapt",
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={
          Line(points={{-40,-40},{-40,0},{-5,0},{-5,40}}, color={176,0,0}),
          Line(points={{0,-40},{0,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{5,0},{5,40}}, color={176,0,0})}),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Rectangle(
            extent={{-60,40},{60,-40}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-38},{-40,0},{-10,0},{-10,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{10,0},{10,40}}, color={176,0,0}),
          Line(points={{0,-40},{0,40}}, color={176,0,0}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Text(
            extent={{-100,-60},{-60,-100}},
            lineColor={176,0,0},
            textString="a"),
          Text(
            extent={{60,-60},{100,-100}},
            lineColor={176,0,0},
            textString="c")}),
      Documentation(info="<html>
</html>"));
  end Heat_a_b_c_abc;

  model HeatV_a_b_ab "Adaptor 2 x ThermalV (vector) to ThermalV (vector)"

    parameter Integer[2] m={1,1} "dimension {port_a, port_b}";
    PowerSystems.Interfaces.ThermalV_p port_a(
                                      final m=m[1]) "vector port a"
      annotation (Placement(transformation(
          origin={-40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_p port_b(
                                      final m=m[2]) "vector port b"
      annotation (Placement(transformation(
          origin={40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_n port_ab(
                                       final m=sum(m)) "vector port {a,b}"
      annotation (Placement(transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    cat(1, port_a.ports.T, port_b.ports.T) = port_ab.ports.T;
    cat(1, port_a.ports.Q_flow, port_b.ports.Q_flow) + port_ab.ports.Q_flow = zeros(sum(m));
    annotation (defaultComponentName="heat_adapt",
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={Line(points={{-40,-40},{-40,0},{-5,0},{-5,
                40}}, color={176,0,0}), Line(points={{40,-40},{40,0},{5,0},{5,
                40}}, color={176,0,0})}),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Rectangle(
            extent={{-60,40},{60,-40}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-38},{-40,0},{-10,0},{-10,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{10,0},{10,40}}, color={176,0,0}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Text(
            extent={{-100,-60},{-60,-100}},
            lineColor={176,0,0},
            textString="a"),
          Text(
            extent={{60,-60},{100,-100}},
            lineColor={176,0,0},
            textString="b")}),
      Documentation(info="<html>
</html>"));
  end HeatV_a_b_ab;

  model HeatV_a_b_c_abc "Adaptor 3 x Thermal (scalar) to ThermalV (vector)"

    parameter Integer[3] m={1,1,1} "dimension {port_a, port_b, port_c}";
    PowerSystems.Interfaces.ThermalV_p port_a(
                                      final m=m[1]) "scalar port a"
      annotation (Placement(transformation(
          origin={-40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_p port_b(
                                      final m=m[2]) "scalar port b"
      annotation (Placement(transformation(
          origin={0,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_p port_c(
                                      final m=m[3]) "scalar port c"
      annotation (Placement(transformation(
          origin={40,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.ThermalV_n port_abc(
                                        final m=sum(m)) "vector port {a,b,c}"
                                                 annotation (Placement(
          transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    cat(1, port_a.ports.T, port_b.ports.T, port_c.ports.T) = port_abc.ports.T;
    cat(1, port_a.ports.Q_flow, port_b.ports.Q_flow, port_c.ports.Q_flow) + port_abc.ports.Q_flow = zeros(sum(m));
    annotation (defaultComponentName="heat_adapt",
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={
          Line(points={{-40,-40},{-40,0},{-5,0},{-5,40}}, color={176,0,0}),
          Line(points={{0,-40},{0,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{5,0},{5,40}}, color={176,0,0})}),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Rectangle(
            extent={{-60,40},{60,-40}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-38},{-40,0},{-10,0},{-10,40}}, color={176,0,0}),
          Line(points={{40,-40},{40,0},{10,0},{10,40}}, color={176,0,0}),
          Line(points={{0,-40},{0,40}}, color={176,0,0}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Text(
            extent={{-100,-60},{-60,-100}},
            lineColor={176,0,0},
            textString="a"),
          Text(
            extent={{60,-60},{100,-100}},
            lineColor={176,0,0},
            textString="c")}),
      Documentation(info="<html>
</html>"));
  end HeatV_a_b_c_abc;

  model HeatV_S "Collector ThermalV (vector) to Thermal (scalar)"

    parameter Integer m(final min=1) = 1 "dimension of port_a";
    PowerSystems.Interfaces.ThermalV_p port_a(
                                      final m=m) "vector port"
      annotation (Placement(transformation(
          origin={0,-58},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    PowerSystems.Interfaces.Thermal_n port_b "scalar port"
      annotation (Placement(transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    port_a.ports.T = fill( port_b.T, port_a.m);
    sum(port_a.ports.Q_flow) + port_b.Q_flow = 0;
    annotation (defaultComponentName="heat_collect",
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Rectangle(
            extent={{-60,40},{60,-40}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-10,-40},{-10,0},{0,0},{0,40}}, color={176,0,0}),
          Line(points={{10,-40},{10,0},{0,0},{0,40}}, color={176,0,0}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name")}),
      Documentation(info="<html>
The temperatures of all vector-heat ports and the temperature of the scalar heat port are equal.<br>
The total of all vector-heat in-flows is equal to the scalar out-flow.
</html>"));
  end HeatV_S;

package Partials "Partial models"
  extends Modelica.Icons.BasesPackage;

model BdCondBase "Default (Neumann) boundary condition base"

  parameter SI.Temperature T_amb=300 "ambient temperature";

  annotation (defaultComponentName="bdCond",
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={
            Text(
              extent={{-100,0},{100,-40}},
              lineColor={0,0,0},
              textString="%name"),
            Rectangle(
              extent={{-80,-50},{80,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Line(points={{-80,-50},{80,-50}}, color={255,255,255})}),
    Documentation(info="<html>
<p>Deault thermal boundary condition for applications where the thermal output of heat-producing components is not needed.<br>
Boundary has fixed temperature T = 0.</p>
</html>

"));

end BdCondBase;

partial model BoundaryBase "Boundary model base"

  parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
    annotation(Evaluate=true, Dialog(tab="Mode"));

  parameter Boolean av=false "time average heat-flow"  annotation(Evaluate=true,Dialog(group="Options"));
  parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
                                                  annotation(Evaluate=true, Dialog(group="Options",enable=av));
  parameter Boolean ideal=true "ideal cooling";
  parameter SI.Temperature T_amb=300 "ambient temperature";
  parameter SI.HeatCapacity C=1 "heat capacity cp*m" annotation(Dialog(enable=not ideal));
  parameter SI.ThermalConductance G=1 "thermal conductance to ambient" annotation(Dialog(enable=not ideal));
  SI.Temperature T(start=300) "temperature";

    protected
  outer System system;

initial equation
  if not ideal and dynType == Types.Dynamics.SteadyInitial then
    der(T) = 0;
  end if;
  annotation (defaultComponentName="boundary",
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={
            Rectangle(
              extent={{-80,-50},{80,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{-80,40},{80,-50}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-50,20},{-40,40},{-30,20}}, color={191,0,0}),
            Line(points={{-10,20},{0,40},{10,20}}, color={191,0,0}),
            Line(points={{30,20},{40,40},{50,20}}, color={191,0,0}),
            Line(points={{-40,-50},{-40,-40}}, color={191,0,0}),
            Line(points={{0,-50},{0,-40}}, color={191,0,0}),
            Line(points={{40,-50},{40,-40}}, color={191,0,0}),
            Line(points={{-40,0},{-40,40}}, color={191,0,0}),
            Line(points={{0,0},{0,40}}, color={191,0,0}),
            Line(points={{40,0},{40,40}}, color={191,0,0}),
            Text(
              extent={{-100,0},{100,-40}},
              lineColor={0,0,0},
              textString="%name")}),
    Documentation(info="<html>
</html>
"));
end BoundaryBase;
  annotation (
    Documentation(
            info="<html>
</html>
"));
end Partials;

  annotation (preferredView="info", Documentation(info="<html>
<p>Auxiliary thermal boundary-conditions, boundary-elements and adptors.</p>
</html>"));
end Thermal;
