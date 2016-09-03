within PowerSystems.AC3ph;
package Nodes "Nodes and adaptors"
  extends Modelica.Icons.VariantsPackage;

  model Ground "AC Ground, 3-phase dq0"
    extends Ports.Port_p;

  equation
    term.v = zeros(3);
    annotation (
      defaultComponentName="grd1",
  Documentation(
          info="<html>
<p>Zero voltage on all phases of terminal.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-4,60},{4,-60}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid), Line(
            points={{-90,0},{-4,0}},
            color={0,120,120},
            thickness=0.5)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-60,0},{-80,0}},
            color={0,120,120},
            thickness=0.5), Rectangle(
            extent={{-60,20},{-54,-20}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid)}));
  end Ground;

  model GroundOne "Ground, one conductor"

    Interfaces.Electric_p term "positive scalar terminal"
                               annotation (Placement(transformation(extent={{
              -110,-10},{-90,10}})));

  equation
    term.v = 0;
    annotation (
      defaultComponentName="grd1",
  Documentation(
          info="<html>
<p>Zero voltage on terminal.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
         "%name"),
          Rectangle(
            extent={{-4,50},{4,-50}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{-4,0}}, color={0,0,255})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(points={{-60,0},{-80,0}}, color={0,0,255}),
            Rectangle(
            extent={{-60,20},{-54,-20}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid)}));
  end GroundOne;

  model BusBar "Busbar, 3-phase dq0"
    extends Ports.PortBase;

    output PS.Voltage v_norm(stateSelect=StateSelect.never);
    output SI.Angle alpha_v(stateSelect=StateSelect.never);
    Ports.ACdq0_p term "bus bar"
  annotation (Placement(transformation(extent={{-8,-66},{8,66}})));
  protected
    Real[2,2] R=Utilities.Transforms.rotation_dq(term.theta[1]);
    function atan2 = Modelica.Math.atan2;

  equation
    term.i = zeros(3);
    v_norm = sqrt(term.v*term.v);
    alpha_v = atan2(R[:, 2]*term.v[1:2], R[:, 1]*term.v[1:2]);
    annotation (
      defaultComponentName="bus1",
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
         "%name"), Rectangle(
            extent={{-10,80},{10,-80}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Documentation(
          info="<html>
<p>Calculates norm and phase-angle of voltage.</p>
</html>
"));
  end BusBar;

/*
  model Y_Delta "Y_Delta switch, 3-phase dq0"
    extends Ports.PortBase;

    Ports.ACdq0_p term_p
      "connect to non source side of windings"
  annotation (Placement(transformation(extent={{110,-70},{90,-50}})));
    Ports.ACdq0_n term_n
      "connect to source side of windings"
  annotation (Placement(transformation(extent={{-90,-70},{-110,-50}})));
    Interfaces.ElectricV_n switchY_D(     final m=3)
      "connect to switch 'commute' position"
                                        annotation (Placement(transformation(
            extent={{-90,10},{-110,30}})));
    Interfaces.ElectricV_p switchD(     final m=3)
      "connect to switch 'Delta' position"
                                        annotation (Placement(transformation(
            extent={{110,-10},{90,10}})));
    Interfaces.ElectricV_p switchY(     final m=3)
      "connect to switch 'Y' position"  annotation (Placement(transformation(
            extent={{108,30},{88,50}})));
    Interfaces.Electric_n neutral "neutral Y"
                                  annotation (Placement(transformation(
          origin={0,-60},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    Real[3,3] Park = Basic.Transforms.park(
                                          term_p.theta[2]);

  equation
    switchY_D.pin.v = transpose(Park)*term_p.v;
    term_p.i + Park*switchY_D.pin.i = zeros(3);
    switchY.pin.v = fill(neutral.v, 3);
    sum(switchY.pin.i) + neutral.i = 0;
    switchD.pin[{3,1,2}].v = transpose(Park)*term_n.v;
    term_n.i + Park*switchD.pin[{3,1,2}].i = zeros(3);
    annotation (
      defaultComponentName="Y_Delta",
  Documentation(
          info="<html>
<p>Can be used for detailed investigation of Y-Delta switching.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
             "%name"),
          Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-80,10},{80,-30}},
            lineColor={0,0,255},
            textString=
                 "Delta"),
          Text(
            extent={{-80,50},{80,10}},
            lineColor={0,0,255},
            textString=
                 "Y"),
          Line(points={{58,30},{88,40}}, color={0,0,255}),
          Line(points={{58,-10},{88,0}}, color={0,0,255})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-96,26},{4,14}},
            lineColor={0,0,255},
            textString=
                 "< commute Y - D"),
          Text(
            extent={{46,6},{86,-6}},
            lineColor={0,0,255},
            textString=
                 "Delta >"),
          Text(
            extent={{54,46},{94,34}},
            lineColor={0,0,255},
            textString=
                 "Y >"),
          Text(
            extent={{31,-54},{92,-66}},
            lineColor={0,120,120},
            textString=
                 "winding >"),
          Text(
            extent={{-40,-74},{40,-86}},
            lineColor={0,0,255},
            textString=
                 "neutral (Y)"),
          Line(
            points={{-100,20},{-110,20},{-110,60},{-26,60}},
            color={0,0,0},
            pattern=LinePattern.Dot),
          Line(
            points={{99,40},{110,40},{110,60},{29,60}},
            color={0,0,0},
            pattern=LinePattern.Dot),
          Line(
            points={{99,0},{120,0},{120,80},{29,80}},
            color={0,0,0},
            pattern=LinePattern.Dot),
          Ellipse(extent={{23,83},{29,77}}, lineColor={0,0,0}),
          Ellipse(extent={{23,63},{29,57}}, lineColor={0,0,0}),
          Ellipse(
            extent={{-29,63},{-23,57}},
            lineColor={0,0,0},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-9,61},{-7,59}},
            lineColor={0,0,0},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid),
          Line(points={{-26,60},{-8,60},{12,80},{24,80}}, color={0,0,0}),
          Line(points={{12,60},{24,60}}, color={0,0,0}),
          Text(
            extent={{-60,100},{60,90}},
            lineColor={0,0,0},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "Use with a commuting switch"),
          Text(
            extent={{-91,-54},{-31,-66}},
            lineColor={255,0,0},
            textString=
                 "< winding"),
          Text(
            extent={{-85,-64},{-25,-76}},
            lineColor={255,0,0},
            textString=
                 "source-side")}));
  end Y_Delta;
*/

  model Ynode "Y-node with neutral-access, 3-phase dq0"
    extends Ports.Port_p;

    Interfaces.Electric_n neutral "neutral Y"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));

  equation
    term.v[3] = sqrt(3)*neutral.v;
    term.i[1:2] = {0, 0};
    neutral.i + sqrt(3)*term.i[3] = 0;
  annotation (
    defaultComponentName="Ynode",
      Documentation(
        info="<html>
<p>Can be used for grounding neutral of AC dq0 3phase components.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,80},{80,-80}},
            lineColor={255,255,255},
            pattern=LinePattern.None,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{14,6},{26,-6}},
            lineColor={0,100,100},
            fillColor={0,100,100},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,60},{-40,60},{20,0},{-40,-60},{-80,-60}}, color={0,
                100,100}),
          Line(points={{-80,0},{20,0}}, color={0,100,100}),
          Line(points={{26,0},{90,0}})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-80,0},{20,0}}, color={0,100,100}),
          Line(points={{-80,60},{-40,60},{20,0},{-40,-60},{-80,-60}}, color={0,
                100,100}),
          Ellipse(
            extent={{14,6},{26,-6}},
            lineColor={0,100,100},
            fillColor={0,100,100},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{10,-10},{50,-30}},
            lineColor={0,100,100},
            textString=
           "neutral point"),
          Line(points={{26,0},{90,0}})}));
  end Ynode;

  model ResistiveGround "Y-node with neutral-access, 3-phase dq0"
    extends Ports.Yport_p;
    extends Common.Nominal.Nominal;

    parameter SIpu.Resistance r_n=0 "resistance neutral to grd";
  protected
    final parameter Real R_base=Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);
    final parameter SI.Resistance R_n=r_n*R_base;

  equation
    v = zeros(3);
    R_n*i_n = v_n "equation neutral to ground";
    annotation (
      defaultComponentName="resGrd",
  Documentation(
          info="<html>
<p>Can be used for grounding neutral of AC dq0 3phase components.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{94,30},{100,-30}},
            lineColor={128,128,128},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,0},{-40,0}}, color={0,100,100}),
          Rectangle(
            extent={{-20,30},{94,-30}},
            lineColor={0,0,0},
            fillColor={255,255,255}),
          Line(points={{-80,60},{-60,60},{-40,0},{-60,-60},{-80,-60}}, color={0,
                100,100}),
          Ellipse(
            extent={{-46,6},{-34,-6}},
            lineColor={0,100,100},
            fillColor={0,100,100},
            fillPattern=FillPattern.Solid),
          Line(points={{-34,0},{-20,0}})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{96,4},{100,-4}},
            lineColor={128,128,128},
            lineThickness=0.5,
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{70,4},{96,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{59,2},{63,-2}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{40,-20},{80,-40}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
           "neutral point")}));
  end ResistiveGround;

  model InductiveGround "Y-node with neutral-access, 3-phase dq0"
    extends Ports.Yport_p;
    extends Common.Nominal.NominalAC;

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));

    parameter SIpu.Reactance x_n=1 "reactance neutral to grd";
    parameter SIpu.Resistance r_n=0 "resistance neutral to grd";
  protected
    final parameter Real R_base=Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);
    final parameter SI.Inductance L_n=x_n*R_base/(2*pi*f_nom);
    final parameter SI.Resistance R_n=r_n*R_base;

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(i_n) = 0;
    end if;

  equation
    v = zeros(3);
    if dynType <> Types.Dynamics.SteadyState then
      L_n*der(i_n) + R_n*i_n = v_n;
    else
      R_n*i_n = v_n;
    end if;
    annotation (
      defaultComponentName="indGrd",
  Documentation(
          info="<html>
<p>Can be used for grounding neutral of AC dq0 3phase components.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-80,0},{-40,0}}, color={0,100,100}),
          Line(points={{-80,60},{-60,60},{-40,0},{-60,-60},{-80,-60}}, color={0,
                100,100}),
          Ellipse(
            extent={{-46,6},{-34,-6}},
            lineColor={0,100,100},
            fillColor={0,100,100},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{94,30},{100,-30}},
            lineColor={128,128,128},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,30},{10,-30}},
            lineColor={0,0,0},
            fillColor={255,255,255}),
          Rectangle(
            extent={{10,30},{94,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-34,0},{-20,0}})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{70,4},{76,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{76,4},{96,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{96,4},{100,-4}},
            lineColor={128,128,128},
            lineThickness=0.5,
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{59,2},{63,-2}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{40,-20},{80,-40}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
           "neutral point")}));
  end InductiveGround;

  model CapacitiveGround "Y-node with neutral-access, 3-phase dq0"
    extends Ports.Yport_p;
    extends Common.Nominal.NominalAC;

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));

    parameter SIpu.Susceptance b_n=1 "susceptance neutral to grd";
    parameter SIpu.Conductance g_n=0 "conductance neutral to grd";
  protected
    final parameter Real R_base=Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);
    final parameter SI.Capacitance C_n=b_n/(R_base*2*pi*f_nom);
    final parameter SI.Conductance G_n=g_n/R_base;

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(i_n) = 0;
    end if;

  equation
    v = zeros(3);
    if dynType <> Types.Dynamics.SteadyState then
      C_n*der(v_n) + G_n*v_n = i_n;
    else
      G_n*v_n = i_n;
    end if;
    annotation (
      defaultComponentName="capGrd",
  Documentation(
          info="<html>
<p>Can be used for grounding neutral of AC dq0 3phase components.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-80,0},{-40,0}}, color={0,100,100}),
          Line(points={{-80,60},{-60,60},{-40,0},{-60,-60},{-80,-60}}, color={0,
                100,100}),
          Ellipse(
            extent={{-46,6},{-34,-6}},
            lineColor={0,100,100},
            fillColor={0,100,100},
            fillPattern=FillPattern.Solid),
          Line(points={{-34,0},{0,0}}),
          Rectangle(
            extent={{8,60},{32,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,60},{8,-60}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{32,60},{40,-60}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{92,16},{96,-16}},
            lineColor={215,215,215},
            pattern=LinePattern.None,
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,16},{92,-16}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{96,16},{100,-16}},
            lineColor={128,128,128},
            lineThickness=0.5,
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Line(points={{70,0},{88,0}}, color={0,0,255}),
          Ellipse(
            extent={{59,2},{63,-2}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{40,-20},{80,-40}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
           "neutral point")}));
  end CapacitiveGround;

model Y_OnePhase "Terminator, ACdq0 to an, bn, cn"
  extends Ports.Port_p;
  extends Icons.Adaptor_dq0;

  Interfaces.Electric_p neutral "neutral Y"
                                annotation (Placement(transformation(
          origin={0,-80},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  AC1ph_DC.Ports.TwoPin_n plug_a "phase a and neutral"
                                                 annotation (Placement(
          transformation(extent={{90,30},{110,50}})));
  AC1ph_DC.Ports.TwoPin_n plug_b "phase b and neutral"
                                                 annotation (Placement(
          transformation(extent={{90,-10},{110,10}})));
  AC1ph_DC.Ports.TwoPin_n plug_c "phase c and neutral"
                                                 annotation (Placement(
          transformation(extent={{90,-50},{110,-30}})));
  protected
    Real[3,3] P=Utilities.Transforms.park(term.theta[2]);

equation
  plug_a.v = cat(1, transpose(P[:, 1:1])*term.v, {neutral.v});
  plug_b.v = cat(1, transpose(P[:, 2:2])*term.v, {neutral.v});
  plug_c.v = cat(1, transpose(P[:, 3:3])*term.v, {neutral.v});
  term.i + P*{plug_a.i[1], plug_b.i[1], plug_c.i[1]} = zeros(3);
annotation (defaultComponentName = "Y_abcn",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,100},{100,80}},
            lineColor={0,120,120},
            textString=
                 "> 1-phase")}),
      Documentation(info="<html>
</html>
"));
end Y_OnePhase;

model ACdq0_a_b_c "Adaptor ACdq0 to pins a, b, c"
  extends Ports.Port_p;
  extends PowerSystems.Icons.Adaptor_dq0;

  Interfaces.Electric_n term_a "phase a"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
  Interfaces.Electric_n term_b "phase b"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  Interfaces.Electric_n term_c "phase c"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
  protected
    Real[3,3] P=Utilities.Transforms.park(term.theta[2]);

equation
  {term_a.v,term_b.v,term_c.v} = transpose(P)*term.v;
  term.i + P*{term_a.i,term_b.i,term_c.i} = zeros(3);
  annotation (defaultComponentName = "acdq0_a_b_c",
      Documentation(info="<html>
</html>
"));
end ACdq0_a_b_c;

/*
model ACdq0_abc "Adaptor ACdq0 to 3-vector abc"
  extends Ports.Port_p;
  extends Basic.Icons.Adaptor_dq0;

  Interfaces.ElectricV_n term_abc(     final m=3) "phase abc vector-pin"
                                                      annotation (Placement(
          transformation(extent={{90,-10},{110,10}})));
  protected
  Real[3,3] P = Basic.Transforms.park(
                                     term.theta[2]);

equation
  term_abc.pin.v = transpose(P)*term.v;
  term.i + P*term_abc.pin.i = zeros(3);
  annotation (defaultComponentName = "acdq0_abc",
      Documentation(info="<html>
</html>
"));
end ACdq0_abc;
*/

  model DefReference "Defines reference frame, 3phase dq0"
    extends Ports.PortBase;
    outer System system;

    Ports.ACdq0_p term "bus bar"
  annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
    Modelica.Blocks.Interfaces.RealInput theta "absolute angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  equation
    term.i = zeros(3);
    Connections.potentialRoot(term.theta);
    if Connections.isRoot(term.theta) then
      term.theta = if system.synRef then {0, theta} else {theta, 0};
    end if;
    annotation (defaultComponentName="reference",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
                 "%name"),
          Rectangle(
            extent={{-10,80},{10,-80}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,85,85}),
          Rectangle(
            extent={{-20,120},{20,80}},
            lineColor={213,170,255},
            fillColor={213,170,255},
            fillPattern=FillPattern.Solid)}),
      Documentation(info="<html>
<p>Explicit definition of relative-angle term.theta[1] and reference-angle term.theta[2]<br>
(only for advanced use needed).</p>
</html>"));
  end DefReference;

  model Break "Breaks transmission of term.theta, 3phase dq0"
    extends Ports.PortBase;

    Ports.ACdq0_p term_p "positive terminal"
  annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    Ports.ACdq0_n term_n "negative terminal"
  annotation (Placement(transformation(extent={{20,-10},{40,10}})));

  equation
    term_p.v = term_n.v;
    term_p.i + term_n.i = zeros(3);
    annotation (defaultComponentName="break1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
                 "%name"), Rectangle(
            extent={{-10,80},{10,-80}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,0})}),
      Documentation(info="<html>
<p>Breaks explicitly transfer of angle theta from term_p to term_n.</p>
<p>The electric connections remain the same as in Base.PortsACdq0.Port_pn, whereas the equation</p>
<pre>  term_n.theta = term_p.theta</pre>
<p>is omitted together with the function 'Connections.branch'<br>
(only for advanced use needed).</p>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-50,60},{50,40}},
            lineColor={0,0,0},
            textString=
                 "theta not transmitted"),
          Line(
            points={{-25,0},{-10,0}},
            color={0,0,0},
            arrow={Arrow.None,Arrow.Filled}),
          Line(
            points={{25,0},{10,0}},
            color={0,0,0},
            arrow={Arrow.None,Arrow.Filled}),
          Line(points={{0,20},{0,-20}}, color={0,0,0})}));
  end Break;

annotation (preferredView="info",
    Documentation(info="<html>
</html>
"));
end Nodes;
