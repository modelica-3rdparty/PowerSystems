within PowerSystems.AC3ph;
package Ports "AC three-phase ports dq0 representation"
  extends Modelica.Icons.InterfacesPackage;

partial model PortBase "base model adapting Spot to PowerSystems"
  package PS = PackagePhaseSystem;
  function j = PS.j annotation(Inline=true);
  function jj = PS.jj annotation(Inline=true);
  function j_dq0 = PhaseSystems.ThreePhase_dq0.j annotation(Inline=true);
  function jj_dq0 = PhaseSystems.ThreePhase_dq0.jj annotation(Inline=true);
end PortBase;

connector ACdq0_p "AC terminal, 3-phase dq0 ('positive')"
  extends Interfaces.Terminal(redeclare package PhaseSystem =
          PackagePhaseSystem);
  annotation (defaultComponentName = "term_p",
      Documentation(info="<html>
<p>AC connector with vector variables in dq0-representation, positive.</p>
</html>"),
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,120,120},
              fillColor={0,120,120},
              fillPattern=FillPattern.Solid), Text(
              extent={{-60,60},{60,-60}},
              lineColor={255,255,255},
              textString="dq0")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{0,50},{100,-50}},
              lineColor={0,120,120},
              fillColor={0,120,120},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{12,40},{90,-40}},
              lineColor={255,255,255},
              pattern=LinePattern.None,
              textString="dq0"),
            Text(
              extent={{-120,120},{100,60}},
              lineColor={0,120,120},
              textString="%name")}));
end ACdq0_p;

connector ACdq0_n "AC terminal, 3-phase dq0 ('negative')"
  extends Interfaces.Terminal(redeclare package PhaseSystem =
          PackagePhaseSystem);
  annotation (defaultComponentName = "term_n",
      Documentation(info="<html>
<p>AC connector with vector variables in dq0-representation, negative.</p>
</html>"),
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,120,120},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Text(
              extent={{-60,60},{60,-60}},
              lineColor={0,120,120},
              textString="dq0")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-100,50},{0,-50}},
              lineColor={0,120,120},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-90,40},{-10,-40}},
              lineColor={0,120,120},
              textString="dq0"),
            Text(
              extent={{-100,120},{120,60}},
              lineColor={0,120,120},
              fillColor={0,100,100},
              fillPattern=FillPattern.Solid,
              textString="%name")}));
end ACdq0_n;

partial model Port_p "AC one port 'positive', 3-phase"
  extends PortBase;

  Ports.ACdq0_p term "positive terminal"
                          annotation (Placement(transformation(extent={{-110,
              -10},{-90,10}})));
  annotation (
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
              {100,100}}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString="%name")}),
    Documentation(info="<html></html>"));
end Port_p;

partial model Port_n "AC one port 'negative', 3-phase"
  extends PortBase;

  Ports.ACdq0_n term "negative terminal"
annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  annotation (
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
              {100,100}}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString="%name")}),
    Documentation(info="<html></html>"));
end Port_n;

partial model Port_f "AC one port 'fault', 3-phase"
  extends PortBase;

  Ports.ACdq0_p term "fault terminal"
annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  annotation (
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
              {100,100}}), graphics={Text(
            extent={{-100,130},{100,90}},
            lineColor={0,0,0},
            textString="%name")}),
    Documentation(info="<html></html>"));
end Port_f;

partial model Port_p_n "AC two port, 3-phase"
  extends PortBase;

  Ports.ACdq0_p term_p "positive terminal"
annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
  Ports.ACdq0_n term_n "negative terminal"
annotation (Placement(transformation(extent={{90,-10},{110,10}})));
equation
  Connections.branch(term_p.theta, term_n.theta);
  term_n.theta = term_p.theta;
  annotation (
Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
          graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString="%name")}),
Documentation(info="<html>
</html>"));
end Port_p_n;

partial model Port_pn "AC two port 'current_in = current_out', 3-phase"
  extends Port_p_n;

equation
  term_p.i + term_n.i = zeros(PS.n);
  annotation (
Documentation(info="<html>
</html>"));
end Port_pn;

partial model Port_p_n_f "AC three port, 3-phase"
  extends Port_p_n;

  Ports.ACdq0_n term_f "fault terminal"
annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

equation
  Connections.branch(term_p.theta, term_f.theta);
  term_f.theta = term_p.theta;
  annotation (
Documentation(info="<html>
</html>"));
end Port_p_n_f;

partial model Yport_p "AC one port Y topology 'positive'"
  extends Port_p;

  PS.Voltage[PS.n] v "voltage terminal to neutral";
  PS.Current[PS.n] i "current terminal in";
  PS.Voltage v_n(final start=0) "voltage neutral";
  PS.Current i_n(final start=0) "current neutral";

equation
  v = term.v - PS.map({0, 0, sqrt(3)*v_n});
  term.i = i;
  if PS.n > 2 then
    i_n = sqrt(3)*term.i[3];
  else
    i_n = 0;
  end if;
  annotation (  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{
              -100,-100},{100,100}}), graphics={Line(points={{30,-16},{52,-16},
                {62,0},{52,16},{30,16}}, color={0,0,255}), Line(points={{30,0},
                {70,0}}, color={0,0,255})}),
    Documentation(info="<html>
<p>Defines Y-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v:     voltage across conductor
  i:     current through conductor
  v_n:   voltage neutral point
  i_n:   current neutral to ground
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}
  i = term.i
  i_n = sqrt(3)*term.i[3]
</pre>
</html>"));
end Yport_p;

partial model Yport_n "AC one port Y topology 'negative'"
  extends Port_n;

  PS.Voltage[PS.n] v "voltage terminal to neutral";
  PS.Current[PS.n] i "current terminal in";
  PS.Voltage v_n(final start=0) "voltage neutral";
  PS.Current i_n(final start=0) "current neutral";

equation
  v = term.v - PS.map({0, 0, sqrt(3)*v_n});
  term.i = i;
  if PS.n > 2 then
    i_n = sqrt(3)*term.i[3];
  else
    i_n = 0;
  end if;
  annotation (  Diagram(graphics={Line(points={{-30,-16},{-52,-16},{-62,0},{-52,
                16},{-30,16}}, color={0,0,255}), Line(points={{-70,0},{-30,0}},
              color={0,0,255})}),
    Documentation(info="<html>
<p>Defines Y-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v:     voltage across conductor
  i:     current through conductor
  v_n:   voltage neutral point
  i_n:   current neutral to ground
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}
  i = term.i
  i_n = sqrt(3)*term.i[3]
</pre>
</html>"));
end Yport_n;

partial model YDport_p "AC one port Y or Delta topology 'positive'"
  extends Port_p;

/*
  replaceable Topology.Y top "Y or Delta topology"
    annotation (                        choices(
    choice(redeclare PowerSystems.AC3ph.Ports.Topology.Y top "Y"),
    choice(redeclare PowerSystems.AC3ph.Ports.Topology.Delta top "Delta")),
        Placement(transformation(extent={{30,-20},{70,20}})));
*/

  replaceable model Topology_p = Topology.Y constrainedby Topology.TopologyBase
      "Y or Delta topology"
    annotation(choices(
    choice(redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Y "Y"),
    choice(redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta
            "Delta")));
  Topology_p top(v_cond=v, i_cond=i, v_n=v_n)
    annotation (Placement(transformation(extent={{30,-20},{70,20}})));

  PS.Voltage[PS.n] v "voltage conductor";
  PS.Current[PS.n] i "current conductor";
  PS.Voltage[n_n] v_n "voltage neutral";
  PS.Current[n_n] i_n=top.i_n "current neutral to ground";
  protected
  final parameter Integer n_n=top.n_n
                              annotation(Evaluate=true);

equation
  term.v = top.v_term;
  term.i = top.i_term;
  annotation (
    Documentation(info="<html>
<p>Defines Y- and Delta-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v:     voltage across conductor
  i:     current through conductor
  v_n:   voltage neutral point if Y-topology
  i_n:   current neutral to ground if Y-topology
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}
  term.i = i
  i_n = sqrt(3)*term.i[3]
</pre>
<p>Relations Delta-topology, (<tt>v, i</tt>: phase terminal to phase terminal)</p>
<pre>
  v[1:2] = sqrt(3)*Rot*term.v[1:2]
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(Rot)*i[1:2]
  term.i[3] = 0
  with Rot = rotation_30deg
</pre>
</html>
"));
end YDport_p;

partial model YDport_n "AC one port Y or Delta topology 'positive'"
  extends Port_n;

  replaceable model Topology_n = Topology.Y constrainedby Topology.TopologyBase
      "Y or Delta topology"
    annotation(choices(
    choice(redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Y "Y"),
    choice(redeclare model Topology_p = PowerSystems.AC3ph.Ports.Topology.Delta
            "Delta")));
  Topology_n top(v_cond=v, i_cond=i, v_n=v_n) "Y or Delta topology"
    annotation (Placement(transformation(extent={{-30,-20},{-70,20}})));
  PS.Voltage[PS.n] v "voltage conductor";
  PS.Current[PS.n] i "current conductor";
  PS.Voltage[n_n] v_n "voltage neutral";
  PS.Current[n_n] i_n=top.i_n "current neutral to ground";
  protected
  final parameter Integer n_n=top.n_n
                              annotation(Evaluate=true);

equation
  term.v = top.v_term;
  term.i = top.i_term;
  annotation (
    Documentation(info="<html>
<p>Defines Y- and Delta-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v:     voltage across conductor
  i:     current through conductor
  v_n:   voltage neutral point if Y-topology
  i_n:   current neutral to ground if Y-topology
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}
  term.i = i
  i_n = sqrt(3)*term.i[3]
</pre>
<p>Relations Delta-topology, (<tt>v, i</tt>: phase terminal to phase terminal)</p>
<pre>
  v[1:2] = sqrt(3)*Rot*term.v[1:2]
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(Rot)*i[1:2]
  term.i[3] = 0
  with Rot = rotation_30deg
</pre>
</html>
"));
end YDport_n;

partial model Y_Dport_p "AC two port, switcheable Y-Delta topology"
  extends Port_p;

  replaceable model Topology_p = Topology.Y_Delta constrainedby
      Topology.TopologyBase "Y-Delta switcheable"
    annotation(choices(
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Y_Delta "Y_Delta"),
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Y_DeltaRegular "Y_DeltaRegular")));
  Topology_p top(v_cond=v, i_cond=i, control=YDcontrol) "Y-Delta switcheable"
    annotation (Placement(transformation(extent={{30,-20},{70,20}})));
  PS.Voltage[PS.n] v "voltage conductor";
  PS.Current[PS.n] i "current conductor";
  Modelica.Blocks.Interfaces.BooleanInput YDcontrol "true:Y, false:Delta"
                                            annotation (Placement(
          transformation(extent={{-110,30},{-90,50}})));

equation
  term.v = top.v_term;
  term.i = top.i_term;
/*
  connect(YDcontrol, top.control) annotation (Line(points={{-100,40},{40,40},{
            40,20}}, color={255,0,255}));
*/
  annotation (
    Documentation(info="<html>
<p>Modification of YDport_p for switcheable Y-Delta transform.<br>
Defines Y- and Delta-topology transform of voltage and current variables.<br>
The neutral point is isolated.</p>
<p>Definitions</p>
<pre>
  v:     voltage across conductor
  i:     current through conductor
  v_n:   voltage neutral point if Y-topology
  i_n=0: current neutral to ground if Y-topology
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}
  term.i = i
  i_n = sqrt(3)*term.i[3]
</pre>
<p>Relations Delta-topology, (<tt>v, i</tt>: phase terminal to phase terminal)</p>
<pre>
  v[1:2] = sqrt(3)*Rot*term.v[1:2]
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(Rot)*i[1:2]
  term.i[3] = 0
  with Rot = rotation_30deg
</pre>
</html>
"));
end Y_Dport_p;

partial model YDportTrafo_p_n
    "AC two port with Y or Delta topology for transformers"
  extends Port_p_n;

  replaceable model Topology_p = Topology.Y constrainedby Topology.TopologyBase
      "p: Y, Delta or PAR topology"
    annotation(choices(
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Y "Y"),
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Delta "Delta"),
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.PAR "PAR")));
  Topology_p top_p(v_cond=v1, i_cond=i1, v_n=v_n1, w=w1)
      "p: Y, Delta or PAR topology"
    annotation (Placement(transformation(extent={{-80,-20},{-40,20}})));

  replaceable model Topology_n = Topology.Y constrainedby Topology.TopologyBase
      "n: Y, Delta or PAR topology"
    annotation(choices(
      choice(redeclare model Topology_n =
        PowerSystems.AC3ph.Ports.Topology.Y "Y"),
      choice(redeclare model Topology_n =
        PowerSystems.AC3ph.Ports.Topology.Delta "Delta"),
      choice(redeclare model Topology_n =
        PowerSystems.AC3ph.Ports.Topology.PAR "PAR")));
  Topology_n top_n(v_cond=v2*w2_nom, i_cond=i2/w2_nom, v_n=v_n2, w=w2)
      "n: Y, Delta or PAR topology"
    annotation (Placement(transformation(extent={{80,-20},{40,20}})));

  PS.Voltage[PS.n] v1 "voltage conductor";
  PS.Current[PS.n] i1 "current conductor";
  PS.Voltage[n_n1] v_n1 "voltage neutral";
  PS.Current[n_n1] i_n1=top_p.i_n "current neutral to ground";

  PS.Voltage[PS.n] v2 "voltage conductor";
  PS.Current[PS.n] i2 "current conductor";
  PS.Voltage[n_n2] v_n2 "voltage neutral";
  PS.Current[n_n2] i_n2=top_n.i_n "current neutral to ground";

  protected
  constant Integer[2] scale={top_p.scale, top_n.scale};
  final parameter Integer n_n1=top_p.n_n
                                        annotation(Evaluate=true);
  final parameter Integer n_n2=top_n.n_n
                                        annotation(Evaluate=true);
  Real w1 "1: voltage ratio to nominal for topology";
  Real w2 "2: voltage ratio to nominal for topology";
  Real w2_nom "2: nominal turns ratio";

equation
  term_p.v = top_p.v_term;
  term_p.i = top_p.i_term;
  term_n.v = top_n.v_term;
  term_n.i = top_n.i_term;
  annotation (
Documentation(info="<html>
<p>Defines Y- and Delta-topology transform of voltage and current variables and contains additionally voltage and current scaling.</p>
<p>Below</p>
<pre>  term, v, i, w</pre>
<p>denote either the 'primary' or 'secondary' side</p>
<pre>
  term_p, v1, i1, w1
  term_n, v2, i2, w2
</pre>
<p>Definitions</p>
<pre>
  v:     scaled voltage across conductor
  i:     scaled current through conductor
  v_n:   voltage neutral point if Y-topology
  i_n:   current neutral to ground if Y-topology
  w:     voltage ratio to nominal (any value, but common for primary and secondary)
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = (term.v - {0, 0, sqrt(3)*v_n})/w
  term.i = i/w
  i_n = sqrt(3)*term.i[3]
</pre>
<p>Relations Delta-topology, (<tt>v, i</tt>: phase terminal to phase terminal)</p>
<pre>
  v[1:2] = sqrt(3)*Rot*term.v[1:2]/w
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(Rot)*i[1:2]/w
  term.i[3] = 0
  with Rot = rotation_30deg
</pre>
</html>
"));
end YDportTrafo_p_n;

partial model YDportTrafo_p_n_n
    "AC three port with Y or Delta topology for 3-winding transformers"
  extends PortBase;

  Ports.ACdq0_p term_p "positive terminal"
                                      annotation (Placement(transformation(
            extent={{-110,-10},{-90,10}})));
  Ports.ACdq0_n term_na "negative terminal a"
                                       annotation (Placement(transformation(
            extent={{90,30},{110,50}})));
  Ports.ACdq0_n term_nb "negative terminal b"
                                       annotation (Placement(transformation(
            extent={{90,-50},{110,-30}})));

  replaceable model Topology_p = Topology.Y constrainedby Topology.TopologyBase
      "p: Y or Delta topology"
    annotation(choices(
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Y "Y"),
      choice(redeclare model Topology_p =
        PowerSystems.AC3ph.Ports.Topology.Delta "Delta")));
  Topology_p top_p(v_cond=v1, i_cond=i1, v_n=v_n1, w=w1)
      "p: Y or Delta topology"
    annotation (Placement(transformation(extent={{-80,-20},{-40,20}})));

  replaceable model Topology_na = Topology.Y constrainedby
      Topology.TopologyBase "na: Y or Delta topology"
    annotation(choices(
      choice(redeclare model Topology_na =
        PowerSystems.AC3ph.Ports.Topology.Y "Y"),
      choice(redeclare model Topology_na =
        PowerSystems.AC3ph.Ports.Topology.Delta "Delta")));
  Topology_na top_na(v_cond=v2a*w2a_nom, i_cond=i2a/w2a_nom, v_n=v_n2a, w=w2a)
      "na: Y or Delta topology"
    annotation (Placement(transformation(extent={{80,20},{40,60}})));

  replaceable model Topology_nb = Topology.Y constrainedby
      Topology.TopologyBase "nb: Y or Delta topology"
    annotation(choices(
      choice(redeclare model Topology_nb =
        PowerSystems.AC3ph.Ports.Topology.Y "Y"),
      choice(redeclare model Topology_nb =
        PowerSystems.AC3ph.Ports.Topology.Delta "Delta")));
  Topology_nb top_nb(v_cond=v2b*w2b_nom, i_cond=i2b/w2b_nom, v_n=v_n2b, w=w2b)
      "nb: Y or Delta topology"
    annotation (Placement(transformation(extent={{80,-60},{40,-20}})));

  PS.Voltage[PS.n] v1 "voltage conductor";
  PS.Current[PS.n] i1 "current conductor";
  PS.Voltage[n_n1] v_n1 "voltage neutral";
  PS.Current[n_n1] i_n1=top_p.i_n "current neutral to ground";

  PS.Voltage[PS.n] v2a "voltage conductor";
  PS.Current[PS.n] i2a "current conductor";
  PS.Voltage[n_n2a] v_n2a "voltage neutral";
  PS.Current[n_n2a] i_n2a=top_na.i_n "current neutral to ground";

  PS.Voltage[PS.n] v2b "voltage conductor";
  PS.Current[PS.n] i2b "current conductor";
  PS.Voltage[n_n2b] v_n2b "voltage neutral";
  PS.Current[n_n2b] i_n2b=top_nb.i_n "current neutral to ground";

  PS.Voltage[PS.n] v0;
  protected
  constant Integer[3] scale={top_p.scale, top_na.scale, top_nb.scale};
  final parameter Integer n_n1=top_p.n_n annotation(Evaluate=true);
  final parameter Integer n_n2a=top_na.n_n
                                          annotation(Evaluate=true);
  final parameter Integer n_n2b=top_nb.n_n
                                          annotation(Evaluate=true);
  Real w1 "1: voltage ratio to nominal for topology";
  Real w2a "2a: voltage ratio to nominal for topology";
  Real w2b "2b: voltage ratio to nominal for topology";
  Real w2a_nom "2a: nominal turns ratio";
  Real w2b_nom "2b: nominal turns ratio";

equation
  Connections.branch(term_p.theta, term_na.theta);
  Connections.branch(term_p.theta, term_nb.theta);
  term_na.theta = term_p.theta;
  term_nb.theta = term_p.theta;

  term_p.v = top_p.v_term;
  term_p.i = top_p.i_term;
  term_na.v = top_na.v_term;
  term_na.i = top_na.i_term;
  term_nb.v = top_nb.v_term;
  term_nb.i = top_nb.i_term;
  annotation (
Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
          graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString="%name")}),
Documentation(info="<html>
<p>Defines Y- and Delta-topology transform of voltage and current variables and contains additionally voltage and current scaling.</p>
<p>Below</p>
<pre>  term, v, i, w</pre>
<p>denote either the 'primary' or 'secondary_a' or 'secondary_b' side</p>
<pre>
  term_p, v1, i1, w1
  term_na, v2a, i2a, w2a
  term_nb, v2b, i2b, w2b
</pre>
<p>Definitions</p>
<pre>
  v:     scaled voltage across conductor
  i:     scaled current through conductor
  v_n:   voltage neutral point if Y-topology
  i_n:   current neutral to ground if Y-topology
  w:     voltage ratio to nominal (any value, but common for primary and secondary)
</pre>
<p>Relations Y-topology, (<tt>v, i</tt>: terminal to neutral point)</p>
<pre>
  v = (term.v - {0, 0, sqrt(3)*v_n})/w
  term.i = i/w
  i_n = sqrt(3)*term.i[3]
</pre>
<p>Relations Delta-topology, (<tt>v, i</tt>: phase terminal to phase terminal)</p>
<pre>
  v[1:2] = sqrt(3)*Rot*term.v[1:2]/w
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(Rot)*i[1:2]/w
  term.i[3] = 0
  with Rot = rotation_30deg
</pre>
</html>
"));
end YDportTrafo_p_n_n;

package Topology "Topology transforms "
  extends Modelica.Icons.BasesPackage;

  partial model TopologyBase "Topology transform base"
    extends PortBase;
    constant Integer scale "for scaling of impedance values";
    parameter Integer n_n(min=0,max=1)=1 "1 for Y, 0 for Delta";
    PS.Voltage[PS.n] v_term "terminal voltage";
    PS.Current[PS.n] i_term "terminal current";
    input PS.Voltage[PS.n] v_cond "conductor voltage";
    input PS.Current[PS.n] i_cond "conductor current";
    input PS.Voltage[n_n] v_n(start=fill(0,n_n)) "voltage neutral";
    input Real w = 1 "voltage ratio to nominal";
    PS.Current[n_n] i_n(start=fill(0,n_n)) "current neutral to ground";
    protected
    constant Real s3=sqrt(3);
      annotation (
        defaultComponentName="Y",
  Documentation(
          info="<html>
</html>
  "),
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-100,100},{100,-100}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name")}));
  end TopologyBase;

  model Y "Y transform"
    extends TopologyBase(final scale=1, final n_n=1);

  equation
    w*v_cond = v_term - PS.map({0, 0, s3*v_n[1]});
    i_term = i_cond/w;
    if PS.n > 2 then
      i_n[1] = s3*i_term[3];
    else
      i_n[1] = 0;
    end if;
    annotation (defaultComponentName="Y",
  Documentation(
          info="<html>
<p><b>Structurally incomplete model</b>. Use only as component within appropriate complete model.<br>
Defines Y-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v_term, i_term:   terminal voltage and current
  v_cond, i_cond:   voltage and current across conductor, (terminal to neutral point)
</pre>
<p>Relations, zero-component and neutral point (grounding)</p>
<pre>
  w*v_cond = v_term - {0, 0, sqrt(3)*v_n}
  i_term = i_cond/w
  i_n = sqrt(3)*i_term[3]
</pre>
<p>Note: parameter sh (phase shift) not used.</p>
</html>"),
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(
              points={{-60,0},{60,0}},
              color={255,0,0},
              thickness=0.5),
            Line(points={{60,0},{100,0}}, color={0,0,255}),
            Line(
              points={{-60,80},{10,80},{60,0},{10,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(points={{-100,80},{-60,80}}, color={0,0,255}),
            Line(points={{-100,0},{-60,0}}, color={0,0,255}),
            Line(points={{-100,-80},{-60,-80}}, color={0,0,255})}),
  Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-88,80},{-60,80}}, color={0,0,255}),
            Line(points={{-90,0},{-60,0}}, color={0,0,255}),
            Line(points={{-90,-80},{-60,-80}}, color={0,0,255}),
            Line(
              points={{-60,80},{10,80},{60,0},{10,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(
              points={{-60,0},{60,0}},
              color={255,0,0},
              thickness=0.5),
            Line(points={{60,0},{90,0}}, color={0,0,255})}));
  end Y;

  model Delta "Delta transform"
    extends TopologyBase(final scale=3, final n_n=0);
    parameter Integer shift(min=0, max=11) = 1 "(0, 1 .. 11)*30deg phase shift"
      annotation(Evaluate=true);

    protected
      Real[2,2] Rot = Utilities.Transforms.rotation_dq(shift*pi/6)*s3/w;

  equation
    v_cond[1:2] = Rot*v_term[1:2];
    i_term[1:2] = i_cond[1:2]*Rot;
    if PS.n > 2 then
      v_cond[3] = 0;
      i_term[3] = 0;
    end if;
    annotation (__Dymola_structurallyIncomplete=true,defaultComponentName="Delta",
        Documentation(
        info="<html>
<p><b>Structurally incomplete model</b>. Use only as component within appropriate complete model.<br>
Defines Delta-topology transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v_term, i_term:   terminal voltage and current
  v_cond, i_cond:   voltage and current across conductor, (phase terminal to phase terminal)
</pre>
<p>Relations, zero-component<br>
<tt>v_n</tt> and <tt>i_n</tt> are not defined, as there is no neutral point.</p>
<pre>
  R = Rot_dq((1-4*sh)*30deg) * sqrt(3) / w
  v_cond[1:2] = R*v_term[1:2]
  v_cond[3] = 0
  i_term[1:2] = transpose(R)*i_cond[1:2]
  i_term[3] = 0
</pre>
<p>with <tt>Rot = rotation_30deg</tt></p>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-100,80},{80,80}}, color={0,0,255}),
            Line(points={{-100,0},{-60,0}}, color={0,0,255}),
            Line(points={{-100,-80},{80,-80}}, color={0,0,255}),
            Polygon(
              points={{-60,0},{80,80},{80,-80},{-60,0}},
              lineColor={255,0,0},
              lineThickness=0.5)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-90,80},{80,80}}, color={0,0,255}),
            Line(points={{-90,0},{-60,0}}, color={0,0,255}),
            Line(points={{-90,-80},{80,-80}}, color={0,0,255}),
            Polygon(
              points={{-60,0},{80,80},{80,-80},{-60,0}},
              lineColor={255,0,0},
              lineThickness=0.5)}));
  end Delta;

  model Y_Delta "Y Delta switcheble transform"
    extends TopologyBase(final scale=1, final n_n=1);

  /*
  Modelica.Blocks.Interfaces.BooleanInput control "true:Y, false:Delta"
annotation (Placement(transformation(
          origin={-50,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
*/
    input Boolean control "true:Y, false:Delta";

  // in switcheable topology impedance defined as WINDING-impedance (i.e. Y-topology)
    protected
    parameter SI.Conductance epsG=1e-5;
      constant Real[2,2] Rot=Utilities.Transforms.rotation_dq(pi/6);

  equation
    if control then
      w*v_cond = v_term - {0, 0, s3*v_n[1]};
      i_term = i_cond/w;
      i_n[1] = s3*i_term[3];
    else
      w*v_cond[1:2] = s3*Rot*v_term[1:2];
      v_cond[3] = 0;
      i_term[1:2] = s3*transpose(Rot)*i_cond[1:2]/w;
      i_term[3] = 0;
      i_n[1] = 0;
    end if;
  //i_n[1] = epsG*v_n[1]; // neutral point isolated
      annotation (defaultComponentName="Y_Delta",
  Documentation(
          info="<html>
<p><b>Structurally incomplete model</b>. Use only as component within appropriate complete model.<br>
Defines switcheable Y-Delta-topology transform of voltage and current variables.<br>
The neutral point is isolated.</p>
<p>More info see Topology.Y and Topology.Delta.</p>
</html>
  "),
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{60,-5},{100,-5}}, color={0,0,255}),
            Line(
              points={{-60,70},{20,70},{60,-5},{20,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(
              points={{-60,-5},{60,-5}},
              color={255,0,0},
              thickness=0.5),
            Polygon(
              points={{-60,5},{80,80},{80,-70},{-60,5}},
              lineColor={255,0,0},
              pattern=LinePattern.Dot,
              lineThickness=0.5),
            Line(
              points={{-60,80},{80,80}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-60,-70},{80,-70}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-100,75},{-70,75}}, color={0,0,255}),
            Polygon(
              points={{-70,75},{-60,80},{-60,70},{-70,75}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-70,-75},{-60,-70},{-60,-80},{-70,-75}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-70,0},{-60,5},{-60,-5},{-70,0}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-100,0},{-70,0}}, color={0,0,255}),
            Line(points={{-100,-75},{-70,-75}}, color={0,0,255})}),
  Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-70,75},{-60,70}}, color={255,0,255}),
            Line(points={{-70,-75},{-60,-80}}, color={255,0,255}),
            Line(
              points={{-70,75},{-60,80}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-70,-75},{-60,-70}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-70,0},{-60,-5}}, color={255,0,255}),
            Line(
              points={{-70,0},{-60,5}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Text(
              extent={{-80,-10},{-60,-20}},
              lineColor={255,0,255},
              pattern=LinePattern.Dot,
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid,
              textString="true"),
            Text(
              extent={{-80,20},{-60,10}},
              lineColor={255,0,255},
              pattern=LinePattern.Dot,
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid,
              textString="false"),
            Line(
              points={{-60,70},{20,70},{60,-5},{20,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(
              points={{-60,-5},{60,-5}},
              color={255,0,0},
              thickness=0.5),
            Polygon(
              points={{-60,5},{80,80},{80,-70},{-60,5}},
              lineColor={255,0,0},
              pattern=LinePattern.Dot,
              lineThickness=0.5),
            Line(points={{60,-5},{90,-5}}, color={0,0,255}),
            Line(points={{-90,0},{-70,0}}, color={0,0,255}),
            Line(points={{-88,75},{-70,75}}, color={0,0,255}),
            Line(points={{-90,-75},{-70,-75}}, color={0,0,255}),
            Line(
              points={{-60,-70},{80,-70}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-60,80},{80,80}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-80,-20},{-60,-20}}, color={255,0,255}),
            Line(
              points={{-80,10},{-60,10}},
              color={255,0,255},
              pattern=LinePattern.Dot)}));
  end Y_Delta;

  model Y_DeltaRegular "Y Delta switcheble transform, eps-regularised"
    extends TopologyBase(final scale=1, final n_n=1);

  /*
  Modelica.Blocks.Interfaces.BooleanInput control "true:Y, false:Delta"
annotation (Placement(transformation(
          origin={-50,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
*/
    input Boolean control "true:Y, false:Delta";

  // in switcheable topology impedance defined as WINDING-impedance (i.e. Y-topology)
    protected
    parameter SI.Conductance epsG=1e-5;
    parameter SI.Resistance epsR=1e-5;
      constant Real[2,2] Rot=Utilities.Transforms.rotation_dq(pi/6);
    SI.Current[3] i_neu;
    SI.Current[3] i_del;

  equation
    i_term[1:2] = i_cond[1:2]/w - Rot*{-i_del[2], i_del[1]};
    i_term[3] = i_cond[3]/w - i_del[3];
    i_cond/w = i_neu + i_del;
    i_n[1] = s3*i_neu[3];
    if control then
      w*v_cond = v_term - {0, 0, s3*v_n[1]} - epsR*i_neu;
      i_del[1:2] = epsG*(s3*Rot*v_term[1:2] - w*v_cond[1:2]);
      i_del[3] = -epsG*w*v_cond[3];
    else
      w*v_cond[1:2] = s3*Rot*v_term[1:2] - epsR*i_del[1:2];
      w*v_cond[3] = -epsR*i_del[3];
      i_neu = epsG*(v_term - w*v_cond - {0, 0, s3*v_n[1]});
    end if;
  //i_n[1] = epsG*v_n[1]; // neutral point isolated
      annotation (defaultComponentName="Y_Delta",
  Documentation(
          info="<html>
<p><b>Structurally incomplete model</b>. Use only as component within appropriate complete model.<br>
Regularised version of Y_Delta. To be used, if device is fed across an inductive component implying a differentiable current.</p>
<p>More info see Topology.Y and Topology.Delta.</p>
</html>
  "),
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{60,-5},{100,-5}}, color={0,0,255}),
            Line(
              points={{-60,70},{20,70},{60,-5},{20,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(
              points={{-60,-5},{60,-5}},
              color={255,0,0},
              thickness=0.5),
            Polygon(
              points={{-60,5},{80,80},{80,-70},{-60,5}},
              lineColor={255,0,0},
              pattern=LinePattern.Dot,
              lineThickness=0.5),
            Line(
              points={{-60,80},{80,80}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-60,-70},{80,-70}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-100,75},{-70,75}}, color={0,0,255}),
            Polygon(
              points={{-70,75},{-60,80},{-60,70},{-70,75}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-70,-75},{-60,-70},{-60,-80},{-70,-75}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-70,0},{-60,5},{-60,-5},{-70,0}},
              lineColor={255,0,255},
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-100,0},{-70,0}}, color={0,0,255}),
            Line(points={{-100,-75},{-70,-75}}, color={0,0,255})}),
  Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-70,75},{-60,70}}, color={255,0,255}),
            Line(points={{-70,-75},{-60,-80}}, color={255,0,255}),
            Line(
              points={{-70,75},{-60,80}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-70,-75},{-60,-70}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-70,0},{-60,-5}}, color={255,0,255}),
            Line(
              points={{-70,0},{-60,5}},
              color={255,0,255},
              pattern=LinePattern.Dot),
            Text(
              extent={{-80,-10},{-60,-20}},
              lineColor={255,0,255},
              pattern=LinePattern.Dot,
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid,
              textString="true"),
            Text(
              extent={{-80,20},{-60,10}},
              lineColor={255,0,255},
              pattern=LinePattern.Dot,
              fillColor={255,0,255},
              fillPattern=FillPattern.Solid,
              textString="false"),
            Line(
              points={{-60,70},{20,70},{60,-5},{20,-80},{-60,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(
              points={{-60,-5},{60,-5}},
              color={255,0,0},
              thickness=0.5),
            Polygon(
              points={{-60,5},{80,80},{80,-70},{-60,5}},
              lineColor={255,0,0},
              pattern=LinePattern.Dot,
              lineThickness=0.5),
            Line(points={{60,-5},{90,-5}}, color={0,0,255}),
            Line(points={{-90,0},{-70,0}}, color={0,0,255}),
            Line(points={{-88,75},{-70,75}}, color={0,0,255}),
            Line(points={{-90,-75},{-70,-75}}, color={0,0,255}),
            Line(
              points={{-60,-70},{80,-70}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(
              points={{-60,80},{80,80}},
              color={0,0,255},
              pattern=LinePattern.Dot),
            Line(points={{-80,-20},{-60,-20}}, color={255,0,255}),
            Line(
              points={{-80,10},{-60,10}},
              color={255,0,255},
              pattern=LinePattern.Dot)}));
  end Y_DeltaRegular;

  model PAR "Phase angle regulating (quadrature booster)"
    extends PowerSystems.AC3ph.Ports.Topology.TopologyBase(
      final scale=1, final n_n=1);

    SI.Angle alpha = atan(w - 1) "phase shift";

    protected
    Real[2, 2] Rot = [1, -w+1; w-1, 1] "Transforms.rotation_dq(alpha)/cos(alpha)";

  equation
    v_term[1:2] = Rot*v_cond[1:2];
    i_cond[1:2] = i_term[1:2]*Rot;
    if PS.n > 2 then
      v_term[3] = s3*v_n[1];
      i_cond[3] = i_term[3];
      i_n[1] = s3*i_term[3];
    else
      i_n[1] = 0;
    end if;
    annotation (defaultComponentName="Y",
  Documentation(
          info="<html>
<p><b>Structurally incomplete model</b>. Use only as component within appropriate complete model.<br>
Defines phase regulating transform of voltage and current variables.</p>
<p>Definitions</p>
<pre>
  v_term, i_term:   terminal voltage and current
  v_cond, i_cond:   voltage and current across conductor, (terminal to neutral point)
</pre>
<p>Relations, zero-component and neutral point (grounding)</p>
<pre>
  alpha = atan(w-1)
  R = Rot_dq(alpha)/cos(alpha)
  v_term = cat(1, R*v_cond[1:2], {sqrt(3)*v_n})
  i_cond = cat(1, i_term[1:2]*R, {i_term[3]})
  i_n = sqrt(3)*i_term[3]
</pre>
<p>Note: parameter sh (phase shift) not used.</p>
</html>"),
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(
              points={{-80,-80},{-20,10},{40,-80},{-80,-80},{-80,-80}},
              color={255,0,0},
              thickness=0.5),
            Line(points={{-100,80},{100,80}},color={0,0,255}),
            Line(points={{60,-80},{60,10}},color={0,0,255}),
            Line(points={{-100,50},{100,50}},color={0,0,255}),
            Line(points={{-100,20},{100,20}},color={0,0,255})}),
  Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{60,-80},{60,80}},color={0,0,255}),
            Ellipse(
              extent={{58,82},{62,78}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,62},{62,58}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,42},{62,38}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,22},{62,18}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,2},{62,-2}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,0},{60,60}},   color={0,0,255}),
            Ellipse(
              extent={{58,-18},{62,-22}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,-38},{62,-42}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,-58},{62,-62}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{58,-78},{62,-82}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-20,-2},{20,-10}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString="v_cond"),
            Text(
              extent={{-20,48},{20,40}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString="v_term"),
            Text(
              extent={{-60,10},{-40,2}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              fontName="Symbol",
              textString="a"),
            Text(
              extent={{60,34},{90,26}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString="w = dv"),
            Line(points={{-80,0},{60,0}},    color={0,0,255})}));
  end PAR;
  annotation (preferredView="info",
    Documentation(info="<HTML>
<p>
Contains transforms for Y and Delta topology dq0.
</p>
</HTML>"));
end Topology;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Electrical ports with connectors ACdq0:</p>
<p>The index notation <tt>_p_n</tt> and <tt>_pn</tt> is used for</p>
<pre>
  _p_n:     no conservation of current
  _pn:      with conservation of current
</pre>
</html>
"));
end Ports;
