within PowerSystems.AC3ph;
package Breakers "Switches and Breakers 3-phase"
  extends Modelica.Icons.VariantsPackage;

  model ForcedSwitch "Forced switch, 3-phase dq0"
    extends Partials.SwitchBase(final n=1);

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));

    parameter SI.Time t_relax=10e-3 "switch relaxation time";
    parameter Integer p_relax(min=2)=4 "power of relaxation exponent";
  protected
    outer System system;
    SI.Time t0(start=-Modelica.Constants.inf);
    Real[2] r(start={0,1});
    Real[3] s;
  /*
  Boolean open(start=not control[1])=not control[1];
  Boolean closed(start=control[1])=control[1];

  Start values not correct, since no parameter expressions.
  Removed start values and initalized the pre(..) values in the
  initial equation section.
*/
    Boolean open=not control[1];
    Boolean closed=control[1];

    function relaxation=PowerSystems.Utilities.Math.relaxation;

  initial equation
    pre(open) = not control[1];
    pre(closed) = control[1];
    if dynType <> Types.Dynamics.SteadyState then
      t0 = -Modelica.Constants.inf;
    end if;
  equation
    if dynType <> Types.Dynamics.SteadyState then
      when edge(open) or edge(closed) then
        t0 = time;
      end when;
      r = relaxation(time - t0, t_relax, p_relax);
      {v,i} = if closed then {(r[1]+r[2]*epsR)*s,(r[1]*epsG+r[2])*s} else {(r[1]*epsR+r[2])*s,(r[1]+r[2]*epsG)*s};
    else
      t0 = 0;
      r = {1,0};
      {v,i} = if closed then {epsR*s,s} else {s,epsG*s};
    end if;
  annotation (defaultComponentName = "switch1",
    Documentation(
            info="<html>
<p>Switching by forced change of current-voltage ratio.</p>
<p>Does not allow single-phase switching. This model acts directly on the current vector in the chosen reference frame and avoids any transformation of variables. To be used, if details of the switching process are not of interest.</p>
<p>'closed' and 'open' determine the mechanical switch-position.<br>
Electrically the switch is on, if it is 'closed', whereas the currents start decreasing exponentially, when it is opened.</p>
<p>A single scalar control-input is sufficient.</p>
<p>The transition between the 'off' conductivity (epsG) and the 'on' resistivity (epsR) is continuous with an exponential relaxation function
<pre>  f = (exp(-dt^p/2) - exp(-1/2))/(1 - exp(-1/2))</pre>
with
<pre>
  dt          relative time measured from switching instant
  t_relax     relaxation time
  p           power of exponent
</pre></p></html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-50,0},{50,0}},
            color={0,120,120},
            pattern=LinePattern.Dot), Text(
            extent={{-80,-20},{80,-60}},
            lineColor={0,120,120},
            pattern=LinePattern.Dot,
            textString="dq0 exp")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-80,0},{-50,0},{30,60}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{-50,0},{50,0}},
            color={0,120,120},
            pattern=LinePattern.Dot),
          Line(
            points={{50,0},{80,0}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{0,90},{0,40}},
            color={255,0,255},
            pattern=LinePattern.Dot),
          Text(
            extent={{-100,-40},{100,-60}},
            lineColor={0,120,120},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
                 " forced switch with exponential relaxation")}));
  end ForcedSwitch;

  model ForcedCommSwitch "Forced commuting switch, 3-phase dq0"
    extends Ports.PortBase;
    extends PowerSystems.Common.Nominal.NominalVI;

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));

    parameter Real[2] eps(final min={0,0}, each unit="1")={1e-4,1e-4}
      "{resistance 'closed', conductance 'open'}";
    parameter SI.Time t_relax=10e-3 "switch relaxation time";
    parameter Integer p_relax(min=2)=4 "power of relaxation exponent";
    PS.Voltage[3] v_t;
    PS.Voltage[3] v_f;
    PS.Current[3] i_t;
    PS.Current[3] i_f;

    Ports.ACdq0_p term_p "positive terminal"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Ports.ACdq0_n term_nt "negative terminal 'true'"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
    Ports.ACdq0_n term_nf "negative terminal 'false'"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    Modelica.Blocks.Interfaces.BooleanInput control
      "true: p - nt closed, false: p - nf closed"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
    outer System system;
    final parameter SI.Resistance epsR=eps[1]*V_nom/I_nom;
    final parameter SI.Conductance epsG=eps[2]*I_nom/V_nom;
    SI.Time t0(start=-Modelica.Constants.inf);
    Real[2] r(start={0,1});
    Real[3] s_t;
    Real[3] s_f;
  /*
  Boolean open_t(start=not control)=not control;
  Boolean closed_t(start=control)=control;

  Start values not correct, since no parameter expressions.
  Removed start values and initalized the pre(..) values in the
  initial equation section.
*/
    Boolean open_t = not control;
    Boolean closed_t = control;

    function relaxation=PowerSystems.Utilities.Math.relaxation;
  initial equation
    pre(open_t) = not control;
    pre(closed_t) = control;
    if dynType <> Types.Dynamics.SteadyState then
      t0 = -Modelica.Constants.inf;
    end if;
  equation
    Connections.branch(term_p.theta, term_nt.theta);
    Connections.branch(term_p.theta, term_nf.theta);
    term_nt.theta = term_p.theta;
    term_nf.theta = term_p.theta;

    v_t = term_p.v - term_nt.v;
    v_f = term_p.v - term_nf.v;
    term_nt.i = -i_t;
    term_nf.i = -i_f;
    term_p.i + term_nt.i + term_nf.i = zeros(3);

    if dynType <> Types.Dynamics.SteadyState then
      when edge(open_t) or edge(closed_t) then
        t0 = time;
      end when;
      r = relaxation(time - t0, t_relax, p_relax);
      {v_t,i_t} = if closed_t then {(r[1]+r[2]*epsR)*s_t,(r[1]*epsG+r[2])*s_t} else {(r[1]*epsR+r[2])*s_t,(r[1]+r[2]*epsG)*s_t};
      {v_f,i_f} = if open_t then {(r[1]+r[2]*epsR)*s_f,(r[1]*epsG+r[2])*s_f} else {(r[1]*epsR+r[2])*s_f,(r[1]+r[2]*epsG)*s_f};
    else
      t0 = 0;
      r = {1,0};
      {v_t,i_t} = if control then {epsR*s_t,s_t} else {s_t,epsG*s_t};
      {v_f,i_f} = if not control then {epsR*s_f,s_f} else {s_f,epsG*s_f};
    end if;
  annotation (defaultComponentName = "switch1",
    Documentation(
            info="<html>
<p>Switching by forced change of current-voltage ratio.</p>
<p>Does not allow single-phase switching. This model acts directly on the current vector in the chosen reference frame and avoids any transformation of variables. To be used, if details of the switching process are not of interest.</p>
<p>'closed' and 'open' determine the mechanical switch-position.<br>
Electrically the switch is on, if it is 'closed', whereas the currents start decreasing exponentially, when it is opened.</p>
<p>A single scalar control-input is sufficient.</p>
<p>The transition between the 'off' conductivity (epsG) and the 'on' resistivity (epsR) is continuous with an exponential relaxation function
<pre>  f = (exp(-dt^p/2) - exp(-1/2))/(1 - exp(-1/2))</pre>
with
<pre>
  dt          relative time measured from switching instant
  t_relax     relaxation time
  p           power of exponent
</pre></p></html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-80},{100,-120}},
            lineColor={0,0,0},
            textString="%name"),
          Rectangle(
            extent={{-80,60},{80,-80}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-80,0},{-50,0},{50,40}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{40,40},{80,40}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{40,-40},{80,-40}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{-48,0},{50,-40}},
            color={0,120,120},
            pattern=LinePattern.Dot),
          Text(
            extent={{-80,-50},{80,-80}},
            lineColor={0,120,120},
            pattern=LinePattern.Dot,
            textString="dq0 exp"),
          Line(
            points={{0,90},{0,22}},
            color={255,0,255},
            pattern=LinePattern.Dot)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-80,0},{-50,0},{50,40}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{-50,0},{50,-40}},
            color={0,120,120},
            pattern=LinePattern.Dot),
          Line(
            points={{40,-40},{80,-40}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{0,90},{0,20}},
            color={255,0,255},
            pattern=LinePattern.Dot),
          Text(
            extent={{-100,-60},{100,-80}},
            lineColor={0,120,120},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid,
            textString=
                 " forced commuting switch with exponential relaxation"),
          Line(
            points={{40,40},{80,40}},
            color={0,120,120},
            thickness=0.5),
          Text(
            extent={{10,50},{50,40}},
            lineColor={255,0,255},
            textString=
                 "true"),
          Text(
            extent={{10,-40},{50,-50}},
            lineColor={255,0,255},
            textString=
                 "false")}));
  end ForcedCommSwitch;

  model Switch "Ideal switch, 3-phase dq0"
    extends Partials.SwitchTrsfBase;

  protected
    Common.Switching.Switch switch_a(
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-70,30},{10,90}})));
    Common.Switching.Switch switch_b(
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-40,-30},{40,30}})));
    Common.Switching.Switch switch_c(
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-10,-90},{70,-30}})));

  equation
    switch_a.v=v_abc[1];
    switch_a.i=i_abc[1];
    switch_b.v=v_abc[2];
    switch_b.i=i_abc[2];
    switch_c.v=v_abc[3];
    switch_c.i=i_abc[3];

    connect(control[1], switch_a.closed)  annotation (Line(points={{0,106.667},
            {0,90},{-30,90}}, color={255,0,255}));
    connect(control[2], switch_b.closed)  annotation (Line(points={{0,100},{0,
            30}}, color={255,0,255}));
    connect(control[3], switch_c.closed)  annotation (Line(points={{0,93.3333},
            {0,90},{30,90},{30,-30}}, color={255,0,255}));
    annotation (defaultComponentName = "switch1",
      Documentation(
              info="<html>
<p>Allows single-phase switching.</p>
<p>'closed' and 'open' determine the mechanical switch-position.<br>
Electrically the switch is on if it is 'closed', whereas it is switched off, if it is mechanically 'open' and the corresponding phase-current crosses zero.</p>
<p>Contains no plasma-arc, in contrast to Breaker.</p>
<p>Note: currently not suitable for steady-state simulation. In this case use ForcedSwitch.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-50,0},{50,0}},
            color={0,100,100},
            pattern=LinePattern.Dot)}));
  end Switch;

  model Breaker "Breaker, 3-phase dq0"
    extends Partials.SwitchTrsfBase;

    replaceable parameter Parameters.BreakerArc par "breaker parameter"
                                             annotation (Placement(
          transformation(extent={{60,70},{80,90}})));
  protected
    replaceable Common.Switching.Breaker breaker_a(
      D=par.D,
      t_opening=par.t_opening,
      Earc=par.Earc,
      R0=par.R0,
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-70,30},{10,90}})));
    replaceable Common.Switching.Breaker breaker_b(
      D=par.D,
      t_opening=par.t_opening,
      Earc=par.Earc,
      R0=par.R0,
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-40,-30},{40,30}})));
    replaceable Common.Switching.Breaker breaker_c(
      D=par.D,
      t_opening=par.t_opening,
      Earc=par.Earc,
      R0=par.R0,
      epsR=epsR,
      epsG=epsG)                  annotation (Placement(transformation(extent={
              {-10,-90},{70,-30}})));

  equation
    breaker_a.v=v_abc[1];
    breaker_a.i=i_abc[1];
    breaker_b.v=v_abc[2];
    breaker_b.i=i_abc[2];
    breaker_c.v=v_abc[3];
    breaker_c.i=i_abc[3];

    connect(control[1], breaker_a.closed)  annotation (Line(points={{0,106.667},
            {0,90},{-30,90}}, color={255,0,255}));
    connect(control[2], breaker_b.closed)
      annotation (Line(points={{0,100},{0,30}}, color={255,0,255}));
    connect(control[3], breaker_c.closed)  annotation (Line(points={{0,93.3333},
            {0,90},{30,90},{30,-30}}, color={255,0,255}));
    annotation (defaultComponentName = "breaker1",
      Documentation(
              info="<html>
<p>Allows single-phase switching.</p>
<p>'closed' and 'open' determine the mechanical switch-position.<br>
Electrically the switch is on if it is 'closed', whereas it is switched off, if it is mechanically fully 'open' (after a given opening duration) and the corresponding phase-current crosses zero.</p>
<p>Contains replaceable single-line breakers with replaceable tanh arc-voltage, i.e. a constant electric field strength E for large currents and a small-signal Ohmic resistance R.</p>
<p>Note: currently not suitable for steady-state simulation. In this case use ForcedSwitch.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-50,0},{-34,-4},{-24,0},{-14,-2},{-4,4},{2,0},{12,-4},{22,
                2},{30,-2},{38,-4},{42,2},{50,0}},
            color={255,255,0},
            thickness=0.5)}));
  end Breaker;

  package Partials "Partial models"
    extends Modelica.Icons.MaterialPropertiesPackage;

    partial model SwitchBase "Switch base, 3-phase dq0"
      extends Ports.Port_pn;
      extends PowerSystems.Common.Nominal.NominalVI;

      parameter Integer n=3 "number of independent switches";
      parameter Real[2] eps(final min={0,0}, each unit="1")={1e-4,1e-4}
        "{resistance 'closed', conductance 'open'}";
      PS.Voltage[3] v;
      PS.Current[3] i;
      Modelica.Blocks.Interfaces.BooleanInput[n] control
        "true:closed, false:open"
      annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
      final parameter SI.Resistance epsR=eps[1]*V_nom/I_nom;
      final parameter SI.Conductance epsG=eps[2]*I_nom/V_nom;

    equation
      v = term_p.v - term_n.v;
      term_p.i = i;
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(
              points={{-80,0},{-50,0},{30,60}},
              color={0,120,120},
              thickness=0.5),
            Line(
              points={{50,0},{80,0}},
              color={0,120,120},
              thickness=0.5),
            Line(
              points={{0,90},{0,40}},
              color={255,0,255},
              pattern=LinePattern.Dot)}));
    end SwitchBase;

    partial model SwitchTrsfBase
      "Switch base, additional abc-variables, 3-phase dq0"
      extends SwitchBase(final n=3);

      PS.Voltage[3] v_abc(each stateSelect=StateSelect.never)
        "voltage switch a, b, c";
      PS.Current[3] i_abc(each stateSelect=StateSelect.never)
        "current switch a, b, c";
    protected
      Real[3,3] Park=PowerSystems.Utilities.Transforms.park(term_p.theta[2]);

    equation
      v = Park*v_abc;
      i_abc = transpose(Park)*i;
    /*
  Loop not solvable for this version:
  static = not (switch_a.arc or switch_b.arc or switch_c.arc); //Switch
  static = not (breaker_a.arc or breaker_b.arc or breaker_c.arc); //Breaker
  if static then
    v_abc = {0,0,0} "v_abc not needed";
    i_abc = {0,0,0} "i_abc not needed)";
  else
    v = Park*v_abc;
    i_abc = transpose(Park)*i;
  end if;
*/
    /*
  Initial equation needed, if the equation
  der(i_abc) = transpose(Park)*(der(i) + omega[2]*j_dq0(i));
  is used:
  initial equation
  i_abc = transpose(Park)*i;
*/
      annotation (
        Documentation(
              info="<html>
</html>
"),        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-80,-60},{0,-60}}, color={0,0,255}),
            Line(points={{60,-60},{80,-60}}, color={0,0,255}),
            Line(points={{-80,0},{-30,0}}, color={0,0,255}),
            Line(points={{30,0},{80,0}}, color={0,0,255}),
            Line(points={{-80,60},{-60,60}}, color={0,0,255}),
            Line(points={{0,60},{80,60}}, color={0,0,255})}));
    end SwitchTrsfBase;

  end Partials;

package Parameters "Parameter data for interactive use"
  extends Modelica.Icons.BasesPackage;

record BreakerArc "Breaker parameters, 3-phase"
  extends Modelica.Icons.Record;

  SI.Distance D=50e-3 "contact distance open" annotation(Dialog);
  SI.Time t_opening=30e-3 "opening duration" annotation(Dialog);
  SI.ElectricFieldStrength Earc=50e3 "electric field arc" annotation(Dialog);
  Real R0=1 "small signal resistance arc" annotation(Dialog);

  annotation (defaultComponentName = "data",
    defaultComponentPrefixes="parameter",
    Documentation(
            info="<html>
</html>"));
end BreakerArc;

  annotation (preferredView="info",
    Documentation(info="<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"));
end Parameters;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Terminology:</p>
<p><tt><b>Forced switch</b></tt> is used for a component that breaks the current independent of a possible zero crossing.<br>
<tt><b>Switch</b></tt> is used for a component, that breaks the current during zero-crossing but does not contain any additional physical properties like arc-voltage etc.<br>
<tt><b>Breaker</b></tt> is used for a component that acts basically like a 'Switch' but contains additionally physical properties of plasma-arcs, opening duration etc.</p>
</html>
"));
end Breakers;
