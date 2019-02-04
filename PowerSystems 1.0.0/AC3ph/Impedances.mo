within PowerSystems.AC3ph;
package Impedances "Impedance and admittance two terminal"
  extends Modelica.Icons.VariantsPackage;

  model Resistor "Resistor, 3-phase dq0"
    extends Partials.ImpedBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Resistance r=1 "resistance";
  protected
    final parameter SI.Resistance R=r*Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);

  equation
    R*i = v;
    annotation (
      defaultComponentName="res1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Resistor;

  model Conductor "Conductor, 3-phase dq0"
    extends Partials.ImpedBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Conductance g=1 "conductance";
  protected
    final parameter SI.Conductance G=g/Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);

  equation
    G*v = i;
    annotation (
      defaultComponentName="cond1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Conductor;

  model Inductor "Inductor with series resistor, 3-phase dq0"
    extends Partials.ImpedBase;

    parameter SIpu.Resistance r=0 "resistance";
    parameter SIpu.Reactance x_s=1 "self reactance";
    parameter SIpu.Reactance x_m=0 "mutual reactance, -x_s/2 < x_m < x_s";
  protected
    final parameter SI.Resistance[2] RL_base=Utilities.Precalculation.baseRL(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    final parameter SI.Resistance R=r*RL_base[1];
    final parameter SI.Inductance L=(x_s-x_m)*RL_base[2];
    final parameter SI.Inductance L0=(x_s+2*x_m)*RL_base[2];

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(i) = omega[1]*j(i);
    elseif dynType == Types.Dynamics.FixedInitial then
      i = i_start;
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      PS.map({L,L,L0}).*der(i) + omega[2]*L*j(i) + R*i = v;
    else
      omega[2]*L*j(i) + R*i = v;
    end if;
    annotation (
      defaultComponentName="ind1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{-40,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-40,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,60},{-40,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,10},{-40,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{-40,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{60,20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-20},{60,-30}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-70},{60,-80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end Inductor;

  model Capacitor "Capacitor with parallel conductor, 3-phase dq0"
    extends Partials.ImpedBase;

    parameter SIpu.Conductance g=0 "conductance";
    parameter SIpu.Susceptance b=1 "susceptance";
  protected
    final parameter SI.Resistance[2] GC_base=Utilities.Precalculation.baseGC(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    final parameter SI.Conductance G=g*GC_base[1];
    final parameter SI.Capacitance C=b*GC_base[2];

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(v) = omega[1]*j(v);
    elseif dynType == Types.Dynamics.FixedInitial then
      v = v_start;
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      C*der(v) + omega[2]*C*j(v) + G*v = i;
    else
      omega[2]*C*j(v) + G*v = i;
    end if;
    annotation (
      defaultComponentName="cap1",
  Documentation(
          info="<html>
<p>No phase to phase capacitance.</p>
<p>Info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-90,0},{-20,0}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{90,0},{20,0}},
            color={0,120,120},
            thickness=0.5),
          Rectangle(
            extent={{-12,60},{12,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,60},{-12,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,70},{-2,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,70},{4,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
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
            extent={{-4,-30},{-2,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-30},{4,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,44},{10,36}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-6},{10,-14}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-56},{10,-64}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,60},{-60,60},{-60,40},{-10,40}}, color={0,0,255}),
          Line(points={{-4,10},{-60,10},{-60,-10},{-10,-10}}, color={0,0,255}),
          Line(points={{-4,-40},{-60,-40},{-60,-60},{-10,-60}}, color={0,0,255}),
          Line(points={{4,60},{60,60},{60,40},{10,40}}, color={0,0,255}),
          Line(points={{4,10},{60,10},{60,-10},{10,-10}}, color={0,0,255}),
          Line(points={{4,-40},{60,-40},{60,-60},{10,-60}}, color={0,0,255})}));
  end Capacitor;

  model Impedance "Impedance (inductive) with series resistor, 3-phase dq0"
    extends Partials.ImpedBase;

    parameter SIpu.Impedance z_abs=1 "abs value of impedance";
    parameter Real cos_phi(min=0,max=1)=0.1 "cos-phi of impedance";
    parameter Real cpl(min=-0.5,max=1)=0
      "phase coupling x_m/x_s, -1/2 < cpl < 1";
  protected
    final parameter SI.Resistance[2] RL_base=Utilities.Precalculation.baseRL(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    function acos = Modelica.Math.acos;
    final parameter SI.Resistance R=z_abs*cos_phi*RL_base[1];
    final parameter SI.Inductance L=z_abs*sin(acos(cos_phi))*RL_base[2];
    final parameter SI.Inductance L0=L*(1 + 2*cpl)/(1-cpl);

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(i) = omega[1]*j(i);
    elseif dynType == Types.Dynamics.FixedInitial then
      i = i_start;
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      PS.map({L,L,L0}).*der(i) + omega[2]*L*j(i) + R*i = v;
    else
      omega[2]*L*j(i) + R*i = v;
    end if;
    annotation (
      defaultComponentName="impedance1",
  Documentation(
          info="<html>
<p>This model corresponds to ACdq0.Inductor, but uses a different determination of the coefficients.<br>
Instead of x_s, x_m, and r the parameters z_abs, cos(phi), and x_o are used.</p>
</p>Relations:</p>
<pre>
  z = Z / R_base
  z_abs = |z|
  r = real(z) = |z|*cos(phi)           resistance
  x = imag(z) = |z|*sin(phi)           inductance dq-components
</pre>
<p>With</p>
<pre>  cpl = x_m/x_s, -1/2 &lt;  cpl &lt;  1        coupling coefficient</pre>
<p>we have</p>
<pre>  x0 = x*(1 + 2*cpl)/(1 - cpl)         inductance o-component</pre>
<p>and</p>
<pre>
  x_s = (2*x + x0)/3 = x/(1 - cpl)     self inductance
  x_m = -(x - x0)/3 = x*cpl/(1 - cpl)  mutual inductance
</pre>
<p>Coupling:</p>
<pre>
  cpl = x_m/x_s  coupling coefficient, -1/2 &lt;  cpl &lt;  1>
  cpl &gt;  0        positive coupling (example lines)
  cpl &lt;  0        negative coupling (example machine windings)
  cpl = (x0/x - 1)/(x0/x + 2) </li>
</pre>
<p> More info see package ACdq0.Impedances.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{-20,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Polygon(
            points={{-80,-30},{80,-30},{80,30},{-20,30},{-80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,60},{-40,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,10},{-40,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{-40,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{60,20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-20},{60,-30}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-70},{60,-80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end Impedance;

  model Admittance
    "Admittance (capacitive) with parallel conductor, 3-phase dq0"
    extends Partials.ImpedBase;

    parameter SIpu.Admittance y_abs=1 "abs value of admittance";
    parameter Real cos_phi(min=0,max=1)=0.1 "cos-phi of admittance";
  protected
    final parameter SI.Resistance[2] GC_base=Utilities.Precalculation.baseGC(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    function acos = Modelica.Math.acos;
    final parameter SI.Conductance G=y_abs*cos_phi*GC_base[1];
    final parameter SI.Capacitance C=y_abs*sin(acos(cos_phi))*GC_base[2];

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(v) = omega[1]*j(v);
    elseif dynType == Types.Dynamics.FixedInitial then
      v = v_start;
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      C*der(v) + omega[2]*C*j(v) + G*v = i;
    else
      omega[2]*C*j(v) + G*v = i;
    end if;
    annotation (
      defaultComponentName="admittance1",
  Documentation(
          info="<html>
<p>This model corresponds to ACdq0.Capacitor, but uses a different determination of the coefficients.<br>
Instead of b and g the parameters y_abs and cos(phi) are used.</p>
</p>Relations:</p>
<pre>
  y = Y / G_base
  y_abs = |y|
  g = real(y) = |y|*cos(phi)     conductance
  b = imag(y) = |y|*sin(phi)     admittance dq0-components
</pre>
<p>No phase to phase capacitance.</p>
<p> More info see package ACdq0.Impedances.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-90,0},{-20,0}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{90,0},{20,0}},
            color={0,120,120},
            thickness=0.5),
          Rectangle(
            extent={{-12,60},{12,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-12,60},{12,60},{-12,-60},{-12,60}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,60},{-12,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,70},{-2,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,70},{4,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
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
            extent={{-4,-30},{-2,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-30},{4,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,44},{10,36}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-6},{10,-14}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-56},{10,-64}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,60},{-60,60},{-60,40},{-10,40}}, color={0,0,255}),
          Line(points={{-4,10},{-60,10},{-60,-10},{-10,-10}}, color={0,0,255}),
          Line(points={{-4,-40},{-60,-40},{-60,-60},{-10,-60}}, color={0,0,255}),
          Line(points={{4,60},{60,60},{60,40},{10,40}}, color={0,0,255}),
          Line(points={{4,10},{60,10},{60,-10},{10,-10}}, color={0,0,255}),
          Line(points={{4,-40},{60,-40},{60,-60},{10,-60}}, color={0,0,255})}));
  end Admittance;

  model ResistorNonSym "Resistor non symmetric, 3-phase dq0."
    extends Partials.ImpedNonSymBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Resistance[3] r={1,1,1} "resistance[3] abc";
  protected
    final parameter SI.Resistance[3] R_abc=r*Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom);
    SI.Resistance[3, 3] R;

  equation
    R = Park*diagonal(R_abc)*transpose(Park);
    R*i = v;
    annotation (
      defaultComponentName="resNonSym",
  Documentation(
          info="<html>
<p>Resistor with general resistance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs a time dependent transform of the coefficient matrix.</p>
<p>More info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Polygon(
            points={{-80,30},{-80,0},{-50,30},{-80,30}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end ResistorNonSym;

  model InductorNonSym
    "Inductor with series resistor non symmetric, 3-phase dq0."
    extends Partials.ImpedNonSymBase;

    parameter SIpu.Resistance[3] r={0,0,0} "resistance[3] abc";
    parameter SIpu.Reactance[3, 3] x=[1, 0, 0; 0, 1, 0; 0, 0, 1]
      "reactance[3,3] abc";
    SI.MagneticFlux[3] psi(each stateSelect=StateSelect.prefer) "magnetic flux";
  protected
    final parameter SI.Resistance[2] RL_base=Utilities.Precalculation.baseRL(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    final parameter SI.Resistance[3] R_abc=r*RL_base[1];
    final parameter SI.Inductance[3, 3] L_abc=x*RL_base[2];
    SI.Resistance[3, 3] R;
    SI.Inductance[3, 3] L;

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(psi[1:2]) = omega[1]*j_dq0(psi[1:2]);
      psi[3] = 0;
    end if;

  equation
    L = Park*L_abc*transpose(Park);
    R = Park*diagonal(R_abc)*transpose(Park);

    psi = L*i;
    der(psi) + omega[2]*j_dq0(psi) + R*i = v;
    annotation (
      defaultComponentName="indNonSym",
  Documentation(
          info="<html>
<p>Inductor with general reactance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs a time dependent transform of the coefficient matrix.</p>
<p>More info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,30},{-40,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-80,30},{-80,0},{-50,30},{-80,30}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,60},{-40,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,10},{-40,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{-40,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{60,20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-20},{60,-30}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-70},{60,-80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end InductorNonSym;

  model CapacitorNonSym
    "Capacitor with parallel conductor non symmetric, 3-phase dq0."
    extends Partials.ImpedNonSymBase;

    parameter SIpu.Conductance[3] g={0,0,0} "conductance[3] abc";
    parameter SIpu.Susceptance[3] b={1,1,1} "susceptance[3] abc";
    SI.ElectricCharge[3] q(each stateSelect=StateSelect.prefer)
      "electric charge";
  protected
    final parameter SI.Resistance[2] GC_base=Utilities.Precalculation.baseGC(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom);
    final parameter SI.Conductance[3] G_abc=g*GC_base[1];
    final parameter SI.Capacitance[3] C_abc=b*GC_base[2];
    SI.Conductance[3, 3] G;
    SI.Capacitance[3, 3] C;

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(q[1:2]) = omega[1]*j_dq0(q[1:2]);
      q[3] = 0;
    end if;

  equation
    C = Park*diagonal(C_abc)*transpose(Park);
    G = Park*diagonal(G_abc)*transpose(Park);

    q = C*v;
    der(q) + omega[2]*j_dq0(q) + G*v = i;
    annotation (
      defaultComponentName="capNonSym",
  Documentation(
          info="<html>
<p>Capacitor with general susceptance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs
a time dependent transform of the coefficient matrix.</p>
<p> No phase to phase capacitance.</p>
<p> More info see package ACdq0.Impedances.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-90,0},{-20,0}},
            color={0,120,120},
            thickness=0.5),
          Line(
            points={{90,0},{20,0}},
            color={0,120,120},
            thickness=0.5),
          Rectangle(
            extent={{-12,60},{12,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-12,60},{-12,30},{12,60},{-12,60}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,60},{-12,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,70},{-2,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,70},{4,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
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
            extent={{-4,-30},{-2,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-30},{4,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,44},{10,36}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-6},{10,-14}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-56},{10,-64}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,60},{-60,60},{-60,40},{-10,40}}, color={0,0,255}),
          Line(points={{-4,10},{-60,10},{-60,-10},{-10,-10}}, color={0,0,255}),
          Line(points={{-4,-40},{-60,-40},{-60,-60},{-10,-60}}, color={0,0,255}),
          Line(points={{4,60},{60,60},{60,40},{10,40}}, color={0,0,255}),
          Line(points={{4,10},{60,10},{60,-10},{10,-10}}, color={0,0,255}),
          Line(points={{4,-40},{60,-40},{60,-60},{10,-60}}, color={0,0,255})}));
  end CapacitorNonSym;

  model Varistor "Varistor, 3-phase dq0"
    extends Partials.ImpedNonSymBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Resistance r0=100 "small voltage resistance";
    parameter SIpu.Voltage v0=1 "saturation voltage";
    PS.Voltage[3] v_abc;
    PS.Current[3] i_abc(start=zeros(3));
  protected
    final parameter Real V0=(v0*Utilities.Precalculation.baseV(puUnits, V_nom));
    final parameter Real H0=(r0*Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom)/V0);

  equation
    i_abc = transpose(Park)*i;
    v_abc = V0*tanh(H0*i_abc);
    v = Park*v_abc;
    annotation (
      defaultComponentName="varistor",
  Documentation(
          info="<html>
<p>Voltage limiter with hyperbolic tangent characteristic.</p>
<p>More info see package ACdq0.Impedances.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Line(points={{30,25},{26,2},{-26,-2},
                {-30,-26}}, color={0,0,0})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,10},{60,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,60},{60,40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{60,-60}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{28,10},{26,2},{-26,-2},{-28,-10}}, color={0,0,0}),
          Line(points={{28,60},{26,52},{-26,48},{-28,40}}, color={0,0,0}),
          Line(points={{28,-40},{26,-48},{-26,-52},{-28,-60}}, color={0,0,0})}));
  end Varistor;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model ImpedBase "Impedance base, 3-phase dq0"
      extends Ports.Port_pn;
      extends Common.Nominal.NominalAC;

      parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter PS.Voltage[PS.n] v_start = zeros(PS.n)
        "start value of voltage drop" annotation(Dialog(tab="Initialization"));
      parameter PS.Current[PS.n] i_start = zeros(PS.n)
        "start value of current" annotation(Dialog(tab="Initialization"));

      PS.Voltage[PS.n] v(start = v_start);
      PS.Current[PS.n] i(start = i_start);

    protected
      SI.AngularFrequency[2] omega;

    equation
      omega = der(term_p.theta);
      v = term_p.v - term_n.v;
      i = term_p.i;
      annotation (
        Documentation(
      info="<html>
</html>
"),        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-80,50},{-60,50}}, color={0,0,255}),
            Line(points={{-80,0},{-60,0}}, color={0,0,255}),
            Line(points={{-80,-50},{-60,-50}}, color={0,0,255}),
            Line(points={{60,50},{80,50}}, color={0,0,255}),
            Line(points={{60,0},{80,0}}, color={0,0,255}),
            Line(points={{60,-50},{80,-50}}, color={0,0,255})}));
    end ImpedBase;

    partial model ImpedNonSymBase "Impedance base non symmetric, 3-phase dq0."
      extends ImpedBase;

    protected
      Real[3,3] Park=Utilities.Transforms.park(term_p.theta[2]);

      annotation (
        Documentation(
    info="<html>
<p>Same as ImpedBase, but contains additionally a Park-transform which is needed for
transformation of general impedance matrices from abc rest to general dq0-system.
(for example when coefficients of non symmetric systems are defined in abc representation.)</p>
</html>"));
    end ImpedNonSymBase;

    partial model ImpedHeat "Impedance base with heat port, 3-phase dq0"
      extends ImpedBase;
      extends PowerSystems.Interfaces.AddHeat;

    equation
      Q_flow = v*i;
      annotation (
        Documentation(
      info="<html>
<p>Same as ImpedBase, but contains an additional heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connector is given by the total dissipated electric energy of all conductors.</p>
</html>
"));
    end ImpedHeat;

    partial model ImpedNonSymHeat
      "Impedance base non symmetric with heat port, 3-phase dq0"
      extends ImpedNonSymBase;
      extends PowerSystems.Interfaces.AddHeatV(final m_heat=3);

      PS.Voltage[3] v_abc=Park*v;
      PS.Current[3] i_abc=Park*i;

    equation
      Q_flow = v_abc.*i_abc;
      annotation (
        Documentation(
      info="<html>
<p>Same as ImpedNonSymBase, but contains an additional vector heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connectors is given by the dissipated electric power per conductor.</p>
</html>
"));
    end ImpedNonSymHeat;

  end Partials;

annotation (preferredView="info",
    Documentation(info="<html>
<p>Contains lumped impedance models and can also be regarded as a collection of basic formulas. Shunts are part of a separate package.</p>
<p>General relations.</p>
<pre>
  r = R / R_base                  resistance
  x = 2*pi*f_nom*L/R_base         reactance
  g = G / G_base                  conductance
  b = (2*pi*f_nom*C) / G_base     susceptance
  G_base = 1/R_base
</pre>
<p>A) <b>Symmetric systems</b>.</p>
<p>The reactance-matrix in abc-representation is</p>
<pre>
          [x_s, x_m, x_m
  x_abc =  x_m, x_s, x_m
           x_m, x_m, x_s]
</pre>
<p>and corresponds to the following diagonal matrix in dq0-representation</p>
<pre>
          [x, 0, 0
  x_dq0 =  0, x, 0
           0, 0, x0]
</pre>
<p>with the relations</p>
<pre>
  x   = x_s - x_m           stray reactance (dq-components)
  x0  = x_s + 2*x_m         zero-reactance (o-component)
  x_s =  (2*x + x0)/3       self reactance single conductor
  x_m = -(x - x0)/3         mutual reactance
</pre>
<p>Coupling.</p>
<pre>
  -x_s/2 &lt;  x_m &lt;  x_s
  uncoupled limit:          x_m = 0,               x0 = x
  fully positive coupled:   x_m = x_s,             x0 = 3*x_s
  fully negative coupled:   x_m = -x_s/2,          x0 = 0
  'practical' value:        x_m = -x_s*(2/13),     x0 = (3/5)*x
</pre>
<p>The corresponding resistance matrix is</p>
<pre>
                  [r, 0, 0
  r_abc = r_dq0 =  0, r, 0
                   0, 0, r]
</pre>
<p>The susceptance matrices in abc- and in dq0-representation are</p>
<pre>
          [ b_pg + 2b_pp, -b_pp,         -b_pp
  b_abc =  -b_pp,          b_pg + 2b_pp, -b_pp
           -b_pp,         -b_pp,          b_pg + 2b_pp]
          [ b_pg + 3*b_pp, 0,             0
  b_dq0 =   0,             b_pg + 3*b_pp, 0
            0,             0,             b_pg]
</pre>
<p>where <tt>_pg</tt> denotes 'phase-to-ground' and <tt>_pp</tt> 'phase-to-phase'.</p>
<p>The corresponding conduction matrices are (in analogy to susceptance)</p>
<pre>
          [ g_pg + 2g_pp, -g_pp,         -g_pp
  g_abc =  -g_pp,          g_pg + 2g_pp, -g_pp
           -g_pp,         -g_pp,          g_pg + 2g_pp]
          [ g_pg + 3*g_pp, 0,             0
  g_dq0 =   0,             g_pg + 3*g_pp, 0
            0,             0,             g_pg]
</pre>
<p>B) <b>Non symmetric systems</b>.</p>
<p><tt>&nbsp; x_abc</tt> is an arbitrary symmetric matrix with positive diagonal elements</p>
<p><tt>&nbsp; r_abc</tt> is an arbitrary diagonal matrix with positive elements</p>
<p><tt>&nbsp; b_abc</tt> (phase-to-ground) is an arbitrary diagonal matrix with positive elements</p>
<p><tt>&nbsp; b_abc</tt> (phase-to-phase) is of the form</p>
<pre>
          [b_pp[2] + b_pp[3], -b_pp[3],           -b_pp[2]
  b_abc = -b_pp[3],            b_pp[3] + b_pp[1], -b_pp[1]
          -b_pp[2],           -b_pp[1],            b_pp[1] + b_pp[2]]
</pre>
<p><tt>&nbsp; g_abc(phase-to-ground)</tt> is an arbitrary diagonal matrix with positive elements</p>
<p><tt>&nbsp; g_abc(phase-to-phase)</tt> is of the form</p>
<pre>
          [g_pp[2] + g_pp[3], -g_pp[3],           -g_pp[2]
  g_abc = -g_pp[3],            g_pp[3] + g_pp[1], -g_pp[1]
          -g_pp[2],           -g_pp[1],            g_pp[1] + g_pp[2]]
</pre>
<p>where</p>
<pre>
  index 1 denotes pair 2-3
  index 2 denotes pair 3-1
  index 3 denotes pair 1-2
</pre>
<p>The corresponding dq0-matrices are all obtained by Park-transformation P</p>
<pre>
  x_dq0 = P*x_abc*transpose(P)
  r_dq0 = P*r_abc*transpose(P)
  b_dq0 = P*b_abc*transpose(P)
  g_dq0 = P*g_abc*transpose(P)
</pre>
</html>"));
end Impedances;
