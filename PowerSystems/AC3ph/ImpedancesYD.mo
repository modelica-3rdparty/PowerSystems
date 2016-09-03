within PowerSystems.AC3ph;
package ImpedancesYD
  "Impedance and admittance one terminal, Y and Delta topology"
  extends Modelica.Icons.VariantsPackage;

  model Resistor "Resistor, 3-phase dq0"
    extends Partials.ImpedYDBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Resistance r=1 "resistance";
  protected
    final parameter SI.Resistance R=r*Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom,
          top.scale);

  equation
    R*i = v;
    annotation (
      defaultComponentName="resYD1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.ImpedancesYD.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{70,-30}},
            lineColor={0,0,0},
            lineThickness=0.5,
            fillColor={255,255,255})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-70,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Resistor;

  model Conductor "Conductor, 3-phase dq0"
    extends Partials.ImpedYDBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Conductance g=1 "conductance";
  protected
    final parameter SI.Conductance G=g/Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom,
          top.scale);

  equation
    G*v = i;
    annotation (
      defaultComponentName="resYD1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.ImpedancesYD.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{70,-30}},
            lineColor={0,0,0},
            lineThickness=0.5,
            fillColor={255,255,255})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-70,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Conductor;

  model Inductor "Inductor with series resistor, 3-phase dq0"
    extends Partials.ImpedYDBase;

    parameter SIpu.Resistance r=0 "resistance";
    parameter SIpu.Reactance x_s=1 "self reactance";
    parameter SIpu.Reactance x_m=0 "mutual reactance, -x_s/2 < x_m < x_s";
  protected
    final parameter SI.Resistance[2] RL_base=Utilities.Precalculation.baseRL(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom,
          top.scale);
    final parameter SI.Resistance R=r*RL_base[1];
    final parameter SI.Inductance L=(x_s-x_m)*RL_base[2];
    final parameter SI.Inductance L0=(x_s+2*x_m)*RL_base[2];

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(i) = omega[1]*j_dq0(i);
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      diagonal({L,L,L0})*der(i) + omega[2]*L*j_dq0(i) + R*i = v;
    else
      omega[2]*L*j_dq0(i) + R*i = v;
    end if;
    annotation (
      defaultComponentName="indYD1",
  Documentation(
          info="<html>
<p>Info see package ACdq0.ImpedancesYD.</p>
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
            extent={{-40,30},{70,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,9},{30,7}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-8},{30,-10}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-23},{30,-25}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end Inductor;

  model Capacitor "Capacitor with parallel conductor, 3-phase dq0"
    extends Partials.ImpedYDBase;

    parameter SIpu.Conductance g=0 "conductance";
    parameter SIpu.Susceptance b=1 "susceptance";
  protected
    final parameter SI.Resistance[2] GC_base=Utilities.Precalculation.baseGC(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom,
          top.scale);
    final parameter SI.Conductance G=g*GC_base[1];
    final parameter SI.Capacitance C=b*GC_base[2];

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(v) = omega[1]*j_dq0(v);
    end if;

  equation
    if dynType <> Types.Dynamics.SteadyState then
      C*der(v) + omega[2]*C*j_dq0(v) + G*v = i;
    else
      omega[2]*C*j_dq0(v) + G*v = i;
    end if;
    annotation (
      defaultComponentName="capYD1",
  Documentation(
          info="<html>
<p>No phase to phase capacitance.</p>
<p>Info see package ACdq0.ImpedancesYD.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-90,0},{-20,0}},
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
          Line(points={{-70,0},{-4,0}}),
          Rectangle(
            extent={{-4,21},{-2,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,21},{4,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,5},{-2,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,5},{4,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,-11},{-2,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-11},{4,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-70,16},{-4,16}}),
          Line(points={{-70,-16},{-4,-16}}),
          Line(points={{4,16},{30,16}}),
          Line(points={{4,0},{30,0}}),
          Line(points={{4,-16},{30,-16}}),
          Rectangle(
            extent={{-20,10},{-10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-6},{-10,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-22},{-10,-26}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-30,16},{-30,8},{-20,8}}, color={0,0,255}),
          Line(points={{-30,0},{-30,-8},{-20,-8}}, color={0,0,255}),
          Line(points={{-30,-16},{-30,-24},{-20,-24}}, color={0,0,255}),
          Line(points={{-10,8},{20,8},{20,16}}, color={0,0,255}),
          Line(points={{-10,-8},{20,-8},{20,0}}, color={0,0,255}),
          Line(points={{-10,-24},{20,-24},{20,-16}}, color={0,0,255})}));
  end Capacitor;

  model ResistorNonSym "Resistor non symmetric, 3-phase dq0"
    extends Partials.ImpedYDNonSymBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

    parameter SIpu.Resistance[3] r={1,1,1} "resistance[3] abc";
  protected
    final parameter SI.Resistance[3] R_abc=r*Utilities.Precalculation.baseR(
          puUnits,
          V_nom,
          S_nom,
          top.scale);
    SI.Resistance[3, 3] R;

  equation
    R = Park*diagonal(R_abc)*transpose(Park);
    R*i = v;
    annotation (
      defaultComponentName="resYDnonSym",
  Documentation(
          info="<html>
<p>Resistor with general resistance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs a time dependent transform of the coefficient matrix.</p>
<p>More info see package ACdq0.ImpedancesYD.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{70,-30}},
            lineColor={0,0,0},
            lineThickness=0.5,
            fillColor={255,255,255}), Polygon(
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
            extent={{-70,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end ResistorNonSym;

  model InductorNonSym
    "Inductor with series resistor non symmetric, 3-phase dq0"
    extends Partials.ImpedYDNonSymBase;

    parameter SIpu.Resistance[3] r={0,0,0} "resistance[3] abc";
    parameter SIpu.Reactance[3, 3] x=[1, 0, 0; 0, 1, 0; 0, 0, 1]
      "reactance[3,3] abc";
    SI.MagneticFlux[3] psi(each stateSelect=StateSelect.prefer) "magnetic flux";
  protected
    final parameter SI.Resistance[2] RL_base=Utilities.Precalculation.baseRL(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom,
          top.scale);
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
      defaultComponentName="indYDnonSym",
  Documentation(
          info="<html>
<p>Inductor with general reactance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs a time dependent transform of the coefficient matrix.</p>
<p>More info see package ACdq0.ImpedancesYD.</p>
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
            extent={{-40,30},{70,-30}},
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
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,9},{30,7}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-8},{30,-10}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-23},{30,-25}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end InductorNonSym;

  model CapacitorNonSym
    "Capacitor with parallel conductor non symmetric, 3-phase dq0"
    extends Partials.ImpedYDNonSymBase;

    parameter SIpu.Conductance[3] g={0,0,0} "conductance[3] abc";
    parameter SIpu.Susceptance[3] b={1,1,1} "susceptance[3] abc";
    SI.ElectricCharge[3] q(each stateSelect=StateSelect.prefer)
      "electric charge";
  protected
    final parameter SI.Resistance[2] GC_base=Utilities.Precalculation.baseGC(
          puUnits,
          V_nom,
          S_nom,
          2*pi*f_nom,
          top.scale);
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
      defaultComponentName="capYDnonSym",
  Documentation(
          info="<html>
<p>Capacitor with general susceptance matrix, defined in abc inertial system.<br>
Use only if 'non symmetric' is really desired because this component needs a time dependent transform of the coefficient matrix.</p>
<p>No phase to phase capacitance.</p>
<p> More info see package ACdq0.ImpedancesYD.</p>
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
          Line(points={{-70,0},{-4,0}}),
          Rectangle(
            extent={{-4,21},{-2,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,21},{4,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,5},{-2,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,5},{4,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,-11},{-2,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-11},{4,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-70,16},{-4,16}}),
          Line(points={{-70,-16},{-4,-16}}),
          Line(points={{4,16},{30,16}}),
          Line(points={{4,0},{30,0}}),
          Line(points={{4,-16},{30,-16}}),
          Rectangle(
            extent={{-20,10},{-10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-6},{-10,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-22},{-10,-26}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-30,16},{-30,8},{-20,8}}, color={0,0,255}),
          Line(points={{-30,0},{-30,-8},{-20,-8}}, color={0,0,255}),
          Line(points={{-30,-16},{-30,-24},{-20,-24}}, color={0,0,255}),
          Line(points={{-10,8},{20,8},{20,16}}, color={0,0,255}),
          Line(points={{-10,-8},{20,-8},{20,0}}, color={0,0,255}),
          Line(points={{-10,-24},{20,-24},{20,-16}}, color={0,0,255})}));
  end CapacitorNonSym;

  model Varistor "Varistor, 3-phase dq0"
    extends Partials.ImpedYDNonSymBase(final f_nom=0, final dynType=Types.Dynamics.SteadyState);

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
      defaultComponentName="varistorYD",
  Documentation(
          info="<html>
<p>Voltage limiter with hyperbolic tangent characteristic.</p>
<p>More info see package ACdq0.ImpedancesYD.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{70,-30}},
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
            extent={{-70,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{8,22},{6,18},{-46,16},{-48,12}}, color={0,0,0}),
          Line(points={{8,5},{6,1},{-46,-1},{-48,-5}}, color={0,0,0}),
          Line(points={{8,-11},{6,-15},{-46,-17},{-48,-21}}, color={0,0,0})}));
  end Varistor;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model ImpedYDBase "One terminal impedance base, 3-phase dq0"
      extends Ports.YDport_p;
      extends Common.Nominal.NominalAC;

      parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SIpu.Resistance r_n=1 "resistance neutral to grd"
        annotation(Dialog(enable));
    protected
      final parameter SI.Resistance R_n=r_n*Utilities.Precalculation.baseR(
              puUnits,
              V_nom,
              S_nom);
      SI.AngularFrequency[2] omega;

    equation
      omega = der(term.theta);
      v_n = R_n*i_n "equation neutral to ground (if Y-topology)";
      annotation (
        Documentation(
      info="<html>
<p>Y-topology: contains an equation for neutral to ground</p>
<p>Delta-topology: <tt>i[3] = 0</tt></p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{70,20},{76,-20}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid)}),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{70,30},{80,-30}},
              lineColor={135,135,135},
              lineThickness=0.5,
              fillColor={135,135,135},
              fillPattern=FillPattern.Solid)}));
    end ImpedYDBase;

    partial model ImpedYDNonSymBase
      "One terminal impedance base non symmetric, 3-phase dq0"
      extends ImpedYDBase;

    protected
      Real[3,3] Park=Utilities.Transforms.park(term.theta[2]);
      annotation (
        Documentation(
      info="<html>
<p>Same as ImpedYDBase, but contains additionally a Park-transform which is needed for
transformation of general impedance matrices from abc rest to general dq0-system.
(for example when coefficients of non symmetric systems are defined in abc representation.)</p>
</html>"));

    end ImpedYDNonSymBase;

    partial model ImpedYDHeat
      "One terminal impedance base with heat port, 3-phase dq0"
      extends ImpedYDBase;
      extends PowerSystems.Interfaces.AddHeat;

    equation
      Q_flow = v*i;
      annotation (
        Documentation(
      info="<html>
<p>Same as ImpedYDBase, but contains an additional heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connector is given by the total dissipated electric energy of all conductors (not included neutral-to-ground!).</p>
</html>
"));
    end ImpedYDHeat;

    partial model ImpedYDNonSymHeat
      "One terminal impedance base non symmetric with heat port, 3-phase dq0"
      extends ImpedYDNonSymBase;
      extends PowerSystems.Interfaces.AddHeatV(
                                       final m_heat=3);

      PS.Voltage[3] v_abc=Park*top.v_cond;
      PS.Current[3] i_abc=Park*top.i_cond;

    equation
      Q_flow = v_abc.*i_abc;
      annotation (
        Documentation(
      info="<html>
<p>Same as ImpedYDNonSymBase, but contains an additional vector heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connectors is given by the dissipated electric power per conductor (not included neutral-to-ground!).</p>
</html>"));
    end ImpedYDNonSymHeat;

  end Partials;

annotation (preferredView="info",
    Documentation(info="<html>
<p>Contains lumped impedance models for Y and Delta topology.</p>
<p>General relations see 'Impedances'.</p>
<p>All elements allow the choice between Y- and Delta-topology.<br>
The impedance parameters are defined 'as seen from the terminals', directly relating terminal voltage and terminal current. With this definition same parameters lead to same network properties, independent of topology. The necessary scaling is performed automatically.</p>
<p>In Delta-topology the conductor voltage is sqrt(3) higher, the current sqrt(3) lower,
compared to the terminal voltage and current. Therefore the impedance relating conductor current and voltage is a factor 3 larger, the admittance a factor 1/3 smaller than the impedance and admittance as seen from the terminal.</p>
<p>If impedance parameters are known for the WINDINGS, choose:</p>
<pre>  input values impedance parameters = (winding values of impedance parameters)/3</pre>
<p>In dq0-representation the following relations hold between<br>
terminal-voltage term.v and -current term.i on the one hand<br>
and conductor-voltage v and -current i on the other:</p>
<p><b>Y-topology</b>:</p>
<pre>
  v = term.v - {0, 0, sqrt(3)*v_n}: voltage between terminal and neutral point
  term.i = i
  i_n = sqrt(3)*term.i[3]
</pre>
<p><b>Delta-topology</b>:</p>
<pre>
  v[1:2] = sqrt(3)*R30*term.v[1:2]: voltage between phase-terminals
  v[3] = 0
  term.i[1:2] = sqrt(3)*transpose(R30)*i[1:2]
  term.i[3] = 0
</pre>
<p>with <tt>R30 = rotation_30deg</tt><br>
(Alternative solutions corresponding to permuted phases are <tt>R-90</tt> and <tt>R150</tt> instead of <tt>R30</tt>).</p>
</html>"));
end ImpedancesYD;
