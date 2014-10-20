within PowerSystems.AC1ph_DC;
package Impedances "Impedance and admittance two terminal"
  extends Modelica.Icons.VariantsPackage;

  model Resistor "Resistor, 1-phase"
    extends Partials.ImpedBase(final f_nom=0);

    parameter SIpu.Resistance[2] r={1,1} "resistance";
  protected
    final parameter SI.Resistance[2] R=r*Basic.Precalculation.baseR(puUnits, V_nom, S_nom);

  equation
    diagonal(R)*i = v;
    annotation (defaultComponentName="res1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-60,30},{60,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-60,-10},{60,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Resistor;

  model Conductor "Conductor, 1-phase"
    extends Partials.ImpedBase(final f_nom=0);

    parameter SIpu.Conductance[2] g={1,1} "conductance";
  protected
    final parameter SI.Conductance[2] G=g/Basic.Precalculation.baseR(puUnits, V_nom, S_nom);

  equation
    diagonal(G)*v = i;
    annotation (defaultComponentName="cond1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-60,30},{60,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-60,-10},{60,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Conductor;

  model Inductor "Inductor with series resistor, 1-phase"
    extends Partials.ImpedBase;

    parameter SIpu.Resistance[2] r={0,0} "resistance";
    parameter SIpu.Reactance[2,2] x=[1,0;0,1] "reactance matrix";
  protected
    final parameter Real[2] RL_base=Basic.Precalculation.baseRL(puUnits, V_nom, S_nom, 2*pi*f_nom);
    final parameter SI.Resistance[2] R=r*RL_base[1];
    final parameter SI.Inductance[2,2] L=x*RL_base[2];

  initial equation
    if steadyIni_t then
      der(i) = zeros(2);
    elseif not system.steadyIni then
      i = i_start;
    end if;

  equation
    L*der(i) + diagonal(R)*i = v;
    annotation (defaultComponentName="ind1",
      Documentation(
              info="<html>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{-40,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{-40,30},{80,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,30},{-40,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{60,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-10},{-40,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-10},{60,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,5},{60,-5}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end Inductor;

  model Capacitor "Capacitor with parallel conductor, 1-phase"
    extends Partials.ImpedBase;

    parameter SIpu.Conductance[2] g={0,0} "conductance";
    parameter SIpu.Susceptance[2] b={1,1} "susceptance";
  protected
    final parameter Real[2] GC_base=Basic.Precalculation.baseGC(puUnits, V_nom, S_nom, 2*pi*f_nom);
    final parameter SI.Conductance[2] G=g*GC_base[1];
    final parameter SI.Capacitance[2] C=b*GC_base[2];

  initial equation
    if steadyIni_t then
      der(v) = zeros(2);
    elseif not system.steadyIni then
      v = v_start;
    end if;

  equation
    diagonal(C)*der(v) + diagonal(G)*v = i;
    annotation (defaultComponentName="cap1",
      Documentation(
              info="<html>
<p>No pair capacitance.</p>
<p>Info see package AC1ph_DC.Impedances.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,0},{-20,0}}, color={0,0,255}),
          Line(points={{90,0},{20,0}}, color={0,0,255}),
          Rectangle(
            extent={{-12,60},{12,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,60},{-12,-60}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,40},{-2,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,40},{4,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,14},{10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,30},{-60,30},{-60,10},{-10,10}}, color={0,0,255}),
          Line(points={{4,30},{60,30},{60,10},{10,10}}, color={0,0,255}),
          Rectangle(
            extent={{-4,0},{-2,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,0},{4,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-26},{10,-34}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,-10},{-60,-10},{-60,-30},{-10,-30}}, color={0,0,255}),
          Line(points={{4,-10},{60,-10},{60,-30},{10,-30}}, color={0,0,255})}));
  end Capacitor;

  model Impedance "Impedance (inductive) with series resistor, 1-phase"
    extends Partials.ImpedBase;

    parameter SIpu.Impedance z_abs=1 "impedance-value";
    parameter Real cos_phi(min=0,max=1)=0.1 "cos-phi of impedance";
    parameter Real cpl(min=-1,max=1)=0 "phase coupling, -1 < cpl < 1";
  protected
    final parameter Real[2] RL_base=Basic.Precalculation.baseRL(puUnits, V_nom, S_nom, 2*pi*f_nom);
    function acos = Modelica.Math.acos;
    final parameter SI.Resistance R=z_abs*cos_phi*RL_base[1];
    final parameter SI.Inductance[2,2] L=([1,cpl;cpl,1]/(1 - cpl))*z_abs*sin(acos(cos_phi))*RL_base[2];

  initial equation
    if steadyIni_t then
      der(i) = zeros(2);
    elseif not system.steadyIni then
      i = i_start;
    end if;

  equation
    L*der(i) + R*i = v;
    annotation (defaultComponentName="impedance1",
      Documentation(
              info="<html>
<p>This model corresponds to AC1ph_DC.Inductor, but uses a different determination of the coefficients.<br>
Instead of x and r the parameters z_abs and cos(phi) are used.</p>
<p>Relations:</p>
<pre>
  z = Z / R_base
  z_abs = |z|
  r = real(z) = |z|*cos(phi)          resistance
  x = imag(z) = |z|*sin(phi)          inductance pair
</pre>
<p>With</p>
<pre>  cpl = x_m/x_s, -1 &lt  cpl &lt  1         coupling coefficient</pre>
<p>we have</p>
<pre>  x0 = x*(1 + cpl)/(1 - cpl)          inductance 0-component</pre>
<p>and</p>
<pre>
  x_s = (x + x0)/2 = x/(1 - cpl)      self inductance single conductor
  x_m = (x0 - x)/2 = x*cpl/(1 - cpl)  mutual inductance
</pre>
<p>Coupling:</p>
<pre>
  cpl &gt  0:        positive coupling (example lines)
  cpl &lt  0:        negative coupling
</pre>
<p>More info see package AC1_DC.Impedances.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{-20,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Polygon(
            points={{-80,-30},{80,-30},{80,30},{-20,30},{-80,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,30},{-40,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,30},{60,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-10},{-40,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-10},{60,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,5},{60,-5}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end Impedance;

  model Admittance "Admittance (capacitive) with parallel conductor, 1-phase"
    extends Partials.ImpedBase;

    parameter SIpu.Admittance y_abs=1 "abs value of admittance";
    parameter Real cos_phi(min=0,max=1)=0.1 "cos-phi of admittance";
  protected
    final parameter Real[2] GC_base=Basic.Precalculation.baseGC(puUnits, V_nom, S_nom, 2*pi*f_nom);
    function acos = Modelica.Math.acos;
    final parameter SI.Conductance G=y_abs*cos_phi*GC_base[1];
    final parameter SI.Capacitance C=y_abs*sin(acos(cos_phi))*GC_base[2];

  initial equation
    if steadyIni_t then
      der(v) = zeros(2);
    elseif not system.steadyIni then
      v = v_start;
    end if;

  equation
    C*der(v) + G*v = i;
    annotation (defaultComponentName="admittance1",
      Documentation(
              info="<html>
<p>This model corresponds to AC1_DC.Capacitor, but uses a different determination of the coefficients.<br>
Instead of b and g the parameters y_abs and cos(phi) are used.</p>
<p>Relations:</p>
<pre>
  y = Y / G_base
  y_abs = |y|
  g = real(y) = |y|*cos(phi):     conductance
  b = imag(y) = |y|*sin(phi):     admittance
</pre>
<p>No pair capacitance.</p>
<p>More info see package AC1_DC.Impedances.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,0},{-20,0}}, color={0,0,255}),
          Line(points={{90,0},{20,0}}, color={0,0,255}),
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
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{12,60},{20,-60}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-4,40},{-2,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,40},{4,20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,14},{10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,30},{-60,30},{-60,10},{-10,10}}, color={0,0,255}),
          Line(points={{4,30},{60,30},{60,10},{10,10}}, color={0,0,255}),
          Rectangle(
            extent={{-4,0},{-2,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,0},{4,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-26},{10,-34}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-4,-10},{-60,-10},{-60,-30},{-10,-30}}, color={0,0,255}),
          Line(points={{4,-10},{60,-10},{60,-30},{10,-30}}, color={0,0,255})}));
  end Admittance;

  model Varistor "Varistor, 1-phase"
    extends Partials.ImpedBase(final f_nom=0);

    parameter SIpu.Resistance r0=100 "small voltage resistance";
    parameter SIpu.Voltage v0=1 "saturation voltage";
  protected
    final parameter Real V0=(v0*Basic.Precalculation.baseV(puUnits, V_nom));
    final parameter Real H0=(r0*Basic.Precalculation.baseR(puUnits, V_nom, S_nom)/V0);

  equation
    v = V0*tanh(H0*i);
    annotation (defaultComponentName="varistor",
      Documentation(
              info="<html>
<p>Voltage limiter with hyperbolic tangent characteristic.</p>
<p>More info see package AC1_DC.Impedances.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Line(points={{30,25},{26,2},{-26,-2},
                {-30,-26}}, color={0,0,0})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,30},{60,10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{28,30},{26,22},{-26,18},{-28,10}}, color={0,0,0}),
          Rectangle(
            extent={{-60,-10},{60,-30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{28,-10},{26,-18},{-26,-22},{-28,-30}}, color={0,0,0})}));
  end Varistor;

  model ResistorSym "Symmetrical capacitor with neutral access"
    extends Partials.ImpedBase0;

    parameter SI.Resistance R=1;
    Interfaces.Electric_n neutral "symmetrical point"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Nodes.Electric_pn_p_n pn_p_n annotation (Placement(transformation(extent={{
              -80,-10},{-60,10}}, rotation=0)));
    Nodes.Electric_pn_p_n p_n_pn annotation (Placement(transformation(extent={{
              80,-10},{60,10}}, rotation=0)));
    ImpedancesSingle.ResistorSym resSym(final R=R)
      annotation (Placement(transformation(
          origin={0,0},
          extent={{-20,-20},{20,20}},
          rotation=270)));

  equation
    connect(pn_p_n.term_p, resSym.term_p) annotation (Line(points={{-64,4},{-40,
            4},{-40,20},{-1.22465e-015,20}}, color={0,0,255}));
    connect(resSym.term_p, p_n_pn.term_p) annotation (Line(points={{
            -1.22465e-015,20},{40,20},{40,4},{64,4}}, color={0,0,255}));
    connect(pn_p_n.term_n, resSym.term_n) annotation (Line(points={{-64,-4},{
            -40,-4},{-40,-20},{1.22465e-015,-20}}, color={0,0,255}));
    connect(resSym.term_n, p_n_pn.term_n) annotation (Line(points={{
            1.22465e-015,-20},{40,-20},{40,-4},{64,-4}}, color={0,0,255}));
    connect(term_p, pn_p_n.term_pn)
      annotation (Line(points={{-100,0},{-76,0}}, color={0,0,255}));
    connect(p_n_pn.term_pn, term_n)
      annotation (Line(points={{76,0},{100,0}}, color={0,0,255}));
    connect(resSym.neutral, neutral) annotation (Line(points={{-20,
            -1.22465e-015},{-20,-40},{0,-40},{0,-100}}, color={0,0,255}));
    annotation (defaultComponentName="resSym",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,10},{0,-10}}, color={0,0,255}),
          Rectangle(extent={{-10,60},{10,10}}, lineColor={0,0,255}),
          Rectangle(extent={{-10,-10},{10,-60}}, lineColor={0,0,255}),
          Line(points={{-80,10},{-40,10},{-40,70},{40,70},{40,10},{80,10}},
              color={0,0,255}),
          Line(points={{-80,-10},{-40,-10},{-40,-70},{40,-70},{40,-10},{80,-10}},
              color={0,0,255}),
          Line(points={{0,70},{0,60}}, color={0,0,255}),
          Line(points={{0,-60},{0,-70}}, color={0,0,255}),
          Line(points={{0,0},{20,0},{20,-100},{10,-100}}, color={0,0,255})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ResistorSym;

  model CapacitorSym "Symmetrical capacitor with neutral access"
    extends Partials.ImpedBase0;

    parameter SI.Voltage Vstart=0 "start voltage";
    parameter SI.Conductance G=1e-6;
    parameter SI.Capacitance C=1e-6;
    Interfaces.Electric_n neutral "symmetrical point"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Nodes.Electric_pn_p_n pn_p_n annotation (Placement(transformation(extent={{
              -80,-10},{-60,10}}, rotation=0)));
    Nodes.Electric_pn_p_n p_n_pn annotation (Placement(transformation(extent={{
              80,-10},{60,10}}, rotation=0)));
    ImpedancesSingle.CapacitorSym capSym(final G=G, final C=C, final Vstart=Vstart)
      annotation (Placement(transformation(
          origin={0,0},
          extent={{-20,-20},{20,20}},
          rotation=270)));
  protected
    outer System system;

  initial equation
    capSym.cap1.v = capSym.cap2.v;
    if system.steadyIni then
      der(capSym.cap1.v) + der(capSym.cap2.v) = 0;
    end if;

  equation
    connect(pn_p_n.term_p, capSym.term_p) annotation (Line(points={{-64,4},{-40,
            4},{-40,20},{-1.22461e-015,20}}, color={0,0,255}));
    connect(capSym.term_p, p_n_pn.term_p) annotation (Line(points={{
            -1.22461e-015,20},{40,20},{40,4},{64,4}}, color={0,0,255}));
    connect(pn_p_n.term_n, capSym.term_n) annotation (Line(points={{-64,-4},{
            -40,-4},{-40,-20},{1.22461e-015,-20}}, color={0,0,255}));
    connect(capSym.term_n, p_n_pn.term_n) annotation (Line(points={{
            1.22461e-015,-20},{40,-20},{40,-4},{64,-4}}, color={0,0,255}));
    connect(term_p, pn_p_n.term_pn)
      annotation (Line(points={{-100,0},{-76,0}}, color={0,0,255}));
    connect(p_n_pn.term_pn, term_n)
      annotation (Line(points={{76,0},{100,0}}, color={0,0,255}));
    connect(capSym.neutral, neutral) annotation (Line(points={{-20,
            -1.22461e-015},{-20,-40},{0,-40},{0,-100}}, color={0,0,255}));
    annotation (defaultComponentName="capSym",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,40},{50,20}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,48},{50,40}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{50,12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-20},{50,-40}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-12},{50,-20}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-40},{50,-48}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,12},{0,-12}}, color={0,0,255}),
          Line(points={{0,0},{20,0},{20,-6}}, color={0,0,255}),
          Line(points={{-80,10},{-70,10},{-70,70},{70,70},{70,10},{80,10}},
              color={0,0,255}),
          Line(points={{-80,-10},{-70,-10},{-70,-70},{70,-70},{70,-10},{80,-10}},
              color={0,0,255}),
          Line(points={{20,-54},{20,-100},{10,-100}}, color={0,0,255}),
          Line(points={{0,70},{0,48}}, color={0,0,255}),
          Line(points={{0,-48},{0,-70}}, color={0,0,255})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end CapacitorSym;

  model DClink "DC-link with filter circuit"
    extends Partials.ImpedBase0;

    parameter SI.Voltage Vstart=0 "start voltage";
    parameter SI.Resistance Rfilter=1e-3;
    parameter SI.Inductance Lfilter=1e-3;
    parameter SI.Capacitance Cfilter=1e-6;
    parameter SI.Capacitance Cdc=1e-6;
    ImpedancesSingle.Resistor res1(final R=Rfilter)
      annotation (Placement(transformation(
          origin={-30,50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.Inductor ind1(final L=Lfilter)
      annotation (Placement(transformation(
          origin={-30,0},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.Capacitor cap1(final C=Cfilter)
      annotation (Placement(transformation(
          origin={-30,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.Capacitor cap(final C=Cdc, final v(start=Vstart))
      annotation (Placement(transformation(
          origin={30,0},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Interfaces.Electric_n grd "ground"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Nodes.Electric_pn_p_n pn_p_n annotation (Placement(transformation(extent={{
              -80,-10},{-60,10}}, rotation=0)));
    Nodes.Electric_pn_p_n p_n_pn annotation (Placement(transformation(extent={{
              80,-10},{60,10}}, rotation=0)));
  protected
    outer System system;

  initial equation
    if system.steadyIni then
      der(cap.v) = 0;
      der(cap1.v) = 0;
    end if;

  equation
    connect(pn_p_n.term_p, res1.term_p) annotation (Line(points={{-64,4},{-60,4},
            {-60,60},{-30,60}}, color={0,0,255}));
    connect(res1.term_n, ind1.term_p)
      annotation (Line(points={{-30,40},{-30,10}}, color={0,0,255}));
    connect(ind1.term_n, cap1.term_p) annotation (Line(points={{-30,-10},{-30,
            -40}}, color={0,0,255}));
    connect(cap1.term_n, pn_p_n.term_n) annotation (Line(points={{-30,-60},{-60,
            -60},{-60,-4},{-64,-4}}, color={0,0,255}));
    connect(p_n_pn.term_p, cap.term_p)  annotation (Line(points={{64,4},{60,4},
            {60,10},{30,10}}, color={0,0,255}));
    connect(p_n_pn.term_n, cap.term_n)  annotation (Line(points={{64,-4},{60,-4},
            {60,-10},{30,-10}}, color={0,0,255}));
    connect(res1.term_p, cap.term_p)  annotation (Line(points={{-30,60},{30,60},
            {30,10}}, color={0,0,255}));
    connect(cap.term_n, cap1.term_n)  annotation (Line(points={{30,-10},{30,-60},
            {-30,-60}}, color={0,0,255}));
    connect(cap.term_n, grd)  annotation (Line(points={{30,-10},{30,-60},{0,-60},
            {0,-100}}, color={0,0,255}));
    connect(term_p, pn_p_n.term_pn)
      annotation (Line(points={{-100,0},{-76,0}}, color={0,0,255}));
    connect(p_n_pn.term_pn, term_n)
      annotation (Line(points={{76,0},{100,0}}, color={0,0,255}));
    annotation (defaultComponentName="dcLink",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-80,20},{80,-20}},
            lineColor={0,0,255},
            textString=
                 "DClink")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end DClink;

  model DClinkSym "Symmetrical DC-link with filter circuit and neutral access"
    extends Partials.ImpedBase0;

    parameter SI.Voltage Vstart=0 "start voltage";
    parameter SI.Resistance Rfilter=1e-3;
    parameter SI.Inductance Lfilter=1e-3;
    parameter SI.Capacitance Cfilter=1e-6;
    parameter SI.Capacitance Cdc=1e-6;
    ImpedancesSingle.Resistor res1(final R=Rfilter)
      annotation (Placement(transformation(
          origin={-30,50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.Inductor ind1(final L=Lfilter)
      annotation (Placement(transformation(
          origin={-30,0},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.Capacitor cap1(final C=Cfilter)
      annotation (Placement(transformation(
          origin={-30,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    ImpedancesSingle.CapacitorSym capSym(final C=Cdc, final Vstart=Vstart)
      annotation (Placement(transformation(
          origin={30,0},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Interfaces.Electric_n neutral "symmetrical point"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Nodes.Electric_pn_p_n pn_p_n annotation (Placement(transformation(extent={{
              -80,-10},{-60,10}}, rotation=0)));
    Nodes.Electric_pn_p_n p_n_pn annotation (Placement(transformation(extent={{
              80,-10},{60,10}}, rotation=0)));
  protected
    outer System system;

  initial equation
    capSym.cap1.v = capSym.cap2.v;
    if system.steadyIni then
      der(cap1.v) = 0;
    end if;

  equation
    connect(pn_p_n.term_p, res1.term_p) annotation (Line(points={{-64,4},{-60,4},
            {-60,60},{-30,60}}, color={0,0,255}));
    connect(res1.term_n, ind1.term_p)
      annotation (Line(points={{-30,40},{-30,10}}, color={0,0,255}));
    connect(ind1.term_n, cap1.term_p) annotation (Line(points={{-30,-10},{-30,
            -40}}, color={0,0,255}));
    connect(cap1.term_n, pn_p_n.term_n) annotation (Line(points={{-30,-60},{-60,
            -60},{-60,-4},{-64,-4}}, color={0,0,255}));
    connect(p_n_pn.term_p, capSym.term_p) annotation (Line(points={{64,4},{60,4},
            {60,10},{30,10}}, color={0,0,255}));
    connect(p_n_pn.term_n, capSym.term_n) annotation (Line(points={{64,-4},{60,
            -4},{60,-10},{30,-10}}, color={0,0,255}));
    connect(res1.term_p, capSym.term_p) annotation (Line(points={{-30,60},{30,
            60},{30,10}}, color={0,0,255}));
    connect(capSym.term_n, cap1.term_n) annotation (Line(points={{30,-10},{30,
            -60},{-30,-60}}, color={0,0,255}));
    connect(capSym.neutral, neutral) annotation (Line(points={{20,-6.12323e-016},
            {10,-6.12323e-016},{10,0},{0,0},{0,-100}}, color={0,0,255}));
    connect(term_p, pn_p_n.term_pn)
      annotation (Line(points={{-100,0},{-76,0}}, color={0,0,255}));
    connect(p_n_pn.term_pn, term_n)
      annotation (Line(points={{76,0},{100,0}}, color={0,0,255}));
    annotation (defaultComponentName="dcLink",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-80,40},{80,0}},
            lineColor={0,0,255},
            textString=
                 "DClink"),
          Text(
            extent={{-80,0},{80,-40}},
            lineColor={0,0,255},
            textString=
                 "sym")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end DClinkSym;

  package Partials "Partial models"
    partial model ImpedBase0 "Impedance base 0, 1-phase"

      Ports.TwoPin_p term_p "positive terminal"
    annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=
               0)));
      Ports.TwoPin_n term_n "negative terminal"
    annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));

    annotation (
      Documentation(
            info="<html>
</html>
"),   Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,130},{100,90}},
              lineColor={0,0,0},
              textString=
               "%name")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end ImpedBase0;
    extends Modelica.Icons.BasesPackage;

    partial model ImpedBase "Impedance base, 1-phase"
      extends Ports.Port_pn;
      extends Basic.Nominal.NominalAC;

      parameter Boolean stIni_en=true "enable steady-state initialization"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SI.Voltage[2] v_start = zeros(2)
        "start value of voltage drop" annotation(Dialog(tab="Initialization"));
      parameter SI.Current[2] i_start = zeros(2)
        "start value of current" annotation(Dialog(tab="Initialization"));

      SI.Voltage[2] v(start = v_start);
      SI.Current[2] i(start = i_start);

    protected
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;

    equation
      v = term_p.v - term_n.v;
      i = term_p.i;
    annotation (
      Documentation(
            info="<html>
</html>
"),   Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-80,20},{-60,20}}, color={0,0,255}),
            Line(points={{-80,-20},{-60,-20}}, color={0,0,255}),
            Line(points={{60,20},{80,20}}, color={0,0,255}),
            Line(points={{60,-20},{80,-20}}, color={0,0,255})}));
    end ImpedBase;

    partial model ImpedBaseHeat "Impedance base with heat port, 1-phase"
      extends ImpedBase;
      extends Interfaces.AddHeat;
      annotation (
    Documentation(
          info="<html>
<p>Same as ImpedBase, but contains an additional heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connector is given by the total dissipated electric power of both conductors.</p>
</html>"),
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end ImpedBaseHeat;

  partial model ImpedNonSymHeat
      "Impedance base non symmetric with heat port, 3-phase abc"
    extends ImpedBase;
    extends Interfaces.AddHeatV(     final m_heat=2);

  equation
    Q_flow = v.*i;
    annotation (
    Documentation(
    info="<html>
<p>Same as ImpedBase, but contains an additional vector heat port.</p>
<p>Does not contain mass and specific heat. These parameters are expected to belong to the corresponding thermal model. The heat-flow at the connectors is given by the total dissipated electric power per conductor.</p>
</html>
"),   Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
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
<p>The reactance-matrix is</p>
<pre>
  x = [x_s, x_m
       x_m, x_s]
</pre>
<p>with the relations</p>
<pre>
  x1   = x_s - x_m,         stray reactance
  x0  = x_s + x_m,          zero reactance
  x_s = (x1 + x0)/2,        self reactance single conductor
  x_m = (x0 - x1)/2,        mutual reactance
</pre>
<p>Coupling.</p>
<pre>
  -x_s &lt  x_m &lt  x_s
  uncoupled limit:          x_m = 0,        x0 = x_s
  fully positive coupled:   x_m = x_s,      x0 = 2*x_s
  fully negative coupled:   x_m = -x_s,     x0 = 0
</pre>
<p>The resistance matrix is</p>
<pre>
  r = [r1, 0
       0,  r2]
</pre>
<p>The susceptance matrix is</p>
<pre>
  b = [ b_pg + b_pp, -b_pp
       -b_pp,         b_pg + b_pp]
</pre>
<p>where <tt>_pg</tt> denotes 'phase-to-ground' and <tt>_pp</tt> 'phase-to-phase' in analogy to the three-phase notation. More precisely (for a one-phase system) <tt>_pp</tt> means 'conductor-to-conductor'.</p>
<p>The corresponding conduction matrix is (in analogy to susceptance)</p>
<pre>
  g = [g_pg + g_pp, -g_pp
      -g_pp,         g_pg + g_pp]
</pre>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Impedances;
