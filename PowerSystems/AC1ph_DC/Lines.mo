within PowerSystems.AC1ph_DC;
package Lines "Transmission lines 1-phase"
  extends Modelica.Icons.VariantsPackage;

  model RXline "RX transmission line, 1-phase"
    extends Ports.Port_pn;
    extends Partials.RXlineBase(final ne=1);

    SI.Voltage[2] v(start = v_start);
    SI.Current[2] i(start = i_start);

  initial equation
    if steadyIni_t then
      der(i) = zeros(2);
    elseif not system.steadyIni then
      i = i_start;
    end if;

  equation
    v = term_p.v - term_n.v;
    i = term_p.i;

    L*der(i) + diagonal(R)*i = v;
    annotation (
      Documentation(
              info="<html>
<p>Transmission line modelled as concentrated RX-impedance.</p>
</html>
"),   Icon(coordinateSystem(
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
          Line(points={{-80,20},{-60,20}}, color={0,0,255}),
          Line(points={{-80,-20},{-60,-20}}, color={0,0,255}),
          Line(points={{60,20},{80,20}}, color={0,0,255}),
          Line(points={{60,-20},{80,-20}}, color={0,0,255}),
          Rectangle(
            extent={{-60,25},{-40,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,25},{60,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-15},{-40,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-15},{60,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,5},{60,-5}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end RXline;

  model PIline "PI transmission line, 1-phase"
    extends Ports.Port_p_n;
    extends Partials.PIlineBase;

    SIpu.Voltage[2,ne] v(start = transpose(fill(v_start/ne, ne)));
    SIpu.Current[2,ne1] i(start = transpose(fill(i_start, ne1)));
  protected
    final parameter Integer ne1=ne + 1;

  initial equation
    if steadyIni_t then
      der(v) = zeros(2,ne);
      der(i[:,2:ne1]) = zeros(2,ne);
    elseif not system.steadyIni then
      v = transpose(fill(v_start/ne, ne));
      i[:,1:ne1] = transpose(fill(i_start, ne1));
    end if;

  equation
    i[:, 1] = term_p.i;
    i[:, ne1] = -term_n.i;

    C*der(v) + G*v = i[:, 1:ne] - i[:, 2:ne1];
    L*der(i) + diagonal(R)*i = [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
    annotation (
      Documentation(
              info="<html>
<p>Transmission line modelled as discretised telegraph-equation, 'pi-elements'.</p>
<p>The line of total length <tt>len</tt> is divided into elements of length <tt>delta = len/n</tt>.
It is composed of <tt>n-1</tt> interior elements of length delta and at each end of a half-element of length <tt>delta/2</tt>.
Therefore it contains <tt>n</tt> interior nodes. Each element corresponds to a series resistor-inductor with values R and L corresponding to its length. A shunt parallel capacitor-conductor is linked to each node.<br>
The minimum of <tt>n</tt> is <tt>1</tt>.</p>
<p>This kind of discretisation is slightly more complicated than the division of the line into n identical elements, but it results in a symmetric model with respect to interchanging positive and negative terminal.
The set of equations of two series connected lines of length len1 and len2 is identical to the set of equations for one line of length len1 + len2 if delta1 = delta2. Otherwise differences occur from the different discretisation length.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,30},{80,-32}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,16},{90,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,-11},{90,-16}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-60,10},{-50,10}}, color={0,0,255}),
          Rectangle(
            extent={{-50,12},{-40,8}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,12},{-10,8}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,1},{-10,-1}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-8},{-10,-12}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-8},{-40,-12}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{15,3},{25,1}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{15,-1},{25,-3}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{32,6},{36,-6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-10,10},{60,10}}, color={0,0,255}),
          Line(points={{-60,-10},{-50,-10}}, color={0,0,255}),
          Line(points={{-10,-10},{60,-10}}, color={0,0,255}),
          Line(points={{20,10},{20,3}}, color={0,0,255}),
          Line(points={{20,-3},{20,-10}}, color={0,0,255}),
          Line(points={{34,10},{34,6}}, color={0,0,255}),
          Line(points={{34,-6},{34,-10}}, color={0,0,255}),
          Rectangle(
            extent={{-1,-28},{9,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-1,-32},{9,-34}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{4,-48},{4,-34}}, color={0,0,255}),
          Rectangle(
            extent={{13,-25},{17,-37}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{15,-37},{15,-48}}, color={0,0,255}),
          Line(points={{15,-10},{15,-25}}, color={0,0,255}),
          Line(points={{4,-10},{4,-28}}, color={0,0,255}),
          Rectangle(
            extent={{35,-28},{45,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{35,-32},{45,-34}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{40,-48},{40,-33}}, color={0,0,255}),
          Rectangle(
            extent={{49,-25},{53,-37}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{51,-37},{51,-48}}, color={0,0,255}),
          Line(points={{51,10},{51,-25}}, color={0,0,255}),
          Line(points={{40,10},{40,-28}}, color={0,0,255}),
          Rectangle(
            extent={{0,-48},{54,-50}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid)}));
  end PIline;

model FaultRXline "Faulted RX transmission line, 1-phase"
  extends Ports.Port_p_n_f;
  parameter Real p(min=0,max=1)=0.5 "rel fault-position (0 < p < 1)";
  extends Partials.RXlineBase(final ne=1);

  SI.Current[2] i1(start = i_start);
  SI.Current[2] i2(start = i_start);

initial equation
  if steadyIni_t then
    der(i1) = zeros(2);
    der(i2) = zeros(2);
  elseif not system.steadyIni then
    i1 = i_start;
    i2 = i_start;
  end if;

equation
  term_p.i + term_n.i + term_f.i = {0,0};
  i1 = term_p.i;
  i2 = -term_n.i;

  p*(L*der(i1) + diagonal(R)*i1) = term_p.v - term_f.v;
  (1 - p)*(L*der(i2) + diagonal(R)*i2) = term_f.v - term_n.v;
  annotation (
    defaultComponentName="faultRXline",
Documentation(
        info="<html>
<p>Transmission line modelled as concentrated RX-impedance, with third terminal for connecting line-fault component.</p>
<p>The fault is at relative length <tt>p(0&lt p&lt 1)</tt>:<br>
<pre>  p*len = distance to fault from terminal term_p</pre>
</html>
"),
Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-40,30},{80,-30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-80,30},{-40,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(
            points={{0,80},{-20,0},{20,30},{0,-40}},
            color={255,255,0},
            thickness=0.5)}),
Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-50,-80},{-29,-100}},
            lineColor={95,95,95},
            textString=
               "p"),
          Text(
            extent={{20,-80},{60,-100}},
            lineColor={95,95,95},
            textString=
               "(1-p)"),
          Line(points={{-80,20},{-60,20}}, color={0,0,255}),
          Line(points={{-80,-20},{-60,-20}}, color={0,0,255}),
          Line(points={{60,35},{80,35}}, color={0,0,255}),
          Line(points={{60,0},{80,0}}, color={0,0,255}),
          Line(points={{60,-20},{80,-20}}, color={0,0,255}),
          Rectangle(
            extent={{-60,25},{-50,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,25},{-20,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,2},{-20,-2}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-15},{-50,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-15},{-20,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,25},{30,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,25},{60,15}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,2},{60,-2}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,-15},{30,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,-15},{60,-25}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,20},{20,20}}, color={0,0,255}),
          Line(points={{-20,-20},{20,-20}}, color={0,0,255}),
          Line(points={{-10,20},{-10,80}}, color={0,0,255}),
          Line(points={{10,-20},{10,80}}, color={0,0,255}),
          Line(
            points={{-60,-70},{-20,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled}),
          Line(
            points={{20,-70},{60,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled})}));
end FaultRXline;

model FaultPIline "Faulted PI transmission line, 1-phase"
  extends Ports.Port_p_n_f;
  parameter Real p(min=0.5/ne,max=1-0.5/ne)=0.5
      "rel fault-pos (1/2ne <= p < 1 - 1/2ne)";
  extends Partials.PIlineBase;

  SI.Voltage[2,ne] v(start = transpose(fill(v_start/ne, ne)));
  SI.Current[2,ne1] i(start = transpose(fill(i_start, ne1)));
  SI.Current[2] iF;
  SI.Current[2,2] iF_p(each stateSelect=StateSelect.never);
  protected
  final parameter Integer ne1=ne + 1;
  final parameter Integer nF=integer(ne*p + 1.5);
  final parameter Real pe=min(0.9, max(0.1, ne*p + 1.5 - nF))
      "relative fault position within element nF";

initial equation
  if steadyIni_t then
    der(v) = zeros(2,ne);
    der(i[:,2:ne1]) = zeros(2,ne);
  elseif not system.steadyIni then
    v = transpose(fill(v_start/ne, ne));
    i[:,1:ne1] = transpose(fill(i_start, ne1));
  end if;

equation
  i[:, 1] = term_p.i;
  i[:, ne1] = -term_n.i;
  iF = -term_f.i;
  iF_p = [(1-pe)*iF, pe*iF];

  C*der(v) + G*v = [i[:,1:nF-2]-i[:, 2:nF-1], i[:,nF-1:nF]-i[:,nF:nF+1]-iF_p, i[:,nF+1:ne]-i[:,nF+2:ne1]];
  L*der(i) + diagonal(R)*i = [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
  L*der(iF) + diagonal(R)*iF = (v[:, nF-1] - term_f.v)/pe + (v[:, nF] - term_f.v)/(1-pe);
  annotation (
    defaultComponentName="faultPIline",
Documentation(
        info="<html>
<p>Transmission line modelled as discretised telegraph-equation, 'pi-elements'.</p>
<p>The line of total length <tt>len</tt> is divided into elements of length <tt>delta = len/n</tt>.
It is composed of <tt>n-1</tt> interior elements of length delta and at each end of a half-element of length <tt>delta/2</tt>.
Therefore it contains <tt>n</tt> interior nodes. Each element corresponds to a series inductor-resistor with values R and L corresponding to its length. A shunt parallel capacitor-conductor is liked to each node.<br>
The minimum of <tt>n</tt> is <tt>1</tt>.</p>
<p>The fault is at relative length <tt>p(0&lt p&lt 1)</tt>:<br>
<pre>  p*len = distance to fault from terminal term_p</pre>
<p><tt>p</tt> is restricted in such a way that faults do not occur in the end-elements of the line. Furthermore the position within an element is restricted to a relative position between <tt>0.1</tt> and <tt>0.9</tt> for numerical reasons.</p>
</html>"),
Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,30},{80,-30}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,16},{90,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,-11},{90,-16}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(
            points={{0,80},{-20,0},{20,30},{0,-40}},
            color={255,255,0},
            thickness=0.5)}),
Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-50,-80},{-29,-100}},
            lineColor={95,95,95},
            textString=
               "p"),
          Text(
            extent={{20,-80},{60,-100}},
            lineColor={95,95,95},
            textString=
               "(1-p)"),
          Line(
            points={{-60,-70},{-20,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled}),
          Line(
            points={{20,-70},{60,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled}),
          Rectangle(
            extent={{-60,11},{-20,9}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-9},{-20,-11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,11},{60,9}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,-9},{60,-11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,10},{20,10}}, color={0,0,255}),
          Line(points={{-20,-10},{20,-10}}, color={0,0,255}),
          Line(points={{-10,10},{-10,80}}, color={0,0,255}),
          Line(points={{10,-10},{10,80}}, color={0,0,255}),
          Line(points={{-80,10},{-60,10}}, color={0,0,255}),
          Line(points={{-80,-10},{-60,-10}}, color={0,0,255}),
          Line(points={{60,10},{80,10}}, color={0,0,255}),
          Line(points={{60,-11},{80,-11}}, color={0,0,255})}));
end FaultPIline;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model RXlineBase "RX-line base, 1-phase"

      parameter Types.Length len=100e3 "line length";
      parameter Integer ne(min=1)=1 "number of pi-elements";
      replaceable parameter Parameters.RXline par "line parameter"
                                           annotation (Placement(transformation(
              extent={{-80,60},{-60,80}})));

      parameter Boolean stIni_en=true "enable steady-state initialization"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SI.Voltage[2] v_start = zeros(2) "start value of voltage drop"
                                       annotation(Dialog(tab="Initialization"));
      parameter SI.Current[2] i_start = zeros(2) "start value of current"
                                  annotation(Dialog(tab="Initialization"));

    protected
      outer System system;
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;
      final parameter Real[2] RL_base=Basic.Precalculation.baseRL(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      final parameter Real delta_len_km(final quantity="Length", final unit="km")=len/1e3/ne;
      final parameter SI.Resistance[2] R=par.r*delta_len_km*RL_base[1];
      final parameter SI.Inductance[2,2] L=([(par.x + par.x0),(par.x0 - par.x);(par.x0 - par.x),(par.x + par.x0)]/2)*delta_len_km*RL_base[2];
      annotation (
        Documentation(
              info="<html>
<p>Precalculation of coefficient matrices.</p>
</html>
"));
    end RXlineBase;

    partial model PIlineBase "PI-line base, 1-phase"
      extends RXlineBase(ne=3, redeclare replaceable parameter
          PowerSystems.AC1ph_DC.Lines.Parameters.PIline par);

    protected
      final parameter Real[2] GC_base=Basic.Precalculation.baseGC(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      final parameter SI.Conductance[2,2] G=[par.g_pg+par.g_pp,-par.g_pp;-par.g_pp,par.g_pg+par.g_pp]*delta_len_km*GC_base[1];
      final parameter SI.Capacitance[2,2] C=[par.b_pg+par.b_pp,-par.b_pp;-par.b_pp,par.b_pg+par.b_pp]*delta_len_km*GC_base[2];

      annotation (
        Documentation(
              info="<html>
<p>Precalculation of coefficient matrices.</p>
</html>
"));
    end PIlineBase;

  end Partials;

package Parameters "Parameter data for interactive use"
 extends Modelica.Icons.MaterialPropertiesPackage;

  record RXline "RX-line parameters, 1-phase"
    extends Basic.Nominal.NominalDataAC(
                                     S_nom=100e6);
    parameter SIpu.Resistance[2] r={0.1,0.1}*1e-3 "resistance/km";
    parameter SIpu.Reactance_km x=1e-3 "reactance/km";
    parameter SIpu.Reactance_km x0(min=x)=3*x "reactance/km zero-comp";

      annotation (
        defaultComponentName="data",
        Documentation(info=
     "<html>
<p>Relations.</p>
<pre>
  x = 2*pi*f_nom*L/R_base     reactance
  r = R / R_base              resistance
</pre>
<p>Coupling.</p>
<pre>
  positive coupled:     x0 &gt  x
  uncoupled limit:      x0 = x
</pre>
<p>More info see package AC1ph_DC.Impedances.</p>
</html>"));
  end RXline;

  record PIline "PI-line parameters, 1-phase"
    extends RXline;
    parameter SIpu.Conductance g_pg=0 "shunt conductance/km ph-grd";
    parameter SIpu.Conductance g_pp=0 "shunt conductance/km ph_ph";
    parameter SIpu.Susceptance_km b_pg=0.025e-3 "susceptance/km ph-grd";
    parameter SIpu.Susceptance_km b_pp=0.025e-3 "susceptance/km ph-ph";

      annotation (
        defaultComponentName="data",
  Documentation(
  info="<html>
<p>Relations.</p>
<pre>
  g = G/G_base                  conductance
  b = (2*pi*f_nom*C)/G_base     susceptance
  G_base = 1/R_base
</pre>
<p>where <tt>_pg</tt> denotes phase-to-ground, and <tt>_pp</tt> phase-to-phase.</p>
<p>More info see package AC1ph_DC.Impedances.</p>
</html>
"));
  end PIline;
 annotation (preferredView="info",
    Documentation(info=
                  "<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"));
end Parameters;
  annotation (preferredView="info",
Documentation(info="<html>
<p>Different types of transmission line models.<br>
Faulted transmission lines contain a third terminal for connection to a fault-component.</p>
<p>The relations between line reactance (<tt>x,x0</tt>) and self- and mutual reactance (<tt>x_s,x_m</tt>) are</p>
<pre>
  x   = x_s - x_m,          reactance dq (stray reactance)
  x0  = x_s + x_m,          reactance o (zero-component reactance)
  x_s = (x + x0)/2,         self reactance single conductor
  x_m = (x0 - x)/2,         mutual reactance
</pre>
<p>Coupling:</p>
<pre>  cpl = x_m/x_s &gt  0,        positive for lines</pre>
<p>More info see package AC1ph_DC.Impedances.</p>
</html>
"));
end Lines;
