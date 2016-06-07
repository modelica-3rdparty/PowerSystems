within PowerSystems.AC3ph;
package Lines "Transmission lines 3-phase"
  extends Modelica.Icons.VariantsPackage;

  model RXline0 "RX line without length parameter, 3-phase dq0"
    extends Impedances.Partials.ImpedBase;

    parameter SIpu.Resistance r=0.01 "resistance";
    parameter SIpu.Reactance x=0.1 "reactance";
    parameter SIpu.Reactance x0=3*x "reactance zero-comp";
  protected
    final parameter SI.Resistance[2] RL_base=Basic.Precalculation.baseRL(puUnits, V_nom, S_nom, 2*pi*f_nom);
    final parameter SI.Resistance R=r*RL_base[1];
    final parameter SI.Inductance L=x*RL_base[2];
    final parameter SI.Inductance L0=x0*RL_base[2];

  initial equation
    if steadyIni_t then
      der(i) = omega[1]*j_dq0(i);
    elseif not system.steadyIni then
      i = i_start;
    end if;

  equation
    if system.transientSim then
      diagonal({L,L,L0})*der(i) + omega[2]*L*j_dq0(i) + R*i = v;
    else
      omega[2]*L*j_dq0(i) + R*i = v;
    end if;
  annotation (
    defaultComponentName="RXline0_1",
      Documentation(
        info="<html>
<p>This component contains the same equations as 'Impedances.Inductor', but it is specified using the parameters x and x0 instead of xs and xm (see info package 'Impedances'), similar to 'RXline'. It does not contain the length parameter 'len'. Together with 'Sources.InfBus' it may be used to model a network specified by voltage and impedance values.</p>
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
  end RXline0;

  model RXline "RX transmission line, 3-phase dq0"
    extends Ports.Port_pn;
    extends Partials.RXlineBase(final ne=1);

    SI.Voltage[3] v(start = v_start);
    SI.Current[3] i(start = i_start);
  protected
    SI.AngularFrequency[2] omega;

  initial equation
    if steadyIni_t then
      der(i) = omega[1]*j_dq0(i);
    elseif not system.steadyIni then
      i = i_start;
    end if;

  equation
    omega = der(term_p.theta);
    v = term_p.v - term_n.v;
    i = term_p.i;

    if system.transientSim then
      diagonal({L,L,L0})*der(i) + omega[2]*L*j_dq0(i) + R*i = v;
    else
      omega[2]*L*j_dq0(i) + R*i = v;
    end if;
    annotation (
      defaultComponentName="RXline1",
  Documentation(
          info="<html>
<p>Transmission line modelled as concentrated RX-impedance.</p>
</html>
"),
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
          Line(points={{-80,35},{-60,35}}, color={0,0,255}),
          Line(points={{-80,0},{-60,0}}, color={0,0,255}),
          Line(points={{-80,-35},{-60,-35}}, color={0,0,255}),
          Rectangle(
            extent={{-60,40},{-40,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,40},{60,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,20},{60,16}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,6},{-40,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,6},{60,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-30},{-40,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-30},{60,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-16},{60,-20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-49},{60,-53}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{60,35},{80,35}}, color={0,0,255}),
          Line(points={{60,0},{80,0}}, color={0,0,255}),
          Line(points={{60,-35},{80,-35}}, color={0,0,255})}));
  end RXline;

  model PIline "PI transmission line, 3-phase dq0"
    extends Ports.Port_p_n;
    extends Partials.PIlineBase;

    SI.Voltage[3,ne] v(start = transpose(fill(v_start/ne, ne)));
    SI.Current[3,ne1] i(start = transpose(fill(i_start, ne1)));
  protected
    final parameter Integer ne1=ne + 1;
    SI.AngularFrequency[2] omega;

  initial equation
    if steadyIni_t then
      der(v) = omega[1]*jj_dq0(v);
      der(i) = omega[1]*jj_dq0(i);
    elseif system.steadyIni_t then
      der(v) = omega[1]*jj_dq0(v);
      der(i[:,2:ne1]) = omega[1]*jj_dq0(i[:,2:ne1]);
    elseif not system.steadyIni then
      v = transpose(fill(v_start/ne, ne));
      i[:,1:ne1] = transpose(fill(i_start, ne1));
    end if;

  equation
    omega = der(term_p.theta);
    i[:, 1] = term_p.i;
    i[:, ne1] = -term_n.i;

    if system.transientSim then
      diagonal({C,C,C0})*der(v) + omega[2]*C*jj_dq0(v) + G*v =
       i[:,1:ne] - i[:,2:ne1];
      diagonal({L,L,L0})*der(i) + omega[2]*L*jj_dq0(i) + R*i =
       [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
    else
      omega[2]*C*jj_dq0(v) + G*v = i[:,1:ne] - i[:,2:ne1];
      omega[2]*L*jj_dq0(i) + R*i =
       [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
    end if;
    annotation (
      defaultComponentName="PIline1",
  Documentation(
          info="<html>
<p>Transmission line modelled as discretised telegraph-equation, 'pi-elements'.</p>
<p>The line of total length <tt>len</tt> is divided into elements of length <tt>delta = len/n</tt>.
It is composed of <tt>n-1</tt> interior elements of length <tt>delta</tt> and at each end of a half-element of length <tt>delta/2</tt>. Therefore it contains <tt>n</tt> interior nodes. Each element corresponds to a series resistor-inductor with values R and L corresponding to its length. A shunt parallel capacitor-conductor is linked to each node.<br>
The minimum of n is 1.</p>
<p>This kind of discretisation is slightly more complicated than the division of the line into n identical elements, but it results in a symmetric model with respect to interchanging positive and negative terminal.
The set of equations of two series connected lines of length len1 and len2 is identical to the set of equations for one line of length len1 + len2 if delta1 = delta2. Otherwise differences occur from the different discretisation length.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-90,30},{90,-30}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,25},{90,20}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,2.5},{90,-2.5}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,-20},{90,-25}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-70,18},{-60,18}}, color={0,0,255}),
          Rectangle(
            extent={{-60,20},{-50,16}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{-20,16}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,10},{-20,8}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,2},{-50,-2}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,2},{-20,-2}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-8},{-20,-10}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-16},{-20,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-16},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,13},{0,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,9},{0,7}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-7},{0,-9}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,-11},{0,-13}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{14,3},{24,1}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{14,-1},{24,-3}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{6,16},{10,4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{6,-4},{10,-16}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,6},{34,-6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,18},{70,18}}, color={0,0,255}),
          Line(points={{-70,0},{-60,0}}, color={0,0,255}),
          Line(
            points={{10,0},{60,0}},
            color={0,0,255},
            pattern=LinePattern.Dot),
          Line(points={{-70,-18},{-60,-18}}, color={0,0,255}),
          Line(points={{-20,-18},{70,-18}}, color={0,0,255}),
          Line(points={{-5,12},{-5,18}}, color={0,0,255}),
          Line(points={{-5,-18},{-5,-12}}, color={0,0,255}),
          Line(points={{-5,7},{-5,-7}}, color={0,0,255}),
          Line(points={{19,18},{19,2}}, color={0,0,255}),
          Line(points={{19,-2},{19,-18}}, color={0,0,255}),
          Line(points={{32,18},{32,6}}, color={0,0,255}),
          Line(points={{32,-6},{32,-18}}, color={0,0,255}),
          Line(points={{60,0},{70,0}}, color={0,0,255}),
          Line(points={{8,18},{8,16}}, color={0,0,255}),
          Line(points={{8,-16},{8,-18}}, color={0,0,255}),
          Line(points={{8,4},{8,-4}}, color={0,0,255}),
          Line(points={{-20,0},{12,0}}, color={0,0,255}),
          Rectangle(
            extent={{-50,-25},{-20,-27}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-16,-37},{-6,-39}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-16,-41},{-6,-43}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-10,-60},{-10,-42}}, color={0,0,255}),
          Rectangle(
            extent={{-2,-34},{2,-46}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,-46},{0,-60}}, color={0,0,255}),
          Line(points={{0,-18},{0,-34}}, color={0,0,255}),
          Line(points={{-11,-18},{-11,-37}}, color={0,0,255}),
          Rectangle(
            extent={{8,-37},{18,-39}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,-41},{18,-43}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{13,-60},{13,-42}}, color={0,0,255}),
          Rectangle(
            extent={{24,-34},{28,-46}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{26,-46},{26,-60}}, color={0,0,255}),
          Rectangle(
            extent={{41,-37},{51,-39}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{41,-41},{51,-43}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{46,-60},{46,-42}}, color={0,0,255}),
          Rectangle(
            extent={{55,-34},{59,-46}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{57,-46},{57,-60}}, color={0,0,255}),
          Line(points={{57,18},{57,-34}}, color={0,0,255}),
          Line(points={{46,18},{46,-37}}, color={0,0,255}),
          Line(points={{26,0},{26,-28},{26,-28},{26,-34}}, color={0,0,255}),
          Line(points={{13,0},{13,-28},{13,-28},{13,-38}}, color={0,0,255}),
          Rectangle(
            extent={{-13,-60},{60,-62}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid)}));
  end PIline;

  model FaultRXline "Faulted RX transmission line, 3-phase dq0"
    extends Ports.Port_p_n_f;
    parameter Real p(min=0,max=1)=0.5 "rel fault-position (0 < p < 1)";
    extends Partials.RXlineBase(final ne=1);

    SI.Current[3] i1(start = i_start);
    SI.Current[3] i2(start = i_start);
  protected
    SI.AngularFrequency[2] omega;

  initial equation
    if steadyIni_t then
      der(i1) = omega[1]*j_dq0(i1);
      der(i2) = omega[1]*j_dq0(i2);
    elseif system.steadyIni_t then
      der(term_f.i) = omega[1]*j_dq0(term_f.i);
    elseif not system.steadyIni then
      i1 = i_start;
      i2 = i_start;
    end if;

  equation
    omega = der(term_p.theta);
    term_p.i + term_n.i + term_f.i = zeros(3);
    i1 = term_p.i;
    i2 = -term_n.i;

    if system.transientSim then
      p*(diagonal({L,L,L0})*der(i1) + omega[2]*L*j_dq0(i1) + R*i1) =
       term_p.v - term_f.v;
      (1 - p)*(diagonal({L,L,L0})*der(i2) + omega[2]*L*j_dq0(i2) + R*i2) =
       term_f.v - term_n.v;
    else
      p*(omega[2]*L*j_dq0(i1) + R*i1) = term_p.v - term_f.v;
      (1 - p)*(omega[2]*L*j_dq0(i2) + R*i2) = term_f.v - term_n.v;
    end if;
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
            lineColor={0,120,120},
            lineThickness=0.5,
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-80,30},{-40,-30}},
            lineColor={0,120,120},
            lineThickness=0.5,
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
          Line(points={{-80,35},{-60,35}}, color={0,0,255}),
          Line(points={{-80,0},{-60,0}}, color={0,0,255}),
          Line(points={{-80,-35},{-60,-35}}, color={0,0,255}),
          Line(points={{60,35},{80,35}}, color={0,0,255}),
          Line(points={{60,0},{80,0}}, color={0,0,255}),
          Line(points={{60,-35},{80,-35}}, color={0,0,255}),
          Rectangle(
            extent={{-60,40},{-50,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,40},{-20,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{-20,16}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,5},{-50,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,5},{-20,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-30},{-50,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-30},{-20,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-16},{-20,-20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,40},{30,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,40},{60,30}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,20},{60,16}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,5},{30,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,5},{60,-5}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,-30},{30,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,-30},{60,-40}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,-16},{60,-20}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,35},{20,35}}, color={0,0,255}),
          Line(points={{-20,0},{20,0}}, color={0,0,255}),
          Line(points={{-20,-35},{20,-35}}, color={0,0,255}),
          Line(points={{-10,35},{-10,80}}, color={0,0,255}),
          Line(points={{0,0},{0,80}}, color={0,0,255}),
          Line(points={{10,-35},{10,80}}, color={0,0,255}),
          Line(
            points={{-60,-70},{-20,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled}),
          Line(
            points={{20,-70},{60,-70}},
            color={95,95,95},
            arrow={Arrow.Filled,Arrow.Filled}),
          Rectangle(
            extent={{-50,-49},{-20,-53}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{30,-49},{60,-53}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end FaultRXline;

  model FaultPIline "Faulted PI transmission line, 3-phase dq0"
    extends Ports.Port_p_n_f;
    parameter Real p(min=0.5/ne,max=1-0.5/ne)=0.5
      "rel fault-pos (1/2ne <= p < 1 - 1/2ne)";
    extends Partials.PIlineBase;

    parameter SI.Current[3] iF_start = zeros(3) "start value of fault current"
      annotation(Dialog(tab="Initialization"));

    SI.Voltage[3,ne] v(start = transpose(fill(v_start/ne, ne)));
    SI.Current[3,ne1] i(start = transpose(fill(i_start, ne1)));
    SI.Current[3] iF(start = iF_start);
    SI.Current[3,2] iF_p(each stateSelect=StateSelect.never);
  protected
    final parameter Integer ne1=ne + 1;
    final parameter Integer nF=integer(ne*p + 1.5);
    final parameter Real pe=min(0.9, max(0.1, ne*p + 1.5 - nF))
      "relative fault position within element nF";
    SI.AngularFrequency[2] omega;

  initial equation
    if steadyIni_t then
      der(v) = omega[1]*jj_dq0(v);
      der(i) = omega[1]*jj_dq0(i);
      der(iF) = omega[1]*j_dq0(iF);
    elseif system.steadyIni_t then
      der(v) = omega[1]*jj_dq0(v);
      der(i[:,2:ne1]) = omega[1]*jj_dq0(i[:,2:ne1]);
      der(iF) = omega[1]*j_dq0(iF);
    elseif not system.steadyIni then
      v = transpose(fill(v_start/ne, ne));
      i[:,1:ne1] = transpose(fill(i_start, ne1));
    end if;

  equation
    omega = der(term_p.theta);
    i[:, 1] = term_p.i;
    i[:, ne1] = -term_n.i;
    iF = -term_f.i;
    iF_p = [(1-pe)*iF, pe*iF];

    if system.transientSim then
      diagonal({C,C,C0})*der(v) + omega[2]*C*jj_dq0(v) + G*v =
       [i[:,1:nF-2]-i[:, 2:nF-1], i[:,nF-1:nF]-i[:,nF:nF+1]-iF_p, i[:,nF+1:ne]-i[:,nF+2:ne1]];
      diagonal({L,L,L0})*der(i) + omega[2]*L*jj_dq0(i) + R*i =
       [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
      diagonal({L,L,L0})*der(iF) + omega[2]*L*j_dq0(iF) + R*iF =
       (v[:, nF-1] - term_f.v)/pe + (v[:, nF] - term_f.v)/(1-pe);
    else
      omega[2]*C*jj_dq0(v) + G*v =
       [i[:,1:nF-2]-i[:, 2:nF-1], i[:,nF-1:nF]-i[:,nF:nF+1]-iF_p, i[:,nF+1:ne]-i[:,nF+2:ne1]];
      omega[2]*L*jj_dq0(i) + R*i =
       [[2*(term_p.v - v[:, 1])], v[:, 1:ne - 1] - v[:, 2:ne], [2*(v[:, ne] - term_n.v)]];
      omega[2]*L*j_dq0(iF) + R*iF =
       (v[:, nF-1] - term_f.v)/pe + (v[:, nF] - term_f.v)/(1-pe);
    end if;
    annotation (
      defaultComponentName="faultPIline",
  Documentation(
          info="<html>
<p>Transmission line modelled as discretised telegraph-equation, 'pi-elements'.</p>
<p>The line of total length <tt>len</tt> is divided into elements of length <tt>delta = len/n</tt>.
It is composed of <tt>n-1</tt> interior elements of length <tt>delta</tt> and at each end of a half-element of length <tt>delta/2</tt>. Therefore it contains <tt>n</tt> interior nodes. Each element corresponds to a series inductor-resistor with values R and L corresponding to its length. A shunt parallel capacitor-conductor is liked to each node.<br>
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
            extent={{-90,30},{90,-30}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,25},{90,20}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,2.5},{90,-2.5}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-90,-20},{90,-25}},
            lineColor={0,120,120},
            fillColor={0,120,120},
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
            extent={{-60,1},{-20,-1}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,12},{-20,10}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-10},{-20,-12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,1},{60,-1}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,12},{60,10}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,-10},{60,-12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,11},{20,11}}, color={0,0,255}),
          Line(points={{-20,0},{20,0}}, color={0,0,255}),
          Line(points={{-20,-11},{20,-11}}, color={0,0,255}),
          Line(points={{-10,11},{-10,80}}, color={0,0,255}),
          Line(points={{0,0},{0,80}}, color={0,0,255}),
          Line(points={{10,-11},{10,80}}, color={0,0,255}),
          Line(points={{-80,11},{-60,11}}, color={0,0,255}),
          Line(points={{-80,0},{-60,0}}, color={0,0,255}),
          Line(points={{-80,-11},{-60,-11}}, color={0,0,255}),
          Line(points={{60,11},{80,11}}, color={0,0,255}),
          Line(points={{60,0},{80,0}}, color={0,0,255}),
          Line(points={{60,-11},{80,-11}}, color={0,0,255})}));
  end FaultPIline;

  package Partials "Partial models"
    extends Modelica.Icons.MaterialPropertiesPackage;

    partial model RXlineBase "RX-line base, 3-phase dq0"

      parameter Boolean stIni_en=true "enable steady-state initial equation"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SI.Voltage[3] v_start = zeros(3) "start value of voltage drop"
        annotation(Dialog(tab="Initialization"));
      parameter SI.Current[3] i_start = zeros(3) "start value of current"
        annotation(Dialog(tab="Initialization"));

      parameter Types.Length len=100e3 "line length";
      parameter Integer ne(min=1)=1 "number of pi-elements";
      replaceable record Data = PowerSystems.AC3ph.Lines.Parameters.RXline
        constrainedby PowerSystems.AC3ph.Lines.Parameters.RXline
        "line parameters"
        annotation (choicesAllMatching=true);
      final parameter Data par "line parameters"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    protected
      outer System system;
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;
      final parameter SI.Resistance[2] RL_base=Basic.Precalculation.baseRL(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      final parameter Real delta_len_km(final quantity="Length", final unit="km")=len/1e3/ne;
      final parameter SI.Resistance R=par.r*delta_len_km*RL_base[1];
      final parameter SI.Inductance L=par.x*delta_len_km*RL_base[2];
      final parameter SI.Inductance L0=par.x0*delta_len_km*RL_base[2];
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficient matrices.</p>
</html>
"));

    end RXlineBase;

    partial model PIlineBase "PI-line base, 3-phase dq0"
      extends RXlineBase(ne=3, redeclare replaceable record Data =
          PowerSystems.AC3ph.Lines.Parameters.PIline);
    protected
      final parameter Real[2] GC_base=Basic.Precalculation.baseGC(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      final parameter SI.Conductance G=(par.g_pg + 3*par.g_pp)*delta_len_km*GC_base[1];
      final parameter SI.Capacitance C=(par.b_pg + 3*par.b_pp)*delta_len_km*GC_base[2];
      final parameter SI.Capacitance C0=par.b_pg*delta_len_km*GC_base[2];
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficient matrices.</p>
</html>
"));
    end PIlineBase;

  end Partials;

 package Parameters "Parameter data for interactive use"
  extends Modelica.Icons.BasesPackage;

   record RXline "RX-line parameters, 3-phase"
     extends Basic.Nominal.NominalDataAC(
                                      S_nom=100e6);
     SIpu.Resistance_km r=0.1e-3 "resistance/km" annotation(Dialog);
     SIpu.Reactance_km x=1e-3 "reactance/km" annotation(Dialog);
     SIpu.Reactance_km x0=3*x "reactance/km zero-comp" annotation(Dialog);

     annotation (defaultComponentName="data",
         defaultComponentPrefixes="parameter",
         Documentation(info="<html>
<p>Relations.</p>
<pre>
  x = 2*pi*f_nom*L/R_base     reactance
  r = R / R_base              resistance
</pre>
<p>Coupling.</p>
<pre>
  positive coupled     x0 > x
  uncoupled limit      x0 = x
</pre>
<p>More info see package ACabc.Impedances.</p>
</html>
"));
   end RXline;

   record PIline "PI-line parameters, 3-phase"
     extends RXline;

     SIpu.Conductance_km g_pg=0 "shunt conductance/km ph-grd" annotation(Dialog);
     SIpu.Conductance_km g_pp=0 "shunt conductance/km ph_ph" annotation(Dialog);
     SIpu.Susceptance_km b_pg=0.025e-3 "susceptance/km ph-grd" annotation(Dialog);
     SIpu.Susceptance_km b_pp=0.025e-3 "susceptance/km ph-ph" annotation(Dialog);

     annotation (defaultComponentName="data",
       defaultComponentPrefixes="parameter",
       Documentation(info="<html>
<p>Relations.</p>
<pre>
  g = G/G_base                  conductance
  b = (2*pi*f_nom*C)/G_base     susceptance
  G_base = 1/R_base
</pre>
<p>where <tt>_pg</tt> denotes phase-to-ground, and <tt>_pp</tt> phase-to-phase.</p>
<p>More info see package ACabc.Impedances.</p>
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
<p>Contains different types of transmission line models.<br>
Faulted transmission lines contain a third terminal for connection to a fault-component.</p>
<p> The relations between line reactance (<tt>x,x0</tt>) and self- and mutual reactance (<tt>x_s,x_m</tt>) are</p>
<pre>
  x   = x_s - x_m,          reactance dq (stray reactance)
  x0  = x_s + 2*x_m,        reactance o (zero-component reactance)
  x_s =  (2*x + x0)/3       self reactance single conductor
  x_m = -(x - x0)/3         mutual reactance
</pre>
<p>Coupling:</p>
<pre>
  cpl = x_m/x_s &gt  0,        positive for lines
</pre>
<p>More info see package ACdq0Impedances.</p>
</html>"));
end Lines;
