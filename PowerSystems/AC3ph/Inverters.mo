within PowerSystems.AC3ph;
package Inverters "Rectifiers and Inverters"
  extends Modelica.Icons.VariantsPackage;

block Select "Select frequency and voltage-phasor type"
  extends PowerSystems.Basic.Icons.Block;

  parameter Boolean fType_sys = true "= true, if inverter has system frequency"
                                               annotation(Evaluate=true, choices(__Dymola_checkBox=true));
  parameter Boolean fType_par = true
      "= true, if inverter has parameter frequency, otherwise defined by input omega"
                                annotation(Evaluate=true, Dialog(enable=not fType_sys));

  parameter SI.Frequency f=system.f "frequency"
    annotation(Dialog(enable=fType_par));
  parameter Boolean uType_par = true
      "= true: uPhasor defined by parameter otherwise by input signal"
   annotation(Evaluate=true, choices(__Dymola_checkBox=true));

  parameter Real u0=1 "voltage ampl pu vDC/2"
                                         annotation(Dialog(enable=uType_par));
  parameter SI.Angle alpha0=0 "phase angle" annotation(Dialog(enable=uType_par));

  Modelica.Blocks.Interfaces.RealInput[2] uPhasor if not uType_par
      "{abs(u), phase(u)}"
    annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  Modelica.Blocks.Interfaces.RealInput omega(final unit="rad/s") if not fType_par
      "ang frequency"
    annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  Modelica.Blocks.Interfaces.RealOutput[2] uPhasor_out
      "{abs(u), phase(u)} to inverter"
    annotation (Placement(transformation(
          origin={60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  Modelica.Blocks.Interfaces.RealOutput theta_out
      "abs angle to inverter, der(theta)=omega"
    annotation (Placement(transformation(
          origin={-60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  outer System system;

  protected
  SI.Angle theta;
  parameter Types.FreqType fType = if fType_sys then Types.FreqType.sys else
                                       if fType_par then Types.FreqType.par else Types.FreqType.sig
      "frequency type";
  Modelica.Blocks.Interfaces.RealInput omega_internal
      "Needed to connect to conditional connector";
  Modelica.Blocks.Interfaces.RealInput[2] uPhasor_internal
      "Needed to connect to conditional connector";

initial equation
  if fType == Types.FreqType.sig then
    theta = 0;
  end if;

equation
  connect(omega, omega_internal);
  connect(uPhasor, uPhasor_internal);
  if fType <> Types.FreqType.sig then
     omega_internal = 0.0;
  end if;
  if uType_par then
     uPhasor_internal = {0,0};
  end if;

  if fType == Types.FreqType.sys then
    theta = system.theta;
  elseif fType == Types.FreqType.par then
    theta = 2*pi*f*(time - system.initime);
  elseif fType == Types.FreqType.sig then
    der(theta) = omega_internal;
  end if;
  theta_out = theta;

  if uType_par then
    uPhasor_out[1] = u0;
    uPhasor_out[2] = alpha0;
  else
    uPhasor_out[1] = uPhasor_internal[1];
    uPhasor_out[2] = uPhasor_internal[2];
  end if;
annotation (defaultComponentName="select1",
  Documentation(
          info="<html>
<p>This is an optional component. If combined with an inverter, a structure is obtained that is equivalent to a voltage source.<br>
The component is not needed, if specific control components are available.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,20},{100,-20}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
               "%name"), Rectangle(
            extent={{-80,-80},{-40,-120}},
            lineColor={213,170,255},
            fillColor={213,170,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Select;

model Rectifier "Rectifier, 3-phase dq0"
  extends Partials.AC_DC_base(heat(final m=3));

  replaceable Components.RectifierEquation rectifier "rectifier model"
    annotation (                         choices(
    choice(redeclare PowerSystems.AC3ph.Inverters.Components.RectifierEquation
            rectifier "equation, with losses"),
    choice(redeclare PowerSystems.AC3ph.Inverters.Components.RectifierModular
            rectifier "modular, with losses")), Placement(transformation(extent=
             {{-10,-10},{10,10}}, rotation=0)));

equation
  connect(AC, rectifier.AC) annotation (Line(points={{100,0},{10,0}}, color={0,
            120,120}));
  connect(rectifier.DC, DC)
      annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
  connect(rectifier.heat, heat)
      annotation (Line(points={{0,10},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="rectifier",
  Documentation(
          info="<html>
<p>Passive rectifier, allows choosing between equation-based and modular version.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Rectifier;

model RectifierAverage "Rectifier time-average, 3-phase dq0"
  extends Partials.SwitchEquation(heat(final m=1));

  replaceable parameter Semiconductors.Ideal.SCparameter par(final Hsw_nom=0)
      "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  parameter Real sigma=1 "power correction" annotation(choices(
    choice=1.0966227 "Sigma",
    choice=1.0 "1"));
  protected
  final parameter Real S_abs=sigma*(2*sqrt(6)/pi);
  final parameter SI.Resistance R_nom=par.V_nom/par.I_nom;
  Real cT;
  Real iAC2;

equation
  cT = if size(par.cT_loss,1)==0 then 1 else loss(T[1]-par.T0_loss, par.cT_loss);
  iAC2 = AC.i*AC.i;

  switch_dq0 = S_abs*AC.i/(sqrt(iAC2) + 1e-5);
  v_dq0 = (vDC1 + cT*par.Vf)*switch_dq0;

  Q_flow = {par.eps[1]*R_nom*iAC2 + (2*sqrt(6)/pi)*par.Vf*sqrt(iAC2)};
  annotation (defaultComponentName="rectifier",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-70},{100,-90}},
            lineColor={176,0,0},
            textString=
                 "average")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-40,-60},{40,-80}},
            lineColor={176,0,0},
            textString=
                 "time average equation"),
          Polygon(
            points={{-10,-38},{0,-18},{10,-38},{-10,-38}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-10,18},{0,38},{10,18},{-10,18}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,-18},{0,18}}, color={0,0,255}),
          Line(points={{-10,-18},{10,-18}}, color={0,0,255}),
          Line(points={{-10,38},{10,38}}, color={0,0,255}),
          Line(points={{0,0},{60,0}}, color={0,0,255}),
          Line(points={{-70,10},{-60,10},{-60,52},{0,52},{0,40}}, color={0,0,
                255}),
          Line(points={{-70,-10},{-60,-10},{-60,-50},{0,-50},{0,-38}}, color={0,
                0,255})}),
      Documentation(info="<html>
<p>Time-averaged passive rectifier, with power-correction.<br>
AC_power = DC_power.</p>
<p>sigma-correction (comparison of AC and DC voltage):
<pre>
   sigma=1.0966227

+) increase S_dq0:  better approximation at vAC smaller
   S_abs=(2*sqrt(6)/pi)*sigma

0) no correction:
   S_abs=(2*sqrt(6)/pi)

-) decrease S_dq0: better approximation at vAC larger
   S_abs=(2*sqrt(6)/pi)/sigma
</pre></p>
<p><i>This is not yet a solution with sufficient precision.</i></p>
</html>"));
end RectifierAverage;

model Inverter "Complete modulator and inverter, 3-phase dq0"
  extends Partials.AC_DC_base(heat(final m=3));

  Modelica.Blocks.Interfaces.RealInput theta "abs angle, der(theta)=omega"
    annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  Modelica.Blocks.Interfaces.RealInput[2] uPhasor "desired {abs(u), phase(u)}"
    annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  replaceable Control.Modulation.PWMasyn modulator
    constrainedby Control.Modulation.Partials.ModulatorBase "modulator type"
    annotation (                        choices(
    choice(redeclare PowerSystems.Control.Modulation.PWMasyn modulator
            "sine PWM asyn"),
    choice(redeclare PowerSystems.Control.Modulation.PWMsyn modulator
            "sine PWM syn"),
    choice(redeclare PowerSystems.Control.Modulation.PWMtab modulator
            "sine PWM syn tabulated"),
    choice(redeclare PowerSystems.Control.Modulation.SVPWMasyn modulator
            "SV PWM asyn"),
    choice(redeclare PowerSystems.Control.Modulation.SVPWMsyn modulator
            "SV PWM syn"),
    choice(redeclare PowerSystems.Control.Modulation.SVPWM modulator
            "SV PWM (using control blocks)"),
    choice(redeclare PowerSystems.Control.Modulation.BlockM modulator
            "block modulation (no PWM)")), Placement(transformation(extent={{
              -10,40},{10,60}}, rotation=0)));
  replaceable Components.InverterSwitch inverter "inverter model"
    annotation (                         choices(
    choice(redeclare PowerSystems.AC3ph.Inverters.Components.InverterSwitch
            inverter "switch, no diode, no losses"),
    choice(redeclare PowerSystems.AC3ph.Inverters.Components.InverterEquation
            inverter "equation, with losses"),
    choice(redeclare PowerSystems.AC3ph.Inverters.Components.InverterModular
            inverter "modular, with losses")), Placement(transformation(extent=
              {{-10,-10},{10,10}}, rotation=0)));
  protected
  outer System system;

equation
  Connections.potentialRoot(AC.theta);
  if Connections.isRoot(AC.theta) then
    AC.theta = if system.synRef then {0, theta} else {theta, 0};
  end if;

  connect(AC, inverter.AC)     annotation (Line(points={{100,0},{10,0}}, color=
            {0,120,120}));
  connect(inverter.DC, DC)
      annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
  connect(theta, modulator.theta)   annotation (Line(points={{-60,100},{-60,70},
            {-6,70},{-6,60}}, color={0,0,127}));
  connect(uPhasor, modulator.uPhasor)   annotation (Line(points={{60,100},{60,
            70},{6,70},{6,60}}, color={0,0,127}));
  connect(modulator.gates, inverter.gates)
      annotation (Line(points={{-6,40},{-6,10}}, color={255,0,255}));
  connect(inverter.heat, heat) annotation (Line(points={{0,10},{0,20},{20,20},{
            20,80},{0,80},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="inverter",
  Documentation(
          info="<html>
<p>Four quadrant switched inverter with modulator. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>The structure of this component is related that of a voltage source, with two minor differences:</p>
<p>1) <tt>theta</tt> is used instead of <tt>omega</tt> as input.</p>
<p>2) <tt>u_phasor</tt> is used instead of <tt>v_phasor</tt> defining the AC-voltage in terms of the DC voltage <tt>v_DC</tt> according to the following relations.</p>
<p>For sine modulation:
<pre>
  |v_AC| = u*sqrt(3/2)*v_DC/2     AC voltage norm

  u[1] &le  1 for pure sine-modulation, but u[1] &gt  1 possible.
  u[1] = 1 corresponds to AC single-phase amplitude = v_DC/2
</pre>
For space-vector modulation:
<pre>
  |v_AC| = u*sqrt(2/3)*v_DC       AC voltage norm

  u[1] &le  sqrt(3)/2 = 0.866: pure sine-pwm,
  sqrt(3)/2 &le  u[1] &le  1:  overmodulation (not implemented in this preliminary version).
  u[1] = sqrt(3)/2 corresponds to AC single-phase amplitude = v_DC/sqrt(3)
</pre>
For block modulation:
<pre>
  ampl(v_abc) = v_DC/2
  w     relative width (0 - 1)
  w = 2/3 corresponds to 'two phases on, one phase off'
  the relation between AC and DC voltage is independent of width
</pre></p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,120},{-40,80}},
            lineColor={213,170,255},
            fillColor={213,170,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Inverter;

model InverterAverage "Inverter time-average, 3-phase dq0"
  extends Partials.SwitchEquation(heat(final m=1));

  replaceable parameter Semiconductors.Ideal.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  parameter Integer modulation=1 "equivalent modulation :"
    annotation(Evaluate=true, choices(
    choice=1 "1: sine PWM, equivalent: v_DC(sine) = 4/3*v_DC(SV)",
    choice=2 "2: SV PWM,   equivalent: v_DC(SV) = 3/4*v_DC(sine)",
    choice=3 "3: block M"));
  parameter Boolean syn=false "synchronous, asynchronous"
    annotation(Evaluate=true, Dialog(enable=modulation<3), choices(
    choice=true "synchronous",
    choice=false "asynchronous"));
  parameter Integer m_carr(min=1)=1 "f_carr/f, pulses/period"
    annotation(Evaluate=true, Dialog(enable=syn and modulation<3));
  parameter SI.Frequency f_carr=1e3 "carrier frequency"
    annotation(Evaluate=true, Dialog(enable=not syn and modulation<3));
  parameter Real width0=2/3 "relative width, (0 - 1)"
    annotation(Dialog(enable=modulation==3));
  Modelica.Blocks.Interfaces.RealInput theta "abs angle, der(theta)=omega"
    annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  Modelica.Blocks.Interfaces.RealInput[2] uPhasor "desired {abs(u), phase(u)}"
    annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
  outer System system;
  final parameter SI.Resistance R_nom=par.V_nom/par.I_nom;
  final parameter Real factor=
    if modulation==1 then sqrt(3/2) else
    if modulation==2 then (4/3)*sqrt(3/2) else
    if modulation==3 then (4/pi)*sin(width0*pi/2)*sqrt(3/2) else 0
      annotation(Evaluate=true);
  SI.Angle phi;
  SI.Voltage Vloss;
  Real iAC2;
  Real cT;
  Real hsw_nom;

equation
  Connections.potentialRoot(AC.theta);
  if Connections.isRoot(AC.theta) then
    AC.theta = if system.synRef then {0, theta} else {theta, 0};
  end if;

  Vloss = if par.Vf<1e-3 then 0 else tanh(10*iDC1/par.I_nom)*2*par.Vf;
  iAC2 = AC.i*AC.i;
  cT = if size(par.cT_loss,1)==0 then 1 else loss(T[1]-par.T0_loss, par.cT_loss);
  hsw_nom = if syn then (2*par.Hsw_nom*m_carr/(pi*par.V_nom*par.I_nom))*der(theta) else
                        4*par.Hsw_nom*f_carr/(par.V_nom*par.I_nom);

  phi = AC.theta[1] + uPhasor[2] + system.alpha0;
  switch_dq0 = factor*uPhasor[1]*{cos(phi), sin(phi), 0};
  v_dq0 = (vDC1 - cT*Vloss)*switch_dq0;
// passive mode?

 Q_flow = {par.eps[1]*R_nom*iAC2 +
                     (2*sqrt(6)/pi)*cT*(par.Vf + hsw_nom*abs(vDC1))*sqrt(iAC2)};
  annotation (defaultComponentName="inverter",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-70},{100,-90}},
            lineColor={176,0,0},
            textString=
                 "average"), Rectangle(
            extent={{-80,120},{-40,80}},
            lineColor={213,170,255},
            fillColor={213,170,255},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-40,-60},{40,-80}},
            lineColor={176,0,0},
            textString=
                 "time average equation"),
          Line(points={{30,-46},{30,46}}, color={0,0,255}),
          Line(points={{20,-14},{40,-14}}, color={0,0,255}),
          Line(points={{20,34},{40,34}}, color={0,0,255}),
          Line(points={{-30,0},{60,0}}, color={0,0,255}),
          Polygon(
            points={{20,14},{30,34},{40,14},{20,14}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{20,-34},{30,-14},{40,-34},{20,-34}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-30,-46},{-30,46}}, color={0,0,255}),
          Polygon(
            points={{-40,34},{-30,14},{-20,34},{-40,34}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,14},{-20,14}}, color={0,0,255}),
          Line(points={{-30,14},{-42,2}}, color={176,0,0}),
          Polygon(
            points={{-40,-14},{-30,-34},{-20,-14},{-40,-14}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-34},{-20,-34}}, color={0,0,255}),
          Line(points={{-30,-34},{-42,-46}}, color={176,0,0}),
          Line(points={{-70,10},{-60,10},{-60,46},{30,46}}, color={0,0,255}),
          Line(points={{-70,-10},{-60,-10},{-60,-46},{30,-46}}, color={0,0,255})}),
      Documentation(info="<html>
<p>Four quadrant time-averaged inverter with modulator. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>The structure of this component is related that of a voltage source, with two minor differences:</p>
<p>1) <tt>theta</tt> is used instead of <tt>omega</tt> as input.</p>
<p>2) <tt>u_phasor</tt> is used instead of <tt>v_phasor</tt> defining the AC-voltage in terms of the DC voltage <tt>v_DC</tt> according to the following relations.</p>
<p>If equivalent to sine modulation:
<pre>
  |v_AC| = u*sqrt(3/2)*v_DC/2     AC voltage norm

  u[1] &le  1 for pure sine-modulation, but u[1] &gt  1 possible.
  u[1] = 1 corresponds to AC single-phase amplitude = v_DC/2
</pre></p>
<p>If equivalent to space-vector modulation:
<pre>
  |v_AC| = u*sqrt(2/3)*v_DC       AC voltage norm

  u[1] &le  sqrt(3)/2 = 0.866: pure sine-pwm,
  sqrt(3)/2 &le  u[1] &le  1:  overmodulation (not implemented in this preliminary version).
  u[1] = sqrt(3)/2 corresponds to AC single-phase amplitude = v_DC/sqrt(3)
</pre></p>
<p>If equivalent to block (rectangular) modulation:<br>
Note that this component works with the fundamental of the rectangular voltage.
<pre>
  |v_AC| = u*(4/pi)*sin(width*pi/2)*sqrt(3/2)*v_DC/2    AC voltage norm of fundamental

  u[1] = 1 for block modulation without pwm, 0 &lt  width &lt  1
  u[1] &le  1 for block modulation with pwm.
  u[1] = 1 corresponds to AC single-phase amplitude = (4/pi)*sin(width*pi/2)*v_DC/2
</pre></p>
</html>"));
end InverterAverage;

  package Components "Equation-based and modular components"
    extends Modelica.Icons.VariantsPackage;

  model RectifierEquation "Rectifier equation, 3-phase dq0"
    extends Partials.SwitchEquation(heat(final m=3));

    replaceable parameter Semiconductors.Ideal.SCparameter par(final Hsw_nom=0)
        "SC parameters"
      annotation (Placement(transformation(extent={{-80,-80},{-60,-60}},
              rotation=0)));
    protected
    SI.Voltage[3] V;
    SI.Voltage[3] v "voltage in inertial abc representation";
    SI.Voltage[3] i_sc
        "current scaled to voltage in inertial abc representation";
    Real[3] s(each start = 0.5) "arc-length on characteristic";
    Real[3] switch "switch function in inertial abc representation";
    Real[3,3] Park = Basic.Transforms.park(        AC.theta[2]);

  equation
    i_sc = transpose(Park)*AC.i*par.V_nom/par.I_nom;

    for k in 1:3 loop
      V[k] = if size(par.cT_loss,1)==0 then  vDC1 + par.Vf else vDC1 + par.Vf*loss(T[k]-par.T0_loss, par.cT_loss);
      if s[k] > V[k] then // vDC+ < vAC
        {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V[k], s[k] - (1 - par.eps[2])*V[k]};
      elseif s[k] < -V[k] then  // vAC < vDC-
        {v[k],i_sc[k]} = {par.eps[1]*s[k] - (1 - par.eps[1])*V[k], s[k] + (1 - par.eps[2])*V[k]};
      else // vDC- < vAC < vDC+
        {v[k],i_sc[k]} = {s[k],par.eps[2]*s[k]};
      end if;
      switch[k] = noEvent(sign(s[k]));
    end for;

    v_dq0 = Park*v;
    switch_dq0 = Park*switch;
    Q_flow = (v - switch*vDC1).*i_sc*par.I_nom/par.V_nom;
    annotation (defaultComponentName="rectifier",
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-60,-70},{60,-90}},
              lineColor={176,0,0},
              textString=
                 "eq")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Polygon(
              points={{-10,-38},{0,-18},{10,-38},{-10,-38}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-10,18},{0,38},{10,18},{-10,18}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{0,-18},{0,18}}, color={0,0,255}),
            Line(points={{-10,-18},{10,-18}}, color={0,0,255}),
            Line(points={{-10,38},{10,38}}, color={0,0,255}),
            Line(points={{0,0},{60,0}}, color={0,0,255}),
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                   "time resolved equation"),
            Line(points={{-70,10},{-60,10},{-60,52},{0,52},{0,40}}, color={0,0,
                  255}),
            Line(points={{-70,-10},{-60,-10},{-60,-50},{0,-50},{0,-38}}, color=
                  {0,0,255})}),
        Documentation(info="<html>
<p>Passive rectifier, based on switch-equation.<br>
Blocking losses are neglected in the expression of dissipated heat <tt>Q_flow</tt>.</p>
</html>"));
  end RectifierEquation;

  model RectifierModular "Rectifier modular, 3-phase"
    extends Partials.AC_DC_base(heat(final m=3));

    package SCpackage=Semiconductors.Ideal "SC package";
    replaceable parameter SCpackage.SCparameter par(final Hsw_nom=0)
        "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
               0)));
    Nodes.ACdq0_a_b_c acdq0_a_b_c annotation (Placement(transformation(extent={
                {80,-10},{60,10}}, rotation=0)));
    Common.Thermal.Heat_a_b_c_abc heat_adapt annotation (Placement(
            transformation(extent={{-10,70},{10,90}}, rotation=0)));
    Semiconductors.PhaseModules.DiodeModule diodeMod_a(final par=par)
        "diode module AC_a"
        annotation (Placement(transformation(extent={{-10,30},{10,50}},
              rotation=0)));
    Semiconductors.PhaseModules.DiodeModule diodeMod_b(final par=par)
        "diode module AC_b"
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
              rotation=0)));
    Semiconductors.PhaseModules.DiodeModule diodeMod_c(final par=par)
        "diode module AC_c"
        annotation (Placement(transformation(extent={{-10,-50},{10,-30}},
              rotation=0)));

  equation
    connect(AC, acdq0_a_b_c.term) annotation (Line(points={{100,0},{80,0}},
            color={0,120,120}));
    connect(acdq0_a_b_c.term_a, diodeMod_a.AC) annotation (Line(points={{60,4},
              {40,4},{40,40},{10,40}}, color={0,0,255}));
    connect(acdq0_a_b_c.term_b, diodeMod_b.AC)
        annotation (Line(points={{60,0},{10,0}}, color={0,0,255}));
    connect(acdq0_a_b_c.term_c, diodeMod_c.AC) annotation (Line(points={{60,-4},
              {40,-4},{40,-40},{10,-40}}, color={0,0,255}));
    connect(diodeMod_a.DC, DC) annotation (Line(points={{-10,40},{-40,40},{-40,
              0},{-100,0}}, color={0,0,255}));
    connect(diodeMod_b.DC, DC)
        annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
    connect(diodeMod_c.DC, DC) annotation (Line(points={{-10,-40},{-40,-40},{
              -40,0},{-100,0}}, color={0,0,255}));
    connect(diodeMod_a.heat, heat_adapt.port_a)   annotation (Line(points={{0,
              50},{0,60},{-4,60},{-4,74}}, color={176,0,0}));
    connect(diodeMod_b.heat, heat_adapt.port_b)   annotation (Line(points={{0,
              10},{0,20},{20,20},{20,64},{0,64},{0,74}}, color={176,0,0}));
    connect(diodeMod_c.heat, heat_adapt.port_c)   annotation (Line(points={{0,
              -30},{0,-20},{30,-20},{30,74},{4,74}}, color={176,0,0}));
    connect(heat_adapt.port_abc, heat)
        annotation (Line(points={{0,86},{0,100}}, color={176,0,0}));
  annotation (defaultComponentName="rectifier",
    Documentation(
            info="<html>
<p>Passive rectifier, using diode-modules.</p>
</html>
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-70},{100,-90}},
              lineColor={176,0,0},
              textString=
                 "modular")}),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
  end RectifierModular;

  model InverterSwitch "Inverter switch, 3-phase dq0"
    extends Partials.SwitchEquation(heat(final m=3));

    Modelica.Blocks.Interfaces.BooleanInput[6] gates
        "gates pairs {a_p, a_n, b_p, b_n, c_p, c_n}"
    annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
    constant Integer[3] pgt={1,3,5} "positive gates";
    constant Integer[3] ngt={2,4,6} "negative gates";
    SI.Voltage[3] v "voltage in inertial abc representation";
    Real[3] switch "switch function in inertial abc representation";
    Real[3,3] Park = Basic.Transforms.park(        AC.theta[2]);

  equation
    for k in 1:3 loop
      if gates[pgt[k]] then // switched mode DC+ to AC
        switch[k] = 1;
        v[k] = vDC1;
      elseif gates[ngt[k]] then // switched mode DC- to AC
        switch[k] = -1;
        v[k] = -vDC1;
      else
        switch[k] = 0;
        v[k] = 0;
      end if;
    end for;

    v_dq0 = Park*v;
    switch_dq0 = Park*switch;
    Q_flow = zeros(heat.m);
    annotation (defaultComponentName="inverter",
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-60,-70},{60,-90}},
              lineColor={176,0,0},
              textString=
                   "switch")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                   "switch, no diode"),
            Line(points={{0,0},{60,0}}, color={0,0,255}),
            Line(points={{0,-46},{0,46}}, color={0,0,255}),
            Polygon(
              points={{-10,34},{0,14},{10,34},{-10,34}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-10,14},{10,14}}, color={0,0,255}),
            Line(points={{0,14},{-12,2}}, color={176,0,0}),
            Polygon(
              points={{-10,-14},{0,-34},{10,-14},{-10,-14}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-10,-34},{10,-34}}, color={0,0,255}),
            Line(points={{0,-34},{-12,-46}}, color={176,0,0}),
            Line(points={{-70,10},{-60,10},{-60,46},{0,46}}, color={0,0,255}),
            Line(points={{-70,-10},{-60,-10},{-60,-46},{0,-46}}, color={0,0,255})}),
        Documentation(info="<html>
<p>Four quadrant switched inverter, based on switch without antiparallel diode (no passive mode). Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
<p>Contains no forward drop voltage Vf. Heat losses are set to zero.</p>
</html>"));
  end InverterSwitch;

  model InverterEquation "Inverter equation, 3-phase dq0"
    extends Partials.SwitchEquation(heat(final m=3));

    replaceable parameter Semiconductors.Ideal.SCparameter par "SC parameters"
      annotation (Placement(transformation(extent={{-80,-80},{-60,-60}},
              rotation=0)));
    Modelica.Blocks.Interfaces.BooleanInput[6] gates
        "gates pairs {a_p, a_n, b_p, b_n, c_p, c_n}"
    annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
    constant Integer[3] pgt={1,3,5} "positive gates";
    constant Integer[3] ngt={2,4,6} "negative gates";
    SI.Voltage[3] V_s;
    SI.Voltage[3] V_d;
    SI.Voltage[3] v "voltage in inertial abc representation";
    SI.Voltage[3] i_sc
        "current scaled to voltage in inertial abc representation";
    Real[3] s(each start = 0.5) "arc-length on characteristic";
    Real[3] switch "switch function in inertial abc representation";
    Real[3,3] Park = Basic.Transforms.park(        AC.theta[2]);

  equation
    i_sc = transpose(Park)*AC.i*par.V_nom/par.I_nom;

    if par.Vf<1e-3 then // faster code if forward voltage drop Vf not used (Vf=0).
      for k in 1:3 loop
        V_s[k] = 0;
        V_d[k] = vDC1;
        if gates[pgt[k]] then // switched mode DC+ to AC
          switch[k] = 1;
          {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V_d[k],s[k] - (1 - par.eps[2])*V_d[k]};
        elseif gates[ngt[k]] then // switched mode DC- to AC
          switch[k] = -1;
          {v[k],i_sc[k]} = {par.eps[1]*s[k] - (1 - par.eps[1])*V_d[k],s[k] + (1 - par.eps[2])*V_d[k]};
        else // rectifier mode
         if s[k] > V_d[k] then // vDC+ < vAC
            {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V_d[k], s[k] - (1 - par.eps[2])*V_d[k]};
          elseif s[k] < -V_d[k] then  // vAC < vDC-
            {v[k],i_sc[k]} = {par.eps[1]*s[k] - (1 - par.eps[1])*V_d[k], s[k] + (1 - par.eps[2])*V_d[k]};
          else // vDC- < vAC < vDC+
            {v[k],i_sc[k]} = {s[k],par.eps[2]*s[k]};
          end if;
          switch[k] = noEvent(sign(s[k]));
        end if;
      end for;
      Q_flow = zeros(heat.m);
    else // slower code if voltage drop used (Vf_s>0 or Vf_d>0).
      for k in 1:3 loop
        {V_s[k], V_d[k]} = if size(par.cT_loss,1)==0 then {vDC1-par.Vf, vDC1+par.Vf} else {vDC1, vDC1} + {-par.Vf, par.Vf}*loss(T[k]-par.T0_loss, par.cT_loss);
        if gates[pgt[k]] then // switched mode DC+ to AC
          switch[k] = 1;
          if s[k] > V_d[k] then
            {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V_d[k],s[k] - (1 - par.eps[2])*V_d[k]};
          elseif s[k] < V_s[k] then
            {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V_s[k],s[k] - (1 - par.eps[2])*V_s[k]};
          else
            {v[k],i_sc[k]} = {s[k],par.eps[2]*s[k]};
          end if;
        elseif gates[ngt[k]] then // switched mode DC- to AC
          switch[k] = -1;
          if s[k] < -V_d[k] then
            {v[k],i_sc[k]} = {par.eps[1]*s[k] - (1 - par.eps[1])*V_d[k],s[k] + (1 - par.eps[2])*V_d[k]};
          elseif s[k] > -V_s[k] then
            {v[k],i_sc[k]} = {par.eps[1]*s[k] -(1 - par.eps[1])*V_s[k],s[k] + (1 - par.eps[2])*V_s[k]};
          else
            {v[k],i_sc[k]} = {s[k],par.eps[2]*s[k]};
          end if;
        else // rectifier mode
          if s[k] > V_d[k] then // vDC+ < vAC
            {v[k],i_sc[k]} = {par.eps[1]*s[k] + (1 - par.eps[1])*V_d[k], s[k] - (1 - par.eps[2])*V_d[k]};
          elseif s[k] < -V_d[k] then  // vAC < vDC-
            {v[k],i_sc[k]} = {par.eps[1]*s[k] - (1 - par.eps[1])*V_d[k], s[k] + (1 - par.eps[2])*V_d[k]};
          else // vDC- < vAC < vDC+
            {v[k],i_sc[k]} = {s[k],par.eps[2]*s[k]};
          end if;
          switch[k] = noEvent(sign(s[k]));
        end if;
      end for;
      Q_flow = (v - switch*vDC1).*i_sc*par.I_nom/par.V_nom;
    end if;

    v_dq0 = Park*v;
    switch_dq0 = Park*switch;
    annotation (defaultComponentName="inverter",
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-60,-70},{60,-90}},
              lineColor={176,0,0},
              textString=
                 "eq")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{30,-46},{30,46}}, color={0,0,255}),
            Line(points={{20,-14},{40,-14}}, color={0,0,255}),
            Line(points={{20,34},{40,34}}, color={0,0,255}),
            Line(points={{-30,0},{60,0}}, color={0,0,255}),
            Polygon(
              points={{20,14},{30,34},{40,14},{20,14}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{20,-34},{30,-14},{40,-34},{20,-34}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-30,-46},{-30,46}}, color={0,0,255}),
            Polygon(
              points={{-40,34},{-30,14},{-20,34},{-40,34}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-40,14},{-20,14}}, color={0,0,255}),
            Line(points={{-30,14},{-42,2}}, color={176,0,0}),
            Polygon(
              points={{-40,-14},{-30,-34},{-20,-14},{-40,-14}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-40,-34},{-20,-34}}, color={0,0,255}),
            Line(points={{-30,-34},{-42,-46}}, color={176,0,0}),
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                   "time resolved equation"),
            Line(points={{-70,10},{-60,10},{-60,46},{30,46}}, color={0,0,255}),
            Line(points={{-70,-10},{-60,-10},{-60,-46},{30,-46}}, color={0,0,
                  255})}),
        Documentation(info="<html>
<p>Four quadrant switched inverter, based on switch equation. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
<p>The Boolean parameter Vf_zero chooses faster code if both Vf_s and Vf_d are zero.<br>
Blocking losses are neglected in the expression of dissipated heat <tt>Q_flow</tt>.</p>
</html>"));
  end InverterEquation;

  model InverterModular "Inverter modular, 3-phase"
    extends Partials.AC_DC_base(heat(final m=3));

    package SCpackage=Semiconductors.Ideal "SC package";
    replaceable parameter SCpackage.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
               0)));
    Modelica.Blocks.Interfaces.BooleanInput[6] gates
        "gates pairs {a_p, a_n, b_p, b_n, c_p, c_n}"
    annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    Nodes.ACdq0_a_b_c acdq0_a_b_c annotation (Placement(transformation(extent={
                {80,-10},{60,10}}, rotation=0)));
    Common.Thermal.Heat_a_b_c_abc heat_adapt annotation (Placement(
            transformation(extent={{-10,70},{10,90}}, rotation=0)));
    Blocks.Multiplex.Gate3demux gate3demux1(final n=2)
      annotation (Placement(transformation(extent={{-50,60},{-30,80}}, rotation=
               0)));
    Semiconductors.PhaseModules.SwitchModule switchMod_a(final par=par)
        "switch + reverse diode module AC_a"
        annotation (Placement(transformation(extent={{-10,30},{10,50}},
              rotation=0)));
    Semiconductors.PhaseModules.SwitchModule switchMod_b(final par=par)
        "switch + reverse diode module AC_b"
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
              rotation=0)));
    Semiconductors.PhaseModules.SwitchModule switchMod_c(final par=par)
        "switch + reverse diode module AC_c"
        annotation (Placement(transformation(extent={{-10,-50},{10,-30}},
              rotation=0)));

  equation
    connect(AC, acdq0_a_b_c.term)   annotation (Line(points={{100,0},{80,0}},
            color={0,120,120}));
    connect(acdq0_a_b_c.term_a, switchMod_a.AC)   annotation (Line(points={{60,
              4},{40,4},{40,40},{10,40}}, color={0,0,255}));
    connect(switchMod_a.DC, DC)   annotation (Line(points={{-10,40},{-60,40},{
              -60,0},{-100,0}}, color={0,0,255}));
    connect(acdq0_a_b_c.term_b, switchMod_b.AC)
        annotation (Line(points={{60,0},{10,0}}, color={0,0,255}));
    connect(switchMod_b.DC, DC)
        annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
    connect(acdq0_a_b_c.term_c, switchMod_c.AC)   annotation (Line(points={{60,
              -4},{40,-4},{40,-40},{10,-40}}, color={0,0,255}));
    connect(switchMod_c.DC, DC)   annotation (Line(points={{-10,-40},{-60,-40},
              {-60,0},{-100,0}}, color={0,0,255}));
    connect(gates, gate3demux1.gates)   annotation (Line(points={{-60,100},{-60,
              80},{-40,80}}, color={255,0,255}));
    connect(gate3demux1.gates_a, switchMod_a.gates)   annotation (Line(points={
              {-46,60},{-46,54},{-6,54},{-6,50}}, color={255,0,255}));
    connect(gate3demux1.gates_b, switchMod_b.gates)   annotation (Line(points={
              {-40,60},{-40,20},{-6,20},{-6,10}}, color={255,0,255}));
    connect(gate3demux1.gates_c, switchMod_c.gates)   annotation (Line(points={
              {-34,60},{-34,-20},{-6,-20},{-6,-30}}, color={255,0,255}));
    connect(switchMod_a.heat, heat_adapt.port_a)   annotation (Line(points={{0,
              50},{0,60},{-4,60},{-4,74}}, color={176,0,0}));
    connect(switchMod_b.heat, heat_adapt.port_b)   annotation (Line(points={{0,
              10},{0,20},{20,20},{20,64},{0,64},{0,74}}, color={176,0,0}));
    connect(switchMod_c.heat, heat_adapt.port_c)   annotation (Line(points={{0,
              -30},{0,-20},{30,-20},{30,74},{4,74}}, color={176,0,0}));
    connect(heat_adapt.port_abc, heat)
        annotation (Line(points={{0,86},{0,100}}, color={176,0,0}));
  annotation (defaultComponentName="inverter",
    Documentation(
            info="<html>
<p>Four quadrant switched inverter,  using switch-modules. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
</html>
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-70},{100,-90}},
              lineColor={176,0,0},
              textString=
                 "modular")}),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
  end InverterModular;

    annotation (preferredView="info",
  Documentation(info="<html>
<p>Contains alternative components:
<ul>
<li>Equation-based: faster code, restricted to ideal V-I characteristic, but including forward threshold voltage, needed for calculation of thermal losses.</li>
<li>Modular: composed from semiconductor-switches and diodes. These components with ideal V-I characteristic can be replaced by custom-specified semiconductor models.</li>
</ul>
</html>"));
  end Components;

package Partials "Partial models"
  extends Modelica.Icons.BasesPackage;

partial model AC_DC_base "AC-DC base, 3-phase dq0"
  extends PowerSystems.Basic.Icons.Inverter_dq0;
  extends Ports.PortBase;

  Ports.ACdq0_n AC "AC 3-phase connection"
    annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));
  AC1ph_DC.Ports.TwoPin_p DC "DC connection"
    annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=
               0)));
  Interfaces.ThermalV_n heat(     m=3) "vector heat port"
    annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));

      annotation (Diagram(graphics),
                           Documentation(info="<html>
</html>"),
        Icon(graphics));

end AC_DC_base;

partial model SwitchEquation "Switch equation, 3-phase dq0"
  extends AC_DC_base;

    protected
  SI.Voltage vDC1=0.5*(DC.v[1] - DC.v[2]);
  SI.Voltage vDC0=0.5*(DC.v[1] + DC.v[2]);
  SI.Current iDC1=(DC.i[1] - DC.i[2]);
  SI.Current iDC0=(DC.i[1] + DC.i[2]);
  Real[3] v_dq0 "switching function voltage in dq0 representation";
  Real[3] switch_dq0 "switching function in dq0 representation";

  SI.Temperature[heat.m] T "component temperature";
  SI.HeatFlowRate[heat.m] Q_flow "component loss-heat flow";
  function loss = Basic.Math.taylor "temp dependence of losses";

equation
  AC.v = v_dq0 + {0,0,sqrt(3)*vDC0};
  iDC1 + switch_dq0*AC.i = 0;
  iDC0 + sqrt(3)*AC.i[3] = 0;

  T = heat.ports.T;
  heat.ports.Q_flow = -Q_flow;
  annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{68,18},{72,22}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{68,-2},{72,2}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{68,-22},{72,-18}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{76,24},{84,16}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString=
               "a"),
            Text(
              extent={{76,4},{84,-4}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString=
               "b"),
            Text(
              extent={{76,-16},{84,-24}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString=
               "c"),
            Ellipse(
              extent={{-72,12},{-68,8}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-72,-8},{-68,-12}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-86,16},{-74,4}},
              lineColor={0,0,255},
              textString=
                 "+"),
            Text(
              extent={{-86,-4},{-74,-16}},
              lineColor={0,0,255},
              textString=
                 "-")}),
      Documentation(info="<html>
</html>"));
end SwitchEquation;

  annotation (
      Documentation(info="<html>
</html>"));
end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>The package contains passive rectifiers and switched/modulated inverters. Different implementations use:
<ul>
<li>Phase-modules (pairs of diodes or pairs of IGBT's with antiparallel diodes).</li>
<li>The switch-equation for ideal components.</li>
<li>The time-averaged switch-equation. As models based on single-switching are generally slow in simulation, alternative 'averaged' models are useful in cases, where details of current and voltage signals can be ignored.</li>
</ul>
<p>Thermal losses are proportional to the forward voltage drop V, which may depend on temperature.<br>
The temperature dependence is given by
<pre>  V(T) = Vf*(1 + cT[1]*(T - T0) + cT[2]*(T - T0)^2 + ...)</pre>
where <tt>Vf</tt> denotes the parameter value. With input <tt>cT</tt> empty, no temperature dependence of losses is calculated.</p>
<p>The switching losses are approximated by
<pre>
  h = Hsw_nom*v*i/(V_nom*I_nom)
  use:
  S_nom = V_nom*I_nom
</pre>
where <tt>Hsw_nom</tt> denotes the dissipated heat per switching operation at nominal voltage and current, averaged over 'on' and 'off'. The same temperature dependence is assumed as for Vf. A generalisation to powers of i and v is straightforward.</p>
<p>NOTE: actually the switching losses are only implemented for time-averaged components!</p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Inverters;
