within PowerSystems.AC1ph_DC;
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
      "angular frequency"
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
  Window(
        x=0,
        y=0.01,
        width=0.44,
        height=0.65),
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
            textString="%name"), Rectangle(
            extent={{-80,-80},{-40,-120}},
            lineColor={213,170,255},
            fillColor={213,170,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Select;

model Rectifier "Rectifier, 1-phase"
  extends Partials.AC_DC_base(heat(final m=2));

  replaceable Components.RectifierEquation rectifier "rectifier model"
    annotation (                         choices(
    choice(redeclare
            PowerSystems.AC1ph_DC.Inverters.Components.RectifierEquation
            rectifier "equation, with losses"),
    choice(redeclare
            PowerSystems.AC1ph_DC.Inverters.Components.RectifierModular
            rectifier "modular, with losses")), Placement(transformation(extent=
             {{-10,-10},{10,10}}, rotation=0)));

equation
  connect(AC, rectifier.AC)
      annotation (Line(points={{100,0},{10,0}}, color={0,0,255}));
  connect(rectifier.DC, DC)
      annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
  connect(rectifier.heat, heat)
      annotation (Line(points={{0,10},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="rectifier",
  Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
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

model Inverter "Complete modulator and inverter, 1-phase"
  extends Partials.AC_DC_base(heat(final m=2));

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
  replaceable Control.Modulation.PWMasyn1ph modulator
    constrainedby Control.Modulation.Partials.ModulatorBase "modulator type"
    annotation (                        choices(
    choice(redeclare Control.Modulation.PWMasyn1ph modulator "sine PWM asyn"),
    choice(redeclare Control.Modulation.PWMsyn1ph modulator "sine PWM syn"),
    choice(redeclare Control.Modulation.PWMtab1ph modulator
            "sine PWM syn tabulated"),
    choice(redeclare Control.Modulation.BlockM1ph modulator
            "block modulation (no PWM)")), Placement(transformation(extent={{
              -10,40},{10,60}}, rotation=0)));

  replaceable Components.InverterSwitch inverter "inverter model"
    annotation (                         choices(
    choice(redeclare PowerSystems.AC1ph_DC.Inverters.Components.InverterSwitch
            inverter "switch, no diode, no losses"),
    choice(redeclare
            PowerSystems.AC1ph_DC.Inverters.Components.InverterEquation
            inverter "equation, with losses"),
    choice(redeclare PowerSystems.AC1ph_DC.Inverters.Components.InverterModular
            inverter "modular, with losses")), Placement(transformation(extent=
              {{-10,-10},{10,10}}, rotation=0)));
  protected
  outer System system;

equation
  connect(theta, modulator.theta) annotation (Line(points={{-60,100},{-60,70},{
            -6,70},{-6,60}}, color={0,0,127}));
  connect(uPhasor, modulator.uPhasor) annotation (Line(points={{60,100},{60,70},
            {6,70},{6,60}}, color={0,0,127}));
  connect(AC, inverter.AC) annotation (Line(points={{100,0},{10,0}}, color={0,0,
            255}));
  connect(inverter.DC, DC)
    annotation (Line(points={{-10,0},{-100,0}}, color={0,0,255}));
  connect(modulator.gates, inverter.gates)
    annotation (Line(points={{-6,40},{-6,10}}, color={255,0,255}));
  connect(inverter.heat, heat) annotation (Line(points={{0,10},{0,20},{20,20},{
            20,80},{0,80},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="inverter",
  Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
  Documentation(
          info="<html>
<p>Four quadrant switched inverter with modulator. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>The structure of this component is related that of a voltage source, with two minor differences:</p>
<p>1) <tt>theta</tt> is used instead of <tt>omega</tt> as input.</p>
<p>2) <tt>u_phasor</tt> is used instead of <tt>v_phasor</tt> defining the AC-voltage in terms of the DC voltage <tt>v_DC</tt> according to the following relations.</p>
<p>For sine modulation:
<pre>
  v_AC_eff = u*v_DC/sqrt(2)     AC effective voltage

  u[1] &le  1 for pure sine-modulation, but u[1] &gt  1 possible.
  u[1] = 1 corresponds to:  AC amplitude = v_DC
</pre>
For block modulation:
<pre>
  ampl(v_AC) = v_DC
  w     relative width (0 - 1)
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

model InverterAverage "Inverter time-average, 1-phase"
  extends Partials.SwitchEquation(heat(final m=1));

  replaceable parameter Semiconductors.Ideal.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  parameter Integer modulation=1 "equivalent modulation :"
    annotation(Evaluate=true, choices(
    choice=1 "1: sine PWM",
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
    if modulation==1 then 1 else
    if modulation==3 then (4/pi)*sin(width0*pi/2) else 0
                                                     annotation(Evaluate=true);
  SI.Angle phi;
  SI.Voltage Vloss;
  Real cT;
  Real hsw_nom;

equation
  Vloss = if par.Vf<1e-3 then 0 else tanh(10*iDC1/par.I_nom)*2*par.Vf;
  cT = if size(par.cT_loss,1)>0 then loss(T[1]-par.T0_loss, par.cT_loss) else 1;
  hsw_nom = if syn then (2*par.Hsw_nom*m_carr/(pi*par.V_nom*par.I_nom))*der(theta) else
                 4*par.Hsw_nom*f_carr/(par.V_nom*par.I_nom);

  phi = theta + uPhasor[2] + system.alpha0;
  switch[1] = factor*uPhasor[1]*cos(phi);
  switch[2] = -switch[1];
  v = (vDC1 - cT*Vloss)*switch;
// passive mode?

  Q_flow = {par.eps[1]*R_nom*AC.i*AC.i+
                       cT*(par.Vf + hsw_nom*abs(vDC1))*(abs(AC.i[1])+abs(AC.i[2]))};
  annotation (defaultComponentName="inverter",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-70},{100,-90}},
            lineColor={176,0,0},
            textString="average"), Rectangle(
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
            textString="time average equation"),
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
<p>Four quadrant time-averaged inverter. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>The structure of this component is related that of a voltage source, with two minor differences:</p>
<p>1) <tt>theta</tt> is used instead of <tt>omega</tt> as input.</p>
<p>2) <tt>u_phasor</tt> is used instead of <tt>v_phasor</tt> defining the AC-voltage in terms of the DC voltage <tt>v_DC</tt> according to the following relations.</p>
<p>If equivalent to sine modulation:
<pre>
  v_AC_eff = u*v_DC/sqrt(2)     AC effective voltage

  u[1] &le  1 for pure sine-modulation, but u[1] &gt  1 possible.
  u[1] = 1 corresponds to:  AC amplitude = v_DC
</pre></p>
<p>If equivalent to block (rectangular) modulation:<br>
Note that this component works with the fundamental of the rectangular voltage.<br>
The method must be improved in this case (in particular in context with inductive devices).
<pre>
  v_AC_eff = u*(4/pi)*sin(width*pi/2)*v_DC/sqrt(2)    AC eff voltage of fundamental

  u[1] = 1 for block modulation without pwm, 0 &lt  width &lt  1
  u[1] &le  1 for block modulation with pwm.
  u[1] = 1 corresponds to AC amplitude = (4/pi)*sin(width*pi/2)*v_DC
</pre></p>
</html>
"));
end InverterAverage;

model Chopper "DC-DC converter"
  extends Partials.DC_DC_base(heat(final m=2));

  Modelica.Blocks.Interfaces.RealInput uDC "desired uDC"
                                                   annotation (Placement(
          transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  replaceable Control.Modulation.ChopperPWM modulator
    constrainedby PowerSystems.Basic.Icons.BlockS "modulator type"
    annotation (Placement(transformation(extent={{-10,40},{10,60}}, rotation=0)));
  replaceable Components.ChopperModular chopper "chopper model"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}}, rotation=0)));

equation
  connect(uDC, modulator.uDC)   annotation (Line(points={{60,100},{60,70},{6,70},
            {6,60}}, color={0,0,127}));
  connect(DCin, chopper.DCin)
      annotation (Line(points={{-100,0},{-10,0}}, color={0,0,255}));
  connect(chopper.DCout, DCout)
      annotation (Line(points={{10,0},{100,0}}, color={0,0,255}));
  connect(modulator.gate, chopper.gate)
      annotation (Line(points={{-6,40},{-6,10}}, color={255,0,255}));
  connect(chopper.heat, heat)   annotation (Line(points={{0,10},{0,20},{20,20},
            {20,80},{0,80},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="chopper",
  Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
  Documentation(
          info="<html>
<p>One quadrant switched converter. Fulfills the power balance:
<pre>  vDCin*iDCin = vDCout*iDCout</pre></p>
<p><tt>u_DC</tt> determines the desired DC-out voltage <tt>v_DCout</tt> in terms of the DC-in voltage <tt>v_DCin</tt> according to the following relation:
<pre>
  v_DCout = u_DC*v_DCin
  u_DC &le  1
</pre></p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Chopper;

model ChopperAverage "DC-DC converter time-average"
  extends Partials.DC_DC_base(heat(final m=1));

  replaceable parameter Semiconductors.Ideal.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
             0)));
  parameter SI.Frequency f_carr=1e3 "carrier frequency"
    annotation(Evaluate=true);
  Modelica.Blocks.Interfaces.RealInput uDC "desired uDC"
   annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
  SI.Voltage vDCin=DCin.v[1] - DCin.v[2];
  SI.Voltage vDCout=DCout.v[1] - DCout.v[2];
  final parameter SI.Resistance R_nom=par.V_nom/par.I_nom;
  Real hsw_nom;
  Real cT;

  SI.Temperature[heat.m] T = heat.ports.T "component temperature";
  SI.HeatFlowRate[heat.m] Q_flow = -heat.ports.Q_flow
      "component loss-heat flow";
  function lossT = Basic.Math.taylor "spec loss function of temperature";

equation
  DCin.v[2] = DCout.v[2];
  DCin.i + DCout.i = {0, 0};
  vDCout = uDC*max(vDCin - cT*par.Vf, 0);

  hsw_nom = 4*par.Hsw_nom*f_carr/(par.V_nom*par.I_nom);
  cT = if size(par.cT_loss,1)==0 then 1 else lossT(T[1]-par.T0_loss, par.cT_loss);
  Q_flow = {par.eps[1]*R_nom*DCin.i[1]*DCin.i[1] +
                     cT*(par.Vf + hsw_nom*abs(vDCin))*abs(DCin.i[1])};
  annotation (
    defaultComponentName="chopper",
Window(
  x=0.45,
      y=0.01,
      width=
0.44,
  height=
 0.65),
Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-70},{100,-90}},
            lineColor={176,0,0},
            textString="average")}),
Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-40,-60},{40,-80}},
            lineColor={176,0,0},
            textString=
                 "time average equation"),
          Line(points={{-20,30},{-20,50}}, color={0,0,255}),
          Line(points={{0,0},{0,20}}, color={0,0,255}),
          Polygon(
            points={{-20,20},{-20,0},{0,10},{-20,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-20,40},{0,50},{0,30},{-20,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-70,-10},{-50,-10},{-50,-46},{50,-46},{50,-10},{70,-10}},
              color={0,0,255}),
          Line(points={{-70,10},{-50,10},{-50,27},{-40,27}}, color={0,0,255}),
          Line(points={{20,27},{50,27},{50,10},{70,10}}, color={0,0,255}),
          Line(points={{30,27},{30,-46}}, color={0,0,255}),
          Polygon(
            points={{20,-30},{30,-10},{40,-30},{20,-30}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{20,-10},{40,-10}}, color={0,0,255}),
          Line(points={{-20,40},{-40,40},{-40,10},{-20,10}}, color={0,0,255}),
          Line(points={{0,40},{20,40},{20,10},{0,10}}, color={0,0,255}),
          Line(points={{0,10},{12,-4}}, color={176,0,0}),
          Ellipse(
            extent={{-72,8},{-68,12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-72,-12},{-68,-8}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{68,12},{72,8}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{68,-8},{72,-12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{74,16},{86,4}},
            lineColor={0,0,255},
            textString=
                 "+"),
          Text(
            extent={{74,-4},{86,-16}},
            lineColor={0,0,255},
            textString=
                 "-"),
          Ellipse(
            extent={{-72,8},{-68,12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-72,-12},{-68,-8}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{68,12},{72,8}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{68,-8},{72,-12}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{74,16},{86,4}},
            lineColor={0,0,255},
            textString=
                 "+"),
          Text(
            extent={{74,-4},{86,-16}},
            lineColor={0,0,255},
            textString=
                 "-"),
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
<p>One quadrant time averaged converter. Fulfills the power balance:
<pre>  vDCin*iDCin = vDCout*iDCout</pre></p>
<p><tt>u_DC</tt> determines the desired DC-out voltage <tt>v_DCout</tt> in terms of the DC-in voltage <tt>v_DCin</tt> according to the following relation:
<pre>
  v_DCout = u_DC*v_DCin
  u_DC &le  1
</pre></p>
</html>"));
end ChopperAverage;

package Components "Equation-based and modular components"
  extends Modelica.Icons.VariantsPackage;

model RectifierEquation "Rectifier equation, 1-phase"
  extends Partials.SwitchEquation(heat(final m=2));

  parameter Semiconductors.Ideal.SCparameter par(final Hsw_nom=0)
        "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
               0)));
    protected
  SI.Voltage[2] V;
  SI.Voltage[2] i_sc "current scaled to voltage in inertial abc representation";
  Real[2] s "arc-length on characteristic";

equation
  i_sc = AC.i*par.V_nom/par.I_nom;

  for k in 1:2 loop
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

  Q_flow = (v - switch*vDC1).*i_sc*par.I_nom/par.V_nom;
  annotation (defaultComponentName="rectifier",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
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
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                 "time resolved equation"),
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
            Line(points={{-70,-10},{-60,-10},{-60,-50},{0,-50},{0,-38}}, color=
                  {0,0,255})}),
      Documentation(info="<html>
<p>Passive rectifier, based on switch-equation.<br>
Blocking losses are neglected in the expression of dissipated heat <tt>Q_flow</tt>.</p>
</html>"));
end RectifierEquation;

model RectifierModular "Rectifier modular, 1-phase"
  extends Partials.AC_DC_base(heat(final m=2));

  package SCpackage=Semiconductors.Ideal "SC package";
  replaceable parameter SCpackage.SCparameter par(final Hsw_nom=0)
        "SC parameters"
  annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=0)));
  AC1ph_DC.Nodes.Electric_pn_p_n pn_p_n
                               annotation (Placement(transformation(extent={{80,
                -10},{60,10}}, rotation=0)));
  Common.Thermal.Heat_a_b_ab heat_adapt annotation (Placement(transformation(
              extent={{-10,60},{10,80}}, rotation=0)));
  Semiconductors.PhaseModules.DiodeModule diodeMod_a1(par=par)
        "diode module AC_a1"
      annotation (Placement(transformation(extent={{-10,20},{10,40}}, rotation=
                0)));
  Semiconductors.PhaseModules.DiodeModule diodeMod_a2(par=par)
        "diode module AC_a2"
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}},
              rotation=0)));

equation
  connect(AC, pn_p_n.term_pn)
      annotation (Line(points={{100,0},{76,0}}, color={0,0,255}));
  connect(pn_p_n.term_p, diodeMod_a1.AC)   annotation (Line(points={{64,4},{40,
              4},{40,30},{10,30}}, color={0,0,255}));
  connect(diodeMod_a1.DC, DC)   annotation (Line(points={{-10,30},{-40,30},{-40,
              0},{-100,0}}, color={0,0,255}));
  connect(pn_p_n.term_n, diodeMod_a2.AC)   annotation (Line(points={{64,-4},{40,
              -4},{40,-30},{10,-30}}, color={0,0,255}));
  connect(diodeMod_a2.DC, DC)   annotation (Line(points={{-10,-30},{-40,-30},{
              -40,0},{-100,0}}, color={0,0,255}));
  connect(diodeMod_a1.heat, heat_adapt.port_a) annotation (Line(points={{0,40},
              {0,54},{-4,54},{-4,64}}, color={176,0,0}));
  connect(diodeMod_a2.heat, heat_adapt.port_b) annotation (Line(points={{0,-20},
              {0,0},{20,0},{20,54},{4,54},{4,64}}, color={176,0,0}));
  connect(heat_adapt.port_ab, heat)
        annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="rectifier",
  Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
  Documentation(
          info="<html>
<p>Passive rectifier, using diode-modules.</p>
</html>
"),
  Icon(coordinateSystem(
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

model InverterSwitch "Inverter equation, 1-phase"
  extends Partials.SwitchEquation(heat(final m=2));

  Modelica.Blocks.Interfaces.BooleanInput[4] gates
        "gates pairs {a1_p, a1_n, a2_p, a2_n}"
  annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
  constant Integer[2] pgt={1,3} "positive gates";
  constant Integer[2] ngt={2,4} "negative gates";

equation
  for k in 1:2 loop
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

  Q_flow = zeros(heat.m);
  annotation (defaultComponentName="inverter",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
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
            Line(points={{-70,-10},{-60,-10},{-60,-46},{0,-46}}, color={0,0,255}),
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                   "switch, no diode")}),
      Documentation(info="<html>
<p>Four quadrant switched inverter, based on switch without antiparallel diode (no passive mode). Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
<p>Contains no forward drop voltage Vf. Heat losses are set to zero.</p>
</html>
"), DymolaStoredErrors);
end InverterSwitch;

model InverterEquation "Inverter equation, 1-phase"
  extends Partials.SwitchEquation(heat(final m=2));

  parameter Semiconductors.Ideal.SCparameter par "SC parameters"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=
               0)));
  Modelica.Blocks.Interfaces.BooleanInput[4] gates
        "gates pairs {a1_p, a1_n, a2_p, a2_n}"
  annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
  constant Integer[2] pgt={1,3} "positive gates";
  constant Integer[2] ngt={2,4} "negative gates";
  SI.Voltage[2] V_s;
  SI.Voltage[2] V_d;
  SI.Voltage[2] i_sc "current scaled to voltage in inertial abc representation";
  Real[2] s "arc-length on characteristic";

equation
  i_sc = AC.i*par.V_nom/par.I_nom;

  if par.Vf<1e-3 then // faster code if forward voltage drop Vf not used (Vf_s=0 and Vf_d=0).
    for k in 1:2 loop
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
    for k in 1:2 loop
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
  annotation (defaultComponentName="inverter",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
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
            Text(
              extent={{-40,-60},{40,-80}},
              lineColor={176,0,0},
              textString=
                 "time resolved equation"),
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
            Line(points={{-70,-10},{-60,-10},{-60,-46},{30,-46}}, color={0,0,
                  255})}),
      Documentation(info="<html>
<p>Four quadrant switched inverter, based on switch equation. Fulfills the power balance:
<pre>  vAC*iAC = vDC*iDC</pre></p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
<p>The Boolean parameter Vf_zero chooses faster code if both Vf_s and Vf_d are zero.<br>
Blocking losses are neglected in the expression of dissipated heat <tt>Q_flow</tt>.</p>
</html>"),
    DymolaStoredErrors);
end InverterEquation;

model InverterModular "Inverter modular, 1-phase"
  extends Partials.AC_DC_base(heat(final m=2));

  package SCpackage=Semiconductors.Ideal "SC package";
  replaceable parameter SCpackage.SCparameter par "SC parameters"
  annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=0)));
  Modelica.Blocks.Interfaces.BooleanInput[4] gates
        "gates pairs {a1_p, a1_n, a2_p, a2_n}"
  annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
  Nodes.Electric_pn_p_n pn_p_n annotation (Placement(transformation(extent={{80,
                -10},{60,10}}, rotation=0)));
  Common.Thermal.Heat_a_b_ab heat_adapt annotation (Placement(transformation(
              extent={{-10,60},{10,80}}, rotation=0)));
  Blocks.Multiplex.Gate2demux gate2demux1(final n=2)
    annotation (Placement(transformation(extent={{-50,60},{-30,80}}, rotation=0)));
  Semiconductors.PhaseModules.SwitchModule switchMod_a1(par=par)
        "switch + reverse diode module AC_a1"
      annotation (Placement(transformation(extent={{-10,20},{10,40}}, rotation=
                0)));
  Semiconductors.PhaseModules.SwitchModule switchMod_a2(par=par)
        "switch + reverse diode module AC_a2"
      annotation (Placement(transformation(extent={{-10,-40},{10,-20}},
              rotation=0)));

equation
  connect(gate2demux1.gates_1, switchMod_a1.gates) annotation (Line(points={{
              -44,60},{-44,50},{-6,50},{-6,40}}, color={255,0,255}));
  connect(gate2demux1.gates_2, switchMod_a2.gates) annotation (Line(points={{
              -36,60},{-36,-10},{-6,-10},{-6,-20}}, color={255,0,255}));
  connect(gates, gate2demux1.gates) annotation (Line(points={{-60,100},{-60,80},
              {-40,80}}, color={255,0,255}));
  connect(AC, pn_p_n.term_pn)
      annotation (Line(points={{100,0},{76,0}}, color={0,0,255}));
  connect(pn_p_n.term_p, switchMod_a1.AC)   annotation (Line(points={{64,4},{40,
              4},{40,30},{10,30}}, color={0,0,255}));
  connect(switchMod_a1.DC, DC)   annotation (Line(points={{-10,30},{-60,30},{
              -60,0},{-100,0}}, color={0,0,255}));
  connect(pn_p_n.term_n, switchMod_a2.AC)   annotation (Line(points={{64,-4},{
              40,-4},{40,-30},{10,-30}}, color={0,0,255}));
  connect(switchMod_a2.DC, DC)   annotation (Line(points={{-10,-30},{-60,-30},{
              -60,0},{-100,0}}, color={0,0,255}));
  connect(switchMod_a1.heat, heat_adapt.port_a) annotation (Line(points={{0,40},
              {0,54},{-4,54},{-4,64}}, color={176,0,0}));
  connect(switchMod_a2.heat, heat_adapt.port_b) annotation (Line(points={{0,-20},
              {0,-10},{20,-10},{20,54},{4,54},{4,64}}, color={176,0,0}));
  connect(heat_adapt.port_ab, heat)
        annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
annotation (defaultComponentName="inverter",
  Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
  Documentation(
          info=""),
  Icon(coordinateSystem(
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

model ChopperModular "DC_DC converter modular"
  extends Partials.DC_DC_base(heat(final m=2));

  package SCpackage=Semiconductors.Ideal "SC package";
  replaceable parameter SCpackage.SCparameter par "SC parameters"
  annotation (Placement(transformation(extent={{-80,-80},{-60,-60}}, rotation=0)));
  Modelica.Blocks.Interfaces.BooleanInput gate "gate"
    annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
  AC1ph_DC.Nodes.Electric_pn_p_n pn_p_n      annotation (Placement(transformation(
              extent={{-80,-10},{-60,10}}, rotation=0)));
  AC1ph_DC.Nodes.Electric_pn_p_n p_n_pn      annotation (Placement(transformation(
              extent={{80,-10},{60,10}}, rotation=0)));
  SCpackage.SCswitch_Diode switch_D(par=par) "switch + reverse diode"
                                          annotation (Placement(transformation(
              extent={{-10,20},{10,40}}, rotation=0)));
  SCpackage.Diode diode(par=par) "diode"
    annotation (Placement(transformation(
            origin={30,-10},
            extent={{-10,-10},{10,10}},
            rotation=90)));

      Common.Thermal.Heat_a_b_ab heat_adapt
        annotation (Placement(transformation(extent={{-10,60},{10,80}},
              rotation=0)));
equation
  connect(gate, switch_D.gate)
                              annotation (Line(points={{-60,100},{-60,50},{6,50},
              {6,40}}, color={255,0,255}));
  connect(DCin, pn_p_n.term_pn) annotation (Line(points={{-100,0},{-76,0}},
            color={0,0,255}));
  connect(pn_p_n.term_p, switch_D.term_p) annotation (Line(points={{-64,4},{-40,
              4},{-40,30},{-10,30}}, color={0,0,255}));
  connect(switch_D.term_n, p_n_pn.term_p) annotation (Line(points={{10,30},{64,
              30},{64,4}}, color={0,0,255}));
  connect(pn_p_n.term_n, p_n_pn.term_n) annotation (Line(points={{-64,-4},{-40,
              -4},{-40,-32},{64,-32},{64,-4}}, color={0,0,255}));
  connect(p_n_pn.term_n, diode.term_p) annotation (Line(points={{64,-4},{64,-32},
              {30,-32},{30,-20}}, color={0,0,255}));
  connect(diode.term_n, p_n_pn.term_p) annotation (Line(points={{30,0},{30,30},
              {64,30},{64,4}}, color={0,0,255}));
  connect(p_n_pn.term_pn, DCout) annotation (Line(points={{76,0},{100,0}},
            color={0,0,255}));
  connect(switch_D.heat, heat_adapt.port_a)     annotation (Line(points={{0,40},
              {0,54},{-4,54},{-4,64}}, color={176,0,0}));
  connect(diode.heat, heat_adapt.port_b)     annotation (Line(points={{20,-10},
              {20,64},{4,64}}, color={176,0,0}));
  connect(heat_adapt.port_ab, heat)
        annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
  annotation (defaultComponentName = "chopper",
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Documentation(
          info="<html>
<p>One-quadrant chopper.</p>
<p>Gates:
<pre>  true=on, false=off.</pre></p>
</html>
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-70},{100,-90}},
              lineColor={176,0,0},
              textString="modular")}),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end ChopperModular;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.32,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Contains alternative components:
<ul>
<li>Equation-based: faster code, restricted to ideal V-I characteristic, but including forward threshold voltage, needed for calculation of thermal losses.</li>
<li>Modular: composed from semiconductor-switches and diodes. These components with ideal V-I characteristic can be replaced by custom-specified semiconductor models.</li>
</ul>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Components;

package Partials "Partial models"
  extends Modelica.Icons.BasesPackage;

partial model AC_DC_base "AC-DC base, 1-phase"
  extends Basic.Icons.Inverter;

  Ports.TwoPin_n AC "AC connection"
      annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=
               0)));
  Ports.TwoPin_p DC "DC connection"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}},
              rotation=0)));
  Interfaces.ThermalV_n heat(     m=2) "vector heat port"
    annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
  annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Text(
              extent={{-70,34},{-10,4}},
              lineColor={0,0,255},
              textString="="),
            Line(points={{-80,-60},{80,60}}, color={0,0,255}),
            Text(extent={{0,-6},{80,-36}}, textString="~")}),
    Window(
      x=0.45,
          y=0.01,
          width=
    0.44,
      height=
     0.65),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
      Documentation(info="<html>
</html>"));

end AC_DC_base;

partial model DC_DC_base "DC-DC base"
  extends Basic.Icons.Inverter;

  Ports.TwoPin_p DCin "DC in connection"
    annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=
               0)));
  Ports.TwoPin_n DCout "DC out connection"
    annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));
  Interfaces.ThermalV_n heat(     m=2) "vector heat port"
    annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
  annotation (
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Documentation(
          info="<html>
</html>
"), Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Line(points={{-80,-60},{80,60}}, color={0,0,255}),
            Text(
              extent={{-70,34},{-10,4}},
              lineColor={0,0,255},
              textString="="),
            Text(extent={{0,-6},{80,-36}}, textString="="),
            Text(
              extent={{-140,40},{-60,20}},
              lineColor={0,0,255},
              textString="in"),
            Text(
              extent={{60,40},{140,20}},
              lineColor={0,0,255},
              textString="out")}));

end DC_DC_base;

partial model SwitchEquation "Switch equation, 1-phase"
  extends AC_DC_base;

    protected
  SI.Voltage vDC1=0.5*(DC.v[1] - DC.v[2]);
  SI.Voltage vDC0=0.5*(DC.v[1] + DC.v[2]);
  SI.Current iDC1=(DC.i[1] - DC.i[2]);
  SI.Current iDC0=(DC.i[1] + DC.i[2]);
  Real[2] v "switching function voltage";
  Real[2] switch "switching function";

  SI.Temperature[heat.m] T "component temperature";
  SI.HeatFlowRate[heat.m] Q_flow "component loss-heat flow";
  function loss = Basic.Math.taylor "spec loss function of temperature";

equation
  AC.v = v + {vDC0,vDC0};
  iDC1 + switch*AC.i = 0;
  iDC0 + sum(AC.i) = 0;

  T = heat.ports.T;
  heat.ports.Q_flow = -Q_flow;
  annotation (
    Window(
      x=
0.45, y=
0.01, width=
    0.44,
      height=
     0.65),
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-72,8},{-68,12}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-72,-12},{-68,-8}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{76,14},{84,6}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString="a1"),
            Text(
              extent={{76,-6},{84,-14}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid,
              textString="a2"),
            Ellipse(
              extent={{68,12},{72,8}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{68,-8},{72,-12}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-72,8},{-68,12}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-72,-12},{-68,-8}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{68,12},{72,8}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{68,-8},{72,-12}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-85,16},{-73,4}},
              lineColor={0,0,255},
              textString="+"),
            Text(
              extent={{-85,-4},{-73,-16}},
              lineColor={0,0,255},
              textString="-")}),
      Documentation(info="<html>
</html>
"));
end SwitchEquation;

  annotation (       Window(
x=0.05,
y=0.44,
width=0.31,
height=0.26,
library=1,
autolayout=1));
end Partials;
  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.32,
  library=1,
  autolayout=1),
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
