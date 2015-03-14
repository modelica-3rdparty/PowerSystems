within PowerSystems.AC3ph;
package Sources "Voltage and Power Sources"
  extends Modelica.Icons.SourcesPackage;

  model Voltage "Ideal voltage, 3-phase dq0"
    extends Partials.VoltageBase;

    parameter SIpu.Voltage v0=1 "voltage" annotation(Dialog(enable=scType_par));
    parameter SI.Angle alpha0=0 "phase angle" annotation(Dialog(enable=scType_par));
  protected
    SI.Voltage V;
    SI.Angle alpha;
    SI.Angle phi;

  equation
    if scType_par then
      V = v0*V_base;
      alpha = alpha0;
    else
      V = vPhasor_internal[1]*V_base;
      alpha = vPhasor_internal[2];
    end if;
    phi = term.theta[1] + alpha + system.alpha0;
    term.v = {V*cos(phi), V*sin(phi), sqrt(3)*neutral.v};
    annotation (defaultComponentName = "voltage1",
      Documentation(
              info="<html>
<p>Voltage with constant amplitude and phase when 'vType' is 'parameter',<br>
with variable amplitude and phase when 'vType' is 'signal'.</p>
<p>Optional input:
<pre>
  omega           angular frequency (choose fType == \"sig\")
  vPhasor         {norm(v), phase(v)}, amplitude(v_abc)=sqrt(2/3)*vPhasor[1]
   vPhasor[1]     in SI or pu, depending on choice of 'units'
   vPhasor[2]     in rad
</pre></p>
</html>
"));
  end Voltage;

  model Vspectrum "Ideal voltage spectrum, 3-phase dq0"
    extends Partials.VoltageBase;

    constant Real s2=sqrt(2);
    parameter Integer[:] h={1,3,5} "{1, ...}, which harmonics?";
    parameter SIpu.Voltage[N] v0={1,0.3,0.1} "voltages";
    parameter SI.Angle[N] alpha0=zeros(N) "phase angles";
  protected
    final parameter Integer N=size(h, 1) "nb of harmonics";
    SI.Voltage V;
    SI.Angle alpha;
    SI.Angle phi;
    Integer[N] h_mod3;
    Real[3, N] H;

  equation
    if scType_par then
      V = V_base;
      alpha = 0;
    else
      V = vPhasor_internal[1]*V_base;
      alpha = vPhasor_internal[2];
    end if;

  algorithm
    h_mod3 := mod(h, 3);
    for n in 1:N loop
      if h_mod3[n] == 1 then
        phi := h[n]*(theta + alpha + system.alpha0 + alpha0[n]) - term.theta[2];
        H[:, n] := {cos(phi), sin(phi), 0};
      elseif h_mod3[n] == 2 then
        phi := h[n]*(theta + alpha + system.alpha0 + alpha0[n]) + term.theta[2];
        H[:, n] := {cos(phi), -sin(phi), 0};
      else
        phi := h[n]*(theta + alpha + system.alpha0 + alpha0[n]);
        H[:, n] := {0, 0, s2*cos(phi)};
      end if;
    end for;
    term.v := V*(H*v0);
    annotation (defaultComponentName = "Vspec1",
      Documentation(
              info="<html>
<p>Voltage spectrum with constant amplitude and phase when 'vType' is 'parameter',<br>
with variable amplitude and phase when 'vType' is 'signal'.</p>
<p>In inertial abc-system the voltage-spectrum is given by the expression
<pre>
  v_spec_ABC = sqrt(2/3)*sum_n(v0[n]*cos(h[n]*(theta + alpha_tot - k*2*pi/3)))
with
  k=0,1,2 for phase a,b,c
and
  alpha_tot = alpha + system.alpha0 + alpha0[n]
where
  alpha = vPhasor[2] (common phase) for signal input, else 0
</pre></p>
<p>Optional input:
<pre>
  omega            angular frequency (if fType == \"sig\")
  vPhasor          {modulation(v), common phase(v)}
   vPhasor[1] = 1  delivers the values for constant amplitudes v0
   vPhasor[1]      in SI or pu, depending on choice of 'units'
   vPhasor[2]      in rad
</pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-40,60},{40,-20}},
            lineColor={176,0,0},
            lineThickness=0.5,
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="~~~")}));
  end Vspectrum;

  model InfBus "Infinite slack bus, 3-phase dq0"
    extends Partials.PowerBase(final S_nom=1);

    Modelica.Blocks.Interfaces.RealInput[2] vPhasor if not scType_par
      "{abs(voltage), phase}"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Boolean scType_par = true
      "= true: voltage defined by parameter otherwise by input signal"
     annotation(Evaluate=true, choices(__Dymola_checkBox=true));
    parameter SIpu.Voltage v0=1 "voltage"  annotation(Dialog(enable=scType_par));
    parameter SI.Angle alpha0=0 "phase angle"  annotation(Dialog(enable=scType_par));

  protected
    SI.Voltage V;
    SI.Angle alpha;
    SI.Angle phi;
    Modelica.Blocks.Interfaces.RealInput[2] vPhasor_internal
      "Needed to connect to conditional connector";
  equation
    connect(vPhasor, vPhasor_internal);

    if scType_par then
      V = v0*V_base;
      alpha = alpha0;
      vPhasor_internal = {0,0};
    else
      V = vPhasor_internal[1]*V_base;
      alpha = vPhasor_internal[2];
    end if;
    phi = term.theta[1] + alpha + system.alpha0;
    term.v = {V*cos(phi), V*sin(phi), sqrt(3)*neutral.v};
    annotation(defaultComponentName = "infBus",
      Documentation(
            info="<html>
<p>Ideal voltage source with constant amplitude and phase when 'vPhasor' unconnected,<br>
with variable amplitude and phase when 'vPhasor' connected to a signal-input.</p>
<p>Optional input:
<pre>
  vPhasor         {norm(v), phase(v)}
   vPhasor[1]     in SI or pu, depending on choice of 'units'
   vPhasor[2]     in rad
</pre></p>
<p>Frequency: the source has always <i>system</i>-frequency.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,54},{60,14}},
            lineColor={215,215,215},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="V"), Text(
            extent={{-60,54},{60,14}},
            lineColor={128,128,128},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="slack")}));
  end InfBus;

  model VsourceRX "Voltage behind reactance source, 3-phase dq0"
    extends Partials.PowerBase;

    parameter Boolean stIni_en=true "enable steady-state initial equation";
    parameter PowerSystems.Basic.Types.IniType     iniType=PowerSystems.Basic.Types.IniType.v_alpha
      "initialisation type";
    parameter SIpu.Voltage v_ini=1 "initial terminal voltage" annotation(Dialog(enable=iniType==PowerSystems.Basic.Types.IniType.v_alpha or iniType==PowerSystems.Basic.Types.IniType.v_p or iniType==PowerSystems.Basic.Types.IniType.v_q));
    parameter SI.Angle alpha_ini=0 "initial terminal phase angle"             annotation(Dialog(enable=iniType==PowerSystems.Basic.Types.IniType.v_alpha));
    parameter SIpu.ApparentPower p_ini=1 "initial terminal active power"
                                                        annotation(Dialog(enable=iniType==PowerSystems.Basic.Types.IniType.v_p));
    parameter SIpu.ApparentPower q_ini=1 "initial terminal reactive power"
      annotation(Dialog(enable=iniType==PowerSystems.Basic.Types.IniType.v_q));
    parameter SIpu.ApparentPower pq_ini[2]={1,0}
      "initial terminal {active, reactive} power"           annotation(Dialog(enable=iniType==PowerSystems.Basic.Types.IniType.p_q));
    parameter SIpu.Resistance r=0.01 "resistance";
    parameter SIpu.Reactance x=1 "reactance d- and q-axis";
    parameter SIpu.Reactance x_o=0.1 "reactance 0-axis";
  protected
    final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;
    final parameter SIpu.Voltage v0(final fixed=false, start=1)
      "voltage behind reactance";
    final parameter SI.Angle alpha0(final fixed=false, start=0)
      "phase angle of voltage b r";
    final parameter SI.Voltage V(start=V_base)=v0*V_base;
    final parameter Real[2] RL_base=Basic.Precalculation.baseRL(         puUnits, V_nom, S_nom, 2*pi*system.f_nom);
    final parameter SI.Resistance R=r*RL_base[1];
    final parameter SI.Inductance L = x*RL_base[2];
    final parameter SI.Inductance L0 = x_o*RL_base[2];
    SI.Voltage[3] v(start={cos(system.alpha0),sin(system.alpha0),0}*V_base);
    SI.Current[3] i(start={0,0,0});
    SI.AngularFrequency[2] omega;
    SI.Angle phi(start=alpha0+system.alpha0);
    function atan2 = Modelica.Math.atan2;

  initial equation
    if iniType==PowerSystems.Basic.Types.IniType.v_alpha then
      sqrt(v[1:2]*v[1:2]) = v_ini*V_base;
      atan2(v[2], v[1]) = alpha_ini + system.alpha0;
    elseif iniType==PowerSystems.Basic.Types.IniType.v_p then
      sqrt(v[1:2]*v[1:2]) = v_ini*V_base;
      v[1:2]*i[1:2] = p_ini*S_base;
    elseif iniType==PowerSystems.Basic.Types.IniType.v_q then
      sqrt(v[1:2]*v[1:2]) = v_ini*V_base;
      {v[2],-v[1]}*i[1:2] = q_ini*S_base;
    elseif iniType==PowerSystems.Basic.Types.IniType.p_q then
      {v[1:2]*i[1:2], {v[2],-v[1]}*i[1:2]} = pq_ini*S_base;
    end if;
    if steadyIni_t then
      der(i) = omega[1]*j_dq0(i);
    end if;

  equation
    omega = der(term.theta);
    v = term.v;
    i = -term.i;
    phi = term.theta[1] + alpha0 + system.alpha0;

    if system.transientSim then
      diagonal({L,L,L0})*der(i) + omega[2]*L*j_dq0(i) + R*i = {V*cos(phi), V*sin(phi), sqrt(3)*neutral.v} - v;
    else
      omega[2]*L*j_dq0(i) + R*i = {V*cos(phi), V*sin(phi), sqrt(3)*neutral.v} - v;
    end if;
    annotation (defaultComponentName = "Vsource1",
      Documentation(
              info="<html>
<p>Ideal voltage source with constant amplitude and phase, and with resistive-inductive inner impedance.</p>
<p>There are 3 different initialisation choices.
<pre>
  1)  v_ini, alpha_ini  initial terminal voltage and phase angle,
  2)  v_ini, p_ini      initial terminal voltage and active power,
  3)  pq_ini            initial terminal {active power, reactive power},
</pre></p>
<p>Note: v0, alpha0 denote the exciting voltage ('behind reactance'), NOT the terminal voltage. v0 and alpha0 are kept constant during simulation. The values are determined at initialisation by the respective initial values above.</p>
<p>Frequency: the source has always <i>system</i>-frequency.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-70,54},{30,14}},
            lineColor={128,128,128},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="V"), Text(
            extent={{-30,42},{70,18}},
            lineColor={128,128,128},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="RX")}));
  end VsourceRX;

  model PVsource "Power-voltage source, 3-phase dq0"
    extends Partials.PowerBase;

    Modelica.Blocks.Interfaces.RealInput[2] pv if not scType_par
      "{active power, abs(voltage)}"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Boolean scType_par = true
      "= true: active power defined by parameter otherwise by input signal"
     annotation(Evaluate=true, choices(__Dymola_checkBox=true));
    parameter SIpu.ApparentPower p0=1 "active power"
                                             annotation(Dialog(enable=scType_par));
    parameter SIpu.Voltage v0=1 "voltage" annotation(Dialog(enable=scType_par));
    parameter SI.Voltage V_start=V_nom "start value terminal voltage";

  protected
    SI.Power P;
    SI.Voltage V;
    SI.Voltage[2] v(start={cos(system.alpha0),sin(system.alpha0)}*V_start);
    SI.Current[2] i(start={0,0});
    Modelica.Blocks.Interfaces.RealInput[2] pv_internal
      "Needed to connect to conditional connector";
  equation
    connect(pv, pv_internal);

    i = -term.i[1:2];
    if scType_par then
      P = p0*S_base;
      V = v0*V_base;
      pv_internal={0.0, 0.0};
    else
      P = pv_internal[1]*S_base;
      V = pv_internal[2]*V_base;
    end if;
    v*v = V*V;
    v*i = P;
    term.v = cat(1, v, {sqrt(3)*neutral.v});
    annotation (defaultComponentName = "PVsource1",
      Documentation(
              info="<html>
<p>Ideal source with constant power and voltage when 'pv' unconnected,<br>
with variable power and voltage when 'pv' connected to a signal-input.</p>
<p>Optional input:
<pre>
  pv                {p_active, norm(v)}
   pv[1], pv[2]     in SI or pu, depending on choice of 'units'
</pre></p>
<p>Frequency: the source has always <i>system</i>-frequency.</p>
<p>Use only if prescibed values for p and v are compatible with the properties of the connected system.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,54},{60,14}},
            lineColor={128,128,128},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="PV")}));
  end PVsource;

  model PQsource "Power source, 3-phase dq0"
    extends Partials.PowerBase;

    Modelica.Blocks.Interfaces.RealInput[2] pq(
                           final unit="1") if not scType_par
      "{active, reactive} power"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Boolean scType_par = true
      "= true: apparent power defined by parameter otherwise by input signal"
     annotation(Evaluate=true, choices(__Dymola_checkBox=true));

    parameter SIpu.ApparentPower pq0[2]={1,0} "{active, reactive} power"
                                                                annotation(Dialog(enable=scType_par));
    parameter SI.Voltage V_start=V_nom "start value terminal voltage";

  protected
    SI.Power[2] P;
    SI.Voltage[2] v(start={cos(system.alpha0),sin(system.alpha0)}*V_start);
    SI.Current[2] i(start={0,0});
    Modelica.Blocks.Interfaces.RealInput[2] pq_internal
      "Needed to connect to conditional connector";
  equation
    connect(pq, pq_internal);

    i = -term.i[1:2];
    if scType_par then
      P = pq0*S_base;
      pq_internal={0.0, 0.0};
    else
      P = pq_internal*S_base;
    end if;
    {v*i, {v[2],-v[1]}*i} = P;
    term.v = cat(1, v, {sqrt(3)*neutral.v});
    annotation (defaultComponentName = "PQsource1",
      Documentation(
              info="<html>
<p>Ideal source with constant (active, reactive) power when 'pq' unconnected,<br>
with variable (active, reactive) power when 'pq' connected to a signal-input.</p>
<p>Optional input:
<pre>
  pq               {p_active, p_reactive}
   pq[1], pq[2]     in SI or pu, depending on choice of 'units'
</pre></p>
<p>Frequency: the source has always <i>system</i>-frequency.</p>
<p>Use only if prescibed values for pq are compatible with the properties of the connected system.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,54},{60,14}},
            lineColor={128,128,128},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString="PQ")}));
  end PQsource;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model SourceBase "Voltage base, 3-phase dq0"
      extends Ports.Port_n;
      extends Basic.Nominal.Nominal;

      Interfaces.Electric_p neutral "(use for grounding)"
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    protected
      outer System system;
      final parameter Real V_base=Basic.Precalculation.baseV(puUnits, V_nom);
      SI.Angle theta(stateSelect=StateSelect.prefer) "absolute angle";

    equation
      Connections.potentialRoot(term.theta);
      if Connections.isRoot(term.theta) then
        term.theta = if system.synRef then {0, theta} else {theta, 0};
      end if;

      sqrt(3)*term.i[3] + neutral.i = 0;
      annotation (
        Documentation(
              info="<html>
<p>If the connector 'neutral' remains unconnected, then the source has an isolated neutral point. In all other cases connect 'neutral' to the desired circuit or ground.</p>
</html>"));
    end SourceBase;

    partial model VoltageBase "Voltage base, 3-phase dq0"
      extends SourceBase(final S_nom=1);

      parameter Boolean fType_sys = true
        "= true, if source has system frequency" annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      parameter Boolean fType_par = true
        "= true, if source has parameter frequency, otherwise defined by input omega"
                                    annotation(Evaluate=true, Dialog(enable=not fType_sys));
      parameter SI.Frequency f=system.f "frequency"
        annotation(Dialog(enable=fType_par));

      parameter Boolean scType_par = true
        "= true: voltage defined by parameter otherwise by input signal"
       annotation(Evaluate=true, choices(__Dymola_checkBox=true));

      Modelica.Blocks.Interfaces.RealInput omega(final unit="rad/s") if not fType_par
        "ang frequency"
        annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Modelica.Blocks.Interfaces.RealInput[2] vPhasor if not scType_par
        "({abs(voltage), phase})"
        annotation (Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
      parameter Types.FreqType fType = if fType_sys then Types.FreqType.sys else
                                           if fType_par then Types.FreqType.par else Types.FreqType.sig
        "frequency type";
      Modelica.Blocks.Interfaces.RealInput omega_internal
        "Needed to connect to conditional connector";
      Modelica.Blocks.Interfaces.RealInput[2] vPhasor_internal
        "Needed to connect to conditional connector";

    initial equation
      if fType == Types.FreqType.sig then
        theta = 0;
      end if;

    equation
      connect(omega, omega_internal);
      connect(vPhasor, vPhasor_internal);
      if fType <> Types.FreqType.sig then
         omega_internal = 0.0;
      end if;
      if scType_par then
         vPhasor_internal = {0,0};
      end if;

      if fType == Types.FreqType.sys then
        theta = system.theta;
      elseif fType == Types.FreqType.par then
        theta = 2*pi*f*(time - system.initime);
      elseif fType == Types.FreqType.sig then
        der(theta) = omega_internal;
      end if;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,-70},{70,70}},
              lineColor={0,100,100},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(
              points={{-70,0},{70,0}},
              color={176,0,0},
              thickness=0.5),
            Text(
              extent={{-50,30},{50,-70}},
              lineColor={176,0,0},
              lineThickness=0.5,
              fillColor={127,0,255},
              fillPattern=FillPattern.Solid,
              textString="~")}));
    end VoltageBase;

    partial model PowerBase "Power source base, 3-phase dq0"
      extends SourceBase;

    protected
      final parameter Real S_base=Basic.Precalculation.baseS(puUnits, S_nom);

    equation
      theta=system.theta;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,-70},{70,70}},
              lineColor={0,120,120},
              lineThickness=0.5,
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid),
            Line(
              points={{-70,0},{70,0}},
              color={176,0,0},
              thickness=0.5),
            Text(
              extent={{-50,30},{50,-70}},
              lineColor={176,0,0},
              lineThickness=0.5,
              fillColor={127,0,255},
              fillPattern=FillPattern.Solid,
              textString="~")}));
    end PowerBase;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>The sources have optional inputs:</p>
<pre>
  vPhasor:   voltage {norm, phase}
  omega:     angular frequency
  pv:        {active power, abs(voltage)}  (only PVsource)
  p:         {active power, rective power} (only PQsource)
</pre>
<p>To use signal inputs, choose parameters vType=signal and/or fType=signal.</p>
<p>General relations between voltage-norms, effective- and peak-values is shown in the table, both
relative to each other (pu, norm = 1) and as example (SI, 400 V).</p>
<table border=1 cellspacing=0 cellpadding=4>
<tr> <th></th> <th></th> <th><b>pu</b></th> <th><b>V</b></th> </tr>
<tr><td>Three-phase norm</td><td>|v_abc|</td><td><b>1</b></td><td><b>400</b></td></tr>
<tr><td>Single-phase amplitude</td><td>ampl (v_a), ..</td><td>sqrt(2/3)</td> <td>326</td> </tr>
<tr><td>Single-phase effective</td><td>eff (v_a), ..</td><td><b>1/sqrt(3)</b></td><td><b>230</b></td></tr>
<tr><td>Phase to phase amplitude</td><td>ampl (v_b - v_c), ..</td><td>sqrt(2)</td><td>565</td></tr>
<tr><td>Phase to phase effective</td><td>eff (v_b - v_c), ..</td><td><b>1</b></td><td><b>400</b></td></tr>
<tr><td>Three-phase norm</td><td>|v_dq0|</td><td><b>1</b></td><td><b>400</b></td> </tr>
<tr><td>Phase to phase dq-norm</td><td>|vpp_dq|</td><td>sqrt(2)</td><td>565</td></tr>
</table>
</html>
"));
end Sources;
