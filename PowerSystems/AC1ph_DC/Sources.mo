within PowerSystems.AC1ph_DC;
package Sources "DC voltage sources"
  extends Modelica.Icons.SourcesPackage;

  model ACvoltage "Ideal AC voltage, 1-phase"
    extends Partials.ACvoltageBase;

    parameter SIpu.Voltage veff=1 "eff voltage"   annotation(Dialog(enable=scType_par));
    parameter SI.Angle alpha0=0 "phase angle"   annotation(Dialog(enable=scType_par));
  protected
    SI.Voltage V;
    SI.Angle alpha;
    SI.Angle phi;

  equation
    if scType_par then
      V = veff*sqrt(2)*V_base;
      alpha = alpha0;
    else
      V = vPhasor_internal[1]*sqrt(2)*V_base;
      alpha = vPhasor_internal[2];
    end if;

    phi = theta + alpha + system.alpha0;
    term.v[1] - term.v[2] = V*cos(phi);
    annotation (defaultComponentName = "voltage1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>AC voltage with constant amplitude and phase when 'vType' is 'parameter',<br>
with variable amplitude and phase when 'vType' is 'signal'.</p>
<p>Optional input:
<pre>
  omega           angular frequency  (choose fType == \"sig\")
  vPhasor         {eff(v), phase(v)}
   vPhasor[1]     in SI or pu, depending on choice of 'units'
   vPhasor[2]     in rad
</pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ACvoltage;

  model Vspectrum "Ideal voltage spectrum, 1-phase"
    extends Partials.ACvoltageBase;

    parameter Integer[:] h={1,3,5} "[1,.. ], which harmonics?";
    parameter SIpu.Voltage[N] veff={1,0.3,0.1} "eff voltages";
    parameter SI.Angle[N] alpha0=zeros(N) "phase angles";
  protected
    final parameter Integer N=size(h, 1) "nb of harmonics";
    SI.Voltage V;
    SI.Angle alpha;
    SI.Angle[N] phi;

  equation
    if scType_par then
      V = sqrt(2)*V_base;
      alpha = 0;
    else
      V = vPhasor_internal[1]*sqrt(2)*V_base;
      alpha = vPhasor_internal[2];
    end if;

    phi = h*(theta + alpha + system.alpha0) + h.*alpha0;
    term.v[1] - term.v[2] = V*veff*cos(phi);
    annotation (defaultComponentName = "voltage1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>AC voltage spectrum with constant amplitude and phase when 'vType' is 'parameter',<br>
with variable amplitude and phase when 'vType' is 'signal'.</p>
<p>The voltage-spectrum is given by the expression
<pre>
  v_spec = sqrt(2)*veff*sum_n(cos(h[n]*(theta + alpha_tot[n])))
with
  alpha_tot[n] = alpha + system.alpha0 + alpha0[n]
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
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-40,60},{40,-20}},
            lineColor={176,0,0},
            lineThickness=0.5,
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "~~~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Vspectrum;

  model DCvoltage "Ideal DC voltage"
    extends Partials.DCvoltageBase(pol=-1);

    parameter SIpu.Voltage v0=1 "DC voltage"   annotation(Dialog(enable=scType_par));
  protected
    SI.Voltage v;

  equation
    if scType_par then
      v = v0*V_base;
    else
      v = vDC_internal*V_base;
    end if;
    term.v[1] - term.v[2] = v;
    annotation (defaultComponentName = "voltage1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>DC voltage with constant amplitude when 'vType' is 'parameter',<br>
with variable amplitude when 'vType' is 'signal'.</p>
<p>Optional input:
<pre>  vDC     DC voltage in SI or pu, depending on choice of 'units' </pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end DCvoltage;

  model Battery "Battery"
    extends Ports.Port_n;
    extends Basic.Nominal.Nominal;

    parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
      annotation(evaluate=true,
      choices(choice=1 "plus",
      choice=0 "symmetrical",
      choice=-1 "negative"));
    parameter SIpu.Voltage v0=1 "battery voltage";
    parameter Types.Charge_Ah Q_nom=1 "nominal Capacity";
  protected
    final parameter Real V_base=Basic.Precalculation.baseV(      puUnits, V_nom);
    SI.Voltage v;
    SI.Current i;

  equation
    v = v0*V_base;
    term.v[2] = 0;
    term.v[1] - term.v[2] = v;
    term.i[1] = -i;
    annotation (defaultComponentName = "battery1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p><b>Preliminary:</b> Battery is DC voltage with constant amplitude.<br>
To be completed later with charging and discharging characteristic.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-70,-70},{70,70}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-70,0},{70,0}},
            color={176,0,0},
            thickness=0.5),
          Line(points={{-34,40},{-34,-40}}, color={176,0,0}),
          Line(points={{-20,20},{-20,-20}}, color={176,0,0}),
          Line(points={{20,40},{20,-40}}, color={176,0,0}),
          Line(points={{34,20},{34,-20}}, color={176,0,0}),
          Line(
            points={{-34,0},{-20,0}},
            color={255,255,255},
            thickness=0.5),
          Line(
            points={{20,0},{34,0}},
            color={255,255,255},
            thickness=0.5)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Battery;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model VoltageBase "Voltage base"
      extends Ports.Port_n;
      extends Basic.Nominal.Nominal(
                                 final S_nom=1);

      parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
        annotation(evaluate=true,
        choices(choice=1 "positive",
        choice=0 "symmetrical",
        choice=-1 "negative"));
      parameter Boolean scType_par = true
        "= true: voltage defined by parameter otherwise by input signal"
       annotation(Evaluate=true, choices(__Dymola_checkBox=true));

                                   Interfaces.Electric_p
        neutral "(use for grounding)"
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}},
              rotation=0)));
    protected
      final parameter Real V_base=Basic.Precalculation.baseV(puUnits, V_nom);

    equation
      if pol==1 then
        term.v[1] = neutral.v;
      elseif pol==-1 then
        term.v[2] = neutral.v;
      else
        term.v[1] + term.v[2] = neutral.v;
      end if;

      sum(term.i) + neutral.i = 0;
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
<p>Allows positive, symmetrical, and negativ grounding according to the choice of parameter 'pol'.<br>
If the connector 'neutral' remains unconnected, then the source is NOT grounded. In all other cases connect 'neutral' to the desired circuit or ground.</p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(
              extent={{-70,-70},{70,70}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Line(
              points={{-70,0},{70,0}},
              color={176,0,0},
              thickness=0.5)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end VoltageBase;

    partial model ACvoltageBase "AC voltage base"

      parameter Boolean fType_sys = true
        "= true, if source has system frequency" annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      parameter Boolean fType_par = true
        "= true, if source has parameter frequency, otherwise defined by input omega"
                                    annotation(Evaluate=true, Dialog(enable=not fType_sys));

      parameter SI.Frequency f=system.f "source frequency" annotation(Dialog(enable= not fType_sys and fType_par));
      extends VoltageBase;

      Modelica.Blocks.Interfaces.RealInput[2] vPhasor if not scType_par
        "{abs(voltage), phase(voltage)}"
        annotation (Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Modelica.Blocks.Interfaces.RealInput omega(final unit="rad/s") if not fType_par
        "Angular frequency of source"
        annotation (Placement(transformation(
            origin={-60,100},
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

      outer System system;
      SI.Angle theta(stateSelect=StateSelect.prefer);

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
        Window(
          x=
    0.45, y=
    0.01, width=
        0.44,
          height=
         0.65),
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-50,30},{50,-70}},
              lineColor={176,0,0},
              lineThickness=0.5,
              fillColor={127,0,255},
              fillPattern=FillPattern.Solid,
              textString=
           "~")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end ACvoltageBase;

    partial model DCvoltageBase "DC voltage base"
      extends VoltageBase;

      parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
        annotation(evaluate=true,
        choices(choice=1 "positive",
        choice=0 "symmetrical",
        choice=-1 "negative"));
      Modelica.Blocks.Interfaces.RealInput vDC if
                                  not scType_par "DC voltage"
        annotation (Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
      Modelica.Blocks.Interfaces.RealInput vDC_internal
        "Needed to connect to conditional connector";
    equation
      connect(vDC, vDC_internal);
      if scType_par then
         vDC_internal = 0.0;
      end if;

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
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-50,10},{50,-60}},
              lineColor={176,0,0},
              lineThickness=0.5,
              fillColor={127,0,255},
              fillPattern=FillPattern.Solid,
              textString=
                   "=")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end DCvoltageBase;
    annotation (       Window(
  x=0.05,
  y=0.44,
  width=0.35,
  height=0.27,
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
<p>AC sources have the optional inputs:</p>
<pre>
  vPhasor:   voltage {norm, phase}
  omega:     angular frequency
</pre>
<p>DC sources have the optional input:</p>
<pre>  vDC:       DC voltage</pre>
<p>To use signal inputs, choose parameters vType=signal and/or fType=signal.</p>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Sources;
