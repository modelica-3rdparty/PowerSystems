within PowerSystems.AC1ph_DC;
package Sources "DC voltage sources"
  extends Modelica.Icons.SourcesPackage;

  model ACvoltage "Ideal AC voltage, 1-phase"
    extends Partials.ACvoltageBase;

    parameter SIpu.Voltage v0eff=1 "fixed effective voltage" annotation(Dialog(enable=not use_vPhasor_in));
    parameter SI.Angle alpha0=0 "fixed phase angle" annotation(Dialog(enable=not use_vPhasor_in));
  protected
    PS.Voltage V;
    SI.Angle alpha;
    SI.Angle phi;

  equation
    if not use_vPhasor_in then
      vPhasor_internal = {v0eff, alpha0};
    end if;
    V = vPhasor_internal[1]*sqrt(2)*V_base;
    alpha = vPhasor_internal[2];

    phi = theta + alpha + system.alpha0;
    term.v[1] - term.v[2] = V*cos(phi);
    annotation (defaultComponentName = "voltage1",
      Documentation(
              info="<html>
<p>AC voltage with constant amplitude and phase when 'vType' is 'parameter',<br>
with variable amplitude and phase when 'vType' is 'signal'.</p>
<p>Optional input:
<pre>
  omega_in           angular frequency  (choose fType == \"sig\")
  vPhasor_in         {eff(v), phase(v)}
   vPhasor_in[1]     in SI or pu, depending on choice of 'units'
   vPhasor_in[2]     in rad
</pre></p>
</html>
"));
  end ACvoltage;

  model Vspectrum "Ideal voltage spectrum, 1-phase"
    extends Partials.ACvoltageBase;

    parameter Integer[:] h={1,3,5} "[1,.. ], which harmonics?";
    parameter SIpu.Voltage[N] v0eff={1,0.3,0.1} "effective voltages";
    parameter SI.Angle[N] alpha0=zeros(N) "phase angles";
  protected
    final parameter Integer N=size(h, 1) "nb of harmonics";
    PS.Voltage V;
    SI.Angle alpha;
    SI.Angle[N] phi;

  equation
    if not use_vPhasor_in then
      vPhasor_internal = {1, 0};
    end if;
    V = vPhasor_internal[1]*sqrt(2)*V_base;
    alpha = vPhasor_internal[2];

    phi = h*(theta + alpha + system.alpha0) + h.*alpha0;
    term.v[1] - term.v[2] = V*v0eff*cos(phi);
    annotation (defaultComponentName = "voltage1",
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
  alpha = vPhasor_in[2] (common phase) for signal input, else 0
</pre></p>
<p>Optional input:
<pre>
  omega_in            angular frequency (if fType == \"sig\")
  vPhasor_in          {modulation(v), common phase(v)}
   vPhasor_in[1] = 1  delivers the values for constant amplitudes v0
   vPhasor_in[1]      in SI or pu, depending on choice of 'units'
   vPhasor_in[2]      in rad
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
                 "~~~")}));
  end Vspectrum;

  model DCvoltage "Ideal DC voltage"
    extends Partials.DCvoltageBase(pol=-1);

    parameter SIpu.Voltage v0=1 "fixed DC voltage"   annotation(Dialog(enable=not use_vDC_in));
  protected
    PS.Voltage v;

  equation
    if not use_vDC_in then
      vDC_internal = v0;
    end if;
    v = vDC_internal*V_base;
    term.v[1] - term.v[2] = v;
    annotation (defaultComponentName = "voltage1",
      Documentation(
              info="<html>
<p>DC voltage with constant amplitude when 'vType' is 'parameter',<br>
with variable amplitude when 'vType' is 'signal'.</p>
<p>Optional input:
<pre>  vDC_in     DC voltage in SI or pu, depending on choice of 'units' </pre></p>
</html>
"));
  end DCvoltage;

  model Battery "Battery"
    extends Ports.Port_n;
    extends Basic.Nominal.Nominal;

    parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
      annotation(Evaluate=true,
      choices(choice=1 "plus",
      choice=0 "symmetrical",
      choice=-1 "negative"));
    parameter SIpu.Voltage v0=1 "battery voltage";
    parameter Types.Charge_Ah Q_nom=1 "nominal Capacity";
  protected
    final parameter Real V_base=Basic.Precalculation.baseV(puUnits, V_nom);
    PS.Voltage v;
    PS.Current i;

  equation
    v = v0*V_base;
    term.v[2] = 0;
    term.v[1] - term.v[2] = v;
    term.i[1] = -i;
    annotation (defaultComponentName = "battery1",
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
            thickness=0.5)}));
  end Battery;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model VoltageBase "Voltage base"
      extends Ports.Port_n;
      extends Basic.Nominal.Nominal(
                                 final S_nom=1);

      parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
        annotation(Evaluate=true,
        choices(choice=1 "positive",
        choice=0 "symmetrical",
        choice=-1 "negative"));

      Interfaces.Electric_p neutral "(use for grounding)"
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
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
              thickness=0.5)}));
    end VoltageBase;

    partial model ACvoltageBase "AC voltage base"

      parameter Basic.Types.FrequencyType fType=PowerSystems.Basic.Types.FrequencyType.System
        "frequency type" annotation(Evaluate=true);
      parameter SI.Frequency f=system.f "frequency if type is parameter"
        annotation(Dialog(enable=fType==PowerSystems.Basic.Types.FrequencyType.Parameter));
      extends VoltageBase;

      parameter Boolean use_vPhasor_in = false
        "= true to use input signal vPhasor_in, otherwise use fixed values"
       annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      Modelica.Blocks.Interfaces.RealInput[2] vPhasor_in if use_vPhasor_in
        "{abs(voltage), phase(voltage)}" annotation (Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Modelica.Blocks.Interfaces.RealInput omega_in(final unit="rad/s") if
           fType == PowerSystems.Basic.Types.FrequencyType.Signal
        "Angular frequency of source" annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

    protected
      Modelica.Blocks.Interfaces.RealInput omega_internal
        "Needed to connect to conditional connector";
      Modelica.Blocks.Interfaces.RealInput[2] vPhasor_internal
        "Needed to connect to conditional connector";

      outer System system;
      SI.Angle theta(stateSelect=StateSelect.prefer);

    initial equation
      if fType == Types.FrequencyType.Signal then
        theta = 0;
      end if;

    equation
      connect(omega_in, omega_internal);
      connect(vPhasor_in, vPhasor_internal);
      if fType <> Types.FrequencyType.Signal then
         omega_internal = 0.0;
      end if;

      if fType == Types.FrequencyType.System then
        theta = system.theta;
      elseif fType == Types.FrequencyType.Parameter then
        theta = 2*pi*f*(time - system.initime);
      elseif fType == Types.FrequencyType.Signal then
        der(theta) = omega_internal;
      end if;
      annotation (
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
           "~")}));
    end ACvoltageBase;

    partial model DCvoltageBase "DC voltage base"
      extends VoltageBase;

      parameter Integer pol(min=-1,max=1)=-1 "grounding scheme"
        annotation(Evaluate=true,
        choices(choice=1 "positive",
        choice=0 "symmetrical",
        choice=-1 "negative"));

      parameter Boolean use_vDC_in = false
        "= true to use input signal vDC_in, otherwise use fixed value"
       annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      Modelica.Blocks.Interfaces.RealInput vDC_in if
                                                  use_vDC_in "DC voltage" annotation (
          Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    protected
      Modelica.Blocks.Interfaces.RealInput vDC_internal
        "Needed to connect to conditional connector";
    equation
      connect(vDC_in, vDC_internal);

      annotation (
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
                   "=")}));
    end DCvoltageBase;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>AC sources have the optional inputs:</p>
<pre>
  vPhasor_in:   voltage {norm, phase}
  omega_in:     angular frequency
</pre>
<p>DC sources have the optional input:</p>
<pre>  vDC_in:       DC voltage</pre>
<p>To use signal inputs, choose parameters vType=signal and/or fType=signal.</p>
</html>"));
end Sources;
