within PowerSystems.Blocks;
package Signals "Special signals"
  extends Modelica.Icons.VariantsPackage;

  block Constant "Constant vector"
    extends Partials.SO;

    parameter Real c=1 "constant";

  equation
    y = c;
    annotation (defaultComponentName = "cstSig1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,20},{100,-20}},
            lineColor={160,160,164},
            textString=
                 "%c")}),
      Documentation(
              info="<html>
</html>
"));
  end Constant;

  block Transient "Transient vector"
    extends Partials.SO;

    parameter SI.Time t_change=0.5 "time when change";
    parameter SI.Time t_duration=1 "transition duration";
    parameter Real s_start=1 "initial value";
    parameter Real s_end=1 "final value";
  protected
    final parameter SI.Frequency coef=2*exp(1)/t_duration;

  equation
    y = 0.5*((s_end + s_start) + (s_end - s_start)*tanh(coef*(time - t_change)));
    annotation (defaultComponentName = "transSig1",
      Documentation(
              info="<html>
<p>The signal changes from <tt>s_start</tt> to <tt>s_end</tt><br>
at time <tt>t_change</tt> with a transition duration <tt>t_duration</tt>.<br><br>
The transition function is a hyperbolic tangent.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-80,-60},{-64,-60},{-44,-58},{-34,-54},{-26,-48},{-20,
                -40},{-14,-30},{-8,-18},{-2,-6},{2,4},{8,18},{14,30},{20,40},{
                26,48},{34,54},{44,58},{64,60},{80,60}}, color={95,0,191}),
          Text(
            extent={{-102,-10},{18,-50}},
            lineColor={160,160,164},
            textString=
                 "start"),
          Text(
            extent={{-10,50},{110,10}},
            lineColor={160,160,164},
            textString=
                 "end")}));
  end Transient;

  block ConstantPhasor "Constant {norm, phase} of vector"
    extends Partials.MO(final n=2);

    parameter Real a=1 "norm |y|";
    parameter SI.Angle ph=0 "phase (y)";

  equation
    y= {a, ph};
    annotation (defaultComponentName = "cstPh1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,100},{100,60}},
            lineColor={175,175,175},
            textString=
                 "phasor"), Text(
            extent={{-100,20},{100,-20}},
            lineColor={160,160,164},
            textString=
                 "{%a,%ph}")}));
  end ConstantPhasor;

  block TransientPhasor "Transient {norm, phase} of vector"
    extends Partials.MO(final n=2);

    parameter SI.Time t_change=0.5 "time when change";
    parameter SI.Time t_duration=1 "transition duration";
    parameter Real a_start=1 "initial norm |y|";
    parameter Real a_end=1 "final norm |y|";
    parameter SI.Angle ph_start=0 "initial phase (y)";
    parameter SI.Angle ph_end=0 "final phase (y)";
  protected
    final parameter SI.Frequency coef=2*exp(1)/t_duration;

  equation
    y = 0.5*({a_end+a_start, ph_end+ph_start} + {a_end-a_start, ph_end-ph_start}*tanh(coef*(time - t_change)));
     annotation (defaultComponentName = "transPh1",
      Documentation(
              info="<html>
<p>The signal is a two-dimensional vector in polar representation.<br>
Norm and phase change from <tt>{a_start, ph_start}</tt> to <tt>{a_end, ph_end}</tt><br>
at time <tt>t_change</tt> with a transition duration <tt>t_duration</tt>.<br><br>
The transition function is a hyperbolic tangent for both norm and phase.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,100},{100,60}},
            lineColor={175,175,175},
            textString=
                 "phasor"),
          Text(
            extent={{-102,-10},{18,-50}},
            lineColor={160,160,164},
            textString=
                 "start"),
          Text(
            extent={{-10,50},{110,10}},
            lineColor={160,160,164},
            textString=
                 "end"),
          Line(points={{-80,-60},{-64,-60},{-44,-58},{-34,-54},{-26,-48},{-20,
                -40},{-14,-30},{-8,-18},{-2,-6},{2,4},{8,18},{14,30},{20,40},{
                26,48},{34,54},{44,58},{64,60},{80,60}}, color={95,0,191})}));
  end TransientPhasor;

  block ConstantFreq "Constant frequency"
    extends Constant(final c=2*pi*f);

    parameter SI.Frequency f=system.f "frequency";
  protected
    outer System system;
    annotation (defaultComponentName = "cstFreq1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,100},{100,60}},
            lineColor={175,175,175},
            textString=
                 "omega")}));
  end ConstantFreq;

  block TransientFreq "Transient frequency"
    extends Transient(final s_start=omega_start, final s_end=omega_end);

    parameter SI.Frequency f_start=system.f "initial frequency";
    parameter SI.Frequency f_end=system.f "final frequency";
  protected
    outer System system;
    final parameter SI.AngularFrequency omega_start=2*pi*f_start;
    final parameter SI.AngularFrequency omega_end=2*pi*f_end;
    annotation (defaultComponentName = "transFreq1",
      Documentation(
              info="<html>
<p>The frequency changes from <tt>f_start</tt> to <tt>f_end</tt><br>
at time <tt>t_change</tt> with a transition duration <tt>t_duration</tt>.<br><br>
The transition function is a hyperbolic tangent.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(points={{-80,-60},{-64,-60},{-44,-58},{
                -34,-54},{-26,-48},{-20,-40},{-14,-30},{-8,-18},{-2,-6},{2,4},{
                8,18},{14,30},{20,40},{26,48},{34,54},{44,58},{64,60},{80,60}},
              color={95,0,191}), Text(
            extent={{-100,100},{100,60}},
            lineColor={175,175,175},
            textString=
                 "omega")}));
  end TransientFreq;

  block Sin "Sine of n phases"
    extends Partials.MO;

    parameter Real a=1 "signal amplitude";
    parameter SI.Frequency f=50 "frequency";
    parameter SI.Angle phComm=0 "common phase angle";
    parameter SI.Angle[n] phShift=(0:n-1)*2*pi/n "phase shift";
  protected
    final parameter SI.AngularFrequency omega=2*pi*f;
    final parameter SI.Angle[n] ph=fill(phComm, n) - phShift;

  equation
    y = a*sin(omega*time*ones(n) + ph);
   annotation (defaultComponentName = "sin",
      Documentation(
              info="<html>
<p>The signal has constant amplitude, phase, and frequency.
The relative angle of the phases can be chosen arbitrarily (for non-symmetric signals).
</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,40},{60,-40}},
            lineColor={160,160,164},
            textString=
                 "~")}));
  end Sin;

  block VarSin "Sine of n phases with variable amplitude and frequency"
    extends Partials.MO;

    parameter SI.Angle phComm=0 "common phase angle";
    parameter SI.Angle[n] phShift=(0:n-1)*2*pi/n "phase shift";
    Modelica.Blocks.Interfaces.RealInput amplitude "signal amplitude"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.RealInput omega "angular frequency"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
  protected
    final parameter SI.Angle[n] ph=fill(phComm, n) - phShift;
    SI.Angle theta(start=0, fixed=true, stateSelect=StateSelect.always);

  equation
    der(theta) = omega;
    y = amplitude*sin(fill(theta, n) + ph);
    annotation (defaultComponentName = "varSin",
      Documentation(
              info="<html>
<p>The signal has variable amplitude and frequency and constant phase.
The relative angle of the phases can be chosen arbitrarily (for non-symmetric signals).</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,40},{60,-40}},
            lineColor={160,160,164},
            textString=
           "~")}));
  end VarSin;

  block VarSinSpec
    "Sine + harmonics of n phases with variable amplitude and frequency"
    extends Partials.MO;

    Modelica.Blocks.Interfaces.RealInput amplitude "common signal amplitude"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.RealInput omega "common angular frequency"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
    parameter SI.Angle phComm=0 "common phase angle";
    parameter SI.Angle phShift[n]=(0:n-1)*2*pi/n "phase shift";
    parameter Integer N=3 "1 + number of harmonics";
    parameter Integer[N] h={1,3,5} "{1, ...} which harmonics?";
    parameter Real[N] a_rel={1,0.3,0.1} "rel amplitudes";
    parameter SI.Angle[N] ph_h={0,0,0} "rel phase angles harmonics";
  protected
    final parameter SI.Angle[n] ph=fill(phComm, n) - phShift;
    SI.Angle theta(start=0, fixed=true, stateSelect=StateSelect.always);

  equation
    der(theta) = omega;
    for k in 1:n loop
      y[k] = amplitude*(a_rel*sin((theta + ph[k])*h + ph_h));
    end for;
   annotation (defaultComponentName = "varSin_spec",
      Documentation(
              info="<html>
<p>The signal has variable amplitude and frequency and constant phase.
The relative angle of the phases can be chosen arbitrarily (for non-symmetric signals).</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,40},{60,-40}},
            lineColor={160,160,164},
            textString=
           "~"), Text(
            extent={{-60,40},{60,0}},
            lineColor={160,160,164},
            textString=
           "~~~")}));
  end VarSinSpec;
  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Signals;
