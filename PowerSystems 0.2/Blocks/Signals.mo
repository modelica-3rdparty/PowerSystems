within PowerSystems.Blocks;
package Signals "Special signals"
  extends Modelica.Icons.VariantsPackage;

  block Constant "Constant vector"
    extends Partials.SO;

    parameter Real c=1 "constant";

  equation
    y = c;
    annotation (defaultComponentName = "cstSig1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Constant;

  block Transient "Transient vector"
    extends Partials.SO;

    parameter SI.Time t_change=0.5 "time when change";
    parameter SI.Time t_duration=1 "transition duration";
    parameter Real s_ini=1 "initial value";
    parameter Real s_fin=1 "final value";
  protected
    final parameter SI.Frequency coef=2*exp(1)/t_duration;

  equation
    y = 0.5*((s_fin + s_ini) + (s_fin - s_ini)*tanh(coef*(time - t_change)));
    annotation (defaultComponentName = "transSig1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The signal changes from <tt>s_ini</tt> to <tt>s_fin</tt><br>
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
            extent={{-110,-10},{10,-50}},
            lineColor={160,160,164},
            textString=
                 "ini"),
          Text(
            extent={{-10,50},{110,10}},
            lineColor={160,160,164},
            textString=
                 "fin")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Transient;

  block ConstantPhasor "Constant {norm, phase} of vector"
    extends Partials.MO(final n=2);

    parameter Real a=1 "norm |y|";
    parameter SI.Angle ph=0 "phase (y)";

  equation
    y= {a, ph};
    annotation (defaultComponentName = "cstPh1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
                 "{%a,%ph}")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ConstantPhasor;

  block TransientPhasor "Transient {norm, phase} of vector"
    extends Partials.MO(final n=2);

    parameter SI.Time t_change=0.5 "time when change";
    parameter SI.Time t_duration=1 "transition duration";
    parameter Real a_ini=1 "initial norm |y|";
    parameter Real a_fin=1 "final norm |y|";
    parameter SI.Angle ph_ini=0 "initial phase (y)";
    parameter SI.Angle ph_fin=0 "final phase (y)";
  protected
    final parameter SI.Frequency coef=2*exp(1)/t_duration;

  equation
    y = 0.5*({a_fin+a_ini, ph_fin+ph_ini} + {a_fin-a_ini, ph_fin-ph_ini}*tanh(coef*(time - t_change)));
     annotation (defaultComponentName = "transPh1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The signal is a two-dimensional vector in polar representation.<br>
Norm and phase change from <tt>{a_ini, ph_ini}</tt> to <tt>{a_fin, ph_fin}</tt><br>
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
            extent={{-110,-10},{10,-50}},
            lineColor={160,160,164},
            textString=
                 "ini"),
          Text(
            extent={{-10,50},{110,10}},
            lineColor={160,160,164},
            textString=
                 "fin"),
          Line(points={{-80,-60},{-64,-60},{-44,-58},{-34,-54},{-26,-48},{-20,
                -40},{-14,-30},{-8,-18},{-2,-6},{2,4},{8,18},{14,30},{20,40},{
                26,48},{34,54},{44,58},{64,60},{80,60}}, color={95,0,191})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end TransientPhasor;

  block ConstantFreq "Constant frequency"
    extends Constant(final c=2*pi*f);

    parameter SI.Frequency f=system.f "frequency";
  protected
    outer System system;
    annotation (defaultComponentName = "cstFreq1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
                 "omega")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ConstantFreq;

  block TransientFreq "Transient frequency"
    extends Transient(final s_ini=omega_ini, final s_fin=omega_fin);

    parameter SI.Frequency f_ini=system.f "initial frequency";
    parameter SI.Frequency f_fin=system.f "final frequency";
  protected
    outer System system;
    final parameter SI.AngularFrequency omega_ini=2*pi*f_ini;
    final parameter SI.AngularFrequency omega_fin=2*pi*f_fin;
    annotation (defaultComponentName = "transFreq1",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The frequency changes from <tt>f_ini</tt> to <tt>f_fin</tt><br>
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
                 "omega")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
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
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
                 "~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Sin;

  block VarSin "Sine of n phases with variable amplitude and frequency"
    extends Partials.MO;

    parameter SI.Angle phComm=0 "common phase angle";
    parameter SI.Angle[n] phShift=(0:n-1)*2*pi/n "phase shift";
    Modelica.Blocks.Interfaces.RealInput amplitude "signal amplitude"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}},
            rotation=0)));
    Modelica.Blocks.Interfaces.RealInput omega "angular frequency"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}},
            rotation=0)));
  protected
    final parameter SI.Angle[n] ph=fill(phComm, n) - phShift;
    SI.Angle theta(start=0, fixed=true, stateSelect=StateSelect.always);

  equation
    der(theta) = omega;
    y = amplitude*sin(fill(theta, n) + ph);
    annotation (defaultComponentName = "varSin",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
           "~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end VarSin;

  block VarSinSpec
    "Sine + harmonics of n phases with variable amplitude and frequency"
    extends Partials.MO;

    Modelica.Blocks.Interfaces.RealInput amplitude "common signal amplitude"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}},
            rotation=0)));
    Modelica.Blocks.Interfaces.RealInput omega "common angular frequency"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}},
            rotation=0)));
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
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
           "~~~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end VarSinSpec;
  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.38,
  library=1,
  autolayout=1),
Documentation(info="<html>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Signals;
