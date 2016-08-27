within PowerSystems.AC1ph_DC;
package Transformers "Transformers 1-phase "
  extends Modelica.Icons.VariantsPackage;

  model TrafoIdeal "Ideal transformer, 1-phase"
    extends Partials.TrafoIdealBase;

  equation
    i1 + i2 = 0;
    v1 = v2;
  annotation (
    defaultComponentName="trafo",
      Documentation(
        info="<html>
<p>Ideal magnetic coupling, no stray-impedance, zero magnetisation current.</p>
</html>
"));
  end TrafoIdeal;

  model TrafoStray "Ideal magnetic coupling transformer, 1-phase"
    extends Partials.TrafoStrayBase;

  initial equation
    if steadyIni_t then
      der(i1) = 0;
    elseif not system.steadyIni then
      i1 = i1_start;
    end if;

  equation
    i1 + i2 = 0;
    sum(L)*der(i1) + sum(R)*i1 = v1 - v2;
  annotation (
    defaultComponentName="trafo",
      Documentation(
        info="<html>
<p>Stray-impedance, but ideal magnetic coupling, i.e. zero magnetisation current.</p>
<p>SI-input: values for stray and coupling impedances winding dependent.</p>
<pre>
  r[k] = R[k]
  x[k] = omega_nom*L[k]
</pre>
<p>pu-input: values for stray and coupling impedances winding-reduced to primary side.</p>
<pre>
  r[k] = R[k]/R_nom[k]
  x[k] = omega_nom*L[k]/R_nom[k]
</pre>
<p>with</p>
<pre>  R_nom[k] = V_nom[k]^2/S_nom,  k = 1(primary), 2(secondary)</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-10,62},{10,-62}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid)}));
  end TrafoStray;

  model TrafoMag "Magnetic coupling transformer, 1-phase"
    extends Partials.TrafoMagBase;

    PS.Voltage v0;
    PS.Current imag;
    PS.Current iedc;
  Real psi0 "unsaturated flux";

  equation
    i1 + i2 = imag + iedc;
    Redc*iedc = v0;
  psi0 = Lm*imag;
    L[1]*der(i1) + R[1]*i1 = v1 - v0;
    L[2]*der(i2) + R[2]*i2 = v2 - v0;
    Lm*der(imag) = v0;
  annotation (
    defaultComponentName="trafo",
      Documentation(
        info="<html>
<p>Stray-impedance and resistance, with non-ideal magnetic coupling, i.e. non-zero magnetisation current
and eddy current losses.</p>
<p>SI-input: values for stray and coupling impedances winding dependent.</p>
<pre>
  r[k] = R[k]
  x[k] = omega_nom*L[k]
  redc = Redc
  xm   = omega_nom*Lm
</pre>
<p>pu-input: values for stray and coupling impedances winding-reduced to primary side.</p>
<pre>
  r[k] = R[k]/R_nom[k]
  x[k] = omega_nom*L[k]/R_nom[k]
  redc = Redc/sqrt(R_nom[1]*R_nom[2])
  xm = omega_nom*Lm/sqrt(R_nom[1]*R_nom[2])
</pre>
<p>with</p>
<pre>  R_nom[k] = V_nom[k]^2/S_nom,  k = 1(primary), 2(secondary)</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-10,62},{10,-62}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-20,62},{-20,-62}},
            color={0,128,255},
            pattern=LinePattern.Dot),
          Line(
            points={{20,62},{20,-62}},
            color={0,128,255},
            pattern=LinePattern.Dot),
          Ellipse(
            extent={{-22,62},{-18,58}},
            lineColor={0,128,255},
            fillColor={0,128,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{18,-58},{22,-62}},
            lineColor={0,128,255},
            fillColor={0,128,255},
            fillPattern=FillPattern.Solid)}));
  end TrafoMag;

  model TrafoSat "Saturation transformer, 1-phase"
    extends Partials.TrafoSatBase;

    PS.Voltage v0;
    PS.Current imag;
    PS.Current iedc;
  protected
    Real psi0 "unsaturated flux";
    Real g;
    function der_sat = Common.IronSaturation.der_saturationAnalytic;

  equation
    i1 + i2 = imag + iedc;
    Redc*iedc = v0;
    psi0 = Lm*imag;
    g = scalar(der_sat({psi0}/psi_nom, c_sat));

    L[1]*der(i1) + R[1]*i1 = v1 - v0;
    L[2]*der(i2) + R[2]*i2 = v2 - v0;
    g*der(psi0) = v0;
  annotation (
    defaultComponentName="trafo",
      Documentation(
        info="<html>
<p>Stray-impedance and resistance, with non-ideal magnetic coupling, i.e. non-zero magnetisation current, eddy current losses and saturation.</p>
<p>SI-input: values for stray and coupling impedances winding dependent.</p>
<pre>
  r[k] = R[k]
  x[k] = omega_nom*L[k]
  redc = Redc
  xm   = omega_nom*Lm
  xm_sat = omega_nom*Lm_sat,  saturation value of inductance
  psi_sat, pu saturation value of flux (no SI-value!)
</pre>
<p>pu-input: values for stray and coupling impedances winding-reduced to primary side.</p>
<pre>
  r[k] = R[k]/R_nom[k]
  x[k] = omega_nom*L[k]/R_nom[k]
  redc = Redc/sqrt(R_nom[1]*R_nom[2])
  xm = omega_nom*Lm/sqrt(R_nom[1]*R_nom[2])
  xm_sat = omega_nom*Lm_sat/sqrt(R_nom[1]*R_nom[2]),  saturation value of inductance
  psi_sat, pu saturation value of flux
</pre>
<p>with</p>
<pre>  R_nom[k] = V_nom[k]^2/S_nom,  k = 1(primary), 2(secondary)</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-10,62},{10,-62}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-20,62},{-20,-62}},
            color={0,128,255},
            pattern=LinePattern.Dot),
          Line(
            points={{20,62},{20,-62}},
            color={0,128,255},
            pattern=LinePattern.Dot),
          Ellipse(
            extent={{-22,62},{-18,58}},
            lineColor={0,128,255},
            fillColor={0,128,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{18,-58},{22,-62}},
            lineColor={0,128,255},
            fillColor={0,128,255},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-15,-40},{-11,-10},{-7,10},{-5,20},{-1,30},{5,36},{15,40}},
            color={0,0,0},
            thickness=0.5)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-15,-30},{-11,0},{-7,20},{-5,30},{-1,40},{5,46},{15,50}},
            color={0,0,0},
            thickness=0.5), Line(
            points={{-15,-50},{-11,-20},{-7,0},{-5,10},{-1,20},{5,26},{15,30}},
            color={0,0,0},
            thickness=0.5)}));
  end TrafoSat;

  model Trafo3Stray "Ideal magnetic coupling transformer, 1-phase"
    extends Partials.Trafo3StrayBase;

  equation
    i1 + i2a + i2b = 0;
    L[1]*der(i1) + R[1]*i1 = v1 - v0;
    L[2]*der(i2a) + R[2]*i2a = v2a - v0;
    L[3]*der(i2b) + R[3]*i2b = v2b - v0;
  annotation (
    defaultComponentName="trafo",
      Documentation(
        info="<html>
<p>Stray-impedance, but ideal magnetic coupling, i.e. zero magnetisation current.</p>
<p>SI-input: values for stray and coupling impedances winding dependent.</p>
<pre>
  r[k] = R[k]
  x[k] = omega_nom*L[k]
</pre>
<p>pu-input: values for stray and coupling impedances winding-reduced to primary side.</p>
<pre>
  r[k] = R[k]/R_nom[k]
  x[k] = omega_nom*L[k]/R_nom[k]
</pre>
<p>with</p>
<pre>  R_nom[k] = V_nom[k]^2/S_nom,  k = 1(primary), 2(secondary)</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-10,62},{10,-62}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid)}));
  end Trafo3Stray;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model TrafoIdealBase "Base for ideal transformer, 1-phase"

      extends Ports.PortTrafo_p_n(i1(start = i1_start), i2(start = i2_start));

      parameter Boolean stIni_en=true "enable steady-state initial equation"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter PS.Current i1_start = 0 "start value of primary current"
        annotation(Dialog(tab="Initialization"));
      parameter PS.Current i2_start = i1_start
        "start value of secondary current"
        annotation(Dialog(tab="Initialization"));

      parameter Boolean use_tap_1_in = false "= true to enable input tap_1_in"
        annotation(Evaluate=true, choices(checkBox=true), Dialog(group="Options"));
      parameter Integer tap_1 = par.tap_neutral[1] "fixed tap_1 position"
        annotation(Dialog(enable=not use_tap_1_in, group="Options"));
      parameter Boolean use_tap_2_in = false "= true to enable input tap_2_in"
        annotation(Evaluate=true, choices(checkBox=true), Dialog(group="Options"));
      parameter Integer tap_2 = par.tap_neutral[2] "fixed tap_2 position"
        annotation(Dialog(enable=not use_tap_2_in, group="Options"));
      parameter Boolean dynTC=false "enable dynamic tap-changing"
        annotation(Evaluate=true, Dialog(group="Options"));

      Modelica.Blocks.Interfaces.IntegerInput tap_1_in if use_tap_1_in
        "1: index of voltage level"
        annotation (Placement(transformation(
            origin={-40,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Modelica.Blocks.Interfaces.IntegerInput tap_2_in if use_tap_2_in
        "2: index of voltage level"
        annotation (Placement(transformation(
            origin={40,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

      replaceable record Data =
        PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoIdeal1ph
        "trafo parameters" annotation(choicesAllMatching=true);
      final parameter Data par "trafo parameter record"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    protected
      final parameter Boolean steadyIni_t = system.steadyIni_t and stIni_en;
      Modelica.Blocks.Interfaces.IntegerInput tap_1_internal
        "Needed to connect to conditional connector";
      Modelica.Blocks.Interfaces.IntegerInput tap_2_internal
        "Needed to connect to conditional connector";

      outer System system;
      constant Real tc=0.01 "time constant tap-chg switching";
      final parameter PS.Voltage[2] V_base=Basic.Precalculation.baseTrafoV(par.puUnits, par.V_nom);
      final parameter Real[2, 2] RL_base=Basic.Precalculation.baseTrafoRL(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      Real w_nom = par.V_nom[2]/par.V_nom[1] "nominal turns ratio";
      Real[2] dv_tap_pu = par.dv_tap .* V_base ./ par.V_nom;
      Real w1_set = (1 + (tap_1_internal - par.tap_neutral[1]) * dv_tap_pu[1])
        "1: set voltage ratio to nominal primary";
      Real w2_set = (1 + (tap_2_internal - par.tap_neutral[2]) * dv_tap_pu[2]) * w_nom
        "2: set voltage ratio to nominal primary";

    initial equation
      if dynTC then
        w1 = w1_set;
        w2 = w2_set;
      end if;

    equation
      connect(tap_1_in, tap_1_internal);
      connect(tap_2_in, tap_2_internal);
      if not use_tap_1_in then
         tap_1_internal = tap_1;
      end if;
      if not use_tap_2_in then
         tap_2_internal = tap_2;
      end if;

      if dynTC then
        der(w1) + (w1 - w1_set)/tc = 0;
        der(w2) + (w2 - w2_set)/tc = 0;
      else
        w1 = w1_set;
        w2 = w2_set;
      end if;
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-80,60},{40,-60}},
              lineColor={44,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-40,60},{80,-60}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(extent={{-80,60},{40,-60}}, lineColor={0,0,255}),
            Text(
              extent={{-120,80},{-80,40}},
              lineColor={0,0,0},
              textString="1"),
            Text(
              extent={{80,80},{120,40}},
              lineColor={0,0,0},
              textString="2"),
            Line(
              points={{-80,0},{-40,0}},
              color={176,0,0},
              thickness=0.5),
            Line(
              points={{40,0},{80,0}},
              color={176,0,0},
              thickness=0.5)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-20,60},{-14,-60}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{14,60},{20,-60}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid)}),
        Documentation(
        info="<html>
<p>Terminology (formal, the models are symmetric).<br>
&nbsp; - index 1 (term_p)     \"primary\"<br>
&nbsp; - index 2 (term_n)     \"secondary\"</p>
<p>Transformer ratio.<br>
The winding ratio is determined indirectly by the choice of nominal voltages.<br>
It may be &gt  or &lt  1.</p>
<p>Tap changers.<br>
For constant transformer ratio no tap changer input needed.<br>
For variable transformer ratio tap changer input needed.</p>
<p>The sequence of the parameters</p>
<pre>  v_tc     tc voltage levels v_tc[1], v_tc[2], v_tc[3], ...</pre>
<p>must be defined in accordance with the input-signals of </p>
<pre>  tap     index of tap voltage levels, v_tc[tap]</pre>
<p>Set <tt>dynTC = true</tt> if tap-index changes during simulation.</p>
</html>
"));
    end TrafoIdealBase;

    partial model TrafoStrayBase
      "Base for ideal magnetic coupling transformer, 1-phase"
      extends TrafoIdealBase(redeclare replaceable record Data =
          PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoStray1ph);
    protected
      SI.Resistance[2] R=par.r.*RL_base[:, 1];
      SI.Inductance[2] L=par.x.*RL_base[:, 2];
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficients for ideal magnetic coupling transformer</p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-26,60},{-20,-60}},
              lineColor={215,215,215},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{20,60},{26,-60}},
              lineColor={215,215,215},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid)}));
    end TrafoStrayBase;

    partial model TrafoMagBase
      "Base for magnetic coupling transformer, 1-phase"
      extends TrafoStrayBase(redeclare replaceable record Data =
          PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoMag1ph);
    protected
      final parameter SI.Resistance[2] RL12_base = sqrt((RL_base[1,:].*RL_base[2,:]));
      SI.Resistance Redc=par.redc*RL12_base[1];
      SI.Inductance Lm=par.xm*RL12_base[2];
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficients for magnetic coupling trafo transformer</p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(
              points={{-30,60},{-30,-60}},
              color={0,0,255},
              pattern=LinePattern.Dot), Line(
              points={{30,60},{30,-60}},
              color={0,0,255},
              pattern=LinePattern.Dot)}));
    end TrafoMagBase;

    partial model TrafoSatBase "Base for saturation transformer, 1-phase"
      extends TrafoMagBase(redeclare replaceable record Data =
          PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoSat1ph);
    protected
      Real xratio=par.xm_sat/par.xm;
      Real[3] c_sat={1-xratio,(1-xratio)/(par.psi_sat-xratio),xratio};
      final parameter SI.MagneticFlux psi_nom=sqrt(2)*par.V_nom[1]/(2*pi*par.f_nom)
        "amplitude!";
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficients for saturation transformer</p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(
              points={{-15,-40},{-11,-10},{-7,10},{-5,20},{-1,30},{5,36},{15,40}},
              color={0,0,0},
              thickness=0.5)}));

    end TrafoSatBase;

    partial model Trafo3IdealBase "Base for ideal transformer, 1-phase"

      extends Ports.PortTrafo_p_n_n;

      parameter Boolean use_tap_1_in = false "= true to enable input tap_1_in"
        annotation(Evaluate=true, choices(checkBox=true), Dialog(group="Options"));
      parameter Integer tap_1 = par.tap_neutral[1] "fixed tap_1 position"
        annotation(Dialog(enable=not use_tap_1_in, group="Options"));
      parameter Boolean use_tap_2_in = false "= true to enable input tap_2_in"
        annotation(Evaluate=true, choices(checkBox=true), Dialog(group="Options"));
      parameter Integer[2] tap_2 = par.tap_neutral[2:3] "fixed tap_2 position"
        annotation(Dialog(enable=not use_tap_2_in, group="Options"));
      parameter Boolean dynTC=false "enable dynamic tap-changing"
        annotation(Evaluate=true, Dialog(group="Options"));

      Modelica.Blocks.Interfaces.IntegerInput tap_1_in if use_tap_1_in
        "1: index of voltage level"
        annotation (Placement(transformation(
            origin={-40,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Modelica.Blocks.Interfaces.IntegerInput[2] tap_2_in if use_tap_2_in
        "2: indices of voltage levels"
        annotation (Placement(transformation(
            origin={40,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

      replaceable record Data =
        PowerSystems.AC1ph_DC.Transformers.Parameters.Trafo3Ideal1ph
        "trafo parameters" annotation(choicesAllMatching=true);
      final parameter Data par "trafo parameter"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    protected
      Modelica.Blocks.Interfaces.IntegerInput tap_1_internal
        "Needed to connect to conditional connector";
      Modelica.Blocks.Interfaces.IntegerInput[2] tap_2_internal
        "Needed to connect to conditional connector";

      outer System system;
      constant Real tc=0.01 "time constant tap-chg switching";
      final parameter PS.Voltage[3] V_base=Basic.Precalculation.baseTrafoV(par.puUnits, par.V_nom);
      final parameter Real[3, 2] RL_base=Basic.Precalculation.baseTrafoRL(par.puUnits, par.V_nom, par.S_nom, 2*pi*par.f_nom);
      Real wa_nom = par.V_nom[2]/par.V_nom[1] "nominal turns ratio";
      Real wb_nom = par.V_nom[3]/par.V_nom[1] "nominal turns ratio";
      Real[3] dv_tap_pu = par.dv_tap .* V_base ./ par.V_nom;
      Real w1_set = (1 + (tap_1_internal - par.tap_neutral[1]) * dv_tap_pu[1])
        "1: set voltage ratio to nominal primary";
      Real w2a_set = (1 + (tap_2_internal[1] - par.tap_neutral[2]) * dv_tap_pu[2]) * wa_nom
        "2a: set voltage ratio to nominal primary";
      Real w2b_set = (1 + (tap_2_internal[2] - par.tap_neutral[3]) * dv_tap_pu[3]) * wb_nom
        "2b: set voltage ratio to nominal primary";

    initial equation
      if dynTC then
        w1 = w1_set;
        w2a = w2a_set;
        w2b = w2b_set;
      end if;

    equation
      connect(tap_1_in, tap_1_internal);
      connect(tap_2_in, tap_2_internal);
      if not use_tap_1_in then
         tap_1_internal = tap_1;
      end if;
      if not use_tap_2_in then
         tap_2_internal = tap_2;
      end if;

      if dynTC then
        der(w1) + (w1 - w1_set)/tc = 0;
        der(w2a) + (w2a - w2a_set)/tc = 0;
        der(w2b) + (w2b - w2b_set)/tc = 0;
      else
        w1 = w1_set;
        w2a = w2a_set;
        w2b = w2b_set;
      end if;
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-80,60},{40,-60}},
              lineColor={44,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-40,60},{80,-60}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(extent={{-80,60},{40,-60}}, lineColor={0,0,255}),
            Text(
              extent={{-120,80},{-80,40}},
              lineColor={0,0,0},
              textString="1"),
            Text(
              extent={{80,20},{120,-20}},
              lineColor={0,0,0},
              textString="2"),
            Line(
              points={{-80,0},{-40,0}},
              color={176,0,0},
              thickness=0.5),
            Line(
              points={{40,0},{80,0}},
              color={176,0,0},
              thickness=0.5),
            Text(
              extent={{80,100},{120,60}},
              lineColor={0,0,0},
              textString=
                   "a"),
            Text(
              extent={{80,-60},{120,-100}},
              lineColor={0,0,0},
              textString=
                   "b")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-20,60},{-14,-60}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{14,60},{20,-60}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid)}),
        Documentation(
        info="<html>
<p>Terminology (formal).<br>
&nbsp; - index 1 (term_p)     \"primary\"<br>
&nbsp; - index 2a (term_na)     \"secondary a\"<br>
&nbsp; - index 2b (term_nb)     \"secondary b\"</p>
<p>Transformer ratio.<br>
The winding ratio is determined indirectly by the choice of nominal voltages.<br>
It may be &gt  or &lt  1.</p>
<p>Tap changers.<br>
For constant transformer ratio no tap changer input needed.<br>
For variable transformer ratio tap changer input needed.</p>
<p>The sequence of the parameters</p>
<pre>  v_tc     tc voltage levels v_tc[1], v_tc[2], v_tc[3], ...</pre>
<p>must be defined in accordance with the input-signals of </p>
<pre>  tap     index of tap voltage levels, v_tc[tap]</pre>
<p>Set <tt>dynTC = true</tt> if tap-index changes during simulation.</p>
</html>
"));
    end Trafo3IdealBase;

    partial model Trafo3StrayBase
      "Base for ideal magnetic coupling transformer, 1-phase"
      extends Trafo3IdealBase(redeclare replaceable record Data =
          PowerSystems.AC1ph_DC.Transformers.Parameters.Trafo3Stray1ph);
    protected
      SI.Resistance[3] R=par.r.*RL_base[:, 1];
      SI.Inductance[3] L=par.x.*RL_base[:, 2];
      annotation (
        Documentation(
        info="<html>
<p>Precalculation of coefficients for ideal magnetic coupling 3-winding transformer</p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-26,60},{-20,-60}},
              lineColor={215,215,215},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{20,60},{26,-60}},
              lineColor={215,215,215},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid)}));
    end Trafo3StrayBase;

  end Partials;

package Parameters "Parameter data for interactive use"
  extends Modelica.Icons.MaterialPropertiesPackage;

record TrafoIdeal1ph "Parameters for ideal transformer, 1-phase"
  extends Basic.Nominal.NominalDataTrafo;
  Integer[2] tap_neutral={0, 0} "{1,2}: neutral tap position"
    annotation(Dialog(group="Options"));
  SIpu.Voltage[2] dv_tap={0, 0} "{1,2}: delta-v per tap change"
    annotation(Dialog(group="Options"));
  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoIdeal1ph;

record TrafoStray1ph
      "Parameters for ideal magnetic coupling transformer, 1-phase"
  extends TrafoIdeal1ph;
  SIpu.Resistance[2] r={0.05,0.05} "{1,2}: resistance";
  SIpu.Reactance[2] x={0.05,0.05} "{1,2}: stray reactance";

  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoStray1ph;

record TrafoMag1ph "Parameters for magnetic coupling transformer, 1-phase"
  extends TrafoStray1ph;
  SIpu.Resistance redc=500 "resistance eddy current";
  SIpu.Reactance xm=500 "mutual reactance";

  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoMag1ph;

record TrafoSat1ph "Parameters for saturation transformer, 1-phase"
  extends TrafoMag1ph;
  Real psi_sat(unit="1")=1.5 "saturation flux";
  SIpu.Reactance xm_sat=1 "mutual reactance saturated";

  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoSat1ph;

record Trafo3Ideal1ph "Parameters for ideal transformer, 1-phase"
  Integer[3] tap_neutral={0, 0, 0} "{1,2a,2b}: neutral tap position"
    annotation(Dialog(group="Options"));
  SIpu.Voltage[3] dv_tap={0, 0, 0} "{1,2a,2b}: delta-v per tap change"
    annotation(Dialog(group="Options"));
  extends Basic.Nominal.NominalDataTrafo(V_nom={1,1,1}
    "{prim,sec_a,sec_b} nom Voltage (= base if pu)");
  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end Trafo3Ideal1ph;

record Trafo3Stray1ph
      "Parameters for ideal magnetic coupling transformer, 1-phase"
  extends Trafo3Ideal1ph;
  SIpu.Resistance[3] r={0.05,0.05,0.05} "{1,2a,2b}: resistance";
  SIpu.Reactance[3] x={0.05,0.05,0.05} "{1,2a,2b}: stray reactance";

  annotation (defaultComponentName="data",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end Trafo3Stray1ph;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"));
end Parameters;

annotation (preferredView="info",
    Documentation(info="<html>
<p>One-phase transformer models in different abstraction levels.</p>
</html>
"));
end Transformers;
