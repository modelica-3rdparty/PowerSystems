within PowerSystems.AC1ph_DC;
package Machines "DC-machines, electric part"
  extends Modelica.Icons.VariantsPackage;

  model EMF "Electro motoric force"
    extends Ports.Port_p;

    parameter Real k(final unit="N.m/A") = 1 "transformation coefficient";

    SI.AngularVelocity w "ang velocity rotor";
    Interfaces.Rotation_n airgap "electro-mechanical connection"
      annotation (                     layer="icon", Placement(transformation(
          origin={0,60},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  equation
    sum(term.i) = 0;

    w = der(airgap.phi);
    k*w = term.v[1] - term.v[2];
    airgap.tau = -k*term.i[1];
    annotation (defaultComponentName = "emf",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-60,60},{60,-60}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-40,40},{40,-40}},
            lineColor={135,135,135},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{26,20},{66,-20}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{26,20},{46,-20},{66,20},{26,20}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,18},{-60,18}}, color={0,0,255}),
          Line(points={{-70,-18},{34,-18},{14,18},{-30,18}}, color={0,0,255}),
          Text(
            extent={{-110,10},{90,-10}},
            lineColor={0,0,255},
            textString=
                 "emf")}),
      Documentation(
              info="<html>
<p>EMF transforms electrical power into rotational mechanical power without losses.
<pre>  P_mec = der(airgap.phi)*airgap.tau = -v*i = -P_el.</pre></p>
<p>The power is independent of the factor <tt>k</tt>. The connector 'airgap' transfers the rotor-torque to the mechanical system.</p>
</html>
"));
  end EMF;

  model DCser "DC machine series excited"
    extends Partials.DCserBase;

  initial equation
    if steadyIni_t then
      der(i) = 0;
    end if;

  equation
    if system.transientSim then
      c.L*der(i) + (w_el*c.L_md + sum(c.R))*i = v;
    else
      (w_el*c.L_md + sum(c.R))*i = v;
    end if;

    tau_el = i*(c.L_md*i);
    heat.ports.Q_flow = -c.R*i*i;
    annotation (defaultComponentName = "DCser",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The field (stator) winding and armature (rotor) winding are series-connected.<br>
Contains in general compensation and commutation poles.<br>
The parameter values l_q and r_q have to be interpreted differently for machines without and with
compensation and commutation poles.</p>
<p>&nbsp; - without:</p>
<pre>
  r_q = r_aq     resistance armature
  l_q = l_aq     inductance armature
</pre>
<p>&nbsp; - with:</p>
<pre>
  r_q = r_aq + r_fq                    resistance armature plus compensation poles
  l_q = (l_aq - l_mq) + (l_fq - l_mq)  stray inductance of armature plus
                                       stray inductance of compensation poles
</pre>
<p>For pu input refer to the nominal (electrical) angular machine velocity pp*w_nom and use </p>
<pre>  L_base = R_base/(pp*w_nom)</pre>
<p>Determination of the mutual inductance L_md.<br>
L_md depends on the winding ratio between armature and field winding</p>
<pre>  L_md = (L_fd - Lsig_fd)*(n_a/n_f)</pre>
<p>It can be determined in several ways,<br>
&nbsp; - using the above formula<br>
&nbsp; - using a constant K_E (back EMF)<br>
&nbsp; - solving the steady-state machine equations for L_md at nominal conditions. This method leads to</p>
<pre>  L_md=(V_nom/I_nom - (R_fd + R_q))/(pp*w_nom)</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,10},{100,-10}},
            lineColor={255,255,255},
            textString=
                 "ser")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      DymolaStoredErrors);
  end DCser;

  model DCpar "DC machine parallel excited"
    extends Partials.DCparBase;

  initial equation
    if steadyIni_t then
      der({i_f, i}) = {0,0};
    end if;

  equation
    if system.transientSim then
      diagonal(c.L)*der({i_f, i}) + {0, w_el*c.L_md*i_f} + diagonal(c.R)*{i_f, i} = {v_f, v};
    else
      {0, w_el*c.L_md*i_f} + diagonal(c.R)*{i_f, i} = {v_f, v};
    end if;

    tau_el = i*(c.L_md*i_f);
    heat.ports.Q_flow = -{c.R[1]*i_f*i_f, c.R[2]*i*i};
    annotation (defaultComponentName = "DCpar",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The field (stator) winding and armature (rotor) winding are parallel-connected
or the field winding has a separate excitation source.<br>
Contains in general compensation and commutation poles.<br>
The parameter values l_q and r_q have to be interpreted differently for machines without and with
compensation and commutation poles.</p>
<p>&nbsp; - without:</p>
<pre>
  r_q = r_aq     resistance armature
  l_q = l_aq     inductance armature
</pre>
<p>&nbsp; - with:</p>
<pre>
  r_q = r_aq + r_fq                    resistance armature plus compensation poles
  l_q = (l_aq - l_mq) + (l_fq - l_mq)  stray inductance of armature plus
                                       stray inductance of compensation poles
</pre></p>
<p>For pu input refer to the nominal (electrical) angular machine velocity pp*w_nom and use
<pre>  L_base = R_base/(pp*w_nom)</pre>
Use the same base-values also for excitation parameters.</p>
<p>Determination of the mutual inductance L_md.<br>
L_md depends on the winding ratio between armature and field winding
<pre>  L_md = (L_fd - Lsig_fd)*(n_a/n_f)</pre>
It can be determined in several ways,<br>
&nbsp; - using the above formula<br>
&nbsp; - using a constant K_E (back EMF)<br>
&nbsp; - solving the steady-state machine equations for L_md at nominal conditions. This method leads to</p>
<pre>  L_md = R_fd/w_el_lim)*(V_nom/Vf_nom)</pre>
<p>with</p>
<pre>
  V_nom       armature nominal voltage
  Vf_nom      field nominal voltage
  w_el_lim = pp*w_nom/(1 - R_q*I_nom/V_nom)  no-load angular velocity (electrical)
</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,10},{100,-10}},
            lineColor={255,255,255},
            textString=
                 "par")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end DCpar;

  model DCpm "DC machine permanent magnet excited"
    extends Partials.DCpmBase;

  initial equation
    if steadyIni_t then
      der(i) = 0;
    end if;

  equation
    if system.transientSim then
      c.L*der(i) + c.R*i = v - w_el*c.Psi_pm;
    else
      c.R*i = v - w_el*c.Psi_pm;
    end if;

    tau_el = i*c.Psi_pm;
    heat.ports.Q_flow = -{0, c.R*i*i};
    annotation (defaultComponentName = "DCpm",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>The field (stator) winding is replaced by a permanent magnet system, no compensation and commutation poles exist.</p>
<p>For pu input refer to the (electrical) angular machine velocity pp*w_nom and use</p>
<pre>  L_base = R_base/(pp*w_nom)</pre>
<p>The permanent magnet exciter flux Psi_pm can be determined either from the steady-state equation, leading to
<pre>
  Psi_pm = (1 - R*I_nom/V_nom)*Psi_nom
  Psi_nom = V_nom/(pp*w_nom)
</pre>
or from the induced armature voltage at nominal (compare with the synchronous machine).</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,10},{100,-10}},
            lineColor={255,255,255},
            textString=
                 "pm")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end DCpm;

  package Partials "Partial models"
     extends Modelica.Icons.BasesPackage;

    partial model DCBase "Base DC machine"
      extends Ports.Port_p;

      parameter Integer pp=2 "pole-pair nb";
      parameter Boolean stIni_en=true "enable steady-state initial equation"
                                                                           annotation(evaluate=true);
      parameter SI.Angle phi_el_ini=0 "initial rotor angle electric";
      parameter SI.AngularVelocity w_el_ini=0
        "initial rotor angular velocity el";
      SI.Angle phi_el(start=phi_el_ini, stateSelect=StateSelect.always)
        "rotor angle electric";
      SI.AngularVelocity w_el(start=w_el_ini, stateSelect=StateSelect.always)
        "rotor angular velocity el";
      SI.Torque tau_el "electromagnetic torque";
      SI.Voltage v "voltage";
      SI.Current i "current";

      Interfaces.Rotation_n airgap "electro-mechanical connection"
        annotation (                   layer="icon", Placement(transformation(
            origin={0,60},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Interfaces.ThermalV_n heat(     m=2) "heat source port {stator, rotor}"
        annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    protected
      outer System system;
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;

    initial equation
      phi_el = phi_el_ini;
      if system.steadyIni then
        der(w_el) = 0;
      else
        w_el = w_el_ini;
      end if;

    equation
      sum(term.i) = 0;
      v = term.v[1] - term.v[2];
      i = term.i[1];

       pp*airgap.phi = phi_el;
      airgap.tau = -pp*tau_el;
      w_el = der(phi_el);
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
<p>Contains the pole-pair transformation</p>
<pre>
  pp*airgap.phi = phi_el;
  airgap.tau = -pp*tau_el;
</pre>
<p>between the 'electrical' variables phi_el and tau_el and the 'mechanical' variables airgap.phi and airgap.tau.<br>
The connector 'airgap' transfers the electromagnetic rotor-torque to the mechanical system.</p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{90,90},{-90,-90}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{70,70},{-70,-70}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{50,50},{-50,-50}},
              lineColor={0,0,0},
              fillPattern=FillPattern.Sphere,
              fillColor={215,215,215}),
            Polygon(
              points={{-64,-10},{-59,10},{-54,-10},{-64,-10}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{55,10},{59,-10},{65,10},{55,10}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-50,-18},{-30,-22}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-18},{50,-22}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-40,40},{40,30}},
              lineColor={0,0,255},
              lineThickness=0.5,
              textString=
                   "stator (field)"),
            Text(
              extent={{-40,-30},{40,-40}},
              lineColor={0,0,255},
              lineThickness=0.5,
              textString=
                   "rotor (armature)"),
            Rectangle(
              extent={{-30,1},{50,-1}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid)}));
    end DCBase;

    partial model DCserBase "DC machine series excited, parameter"
      extends DCBase(final pp=par.pp);

      parameter Parameters.DCser par "machine parameter"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}},
              rotation=0)));
    protected
      final parameter Coefficients.DCser c = Basic.Precalculation.machineDCser(
                                                                              par);
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
            grid={2,2}), graphics),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-30,22},{50,18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,22},{-30,18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-50,20},{-70,20},{-70,4},{-80,4}}, color={0,0,255}),
            Line(points={{50,20},{60,20},{60,12},{-60,12},{-60,-20},{-50,-20}},
                color={0,0,255}),
            Line(points={{50,-20},{60,-20},{60,-28},{-70,-28},{-70,-4},{-80,-4}},
                color={0,0,255})}));
    end DCserBase;

    partial model DCparBase "DC machine parallel excited, parameter"
      extends DCBase(final pp=par.pp);

      parameter Parameters.DCpar par "machine parameter"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}},
              rotation=0)));
      SI.Voltage v_f;
      SI.Current i_f;
      Ports.TwoPin_p field
        annotation (Placement(transformation(extent={{-110,-50},{-90,-30}},
              rotation=0)));
    protected
      final parameter Coefficients.DCpar c = Basic.Precalculation.machineDCpar(
                                                                              par);

    equation
      sum(field.i) = 0;
      v_f = field.v[1] - field.v[2];
      i_f = field.i[1];
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
            grid={2,2}), graphics),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-30,22},{50,18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,22},{-30,18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,4},{-60,4},{-60,-20},{-50,-20}}, color={0,0,255}),
            Line(points={{-80,-4},{-70,-4},{-70,-28},{60,-28},{60,-20},{50,-20}},
                color={0,0,255}),
            Line(points={{-50,20},{-60,20},{-60,12},{72,12},{72,-36},{40,-36}},
                color={0,0,255}),
            Line(points={{50,20},{80,20},{80,-44},{-80,-44}}, color={0,0,255}),
            Line(points={{-40,-36},{-80,-36}}, color={0,0,255})}));
    end DCparBase;

    partial model DCpmBase "DC machine permanent magnet excited, parameter"
      extends DCBase(final pp=par.pp);

      parameter Parameters.DCpm par "machine parameter"
        annotation (Placement(transformation(extent={{-80,60},{-60,80}},
              rotation=0)));
    protected
      final parameter Coefficients.DCpm c = Basic.Precalculation.machineDCpm(
                                                                            par);
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
<p>Magnetic flux base for pu-choice is
<pre>  Psi_base = (1 - r_aq)*V_nom/omega_nom = (1 - r_aq)*V_nom/(pp*w_nom)</pre></p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-30,22},{50,18}},
              lineColor={176,0,0},
              fillColor={176,0,0},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,4},{-60,4},{-60,-20},{-50,-20}}, color={0,0,255}),
            Line(points={{50,-20},{60,-20},{60,-28},{-70,-28},{-70,-4},{-80,-4}},
                color={0,0,255})}));

    end DCpmBase;

    annotation (       Window(
  x=0.05,
  y=0.44,
  width=0.31,
  height=0.23,
  library=1,
  autolayout=1),
          Coordsys(
  extent=[-100, -100; 100, 100],
  grid=[2, 2],
  component=[20, 20]), Window(
  x=0.05,
  y=0.44,
  width=0.31,
  height=0.23,
  library=1,
  autolayout=1));
  end Partials;

package Parameters "Parameter data for interactive use"
  extends Modelica.Icons.MaterialPropertiesPackage;

record DCser "DC machine parameters series excited"
  extends Basic.Nominal.NominalDataDC(w_nom=157.079632679489661923);

  parameter Integer pp=2 "pole-pair nb";
  parameter SIpu.Inductance l_fd=0.15 "inductance field (d-axis)";
  parameter SIpu.Resistance r_fd=0.01 "resistance field (d-axis)";
  parameter SIpu.Inductance l_q=0.5 "inductance armature+ (q-axis)";
  parameter SIpu.Resistance r_q=0.05 "resistance armature+ (q-axis)";

  annotation (defaultComponentName="dc_serPar",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCser;

record DCpar "DC machine parameters parallel excited"
  extends Basic.Nominal.NominalDataDC(w_nom=157.079632679489661923);

  parameter SI.Voltage Vf_nom=1 "nom field voltage"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter Integer pp=2 "pole-pair nb";
  parameter SIpu.Inductance l_fd=100*pi "inductance field (d-axis)";
  parameter SIpu.Resistance r_fd=100 "resistance field (d-axis)";
  parameter SIpu.Inductance l_q=0.5 "inductance armature+ (q-axis)";
  parameter SIpu.Resistance r_q=0.05 "resistance armature+ (q-axis)";

  annotation (defaultComponentName="dc_parPar",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCpar;

record DCpm "DC machine parameters permanent magnet excited"
  extends Basic.Nominal.NominalDataDC(w_nom=157.079632679489661923);

  parameter Integer pp=2 "pole-pair nb";
  parameter SIpu.Inductance l_aq=0.5 "inductance armature (q-axis)";
  parameter SIpu.Resistance r_aq=0.05 "resistance armature (q-axis)";

  annotation (defaultComponentName="dc_pmPar",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCpm;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.38,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Parameters;

package Coefficients "Coefficient matrices of machine equations"
  extends Modelica.Icons.MaterialPropertiesPackage;

record DCser "Coefficients of DC machine series excited"
  extends Modelica.Icons.Record;

  final parameter SI.Inductance L "series inductance";
  final parameter SI.Resistance[2] R
        "resistance {d (field), q (armature)} axis";
  final parameter SI.Inductance L_md "mutual inductance";

  annotation (defaultComponentName="data",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCser;

record DCpar "Coefficients of DC machine parallel excited"
  extends Modelica.Icons.Record;

  final parameter SI.Inductance[2] L
        "inductance {d (field), q (armature)} axis";
  final parameter SI.Resistance[2] R
        "resistance {d (field), q (armature)} axis";
  final parameter SI.Inductance L_md "mutual inductance";

  annotation (defaultComponentName="data",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCpar;

record DCpm "Coefficients of DC machine permanent magnet excited"
  extends Modelica.Icons.Record;

  final parameter SI.Resistance R "resistance";
  final parameter SI.Inductance L "inductance";
  final parameter SI.MagneticFlux Psi_pm "flux permanent magnet";

  annotation (defaultComponentName="data",
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
            grid={2,2}), graphics),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
end DCpm;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.38,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Records containing the result of precalculation, and used in the dynamical equations of the corresponding components.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
end Coefficients;
  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.32,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>This package contains the <b>electrical part</b> (electrical equations) of DC machines.<br>
Complete drives are found in package Drives.</p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics),
    Diagram(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics={Line(
          points={{-110,-110},{90,90}},
          color={255,0,0},
          thickness=0.5)}));
end Machines;
