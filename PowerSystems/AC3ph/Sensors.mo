within PowerSystems.AC3ph;
package Sensors "Sensors and meters 3-phase"
  extends Modelica.Icons.SensorsPackage;

  model VnormSensor "Voltage-norm sensor, 3-phase dq0"
    extends Partials.Sensor1Base(final signalTrsf=0);

    parameter Integer n_eval(
      min=2,
      max=3) = 2 "dq- or dq0-norm" annotation(choices(
      choice=2 "2: dq-norm",
      choice=3 "3: dq0-norm"));
    Modelica.Blocks.Interfaces.RealOutput v "voltage norm, phase-to-ground"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    v = sqrt(term.v[1:n_eval]*term.v[1:n_eval]);
  annotation (defaultComponentName = "Vsensor1",
    Documentation(
            info="<html>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end VnormSensor;

  model InormSensor "Current-norm sensor, 3-phase dq0"
    extends Partials.Sensor2Base(final signalTrsf=0);

    parameter Integer n_eval(
      min=2,
      max=3) = 2 "dq- or dq0-norm" annotation(choices(
      choice=2 "2: dq-norm",
      choice=3 "3: dq0-norm"));
    Modelica.Blocks.Interfaces.RealOutput i "current norm, term_p to term_n"
                                       annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    i = sqrt(term_p.i[1:n_eval]*term_p.i[1:n_eval]);
  annotation (defaultComponentName = "Isensor1",
    Documentation(
            info="<html>
</html>
"));
  end InormSensor;

  model Vsensor "Voltage sensor, 3-phase dq0"
    extends Partials.Sensor1Base;

    Modelica.Blocks.Interfaces.RealOutput[3] v "voltage, phase-to-ground"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    if signalTrsf == 0 then
      v = term.v; // actual
    elseif signalTrsf == 1 then
      v = cat(1, transpose(rot_dq(term.theta[1]))*term.v[1:2], term.v[3:3]); // dq0
    elseif signalTrsf == 2 then
      v = cat(1, rot_dq(term.theta[2])*term.v[1:2], term.v[3:3]); // alpha-beta_o
    elseif signalTrsf == 3 then
      v = transpose(park(term.theta[2]))*term.v; // abc
    end if;
  annotation (defaultComponentName = "Vsensor1",
    Documentation(
            info="<html>
<p>The parameter 'signalTrsf' allows the choice of different reference systems for the output signal<br>
<pre>
  signalTrsf=0     voltage in actual ref frame
  signalTrsf=1     voltage in dq0 synchronous frame
  signalTrsf=2     voltage in alpha_beta_o frame
  signalTrsf=3     voltage in abc inertial frame
</pre>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-20,60},{20,80}}, color={135,135,135}),
          Line(points={{-20,50},{20,70}}, color={135,135,135}),
          Line(points={{-20,40},{20,60}}, color={135,135,135})}));
  end Vsensor;

  model Isensor "Current sensor, 3-phase dq0"
    extends Partials.Sensor2Base;

    Modelica.Blocks.Interfaces.RealOutput[3] i "current, term_p to term_n"              annotation (Placement(
          transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    if signalTrsf == 0 then
      i = term_p.i;
    elseif signalTrsf == 1 then // actual
      i = cat(1, transpose(rot_dq(term_p.theta[1]))*term_p.i[1:2], term_p.i[3:3]); // dq0
    elseif signalTrsf == 2 then
      i = cat(1, rot_dq(term_p.theta[2])*term_p.i[1:2], term_p.i[3:3]); // alpha-beta_o
    elseif signalTrsf == 3 then
      i = transpose(park(term_p.theta[2]))*term_p.i; // abc
    end if;
  annotation (defaultComponentName = "Isensor1",
    Documentation(
            info="<html>
<p>The parameter 'signalTrsf' allows the choice of different reference systems for the output signal<br>
<pre>
  signalTrsf=0     current in actual ref frame
  signalTrsf=1     current in dq0 synchronous frame
  signalTrsf=2     current in alpha_beta_o frame
  signalTrsf=3     current in abc inertial frame
</pre>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-20,60},{20,80}}, color={135,135,135}),
          Line(points={{-20,50},{20,70}}, color={135,135,135}),
          Line(points={{-20,40},{20,60}}, color={135,135,135})}));
  end Isensor;

  model Psensor "Power sensor, 3-phase dq0"
    extends Partials.Sensor2Base(final signalTrsf=0);

    Modelica.Blocks.Interfaces.RealOutput[3] p
      "{active, reactive, DC} power, term_p to term_n"
    annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    p = {term_p.v[1:2]*term_p.i[1:2], -j_dq0(term_p.v[1:2])*term_p.i[1:2], term_p.v[3]*term_p.i[3]};
  annotation (defaultComponentName = "Psensor1",
    Documentation(
            info="<html>
<p><i>Comment on the sign-definition of reactive power see</i> ACdq0.Sensors.</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid), Line(
            points={{0,0},{20,0}},
            color={0,100,100},
            thickness=0.5)}));
  end Psensor;

  model Vmeter "Voltage meter, 3-phase dq0"
    extends Partials.Meter1Base(final S_nom=1);

    output SIpu.Voltage[3] v(each stateSelect=StateSelect.never);
    output SIpu.Voltage[2] vpp(each stateSelect=StateSelect.never);

    output SIpu.Voltage[3] v_abc(each stateSelect=StateSelect.never)=transpose(Park)*v if abc;
    output SIpu.Voltage[3] vpp_abc(each stateSelect=StateSelect.never)=
      {v_abc[2],v_abc[3],v_abc[1]} - {v_abc[3],v_abc[1],v_abc[2]} if abc;

    output SIpu.Voltage v_norm(stateSelect=StateSelect.never)=sqrt(v*v) if phasor;
    output SI.Angle alpha_v(stateSelect=StateSelect.never)=atan2(Rot_dq[:,2]*v[1:2], Rot_dq[:,1]*v[1:2]) if phasor;
  protected
    final parameter SI.Voltage V_base=Basic.Precalculation.baseV(puUnits, V_nom);

  equation
    v = term.v/V_base;
    vpp = sqrt(3)*{v[2],-v[1]};
    annotation (defaultComponentName = "Vmeter1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-15,50},{15,64}}, color={135,135,135}),
          Line(points={{-15,40},{15,54}}, color={135,135,135}),
          Line(points={{-15,30},{15,44}}, color={135,135,135})}),
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals in alternative representations, both in SI-units or in 'pu'.<br>
As they use time-dependent coordinate transforms, use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables in the chosen reference system:</p>
<pre>
  v         voltage phase-to-ground
  vpp       voltage phase-to-phase
</pre>
<p>Optional output variables:</p>
<pre>
  v_abc     voltage phase-to-ground, abc-inertial system
  vpp_abc   voltage phase-to-phase,  abc-inertial system
  v_norm    norm(v)
  alpha_v   phase(v)
</pre>
</html>
"));
  end Vmeter;

  model Imeter "Current meter, 3-phase dq0"
    extends Partials.Meter2Base;

    output SIpu.Current[3] i(each stateSelect=StateSelect.never);

    output SIpu.Current[3] i_abc(each stateSelect=StateSelect.never)=transpose(Park)*i if abc;

    output SIpu.Current i_norm(stateSelect=StateSelect.never)=sqrt(i*i) if phasor;
    output SI.Angle alpha_i(stateSelect=StateSelect.never)=atan2(Rot_dq[:,2]*i[1:2], Rot_dq[:,1]*i[1:2]) if phasor;
  protected
    final parameter SI.Current I_base=Basic.Precalculation.baseI(puUnits, V_nom, S_nom);

  equation
    i = term_p.i/I_base;
    annotation (defaultComponentName = "Imeter1",
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals in alternative representations, both in SI-units or in 'pu'.<br>
As they use time-dependent coordinate transforms, use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables in the chosen reference system:</p>
<pre>  i          current term_p to term_n</pre>
<p>Optional output variables:</p>
<pre>
  i_abc      current term_p to term_n, abc-inertial system
  i_norm     norm(i)
  alpha_i    phase(i)
</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-15,50},{15,64}}, color={135,135,135}),
          Line(points={{-15,40},{15,54}}, color={135,135,135}),
          Line(points={{-15,30},{15,44}}, color={135,135,135})}));
  end Imeter;

  model Pmeter "Power meter, 3-phase dq0"

    parameter Boolean av=false "time average power"  annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
                                                    annotation(Evaluate=true, Dialog(group="Options",enable=av));
    extends Partials.Meter2Base(final V_nom=1, final abc=false, final phasor=false);

    output SIpu.Power[3] p(each stateSelect=StateSelect.never);
    output SIpu.Power[3] p_av=pav if av;
  protected
    outer System system;
    final parameter SI.ApparentPower S_base=Basic.Precalculation.baseS(puUnits, S_nom);
    SIpu.Power[3] pav;

  initial equation
    if av then
      pav = p;
    end if;

  equation
    p = {term_p.v[1:2]*term_p.i[1:2], -j_dq0(term_p.v[1:2])*term_p.i[1:2], term_p.v[3]*term_p.i[3]}/S_base;
    if av then
      der(pav) = (p - pav)/tcst;
    else
      pav = zeros(3);
    end if;
    annotation (defaultComponentName = "Pmeter1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(
            points={{0,0},{20,0}},
            color={0,100,100},
            thickness=0.5),
          Ellipse(extent={{-70,70},{70,-70}}, lineColor={135,135,135})}),
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals in alternative representations, both in SI-units or in 'pu'.<br>
Use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables:</p>
<pre>  p         {AC active, AC reactive, DC} power term_p to term_n</pre>
<p>Optional output variables:</p>
<pre>  p_av       power term_p to term_n, time tau average of p</pre>
<p><i>Comment on the sign-definition of reactive power see</i> ACdq0.Sensors.</p>
</html>
"));
  end Pmeter;

  model PVImeter "Power-voltage-current meter, 3-phase dq0"
    extends Partials.Meter2Base;

    parameter Boolean av=false "time average power"  annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
                                                    annotation(Evaluate=true, Dialog(group="Options",enable=av));

    function v2vpp_abc
      input SIpu.Voltage[3] v_abc;
      output SIpu.Voltage[3] vpp_abc;
    algorithm
      vpp_abc := {v_abc[2],v_abc[3],v_abc[1]} - {v_abc[3],v_abc[1],v_abc[2]};
    end v2vpp_abc;

    output SIpu.Power[3] p(each stateSelect=StateSelect.never);
    output SIpu.Power[3] p_av=pav if av;
    output SIpu.Voltage[3] v(each stateSelect=StateSelect.never);
    output SIpu.Voltage[2] vpp(each stateSelect=StateSelect.never);
    output SIpu.Current[3] i(each stateSelect=StateSelect.never);

    output SIpu.Voltage[3] v_abc(each stateSelect=StateSelect.never)=transpose(Park)*v if abc;
    output SIpu.Voltage[3] vpp_abc(each stateSelect=StateSelect.never)=v2vpp_abc(transpose(Park)*v) if abc;
    output SIpu.Current[3] i_abc(each stateSelect=StateSelect.never)=transpose(Park)*i if abc;

    output SIpu.Voltage v_norm(stateSelect=StateSelect.never)=sqrt(v*v) if phasor;
    output SI.Angle alpha_v(stateSelect=StateSelect.never);
    output SIpu.Current i_norm(stateSelect=StateSelect.never)=sqrt(i*i) if phasor;
    output SI.Angle alpha_i(stateSelect=StateSelect.never);
    output Real cos_phi(stateSelect=StateSelect.never)=cos(alpha_v - alpha_i) if phasor;
  protected
    outer System system;
    final parameter SI.Voltage V_base=Basic.Precalculation.baseV(puUnits, V_nom);
    final parameter SI.Current I_base=Basic.Precalculation.baseI(puUnits, V_nom, S_nom);
    SIpu.Power[3] pav;

  initial equation
    if av then
      pav = p;
    end if;

  equation
    v = term_p.v/V_base;
    vpp = sqrt(3)*{v[2],-v[1]};
    i = term_p.i/I_base;
    p = {v[1:2]*i[1:2], -j_dq0(v[1:2])*i[1:2], v[3]*i[3]};
    if av then
      der(pav) = (p - pav)/tcst;
    else
      pav = zeros(3);
    end if;
    if phasor then
      alpha_v = atan2(Rot_dq[:,2]*v[1:2], Rot_dq[:,1]*v[1:2]);
      alpha_i = atan2(Rot_dq[:,2]*i[1:2], Rot_dq[:,1]*i[1:2]);
    else
      alpha_v = 0;
      alpha_i = 0;
    end if;
    annotation (defaultComponentName = "PVImeter1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-20,24},{20,20}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-8,8},{8,-8}},
            lineColor={135,135,135},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(
            points={{0,0},{20,0}},
            color={0,100,100},
            thickness=0.5),
          Line(points={{-15,50},{15,64}}, color={135,135,135}),
          Line(points={{-15,40},{15,54}}, color={135,135,135}),
          Line(points={{-15,30},{15,44}}, color={135,135,135})}),
      Documentation(
              info="<html>
<p>'Meters' are intended as diagnostic instruments. They allow displaying signals in alternative representations, both in SI-units or in 'pu'.<br>
As they use time-dependent coordinate transforms, use them only when and where needed. Otherwise use 'Sensors'.</p>
<p>Output variables in the chosen reference system:</p>
<pre>
  p         {AC active, AC reactive, DC} power term_p to term_n
  v          voltage phase-to-ground dq0
  vpp        voltage phase-to-phase dq
  i          current dq0, term_p to term_n
</pre>
<p>Optional output variables:</p>
<pre>
  p_av       power term_p to term_n, time tau average of p
  v_abc      voltage phase-to-ground,  abc-inertial system
  vpp_abc    voltage phase-to-phase,   abc-inertial system
  i_abc      current term_p to term_n, abc-inertial system
  v_norm     norm(v)
  i_norm     norm(i)
  alpha_v    phase(v)
  alpha_i    phase(i)
  cos_phi    cos(alpha_v - alpha_i)
</pre>
<p><i>Comment on the sign-definition of reactive power see</i> ACdq0.Sensors.</p>
</html>
"));
  end PVImeter;

  model Efficiency "Power sensor, 3-phase dq0"
    extends Partials.Sensor2Base(final signalTrsf=0);

    Interfaces.ThermalV_p heat(     m=m) "vector heat port"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Boolean dir_in=true "direction" annotation(Evaluate=true, choices(
      choice=true "points into the component",
      choice=false "point out of the component"));
    parameter Integer m(final min=1)=1 "dimension of heat port";
    parameter Boolean av=false "time average efficiency" annotation(Evaluate=true,Dialog(group="Options"));
    parameter SI.Time tcst(min=1e-9)=1 "average time-constant"
      annotation(Evaluate=true, Dialog(group="Options",enable=av));
    parameter SI.Temperature T_amb=300 "ambient temperature";
    output Real eta "efficiency";
  protected
    SI.Power p "total el power, term_p to term_n";
    SI.HeatFlowRate q "total heat flow 'in'";
    SI.Power pav;
    SI.HeatFlowRate qav;

  initial equation
    if av then
      pav = p;
      qav = q;
    end if;

  equation
    heat.ports.T = fill(T_amb, heat.m);
    p = term_p.v*term_p.i;
    q = sum(heat.ports.Q_flow);

    if av then
      der(pav) = (p - pav)/tcst;
      der(qav) = (q - qav)/tcst;
    else
      pav = p;
      qav = q;
    end if;

    if qav < abs(pav) then
      if dir_in then
        eta = if pav > 0 then 100*(pav - qav)/pav else -100*pav/(pav - qav);
      else
        eta = if pav > 0 then 100*pav/(pav + qav) else -100*(pav + qav)/pav;
      end if;
    else
      eta = 0;
    end if;
  annotation (defaultComponentName = "efficiency",
    Documentation(
            info="<html>
<p>Measures the electric power <tt>p</tt> flowing from 'term_p' to 'term_n' and the total heat inflow <tt>q</tt> at term 'heat'. The efficiency eta in % is then defined by
<pre>
  eta = 100*(p - q)/p     if arrow points into the measured component and q &lt  abs(p)
  eta = 100*p/(p + q)     if arrow points out of the measured component and q &lt  abs(p)
  eta = 0                 else
</pre>
Positive values of eta indicate powerflow in direction of arrow,
negative values of eta indicate powerflow against direction of arrow.</p>
<p>Note: Take care about the above definitions if approximations are used in measured components.<br>
In problematic cases use power sensors electrical and mechanical.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-20,20},{20,-20}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid), Line(
            points={{0,0},{20,0}},
            color={0,100,100},
            thickness=0.5)}));
  end Efficiency;

  model Phasor "Visualiser of voltage and current phasor, 3-phase dq0"
    extends Partials.PhasorBase;

    Basic.Types.Color     color_p;
    Basic.Types.Color     color_n;
    Basic.Visualise.Bar     activePower(
                                   color={0,127,127}, x=x_norm*abs(p[1]))
    annotation (Placement(transformation(extent={{-104,-100},{-94,
              100}})));
    Basic.Visualise.Bar     reactivePower(
                                     color={127,0,127}, x=x_norm*abs(p[2]))
    annotation (Placement(transformation(extent={{94,-100},{104,
              100}})));
    Basic.Visualise.DoubleNeedle     voltage_current(
      color1={255,0,0},
      color2={0,0,255},
      x1=r_norm*v_dq[1],
      y1=r_norm*v_dq[2],
      x2=r_norm*i_dq[1],
      y2=r_norm*i_dq[2])
    annotation (Placement(transformation(extent={{-100,-100},{100,
              100}})));

  equation
    color_p = if p[1]>0 then {0,127,127} else {215,215,215};
    color_n = if p[1]<0 then {0,127,127} else {215,215,215};
  annotation (
    defaultComponentName="phasor",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(points={{4,100},{84,100},{54,88}}, color=
                {128,128,128}), Line(points={{-4,100},{-84,100},{-54,88}},
              color={128,128,128})}),
      Documentation(
        info="<html>
<p>Phase representation of voltage and current in 3-phase networks:</p>
<pre>
  red needle      voltage
  blue needle     current
</pre>
<p>(The black circle indicates 1 pu).</p>
<p>Additional bars for power flow:</p>
<pre>
  green left bar        active power
  violet right bar      reactive power
  green arrow           direction of active power flow
</pre>
<p>(The black marks indicate 1 pu).</p>
<p><i>Select 'Diagram' in the Simulation layer, when simulating with this component.</i></p>
</html>
"));
  end Phasor;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model Sensor1Base "Sensor 1 terminal base, 3-phase dq0"
      extends Ports.Port_p;

      parameter Integer signalTrsf=0 "signal in which reference frame?"
       annotation(Evaluate=true,Dialog(group="Options"), choices(
         choice=0 "0: actual ref frame",
         choice=1 "1: dq0 synchronous",
         choice=2 "2: alpha_beta_o",
         choice=3 "3: abc inertial"));
    protected
      function park = Basic.Transforms.park;
      function rot_dq = Basic.Transforms.rotation_dq;

    equation
      term.i = zeros(3);
    annotation (
      Documentation(
            info="<html>
</html>"),
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,70},{70,-70}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(
              points={{-90,0},{40,0}},
              color={0,100,100},
              thickness=0.5),
            Line(points={{0,20},{0,90}}, color={135,135,135})}));
    end Sensor1Base;

    partial model Sensor2Base "Sensor 2 terminal base, 3-phase dq0"
      extends Ports.Port_pn;

      parameter Integer signalTrsf=0 "signal in which reference frame?"
       annotation(Evaluate=true,Dialog(group="Options"), choices(
         choice=0 "0: actual ref frame",
         choice=1 "1: dq0 synchronous",
         choice=2 "2: alpha_beta_o",
         choice=3 "3: abc inertial"));
    protected
      function park = Basic.Transforms.park;
      function rot_dq = Basic.Transforms.rotation_dq;

    equation
      term_p.v = term_n.v;
    annotation (
      Documentation(
            info="<html>
</html>"),
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,70},{70,-70}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{0,20},{0,90}}, color={135,135,135}),
            Line(
              points={{-90,0},{-20,0}},
              color={0,100,100},
              thickness=0.5),
            Line(
              points={{0,0},{90,0}},
              color={0,100,100},
              thickness=0.5),
            Line(
              points={{30,20},{70,0},{30,-20}},
              color={0,100,100},
              thickness=0.5),
            Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135})}));
    end Sensor2Base;

    partial model Meter1Base "Meter 1 terminal base, 3-phase dq0"
      extends Sensor1Base(final signalTrsf=0);

      parameter Boolean abc=false "abc inertial"
        annotation(Evaluate=true,Dialog(group="Options"));
      parameter Boolean phasor=false "phasor"
        annotation(Evaluate=true,Dialog(group="Options"));
      extends Basic.Nominal.Nominal;
    protected
      Real[3,3] Park;
      Real[2,2] Rot_dq;
      function atan2 = Modelica.Math.atan2;
    equation
      if abc then
        Park = park(term.theta[2]);
      else
        Park = zeros(3,3);
      end if;
      if phasor then
        Rot_dq = rot_dq(term.theta[1]);
      else
        Rot_dq = zeros(2,2);
      end if;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(extent={{-70,70},{70,-70}},
                lineColor={135,135,135})}));
    end Meter1Base;

    partial model Meter2Base "Meter 2 terminal base, 3-phase dq0"
      extends Sensor2Base(final signalTrsf=0);

      parameter Boolean abc=false "abc inertial"
        annotation(Evaluate=true,Dialog(group="Options"));
      parameter Boolean phasor=false "phasor"
        annotation(Evaluate=true,Dialog(group="Options"));
      extends Basic.Nominal.Nominal;
    protected
      Real[3,3] Park;
      Real[2,2] Rot_dq;
      function atan2 = Modelica.Math.atan2;
    equation
      if abc then
        Park = park(term_p.theta[2]);
      else
        Park = zeros(3,3);
      end if;
      if phasor then
        Rot_dq = rot_dq(term_p.theta[1]);
      else
        Rot_dq = zeros(2,2);
      end if;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Ellipse(extent={{-70,70},{70,-70}},
                lineColor={135,135,135})}));
    end Meter2Base;

  partial model PhasorBase "Phasor base, 3-phase dq0"
    extends Ports.Port_pn;
    extends Basic.Nominal.Nominal;

    Real[2] v_dq;
    Real[2] i_dq;
    Real[2] p;
    protected
    constant Real r_norm(unit="1")=0.8 "norm radius phasor";
    constant Real x_norm(unit="1")=0.8 "norm amplitude power";
    final parameter SI.Voltage V_base=Basic.Precalculation.baseV(puUnits, V_nom);
    final parameter SI.Current I_base=Basic.Precalculation.baseI(puUnits, V_nom, S_nom);
    Real[2,2] Rot_dq = Basic.Transforms.rotation_dq(
                                                   term_p.theta[1]);

  equation
    term_p.v = term_n.v;
    v_dq = transpose(Rot_dq)*term_p.v[1:2]/V_base;
    i_dq = transpose(Rot_dq)*term_p.i[1:2]/I_base;
    p = {v_dq*i_dq, -j_dq0(v_dq)*i_dq};
    annotation (
      Documentation(
      info="<html>
</html>"),
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,100},{100,-100}},
              lineColor={215,215,215},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-90,90},{90,-90}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-80,80},{80,-80}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-2,2},{2,-2}},
              lineColor={95,95,95},
              fillColor={95,95,95},
              fillPattern=FillPattern.Solid),
            Line(
              points={{-90,0},{90,0}},
              color={135,135,135},
              pattern=LinePattern.Dot),
            Line(
              points={{-64,-64},{64,64}},
              color={135,135,135},
              pattern=LinePattern.Dot),
            Line(
              points={{0,-90},{0,90}},
              color={135,135,135},
              pattern=LinePattern.Dot),
            Line(
              points={{-64,64},{64,-64}},
              color={135,135,135},
              pattern=LinePattern.Dot),
            Line(
              points={{-94,60},{-84,60}},
              color={0,0,0},
              thickness=0.5),
            Line(
              points={{84,60},{94,60}},
              color={0,0,0},
              thickness=0.5),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
             "%name")}));
  end PhasorBase;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Sensors output terminal signals (voltage, current, power) in a defined reference system chosen by the user.</p>
<p>Meters allow choosing base-units for output variables.</p>
<p><i>Comment on the sign-definition of reactive power:</i></p>
<p>From a mathematical point of view, it would be desirable to define power in the following way:
<pre>
  p_active = v*i
  p_reactive = (J*v)*i
</pre>
<p>with</p>
<pre>  J = [0,-1,0; 1,0,0; 0,0,0]</pre>
<p>the rotation of pi/2 in the positive sense.</p>
<p>This definition keeps all coordinate systems positively oriented.
The power-vector then can be interpreted as current-vector, normalised by voltage and transformed into a positively oriented coordinate system, whose first axis is given by the voltage vector <tt>v</tt>, and second axis by <tt>J*v</tt>.</p>
<p>From a practical point of view it is more convenient to use the inverse sign for reactive power, in order to obtain positive reactive power in the standard-situation of power-transfer
across an inductive line.
We adapt the sign-definition to this practical convention:</p>
<pre>  p_reactive = -(J*v)*i</pre>
</html>
"));
end Sensors;
