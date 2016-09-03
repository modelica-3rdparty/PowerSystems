within PowerSystems.Mechanics;
package Rotation "Rotating parts "
  extends Modelica.Icons.VariantsPackage;

  package Ports "One- and two-flange base for rotating mechanical components."
  extends Modelica.Icons.BasesPackage;

  partial model Flange_p "One flange, 'positive'"

    Interfaces.Rotation_p flange "positive flange"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    annotation (
  Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
            graphics={Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              textString="%name")}),
  Documentation(info="<html>
</html>"));
  end Flange_p;

  partial model Flange_n "One flange, 'negative'"

    Interfaces.Rotation_n flange "negative flange"
  annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    annotation (
  Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
            graphics={Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              textString="%name")}),
  Documentation(info="<html>
</html>"));
  end Flange_n;

  partial model Flange_p_n "Two flange"

    Interfaces.Rotation_p flange_p "positive flange"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Interfaces.Rotation_n flange_n "negative flange"
  annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    annotation (
  Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
            graphics={Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              textString="%name")}),
  Documentation(info="<html>
</html>"));
  end Flange_p_n;

  partial model Rigid "Rigid two-flange"
    extends Flange_p_n;

  equation
    flange_p.phi = flange_n.phi;
    annotation (
  Documentation(info="<html>
</html>"));
  end Rigid;

  partial model Compliant "Compliant two-flange"
    extends Flange_p_n;

    SI.Angle d_phi "difference angle (twist)";
    SI.Torque d_tau "twisting torque";

  equation
    flange_n.phi - flange_p.phi = d_phi;
    flange_n.tau - flange_p.tau = 2*d_tau;
    annotation (
  Documentation(info="<html>
</html>"));
  end Compliant;

    annotation (
      preferredView="info",
  Documentation(info="<html>
<p>Contains mechanical one and two-ports with rotational connectors.</p>
</html>"));
  end Ports;

  model Speed "Rotation with given angular velocity"
    extends Ports.Flange_n;

    parameter SI.Time tcst(min=1e-9)=0.1 "time-constant";

    parameter Boolean use_w_in = false
      "= true if speed defined by input w_in, otherwise by parameter w0"
     annotation(Evaluate=true, choices(checkBox=true));

    parameter SI.AngularVelocity w0=1 "angular velocity"
     annotation(Dialog(enable=not use_w_in));
    Modelica.Blocks.Interfaces.RealInput w_in(final unit="rad/s") if use_w_in
      "(signal ang-velocity)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
  protected
    SI.AngularVelocity phi_dot;
    Modelica.Blocks.Interfaces.RealInput w_internal
      "Needed to connect to conditional connector";

  equation
    connect(w_in, w_internal);
    if not use_w_in then
       w_internal = w0;
    end if;

    der(flange.phi) = phi_dot;
    der(phi_dot) = (w_internal - phi_dot)/tcst;
    annotation(defaultComponentName = "speed1",
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={
          Ellipse(
            extent={{-60,60},{60,-60}},
            lineColor={0,0,0},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-40,40},{40,-40}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{26,20},{66,-20}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{26,20},{46,-20},{66,20},{26,20}},
            lineColor={0,0,0},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}),
    Diagram(graphics={
          Text(
            extent={{-50,10},{50,-10}},
            lineColor={0,0,127},
            textString=
                 "signal-angular-velocity w"),
          Text(
            extent={{-70,70},{70,50}},
            lineColor={0,0,127},
            textString=
                 "parameter-angular-velocity w0"),
          Line(points={{-90,0},{-60,0}}, color={0,0,127}),
          Text(
            extent={{-20,40},{20,20}},
            lineColor={0,0,127},
            textString=
               "or")}),
    Documentation(
            info="<html>
<p>'flange' moves with parameter-ang-velocity w0 or with signal angular-velocity w, depending on 'scType'.<br>
This is a \"soft\" speed, using a differential equation. It is needed for compatibility with the default initial equations used in the machine models.<br>
The start value is always given by <tt>w0</tt>.</p>
</html>"));
  end Speed;

  model Torque "Driving torque"
    extends Ports.Flange_n;

    parameter Boolean use_tau_in = false
      "= true if torque defined by input tau_in, otherwise by parameter tau0"
     annotation(Evaluate=true, choices(checkBox=true));

     parameter SI.Torque tau0=1 "torque"
     annotation(Dialog(enable=not use_tau_in));
    Modelica.Blocks.Interfaces.RealInput tau_in(final unit="N.m") if use_tau_in
      "(signal torque)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
  protected
    Modelica.Blocks.Interfaces.RealInput tau_internal
      "Needed to connect to conditional connector";

  equation
    connect(tau_in, tau_internal);
    if not use_tau_in then
       tau_internal = tau0;
    end if;

    flange.tau = -tau_internal;
  annotation (defaultComponentName = "torq1",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-60,60},{60,-60}},
            lineColor={0,0,0},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-40,40},{40,-40}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{26,20},{66,-20}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{26,20},{46,-20},{66,20},{26,20}},
            lineColor={0,0,0},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-50,10},{50,-10}},
            lineColor={0,0,127},
            textString=
                 "signal-torque tau"),
          Text(
            extent={{-70,70},{70,50}},
            lineColor={0,0,127},
            textString=
                 "parameter-torque tau0"),
          Line(points={{-90,0},{-60,0}}, color={0,0,127}),
          Text(
            extent={{-20,40},{20,20}},
            lineColor={0,0,127},
            textString=
               "or")}),
    Documentation(
            info="<html>
<p>Torque <tt>tau</tt> acts in positive direction on the connected component if <tt>tau > 0</tt>.</p>
</html>
"));
  end Torque;

model TabTimeTorque "Torque using table (time... torque)"
  extends Partials.TabTorque;

  parameter SI.Time t_unit=1 "unit of 'time' in tab";
  parameter SI.Torque tau_unit=1 "unit of 'torque' in tab";
  parameter Real[2] t_bd(unit="s")={0,1} "{first, last} time in tab";
  parameter Integer drive_load=1 "driving or load"
    annotation(choices(
    choice=1 "driving torque (+1)", choice=-1 "load torque (-1)"));
  parameter Integer direction(min=-1,max=1)=1 "forward or backward in time"
    annotation(choices(
    choice=1 "t_first --> t_last", choice=-1 "t_first <-- t_last"));
  parameter Boolean scale=false "scale time and torque";
  parameter SI.Time T=1 "scale duration to T"
    annotation(Dialog(enable=scale));
  parameter Types.Percent tau_perc=
                                  100 "scale tab torque to tau_perc"
    annotation(Dialog(enable=scale));
  protected
  SI.Time t;
  final parameter SI.Time t_factor=if scale then T/abs(t_bd[2]-t_bd[1]) else t_unit;
  final parameter SI.Torque tau_factor=if scale then drive_load*0.01*tau_perc*tau_unit else drive_load*tau_unit;

equation
  if direction == 1 then
    t = t_factor*t_bd[1] + time;
  elseif direction == -1 then
    t = t_factor*t_bd[2] - time;
  else
    t = 0;
  end if;
  table.u = t/t_factor;
  tau = tau_factor*table.y[1];

  when t > t_factor*t_bd[2] or t < t_factor*t_bd[1] then
    terminate("BOUNDARY TIME REACHED!");
  end when;
  annotation (defaultComponentName = "tabTorq1",
    Documentation(
            info="<html>
<p>The torque is defined in a table as a function of time.
<pre>
  Column 1 contains the time in units <tt>t_unit</tt> (<tt>t_unit</tt> in \"s\").
  Column 'colData' contains the torque in units <tt>tau_unit</tt> (<tt>tau_unit</tt> in \"N.m\").
</pre></p>
<p>Both time and torque may be linearly scaled by a factor if 'scale = true'.</p>
<p>Flange_p and flange_n are rigidly connected. The torque acts on the connected component(s) in
<pre>
positive direction, if tau_table &gt  0 and drive_load = +1 or tau_table &lt  0 and drive_load = -1
negative direction, if tau_table &gt  0 and drive_load = -1 or tau_table &lt  0 and drive_load = +1
</pre></p>
<p>Note: start integration at time = 0</p>
</html>"));
end TabTimeTorque;

model TabPosSlopeTorque "Torque using table (position... slope)"
  extends Partials.TabTorque;

  constant SI.Force g_n=Modelica.Constants.g_n;
  parameter SI.Length s_unit=1 "unit of 'position' in tab";
  parameter Real[2] s_bd(each unit="m")={0,1} "{first, last} position in tab";
  parameter Integer dirTrack(min=-1,max=1)=+1 "forward or backward track"
    annotation(choices(
    choice=1 "first-pos ---> last-pos", choice=-1 "first-pos <--- last-pos"));
  parameter Integer dirVehicle(min=-1,max=1)=+1
      "vehicle forward or backward driving"
    annotation(choices(
    choice=1 "forward", choice=-1 "backward"));
  parameter Boolean scale=false "scale position and slope";
  parameter SI.Length D=1 "scale distance to D" annotation(Dialog(enable=scale));
  parameter Types.Percent slope_perc=100 "scale slope to slope_perc"
                                                             annotation(Dialog(enable=scale));
  parameter SI.Mass mass=1 "mass";
  parameter Real[2] cFrict(each min=0)={0,0}
      "friction cst {lin, quadr} (translation) in {[N.s/m], [N.s2/m2]}";
  parameter SI.Length r=1 "radius wheel";
  parameter Real gRatio=1 "gear-ratio";
  SI.Length s;
  SI.Velocity vVehicle(start=0) "vehicle horizontal velocity";
  protected
  final parameter Real s_factor=if scale then D/abs(s_bd[2]-s_bd[1]) else s_unit;
  final parameter Real slope_factor=if scale then 0.01*slope_perc else 1;
  final parameter Integer sig=dirTrack*dirVehicle;
  Real slope;
  Real sin_gam;
  SI.Angle phi;
  SI.Force f;
  constant Real cFrictUnit1(unit="N.s/m") = 1    annotation(HideResult=true);
  constant Real cFrictUnit2(unit="N.s2/m2") = 1    annotation(HideResult=true);

initial equation
  s = if dirTrack == 1 then s_factor*s_bd[1] else s_factor*s_bd[2];

equation
  phi = flange_p.phi;
  gRatio*vVehicle = r*der(phi);
  r*f = gRatio*tau;
  table.u = s/s_factor;
  slope = slope_factor*table.y[1];
  sin_gam = slope/sqrt(1 + slope*slope); // = sin(atan(slope))
  der(s) = sig*vVehicle;
  mass*der(vVehicle) = -(f + sig*mass*g_n*sin_gam +
         (cFrict[1]*cFrictUnit1 + cFrict[2]*cFrictUnit2*abs(vVehicle))*vVehicle);

  when s > s_factor*s_bd[2] or s < s_factor*s_bd[1] then
    terminate("BOUNDARY POSITION REACHED!");
  end when;
  annotation (defaultComponentName = "tabTorq1",
    Documentation(
            info="<html>
<p>This model uses a position-slope table. It is mainly intended for test-purposes.</p>
<pre>
  Column 1 contains the (horizontal) position in units <tt>s_unit</tt> (<tt>s_unit</tt> in \"m\").
  Column 'colData' contains the slope dh/ds of a height profile in dimensionless units.
</pre></p>
<p>Both position and slope may be linearly scaled by a factor if 'scale = true' (for example using a normalised height-profile).</p>
<p>The torque load <tt>tau</tt> is related to a driving force <tt>f</tt> and a parameter (wheel-) radius <tt>r</tt> by
<pre>  tau = f/r</pre>
The force as a function of position <tt>s</tt> corresponds to a mass moving along a height profile with linear and quadratic friction. Flange_p and flange_n are rigidly connected.</p>
<p>Note: If the height h is also needed, it has to be scaled with the factor (slope_perc/100)*D.<br>
Start integration at time = 0.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(points={{-38,190},{-38,190}}, lineColor={255,250,110}),
          Polygon(
            points={{-40,110},{-60,90},{60,110},{40,123},{-40,110}},
            lineColor={95,95,95},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-60,80},{-80,60},{40,80},{60,100},{-60,80}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-60,90},{-60,80},{60,100},{60,110},{-60,90}},
            lineColor={255,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
end TabPosSlopeTorque;

  model FrictionTorque "Friction torque"
    extends Ports.Flange_p;

     parameter Real[2] cFrict(each min=0)={0,0}
      "friction cst {lin, quadr} in {[N.s/m], [N.s2/m2]}";
    SI.Angle phi;
    SI.AngularVelocity w;

  protected
    constant Real cFrictUnit1(unit="N.s/m") = 1    annotation(HideResult=true);
    constant Real cFrictUnit2(unit="N.s2/m2") = 1    annotation(HideResult=true);
  equation
    phi = flange.phi;
    w = der(phi);
    flange.tau = (cFrict[1]*cFrictUnit1 + cFrict[2]*cFrictUnit2*noEvent(abs(w)))*w;
  annotation (defaultComponentName = "frictTorq1",
    Documentation(
            info="<html>
<p>Linear and quadratic friction torque <tt>tau</tt>.</p>
<pre>
  tau = c_frict[1]*w + c_frict[2]*abs(w)*w
  w     angular velocity
</pre>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(points={{-38,20},{-38,20}}, lineColor={255,250,110}),
          Ellipse(
            extent={{-60,60},{60,-60}},
            lineColor={135,135,135},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Line(points={{-80,0},{-60,0}}, color={95,95,95}),
          Ellipse(
            extent={{-50,50},{50,-50}},
            lineColor={255,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-44,44},{44,-43}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid)}));
  end FrictionTorque;

model FixedAngle "Flange at fixed angular position"
  parameter SI.Angle phi0=0 "angle";

   Interfaces.Rotation_p flange  annotation (Placement(transformation(extent={{
              -10,-10},{10,10}})));

equation
  flange.phi = phi0;
  annotation (defaultComponentName = "fixAng1",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{60,-40},{-80,-40},{20,60}}, color={0,0,0}),
          Polygon(
            points={{-80,-40},{-60,-20},{-52,-40},{-80,-40}},
            lineColor={175,175,175},
            fillColor={255,170,170},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name")}),
    Documentation(info="<html>
<p>Fixes the angular variable <tt>phi</tt> of a connected flange to a parameter value <tt>phi0</tt>.</p>
</html>
"));
end FixedAngle;

model Rotor "Rigid rotating mass"
  extends Partials.RigidRotorBase;

equation
  J*a = flange_p.tau + flange_n.tau;
  annotation (defaultComponentName = "rotor1",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,50},{60,-50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{60,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-100,10},{-60,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215})}),
    Documentation(
            info="<html>
<p>Rotating rigid mass with two flanges. </p>
</html>
"));
end Rotor;

  model ThermalTurbineRotor "Thermal turbine rotor"
    extends Partials.RigidRotorCase;

    annotation (defaultComponentName = "turbRotor1",
      Documentation(
              info="<html>
<p>Turbine-rotor as one single rigid mass</p>
<pre>
  flange_p, flange_n:  connectors to other rotating parts
                       of the turbo-generator group
</pre>
<p><i>
No pole pair reduction of equations of motion.<br>
phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{-100,30},{100,70},{100,-70},{-100,-30},{-100,30}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Polygon(
            points={{-100,70},{-100,40},{60,70},{-100,70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-100,-70},{-100,-40},{60,-70},{-100,-70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Line(points={{0,-70},{0,-50}}, color={0,0,0}),
          Rectangle(
            extent={{-100,90},{100,70}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-70},{100,-90}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));

  end ThermalTurbineRotor;

  model HydroTurbineRotor "Hydro turbine rotor"
    extends Partials.RigidRotorCase;

    annotation (defaultComponentName = "turbRotor1",
      Documentation(
              info="<html>
<p>Turbine-rotor as one single rigid mass</p>
<pre>
  flange_p, flange_n:  connectors to other rotating parts
                       of the turbo-generator group
</pre>
<p><i>
No pole pair reduction of equations of motion.<br>
phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{20,10},{40,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-60,-80},{100,-100}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,80},{60,-80}},
            lineColor={170,213,255},
            fillColor={170,213,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,100},{60,80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,60},{20,-60}},
            lineColor={95,95,95},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-20,80},{20,40}},
            lineColor={0,0,0},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-20,-40},{20,-80}},
            lineColor={0,0,0},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215})}));
  end HydroTurbineRotor;

  model DieselRotor "Diesel rotor"
    extends Partials.RigidRotorCase;

    annotation (defaultComponentName = "turbRotor1",
      Documentation(
              info="<html>
<p>Turbine-rotor as one single rigid mass</p>
<pre>
  flange_p, flange_n:  connectors to other rotating parts
                       of the turbo-generator group
</pre>
<p><i>
No pole pair reduction of equations of motion.<br>
phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{20,10},{40,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-60,-80},{100,-100}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,80},{60,-80}},
            lineColor={255,128,0},
            fillColor={255,128,0},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,80},{40,-80}},
            lineColor={95,95,95},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-30,100},{30,80}},
            lineColor={95,95,95},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,4},{30,-56}},
            lineColor={95,95,95},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end DieselRotor;

  model WindTurbineRotor "Wind turbine rotor"
    extends Partials.RigidRotorCase;

  annotation (defaultComponentName = "turbRotor1",
    Documentation(
            info="<html>
<p>Turbine-rotor as one single rigid mass</p>
<pre>
  flange_p, flange_n:  connectors to other rotating parts
                       of the turbo-generator group
</pre>
<p><i>
No pole pair reduction of equations of motion.<br>
phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,90},{40,70}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-70},{40,-90}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,100},{60,-100}},
            lineColor={255,255,170},
            fillColor={255,255,170},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,14},{40,-14}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{0,-120},{0,120},{8,80},{16,40},{16,20},{12,6},{0,0},{-12,-6},
                {-16,-20},{-16,-40},{-10,-80},{0,-120}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{40,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215})}));
  end WindTurbineRotor;

  model ElectricRotor "Electric generator/motor rotor, mechanical"
    extends Partials.RigidRotorCase;

  annotation (defaultComponentName = "elRotor",
      Documentation(
              info="<html>
<p>Rotor as one single stiff mass.</p>
<pre>
  flange_p, flange_n:  connectors to other rotating parts
                       of the turbo-generator group
</pre>
<p><i>
No pole pair reduction of equations of motion.<br>
phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-100,50},{100,-50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-100,70},{100,50}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-50},{100,-70}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid),
          Line(points={{0,-70},{0,-50}}, color={0,0,0}),
          Rectangle(
            extent={{-100,90},{100,70}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-70},{100,-90}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid)}));
  end ElectricRotor;

model ShaftNoMass "Elastic massless shaft"
  extends Ports.Compliant;

  parameter Types.TorsionStiffness stiff=
                                        1e6 "torsion stiffness";

equation
  flange_p.tau + flange_n.tau = 0;
  d_tau = stiff*d_phi;
  annotation (defaultComponentName = "shaft1",
    Documentation(
            info="<html>
<p>Rotating torsion-elastic massless shaft. It is equivalent to a massless torsion spring.<br><br>
The parameter <tt>stiffness</tt> is a length-independent specification, in contrast to a spring-constant.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-100,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255})}));
end ShaftNoMass;

model Shaft "Elastic massive shaft"
  extends Ports.Compliant;

  parameter SI.Inertia J=1 "inertia";
  parameter Types.TorsionStiffness stiff=
                                        1e6 "torsion stiffness";
  SI.Angle phi(stateSelect=StateSelect.prefer) "rotation angle center";
  SI.AngularVelocity w(stateSelect=StateSelect.prefer);
  SI.AngularAcceleration a;

equation
  flange_p.phi + flange_n.phi = 2*phi;
  w = der(phi);
  a = der(w);
  J*a = flange_p.tau + flange_n.tau;
  d_tau = stiff*d_phi;
  annotation (defaultComponentName = "shaft1",
    Documentation(
            info="<html>
<p>Rotating torsion-elastic massive shaft. It is equivalent to a massive torsion spring.<br>
(Approximation for small torsion-angles / lowest mode to avoid wave-equation)<br><br>
The parameter <tt>stiffness</tt> is a length-independent specification, in contrast to a spring-constant.</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-100,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175})}));
end Shaft;

model GearNoMass "Massless gear"
  extends Ports.Flange_p_n;

  parameter Real[:] ratio={1,1}
      "gear-ratio {p, .., n}, (speeds in arbitrary units)";
  protected
  final parameter Real ratio_pn=ratio[1]/ratio[end];

equation
  flange_p.phi = ratio_pn*flange_n.phi;
  ratio_pn*flange_p.tau + flange_n.tau = 0;
  annotation (defaultComponentName = "gear",
    Documentation(
            info="<html>
<p>Ideal massless gear. Rigid coupling with gear-ratio</p>
<pre>  ratio[1]/ratio[end]</pre>
<p>Input identical with massive gear 'Gear'.<br>
Gear ratios are defined by <b>relative</b> speed. The following specifications are equivalent
<pre>
  ratio = {6, 2, 1}
  ratio = {9000, 3000, 1500}
</pre></p>
<p>For memorising
<pre>  ratio[1]/ratio[end] > 1 if flange_a faster flange_b.</pre></p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{30,40},{50,-40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{30,80},{50,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{50,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-30,70},{30,50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-50,100},{-30,20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-50,20},{-30,-20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-100,10},{-50,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-30,50},{30,-20}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-80,10},{-50,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-50,20},{-30,-20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-50,100},{-30,20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{-30,70},{30,50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{30,80},{50,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{30,40},{50,-40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Rectangle(
            extent={{50,10},{80,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={255,255,255}),
          Text(
            extent={{-100,-80},{100,-100}},
            lineColor={255,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "ratio = {4, 2, 1}  or  {6000, 3000, 1500}"),
          Text(
            extent={{-70,-30},{-10,-50}},
            lineColor={0,0,0},
            textString=
               "J[1] = 0"),
          Text(
            extent={{-30,100},{30,80}},
            lineColor={0,0,0},
            textString=
               "J[2] = 0"),
          Text(
            extent={{10,-50},{70,-70}},
            lineColor={0,0,0},
            textString=
               "J[3] = 0")}));
end GearNoMass;

model Gear "Massive gear"
  extends Ports.Flange_p_n;

  parameter Real[:] ratio={1,1}
      "gear-ratio {p, .., n}, (speeds in arbitrary units)";
  parameter SI.Inertia[:] J={1,1} "inertias {p, .., n}, (not reduced)";
  SI.Angle phi(stateSelect=StateSelect.prefer);
  SI.AngularVelocity w(stateSelect=StateSelect.prefer);
  SI.AngularAcceleration a;
  protected
  final parameter Real ratio_pn=ratio[1]/ratio[end];
  final parameter Real[size(ratio,1)] ratio2=diagonal(ratio)*ratio/(ratio[end]*ratio[end]);

equation
  flange_p.phi = ratio_pn*flange_n.phi;
  phi = flange_n.phi;
  w = der(phi);
  a = der(w);
  (ratio2*J)*a = ratio_pn*flange_p.tau + flange_n.tau;
  annotation (defaultComponentName = "gear",
    Documentation(
            info="<html>
<p>Ideal massive gear. N rigidly coupled inertias with gear-ratio
<pre>  ratio[1]/ratio[2], .., ratio[end-1]/ratio[end]</pre></p>
<p>Input identical with massless gear 'GearNoMass'.</p>
<p>Gear ratios are defined by <b>relative</b> speed. The following specifications are equivalent
<pre>
  ratio = {6, 2, 1}
  ratio = {9000, 3000, 1500}
</pre></p>
<p>For memorising
<pre>  ratio[1]/ratio[end] > 1 if flange_a faster flange_b.</pre></p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{30,40},{50,-40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{30,80},{50,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{50,10},{100,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-30,70},{30,50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-50,100},{-30,20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{-50,20},{-30,-20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{-100,10},{-50,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-30,50},{30,-20}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-70,-30},{-10,-50}},
            lineColor={0,0,0},
            textString=
               "J[1]"),
          Text(
            extent={{10,-50},{70,-70}},
            lineColor={0,0,0},
            textString=
               "J[3]"),
          Rectangle(
            extent={{30,40},{50,-40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{30,80},{50,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{50,10},{80,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-30,70},{30,50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-50,100},{-30,20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{-50,20},{-30,-20}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={175,175,175}),
          Rectangle(
            extent={{-80,10},{-50,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Text(
            extent={{-30,100},{30,80}},
            lineColor={0,0,0},
            textString=
               "J[2]"),
          Text(
            extent={{-100,-80},{100,-100}},
            lineColor={255,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "ratio = {4, 2, 1}  or  {6000, 3000, 1500}")}));
end Gear;

model NoGear "Placeholder for gear"
  extends Ports.Flange_p_n;

equation
  flange_p.phi = flange_n.phi;
  flange_p.tau + flange_n.tau = 0;
  annotation (defaultComponentName = "joint",
    Documentation(
            info="<html>
<p>Joining two rotational flanges directly, in place of gear.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-80,80},{80,-80}},
            lineColor={0,0,0},
            pattern=LinePattern.Dot,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Line(points={{-90,0},{90,0}}, color=
               {0,0,0})}));
end NoGear;

model AngleSensor "Angle and angular velocity sensor (mechanical)"
  extends Ports.Flange_p;

  Modelica.Blocks.Interfaces.RealOutput phi "angle"
    annotation (Placement(transformation(
          origin={-40,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  Modelica.Blocks.Interfaces.RealOutput w "angular velocity"
    annotation (Placement(transformation(
          origin={40,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

equation
  flange.tau = 0;
  phi = flange.phi;
  w = der(flange.phi);
  annotation (defaultComponentName = "angleSens1",
    Documentation(
            info="<html>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-70,70},{70,-70}},
            lineColor={128,128,128},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,70},{0,40}}, color={128,128,128}),
          Line(points={{22.9,32.8},{40.2,57.3}}, color={128,128,128}),
          Line(points={{-22.9,32.8},{-40.2,57.3}}, color={128,128,128}),
          Line(points={{37.6,13.7},{65.8,23.9}}, color={128,128,128}),
          Line(points={{-37.6,13.7},{-65.8,23.9}}, color={128,128,128}),
          Line(points={{0,0},{9.02,28.6}}, color={95,95,95}),
          Polygon(
            points={{-0.48,31.6},{18,26},{18,57.2},{-0.48,31.6}},
            lineColor={95,95,95},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-5,5},{5,-5}},
            lineColor={0,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{0,0}}, color={135,135,135}),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name")}));
end AngleSensor;

model PowerSensor "Power and torque sensor (mechanical)"
  extends Ports.Rigid;

  Modelica.Blocks.Interfaces.RealOutput p "power, flange_p to flange_n"
    annotation (Placement(transformation(
          origin={-40,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  Modelica.Blocks.Interfaces.RealOutput tau "torque, flange_p to flange_n"
    annotation (Placement(transformation(
          origin={40,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

equation
  flange_p.tau + flange_n.tau = 0;
  tau = flange_p.tau;
  p = der(flange_p.phi)*flange_p.tau;
  annotation (defaultComponentName = "powerSens1",
    Documentation(
            info="<html>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-70,70},{70,-70}},
            lineColor={128,128,128},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Line(points={{-90,0},{-20,0}}, color={95,95,95}),
          Line(points={{0,0},{90,0}}, color={95,95,95}),
          Line(points={{30,20},{70,0},{30,-20}}, color={95,95,95}),
          Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135}),
          Line(points={{-66,24},{-48,18}}, color={95,95,95}),
          Line(points={{-40,57},{-30,43}}, color={95,95,95}),
          Line(points={{0,70},{0,52}}, color={95,95,95}),
          Line(points={{40,57},{30,42}}, color={95,95,95}),
          Line(points={{66,24},{48,18}}, color={95,95,95})}));
end PowerSensor;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model TabTorque "Table data to torque"
      extends Ports.Flange_p_n;

      parameter String tableName="" "table name in file";
      parameter String fileName=TableDir + "" "name of file containing table";
      parameter Integer colData=2 "column # used data";
      SI.Torque tau;

    Modelica.Blocks.Tables.CombiTable1Ds table(
      final tableName=tableName,
      final fileName=fileName,
      final columns={colData},
      tableOnFile=true) "{time t .. force f ..}"
        annotation (Placement(transformation(extent={{-20,-20},{20,20}})));

    equation
      flange_p.phi = flange_n.phi;
      flange_p.tau + flange_n.tau + tau = 0;
      annotation (defaultComponentName = "tabForce1",
        Documentation(
                info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(extent={{-40,-90},{40,-70}}, lineColor={128,128,128}),
            Line(points={{-40,-80},{40,-80}}, color={128,128,128}),
            Line(points={{-20,-70},{-20,-90}}, color={128,128,128}),
            Ellipse(
              extent={{-60,60},{60,-60}},
              lineColor={0,0,0},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{-40,40},{40,-40}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{26,20},{66,-20}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{26,20},{46,-20},{66,20},{26,20}},
              lineColor={0,0,0},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid)}));
    end TabTorque;

    partial model RigidRotorBase "Rigid rotor base"
      extends Ports.Rigid;

      parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SI.Inertia J=1 "inertia";
      parameter SI.AngularVelocity w_start = 0
        "start value of angular velocity"
        annotation(Dialog(tab="Initialization"));
      SI.Angle phi "rotation angle absolute";
      SI.AngularVelocity w(start = w_start);
      SI.AngularAcceleration a;

    protected
      outer System system;

    initial equation
      if dynType == Types.Dynamics.SteadyInitial then
        der(w) = 0;
      else
        w = w_start;
      end if;

    equation
      phi = flange_p.phi;
      w = der(phi);
      a = der(w);
      annotation (
        Documentation(
              info="<html>
</html>"));
    end RigidRotorBase;

    partial model RigidRotorCase "Rigid rotor with case"
      extends RigidRotorBase;

      Interfaces.Rotation_p rotor
        "connector to turbine (mech) or airgap (el) torque"
        annotation (Placement(transformation(
            origin={0,60},
            extent={{10,-10},{-10,10}},
            rotation=270)));
      Interfaces.Rotation_p stator "access for stator reaction moment"
        annotation (Placement(transformation(
            origin={100,-80},
            extent={{-10,10},{10,-10}},
            rotation=180)));
      Interfaces.Rotation_n friction "access for friction model"
    annotation (Placement(transformation(
            origin={0,-80},
            extent={{-10,-10},{10,10}},
            rotation=90)));

    equation
      if cardinality(stator) == 0 then
        stator.phi = 0;
      else
        rotor.tau + stator.tau + friction.tau = 0;
      end if;
      rotor.phi = phi - stator.phi;
      friction.phi = rotor.phi;
      J*a = rotor.tau + flange_p.tau + flange_n.tau + friction.tau;
      annotation (
        Documentation(
              info="<html>
<p>Rigid rotor base with an additional access for torque on rotor, stator (case) reaction torque, and a collective access for friction.</p>
</html>
"),        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(points={{0,-80},{0,-60}}, color={0,0,0}),
              Line(
              points={{-80,-60},{80,-60}},
              color={0,0,0},
              thickness=0.5)}));
    end RigidRotorCase;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
</html>
"));
end Rotation;
