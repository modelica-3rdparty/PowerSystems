within PowerSystems;
model System "System reference"
  parameter Types.SystemFrequency fType=PowerSystems.Types.SystemFrequency.Parameter
    "system frequency type"
    annotation (Evaluate=true, Dialog(group="Frequency"));
  parameter SI.Frequency f = f_nom
    "frequency if type is parameter, else initial frequency"
    annotation(Evaluate=true, Dialog(group="Frequency",
      enable=fType == PowerSystems.Types.SystemFrequency.Parameter));
  parameter SI.Frequency f_nom = 50 "nominal frequency"
   annotation(Evaluate=true, Dialog(group="Nominal"), choices(choice=50 "50 Hz", choice=60 "60 Hz"));
  parameter SI.Frequency f_lim[2]={0.5*f_nom, 2*f_nom}
    "limit frequencies (for supervision of average frequency)"
   annotation(Evaluate=true, Dialog(group="Frequency",
     enable=fType <> PowerSystems.Types.SystemFrequency.Parameter));
  parameter SI.Angle alpha0 = 0 "phase angle"
   annotation(Evaluate=true, Dialog(group="System"));
  parameter Types.ReferenceFrame refType = PowerSystems.Types.ReferenceFrame.Synchron
    "reference frame (3-phase)"
    annotation(Evaluate=true, Dialog(group="System", enable=dynType<>PowerSystems.Types.Dynamics.SteadyState));
  parameter Types.Dynamics dynType = PowerSystems.Types.Dynamics.SteadyInitial
    "transient or steady-state model"
    annotation(Evaluate=true, Dialog(group="Mode"));

  final parameter SI.AngularFrequency omega_nom = 2*pi*f_nom
    "nominal angular frequency" annotation(Evaluate=true);
  final parameter Types.AngularVelocity w_nom = 2*pi*f_nom "nom r.p.m."
                 annotation(Evaluate=true, Dialog(group="Nominal"));
  final parameter Boolean synRef=if transientSim then refType==PowerSystems.Types.ReferenceFrame.Synchron else true
    annotation(Evaluate=true);

  final parameter Boolean steadyIni = dynType<>PowerSystems.Types.Dynamics.FixedInitial
    "steady state initialisation of electric equations" annotation(Evaluate=true);
  final parameter Boolean transientSim = dynType<>PowerSystems.Types.Dynamics.SteadyState
    "transient mode of electric equations" annotation(Evaluate=true);
  final parameter Boolean steadyIni_t = steadyIni and transientSim
    annotation(Evaluate=true);
  discrete SI.Time initime;
  SI.Angle theta(final start=0,
    stateSelect=if fType==Types.SystemFrequency.Parameter then StateSelect.default else StateSelect.always);
  SI.AngularFrequency omega(final start=2*pi*f);
  Modelica.Blocks.Interfaces.RealInput omega_in(min=0) if fType == PowerSystems.Types.SystemFrequency.Signal
    "system angular frequency (optional if fType==Signal)"
    annotation (extent=[-110,-10; -90,10]);

  Interfaces.Frequency receiveFreq
    "receives weighted frequencies from generators"
   annotation (Placement(transformation(extent={{-96,64},{-64,96}})));
protected
  Modelica.Blocks.Interfaces.RealInput omega_internal;
initial equation
  if fType <> Types.SystemFrequency.Parameter then
    theta = omega*time;
  end if;

equation
  connect(omega_in, omega_internal);
  omega = omega_internal;
  when initial() then
    initime = time;
  end when;
  if fType == Types.SystemFrequency.Parameter then
    omega = 2*pi*f;
    theta = omega*time;
  elseif fType == Types.SystemFrequency.Signal then
    //omega defined by omega_in
    theta = omega*time;
  else
    omega = if initial() then 2*pi*f else receiveFreq.w_H/receiveFreq.H;
    der(theta) = omega;
    when (omega < 2*pi*f_lim[1]) or (omega > 2*pi*f_lim[2]) then
      terminate("FREQUENCY EXCEEDS BOUNDS!");
    end when;
  end if;
  // set dummy values (to achieve balanced model)
  receiveFreq.h = 0.0;
  receiveFreq.w_h = 0.0;
  annotation (
  preferredView="info",
  defaultComponentName="system",
  defaultComponentPrefixes="inner",
  missingInnerMessage="No \"system\" component is defined.
    Drag PowerSystems.System into the top level of your model.",
  Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics={
        Rectangle(
          extent={{-100,100},{100,-100}},
          lineColor={0,120,120},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-100,100},{100,60}},
          lineColor={0,120,120},
          fillColor={0,120,120},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{-60,100},{100,60}},
          lineColor={215,215,215},
          textString="%name"),
        Text(
          extent={{-100,50},{100,20}},
          lineColor={0,0,0},
          textString="f:%fType"),
        Text(
          extent={{-100,-20},{100,10}},
          lineColor={0,0,0},
          textString="f_nom=%f_nom"),
        Text(
          extent={{-100,-30},{100,-60}},
          lineColor={0,120,120},
          textString="%refType"),
        Text(
          extent={{-100,-70},{100,-100}},
          lineColor={176,0,0},
          textString="%dynType")}),
  Documentation(info="<html>
<p>The model <b>System</b> represents a global reference for the following purposes:</p>
<p>It allows the choice of </p>
<ul>
<li> nominal frequency (default 50 or 60 Hertz, but arbitrary positive choice allowed)
<li> system frequency or initial system frequency, depending on frequency type</li>
<li> frequency type: parameter, signal, or average (machine-dependent) system frequency</li>
<li> lower and upper limit-frequencies</li>
<li> common phase angle for AC-systems</li>
<li> synchronous or inertial reference frame for AC-3phase-systems</li>
<li> transient or steady-state initialisation and simulation modes<br>
     For 'transient' initialisation no specific initial equations are defined.<br>
     This case allows also to use Dymola's steady-state initialisation, that is DIFFERENT from ours.<br>
     <b>Note:</b> the parameter 'sim' only affects AC three-phase components.</li>
</ul>
<p>It provides</p>
<ul>
<li> the system angular-frequency omega<br>
     For fType Parameter this is simply a parameter value.<br>
     For fType Signal it is a positive input signal.<br>
     For fType Average it is a weighted average over the relevant generator frequencies.</li>
<li> the system angle theta by integration of
<pre> der(theta) = omega </pre><br>
     This angle allows the definition of a rotating electrical <b>coordinate system</b><br>
     for <b>AC three-phase models</b>.<br>
     Root-nodes defining coordinate-orientation will choose a reference angle theta_ref (connector-variable theta[2]) according to the parameter <tt>refType</tt>:<br><br>
     <tt>theta_ref = theta if refType = Synchron</tt> (reference frame is synchronously rotating with theta).<br>
     <tt>theta_ref = 0 if refType = Inertial</tt> (inertial reference frame, not rotating).<br></li>
</ul>
<p><b>Note</b>: Each model using <b>System</b> must use it with an <b>inner</b> declaration and instance name <b>system</b> in order that it can be accessed from all objects in the model.<br>When dragging the 'System' from the package browser into the diagram layer, declaration and instance name are automatically generated.</p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Overview\">up users guide</a></p>
</html>
"));
end System;
