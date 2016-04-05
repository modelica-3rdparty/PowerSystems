within PowerSystems.AC1ph_DC;
package Drives "DC-drives"
  extends Modelica.Icons.VariantsPackage;

  model DCMser "DC machine, series connected"
    extends Partials.DriveBase(heat(final m=2));

    replaceable model Motor = PowerSystems.AC1ph_DC.Machines.DCser (
      w_el_ini=w_ini*motor.par.pp) "DC motor series"
                        annotation (choicesAllMatching=true);
    Motor motor "DC motor series"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

  equation
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,0,255}));
  annotation (defaultComponentName = "dcm_ser",
    Documentation(
            info="<html>
<p>Complete DC drive series connected.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
               "DC ser"),
          Line(points={{-90,-10},{-60,-10}}, color={0,0,255}),
          Line(points={{-90,10},{-80,10},{-80,50},{-60,50}}, color={0,0,255}),
          Line(points={{-60,10},{-70,10},{-70,-6}}, color={0,0,255}),
          Line(points={{-70,-14},{-70,-50},{-60,-50}}, color={0,0,255})}));
  end DCMser;

  model DCMpar "DC machine, parallel connected"
    extends Partials.DriveBase(heat(final m=2));

    replaceable model Motor = PowerSystems.AC1ph_DC.Machines.DCpar (
      w_el_ini = w_ini*motor.par.pp) "DC motor parallel"
                          annotation (choicesAllMatching=true);
    Motor motor "DC motor parallel"
       annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    AC1ph_DC.Ports.TwoPin_p field
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));

  equation
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,0,255}));
    connect(field, motor.field) annotation (Line(points={{-100,-40},{-60,-40},{
            -60,-4},{-40,-4}}, color={0,0,255}));
  annotation (defaultComponentName = "dcm_par",
    Documentation(
            info="<html>
<p>Complete DC drive parallel connected.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,10},{-60,10}}, color={0,0,255}),
          Line(points={{-90,-10},{-60,-10}}, color={0,0,255}),
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
               "DC par"),
          Line(points={{-90,-50},{-60,-50}}, color={0,0,255}),
          Line(points={{-60,50},{-68,50},{-68,14}}, color={0,0,255}),
          Line(points={{-68,-14},{-68,-30},{-90,-30}}, color={0,0,255})}));
  end DCMpar;

  model DCMpm "DC machine, permanent magnet"
    extends Partials.DriveBase(heat(final m=2));

    replaceable model Motor = PowerSystems.AC1ph_DC.Machines.DCpm (
      w_el_ini = w_ini*motor.par.pp) "DC motor magnet"
                        annotation (choicesAllMatching=true);
    Motor motor "DC motor magnet"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

  equation
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,0,255}));
  annotation (defaultComponentName = "dcm_pm",
    Documentation(
            info="<html>
<p>Complete DC drive permanent magnet excited.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
               "DC pm"),
          Line(points={{-90,10},{-60,10}}, color={0,0,255}),
          Line(points={{-90,-10},{-60,-10}}, color={0,0,255}),
          Rectangle(
            extent={{-60,44},{80,40}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-40},{80,-44}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid)}));
  end DCMpm;

  model BLDC "BLDC machine"
    extends Partials.DriveBase(heat(final m=sum(heat_adapt.m)));

    replaceable model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage (
      modulation=3) constrainedby
      PowerSystems.AC3ph.Inverters.Partials.AC_DC_base
      "inverter (average or modulated)" annotation (choices(
      choice(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.InverterAverage(final modulation=3)
            "inverter time-average"),
      choice(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.Inverter(redeclare final model Modulator
                =
          Control.Modulation.BlockM "block modulation (no PWM)")
            "inverter with modulator")));
    Inverter inverter "inverter (average or modulated)"
      annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
    replaceable model Motor =
        PowerSystems.AC1ph_DC.Drives.Partials.Synchron3rd_bldc (
      w_el_ini = w_ini*motor.par.pp) "BLDC motor (syn pm machine)"
      annotation (choices(
      choice(redeclare model Motor =
              PowerSystems.AC3ph.Machines.Synchron3rd_bldc "synchron 3rd order")));
    Motor motor "BLDC motor (syn pm machine)"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    Common.Thermal.HeatV_a_b_ab heat_adapt(final m={2,inverter.heat.m})
      annotation (Placement(transformation(extent={{10,60},{-10,80}})));

  equation
    connect(motor.heat, heat_adapt.port_a) annotation (Line(points={{-30,10},{
            -30,54},{4,54},{4,64}}, color={176,0,0}));
    connect(inverter.heat, heat_adapt.port_b) annotation (Line(points={{-70,10},
            {-70,64},{-4,64}}, color={176,0,0}));
    connect(heat_adapt.port_ab, heat)
      annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, inverter.DC)
      annotation (Line(points={{-100,0},{-80,0}}, color={0,0,255}));
    connect(inverter.AC, motor.term) annotation (Line(points={{-60,0},{-40,0}},
          color={0,120,120}));
    connect(motor.rotorAngle, inverter.theta) annotation (Line(points={{-20,10},
            {-16,10},{-16,20},{-84,20},{-84,10},{-76,10}}, color={0,0,127}));
    connect(motor.uPhasor, inverter.uPhasor) annotation (Line(points={{-40,10},
            {-64,10}}, color={0,0,127}));
  annotation (defaultComponentName = "bldcm",
    Documentation(
            info="<html>
<p>Complete brushless DC drive with inverter.</p>
<p>For block (rectangular) non-pulsed modulation the input to uPhasor[1] is arbitrary and has no influence.</p>
<p>For pulsed rectangles uPhasor[1] can be used<br>
a) to simulate time-average voltage amplitudes.<br>
b) to determine the ratio of on- to off-time when using a pulsed modulator.<br>
where 0 &lt  uPhasor[1] &lt  1.</p>
<p>The rectangle width is a parameter with default value 2/3, corresponding to '2 phases on, 1 phase off' at a time.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-60,30},{80,26}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,-26},{80,-30}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "BLDC")}));
  end BLDC;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

  partial model DriveBase "DC drives base"

    parameter Types.AngularVelocity  w_ini = 0
        "initial rpm (start-value if ini='st')"
      annotation(Dialog(enable=not system.steadyIni));
    AC1ph_DC.Ports.TwoPin_p term "electric terminal"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Interfaces.Rotation_n flange "mechanical flange"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    replaceable model Rotor = PowerSystems.Mechanics.Rotation.ElectricRotor
        "machine rotor"
                      annotation (choicesAllMatching=true);
    Rotor rotor(w(start=w_ini)) "machine rotor"
      annotation (Placement(transformation(extent={{0,-10},{20,10}})));
    replaceable model Gear = PowerSystems.Mechanics.Rotation.NoGear
        "type of gear"
                     annotation (                                                                                                    choices(
        choice(redeclare model Gear = PowerSystems.Mechanics.Rotation.Joint
              "no gear"),
        choice(redeclare model Gear =
                PowerSystems.Mechanics.Rotation.GearNoMass "massless gear"),
        choice(redeclare model Gear = PowerSystems.Mechanics.Rotation.Gear
              "massive gear")));
    Gear gear "type of gear"
      annotation (Placement(transformation(extent={{40,-10},{60,10}})));
    Interfaces.ThermalV_n heat(     m=2) "heat source port {stator, rotor}"
      annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    protected
    outer System system;

  equation
    connect(rotor.flange_n, gear.flange_p)
        annotation (Line(points={{20,0},{40,0}}, color={0,0,0}));
    connect(gear.flange_n, flange)
        annotation (Line(points={{60,0},{100,0}}, color={0,0,0}));
    annotation (
  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-90,80},{90,-80}},
              lineColor={95,95,95},
              fillColor={184,189,116},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{80,10},{100,-10}},
              lineColor={0,0,0},
              fillPattern=FillPattern.HorizontalCylinder,
              fillColor={235,235,235}),
            Rectangle(
              extent={{-60,60},{80,40}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-60,30},{80,-30}},
              lineColor={0,0,0},
              fillPattern=FillPattern.HorizontalCylinder,
              fillColor={215,215,215}),
            Rectangle(
              extent={{-60,-40},{80,-60}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
                   "%name"),
            Rectangle(
              extent={{-60,40},{80,30}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-60,-30},{80,-40}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-60},{100,-80}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid,
              textString=
          "stator reaction torque- and friction-models may be added here")}),
  Documentation(info="<html>
</html>"));
  end DriveBase;

  model Synchron3rd_bldc
      "Synchronous machine, torque-control, 3rd order model, 3-phase dq0"
    extends AC3ph.Machines.Synchron3rd_pm(i_s(start={0,1,0}));

    Modelica.Blocks.Interfaces.RealOutput rotorAngle=
                      phi_el "rotor angle el"
      annotation (Placement(transformation(
            origin={100,100},
            extent={{10,-10},{-10,10}},
            rotation=180)));
    Modelica.Blocks.Interfaces.RealOutput[2] uPhasor
        "desired {abs(u), phase(u)}"
      annotation (Placement(transformation(
            origin={-100,100},
            extent={{-10,-10},{10,10}},
            rotation=180)));

  equation
    uPhasor[1] = 1; //not used if no PWM
    uPhasor[2] = atan2(c.R_s*i_s[2] + w_el*psi_e, -w_el*c.L_s[2]*i_s[2]);
    annotation (
  Documentation(info="<html>
<p>The model is valid for permanent magnet (<tt>excite=2</tt>) machines.</p>
<p>For rectangular voltage without additional PWM the only degree of freedom is the voltage phase. It is chosen such that the d-axis current (magnetising current) is zero <tt> i_s[1] = 0</tt>.<br>
Under theese assumptions the torque-speed characteristic is different from that of a DC-machine. In order to obtain a 'true' DC-characteristic with linear dependence of torque and i_s[2] from w_el, a second degree of freedom is needed.</p>
Example using 'average' inverter (approximation of rectangle by its fundamental)
<p><pre>
  v_sd = -w_el*c.L_s[2]*i_s[2]/((4/pi)*sin(width*pi/2)*par.V_nom);
  v_sq = 1;
  uPhasor[1] = sqrt(v_sd*v_sd + v_sq*v_sq);
  uPhasor[2] = atan2(v_sq, v_sd);
</pre></p>
<p>For reluctance (<tt>excite=3</tt>) machines psi_e is replaced by a desired magnetising current in d-axis <tt>i_sd</tt>.
<pre>
  uPhasor[2] = atan2(c.R_s*i_s[2] + w_el*c.L_s[1]*i_sd, c.R_s*i_sd - w_el*c.L_s[2]*i_s[2]);
</pre>
Using a pu-current <tt>i_sd_pu</tt> we obtain
<pre>
  uPhasor[2] = atan2(c.R_s*i_s[2] + w_el*c.L_s[1]*i_sd, (par.V_nom/(c.omega_nom*c.L_s[1]))*i_sd_pu - w_el*c.L_s[2]*i_s[2]);
</pre></p>
<p>More information see Partials.Synchron3rdBase.</p>
</html>"), Icon(graphics={Rectangle(
              extent={{-90,112},{90,88}},
              lineColor={0,0,127},
              fillColor={170,213,255},
              fillPattern=FillPattern.Solid)}));
  end Synchron3rd_bldc;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Contains both electrical and mechanical parts of DC-drives.</p>
<p>Heat ports must be connected. In cases where they are not needed, use 'Common.Thermal.BdCond(V)'.</p><p><a <p><a href=\"modelica://PowerSystems.UsersGuide.Overview\">up users guide</a></p>
</html>
"));
end Drives;
