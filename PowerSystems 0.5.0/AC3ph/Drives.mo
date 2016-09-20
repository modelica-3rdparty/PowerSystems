within PowerSystems.AC3ph;
package Drives "AC-drives dq0"
  extends Modelica.Icons.VariantsPackage;

  model ASM "Asynchronous machine with cage rotor"

    extends Partials.DriveBase(rotor(w(start=w_start)));
    replaceable model Motor = PowerSystems.AC3ph.Machines.Asynchron
      "asyn motor" annotation(choicesAllMatching=true);
    Motor motor "asyn motor"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

  equation
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term) annotation (Line(points={{-100,0},{-40,0}}, color=
           {0,120,120}));
    annotation (defaultComponentName = "asm",
      Documentation(
              info="<html>
<p>Complete ASM drive.</p>
<p>Note: for machines with gear <tt>w_start</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "asyn")}));
  end ASM;

  model ASM_Y_D "Asynchronous machine with cage rotor, Y-Delta switcheable"

    extends Partials.DriveBase;
    replaceable model Motor = PowerSystems.AC3ph.Machines.AsynchronY_D
      "asyn motor Y-Delta switcheable" annotation (choicesAllMatching=true);
    Motor motor "asyn motor Y-Delta switcheable"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    input Modelica.Blocks.Interfaces.BooleanInput YDcontrol
      "true:Y, false:Delta"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));

  equation
    connect(YDcontrol, motor.YDcontrol) annotation (Line(points={{-100,40},{-60,
            40},{-60,6},{-40,6}}, color={255,0,255}));
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term) annotation (Line(points={{-100,0},{-40,0}}, color=
           {0,120,120}));
    annotation (defaultComponentName = "asm_Y_D",
      Documentation(
              info="<html>
<p>Complete ASM drive with switcheable Y-Delta topology.</p>
<p>Note: for machines with gear <tt>w_start</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "asynY-D")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,90},{-10,80}},
            lineColor={255,0,255},
            textString=
                 "switcheable Y - Delta topology"), Text(
            extent={{-90,68},{-30,60}},
            lineColor={255,0,255},
            textString=
                 "true: Y     false: Delta")}));
  end ASM_Y_D;

  model ASM_ctrl "Asynchronous machine, current-control"
    extends Partials.DriveBase_ctrl(heat_adapt(final m={2,inverter.heat.m}));

    replaceable model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage
      constrainedby PowerSystems.AC3ph.Inverters.Partials.AC_DC_base
      "inverter (average or modulated)" annotation (choices(
      choice(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.InverterAverage(final autosyn=false)
            "inverter time-average"),
      choice(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.Inverter(final autosyn=false)
            "inverter with modulator")));
    Inverter inverter "inverter (average or modulated)"
      annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));

    replaceable model Motor = PowerSystems.AC3ph.Machines.Asynchron_ctrl
      "asyn motor, current controlled" annotation (choices(
      choice(redeclare model Motor =
              PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl
            "synchron 3rd order"),
      choice(redeclare model Motor =
              PowerSystems.AC3ph.Machines.Synchron_pm_ctrl "synchron general")));
    Motor motor "asyn motor, current controlled"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

  equation
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, inverter.DC)
      annotation (Line(points={{-100,0},{-80,0}}, color={0,0,255}));
    connect(inverter.AC, motor.term)
      annotation (Line(points={{-60,0},{-40,0}}, color={0,120,120}));
    connect(motor.heat, heat_adapt.port_a) annotation (Line(points={{-30,10},{
            -30,54},{4,54},{4,64}}, color={176,0,0}));
    connect(inverter.heat, heat_adapt.port_b) annotation (Line(points={{-70,10},
            {-70,64},{-4,64}}, color={176,0,0}));
    connect(motor.vPhasor,inverter.vPhasor)
      annotation (Line(points={{-40,10},{-64,10}}, color={0,0,127}));
    connect(motor.i_meas, i_meas)       annotation (Line(points={{-36,10},{-36,
            40},{-60,40},{-60,100}}, color={0,0,127}));
    connect(i_act, motor.i_act)       annotation (Line(points={{60,100},{60,40},
            {-24,40},{-24,10}}, color={0,0,127}));
    connect(motor.phiRotorflux, inverter.theta) annotation (Line(points={{-20,
            10},{-16,10},{-16,20},{-84,20},{-84,10},{-76,10}}, color={0,0,127}));
      annotation (
      defaultComponentName="sm_ctrlAv",
      Icon(graphics={Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "asyn")}),
      Documentation(info="<html>
<p>Complete ASM drive with inverter and motor for field oriented current control.</p>
<p>Note: for machines with gear <tt>w_start</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"));
  end ASM_ctrl;

  model SM_el "Synchronous machine, electric excitation"

    extends Partials.DriveBase;

    replaceable model Excitation =
        PowerSystems.AC3ph.Machines.Control.Excitation "excitation model"
        annotation (choicesAllMatching=true);
    Excitation excitation "excitation model"
      annotation (Placement(transformation(extent={{-70,20},{-50,40}})));

    replaceable model Motor = PowerSystems.AC3ph.Machines.Synchron3rd_ee
      "syn motor" annotation (choices(
      choice(redeclare model Motor = PowerSystems.AC3ph.Machines.Synchron3rd_ee
            "synchron 3rd order"),
      choice(redeclare model Motor = PowerSystems.AC3ph.Machines.Synchron_ee
            "synchron general")));
    Motor motor "syn motor"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

    Modelica.Blocks.Interfaces.RealInput fieldVoltage(final unit="1")
      "field voltage pu from exciter control"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.RealOutput[3] termVoltage(final unit="1")
      "terminal voltage pu to exciter control"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
    connect(fieldVoltage, excitation.fieldVoltage)
                                                  annotation (Line(points={{60,
            100},{60,60},{-54,60},{-54,40}}, color={0,0,127}));
    connect(excitation.term, motor.term) annotation (Line(points={{-70,30},{-80,
            30},{-80,0},{-40,0}}, color={0,120,120}));
    connect(excitation.field, motor.field) annotation (Line(points={{-70,26},{
            -70,-4},{-40,-4}}, color={0,0,255}));
    connect(term, motor.term)
      annotation (Line(points={{-100,0},{-40,0}}, color={0,120,120}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(excitation.termVoltage, termVoltage) annotation (Line(points={{-66,
            40},{-66,60},{-60,60},{-60,100}}, color={0,0,127}));
    annotation (defaultComponentName = "sm_el",
      Documentation(
              info="<html>
<p>Complete SM drive with electrically excited motor.</p>
<p>Note: for machines with gear <tt>w_start</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-90,-10},{-60,-10}}, color={0,0,255}),
          Line(points={{-90,10},{-60,10}}, color={0,0,255}),
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "syn")}));
  end SM_el;

  model SM_ctrl "Synchronous machine, current-control"
    extends Partials.DriveBase_ctrl(heat_adapt(final m={2,inverter.heat.m}));
    replaceable model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage
      constrainedby PowerSystems.AC3ph.Inverters.Partials.AC_DC_base
      "inverter (average or modulated)" annotation (choices(
      choice(redeclare model Inverter =
              PowerSystems.AC3ph.Inverters.InverterAverage
            "inverter time-average"),
      choice(redeclare model Inverter = PowerSystems.AC3ph.Inverters.Inverter
            "inverter with modulator")));
    Inverter inverter "inverter (average or modulated)"
      annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));

    replaceable model Motor = PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl
      "syn motor, current controlled" annotation (choices(
      choice(redeclare model Motor =
              PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl
            "synchron 3rd order"),
      choice(redeclare model Motor =
              PowerSystems.AC3ph.Machines.Synchron_pm_ctrl "synchron general")));
    Motor motor "syn motor, current controlled"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));

  equation
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, inverter.DC)
      annotation (Line(points={{-100,0},{-80,0}}, color={0,0,255}));
    connect(inverter.AC, motor.term)
      annotation (Line(points={{-60,0},{-40,0}}, color={0,120,120}));
    connect(motor.heat, heat_adapt.port_a) annotation (Line(points={{-30,10},{
            -30,54},{4,54},{4,64}}, color={176,0,0}));
    connect(inverter.heat, heat_adapt.port_b) annotation (Line(points={{-70,10},
            {-70,64},{-4,64}}, color={176,0,0}));
    connect(motor.phiRotor, inverter.theta)   annotation (Line(points={{-20,10},
            {-16,10},{-16,20},{-84,20},{-84,10},{-76,10}}, color={0,0,127}));
    connect(motor.vPhasor,inverter.vPhasor)
      annotation (Line(points={{-40,10},{-64,10}}, color={0,0,127}));
    connect(motor.i_meas, i_meas)       annotation (Line(points={{-36,10},{-36,
            40},{-60,40},{-60,100}}, color={0,0,127}));
    connect(i_act, motor.i_act)       annotation (Line(points={{60,100},{60,40},
            {-24,40},{-24,10}}, color={0,0,127}));
      annotation (
      defaultComponentName="sm_ctrl",
      Icon(graphics={
          Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "syn"),
          Rectangle(
            extent={{-60,-26},{80,-30}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-60,30},{80,26}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid)}),
      Documentation(info="<html>
<p>Complete SM drive with inverter and motor for field oriented current control.</p>
<p>Note: for machines with gear <tt>w_start</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"));
  end SM_ctrl;

  package Partials "Partial models"
  partial model DriveBase0 "AC drives base mechanical"

    parameter Types.AngularVelocity  w_start=0
      "initial rpm (start-value if steady init)"
      annotation(Dialog(tab="Initialization"));
    Interfaces.Rotation_n flange "mechanical flange"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    replaceable model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor
        "machine rotor"
                      annotation(choicesAllMatching=true);
    Rotor rotor(w_start=w_start) "machine rotor"
      annotation (Placement(transformation(extent={{0,-10},{20,10}})));
    replaceable model Gear = PowerSystems.Mechanics.Rotational.NoGear
        "type of gear"
      annotation (choices(
        choice(redeclare model Gear = PowerSystems.Mechanics.Rotational.Joint
              "no gear"),
        choice(redeclare model Gear =
                PowerSystems.Mechanics.Rotational.GearNoMass
                                                           "massless gear"),
        choice(redeclare model Gear = PowerSystems.Mechanics.Rotational.Gear
              "massive gear")));
    Gear gear "type of gear"
      annotation (Placement(transformation(extent={{40,-10},{60,10}})));
    Interfaces.ThermalV_n heat(              m=2)
        "heat source port {stator, rotor}"
      annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    protected
    outer System system;

  equation
    connect(rotor.flange_b,gear.flange_a)
      annotation (Line(points={{20,0},{40,0}}, color={0,0,0}));
    connect(gear.flange_b, flange)
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
  end DriveBase0;
    extends Modelica.Icons.BasesPackage;

  partial model DriveBase "AC drives base"
    extends DriveBase0;

    AC3ph.Ports.ACdq0_p term "electric terminal"
                            annotation (Placement(transformation(extent={{-110,
                -10},{-90,10}})));

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

  partial model DriveBase_ctrl "AC drives base control"

    extends DriveBase0(heat(final m=sum(heat_adapt.m)));

    AC1ph_DC.Ports.TwoPin_p term "electric terminal DC"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput[2] i_meas(each final unit="1")
        "measured current {i_d, i_q} pu"
      annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    Modelica.Blocks.Interfaces.RealInput[2] i_act(each final unit="1")
        "actuated current {i_d, i_q} pu"
      annotation (Placement(transformation(
            origin={60,100},
            extent={{10,-10},{-10,10}},
            rotation=90)));
    protected
    Common.Thermal.HeatV_a_b_ab heat_adapt annotation (Placement(transformation(
              extent={{10,60},{-10,80}})));

  equation
    connect(heat_adapt.port_ab, heat)
      annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
  annotation (
    Documentation(
            info="<html>
</html>"),
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-90,112},{90,88}},
              lineColor={0,0,127},
              fillColor={170,213,255},
              fillPattern=FillPattern.Solid)}));
  end DriveBase_ctrl;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Contains both electrical and mechanical parts of AC-drives, dq0-representation.</p>
<p>Heat ports must be connected. In cases where they are not needed, use 'Common.Thermal.BdCond(V)'.</p><p><a <p><a href=\"modelica://PowerSystems.UsersGuide.Overview\">up users guide</a></p>
</html>
"));
end Drives;
