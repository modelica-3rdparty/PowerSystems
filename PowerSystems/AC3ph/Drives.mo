within PowerSystems.AC3ph;
package Drives "AC-drives dqo"
  extends Modelica.Icons.VariantsPackage;

  model ASM "Asynchronous machine with cage rotor"

    parameter SIpu.AngularVelocity speed_ini(          unit="1")=0
      "initial speed (start-value if ini='st')"
      annotation(Dialog(enable=not system.steadyIni));
    extends Partials.DriveBase(rotor(
                               w(      start=speed_ini*2*pi*motor.par.f_nom/2)));
    replaceable PowerSystems.AC3ph.Machines.Asynchron motor(final w_el_ini=speed_ini*2*pi*motor.par.f_nom)
      "asyn motor"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}},
            rotation=0)));

  equation
    connect(motor.heat, heat) annotation (Line(points={{-30,10},{-30,40},{0,40},
            {0,100}}, color={176,0,0}));
    connect(motor.airgap, rotor.rotor)
      annotation (Line(points={{-30,6},{10,6}}, color={0,0,0}));
    connect(term, motor.term) annotation (Line(points={{-100,0},{-40,0}}, color=
           {0,120,120}));
    annotation (defaultComponentName = "asm",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>Complete ASM drive.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "asyn")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ASM;

  model ASM_Y_D "Asynchronous machine with cage rotor, Y-Delta switcheable"

    parameter SIpu.AngularVelocity speed_ini(          unit="1")=0
      "initial speed (start-value if ini='st')"
      annotation(Dialog(enable=not system.steadyIni));
    extends Partials.DriveBase;
    replaceable PowerSystems.AC3ph.Machines.AsynchronY_D motor(final w_el_ini=speed_ini*2*pi*motor.par.f_nom)
      "asyn motor Y-Delta switcheable"
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}},
            rotation=0)));
    input Modelica.Blocks.Interfaces.BooleanInput YDcontrol
      "true:Y, false:Delta"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}},
            rotation=0)));

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
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>Complete ASM drive with switcheable Y-Delta topology.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
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
    replaceable AC3ph.Inverters.InverterAverage inverter
      constrainedby PowerSystems.AC3ph.Inverters.Partials.AC_DC_base
      "inverter (average or modulated)"           annotation (                          choices(
      choice(redeclare PowerSystems.AC3ph.Inverters.InverterAverage inverter(final
              autosyn=false) "inverter time-average"),
      choice(redeclare PowerSystems.AC3ph.Inverters.Inverter inverter(final autosyn=false)
            "inverter with modulator")), Placement(transformation(extent={{-80,
              -10},{-60,10}}, rotation=0)));
    replaceable AC3ph.Machines.Asynchron_ctrl motor(
      final w_el_ini=w_ini*motor.par.pp) "asyn motor, current controlled"
      annotation (                         choices(
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl
                                                                    motor
            "synchron 3rd order"),
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron_pm_ctrl
                                                                 motor
            "synchron general")), Placement(transformation(extent={{-40,-10},{
              -20,10}}, rotation=0)));

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
    connect(motor.uPhasor, inverter.uPhasor)
      annotation (Line(points={{-40,10},{-64,10}}, color={0,0,127}));
    connect(motor.i_meas, i_meas)       annotation (Line(points={{-36,10},{-36,
            40},{-60,40},{-60,100}}, color={0,0,127}));
    connect(i_act, motor.i_act)       annotation (Line(points={{60,100},{60,40},
            {-24,40},{-24,10}}, color={0,0,127}));
    connect(motor.phiRotorflux, inverter.theta) annotation (Line(points={{-20,
            10},{-16,10},{-16,20},{-84,20},{-84,10},{-76,10}}, color={0,0,127}));
      annotation (
      defaultComponentName="sm_ctrlAv",
      Diagram(graphics),
      Icon(graphics={Text(
            extent={{-60,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "asyn")}),
      Documentation(info="<html>
<p>Complete ASM drive with inverter and motor for field oriented current control.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"));
  end ASM_ctrl;

  model SM_el "Synchronous machine, electric excitation"

    parameter SIpu.AngularVelocity speed_ini(          unit="1")=0
      "initial speed (start-value if ini='st')"
      annotation(Dialog(enable=not system.steadyIni));
    extends Partials.DriveBase;
    replaceable PowerSystems.AC3ph.Machines.Control.Excitation excitation
      "excitation model"                annotation (Placement(transformation(
            extent={{-70,20},{-50,40}}, rotation=0)));
    replaceable PowerSystems.AC3ph.Machines.Synchron3rd_ee motor(final w_el_ini=speed_ini*2*pi*motor.par.f_nom)
      "syn motor"
      annotation (                         choices(
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron3rd_ee motor
            "synchron 3rd order"),
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron_ee motor
            "synchron general")), Placement(transformation(extent={{-40,-10},{
              -20,10}}, rotation=0)));
    Modelica.Blocks.Interfaces.RealInput fieldVoltage(
                             final unit="1")
      "field voltage pu from exciter control"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.RealOutput[3] termVoltage(
                                        final unit="1")
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
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>Complete SM drive with electrically excited motor.</p>
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
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
                 "syn")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end SM_el;

  model SM_ctrl "Synchronous machine, current-control"
    extends Partials.DriveBase_ctrl(heat_adapt(final m={2,inverter.heat.m}));
    replaceable AC3ph.Inverters.InverterAverage inverter
      constrainedby PowerSystems.AC3ph.Inverters.Partials.AC_DC_base
      "inverter (average or modulated)" annotation (                          choices(
      choice(redeclare PowerSystems.AC3ph.Inverters.InverterAverage inverter
            "inverter time-average"),
      choice(redeclare PowerSystems.AC3ph.Inverters.Inverter inverter
            "inverter with modulator")), Placement(transformation(extent={{-80,
              -10},{-60,10}}, rotation=0)));
    replaceable Machines.Synchron3rd_pm_ctrl    motor(
      final w_el_ini=w_ini*motor.par.pp) "syn motor, current controlled"
      annotation (                         choices(
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl
                                                                    motor
            "synchron 3rd order"),
      choice(redeclare PowerSystems.AC3ph.Machines.Synchron_pm_ctrl
                                                                 motor
            "synchron general")), Placement(transformation(extent={{-40,-10},{
              -20,10}}, rotation=0)));

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
    connect(motor.uPhasor, inverter.uPhasor)
      annotation (Line(points={{-40,10},{-64,10}}, color={0,0,127}));
    connect(motor.i_meas, i_meas)       annotation (Line(points={{-36,10},{-36,
            40},{-60,40},{-60,100}}, color={0,0,127}));
    connect(i_act, motor.i_act)       annotation (Line(points={{60,100},{60,40},
            {-24,40},{-24,10}}, color={0,0,127}));
      annotation (
      defaultComponentName="sm_ctrl",
      Diagram(graphics),
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
<p>Note: for machines with gear <tt>w_ini</tt> denotes the initial angular velocity at the generator-side!</p>
</html>"));
  end SM_ctrl;

  package Partials "Partial models"
  partial model DriveBase0 "AC drives base mechanical"

    Interfaces.Rotation_n flange "mechanical flange"
      annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=
               0)));
    replaceable PowerSystems.Mechanics.Rotation.ElectricRotor rotor
        "machine rotor"
      annotation (Placement(transformation(extent={{0,-10},{20,10}}, rotation=0)));
    replaceable PowerSystems.Mechanics.Rotation.NoGear gear "type of gear"
      annotation (                                                                                                    choices(
        choice(redeclare PowerSystems.Mechanics.Rotation.Joint gear "no gear"),
        choice(redeclare PowerSystems.Mechanics.Rotation.GearNoMass gear
              "massless gear"),
        choice(redeclare PowerSystems.Mechanics.Rotation.Gear gear
              "massive gear")),
          Placement(transformation(extent={{40,-10},{60,10}}, rotation=0)));
    Interfaces.ThermalV_n heat(              m=2)
        "heat source port {stator, rotor}"
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
  Window(
    x=0.41,
        y=0.01,
        width=
  0.6,
    height=
   0.6),
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

    AC3ph.Ports.ACdqo_p term "electric terminal"
                            annotation (Placement(transformation(extent={{-110,
                -10},{-90,10}}, rotation=0)));

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
  Window(
    x=0.41,
        y=0.01,
        width=
  0.6,
    height=
   0.6),
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
    extends DriveBase0(heat(final m=sum(heat_adapt.m)),
      rotor(w(start=w_ini)));

    parameter Types.AngularVelocity  w_ini=0
        "initial rpm (start-value if ini='st')"
      annotation(Dialog(enable=not system.steadyIni));
    AC1ph_DC.Ports.TwoPin_p term "electric terminal DC"
                            annotation (Placement(transformation(extent={{-110,
                -10},{-90,10}}, rotation=0)));
    Modelica.Blocks.Interfaces.RealOutput[2] i_meas(
                             each final unit="1")
        "measured current {i_d, i_q} pu"
      annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    Modelica.Blocks.Interfaces.RealInput[2] i_act(
                             each final unit="1")
        "actuated current {i_d, i_q} pu"
      annotation (Placement(transformation(
            origin={60,100},
            extent={{10,-10},{-10,10}},
            rotation=90)));
    protected
    Common.Thermal.HeatV_a_b_ab heat_adapt annotation (Placement(transformation(
              extent={{10,60},{-10,80}}, rotation=0)));

  equation
    connect(heat_adapt.port_ab, heat)
      annotation (Line(points={{0,76},{0,100}}, color={176,0,0}));
  annotation (
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
            grid={2,2}), graphics={Rectangle(
              extent={{-90,112},{90,88}},
              lineColor={0,0,127},
              fillColor={170,213,255},
              fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
  end DriveBase_ctrl;

    annotation (       Window(
  x=0.05,
  y=0.44,
  width=0.31,
  height=0.23,
  library=1,
  autolayout=1));
  end Partials;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.32,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>Contains both electrical and mechanical parts of AC-drives, dqo-representation.</p>
<p>Heat ports must be connected. In cases where they are not needed, use 'Common.Thermal.BdCond(V)'.</p><p><a <p><a href=\"PowerSystems.UsersGuide.Overview\">up users guide</a></p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Drives;
