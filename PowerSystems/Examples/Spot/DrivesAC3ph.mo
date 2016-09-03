within PowerSystems.Examples.Spot;
package DrivesAC3ph "AC drives, dq0"
  extends Modelica.Icons.ExamplesPackage;

  model ASMcharacteristic
    "AC asynchronous machine: torque - slip characteristic"

    inner PowerSystems.System system(dynType=PowerSystems.Types.Dynamics.SteadyState)
    annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd
      annotation (Placement(transformation(extent={{-60,-20},{-80,0}})));
    PowerSystems.AC3ph.Sources.Voltage voltage(V_nom=400)
                                     annotation (Placement(transformation(
            extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Drives.ASM asm(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron (
        redeclare record Data =
            PowerSystems.Examples.Spot.Data.Machines.Asynchron400V_30kVA))
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2)
                                          annotation (Placement(transformation(
            extent={{-20,0},{0,20}})));
    PowerSystems.Mechanics.Rotational.Speed speed(
      w0=system.omega_nom/2,
      use_w_in=true,
      tcst=0.01)
      annotation (Placement(transformation(extent={{40,-20},{20,0}})));
    PowerSystems.Blocks.Signals.Transient speedSignal(
    t_duration=0.5,
      s_end=2*system.omega_nom/asm.motor.pp,
      s_start=-system.omega_nom/asm.motor.pp)
      annotation (Placement(transformation(extent={{78,-20},{58,0}})));

  equation
    connect(grd.term, voltage.neutral)
                                     annotation (Line(points={{-60,-10},{-60,
            -10}}, color={0,0,255}));
    connect(asm.flange, speed.flange)
    annotation (Line(points={{0,-10},{20,-10}}, color={0,0,0}));
    connect(asm.heat, bdCond.heat)
    annotation (Line(points={{-10,0},{-10,0}}, color={176,0,0}));
    connect(voltage.term, asm.term)
                                  annotation (Line(points={{-40,-10},{-20,-10}},
          color={0,120,120}));
    connect(speedSignal.y, speed.w_in)
      annotation (Line(points={{58,-10},{40,-10}}, color={0,0,127}));
  annotation (      experiment(StopTime=1),
      Documentation(info="<html>
<p>Steady-state simulation to produce motor characteristic 'torque vs slip'.<br>
<pre>
  asm.torque         motor mechanical torque
  asm.motor.slip     slip (negative: motor, positive: generator mode)

       slip &lt  -1    motor breake mode
  -1 &lt  slip &lt  0     motor drive mode
       slip &gt  0     generator mode
</pre></p>
<p>Plot torque vs slip:<br>
plot 'asm.torque', then right-click 'asm.motor.slip' and choose 'Independent variable': 'asm.motor.slip'.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"));
  end ASMcharacteristic;

  model ASM_Y_D "AC asynchronous machine Y-Delta switcheable"

    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC3ph.Sources.Voltage voltage(V_nom=400, v0=1)
                                        annotation (Placement(transformation(
            extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Sensors.Psensor power
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Drives.ASM_Y_D asm_Y_D(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Motor = PowerSystems.AC3ph.Machines.AsynchronY_D (
        redeclare record Data =
            PowerSystems.Examples.Spot.Data.Machines.Asynchron400V_30kVA),
      w_start=4.1887902047864)
      annotation (Placement(transformation(extent={{-10,-20},{10,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2) annotation (Placement(
          transformation(extent={{-10,0},{10,20}})));
    PowerSystems.Mechanics.Rotational.Rotor loadInertia(J=40)
      annotation (Placement(transformation(extent={{30,-20},{50,0}})));
    PowerSystems.Mechanics.Rotational.FrictionTorque frictTorq(cFrict={1,0.05})
      annotation (Placement(transformation(extent={{70,-20},{90,0}})));
    PowerSystems.Control.Relays.Y_DeltaControl relay1(t_switch={1.5})
      annotation (Placement(transformation(extent={{-50,20},{-30,40}})));

  equation
    connect(relay1.y,asm_Y_D. YDcontrol) annotation (Line(points={{-30,30},{-20,
            30},{-20,-6},{-10,-6}}, color={255,0,255}));
    connect(voltage.term, power.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,120,120}));
    connect(asm_Y_D.flange,loadInertia.flange_a)
      annotation (Line(points={{10,-10},{30,-10}}, color={0,0,0}));
    connect(loadInertia.flange_b, frictTorq.flange)
      annotation (Line(points={{50,-10},{70,-10}}, color={0,0,0}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-80,-10},{-80,-10}}, color={0,0,255}));
    connect(power.term_n, asm_Y_D.term) annotation (Line(points={{-30,-10},{-10,
            -10}}, color={0,120,120}));
    connect(asm_Y_D.heat, bdCond.heat) annotation (Line(points={{0,0},{0,0}},
          color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Asynchron machine, Y-Delta switcheable, start-up.</p>
<p><i>See for example:</i>
<pre>
  power.p
  asm_Y_D.motor.slip
  loadInertia.flange_a.tau
  frictTorq.flange.tau
  frictTorq.w
</pre>
Compare 'transient' and 'steady-state' mode.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>
"),      experiment(StopTime=3));
  end ASM_Y_D;

  model ASMav
    "AC asynchronous machine, voltage controlled with average inverter"

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Synchron)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*6000)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Inverters.Select select
      annotation (Placement(transformation(extent={{-50,20},{-30,40}})));
    PowerSystems.AC3ph.Inverters.InverterAverage inverter(redeclare record Data =
      PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC3kV_500A)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Sensors.PVImeter power(av=true,
      tcst=0.05,
      phasor=true)
      annotation (Placement(transformation(extent={{-10,-20},{10,0}})));
    PowerSystems.AC3ph.Drives.ASM asm(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=6.4),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron (
        redeclare record Data =
            PowerSystems.Examples.Spot.Data.Machines.Asynchron3kV_1p5MVA),
      w_start=157.07963267949)
      annotation (Placement(transformation(extent={{30,-20},{50,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond1(m=2)
                                            annotation (Placement(
          transformation(extent={{30,0},{50,20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond2(m=1)
      annotation (Placement(transformation(extent={{-50,0},{-30,20}})));
    PowerSystems.Mechanics.Rotational.TabPosSlopeTorque tabLoad(
      r=0.4,
      gRatio=40/17,
      cFrict={50,15},
      mass=200e3,
      scale=true,
      D=1.5e3,
      slope_perc=2.5,
      tableName="height",
      fileName=TableDir + "hNormProfile.tab",
      colData=3)
      annotation (Placement(transformation(extent={{70,-20},{90,0}})));

  equation
    connect(select.theta_out, inverter.theta) annotation (Line(points={{-46,20},
            {-46,0}}, color={0,0,127}));
    connect(select.vPhasor_out,inverter.vPhasor)  annotation (Line(points={{-34,
            20},{-34,0}}, color={0,0,127}));
    connect(voltage.term, inverter.DC)
      annotation (Line(points={{-60,-10},{-50,-10}}, color={0,0,255}));
    connect(inverter.AC, power.term_p) annotation (Line(points={{-30,-10},{-10,
            -10}}, color={0,120,120}));
    connect(power.term_n, asm.term)
      annotation (Line(points={{10,-10},{30,-10}}, color={0,120,120}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-80,-10},{-80,-10}}, color={0,0,255}));
    connect(asm.flange,tabLoad.flange_a)
      annotation (Line(points={{50,-10},{70,-10}}, color={0,0,0}));
    connect(asm.heat, bdCond1.heat)
                                   annotation (Line(points={{40,0},{40,0}},
          color={176,0,0}));
    connect(inverter.heat,bdCond2. heat) annotation (Line(points={{-40,0},{-40,
            0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Asynchron machine with load (drive along height-profile), on-load steady-state start.<br>
The model uses a time-average inverter. With the actual parameter values the 'inverter' corresponds exactly to an AC voltage source of 3kV.</p>
<p><i>See for example:</i>
<pre>
  power.p
  asm.motor.slip
  tabLoad.vVehicle
</pre>
Compare 'transient' and 'steady-state' mode.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>
"),      experiment(
        StopTime=60,
        Interval=5e-2,
        Tolerance=1e-006));
  end ASMav;

  model ASMav_icontrol
    "AC asynchronous machine, current controlled with average inverter"

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Synchron)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-40},{-60,-20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*6000)
      annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
    PowerSystems.AC3ph.Drives.ASM_ctrl asm_ctrl(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron_ctrl (
        redeclare record Data =
            PowerSystems.Examples.Spot.Data.Machines.Asynchron3kV_1p5MVA),
      redeclare model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC3kV_500A),
      w_start=157.07963267949)
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{0,-20},{20,0}})));
    PowerSystems.Mechanics.Rotational.TabPosSlopeTorque tabLoad(
      r=0.4,
      gRatio=40/17,
      cFrict={50,15},
      mass=200e3,
      scale=true,
      D=1.5e3,
      slope_perc=2.5,
      tableName="height",
      fileName=TableDir + "hNormProfile.tab",
      colData=3)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.Blocks.Signals.Transient i_q(s_end=0.2, s_start=0)
      "phase of modulation signal"          annotation (Placement(
          transformation(extent={{-90,0},{-70,20}})));
    PowerSystems.Blocks.Signals.Transient i_d(
      s_start=0.36,
      s_end=0.36,
      t_change=30,
      t_duration=60) "phase of modulation signal"
                                            annotation (Placement(
          transformation(extent={{-70,30},{-50,50}})));
    Modelica.Blocks.Continuous.LimPID PI_i_q(
      Td=0.1,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      Ti=0.1,
      yMax=1,
      initType=Modelica.Blocks.Types.InitPID.SteadyState,
      gainPID(y(start=0.1)))
           annotation (Placement(transformation(extent={{-50,0},{-30,20}})));

  equation
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-40,-30},{-40,-30}}, color={0,0,255}));
    connect(voltage.term, asm_ctrl.term)
      annotation (Line(points={{-20,-30},{0,-30}}, color={0,0,255}));
    connect(asm_ctrl.flange,tabLoad.flange_a)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,0}));
    connect(i_q.y,PI_i_q. u_s)
                              annotation (Line(points={{-70,10},{-52,10}},
          color={0,0,127}));
    connect(i_d.y, asm_ctrl.i_act[1])       annotation (Line(points={{-50,40},{16,
            40},{16,-19.5}},    color={0,0,127}));
    connect(asm_ctrl.i_meas[2], PI_i_q.u_m) annotation (Line(points={{4,-19.5},
            {4,-12},{-40,-12},{-40,-2}}, color={0,0,127}));
    connect(PI_i_q.y, asm_ctrl.i_act[2]) annotation (Line(points={{-29,10},{16,10},
            {16,-20.5}},     color={0,0,127}));
    connect(asm_ctrl.heat, bdCond.heat)
      annotation (Line(points={{10,-20},{10,-20}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Current (torque) controlled asynchron machine with load (drive along height-profile), steady-state start, torque-increase after start.<br>
The model uses a time-average inverter. For comparison with the previous example 'ASMav'.</p>
<p><i>See for example:</i>
<pre>
  asm.motor.tau_el
  tabLoad.vVehicle
</pre>
Compare 'transient' and 'steady-state' mode.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>
"),      experiment(
        StopTime=60,
        Interval=5e-2,
        Tolerance=1e-006));
  end ASMav_icontrol;

  model ASM "AC asynchronous machine, voltage controlled"

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Inertial)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*6000)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC3ph.Inverters.Select select
      annotation (Placement(transformation(extent={{-50,20},{-30,40}})));
    PowerSystems.AC3ph.Inverters.Inverter inverter
                                          annotation (Placement(transformation(
            extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Sensors.PVImeter power(
      av=true,
      tcst=0.05,
      abc=true)
      annotation (Placement(transformation(extent={{-10,-20},{10,0}})));
    PowerSystems.AC3ph.Drives.ASM asm(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=6.4),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron (
        redeclare record Data =
            PowerSystems.Examples.Spot.Data.Machines.Asynchron3kV_1p5MVA),
      w_start=157.07963267949)
      annotation (Placement(transformation(extent={{30,-20},{50,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond1(m=2)
                                            annotation (Placement(
          transformation(extent={{30,0},{50,20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond2(m=3)
      annotation (Placement(transformation(extent={{-50,0},{-30,20}})));
    PowerSystems.Mechanics.Rotational.TabPosSlopeTorque tabLoad(
      r=0.4,
      gRatio=40/17,
      cFrict={50,15},
      mass=200e3,
      scale=true,
      D=1.5e3,
      slope_perc=2.5,
      tableName="height",
      fileName=TableDir + "hNormProfile.tab",
      colData=3)
      annotation (Placement(transformation(extent={{70,-20},{90,0}})));

  equation
    connect(select.theta_out, inverter.theta) annotation (Line(points={{-46,20},
            {-46,0}}, color={0,0,127}));
    connect(select.vPhasor_out,inverter.vPhasor)  annotation (Line(points={{-34,
            20},{-34,0}}, color={0,0,127}));
    connect(voltage.term, inverter.DC)
      annotation (Line(points={{-60,-10},{-50,-10}}, color={0,0,255}));
    connect(inverter.AC, power.term_p) annotation (Line(points={{-30,-10},{-10,
            -10}}, color={0,120,120}));
    connect(power.term_n, asm.term)
      annotation (Line(points={{10,-10},{30,-10}}, color={0,120,120}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-80,-10},{-80,-10}}, color={0,0,255}));
    connect(asm.flange,tabLoad.flange_a)
      annotation (Line(points={{50,-10},{70,-10}}, color={0,0,0}));
    connect(asm.heat, bdCond1.heat)
                                   annotation (Line(points={{40,0},{40,0}},
          color={176,0,0}));
    connect(inverter.heat,bdCond2. heat) annotation (Line(points={{-40,0},{-40,
            0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>Asynchron machine with load (drive along height-profile), on-load transient start.<br>
The machine defines the reference-system independent of the system choice (as needed for example in hardware-in-the-loop simulation). This model uses a switched inverter.</p>
<p><i>See for example:</i>
<pre>
  power.p_av        time-average power
  time_av.y         time-average pu stator currents
  asm.motor.tau_el  electric torque
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"),
      experiment(StopTime=1, Tolerance=1e-005));
  end ASM;

  model SM_ctrlAv
    "AC synchronous pm machine, current controlled with average inverter"

    inner PowerSystems.System system
    annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-40},{-60,-20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*2*400)
                                      annotation (Placement(transformation(
            extent={{-40,-40},{-20,-20}})));
    PowerSystems.AC3ph.Drives.SM_ctrl sm_ctrl(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC1kV_100A),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Machines.Synchron3rd_pm400V_30kVA))
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{0,-20},{20,0}})));
    PowerSystems.Mechanics.Rotational.Rotor loadInertia(J=0.5)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.Mechanics.Rotational.FrictionTorque frictTorq(cFrict={0.1,0.01})
      annotation (Placement(transformation(extent={{80,-40},{100,-20}})));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      offsetTorque=0,
      startTime=6,
      stepTorque=-100,
      useSupport=false)
                annotation (Placement(transformation(extent={{100,0},{80,20}})));
    PowerSystems.Blocks.Signals.Transient i_q(t_change=3, s_start=0.1)
      "phase of modulation signal"          annotation (Placement(
          transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.Blocks.Signals.Transient i_d(
      t_change=3,
      s_start=0,
      s_end=0) "phase of modulation signal" annotation (Placement(
          transformation(extent={{-80,40},{-60,60}})));
    Modelica.Blocks.Continuous.LimPID PI_i_q(
      Ti=0.2,
      Td=0.1,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      initType=Modelica.Blocks.Types.InitPID.SteadyState,
      yMax=1,
      gainPID(y(start=0.1)))
           annotation (Placement(transformation(extent={{-60,10},{-40,30}})));

  equation
    connect(sm_ctrl.heat, bdCond.heat)  annotation (Line(points={{10,-20},{10,
            -20}}, color={176,0,0}));
    connect(grd.term, voltage.neutral) annotation (Line(points={{-40,-30},{-40,
            -30}}, color={0,0,255}));
    connect(i_q.y, PI_i_q.u_s)
                              annotation (Line(points={{-80,20},{-62,20}},
          color={0,0,127}));
    connect(sm_ctrl.flange,loadInertia.flange_a)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, frictTorq.flange)
      annotation (Line(points={{60,-30},{80,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, torqueStep.flange) annotation (Line(points={{
            60,-30},{70,-30},{70,10},{80,10}}, color={0,0,0}));
    connect(voltage.term, sm_ctrl.term)
      annotation (Line(points={{-20,-30},{0,-30}}, color={0,0,255}));
    connect(sm_ctrl.i_meas[2], PI_i_q.u_m)        annotation (Line(points={{4,
            -19.5},{4,0},{-50,0},{-50,8}}, color={0,0,127}));
    connect(PI_i_q.y, sm_ctrl.i_act[2])        annotation (Line(points={{-39,20},
            {16,20},{16,-20.5}}, color={0,0,127}));
    connect(i_d.y, sm_ctrl.i_act[1])        annotation (Line(points={{-60,50},{
            16,50},{16,-19.5}}, color={0,0,127}));
  annotation (
    Documentation(
            info="<html>
<p>Field oriented control of pm synchronous machine with time-average inverter. The first component of i_dq controls 'field', the second controls 'torque' at constant 'field'.<br>
For pm machine (psi_pm &gt  0, x_d = x_q) i_d can be set to zero. For reluctance machines (psi_pm = 0, x_d &gt  x_q) i_d must have a positive value.</p>
On-load steady-state start with torque-increase at 3 s and load-step 6 s.</p>
<p><i>See for example:</i>
<pre>
  sm_ctrl.motor.tau_el
  loadInertia.flange_a.tau
  sm_ctrl.motor.w_el
  loadInertia.w
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"),
    experiment(StopTime=10));
  end SM_ctrlAv;

  model SM_ctrl "AC synchronous pm machine, current controlled"

    inner PowerSystems.System system
    annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-40},{-60,-20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*2*400)
                                      annotation (Placement(transformation(
            extent={{-40,-40},{-20,-20}})));
    PowerSystems.AC3ph.Drives.SM_ctrl sm_ctrl(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Inverter = PowerSystems.AC3ph.Inverters.Inverter (
        redeclare model Inverter =
          PowerSystems.AC3ph.Inverters.Components.InverterSwitch
            "switch, no diode, no losses") "inverter with modulator",
      redeclare model Motor = PowerSystems.AC3ph.Machines.Synchron3rd_pm_ctrl (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Machines.Synchron3rd_pm400V_30kVA(r_n=0)))
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=5) annotation (Placement(
          transformation(extent={{0,-20},{20,0}})));
    PowerSystems.Mechanics.Rotational.Rotor loadInertia(J=0.5)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.Mechanics.Rotational.FrictionTorque frictTorq(cFrict={0.1,0.01})
      annotation (Placement(transformation(extent={{80,-40},{100,-20}})));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      offsetTorque=0,
      stepTorque=-100,
      startTime=2,
      useSupport=false)
                annotation (Placement(transformation(extent={{100,0},{80,20}})));
    PowerSystems.Blocks.Signals.Transient i_q(
                    s_start=0.1) "phase of modulation signal"
                                            annotation (Placement(
          transformation(extent={{-100,10},{-80,30}})));
    Modelica.Blocks.Continuous.LimPID PI_i_q(
      Td=0.05,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      Ti=0.2,
      initType=Modelica.Blocks.Types.InitPID.InitialState,
      xi_start=0.1,
      yMax=1)
           annotation (Placement(transformation(extent={{-60,10},{-40,30}})));
    PowerSystems.Blocks.Signals.Transient i_d(
      s_start=0,
      s_end=0) "phase of modulation signal" annotation (Placement(
          transformation(extent={{-80,40},{-60,60}})));

  equation
    connect(sm_ctrl.heat, bdCond.heat)  annotation (Line(points={{10,-20},{10,
            -20}}, color={176,0,0}));
    connect(grd.term, voltage.neutral) annotation (Line(points={{-40,-30},{-40,
            -30}}, color={0,0,255}));
    connect(i_q.y, PI_i_q.u_s)
                            annotation (Line(points={{-80,20},{-62,20}}, color=
            {0,0,127}));
    connect(sm_ctrl.flange,loadInertia.flange_a)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, frictTorq.flange)
      annotation (Line(points={{60,-30},{80,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, torqueStep.flange) annotation (Line(points={{
            60,-30},{70,-30},{70,10},{80,10}}, color={0,0,0}));
    connect(voltage.term, sm_ctrl.term)
      annotation (Line(points={{-20,-30},{0,-30}}, color={0,0,255}));
    connect(sm_ctrl.i_meas[2], PI_i_q.u_m)        annotation (Line(points={{4,
            -19.5},{4,0},{-50,0},{-50,8}}, color={0,0,127}));
    connect(PI_i_q.y, sm_ctrl.i_act[2])        annotation (Line(points={{-39,20},{
            16,20},{16,-20.5}},  color={0,0,127}));
    connect(i_d.y, sm_ctrl.i_act[1])        annotation (Line(points={{-60,50},{16,
            50},{16,-19.5}},    color={0,0,127}));
  annotation (
    Documentation(
            info="<html>
<p>Field oriented control of pm synchronous machine with modulated inverter. The first component of i_dq controls 'field', the second controls 'torque' at constant 'field'.<br>
For pm machine (psi_pm &gt  0, x_d = x_q) i_d can be set to zero. For reluctance machines (psi_pm = 0, x_d &gt  x_q) i_d must have a positive value.</p>
Transient start with torque-increase at 0.5 s and load-step 2 s.</p>
<p><i>See for example:</i>
<pre>
  sm_ctrl.motor.tau_el
  loadInertia.flange_a.tau
  sm_ctrl.motor.w_el
  loadInertia.w
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"),
    experiment(
        StopTime=3,
        Tolerance=1e-005));
  end SM_ctrl;

  model ASM_ctrlAv
    "AC asynchronous machine, current controlled with average inverter"

    inner PowerSystems.System system
    annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-40},{-60,-20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*2*400)
                                      annotation (Placement(transformation(
            extent={{-40,-40},{-20,-20}})));
    PowerSystems.AC3ph.Drives.ASM_ctrl asm_ctrl(
      rotor(J=0.3),
      redeclare model Inverter = PowerSystems.AC3ph.Inverters.InverterAverage (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC1kV_100A),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron_ctrl (
            redeclare record Data =
              PowerSystems.Examples.Spot.Data.Machines.Asynchron400V_30kVA,
            i_start={10,10,0}))
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{0,-20},{20,0}})));
    PowerSystems.Mechanics.Rotational.Rotor loadInertia(J=0.5)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.Mechanics.Rotational.FrictionTorque frictTorq(cFrict={5,0.5})
      annotation (Placement(transformation(extent={{80,-40},{100,-20}})));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      offsetTorque=0,
      startTime=6,
      stepTorque=-200,
      useSupport=false)
                annotation (Placement(transformation(extent={{100,0},{80,20}})));
    PowerSystems.Blocks.Signals.Transient i_q(t_change=3,
      s_end=0.7,
      s_start=0.6) "phase of modulation signal"
                                            annotation (Placement(
          transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.Blocks.Signals.Transient i_d(
      t_change=8,
      s_end=0.45,
      s_start=0.35) "phase of modulation signal"
                                            annotation (Placement(
          transformation(extent={{-80,40},{-60,60}})));
    Modelica.Blocks.Continuous.LimPID PI_i_q(
      Td=0.1,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      Ti=0.1,
      yMax=1,
      gainPID(y(start=0.1)),
      initType=Modelica.Blocks.Types.InitPID.SteadyState)
           annotation (Placement(transformation(extent={{-60,10},{-40,30}})));

  equation
    connect(asm_ctrl.heat, bdCond.heat) annotation (Line(points={{10,-20},{10,
            -20}}, color={176,0,0}));
    connect(grd.term, voltage.neutral) annotation (Line(points={{-40,-30},{-40,
            -30}}, color={0,0,255}));
    connect(i_q.y, PI_i_q.u_s)
                              annotation (Line(points={{-80,20},{-62,20}},
          color={0,0,127}));
    connect(asm_ctrl.flange,loadInertia.flange_a)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, frictTorq.flange)
      annotation (Line(points={{60,-30},{80,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, torqueStep.flange) annotation (Line(points={{
            60,-30},{70,-30},{70,10},{80,10}}, color={0,0,0}));
    connect(voltage.term, asm_ctrl.term)
      annotation (Line(points={{-20,-30},{0,-30}}, color={0,0,255}));
    connect(asm_ctrl.i_meas[2], PI_i_q.u_m)       annotation (Line(points={{4,
            -19.5},{4,0},{-50,0},{-50,8}}, color={0,0,127}));
    connect(PI_i_q.y, asm_ctrl.i_act[2])       annotation (Line(points={{-39,20},{
            16,20},{16,-20.5}},  color={0,0,127}));
    connect(i_d.y, asm_ctrl.i_act[1])       annotation (Line(points={{-60,50},{16,
            50},{16,-19.5}},    color={0,0,127}));
  annotation (
    Documentation(
            info="<html>
<p>Field oriented control of asynchronous machine with time-average inverter. The first component of i_dq controls 'field', the second controls 'torque' at constant 'field'.</p>
On-load steady-state start with torque-increase at 3 s, load-step 6 s and field-increase at 8 s.</p>
<p><i>See for example:</i>
<pre>
  asm_ctrl.motor.tau_el
  asm_ctrl.motor.w_el
  asm_ctrl.motor.vPhasor
  asm_ctrl.motor.slip
</pre></p>
Check vPhasor[1] &lt  1.<br>The time-average inverter produces a desired voltage proportional to vPhasor[1] even if vPhasor[1] &gt  1. For a time-resolved converter this corresponds to overmodulation.
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"),
    experiment(
        StopTime=10,
        Interval=0.001));
  end ASM_ctrlAv;

  model ASM_ctrl "AC asynchronous machine, current controlled"

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Inertial)
    annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-40},{-60,-20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(pol=0, V_nom=sqrt(2/3)*2*400)
                                      annotation (Placement(transformation(
            extent={{-40,-40},{-20,-20}})));
    PowerSystems.AC3ph.Drives.ASM_ctrl asm_ctrl(
      redeclare model Rotor = PowerSystems.Mechanics.Rotational.ElectricRotor (
                                                                            J=0.3),
      redeclare model Motor = PowerSystems.AC3ph.Machines.Asynchron_ctrl (
        redeclare record Data =
          PowerSystems.Examples.Spot.Data.Machines.Asynchron400V_30kVA(r_n=0)),
      redeclare model Inverter = PowerSystems.AC3ph.Inverters.Inverter (
        redeclare model Inverter =
          PowerSystems.AC3ph.Inverters.Components.InverterSwitch
            "switch, no diode, no losses") "inverter with modulator",
      w_start=0.10471975511966)
                annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=5) annotation (Placement(
          transformation(extent={{0,-20},{20,0}})));
    PowerSystems.Mechanics.Rotational.Rotor loadInertia(J=0.5, w_start(
          displayUnit="rpm") = 0.10471975511966)
      annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.Mechanics.Rotational.FrictionTorque frictTorq(cFrict={5,0.5})
      annotation (Placement(transformation(extent={{80,-40},{100,-20}})));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      offsetTorque=0,
      startTime=2,
      stepTorque=-200,
      useSupport=false)
                annotation (Placement(transformation(extent={{100,0},{80,20}})));
    PowerSystems.Blocks.Signals.Transient i_q(s_start=0.6, s_end=0.7)
      "phase of modulation signal"          annotation (Placement(
          transformation(extent={{-100,10},{-80,30}})));
    Modelica.Blocks.Continuous.LimPID PI_i_q(
      Td=0.05,
      controllerType=Modelica.Blocks.Types.SimpleController.PI,
      Ti=0.1,
      yMax=1)
           annotation (Placement(transformation(extent={{-60,10},{-40,30}})));
    PowerSystems.Blocks.Signals.Transient i_d(
      s_end=0.45,
      t_change=2.5,
      t_duration=0.5,
      s_start=0.35) "phase of modulation signal"
                                            annotation (Placement(
          transformation(extent={{-80,40},{-60,60}})));

  equation
    connect(asm_ctrl.heat, bdCond.heat) annotation (Line(points={{10,-20},{10,
            -20}}, color={176,0,0}));
    connect(grd.term, voltage.neutral) annotation (Line(points={{-40,-30},{-40,
            -30}}, color={0,0,255}));
    connect(i_q.y, PI_i_q.u_s)
                            annotation (Line(points={{-80,20},{-62,20}}, color=
            {0,0,127}));
    connect(asm_ctrl.flange,loadInertia.flange_a)
      annotation (Line(points={{20,-30},{40,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, frictTorq.flange)
      annotation (Line(points={{60,-30},{80,-30}}, color={0,0,0}));
    connect(loadInertia.flange_b, torqueStep.flange) annotation (Line(points={{
            60,-30},{70,-30},{70,10},{80,10}}, color={0,0,0}));
    connect(voltage.term, asm_ctrl.term)
      annotation (Line(points={{-20,-30},{0,-30}}, color={0,0,255}));
    connect(asm_ctrl.i_meas[2], PI_i_q.u_m)       annotation (Line(points={{4,
            -19.5},{4,0},{-50,0},{-50,8}}, color={0,0,127}));
    connect(PI_i_q.y, asm_ctrl.i_act[2])       annotation (Line(points={{-39,20},
            {16,20},{16,-20.5}}, color={0,0,127}));
    connect(i_d.y, asm_ctrl.i_act[1])       annotation (Line(points={{-60,50},{
            16,50},{16,-19.5}}, color={0,0,127}));
  annotation (
    Documentation(
            info="<html>
<p>Field oriented control of asynchronous machine with modulated inverter. The first component of i_dq controls 'field', the second controls 'torque' at constant 'field'.</p>
Transient start with torque-increase at 0.5 s, load-step 2 s and field-increase at 2.5 s.</p>
<p><i>See for example:</i>
<pre>
  asm_ctrl.motor.tau_el
  asm_ctrl.motor.w_el
  asm_ctrl.motor.vPhasor
  asm_ctrl.motor.slip
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.Spot.DrivesAC3ph\">up users guide</a></p>
</html>"),
    experiment(
        StopTime=3,
        Tolerance=1e-005));
  end ASM_ctrl;

  annotation (preferredView="info",
Documentation(info="<html>
<p>AC drives (motors electrical and mechanical). Electric motor terminal in dq0-representation.</p>
<p><a href=\"modelica://PowerSystems.Examples.Spot\">up users guide</a></p>
</html>
"));
end DrivesAC3ph;
