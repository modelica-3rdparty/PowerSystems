within PowerSystems.Examples.Spot;
package DrivesDC "DC drives"
  extends Modelica.Icons.ExamplesPackage;

  model DCmotor_ser "DC motor series excited"

    inner PowerSystems.System system(ref="inertial")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.Blocks.Signals.Transient ramp(
      t_change=0,
      t_duration=10)
    annotation (Placement(transformation(extent={{-80,10},{-60,30}}, rotation=0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-60,-20},{-80,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(V_nom=1500, scType_par=false)
                                      annotation (Placement(transformation(
            extent={{-60,-20},{-40,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Psensor power      annotation (Placement(
          transformation(extent={{-20,-20},{0,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Drives.DCMser dcm_ser(
      rotor(J=6.4),
      motor(par=DCs1500_1p5M),
      w_ini=146.60765716752)              annotation (Placement(transformation(
            extent={{20,-20},{40,0}}, rotation=0)));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2) annotation (Placement(
          transformation(extent={{20,0},{40,20}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.TabPosSlopeTorque tabLoad(
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
    annotation (Placement(transformation(extent={{60,-20},{80,0}}, rotation=0)));
    parameter PowerSystems.Examples.Spot.Data.Machines.DCser1500V_1p5MVA DCs1500_1p5M
      annotation (Placement(transformation(extent={{-60,80},{-20,100}},
            rotation=0)));

  equation
    connect(voltage.term, power.term_p)
    annotation (Line(points={{-40,-10},{-20,-10}}, color={0,0,255}));
    connect(power.term_n,dcm_ser. term)
    annotation (Line(points={{0,-10},{20,-10}}, color={0,0,255}));
    connect(grd.term, voltage.neutral) annotation (Line(points={{-60,-10},{-60,
            -10}}, color={0,0,255}));
    connect(dcm_ser.flange, tabLoad.flange_p)
      annotation (Line(points={{40,-10},{60,-10}}, color={0,0,0}));
    connect(dcm_ser.heat, bdCond.heat) annotation (Line(points={{30,0},{30,0}},
          color={176,0,0}));
    connect(ramp.y, voltage.vDC) annotation (Line(points={{-60,20},{-44,20},{
            -44,0}}, color={0,0,127}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>DC machine (series-connected) with load (drive along height-profile).</p>
<p><i>See for example:</i>
<pre>
  power.p
  tabLoad.vVehicle
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(StopTime=60),
    experimentSetupOutput);
  end DCmotor_ser;

  model DCmotor_par "DC motor parallel excited"

    inner PowerSystems.System system(ref="inertial")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-60,-20},{-80,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage armVoltage(V_nom=1500, scType_par=true)
                                      annotation (Placement(transformation(
            extent={{-60,-20},{-40,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Psensor power      annotation (Placement(
          transformation(extent={{-20,-20},{0,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage excVoltage(V_nom=1500, scType_par=true)
                                      annotation (Placement(transformation(
            extent={{-60,-60},{-40,-40}}, rotation=0)));
    PowerSystems.AC1ph_DC.Drives.DCMpar dcm_par(
      rotor(J=6.4),
      motor(par=DCp1500_1p5M))             annotation (Placement(transformation(
            extent={{20,-20},{40,0}}, rotation=0)));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=2) annotation (Placement(
          transformation(extent={{20,0},{40,20}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.TabPosSlopeTorque tabLoad(
      r=0.4,
      gRatio=40/17,
      scale=true,
      D=1.5e3,
      tableName="height",
      fileName=TableDir + "hNormProfile.tab",
      colData=3,
      mass=200e3,
      slope_perc=2.5,
      cFrict={50,15})
    annotation (Placement(transformation(extent={{60,-20},{80,0}}, rotation=0)));
    parameter PowerSystems.Examples.Spot.Data.Machines.DCpar1500V_1p5MVA DCp1500_1p5M
      annotation (Placement(transformation(extent={{-60,80},{-20,100}},
            rotation=0)));

  equation
    connect(armVoltage.term, power.term_p) annotation (Line(points={{-40,-10},{
            -20,-10}}, color={0,0,255}));
    connect(power.term_n, dcm_par.term)
      annotation (Line(points={{0,-10},{20,-10}}, color={0,0,255}));
    connect(excVoltage.term, dcm_par.field) annotation (Line(points={{-40,-50},
            {10,-50},{10,-14},{20,-14}}, color={0,0,255}));
    connect(grd.term, armVoltage.neutral) annotation (Line(points={{-60,-10},{
            -60,-10}}, color={0,0,255}));
    connect(excVoltage.neutral, armVoltage.neutral) annotation (Line(points={{
            -60,-50},{-60,-10}}, color={0,0,255}));
    connect(dcm_par.flange, tabLoad.flange_p)
      annotation (Line(points={{40,-10},{60,-10}}, color={0,0,0}));
    connect(dcm_par.heat, bdCond.heat) annotation (Line(points={{30,0},{30,0}},
          color={176,0,0}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>DC machine (parallel-connected) with load (drive along height-profile).</p>
<p><i>See for example:</i>
<pre>
  power.p
  tabLoad.vVehicle
</pre></p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(StopTime=60),
    experimentSetupOutput);
  end DCmotor_par;

  model DCmotor_pm "DC motor permanent magnet excited"

    inner PowerSystems.System system(ref="inertial", ini="tr")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(V_nom=100)
                                      annotation (Placement(transformation(
            extent={{-80,-20},{-60,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Psensor power annotation (Placement(transformation(
            extent={{-50,-20},{-30,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Efficiency efficiency(tcst=0.1, m=2)
      annotation (Placement(transformation(extent={{-20,-20},{0,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Drives.DCMpm dcm_pm(rotor(J=0.02), motor(par=DCpm100_1k))
                                          annotation (Placement(transformation(
            extent={{10,-20},{30,0}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.Rotor loadInertia(J=0.03)
    annotation (Placement(transformation(extent={{40,-20},{60,0}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.FrictionTorque frictTorq(cFrict={0.01,0.0002})
      annotation (Placement(transformation(extent={{70,-20},{90,0}}, rotation=0)));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      stepTorque=-10,
      startTime=1.5,
      useSupport=false)
                annotation (Placement(transformation(extent={{90,20},{70,40}},
            rotation=0)));
    parameter PowerSystems.Examples.Spot.Data.Machines.DCpm100V_1kVA DCpm100_1k
                                           annotation (Placement(transformation(
            extent={{-60,80},{-20,100}}, rotation=0)));

  equation
    connect(grd.term, voltage.neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(voltage.term, power.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,0,255}));
    connect(power.term_n, efficiency.term_p) annotation (Line(points={{-30,-10},
            {-20,-10}}, color={0,0,255}));
    connect(efficiency.term_n, dcm_pm.term)
      annotation (Line(points={{0,-10},{10,-10}}, color={0,0,255}));
    connect(dcm_pm.flange, loadInertia.flange_p)
      annotation (Line(points={{30,-10},{40,-10}}, color={0,0,0}));
    connect(loadInertia.flange_n, frictTorq.flange)
      annotation (Line(points={{60,-10},{70,-10}}, color={0,0,0}));
    connect(loadInertia.flange_n, torqueStep.flange) annotation (Line(points={{
            60,-10},{64,-10},{64,30},{70,30}}, color={0,0,0}));
    connect(dcm_pm.heat, efficiency.heat) annotation (Line(points={{20,0},{20,
            10},{-10,10},{-10,0}}, color={176,0,0}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>DC machine (permanent magnet) start-up and step-load.</p>
<p><i>See for example:</i>
<pre>
  power.p                      dc power
  loadInertia.w                angular velocity load
  loadInertia.flange_p.tau     torque on load
  efficiency.eta               efficiency
</pre></p>
<p>See also example DCcharSpeed.</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(StopTime=3),
    experimentSetupOutput);
  end DCmotor_pm;

  model BLDC "Brushless DC motor"

    inner PowerSystems.System system(f_nom=60,
      ini="tr",
      ref="inertial")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(           pol=0, V_nom=100)
                                      annotation (Placement(transformation(
            extent={{-80,-20},{-60,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Psensor power annotation (Placement(transformation(
            extent={{-50,-20},{-30,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Efficiency efficiency(
      av=true,
      tcst=0.1,
      m=5)      annotation (Placement(transformation(extent={{-20,-20},{0,0}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Drives.BLDC bldcm(
      motor(par=bldc100_1k),
      rotor(J=0.02),
      redeclare PowerSystems.AC3ph.Inverters.Inverter inverter(redeclare final
          PowerSystems.Control.Modulation.BlockM modulator
          "block modulation (no PWM)",
          redeclare PowerSystems.AC3ph.Inverters.Components.InverterSwitch inverter
          "switch, no diode, no losses") "inverter with modulator")
                             annotation (Placement(transformation(extent={{10,
              -20},{30,0}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.Rotor loadInertia(J=0.03)
    annotation (Placement(transformation(extent={{40,-20},{60,0}}, rotation=0)));
    PowerSystems.Mechanics.Rotation.FrictionTorque frictTorq(cFrict={0.01,0.0002})
      annotation (Placement(transformation(extent={{70,-20},{90,0}}, rotation=0)));
    Modelica.Mechanics.Rotational.Sources.TorqueStep torqueStep(
      startTime=1.5, stepTorque=-10,
      useSupport=false)
                annotation (Placement(transformation(extent={{90,20},{70,40}},
            rotation=0)));
    parameter PowerSystems.Examples.Spot.Data.Machines.BLDC100V_1kVA bldc100_1k
                                           annotation (Placement(transformation(
            extent={{-60,80},{-20,100}}, rotation=0)));
    PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC100V_10A idealSC100V_10A
      annotation (Placement(transformation(extent={{0,80},{40,100}}, rotation=0)));

  equation
    connect(grd.term, voltage.neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(bldcm.heat, efficiency.heat) annotation (Line(points={{20,0},{20,10},
            {-10,10},{-10,0}}, color={176,0,0}));
    connect(voltage.term, power.term_p) annotation (Line(points={{-60,-10},{-50,
            -10}}, color={0,0,255}));
    connect(power.term_n, efficiency.term_p) annotation (Line(points={{-30,-10},
            {-20,-10}}, color={0,0,255}));
    connect(efficiency.term_n, bldcm.term)
      annotation (Line(points={{0,-10},{10,-10}}, color={0,0,255}));
    connect(bldcm.flange, loadInertia.flange_p)
      annotation (Line(points={{30,-10},{40,-10}}, color={0,0,0}));
    connect(loadInertia.flange_n, frictTorq.flange)
      annotation (Line(points={{60,-10},{70,-10}}, color={0,0,0}));
    connect(loadInertia.flange_n, torqueStep.flange) annotation (Line(points={{
            60,-10},{64,-10},{64,30},{70,30}}, color={0,0,0}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>Brushless DC machine (permanent magnet synchronous machine) start-up and step-load.</p>
<p><i>See for example:</i>
<pre>
  power.p                      dc power
  loadInertia.w                angular velocity load
  loadInertia.flange_p.tau     torque on load
  efficiency.eta               efficiency including semiconductor losses
</pre></p>
<p>See also example BLDCcharSpeed.</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>
"), Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(
        StopTime=3,
        Tolerance=1e-005,
        Algorithm="Dassl"),
    experimentSetupOutput(events=false));
  end BLDC;

  model DCcharSpeed "DC pm: torque - speed characteristic"

    inner PowerSystems.System system(f_nom=60, sim="st")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(           pol=0, V_nom=100)
                                      annotation (Placement(transformation(
            extent={{-80,-20},{-60,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Efficiency efficiency(
      av=true,
      tcst=0.1,
      m=2)      annotation (Placement(transformation(extent={{-40,-20},{-20,0}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Drives.DCMpm machine(rotor(J=0.02), motor(par=DCpm100_1k))
                                          annotation (Placement(transformation(
            extent={{0,-20},{20,0}}, rotation=0)));
    PowerSystems.Blocks.Signals.Transient speedSignal(
      s_ini=0, s_fin=160)
      annotation (Placement(transformation(extent={{100,-20},{80,0}}, rotation=
              0)));
    PowerSystems.Mechanics.Rotation.Speed speed(tcst=0.01,
      w0=100,
      scType_par=false)
      annotation (Placement(transformation(extent={{60,-20},{40,0}}, rotation=0)));
    PowerSystems.Examples.Spot.Data.Machines.DCpm100V_1kVA DCpm100_1k
                                           annotation (Placement(transformation(
            extent={{-60,80},{-20,100}}, rotation=0)));

  equation
    connect(grd.term, voltage.neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(speedSignal.y, speed.w) annotation (Line(points={{80,-10},{60,-10}},
          color={0,0,127}));
    connect(voltage.term, efficiency.term_p) annotation (Line(points={{-60,-10},
            {-40,-10}}, color={0,0,255}));
    connect(efficiency.term_n, machine.term)
      annotation (Line(points={{-20,-10},{0,-10}}, color={0,0,255}));
    connect(machine.flange, speed.flange)
      annotation (Line(points={{20,-10},{40,-10}}, color={0,0,0}));
    connect(machine.heat, efficiency.heat) annotation (Line(points={{10,0},{10,
            10},{-30,10},{-30,0}}, color={176,0,0}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>DC machine (permanent magnet) torque-speed characteristic.</p>
<p></p>
<p><i>See for example as a function of phase:</i>
<pre>
  efficiency.eta          efficiency
  machine.motor.w_el       angular velocity (el)
  machine.motor.tau_el     torque (el)
</pre>
(right click dcm_pm.motor.w_el and choose Independent variable: w_el).</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(Tolerance=1e-005, Algorithm="Dassl"),
    experimentSetupOutput(events=false));
  end DCcharSpeed;

  model BLDCcharSpeed "BLDC: torque - speed characteristic"

    inner PowerSystems.System system(f_nom=60, sim="st")
    annotation (Placement(transformation(extent={{-100,80},{-80,100}}, rotation=
             0)));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-20},{-100,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(V_nom=100, pol=0)
                                      annotation (Placement(transformation(
            extent={{-80,-20},{-60,0}}, rotation=0)));
    PowerSystems.AC1ph_DC.Sensors.Efficiency efficiency(
      tcst=0.1, m=3)
                annotation (Placement(transformation(extent={{-40,-20},{-20,0}},
            rotation=0)));
    PowerSystems.AC1ph_DC.Drives.BLDC machine(
      rotor(J=0.02),
      motor(par=bldc100_1k),
      redeclare PowerSystems.AC3ph.Inverters.InverterAverage inverter(final
          modulation=
            3, par=idealSC100V_10A) "inverter time-average")
                             annotation (Placement(transformation(extent={{0,
              -20},{20,0}}, rotation=0)));
    PowerSystems.Blocks.Signals.Transient speedSignal(
      s_ini=0, s_fin=160)
      annotation (Placement(transformation(extent={{100,-20},{80,0}}, rotation=
              0)));
    PowerSystems.Mechanics.Rotation.Speed speed(tcst=0.01, scType_par=false)
      annotation (Placement(transformation(extent={{60,-20},{40,0}}, rotation=0)));
    PowerSystems.Examples.Spot.Data.Machines.BLDC100V_1kVA bldc100_1k
                                           annotation (Placement(transformation(
            extent={{-60,80},{-20,100}}, rotation=0)));
    PowerSystems.Examples.Spot.Data.Semiconductors.IdealSC100V_10A idealSC100V_10A
      annotation (Placement(transformation(extent={{0,80},{40,100}}, rotation=0)));

  equation
    connect(grd.term, voltage.neutral) annotation (Line(points={{-80,-10},{-80,
            -10}}, color={0,0,255}));
    connect(voltage.term, efficiency.term_p) annotation (Line(points={{-60,-10},
            {-40,-10}}, color={0,0,255}));
    connect(machine.heat, efficiency.heat)
                                         annotation (Line(points={{10,0},{10,10},
            {-30,10},{-30,0}}, color={176,0,0}));
    connect(efficiency.term_n, machine.term)
      annotation (Line(points={{-20,-10},{0,-10}}, color={0,0,255}));
    connect(machine.flange, speed.flange)
      annotation (Line(points={{20,-10},{40,-10}}, color={0,0,0}));
    connect(speedSignal.y, speed.w) annotation (Line(points={{80,-10},{60,-10}},
          color={0,0,127}));
  annotation (
    Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
    Documentation(
            info="<html>
<p>Brushless DC machine (permanent magnet synchronous machine) torque-speed characteristic.</p>
<p>This example uses rectangular modulation with constant voltage amplitude and width=2/3, corresponding to 2 phases on, 1 phase off (no additional PWM).</p>
<p><i>See for example as a function of phase:</i>
<pre>
  efficiency.eta          efficiency
  machine.motor.w_el       angular velocity (el)
  machine.motor.tau_el     torque (el)
</pre>
(right click dcm_pm.motor.w_el and choose Independent variable: w_el).</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    experiment(Tolerance=1e-005, Algorithm="Dassl"),
    experimentSetupOutput(events=false));
  end BLDCcharSpeed;

  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.41,
  width=0.4,
  height=0.42,
  library=1,
  autolayout=1),
Documentation(info="<html>
<p>DC drives (motors electrical and mechanical).</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end DrivesDC;
