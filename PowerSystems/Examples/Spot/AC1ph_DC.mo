within PowerSystems.Examples.Spot;
package AC1ph_DC "AC 1-phase and DC components"
  extends Modelica.Icons.ExamplesPackage;

  model Breaker "Breaker"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.Control.Relays.SwitchRelay relay(
      n=1,
      ini_state=true,
      t_switch={0.1})
        annotation (Placement(transformation(
          origin={50,70},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(V_nom=10e3, scType_par=false)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Impedances.Inductor ind(r={0.1,0.1},
      V_nom=10e3,
      S_nom=1e6)
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter(V_nom=10e3, S_nom=1e6)
                                           annotation (Placement(transformation(
            extent={{-10,-10},{10,10}})));
    replaceable PowerSystems.AC1ph_DC.Breakers.Breaker breaker(V_nom=10e3, I_nom=100)
                                            annotation (Placement(
          transformation(extent={{40,-10},{60,10}})));
    PowerSystems.AC1ph_DC.Nodes.Ground grd      annotation (Placement(transformation(
            extent={{90,-10},{110,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
      annotation (Line(points={{-80,20},{-54,20},{-54,10}}, color={0,0,127}));
    connect(voltage.term, ind.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(ind.term_n, meter.term_p)
      annotation (Line(points={{-20,0},{-10,0}}, color={0,0,255}));
    connect(meter.term_n, breaker.term_p)
      annotation (Line(points={{10,0},{40,0}}, color={0,0,255}));
    connect(breaker.term_n, grd.term)
      annotation (Line(points={{60,0},{90,0}}, color={0,0,255}));
    connect(relay.y[1], breaker.control)
      annotation (Line(points={{50,60},{50,10}}, color={255,0,255}));
    connect(grd1.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=0.2, Interval=1e-4));
  end Breaker;

  model Fault "Fault"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Control.Relays.SwitchRelay relay1(                       n=2, t_switch=
         {3.5,29.5}/50)
      annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
    PowerSystems.Control.Relays.SwitchRelay relay2(                       n=2, t_switch=
         {3.6,29.4}/50)
      annotation (Placement(transformation(extent={{80,0},{60,20}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage1(V_nom=10e3, alpha0=
          0.17453292519943)
      annotation (Placement(transformation(extent={{-90,-40},{-70,-20}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage2(V_nom=10e3)
      annotation (Placement(transformation(extent={{90,-40},{70,-20}})));
    PowerSystems.AC1ph_DC.Breakers.DoubleSwitch switch1(V_nom=10e3,I_nom=100)
      annotation (Placement(transformation(extent={{-60,-40},{-40,-20}})));
    PowerSystems.AC1ph_DC.Breakers.DoubleSwitch switch2(V_nom=10e3, I_nom=100)
                                            annotation (Placement(
          transformation(extent={{40,-40},{60,-20}})));
    PowerSystems.AC1ph_DC.Lines.FaultRXline line(par(V_nom = 10e3, S_nom=1e6))
                                           annotation (Placement(transformation(
            extent={{-10,-40},{10,-20}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter(V_nom=10e3, S_nom=1e6)
                                            annotation (Placement(
          transformation(
          extent={{-10,-10},{10,10}},
          rotation=90)));
    replaceable PowerSystems.AC1ph_DC.Faults.Fault_Ab fault_Ab
                                              annotation (Placement(
          transformation(extent={{-10,40},{10,60}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-90,-40},{-110,-20}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-40},{110,-20}})));

  equation
    connect(voltage1.term, switch1.term_p)
                                          annotation (Line(points={{-70,-30},{
            -60,-30}}, color={0,0,255}));
    connect(switch1.term_n, line.term_p) annotation (Line(points={{-40,-30},{
            -10,-30}}, color={0,0,255}));
    connect(line.term_n, switch2.term_p)
      annotation (Line(points={{10,-30},{40,-30}}, color={0,0,255}));
    connect(switch2.term_n, voltage2.term)
      annotation (Line(points={{60,-30},{70,-30}}, color={0,0,255}));
    connect(line.term_f, meter.term_p) annotation (Line(points={{0,-20},{0,-10},
            {-6.12303e-016,-10}}, color={0,0,255}));
    connect(meter.term_n,fault_Ab. term) annotation (Line(points={{6.12303e-016,
            10},{0,10},{0,40}}, color={0,0,255}));
    connect(relay1.y, switch1.control) annotation (Line(points={{-60,10},{-50,
            10},{-50,-20}}, color={255,0,255}));
    connect(relay2.y, switch2.control) annotation (Line(points={{60,10},{50,10},
            {50,-20}}, color={255,0,255}));
    connect(grd1.term, voltage1.neutral) annotation (Line(points={{-90,-30},{
            -90,-30}}, color={0,0,255}));
    connect(voltage2.neutral, grd2.term)
      annotation (Line(points={{90,-30},{90,-30}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>
"),   experiment(StopTime=0.2, Interval=1e-4));
  end Fault;

  model Impedance "Impedance"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(scType_par=false)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter      annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.Impedances.Inductor ind(r={0.1,0.1})
                                            annotation (Placement(
          transformation(extent={{20,-10},{40,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1
                                annotation (Placement(transformation(extent={{
              -70,-10},{-90,10}})));
    PowerSystems.AC1ph_DC.Nodes.Ground grd2     annotation (Placement(transformation(
            extent={{80,-10},{100,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
      annotation (Line(points={{-80,20},{-54,20},{-54,10}}, color={0,0,127}));
    connect(voltage.term, meter.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(meter.term_n, ind.term_p)
      annotation (Line(points={{-20,0},{20,0}}, color={0,0,255}));
    connect(ind.term_n, grd2.term)
      annotation (Line(points={{40,0},{80,0}}, color={0,0,255}));
    connect(grd1.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=0.2, Interval=2.7e-4));
  end Impedance;

  model ImpedanceOneTerm "Impedance One-terminal"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(scType_par=false)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter      annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.ImpedancesOneTerm.Inductor ind(r=0.1)
      annotation (Placement(transformation(extent={{30,-10},{50,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
      annotation (Line(points={{-80,20},{-54,20},{-54,10}}, color={0,0,127}));
    connect(voltage.term, meter.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(meter.term_n, ind.term)
      annotation (Line(points={{-20,0},{30,0}}, color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=0.2, Interval=2.7e-4));
  end ImpedanceOneTerm;

  model Line "Line"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh(ph_fin=
          0.087266462599716)
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage1(
      V_nom=132e3,
      scType_par=false,
      alpha0=0.087266462599716)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage2(V_nom=132e3)
      annotation (Placement(transformation(extent={{90,-10},{70,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter(V_nom=132e3, S_nom=100e6)
                                            annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.Lines.PIline line(redeclare replaceable parameter
        PowerSystems.AC1ph_DC.Lines.Parameters.PIline           par(
                                                 V_nom=132e3))
                                      annotation (Placement(transformation(
            extent={{20,-10},{40,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-10},{110,10}})));

  equation
    connect(transPh.y, voltage1.vPhasor)
      annotation (Line(points={{-80,20},{-54,20},{-54,10}}, color={0,0,127}));
    connect(voltage1.term, meter.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(meter.term_n, line.term_p)
      annotation (Line(points={{-20,0},{20,0}}, color={0,0,255}));
    connect(line.term_n, voltage2.term)
      annotation (Line(points={{40,0},{70,0}}, color={0,0,255}));
    connect(grd1.term, voltage1.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    connect(voltage2.neutral, grd2.term)
      annotation (Line(points={{90,0},{90,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=0.2, Interval=1e-4));
  end Line;

  model LoadAC "AC load"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.Transient[2] trsSignal(s_ini={1,2}, s_fin={2,3})
      annotation (Placement(transformation(
          origin={40,60},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.Blocks.Signals.TransientPhasor transPh(a_fin=0.9)
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(scType_par=false)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter      annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.Loads.ZloadAC zLoadAC
                                           annotation (Placement(transformation(
            extent={{30,-10},{50,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
      annotation (Line(points={{-80,20},{-54,20},{-54,10}}, color={0,0,127}));
    connect(voltage.term, meter.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(meter.term_n, zLoadAC.term)
      annotation (Line(points={{-20,0},{30,0}}, color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    connect(trsSignal.y, zLoadAC.p_set)
      annotation (Line(points={{40,50},{40,10}}, color={0,0,127}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=1, Interval=1e-4));
  end LoadAC;

  model LoadDC "AC load"

    inner PowerSystems.System system(ref="inertial")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.Transient trsSignalL(s_ini=0.5, s_fin=1)
      annotation (Placement(transformation(
          origin={40,60},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage(scType_par=false)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter      annotation (Placement(
          transformation(extent={{-40,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.Loads.PindLoadDC pLoadDC
                                              annotation (Placement(
          transformation(extent={{30,-10},{50,10}})));
    PowerSystems.Blocks.Signals.Transient transV(s_fin=0.9)
                                        annotation (Placement(transformation(
            extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));

  equation
    connect(voltage.term, meter.term_p)
      annotation (Line(points={{-50,0},{-40,0}}, color={0,0,255}));
    connect(meter.term_n, pLoadDC.term)
      annotation (Line(points={{-20,0},{30,0}}, color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    connect(transV.y, voltage.vDC) annotation (Line(points={{-80,20},{-54,20},{
            -54,10}}, color={0,0,127}));
    connect(trsSignalL.y, pLoadDC.p_set)
      annotation (Line(points={{40,50},{40,10}}, color={0,0,127}));
    annotation (
      Documentation(
              info="<html>
</html>"),
      experiment(StopTime=1, Interval=2.7e-4));
  end LoadDC;

  model Machines "Machines"

    inner PowerSystems.System system(ref="inertial")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Mechanics.Rotation.Rotor rotor
      annotation (Placement(transformation(extent={{28,-10},{48,10}})));
    PowerSystems.Mechanics.Rotation.Torque torq(tau0=-1)
                                              annotation (Placement(
          transformation(extent={{80,-10},{60,10}})));
    PowerSystems.Blocks.Signals.Transient transTau(s_ini=0, s_fin=-1)
                                       annotation (Placement(transformation(
            extent={{100,-10},{80,10}})));

    PowerSystems.AC1ph_DC.Sources.DCvoltage voltage1(scType_par=false, V_nom=
          100)                        annotation (Placement(transformation(
            extent={{-80,-10},{-60,10}})));
    PowerSystems.Blocks.Signals.Transient transV(s_ini=0, s_fin=1)
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sensors.Psensor power
                                 annotation (Placement(transformation(extent={{
              -50,-10},{-30,10}})));
    replaceable PowerSystems.AC1ph_DC.Machines.DCser motor(par(V_nom=100, S_nom=1e3))
      "DC machine series"                   annotation (Placement(
          transformation(extent={{-10,-10},{10,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-10},{-100,10}})));
    PowerSystems.Common.Thermal.BoundaryV boundary(m=2)
      annotation (Placement(transformation(extent={{-10,10},{10,30}})));

  equation
    connect(voltage1.term, power.term_p)
      annotation (Line(points={{-60,0},{-50,0}}, color={0,0,255}));
    connect(power.term_n, motor.term)
      annotation (Line(points={{-30,0},{-10,0}}, color={0,0,255}));
    connect(motor.airgap, rotor.flange_p) annotation (Line(points={{0,6},{14,6},
            {14,0},{28,0}}, color={0,0,0}));
    connect(rotor.flange_n, torq.flange)
      annotation (Line(points={{48,0},{60,0}}, color={0,0,0}));
    connect(grd.term, voltage1.neutral)
      annotation (Line(points={{-80,0},{-80,0}}, color={0,0,255}));
    connect(transV.y, voltage1.vDC) annotation (Line(points={{-80,20},{-64,20},
            {-64,10}}, color={0,0,127}));
    connect(motor.heat, boundary.heat)
      annotation (Line(points={{0,10},{0,10}}, color={176,0,0}));
    connect(transTau.y, torq.tau)
      annotation (Line(points={{80,0},{80,0}}, color={0,0,127}));
    annotation (
      Documentation(
              info="<html>
</html>
"),   experiment(StopTime=1));
  end Machines;

  model Sensor "Sensor and meter"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Resistor res
      annotation (Placement(transformation(extent={{80,-10},{100,10}})));
    replaceable PowerSystems.AC1ph_DC.Sensors.PVImeter meter
                                            annotation (Placement(
          transformation(extent={{0,-10},{20,10}})));
    PowerSystems.AC1ph_DC.Sources.Vspectrum voltage
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-70,-10},{-90,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
                                    annotation (Line(points={{-80,20},{-54,20},
            {-54,10}}, color={0,0,127}));
    connect(voltage.term, meter.term_p) annotation (Line(points={{-50,0},{0,0}},
          color={0,0,255}));
    connect(meter.term_n, res.term) annotation (Line(points={{20,0},{80,0}},
          color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>
"),      experiment(StopTime=0.2, Interval=2.7e-4));
  end Sensor;

  model Source "Source"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    replaceable PowerSystems.AC1ph_DC.Sources.ACvoltage voltage
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter      annotation (Placement(
          transformation(extent={{40,-10},{60,10}})));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Inductor ind
      annotation (Placement(transformation(extent={{70,-10},{90,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-40,-10},{-60,10}})));

  equation
    connect(voltage.term, meter.term_p) annotation (Line(points={{-20,0},{40,0}},
          color={0,0,255}));
    connect(meter.term_n, ind.term) annotation (Line(points={{60,0},{70,0}},
          color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-40,0},{-40,0}}, color={0,0,255}));
    annotation (
      Documentation(
              info="<html>
</html>
"),      experiment(StopTime=0.2, Interval=2.7e-4));
  end Source;

  model Transformer "Transformer"

    inner PowerSystems.System system(ref="inertial", ini="tr")
      annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
                    annotation (Placement(transformation(extent={{-100,10},{-80,
              30}})));
    PowerSystems.Control.Relays.TapChangerRelay TapChanger(
      preset_1={0,1,2},
      preset_2={0,1,2},
      t_switch_1={0.9,1.9},
      t_switch_2={1.1,2.1})
      annotation (Placement(transformation(
          origin={10,60},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    PowerSystems.AC1ph_DC.Sources.ACvoltage voltage(scType_par=false)
      annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter1
      annotation (Placement(transformation(extent={{-50,-10},{-30,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meter2(V_nom=10)
      annotation (Placement(transformation(extent={{50,-10},{70,10}})));
    replaceable PowerSystems.AC1ph_DC.Transformers.TrafoStray trafo(par(
      v_tc1 = {1,1.1},
      v_tc2 = {1,1.2},
      V_nom = {1,10}))
                    annotation (Placement(transformation(extent={{0,-10},{20,10}})));
    PowerSystems.AC1ph_DC.ImpedancesOneTerm.Resistor res(V_nom=10, r=100)
      annotation (Placement(transformation(extent={{80,-10},{100,10}})));
    PowerSystems.AC1ph_DC.Nodes.PolarityGround polGrd1(pol=0)
      annotation (Placement(transformation(extent={{80,-40},{100,-20}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd annotation (Placement(transformation(
            extent={{-80,-10},{-100,10}})));

  equation
    connect(transPh.y, voltage.vPhasor)
      annotation (Line(points={{-80,20},{-64,20},{-64,10}}, color={0,0,127}));
    connect(voltage.term, meter1.term_p)
      annotation (Line(points={{-60,0},{-50,0}}, color={0,0,255}));
    connect(meter1.term_n, trafo.term_p)
      annotation (Line(points={{-30,0},{0,0}}, color={0,0,255}));
    connect(trafo.term_n, meter2.term_p)
      annotation (Line(points={{20,0},{50,0}}, color={0,0,255}));
    connect(meter2.term_n, res.term)
      annotation (Line(points={{70,0},{80,0}}, color={0,0,255}));
    connect(res.term, polGrd1.term)
      annotation (Line(points={{80,0},{80,-30}}, color={0,0,255}));
    connect(grd.term, voltage.neutral)
      annotation (Line(points={{-80,0},{-80,0}}, color={0,0,255}));
    connect(TapChanger.tap_p, trafo.tap_p)
      annotation (Line(points={{6,50},{6,10}}, color={255,127,0}));
    connect(TapChanger.tap_n, trafo.tap_n) annotation (Line(points={{14,50},{14,
            10}}, color={255,127,0}));
    annotation (
      Documentation(
              info="<html>
</html>
"),      experiment(StopTime=3, Interval=4e-4));
  end Transformer;

  model Rectifier "Rectifier"

    inner PowerSystems.System system(ref="inertial", ini="tr")
                        annotation (Placement(transformation(extent={{-80,60},{
              -60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
         annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage vAC(V_nom=2, scType_par=false)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
    PowerSystems.AC1ph_DC.Impedances.Inductor ind
      annotation (Placement(transformation(extent={{-50,-10},{-30,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterAC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{-20,-10},{0,10}})));
    replaceable PowerSystems.AC1ph_DC.Inverters.Rectifier rectifier
      annotation (Placement(transformation(extent={{30,-10},{10,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{40,-10},{60,10}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0)
      annotation (Placement(transformation(extent={{90,-10},{70,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{-80,-10},{-100,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{90,-10},{110,10}})));
    PowerSystems.Common.Thermal.BoundaryV boundary(m=2)
      annotation (Placement(transformation(extent={{10,10},{30,30}})));

  equation
    connect(transPh.y, vAC.vPhasor)
      annotation (Line(points={{-80,20},{-64,20},{-64,10}}, color={0,0,127}));
    connect(vAC.term, ind.term_p)
      annotation (Line(points={{-60,0},{-50,0}}, color={0,0,255}));
    connect(ind.term_n, meterAC.term_p)
      annotation (Line(points={{-30,0},{-20,0}}, color={0,0,255}));
    connect(meterAC.term_n, rectifier.AC)
      annotation (Line(points={{0,0},{10,0}}, color={0,0,255}));
    connect(rectifier.DC, meterDC.term_p) annotation (Line(points={{30,0},{40,0}},
          color={0,0,255}));
    connect(meterDC.term_n, vDC.term)
      annotation (Line(points={{60,0},{70,0}}, color={0,0,255}));
    connect(grd1.term, vAC.neutral)
      annotation (Line(points={{-80,0},{-80,0}}, color={0,0,255}));
    connect(vDC.neutral, grd2.term)
      annotation (Line(points={{90,0},{90,0}}, color={0,0,255}));
    connect(rectifier.heat, boundary.heat)
      annotation (Line(points={{20,10},{20,10}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
</html>
"),   experiment(StopTime=0.2, Interval=0.2e-3));
  end Rectifier;

  model Inverter "Inverter, controlled rectifier"

    inner PowerSystems.System system(ref="inertial", ini="tr")
                        annotation (Placement(transformation(extent={{-80,60},{
              -60,80}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh
         annotation (Placement(transformation(extent={{-100,10},{-80,30}})));
    PowerSystems.AC1ph_DC.Sources.ACvoltage vAC(         scType_par=false)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
    PowerSystems.AC1ph_DC.Impedances.Inductor ind
      annotation (Placement(transformation(extent={{-50,-10},{-30,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterAC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{0,-10},{-20,10}})));
    replaceable PowerSystems.AC1ph_DC.Inverters.Inverter dc_ac
                                             annotation (Placement(
          transformation(extent={{30,-10},{10,10}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1)
      annotation (Placement(transformation(extent={{60,-10},{40,10}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0, V_nom=2)
      annotation (Placement(transformation(extent={{90,-10},{70,10}})));
    PowerSystems.AC1ph_DC.Inverters.Select select(alpha0=0.5235987755983)
                                   annotation (Placement(transformation(extent=
              {{30,40},{10,60}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd1 annotation (Placement(transformation(
            extent={{90,-10},{110,10}})));
    PowerSystems.AC1ph_DC.Nodes.GroundOne grd2 annotation (Placement(transformation(
            extent={{-80,-10},{-100,10}})));
    PowerSystems.Common.Thermal.BoundaryV boundary(m=2)
      annotation (Placement(transformation(extent={{10,10},{30,30}})));

  equation
    connect(transPh.y, vAC.vPhasor) annotation (Line(points={{-80,20},{-64,20},
            {-64,10}}, color={0,0,127}));
    connect(select.theta_out,dc_ac. theta)
      annotation (Line(points={{26,40},{26,10}}, color={0,0,127}));
    connect(select.uPhasor_out,dc_ac. uPhasor)
      annotation (Line(points={{14,40},{14,10}}, color={0,0,127}));
    connect(vDC.term, meterDC.term_p)
      annotation (Line(points={{70,0},{60,0}}, color={0,0,255}));
    connect(meterDC.term_n, dc_ac.DC)
      annotation (Line(points={{40,0},{30,0}}, color={0,0,255}));
    connect(dc_ac.AC, meterAC.term_p)
      annotation (Line(points={{10,0},{0,0}}, color={0,0,255}));
    connect(meterAC.term_n, ind.term_n)
      annotation (Line(points={{-20,0},{-30,0}}, color={0,0,255}));
    connect(ind.term_p, vAC.term)
      annotation (Line(points={{-50,0},{-60,0}}, color={0,0,255}));
    connect(grd1.term, vDC.neutral)
      annotation (Line(points={{90,0},{90,0}}, color={0,0,255}));
    connect(grd2.term, vAC.neutral)
      annotation (Line(points={{-80,0},{-80,0}}, color={0,0,255}));
    connect(dc_ac.heat, boundary.heat)
      annotation (Line(points={{20,10},{20,10}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
</html>
"),   experiment(StopTime=0.2, Interval=0.2e-3));
  end Inverter;

  annotation (preferredView="info",
Documentation(info="<html>
<p>This package contains small models for testing single components from AC1ph_DC.
The replaceable component can be replaced by a user defined component of similar type.</p>
<p><a href=\"PowerSystems.UsersGuide.Examples\">up users guide</a></p>
</html>"));
end AC1ph_DC;
