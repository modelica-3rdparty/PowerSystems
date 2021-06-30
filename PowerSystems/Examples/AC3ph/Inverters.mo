within PowerSystems.Examples.AC3ph;
package Inverters "Inverters dq0"
  extends Modelica.Icons.ExamplesPackage;

  model Rectifier "Rectifier"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system(dynType=PowerSystems.Types.Dynamics.FixedInitial,
      refType=PowerSystems.Types.ReferenceFrame.Inertial)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.Blocks.Signals.TransientPhasor transPh(
      t_change=0.1,
      t_duration=0.1,
      a_start=2,
      a_end=1)
         annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
    PowerSystems.AC3ph.Sources.Voltage vAC(use_vPhasor_in=true, V_nom=100)
          annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
    PowerSystems.AC3ph.Impedances.Inductor ind(r=0.05,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{-50,0},{-30,20}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterAC(abc=true,
      av=true,
      tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{-20,0},{0,20}})));
    PowerSystems.AC3ph.Inverters.Rectifier rectifier(redeclare model Rectifier =
      PowerSystems.AC3ph.Inverters.Components.RectifierEquation(redeclare
            record Data =
              PowerSystems.Examples.Data.Semiconductors.IdealSC100V_10A))
      annotation (Placement(transformation(extent={{30,0},{10,20}})));

    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{40,0},{60,20}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0, V_nom=100)
      annotation (Placement(transformation(extent={{90,0},{70,20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd1
                               annotation (Placement(transformation(extent={{
              -80,0},{-100,20}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd2
                               annotation (Placement(transformation(extent={{90,
              0},{110,20}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{10,20},{30,40}})));

  equation
    connect(vAC.term, ind.term_p) annotation (Line(points={{-60,10},{-50,10}},
          color={0,110,110}));
    connect(ind.term_n, meterAC.term_p) annotation (Line(points={{-30,10},{-20,
            10}}, color={0,110,110}));
    connect(meterAC.term_n, rectifier.AC)
      annotation (Line(points={{0,10},{10,10}}, color={0,110,110}));
    connect(rectifier.DC, meterDC.term_p)
      annotation (Line(points={{30,10},{40,10}}, color={0,0,255}));
    connect(meterDC.term_n, vDC.term)
      annotation (Line(points={{60,10},{70,10}}, color={0,0,255}));
    connect(transPh.y, vAC.vPhasor_in)
      annotation (Line(points={{-80,30},{-64,30},{-64,20}}, color={0,0,127}));
    connect(grd1.term, vAC.neutral)
      annotation (Line(points={{-80,10},{-80,10}}, color={0,0,255}));
    connect(vDC.neutral, grd2.term)
      annotation (Line(points={{90,10},{90,10}}, color={0,0,255}));
    connect(rectifier.heat, bdCond.heat)
      annotation (Line(points={{20,20},{20,20}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>3-phase rectifier. Compare 'equation' and 'modular' version.</p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Inverters\">up users guide</a></p>
</html>
"),   experiment(
        StopTime=0.2,
        Interval=0.2e-3,
        Tolerance=1e-005));
  end Rectifier;

  model InverterToLoad "Inverter to load"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Inertial)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd
                              annotation (Placement(transformation(extent={{-80,
              -20},{-100,0}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0, V_nom=100)
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{-50,-20},{-30,0}})));
    PowerSystems.AC3ph.Inverters.Inverter inverter(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.Components.InverterEquation(redeclare
            record Data =
            PowerSystems.Examples.Data.Semiconductors.IdealSC100V_10A)
        "equation, with losses")
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    PowerSystems.AC3ph.Inverters.Select select(
      fType=PowerSystems.Types.SourceFrequency.Parameter,
      f=100,
      use_vPhasor_in=true)
      annotation (Placement(transformation(extent={{-20,20},{0,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterAC(
      abc=true,
      av=true,
      tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{10,-20},{30,0}})));
    PowerSystems.Blocks.Signals.TransientPhasor vCtrl(
      t_change=0.05,
      t_duration=0.05,
      a_start=0)
         annotation (Placement(transformation(extent={{-50,40},{-30,60}})));
    PowerSystems.AC3ph.Loads.PQindLoad pqLoad(
      tcst=0.01, imax=1,
      V_nom=100,
      S_nom=1e3)                                 annotation (Placement(
          transformation(extent={{40,-20},{60,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{-20,0},{0,20}})));

  equation
    connect(vDC.term, meterDC.term_p)
      annotation (Line(points={{-60,-10},{-50,-10}}, color={0,0,255}));
    connect(meterDC.term_n, inverter.DC)
      annotation (Line(points={{-30,-10},{-20,-10}}, color={0,0,255}));
    connect(inverter.AC, meterAC.term_p)
      annotation (Line(points={{0,-10},{10,-10}}, color={0,120,120}));
    connect(select.theta_out, inverter.theta)
      annotation (Line(points={{-16,20},{-16,0}}, color={0,0,127}));
    connect(select.vPhasor_out,inverter.vPhasor)  annotation (Line(points={{-4,
            20},{-4,0}}, color={0,0,127}));
    connect(vCtrl.y,select.vPhasor_in)
      annotation (Line(points={{-30,50},{-4,50},{-4,40}}, color={0,0,127}));
    connect(meterAC.term_n, pqLoad.term) annotation (Line(points={{30,-10},{40,
            -10}}, color={0,120,120}));
    connect(grd.term, vDC.neutral)
      annotation (Line(points={{-80,-10},{-80,-10}}, color={0,0,255}));
    connect(inverter.heat, bdCond.heat)
      annotation (Line(points={{-10,0},{-10,0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>3-phase inverter, feeding load at constant 100Hz with increasing amplitude.</p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Inverters\">up users guide</a></p>
</html>
"),   experiment(
        StopTime=0.2,
        Interval=0.2e-3,
        Tolerance=1e-005));
  end InverterToLoad;

  model InverterToGrid "Inverter to grid"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system(refType=PowerSystems.Types.ReferenceFrame.Synchron)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0,
      V_nom=100,
      v0=2)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(tcst=0.1,
      V_nom=100,
      S_nom=1e3,
      av=true)
      annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Inverters.Inverter inverter(redeclare model Inverter =
        PowerSystems.AC3ph.Inverters.Components.InverterEquation(redeclare
            record Data =
              PowerSystems.Examples.Data.Semiconductors.IdealSC100V_10A)
        "equation, with losses")
      annotation (Placement(transformation(extent={{-30,-20},{-10,0}})));
    PowerSystems.AC3ph.Inverters.Select select(use_vPhasor_in=true)
                                  annotation (Placement(transformation(extent={
              {-30,20},{-10,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterAC(
      V_nom=100,
      S_nom=1e3,
      av=true,
      tcst=0.1,
      abc=true)
      annotation (Placement(transformation(extent={{0,-20},{20,0}})));
    PowerSystems.AC3ph.Impedances.Inductor ind(r=0.05,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{50,-20},{30,0}})));
    PowerSystems.AC3ph.Sources.Voltage vAC(V_nom=100)
          annotation (Placement(transformation(extent={{80,-20},{60,0}})));
    PowerSystems.Blocks.Signals.TransientPhasor vCtrl(
      t_change=0.1,
      t_duration=0.1,
      ph_end=0.5235987755983)
         annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd
                               annotation (Placement(transformation(extent={{80,
              -20},{100,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=3) annotation (Placement(
          transformation(extent={{-30,0},{-10,20}})));

  equation
    connect(vAC.term, ind.term_p) annotation (Line(points={{60,-10},{50,-10}},
          color={0,110,110}));
    connect(vDC.term, meterDC.term_p)
      annotation (Line(points={{-70,-10},{-60,-10}}, color={0,0,255}));
    connect(meterDC.term_n, inverter.DC)
      annotation (Line(points={{-40,-10},{-30,-10}}, color={0,0,255}));
    connect(inverter.AC, meterAC.term_p) annotation (Line(points={{-10,-10},{0,
            -10}}, color={0,120,120}));
    connect(meterAC.term_n, ind.term_n) annotation (Line(points={{20,-10},{30,
            -10}}, color={0,120,120}));
    connect(select.theta_out, inverter.theta) annotation (Line(points={{-26,20},
            {-26,0}}, color={0,0,127}));
    connect(select.vPhasor_out,inverter.vPhasor)  annotation (Line(points={{-14,
            20},{-14,0}}, color={0,0,127}));
    connect(vCtrl.y,select.vPhasor_in)
      annotation (Line(points={{-40,50},{-14,50},{-14,40}}, color={0,0,127}));
    connect(vAC.neutral, grd.term)
      annotation (Line(points={{80,-10},{80,-10}}, color={0,0,255}));
    connect(inverter.heat, bdCond.heat)
      annotation (Line(points={{-20,0},{-20,0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>3-phase inverter, feeding into grid with increasing phase. Compare 'switch', 'equation' and 'modular' version.</p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Inverters\">up users guide</a></p>
</html>"),
      experiment(
        StopTime=0.2,
        Interval=0.2e-3,
        Tolerance=1e-005));
  end InverterToGrid;

  model InverterAvToGrid "Inverter to grid"
    extends Modelica.Icons.Example;

    inner PowerSystems.System system(dynType=PowerSystems.Types.Dynamics.FixedInitial)
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    PowerSystems.AC1ph_DC.Sources.DCvoltage vDC(pol=0,
      V_nom=100,
      v0=2)
      annotation (Placement(transformation(extent={{-90,-20},{-70,0}})));
    PowerSystems.AC1ph_DC.Sensors.PVImeter meterDC(av=true, tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
    PowerSystems.AC3ph.Inverters.InverterAverage inverter(redeclare record Data =
      PowerSystems.Examples.Data.Semiconductors.IdealSC100V_10A)
      annotation (Placement(transformation(extent={{-30,-20},{-10,0}})));
    PowerSystems.AC3ph.Inverters.Select select(use_vPhasor_in=true)
                                  annotation (Placement(transformation(extent={
              {-30,20},{-10,40}})));
    PowerSystems.AC3ph.Sensors.PVImeter meterAC(
      abc=true,
      av=true,
      tcst=0.1,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{0,-20},{20,0}})));
    PowerSystems.AC3ph.Impedances.Inductor ind(r=0.05,
      V_nom=100,
      S_nom=1e3)
      annotation (Placement(transformation(extent={{50,-20},{30,0}})));
    PowerSystems.AC3ph.Sources.Voltage vAC(V_nom=100)
          annotation (Placement(transformation(extent={{80,-20},{60,0}})));
    PowerSystems.Blocks.Signals.TransientPhasor vCtrl(
      t_change=0.1,
      t_duration=0.1,
      ph_end=0.5235987755983)
         annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
    PowerSystems.AC3ph.Nodes.GroundOne grd
                               annotation (Placement(transformation(extent={{80,
              -20},{100,0}})));
    PowerSystems.Common.Thermal.BdCondV bdCond(m=1) annotation (Placement(
          transformation(extent={{-30,0},{-10,20}})));

  equation
    connect(vAC.term, ind.term_p) annotation (Line(points={{60,-10},{50,-10}},
          color={0,110,110}));
    connect(vDC.term, meterDC.term_p)
      annotation (Line(points={{-70,-10},{-60,-10}}, color={0,0,255}));
    connect(meterDC.term_n, inverter.DC)
      annotation (Line(points={{-40,-10},{-30,-10}}, color={0,0,255}));
    connect(inverter.AC, meterAC.term_p) annotation (Line(points={{-10,-10},{0,
            -10}}, color={0,120,120}));
    connect(meterAC.term_n, ind.term_n) annotation (Line(points={{20,-10},{30,
            -10}}, color={0,120,120}));
    connect(select.theta_out, inverter.theta) annotation (Line(points={{-26,20},
            {-26,0}}, color={0,0,127}));
    connect(select.vPhasor_out,inverter.vPhasor)  annotation (Line(points={{-14,
            20},{-14,0}}, color={0,0,127}));
    connect(vCtrl.y,select.vPhasor_in)
      annotation (Line(points={{-40,50},{-14,50},{-14,40}}, color={0,0,127}));
    connect(vAC.neutral, grd.term)
      annotation (Line(points={{80,-10},{80,-10}}, color={0,0,255}));
    connect(inverter.heat, bdCond.heat)
      annotation (Line(points={{-20,0},{-20,0}}, color={176,0,0}));
    annotation (
      Documentation(
              info="<html>
<p>3-phase inverter based on AVERAGED switch-equation, feeding into grid with increasing phase.</p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Inverters\">up users guide</a></p>
</html>"),
      experiment(
        StopTime=0.2,
        Interval=0.2e-3));
  end InverterAvToGrid;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Comparison of different three-phase rectifier and inverter models.</p>
<p><a href=\"modelica://PowerSystems.Examples\">up users guide</a></p>
</html>
"));
end Inverters;
