within PowerSystems;
package Interfaces
  extends Modelica.Icons.InterfacesPackage;

  connector Terminal "General power terminal"
    replaceable package PhaseSystem = PhaseSystems.PartialPhaseSystem
      "Phase system"
      annotation (choicesAllMatching=true);
    PhaseSystem.Voltage v[PhaseSystem.n] "voltage vector";
    flow PhaseSystem.Current i[PhaseSystem.n] "current vector";
    PhaseSystem.ReferenceAngle theta[PhaseSystem.m] if PhaseSystem.m > 0
      "optional vector of phase angles";
  end Terminal;

  connector Electric_p "Electric terminal ('positive')"
    extends Modelica.Electrical.Analog.Interfaces.Pin;
    annotation (defaultComponentName = "term_p",
  Documentation(info="<html>
</html>
"),
  Window(
    x=0.45,
    y=0.01,
    width=0.44,
    height=0.65),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-100,100},{100,-100}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{0,50},{100,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-120,120},{100,60}},
            lineColor={0,0,255},
            textString="%name")}));
  end Electric_p;

  connector Electric_n "Electric terminal ('negative')"
    extends Modelica.Electrical.Analog.Interfaces.Pin;
    annotation (defaultComponentName = "term_n",
  Documentation(info="<html>
</html>"),
  Window(
    x=0.45,
    y=0.01,
    width=0.44,
    height=0.65),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-100,100},{100,-100}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-100,50},{0,-50}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-100,120},{120,60}},
            lineColor={0,0,255},
            textString="%name")}));
  end Electric_n;

  connector Rotation_p = Modelica.Mechanics.Rotational.Interfaces.Flange_a;
  connector Rotation_n = Modelica.Mechanics.Rotational.Interfaces.Flange_b;

  connector Thermal_p "Thermal heat port ('positive')"
    extends Modelica.Thermal.HeatTransfer.Interfaces.HeatPort;

  annotation (defaultComponentName = "heat_p",
    Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={Rectangle(
            extent={{0,50},{100,-50}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid), Text(
            extent={{-120,120},{100,60}},
            lineColor={176,0,0},
            textString=
                 "%name")}),
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={Rectangle(
            extent={{-100,100},{100,-100}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid)}),
    Documentation(info="<html>
</html>
"));
  end Thermal_p;

  connector Thermal_n "Thermal heat port ('negative')"
    extends Modelica.Thermal.HeatTransfer.Interfaces.HeatPort;

  annotation (defaultComponentName = "heat_n",
    Documentation(info="<html>
</html>
"), Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={Rectangle(
            extent={{-100,50},{0,-50}},
            lineColor={176,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-100,120},{120,60}},
            lineColor={176,0,0},
            textString="%name")}),
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={Rectangle(
            extent={{-100,100},{100,-100}},
            lineColor={176,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Thermal_n;

  connector ThermalV_p "Thermal vector heat port ('positive')"
    parameter Integer m(final min=1) = 1 "number of single heat-ports";
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a[m] ports
      "vector of single heat ports";

  annotation (defaultComponentName = "heat_p",
    Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={
          Text(
            extent={{-120,120},{100,60}},
            lineColor={176,0,0},
            textString="%name"),
          Polygon(
            points={{-20,0},{40,-60},{100,0},{40,60},{-20,0}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-10,50},{90,-50}},
            lineColor={235,235,235},
            pattern=LinePattern.None,
            textString="%m")}),
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={Polygon(
            points={{-120,0},{0,-120},{120,0},{0,120},{-120,0}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid), Text(
            extent={{-60,60},{60,-60}},
            lineColor={255,255,255},
            pattern=LinePattern.None,
            textString="%m")}),
    Documentation(info="<html>
<p>Thermal connector with a vector of 'port's, positive.</p>
</html>
"));
  end ThermalV_p;

  connector ThermalV_n "Thermal vector heat port ('negative')"
    parameter Integer m(final min=1) = 1 "number of single heat-ports";
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b[m] ports
      "vector of single heat ports";

  annotation (defaultComponentName = "heat_n",
    Documentation(info="<html>
<p>Thermal connector with a vector of 'port's, negative.</p>
</html>
"), Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics={
          Text(
            extent={{-100,120},{120,60}},
            lineColor={176,0,0},
            textString="%name"),
          Polygon(
            points={{-100,0},{-40,-60},{20,0},{-40,60},{-100,0}},
            lineColor={176,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-90,50},{10,-50}},
            lineColor={176,0,0},
            textString="%m")}),
    Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
              100}}), graphics={Polygon(
            points={{-120,0},{0,-120},{120,0},{0,120},{-120,0}},
            lineColor={176,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-60,60},{60,-60}},
            lineColor={176,0,0},
            textString="%m")}));
  end ThermalV_n;

  partial model AddHeat "Additional heat port"

    Modelica.SIunits.Temperature T "Temperature of conductor";
    Modelica.SIunits.HeatFlowRate Q_flow "Dissipated heat of conductor";
    Thermal_p heat "heat port"
    annotation (                                       Documentation(info="<html>
  <pre>
  Adds a scalar heat port to the component.
  The port collects the total heat dissipated by the component.
  </pre>
  </html>"),
           Diagram(graphics),
      Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
      T = heat.T;
      Q_flow = -heat.Q_flow;

      annotation (Diagram(graphics),
                           Documentation(info="<html>
<p>Adds a heat-port to an electrical component.</p>
<p>Copper data at 20degC.</p>
<pre>
  rho_m = 8960 kg/m^3:     density
  c_p = 382.3 J/(kg.K):    specific heat
  rho = 1.673e-8 Ohm.m:    specific resistance
</pre>
</html>"));
  end AddHeat;

  partial model AddHeatV "Additional vector heat port"

    parameter Integer m_heat(final min=1) = 1 "number of heat conductors";
    Modelica.SIunits.Temperature[m_heat] T "Temperature of heat conductors";
    Modelica.SIunits.HeatFlowRate[m_heat] Q_flow
      "Dissipated heat of conductors";
    ThermalV_p heat(final m=m_heat) "vector heat port"
    annotation (                                       Documentation(info="<html>
  <pre>
  Adds a vector heat port to the component.
  Each port-component collects the heat dissipated by one conductor of the device.
  </pre>
  </html>"),
           Diagram(graphics),
      Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));

  equation
      T = heat.ports.T;
      Q_flow = -heat.ports.Q_flow;

      annotation (Diagram(graphics),
                           Documentation(info="<html>
<p>Adds a vector heat-port to an electrical component.</p>
<p>Copper data at 20degC.</p>
<pre>
  rho_m = 8960 kg/m^3:     density
  c_p = 382.3 J/(kg.K):    specific heat
  rho = 1.673e-8 Ohm.m:    specific resistance
</pre>
</html>"));
  end AddHeatV;

  connector Frequency "Weighted frequency"
    flow SI.Time H "inertia constant";
    flow SI.Angle w_H "angular velocity, inertia-weighted";
    Real h "Dummy potential-variable to balance flow-variable H";
    Real w_h "Dummy potential-variable to balance flow-variable w_H";

  annotation (defaultComponentName = "frequency",
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-80,80},{80,-80}},
            lineColor={120,0,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-60,30},{60,-30}},
            lineColor={120,0,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="f")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-120,120},{120,60}},
            lineColor={120,0,120},
            textString=
               "%name"), Ellipse(
            extent={{-40,40},{40,-40}},
            lineColor={120,0,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Documentation(info="<html>
<p>System frequency reference.<br>
Used in 'System' for sending/receiving weighted frequency-data.</p>
<pre>
  H:        weight, i.e. inertia constant of machine (dimension time)
  H_omega:  weighted angular frequency H*omega
</pre>
</html>"));
  end Frequency;

  model Sender "Sender of weighted frequency"
    input SI.Time H "inertia constant" annotation(Dialog);
    input SI.AngularVelocity w "angular velocity" annotation(Dialog);
    Frequency sendFreq annotation (Placement(transformation(extent={{
              -60,-92},{60,28}}, rotation=0), iconTransformation(extent={{-60,-92},
              {60,28}})));
  equation
    sendFreq.H = -H;
    sendFreq.w_H = -H*w;
    annotation (defaultComponentName = "sendFreq",
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
      Documentation(
            info="<html>
<p>Contains system frequency reference.<br>
Needed within certain models to establish the connection to 'system' for sending/receiving weighted frequency-data.</p>
<p>Used in generator models.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-100},{100,-140}},
            lineColor={120,0,120},
            textString=
                   "%name"), Polygon(
            points={{-100,-100},{0,100},{100,-100},{100,-100},{-100,-100}},
            lineColor={120,0,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Sender;

end Interfaces;
