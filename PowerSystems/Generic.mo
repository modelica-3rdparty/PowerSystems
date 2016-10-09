within PowerSystems;
package Generic "Simple components for basic investigations"
  extends Modelica.Icons.VariantsPackage;

  model Impedance
    extends PowerSystems.Generic.Ports.PartialTwoTerminal;
    parameter SI.Resistance R = 1 "active component";
    parameter SI.Inductance L = 1/314 "reactive component";
    SI.AngularFrequency omegaRef;
  equation
    if PhaseSystem.m > 0 then
      omegaRef = der(PhaseSystem.thetaRef(terminal_p.theta));
    else
      omegaRef = 0;
    end if;
    v = R*i + omegaRef*L*j(i);
    zeros(PhaseSystem.n) = terminal_p.i + terminal_n.i;
    if PhaseSystem.m > 0 then
      terminal_p.theta = terminal_n.theta;
    end if;
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={
          Rectangle(
            extent={{-70,30},{70,-30}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-100,0},{-70,0}}, color={0,120,120}),
          Line(points={{70,0},{100,0}}, color={0,120,120}),
          Text(
            extent={{-150,-60},{150,-100}},
            lineColor={0,0,0},
            textString="R=%R, L=%L"),
          Text(
            extent={{-150,60},{150,100}},
            lineColor={0,0,0},
            textString="%name"),
          Rectangle(
            extent={{0,10},{66,-10}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}));
  end Impedance;

  model Admittance
    extends PowerSystems.Generic.Ports.PartialTwoTerminal;
    parameter SI.Conductance G = 1 "active component";
    parameter SI.Capacitance C = 1/314 "reactive component";
    SI.AngularFrequency omegaRef;
  equation
    if PhaseSystem.m > 0 then
      omegaRef = der(PhaseSystem.thetaRef(terminal_p.theta));
    else
      omegaRef = 0;
    end if;
    i = G*v + omegaRef*C*j(v);
    zeros(PhaseSystem.n) = terminal_p.i + terminal_n.i;
    if PhaseSystem.m > 0 then
      terminal_p.theta = terminal_n.theta;
    end if;
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={
          Rectangle(
            extent={{-70,30},{70,-30}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-100,0},{-70,0}}, color={0,120,120}),
          Line(points={{70,0},{100,0}}, color={0,120,120}),
          Text(
            extent={{-150,-60},{150,-100}},
            lineColor={0,0,0},
            textString="G=%G, C=%C"),
          Text(
            extent={{-150,60},{150,100}},
            lineColor={0,0,0},
            textString="%name"),
          Rectangle(
            extent={{14,30},{24,-30}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{36,30},{46,-30}},
            lineColor={0,120,120},
            fillColor={0,120,120},
            fillPattern=FillPattern.Solid)}));
  end Admittance;

  model VoltageConverter
    extends PowerSystems.Generic.Ports.PartialTwoTerminal;
    parameter Real ratio = 1 "conversion ratio terminal_p.v/terminal_n.v";
  equation
    terminal_p.v = ratio*terminal_n.v;
    zeros(PhaseSystem.n) = ratio*terminal_p.i + terminal_n.i;
    if PhaseSystem.m > 0 then
      terminal_p.theta = terminal_n.theta;
    end if;
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={
          Line(points={{-100,0},{-70,0}}, color={0,0,0}),
          Line(points={{70,0},{100,0}}, color={0,0,0}),
          Text(
            extent={{-150,-60},{150,-100}},
            lineColor={0,0,0},
            textString="%ratio"),
          Text(
            extent={{-150,60},{150,100}},
            lineColor={0,0,0},
            textString="%name"),
          Ellipse(
            extent={{-80,50},{20,-50}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-20,50},{80,-50}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Ellipse(extent={{-80,50},{20,-50}}, lineColor={0,120,120})}));
  end VoltageConverter;

  model Ground
    extends PowerSystems.Generic.Ports.PartialLoad;
  equation
    terminal.v = zeros(PhaseSystem.n);
    annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics={
          Line(points={{0,0},{0,-60}}, color={0,120,120}),
          Line(points={{-80,-60},{80,-60}}, color={0,120,120}),
          Line(points={{-50,-80},{50,-80}}, color={0,120,120}),
          Line(points={{-20,-100},{20,-100}}, color={0,120,120}),
          Line(points={{-100,0},{0,0}}, color={0,120,120}),
          Text(
            extent={{-150,60},{150,100}},
            lineColor={0,0,0},
            textString="%name")}));
  end Ground;

  model Generator "Basic transformation of rotational to electrical power"
    extends PowerSystems.Generic.Ports.PartialSource(
                                     final potentialReference = synchronous);
    parameter Boolean synchronous = PhaseSystem.m > 0 "synchronous machine";
    parameter Integer pp = 1 "pole-pair number";
    parameter SI.Frequency f_nom = system.f_nom "nominal value of frequency"
      annotation (Dialog(group="Reference Parameters"));
    parameter PS.Voltage V_nom = 10e3 "nominal value of voltage"
      annotation (Dialog(group="Reference Parameters"));
    Modelica.Mechanics.Rotational.Interfaces.Flange_a flange
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    SI.AngularVelocity w = pp*der(flange.phi);
    PS.Voltage V(start = V_nom);
    SI.Angle thetaRel;
  protected
    outer System system;

  equation
    if PhaseSystem.m > 0 then
      if synchronous then
        pp*flange.phi = PhaseSystem.thetaRef(terminal.theta);
        if Connections.isRoot(terminal.theta) then
          V = V_nom;
          if PhaseSystem.m > 1 then
            PhaseSystem.thetaRel(terminal.theta) = 0;
          end if;
        end if;
      end if;
      thetaRel = PhaseSystem.thetaRel(terminal.theta);
    else
      thetaRel = 0;
    end if;
    if PhaseSystem.m == 0 or not synchronous then
      V = V_nom/f_nom*w/2/pi;
    end if;
    0 = PhaseSystem.activePower(terminal.v, terminal.i) + w*flange.tau;
    terminal.v = PhaseSystem.phaseVoltages(V, thetaRel);
    annotation (Icon(coordinateSystem(preserveAspectRatio=true,  extent={{-100,
              -100},{100,100}}), graphics={
          Line(points={{-100,0},{-50,0}}, color={0,120,120}),
          Line(points={{50,0},{100,0}}, color={0,120,120}),
          Text(
            extent={{-150,-60},{150,-100}},
            lineColor={0,0,0},
            textString="V=%V_ref V"),
          Text(
            extent={{-150,60},{150,100}},
            lineColor={0,0,0},
            textString="%name"),
          Ellipse(
            extent={{-50,50},{50,-50}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-40,30},{40,-30}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="A",
            visible=not synchronous),
          Rectangle(
            extent={{-28,30},{30,-30}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            visible=synchronous),
          Text(
            extent={{-40,30},{40,-30}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="S",
            visible=synchronous)}));
  end Generator;

  model Inverter "Convert direct current to alternating current"
    extends PowerSystems.Generic.Ports.PartialSource;
    package PhaseSystem_dc = PowerSystems.PhaseSystems.DirectCurrent;
    PowerSystems.Generic.Ports.Terminal_p terminal_dc(
      redeclare package PhaseSystem = PhaseSystem_dc)
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    parameter PhaseSystem_dc.Voltage V_dc = 150e3 "voltage of dc system";
    PS.Current I "value of current";
    SI.Angle thetaRel;
  protected
    outer System system;
  equation
    if PhaseSystem.m > 0 then
      if Connections.isRoot(terminal.theta) then
        PhaseSystem.thetaRef(terminal.theta) = system.theta;
        if PhaseSystem.m > 1 then
          PhaseSystem.thetaRel(terminal.theta) = 0;
        end if;
      end if;
      thetaRel = PhaseSystem.thetaRel(terminal.theta);
    else
      thetaRel = 0;
    end if;
    if true then
      Connections.branch(terminal.theta, terminal_dc.theta)
        "needed because terminal_dc is a generic Terminal with theta";
    end if;
    terminal_dc.v = PhaseSystem_dc.phaseVoltages(V_dc);
    terminal.i = PhaseSystem.phaseCurrents(I, thetaRel);
    0 = PhaseSystem_dc.activePower(terminal_dc.v, terminal_dc.i)
      + PhaseSystem.activePower(terminal.v, terminal.i);
    annotation (Icon(coordinateSystem(preserveAspectRatio=true,
            extent={{-100,-100},{100,100}}), graphics={
          Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{0,-68},{80,12}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="~"),
          Line(
            points={{-90,-90},{90,90}},
            color={0,120,120},
            smooth=Smooth.None),
          Text(
            extent={{-68,-10},{12,70}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="="),
          Text(
            extent={{0,-84},{80,-4}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="~"),
          Text(
            extent={{0,-100},{80,-20}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="~"),
          Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end Inverter;

  model FixedVoltageSource
    extends PowerSystems.Generic.Ports.PartialSource;
    parameter PS.Voltage V = 10e3 "value of constant voltage";
    SI.Angle thetaRel;
  protected
    outer System system;
  equation
    if PhaseSystem.m > 0 then
      if Connections.isRoot(terminal.theta) then
        PhaseSystem.thetaRef(terminal.theta) = system.theta;
        if PhaseSystem.m > 1 then
          PhaseSystem.thetaRel(terminal.theta) = 0;
        end if;
      end if;
      thetaRel = PhaseSystem.thetaRel(terminal.theta);
    else
      thetaRel = 0;
    end if;
    terminal.v = PhaseSystem.phaseVoltages(V, thetaRel);
    annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics={Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.CrossDiag),
          Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end FixedVoltageSource;

  model FixedLoad
    extends PowerSystems.Generic.Ports.PartialLoad;
    parameter SI.Power P = 0 "rms value of constant active power";
    parameter SI.Angle phi = 0 "phase angle";
  equation
    PhaseSystem.phasePowers_vi(terminal.v, terminal.i) = PhaseSystem.phasePowers(P, phi);
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-38,-68},{38,-26}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="VA"), Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end FixedLoad;

  model FixedCurrent
    extends PowerSystems.Generic.Ports.PartialLoad;
    parameter SI.Current I = 0 "rms value of constant current";
    parameter SI.Angle phi = 0 "phase angle"
    annotation (Dialog(group="Reference Parameters", enable = definiteReference));
  equation
    terminal.i = PhaseSystem.phaseCurrents(I, phi);
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={
          Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-78,22},{80,64}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="%I A"),
          Text(
            extent={{-38,-74},{38,-32}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString="%phi"),
          Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end FixedCurrent;

  model PrescribedPowerSource "Prescribed power source"
    extends PowerSystems.Generic.Ports.PartialSource(
      final potentialReference=true);
    Modelica.Blocks.Interfaces.RealInput P(unit="W") annotation (Placement(
          transformation(extent={{-130,-20},{-90,20}})));
    PS.Current I "value of current";
    SI.Angle thetaRel;
  protected
    outer System system;
  equation
    if PhaseSystem.m > 0 then
      if Connections.isRoot(terminal.theta) then
        PhaseSystem.thetaRef(terminal.theta) = system.theta;
        if PhaseSystem.m > 1 then
          PhaseSystem.thetaRel(terminal.theta) = 0;
        end if;
      end if;
      thetaRel = PhaseSystem.thetaRel(terminal.theta);
    else
      thetaRel = 0;
    end if;
    terminal.i = PhaseSystem.phaseCurrents(I, thetaRel);
    0 = PhaseSystem.activePower(terminal.v, terminal.i) + P;
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end PrescribedPowerSource;

  model PrescribedPowerLoad "Prescribed power load"
    extends PowerSystems.Generic.Ports.PartialLoad;
    parameter SI.Angle phi = 0 "phase angle";
    Modelica.Blocks.Interfaces.RealInput P(unit="W") annotation (Placement(
          transformation(extent={{130,-20},{90,20}})));
  equation
    PhaseSystem.phasePowers_vi(terminal.v, terminal.i) = PhaseSystem.phasePowers(P, phi);
    annotation (Icon(coordinateSystem(preserveAspectRatio=false,
            extent={{-100,-100},{100,100}}), graphics={Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,120,120},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-150,100},{150,140}},
            lineColor={0,0,0},
            textString="%name")}));
  end PrescribedPowerLoad;

  package Ports "Interfaces for generic components"
    extends Modelica.Icons.InterfacesPackage;
    connector Terminal_p "Positive terminal"
      extends PowerSystems.Interfaces.Terminal;
      annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics={Polygon(
                  points={{60,60},{-60,60},{-60,-60},{60,-60},{60,60}},
                  lineColor={0,120,120},
                  fillColor={0,120,120},
                  fillPattern=FillPattern.Solid), Text(
                  extent={{-150,60},{150,100}},
                  lineColor={0,0,0},
                  textString="%name")}),     Icon(coordinateSystem(
                  preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
                graphics={Polygon(
                  points={{100,100},{-100,100},{-100,-100},{100,-100},{100,100}},
                  lineColor={0,120,120},
                  fillColor={0,120,120},
                  fillPattern=FillPattern.Solid)}));
    end Terminal_p;

    connector Terminal_n "Negative terminal"
      extends PowerSystems.Interfaces.Terminal;
      annotation (Diagram(coordinateSystem(preserveAspectRatio=true,  extent={{-100,
                -100},{100,100}}),     graphics={
                Text(
                  extent={{-150,60},{150,100}},
                  lineColor={0,0,0},
                  textString="%name"), Rectangle(
              extent={{-60,60},{60,-60}},
              lineColor={0,120,120},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}),             Icon(coordinateSystem(
                  preserveAspectRatio=true,  extent={{-100,-100},{100,100}}),
                graphics={Rectangle(
              extent={{-100,100},{100,-100}},
              lineColor={0,120,120},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}));
    end Terminal_n;

    partial model PartialTwoTerminal
      replaceable package PhaseSystem = PackagePhaseSystem constrainedby
        PowerSystems.PhaseSystems.PartialPhaseSystem "Phase system"
        annotation (choicesAllMatching=true);
      package PS = PhaseSystem;
      function j = PhaseSystem.j annotation(Inline=true);
      parameter PS.Voltage[PhaseSystem.n] v_start = zeros(PhaseSystem.n)
        "Start value for voltage drop" annotation(Dialog(tab="Initialization"));
      parameter PS.Current[PhaseSystem.n] i_start = zeros(PhaseSystem.n)
        "Start value for current" annotation(Dialog(tab="Initialization"));
      PowerSystems.Generic.Ports.Terminal_p terminal_p(
        redeclare package PhaseSystem = PhaseSystem)
        annotation (Placement(
          transformation(extent={{-110,-10},{-90,10}})));
      PowerSystems.Generic.Ports.Terminal_n terminal_n(
        redeclare package PhaseSystem = PhaseSystem)
        annotation (Placement(
          transformation(extent={{90,-10},{110,10}})));
      PS.Voltage[PhaseSystem.n] v(start = v_start);
      PS.Current[PhaseSystem.n] i(start = i_start);
      SI.Power p[PhaseSystem.n] = PhaseSystem.phasePowers_vi(v, i);
      PS.Voltage V = PhaseSystem.systemVoltage(v);
      PS.Current I = PhaseSystem.systemCurrent(i);
      SI.Angle phi = PhaseSystem.phase(v) - PhaseSystem.phase(i);
    equation
      v = terminal_p.v - terminal_n.v;
      i = terminal_p.i;
      if true or PhaseSystem.m > 0 then
        Connections.branch(terminal_p.theta, terminal_n.theta);
      end if;
    end PartialTwoTerminal;

    partial model PartialSource
      replaceable package PhaseSystem = PackagePhaseSystem constrainedby
        PowerSystems.PhaseSystems.PartialPhaseSystem "Phase system"
        annotation (choicesAllMatching=true);
      package PS = PhaseSystem;
      function j = PhaseSystem.j annotation(Inline=true);
      PowerSystems.Generic.Ports.Terminal_n
                                         terminal(
                                      redeclare package PhaseSystem =
            PhaseSystem)
        annotation (Placement(transformation(extent={{90,-10},{110,10}})));
      SI.Power p[PhaseSystem.n] = PhaseSystem.phasePowers_vi(terminal.v, terminal.i);
      SI.Angle phi = PhaseSystem.phase(terminal.v) - PhaseSystem.phase(-terminal.i);
      parameter Boolean potentialReference = true "serve as potential root"
         annotation (Evaluate=true, Dialog(group="Reference Parameters"));
      parameter Boolean definiteReference = false "serve as definite root"
         annotation (Evaluate=true, Dialog(group="Reference Parameters"));
    equation
      if true or PhaseSystem.m > 0 then
        if potentialReference then
          if definiteReference then
            Connections.root(terminal.theta);
          else
            Connections.potentialRoot(terminal.theta);
          end if;
        end if;
      end if;
    end PartialSource;

    partial model PartialLoad
      replaceable package PhaseSystem = PackagePhaseSystem constrainedby
        PowerSystems.PhaseSystems.PartialPhaseSystem "Phase system"
        annotation (choicesAllMatching=true);
      package PS = PhaseSystem;
      function j = PhaseSystem.j annotation(Inline=true);
      parameter PS.Voltage[PhaseSystem.n] v_start = ones(PhaseSystem.n)
        "Start value for voltage drop" annotation(Dialog(tab="Initialization"));
      PowerSystems.Generic.Ports.Terminal_p terminal(
        redeclare package PhaseSystem = PhaseSystem,
        v(start = v_start))
        annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
      SI.Power p[PhaseSystem.n] = PhaseSystem.phasePowers_vi(terminal.v, terminal.i);
    end PartialLoad;
  end Ports;

  package Sensors
    extends Modelica.Icons.SensorsPackage;
    model PMeter "measure power flow"
      extends Ports.PartialTwoTerminal;
      Modelica.Blocks.Interfaces.RealOutput P(quantity="Power",
                                              unit="W")
        "Active power flow from terminal_p to terminal_n"
                                               annotation (Placement(
            transformation(
            origin={0,110},
            extent={{10,-10},{-10,10}},
            rotation=270)));
    equation
      P = PhaseSystem.activePower(terminal_p.v, i);
      v = zeros(PhaseSystem.n);
      zeros(PhaseSystem.n) = terminal_p.i + terminal_n.i;
      if PhaseSystem.m > 0 then
        terminal_p.theta = terminal_n.theta;
      end if;
      annotation(Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-70,70},{70,-70}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{0,20},{0,90}}, color={135,135,135}),
            Line(
              points={{-90,0},{-20,0}},
              color={0,100,100},
              thickness=0.5),
            Line(
              points={{0,0},{90,0}},
              color={0,100,100},
              thickness=0.5),
            Line(
              points={{30,20},{70,0},{30,-20}},
              color={0,100,100},
              thickness=0.5),
            Ellipse(extent={{-20,20},{20,-20}}, lineColor={135,135,135}),
            Text(
              extent={{-150,-130},{150,-90}},
              lineColor={0,0,0},
              textString="%name"),
            Ellipse(
              extent={{-20,20},{20,-20}},
              lineColor={135,135,135},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Line(
              points={{0,0},{20,0}},
              color={0,100,100},
              thickness=0.5),
            Ellipse(extent={{-70,70},{70,-70}}, lineColor={135,135,135}),
            Line(points={{0,100},{0,70}}, color={0,0,127}),
            Text(
              extent={{162,120},{2,90}},
              lineColor={0,0,0},
              textString="P")}));
    end PMeter;
  end Sensors;
end Generic;
