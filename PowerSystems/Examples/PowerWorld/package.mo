within PowerSystems.Examples;
package PowerWorld "Demonstrate stabilization of wind power in Eurosyslib work package 5.3"


  extends Modelica.Icons.ExamplesPackage;


  model PowerWorld "Interoperation of wind power and thermal power"
    extends Modelica.Icons.Example;

    Components.WindFarm windFarm(redeclare package PhaseSystem =
          PowerSystems.PhaseSystems.DirectCurrent)
                                 annotation (Placement(transformation(extent={{-50,60},
                {-30,80}})));
    Components.City city annotation (                      Placement(
            transformation(extent={{60,-50},{80,-30}})));
    Components.LoadDispatcher dispatcher
      annotation (                           Placement(transformation(extent={{-90,-60},
                {-70,-40}})));
    Components.PowerPlant powerPlant(primaryControlMax=40)
                                     annotation (
          Placement(transformation(extent={{-62,-10},{-40,12}})));
    PowerSystems.Generic.VoltageConverter trafoPlant(ratio=10/380)
      annotation (Placement(transformation(extent={{-36,-6},{-24,6}})));
    PowerSystems.Generic.VoltageConverter distribution(ratio=380/50)
      annotation (Placement(transformation(extent={{44,-46},{56,-34}})));
    PowerSystems.Generic.Inverter HVDC(potentialReference=false)
      annotation (Placement(transformation(extent={{-16,34},{-4,46}})));
    Components.HydroPlant hydroPlant(primaryControlMax=200)
      annotation (Placement(transformation(extent={{80,20},{60,40}})));
    PowerSystems.Generic.VoltageConverter trafoHydro(ratio=380/10)
      annotation (Placement(transformation(extent={{44,24},{56,36}})));
    PowerSystems.Generic.Impedance linePlant(R=1, L=1/314)
      annotation (Placement(transformation(extent={{-16,-46},{-4,-34}})));
    PowerSystems.Generic.Impedance lineWind(R=1, L=1/314)
      annotation (Placement(transformation(
            extent={{-6,-6},{6,6}},
            rotation=-90,
            origin={10,10})));
    PowerSystems.Generic.Impedance lineHydro(R=1, L=1/314)
      annotation (Placement(transformation(
            extent={{-6,-6},{6,6}},
            rotation=-90,
            origin={40,-10})));
    Modelica.Blocks.Sources.RealExpression frequency(y=system.omega/2/pi)
    "Average frequency"   annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-80,-20})));
    inner System system(fType_par=false, ini="tr")
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
  equation
    connect(powerPlant.terminal, trafoPlant.terminal_p)
      annotation (Line(
          points={{-40,0},{-36,0}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(distribution.terminal_n, city.terminal) annotation (Line(
          points={{56,-40},{60,-40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(windFarm.terminal, HVDC.terminal_dc) annotation (Line(
          points={{-30,70},{-20,70},{-20,40},{-16,40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(dispatcher.plantDispatch, powerPlant.plantDispatch) annotation (Line(
          points={{-73,-46},{-70,-46},{-70,-6},{-62,-6}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(dispatcher.hydroDispatch, hydroPlant.hydroDispatch) annotation (Line(
          points={{-73,-52},{-70,-52},{-70,-80},{90,-80},{90,30},{80,30}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(trafoPlant.terminal_n, linePlant.terminal_p) annotation (Line(
          points={{-24,0},{-20,0},{-20,-40},{-16,-40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(linePlant.terminal_n, distribution.terminal_p) annotation (Line(
          points={{-4,-40},{44,-40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(HVDC.terminal, lineWind.terminal_p) annotation (Line(
          points={{-4,40},{10,40},{10,16}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(lineWind.terminal_n, distribution.terminal_p) annotation (Line(
          points={{10,4},{10,-38},{44,-38},{44,-40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(lineHydro.terminal_n, distribution.terminal_p) annotation (Line(
          points={{40,-16},{40,-36},{44,-36},{44,-40}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(frequency.y, dispatcher.frequency) annotation (Line(
          points={{-80,-31},{-80,-43}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(hydroPlant.terminal, trafoHydro.terminal_n) annotation (Line(
          points={{60,30},{56,30}},
          color={0,120,120},
          smooth=Smooth.None));
    connect(trafoHydro.terminal_p, lineHydro.terminal_p) annotation (Line(
          points={{44,30},{40,30},{40,-4}},
          color={0,120,120},
          smooth=Smooth.None));
    annotation (
        experiment(StopTime=86400),
        __Dymola_Commands(file(ensureSimulated=true)="Examples/PowerWorld/Resources/plot summary.mos"
        "plot summary",
                 file(ensureSimulated=true)="Examples/PowerWorld/Resources/plot powerPlant.mos"
        "plot powerPlant",
                 file(ensureSimulated=true)="Examples/PowerWorld/Resources/plot hydroPlant.mos"
        "plot hydroPlant"),
        preferredView="diagram",
    Documentation(info=
                    "<html>
<p>
This example models a control area for power distribution in island mode, i.e. without connection to a larger net.
It contains the following consumers and producers:
<ul>
<li>a city with a load of about 1000 MW, i.e. 1 Mio inhabitants,</li>
<li>a thermal power plant with
   <ul>
   <li>800 MW nominal power</li>
   <li>60 MW for secondary frequency control</li>
   <li>40 MW for primary frequency control</li>
   </ul></li>
<li>a wind park with a max power of 300 MW,</li>
<li>a hydro plant providing:
   <ul>
   <li>50 MW base load using a river turbine</li>
   <li>+-25 MW pumping power to support the day/night load cycle</li>
   <li>up to 200 MW peak power on demand.</li>
   </ul></li>
</ul>
</p>
<p>
The following switches/features are provided:
<ul>
<li><b>powerPlant.Modakond</b>: enhance the frequency/power control of the power plant to reduce throttle losses by utilizing condensate stop (see powerPlant.hotwellLevel.y, powerPlant.throttleReserve.y, powerPlant.pressureLoss.y, powerPlant.throttleCosts.y)</li>
<li><b>windForm.cut_off</b>: suddenly take off the wind farm as the wind speed exceeds the cut-off speed, e.g. in case of a storm</li>
</ul>
</p>
    </html>"));
  end PowerWorld;


  package Components "Aggregated components for PowerWorld example"
    extends Modelica.Icons.VariantsPackage;

    model PowerPlant "Thermal Power Plant with primary and secondary control"
      extends PowerSystems.Generic.Ports.PartialSource(
        final potentialReference=true);

      parameter Boolean Modakond = false "Install Modakond for condensate stop"
        annotation(Dialog(group="Features"));
      parameter Real primaryControlMax(unit="MW") = 40
      "Maximum power for primary frequency control"
        annotation(Dialog(group="Features"));

      Real P_generator(unit="MW") = -generator.S[1]/1e6;
      Real P_control(unit="MW") = -(P_generator - plantDispatch[1]);

    PowerSystems.Generic.Generator generator(redeclare package PhaseSystem =
          PhaseSystem, definiteReference=definiteReference) annotation (
        Placement(transformation(extent={{60,-20},{80,0}})));
      Modelica.Mechanics.Rotational.Components.Inertia rotor(
                       J=10e6,
      a(start=0),
        w(fixed=false, start=system.w_nom/generator.pp))
        annotation (                        Placement(transformation(extent={{30,-20},
                {50,0}})));
      Modelica.Mechanics.Rotational.Sources.Torque turbine
        annotation (                       Placement(transformation(extent={{0,-20},
                {20,0}})));
      Modelica.Mechanics.Rotational.Sensors.SpeedSensor angularVelocity
        annotation (                         Placement(transformation(extent={{46,4},{
                34,16}})));
      Modelica.Blocks.Sources.Constant reference(k=system.f_nom)
        annotation (                         Placement(transformation(extent={{46,24},
                {34,36}})));
      Modelica.Blocks.Continuous.LimPID primaryControl(
                                               k=0.05*800/0.2,
        controllerType=Modelica.Blocks.Types.SimpleController.P,
        yMax=primaryControlMax) "UCTE: df of 200 mHz corresponds to 5% of load"
        annotation (                         Placement(transformation(extent={{20,20},
                {0,40}})));
      Modelica.Blocks.Interfaces.RealInput[3] plantDispatch(each unit="MW")
        annotation (                            Placement(transformation(extent={{-130,
                -70},{-110,-50}})));
      Modelica.Blocks.Math.Add loadControl(k1=1e6/50/2/pi, k2=1e6/50/2/pi)
        annotation (                          Placement(transformation(extent={{-30,-20},
                {-10,0}})));
      Modelica.Blocks.Continuous.FirstOrder evaporator(T=60, y_start=490,
      initType=Modelica.Blocks.Types.Init.InitialState)
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-90,10})));
      Modelica.Blocks.Continuous.FirstOrder superheater1(T=60, y_start=490,
      initType=Modelica.Blocks.Types.Init.InitialState)
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-90,50})));
      Modelica.Blocks.Continuous.FirstOrder superheater2(T=60, y_start=490,
      initType=Modelica.Blocks.Types.Init.InitialState)
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-90,90})));
      Modelica.Blocks.Math.Add fuel annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-90,-30})));
      Modelica.Blocks.Continuous.TransferFunction transferFunction(b={180,1}, a=
           {60,1},
      initType=Modelica.Blocks.Types.Init.SteadyState)
      "achieve a power ramp of 2% per minute, minus 0.8% tolerance"
        annotation (Placement(transformation(extent={{-76,-86},{-64,-74}})));
      Modelica.Blocks.Math.Add add
        annotation (Placement(transformation(extent={{-96,-86},{-84,-74}})));
      Modelica.Blocks.Continuous.LimIntegrator hotwellLevel(
        k=1,
        outMax=10,
        outMin=0,
        y(unit="m")) "level of hotwell"
        annotation (Placement(transformation(extent={{50,-70},{70,-50}})));
      Modelica.Blocks.Math.Max on
        annotation (Placement(transformation(extent={{4,-46},{16,-34}})));
      Modelica.Blocks.Math.Sign condStop
        annotation (Placement(transformation(extent={{-30,-70},{-10,-50}})));
      Modelica.Blocks.Math.Min off
        annotation (Placement(transformation(extent={{4,-86},{16,-74}})));
      Modelica.Blocks.Math.Add accumulation(k2=1e-2, k1=if Modakond then 5e-4 else
                  0)
        annotation (Placement(transformation(extent={{24,-66},{36,-54}})));
      Modelica.Blocks.Sources.Constant zero(k=0)
        annotation (Placement(transformation(extent={{16,-66},{4,-54}})));
      Modelica.Blocks.Continuous.FirstOrder throttleDynamics(T=120, k=-300/800,
      initType=Modelica.Blocks.Types.Init.SteadyState)
        annotation (Placement(transformation(extent={{-12,68},{0,80}})));
      Modelica.Blocks.Math.Add pressure(k1=300/800)
        annotation (Placement(transformation(extent={{60,90},{80,110}})));
      Modelica.Blocks.Math.Min throttling
        annotation (Placement(transformation(extent={{-32,68},{-20,80}})));
      Modelica.Blocks.Sources.Constant throttleMin(k=if Modakond then 0 else
            Modelica.Constants.inf)
        annotation (Placement(transformation(extent={{-20,88},{-32,100}})));
      Modelica.Blocks.Sources.Constant throttleReserve(k=if Modakond then 0 else
                  5)
        annotation (                         Placement(transformation(extent={{-12,88},
                {0,100}})));
      Modelica.Blocks.Math.Add pressureLoss
        annotation (Placement(transformation(extent={{20,70},{40,90}})));
      Modelica.Blocks.Continuous.Integrator throttleCosts(k=1/365)
        annotation (Placement(transformation(extent={{60,60},{80,80}})));
      Modelica.Blocks.Math.Gain frequency(k=1/(2*pi))
        annotation (Placement(transformation(extent={{24,6},{16,14}})));
  protected
      outer System system;

  public
      Interfaces.Sender sender(w=generator.w, H=0.5*rotor.J*rotor.w^2/800e6)
      annotation (Placement(transformation(extent={{66,6},{74,14}})));
    initial equation
      if Connections.isRoot(generator.terminal.theta) then
        if system.steadyIni then
          rotor.a = 0;
        else
           rotor.w = system.omega/generator.pp;
        end if;
        rotor.phi = system.theta/generator.pp;
      end if;
    equation
      connect(sender.sendFreq, system.receiveFreq);
      connect(rotor.flange_b, angularVelocity.flange)
                                                  annotation (Line(
          points={{50,-10},{54,-10},{54,10},{46,10}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(primaryControl.y, loadControl.u1)
                                         annotation (Line(
          points={{-1,30},{-40,30},{-40,-4},{-32,-4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(evaporator.y, superheater1.u)
                                    annotation (Line(
          points={{-90,21},{-90,38}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(superheater1.y, superheater2.u)
                                    annotation (Line(
          points={{-90,61},{-90,78}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(superheater2.y, loadControl.u2)
                                   annotation (Line(
          points={{-90,101},{-90,110},{-60,110},{-60,-16},{-32,-16}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(loadControl.y, turbine.tau)
                                    annotation (Line(
          points={{-9,-10},{-2,-10}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(turbine.flange, rotor.flange_a)   annotation (Line(
          points={{20,-10},{30,-10}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(rotor.flange_b, generator.flange)   annotation (Line(
          points={{50,-10},{60,-10}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(fuel.y, evaporator.u)
                                 annotation (Line(
          points={{-90,-19},{-90,-2}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(transferFunction.y, fuel.u2) annotation (Line(
          points={{-63.4,-80},{-60,-80},{-60,-60},{-84,-60},{-84,-42}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(add.y, transferFunction.u) annotation (Line(
          points={{-83.4,-80},{-77.2,-80}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(loadControl.u1, add.u2) annotation (Line(
          points={{-32,-4},{-40,-4},{-40,-90},{-104,-90},{-104,-83.6},{-97.2,
              -83.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(accumulation.y, hotwellLevel.u)
                                     annotation (Line(
          points={{36.6,-60},{48,-60}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(loadControl.u1, condStop.u) annotation (Line(
          points={{-32,-4},{-40,-4},{-40,-60},{-32,-60}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(condStop.y, on.u1) annotation (Line(
          points={{-9,-60},{-4,-60},{-4,-36.4},{2.8,-36.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(condStop.y, off.u2) annotation (Line(
          points={{-9,-60},{-4,-60},{-4,-83.6},{2.8,-83.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(zero.y, on.u2) annotation (Line(
          points={{3.4,-60},{0,-60},{0,-43.6},{2.8,-43.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(zero.y, off.u1) annotation (Line(
          points={{3.4,-60},{0,-60},{0,-76.4},{2.8,-76.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(on.y, accumulation.u1) annotation (Line(
          points={{16.6,-40},{20,-40},{20,-56.4},{22.8,-56.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(off.y, accumulation.u2) annotation (Line(
          points={{16.6,-80},{20,-80},{20,-63.6},{22.8,-63.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(primaryControl.y, throttling.u2) annotation (Line(
          points={{-1,30},{-40,30},{-40,70.4},{-33.2,70.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(throttleMin.y, throttling.u1) annotation (Line(
          points={{-32.6,94},{-40,94},{-40,77.6},{-33.2,77.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(throttling.y, throttleDynamics.u) annotation (Line(
          points={{-19.4,74},{-13.2,74}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(superheater2.y, pressure.u1) annotation (Line(
          points={{-90,101},{-90,110},{50,110},{50,106},{58,106}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(throttleDynamics.y, pressureLoss.u2) annotation (Line(
          points={{0.6,74},{18,74}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(throttleReserve.y, pressureLoss.u1) annotation (Line(
          points={{0.6,94},{10,94},{10,86},{18,86}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pressureLoss.y, pressure.u2) annotation (Line(
          points={{41,80},{50,80},{50,94},{58,94}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pressureLoss.y, throttleCosts.u) annotation (Line(
          points={{41,80},{50,80},{50,70},{58,70}},
          color={0,0,127},
          smooth=Smooth.None));

      connect(plantDispatch[2], add.u1) annotation (Line(
          points={{-120,-60},{-106,-60},{-106,-76.4},{-97.2,-76.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(plantDispatch[1], fuel.u1) annotation (Line(
          points={{-120,-66.6667},{-96,-66.6667},{-96,-42}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(reference.y, primaryControl.u_s) annotation (Line(
          points={{33.4,30},{22,30}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(angularVelocity.w, frequency.u) annotation (Line(
          points={{33.4,10},{24.8,10}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(frequency.y, primaryControl.u_m) annotation (Line(
          points={{15.6,10},{10,10},{10,18}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(generator.terminal, terminal) annotation (Line(
        points={{80,-10},{90,-10},{90,0},{100,0}},
        color={0,120,120},
        smooth=Smooth.None));
      annotation (Icon(graphics={
            Polygon(
              points={{-30,100},{40,100},{60,-100},{-50,-100},{-30,100}},
              lineColor={0,0,0},
              pattern=LinePattern.None,
              fillPattern=FillPattern.VerticalCylinder,
              fillColor={189,189,126}),
            Rectangle(
              extent={{-44,-100},{80,-28}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={0,127,127},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name%"),
            Rectangle(
              extent={{-100,-100},{-30,50}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={0,127,127},
              fillPattern=FillPattern.Solid)},
          coordinateSystem(preserveAspectRatio=true, extent={{-120,-100},{100,
                120}})),       Diagram(coordinateSystem(preserveAspectRatio=false,
                      extent={{-120,-100},{100,120}}), graphics));
    end PowerPlant;

    model HydroPlant
      extends PowerSystems.Generic.Ports.PartialSource(
        final potentialReference=true);

      parameter Real primaryControlMax(unit="MW") = 200
      "Maximum power for primary frequency control"
        annotation(Dialog(group="Features"));

      Real P_generator(unit="MW") = -generator.S[1]/1e6;
      Real P_control(unit="MW") = -(P_generator - hydroDispatch[1]);

      Modelica.Blocks.Interfaces.RealInput[3] hydroDispatch(each unit="MW")
        annotation (                            Placement(transformation(extent={{-110,
                -10},{-90,10}})));
      Modelica.Blocks.Continuous.Integrator reservoirLevel(y_start=10, k=-1/5e4,
        y(unit="m"))
        annotation (Placement(transformation(extent={{-30,40},{-10,20}})));
      Modelica.Mechanics.Rotational.Sources.Torque reservoirTurbine
        annotation (                       Placement(transformation(extent={{-10,-10},
                {10,10}})));
      Modelica.Mechanics.Rotational.Components.Inertia rotor(J=1e6,
      a(start=0),
        w(fixed=false, start=system.w_nom/generator.pp))
        annotation (                        Placement(transformation(extent={{20,-10},
                {40,10}})));
    PowerSystems.Generic.Generator generator(redeclare package PhaseSystem =
          PhaseSystem, definiteReference=definiteReference) annotation (
        Placement(transformation(extent={{60,-10},{80,10}})));
      Modelica.Mechanics.Rotational.Sensors.SpeedSensor angularVelocity
        annotation (                         Placement(transformation(extent={{36,34},
                {24,46}})));
      Modelica.Blocks.Sources.Constant reference(k=system.f_nom)
        annotation (                         Placement(transformation(extent={{36,64},
                {24,76}})));
      Modelica.Blocks.Continuous.LimPID primaryControl(
        k=50/0.2,
        controllerType=Modelica.Blocks.Types.SimpleController.P,
        yMax=primaryControlMax) "200 mHz corresponds to 50 MW"
        annotation (                         Placement(transformation(extent={{10,60},
                {-10,80}})));
      Modelica.Blocks.Math.Gain powerControl(k=1e6/50/2/pi)
        annotation (                         Placement(transformation(extent={{-36,-6},
                {-24,6}})));
      Modelica.Blocks.Continuous.TransferFunction controlDynamics(a={1,1}, b={1},
        initType=Modelica.Blocks.Types.Init.InitialState)
        annotation (Placement(transformation(extent={{-56,-6},{-44,6}})));
      Modelica.Blocks.Math.Add add
        annotation (Placement(transformation(extent={{-76,-6},{-64,6}})));
      Modelica.Blocks.Math.Gain riverControl(k=1e6/50/2/pi)
        annotation (                         Placement(transformation(extent={{-36,-46},
                {-24,-34}})));
      Modelica.Mechanics.Rotational.Sources.Torque riverTurbine
        annotation (                       Placement(transformation(extent={{-10,-50},
                {10,-30}})));
      Modelica.Mechanics.Rotational.Components.Inertia rotorRiver(
                       J=0.5e6,
      a(start=0),
        w(fixed=false, start=system.w_nom/generator.pp))
        annotation (                        Placement(transformation(extent={{20,-50},
                {40,-30}})));
      Modelica.Blocks.Math.Gain frequency(k=1/(2*pi))
        annotation (Placement(transformation(extent={{14,36},{6,44}})));
  protected
      outer System system;

  public
      Interfaces.Sender sender(w=generator.w, H=0.5*rotor.J*rotor.w^2/200e6)
      annotation (Placement(transformation(extent={{66,16},{74,24}})));
    initial equation
      if Connections.isRoot(generator.terminal.theta) then
        if system.steadyIni then
          rotor.a = 0;
        else
          rotor.w = system.omega/generator.pp;
        end if;
        rotor.phi = system.theta/generator.pp;
      end if;
    equation
      connect(sender.sendFreq, system.receiveFreq);
      connect(rotor.flange_b, angularVelocity.flange)
                                                  annotation (Line(
          points={{40,0},{44,0},{44,40},{36,40}},
          color={0,0,0},
          smooth=Smooth.None));

      connect(reservoirTurbine.flange, rotor.flange_a)
                                                annotation (Line(
          points={{10,0},{20,0}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(rotor.flange_b, generator.flange)   annotation (Line(
          points={{40,0},{60,0}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(powerControl.y, reservoirTurbine.tau)
                                           annotation (Line(
          points={{-23.4,0},{-12,0}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(controlDynamics.y, reservoirLevel.u) annotation (Line(
          points={{-43.4,0},{-40,0},{-40,30},{-32,30}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(controlDynamics.y, powerControl.u) annotation (Line(
          points={{-43.4,0},{-37.2,0}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(add.y, controlDynamics.u) annotation (Line(
          points={{-63.4,0},{-57.2,0}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(hydroDispatch[2], add.u2) annotation (Line(
          points={{-100,4.44089e-016},{-84,4.44089e-016},{-84,-3.6},{-77.2,-3.6}},
          color={0,0,127},
          smooth=Smooth.None));

      connect(hydroDispatch[1], riverControl.u) annotation (Line(
          points={{-100,-6.66667},{-80,-6.66667},{-80,-40},{-37.2,-40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(riverControl.y, riverTurbine.tau) annotation (Line(
          points={{-23.4,-40},{-12,-40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(reference.y, primaryControl.u_s)
                                        annotation (Line(
          points={{23.4,70},{12,70}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(primaryControl.y, add.u1) annotation (Line(
          points={{-11,70},{-80,70},{-80,3.6},{-77.2,3.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(riverTurbine.flange, rotorRiver.flange_a) annotation (Line(
          points={{10,-40},{20,-40}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(rotorRiver.flange_b, generator.flange) annotation (Line(
          points={{40,-40},{50,-40},{50,0},{60,0}},
          color={0,0,0},
          smooth=Smooth.None));
      connect(angularVelocity.w, frequency.u) annotation (Line(
          points={{23.4,40},{14.8,40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(frequency.y, primaryControl.u_m) annotation (Line(
          points={{5.6,40},{0,40},{0,58}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(generator.terminal, terminal) annotation (Line(
        points={{80,0},{100,0}},
        color={0,120,120},
        smooth=Smooth.None));
      annotation (Icon(coordinateSystem(preserveAspectRatio=true,  extent={{-100,
                -100},{100,100}}), graphics={
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name%"),
            Polygon(
              points={{-18,-18},{16,-56},{36,-34},{76,-36},{18,24},{-104,20},{-18,
                  -18}},
              lineColor={0,0,255},
              smooth=Smooth.Bezier,
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{14,-44},{34,-86},{54,-66},{82,-68},{66,-28},{14,-44}},
              lineColor={0,0,255},
              smooth=Smooth.Bezier,
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{52,74},{54,50},{96,50},{98,74},{96,98},{54,98},{52,74}},
              lineColor={0,0,255},
              smooth=Smooth.Bezier,
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Line(
              points={{56,-30},{74,50}},
              color={0,128,255},
              smooth=Smooth.Bezier,
              thickness=1)}));
    end HydroPlant;

    model WindFarm
      extends PowerSystems.Generic.Ports.PartialSource(
        final potentialReference=false, final definiteReference=false);
      parameter Boolean cut_out = false
      "stop producing energy as wind exceeds cut-out speed of 20-25 m/s"
        annotation(Dialog(group="Features"));

      Modelica.Blocks.Sources.CombiTimeTable wind(
        tableName="tab",
        fileName=PowerWorldResources + "LoadData.txt",
        tableOnFile=true,
        table=fill(
                0.0,
                0,
                10)) annotation (Placement(transformation(extent={{-80,-10},{
                -60,10}})));
      PowerSystems.Generic.PrescribedPowerSource mills(
        redeclare package PhaseSystem = PhaseSystem,
        definiteReference = definiteReference)
          annotation (Placement(transformation(extent={{40,-10},{60,10}})));
      Modelica.Blocks.Sources.Trapezoid disturbance(
        rising=120,
        period=86400,
        offset=1,
        width=1800,
        falling=1800,
        startTime=20040,
        amplitude=if cut_out then -1 else 0)
        annotation (Placement(transformation(extent={{-60,20},{-40,40}})));
      Modelica.Blocks.Math.Product product
        annotation (Placement(transformation(extent={{-20,-10},{0,10}})));
    Modelica.Blocks.Math.Gain MW2W(k=1e6)
      annotation (Placement(transformation(extent={{14,-6},{26,6}})));
    equation

      connect(wind.y[4], product.u2) annotation (Line(
          points={{-59,0},{-40,0},{-40,-6},{-22,-6}},
          color={0,0,127},
          smooth=Smooth.None));

      connect(disturbance.y, product.u1) annotation (Line(
          points={{-39,30},{-30,30},{-30,6},{-22,6}},
          color={0,0,127},
          smooth=Smooth.None));
    connect(mills.terminal, terminal) annotation (Line(
        points={{60,0},{100,0}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(product.y, MW2W.u) annotation (Line(
        points={{1,0},{12.8,0}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(MW2W.y, mills.P) annotation (Line(
        points={{26.6,0},{39,0}},
        color={0,0,127},
        smooth=Smooth.None));
      annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                -100},{100,100}}), graphics={
            Rectangle(
              extent={{-48,24},{-32,-100}},
              lineColor={85,85,255},
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-44,24},{-98,22},{-46,38},{-28,88},{-32,36},{0,-6},{-44,
                  24}},
              lineColor={85,85,255},
              smooth=Smooth.None,
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{32,24},{48,-100}},
              lineColor={85,85,255},
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{36,24},{-12,20},{32,38},{54,86},{48,36},{82,-2},{36,24}},
              lineColor={85,85,255},
              smooth=Smooth.None,
              fillColor={85,85,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name%")}));
    end WindFarm;

    model LoadDispatcher

      Modelica.Blocks.Sources.CombiTimeTable data(
        tableName="tab",
        fileName=PowerWorldResources + "LoadData.txt",
        tableOnFile=true,
        table=fill(
            0.0,
            0,
            10)) annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
      Modelica.Blocks.Interfaces.RealOutput[3] plantDispatch(each unit="MW")
        annotation (Placement(transformation(extent={{60,30},{80,50}})));
      Modelica.Blocks.Interfaces.RealOutput loadForcast(unit="MW")
        annotation (Placement(transformation(extent={{-80,30},{-60,50}})));
      Modelica.Blocks.Interfaces.RealOutput windForcast(unit="MW")
        annotation (Placement(transformation(extent={{-80,-30},{-60,-10}})));
      Modelica.Blocks.Sources.Constant hydroBase(k=25)
        annotation (Placement(transformation(extent={{-80,-90},{-60,-70}})));
      Modelica.Blocks.Sources.Constant primaryControl(k=40)
        annotation (Placement(transformation(extent={{20,70},{40,90}})));
      Modelica.Blocks.Interfaces.RealOutput[3] hydroDispatch(each unit="MW")
        annotation (Placement(transformation(extent={{60,-30},{80,-10}})));
      Modelica.Blocks.Sources.Constant controlHydro(k=200)
        annotation (Placement(transformation(extent={{80,0},{60,20}})));
      Modelica.Blocks.Interfaces.RealInput frequency(unit="Hz")
        annotation (Placement(transformation(extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={0,70})));
      Modelica.Blocks.Continuous.LimPID secondaryControl(   k=60/0.4,
        controllerType=Modelica.Blocks.Types.SimpleController.PI,
        Ti=600,
        yMax=60) "400 mHz corresponds to 60 MW"
        annotation (Placement(transformation(extent={{20,50},{40,30}})));
      Modelica.Blocks.Sources.Constant reference(k=system.f_nom)
        annotation (                         Placement(transformation(extent={{-6,34},
                {6,46}})));
      Modelica.Blocks.Math.Add3 plantSchedule(k2=-1, k3=-1,
        y(unit="MW"))
        annotation (Placement(transformation(extent={{10,-10},{30,10}})));
      Modelica.Blocks.Sources.Trapezoid hydroDaily(
        rising=86400/4,
        width=86400/4,
        falling=86400/4,
        period=86400,
        startTime=86400/8,
        amplitude=50,
        offset=-25,
        y(unit="MW"))
        annotation (Placement(transformation(extent={{-80,-60},{-60,-40}})));
      Modelica.Blocks.Math.Add add
        annotation (Placement(transformation(extent={{-16,-36},{-4,-24}})));
      Modelica.Blocks.Math.Gain distributionLoss(k=1)      annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            origin={-16,16})));
  protected
      outer System system;
    equation
      connect(primaryControl.y, plantDispatch[3]) annotation (Line(
          points={{41,80},{50,80},{50,46.6667},{70,46.6667}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(hydroBase.y, hydroDispatch[1])  annotation (Line(
          points={{-59,-80},{56,-80},{56,-26.6667},{70,-26.6667}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(controlHydro.y, hydroDispatch[3]) annotation (Line(
          points={{59,10},{56,10},{56,-13.3333},{70,-13.3333}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(data.y[3], windForcast) annotation (Line(
          points={{-59,10},{-40,10},{-40,-20},{-70,-20}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(secondaryControl.y, plantDispatch[2]) annotation (Line(
          points={{41,40},{70,40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(windForcast, plantSchedule.u2) annotation (Line(
          points={{-70,-20},{-20,-20},{-20,0},{8,0}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(plantSchedule.y, plantDispatch[1]) annotation (Line(
          points={{31,0},{50,0},{50,33.3333},{70,33.3333}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(hydroDaily.y, hydroDispatch[2]) annotation (Line(
          points={{-59,-50},{48,-50},{48,-20},{70,-20}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(hydroDaily.y, add.u1) annotation (Line(
          points={{-59,-50},{-32,-50},{-32,-26.4},{-17.2,-26.4}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(hydroBase.y, add.u2) annotation (Line(
          points={{-59,-80},{-26,-80},{-26,-33.6},{-17.2,-33.6}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(add.y, plantSchedule.u3) annotation (Line(
          points={{-3.4,-30},{0,-30},{0,-8},{8,-8}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(frequency, secondaryControl.u_m) annotation (Line(
          points={{0,70},{0,56},{30,56},{30,52}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(reference.y, secondaryControl.u_s) annotation (Line(
          points={{6.6,40},{18,40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(data.y[1], loadForcast) annotation (Line(
          points={{-59,10},{-40,10},{-40,40},{-70,40}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(loadForcast, distributionLoss.u) annotation (Line(
          points={{-70,40},{-40,40},{-40,16},{-28,16}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(distributionLoss.y, plantSchedule.u1) annotation (Line(
          points={{-5,16},{0,16},{0,8},{8,8}},
          color={0,0,127},
          smooth=Smooth.None));
      annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                -100},{100,100}}), graphics={
            Polygon(
              points={{-80,-80},{-60,-40},{60,-40},{80,-80},{-80,-80}},
              lineColor={96,96,96},
              smooth=Smooth.None,
              fillColor={96,96,96},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-60,60},{60,-30}},
              lineColor={96,96,96},
              fillColor={96,96,96},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-48,50},{48,42}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-48,30},{18,22}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-48,10},{40,2}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-48,-12},{36,-20}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name%")}));
    end LoadDispatcher;

    model City
      extends PowerSystems.Generic.Ports.PartialLoad;
      Modelica.Blocks.Sources.CombiTimeTable data(
        tableName="tab",
        fileName=PowerWorldResources + "LoadData.txt",
        tableOnFile=true,
        table=fill(
                0.0,
                0,
                10)) annotation (Placement(transformation(extent={{-60,40},{-40,
                60}})));
      PowerSystems.Generic.PrescribedPowerLoad load(phi=0.34906585039887)
                                  annotation (Placement(transformation(extent={
                {-60,-10},{-40,10}})));
    Modelica.Blocks.Math.Gain MW2W(k=1e6)
      annotation (Placement(transformation(extent={{-14,-6},{-26,6}})));
    equation
    connect(terminal, load.terminal) annotation (Line(
        points={{-100,0},{-60,0}},
        color={0,120,120},
        smooth=Smooth.None));
    connect(data.y[2], MW2W.u) annotation (Line(
        points={{-39,50},{0,50},{0,0},{-12.8,0}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(MW2W.y, load.P) annotation (Line(
        points={{-26.6,0},{-39,0}},
        color={0,0,127},
        smooth=Smooth.None));
      annotation (                           Icon(coordinateSystem(preserveAspectRatio=false,
              extent={{-100,-100},{100,100}}), graphics={
            Polygon(
              points={{-100,-100},{-100,100},{-66,100},{-66,0},{-40,0},{-40,40},
                  {0,40},{0,0},{40,0},{40,60},{80,60},{80,-20},{100,-20},{100,-100},
                  {-100,-100}},
              lineColor={0,0,0},
              smooth=Smooth.None,
              fillColor={0,0,0},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-92,70},{-74,52}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-92,44},{-74,26}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-28,30},{-10,12}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{52,50},{70,32}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{52,18},{70,0}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-92,-12},{-74,-30}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-62,-28},{-44,-46}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-26,-28},{-8,-46}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{12,-28},{30,-46}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{52,-28},{70,-46}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={255,255,85},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name%")}));
    end City;

  end Components;


  package Test "Component tests for PowerWorld example"

    model PowerPlantTest1 "Test primary control"
    import PowerSystems;
      Components.PowerPlant powerPlant
        annotation (Placement(transformation(extent={{-20,0},{2,22}})));
      Modelica.Blocks.Sources.Constant schedule(k=450)
        annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
      PowerSystems.Generic.PrescribedPowerLoad prescribedLoad(phi=
                                                              0.34906585039887)               annotation (Placement(transformation(extent={{20,0},{40,20}})));
      Modelica.Blocks.Sources.Step step(startTime=10,
                                        height=-40,
                                        offset=490)
        annotation (Placement(transformation(extent={{40,40},{60,60}})));
      Modelica.Blocks.Sources.Constant secondary(k=40)
        annotation (                       Placement(transformation(extent={{-80,0},{-60,20}})));
      Modelica.Blocks.Sources.Constant primary(k=40)
        annotation (                       Placement(transformation(extent={{-80,40},{-60,60}})));
    Modelica.Blocks.Math.Gain MW2W(k=1e6)
      annotation (Placement(transformation(extent={{66,4},{54,16}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(powerPlant.terminal, prescribedLoad.terminal) annotation (Line(points={{2,10},{20,10}},
                                                                             color={0,0,0},
                                                                             pattern=LinePattern.None,
                                                                             smooth=Smooth.None));
      connect(secondary.y, powerPlant.plantDispatch[2])
        annotation (Line(points={{-59,10},{-40,10},{-40,4},{-20,4}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(schedule.y, powerPlant.plantDispatch[1])
        annotation (Line(points={{-59,-30},{-30,-30},{-30,3.33333},{-20,3.33333}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(primary.y, powerPlant.plantDispatch[3]) annotation (Line(points={{-59,50},
            {-30,50},{-30,4.66667},{-20,4.66667}},                     color={0,0,127},
                                                                       smooth=Smooth.None));
    connect(step.y, MW2W.u) annotation (Line(
        points={{61,50},{80,50},{80,10},{67.2,10}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(MW2W.y, prescribedLoad.P) annotation (Line(
        points={{53.4,10},{41,10}},
        color={0,0,127},
        smooth=Smooth.None));
      annotation (experiment(StopTime=30));
    end PowerPlantTest1;

    model PowerPlantTest2 "Test secondary control"
    import PowerSystems;
      Components.PowerPlant powerPlant
        annotation (Placement(transformation(extent={{-20,0},{2,22}})));
      Modelica.Blocks.Sources.Constant schedule(k=490)
        annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
      PowerSystems.Generic.PrescribedPowerLoad prescribedLoad(phi=
                                                              0.34906585039887)               annotation (Placement(transformation(extent={{20,0},{40,20}})));
      Modelica.Blocks.Sources.Ramp ramp(offset=490,
                                        height=48,
                                        duration=180,
                                        startTime=100)
        annotation (Placement(transformation(extent={{40,40},{60,60}})));
      Modelica.Blocks.Sources.Step secondary(height=48, startTime=100)
        annotation (                       Placement(transformation(extent={{-80,0},{-60,20}})));
      Modelica.Blocks.Sources.Constant primary(k=40)
        annotation (                       Placement(transformation(extent={{-80,40},{-60,60}})));
    Modelica.Blocks.Math.Gain MW2W(k=1e6)
      annotation (Placement(transformation(extent={{66,4},{54,16}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(powerPlant.terminal, prescribedLoad.terminal) annotation (Line(points={{2,10},{20,10}},
                                                                             color={0,0,0},
                                                                             pattern=LinePattern.None,
                                                                             smooth=Smooth.None));
      connect(schedule.y, powerPlant.plantDispatch[1])
        annotation (Line(points={{-59,-30},{-30,-30},{-30,3.33333},{-20,3.33333}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(secondary.y, powerPlant.plantDispatch[2])
        annotation (Line(points={{-59,10},{-40,10},{-40,4},{-20,4}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(primary.y, powerPlant.plantDispatch[3]) annotation (Line(points={{-59,50},
            {-30,50},{-30,4.66667},{-20,4.66667}},                     color={0,0,127},
                                                                       smooth=Smooth.None));
    connect(ramp.y, MW2W.u) annotation (Line(
        points={{61,50},{80,50},{80,10},{67.2,10}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(MW2W.y, prescribedLoad.P) annotation (Line(
        points={{53.4,10},{41,10}},
        color={0,0,127},
        smooth=Smooth.None));
      annotation (experiment(StopTime=600));
    end PowerPlantTest2;

    model PowerPlantTest3 "Test connection to a large net"
    import PowerSystems;
      Components.PowerPlant powerPlant
        annotation (Placement(transformation(extent={{-20,0},{2,22}})));
      Modelica.Blocks.Sources.Constant schedule(k=490)
        annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
      PowerSystems.Generic.FixedVoltageSource largeGrid annotation (Placement(
          transformation(extent={{80,0},{60,20}})));
      Modelica.Blocks.Sources.Ramp secondary(startTime=100,
                                             height=300,
                                             duration=0)
        annotation (                       Placement(transformation(extent={{-80,0},{-60,20}})));
      PowerSystems.Generic.Impedance
        line(R=1, L=1/314)
        annotation (Placement(transformation(extent={{20,0},{40,20}})));
      Modelica.Blocks.Sources.Constant primary(k=40)
        annotation (                       Placement(transformation(extent={{-80,40},{-60,60}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(schedule.y, powerPlant.plantDispatch[1])
        annotation (Line(points={{-59,-30},{-30,-30},{-30,3.33333},{-20,3.33333}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(secondary.y, powerPlant.plantDispatch[2])
        annotation (Line(points={{-59,10},{-40,10},{-40,4},{-20,4}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(powerPlant.terminal, line.terminal_p)      annotation (Line(points={{2,10},{20,10}},
                                                                          color={0,0,0},
                                                                          smooth=Smooth.None));
    connect(line.terminal_n, largeGrid.terminal) annotation (Line(
        points={{40,10},{60,10}},
        color={0,0,0},
        smooth=Smooth.None));
      connect(primary.y, powerPlant.plantDispatch[3]) annotation (Line(points={{-59,50},
            {-30,50},{-30,4.66667},{-20,4.66667}},                     color={0,0,127},
                                                                       smooth=Smooth.None));
      annotation (experiment(StopTime=600));
    end PowerPlantTest3;

    model HydroPlantTest1 "Test primary control"
    import PowerSystems;
      Components.HydroPlant hydroPlant(primaryControlMax=310)
        annotation (Placement(transformation(extent={{-20,0},{0,20}})));
      Modelica.Blocks.Sources.Constant schedule(k=50)
        annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
      PowerSystems.Generic.PrescribedPowerLoad prescribedLoad
        annotation (Placement(transformation(extent={{20,0},{40,20}})));
      Modelica.Blocks.Sources.Trapezoid trapezoid(startTime=100,
                                                  offset=50,
                                                  amplitude=300,
                                                  width=300,
                                                  falling=300,
                                                  nperiod=1,
                                                  rising=60,
                                                  period=900)
        annotation (Placement(transformation(extent={{40,40},{60,
                                                              60}})));
      Modelica.Blocks.Sources.Constant primary(k=400)
        annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
      Modelica.Blocks.Sources.Constant secondary(k=0)
        annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
    Modelica.Blocks.Math.Gain MW2W(k=1e6)
      annotation (Placement(transformation(extent={{66,4},{54,16}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(schedule.y, hydroPlant.hydroDispatch[1])
        annotation (Line(points={{-59,-30},{-40,-30},{-40,9.33333},{-20,9.33333}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(hydroPlant.terminal, prescribedLoad.terminal) annotation (Line(points={{0,10},{20,10}},
                                                                             color={0,0,0},
                                                                             smooth=Smooth.None));
      connect(primary.y, hydroPlant.hydroDispatch[3])
        annotation (Line(points={{-59,50},{-40,50},{-40,10.6667},{-20,10.6667}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(secondary.y, hydroPlant.hydroDispatch[2]) annotation (Line(points={{-59,10},{-20,10}},
                                                                         color={0,0,127},
                                                                         smooth=Smooth.None));
    connect(trapezoid.y, MW2W.u) annotation (Line(
        points={{61,50},{80,50},{80,10},{67.2,10}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(MW2W.y, prescribedLoad.P) annotation (Line(
        points={{53.4,10},{41,10}},
        color={0,0,127},
        smooth=Smooth.None));
      annotation (experiment(StopTime=900));
    end HydroPlantTest1;

    model HydroPlantTest2 "Test secondary control"
    import PowerSystems;
      Components.HydroPlant hydroPlant
        annotation (Placement(transformation(extent={{-20,0},{0,20}})));
      Modelica.Blocks.Sources.Constant schedule(k=50)
        annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
      Modelica.Blocks.Sources.Constant primary(k=400)
        annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
      Modelica.Blocks.Sources.Trapezoid secondary(amplitude=100,
                                                  offset=-50,
                                                  rising=86400/4,
                                                  width=86400/4,
                                                  falling=86400/4,
                                                  period=86400,
                                                  startTime=86400/8)
        annotation (Placement(transformation(extent={{-80,0},{-60,20}})));
      PowerSystems.Generic.FixedVoltageSource largeGrid annotation (Placement(
          transformation(extent={{80,0},{60,20}})));
      PowerSystems.Generic.Impedance
        line(R=1, L=1/314)
        annotation (Placement(transformation(extent={{20,0},{40,20}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(schedule.y, hydroPlant.hydroDispatch[1])
        annotation (Line(points={{-59,-30},{-40,-30},{-40,9.33333},{-20,9.33333}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(primary.y, hydroPlant.hydroDispatch[3])
        annotation (Line(points={{-59,50},{-40,50},{-40,10.6667},{-20,10.6667}},
                         color={0,0,127},
                         smooth=Smooth.None));
      connect(secondary.y, hydroPlant.hydroDispatch[2]) annotation (Line(points={{-59,10},{-20,10}},
                                                                         color={0,0,127},
                                                                         smooth=Smooth.None));
    connect(line.terminal_n, largeGrid.terminal) annotation (Line(
        points={{40,10},{60,10}},
        color={0,0,0},
        smooth=Smooth.None));
      connect(hydroPlant.terminal, line.terminal_p) annotation (Line(points={{0,10},{20,10}},
                                                                     color={0,0,0},
                                                                     smooth=Smooth.None));
      annotation (experiment(StopTime=86400));
    end HydroPlantTest2;

    model WindFarmLoadTest "WindFarm connected to a load"
    import PowerSystems;
      Components.WindFarm windFarm
        annotation (Placement(transformation(extent=
                                             {{-60,0},{-40,20}})));
      PowerSystems.Generic.Impedance
        load(R=30, L=10/314)
        annotation (Placement(transformation(extent={{-20,0},{0,20}})));
      PowerSystems.Generic.Ground
        ground
        annotation (Placement(transformation(extent={{20,0},{40,20}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(load.terminal_n,ground. terminal)
        annotation (Line(points={{0,10},{20,10}}, color={0,0,0}));
      connect(windFarm.terminal, load.terminal_p) annotation (Line(points={{-40,10},{-20,10}},
                                                                   color={0,0,0},
                                                                   smooth=Smooth.None));
      annotation (experiment(StopTime=86400));
    end WindFarmLoadTest;

    model WindFarmGridTest "WindFarm connected to a large net"
    import PowerSystems;
      Components.WindFarm windFarm
        annotation (Placement(transformation(extent=
                                             {{-60,0},{-40,20}})));
      PowerSystems.Generic.FixedVoltageSource largeGrid
      annotation (Placement(transformation(extent={{40,0},{20,20}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
    connect(windFarm.terminal, largeGrid.terminal) annotation (Line(
        points={{-40,10},{20,10}},
        color={0,0,0},
        smooth=Smooth.None));
      annotation (experiment(StopTime=86400));
    end WindFarmGridTest;

    model WindFarmHVDCTest "WindFarm connected to a large net via HVDC"
    import PowerSystems;
      Components.WindFarm windFarm(redeclare package PhaseSystem =
            PowerSystems.PhaseSystems.DirectCurrent)
        annotation (Placement(transformation(extent=
                                             {{-60,0},{-40,20}})));

      PowerSystems.Generic.FixedVoltageSource largeGrid
      annotation (Placement(transformation(extent={{40,0},{20,20}})));
      PowerSystems.Generic.Inverter
        inverter
        annotation (Placement(transformation(extent={{-20,0},{0,20}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
      connect(windFarm.terminal, inverter.terminal_dc) annotation (Line(points={{-40,10},{-20,10}},
                                                                        color={0,0,0},
                                                                        smooth=Smooth.None));
    connect(inverter.terminal, largeGrid.terminal) annotation (Line(
        points={{0,10},{20,10}},
        color={0,0,0},
        smooth=Smooth.None));
      annotation (experiment(StopTime=86400));
    end WindFarmHVDCTest;

    model CityTest
    import PowerSystems;
      PowerSystems.Generic.FixedVoltageSource largeGrid annotation (Placement(
          transformation(extent={{-60,0},{-40,20}})));
      Components.City city
        annotation (Placement(transformation(extent={{20,0},{40,20}})));
    inner PowerSystems.System system
      annotation (Placement(transformation(extent={{-100,80},{-80,100}})));
    equation
    connect(largeGrid.terminal, city.terminal) annotation (Line(
        points={{-40,10},{20,10}},
        color={0,0,0},
        smooth=Smooth.None));
      annotation (experiment(StopTime=86400));
    end CityTest;

    model LoadDispatcherTest
    import PowerSystems;
      Components.LoadDispatcher loadDispatcher
        annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
      Modelica.Blocks.Sources.Trapezoid frequency(amplitude=2,
                                                  rising=15,
                                                  falling=15,
                                                  period=60,
                                                  offset=49,
                                                  width=15)
        annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
    equation
      connect(frequency.y, loadDispatcher.frequency) annotation (Line(points={{-59,30},{-30,30},{-30,17}},
                                                                      color={0,0,127},
                                                                      smooth=Smooth.None));
    end LoadDispatcherTest;
  end Test;

  constant String PowerWorldResources = Modelica.Utilities.Files.loadResource("modelica://PowerSystems.Examples.PowerWorld/Resources/");


annotation (preferredView="info",Documentation(info="<html>
<p>The example demonstrates power/frequency control exploiting a thermal power plant and a pump store for the stabilization of fluctuating wind power.
The demonstrator developed for Eurosyslib WP5.3 complements the model with a 3D animation using DLR&apos;s SimVis software.</p>
<p><img src=\"modelica://PowerSystems.Examples.PowerWorld/Resources/PowerWorld.png\"/></p>
</html>"));
end PowerWorld;
