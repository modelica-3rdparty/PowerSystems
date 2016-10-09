within PowerSystems.Mechanics;
package TurboGroups "Turbines including generator-rotor"
  extends Modelica.Icons.VariantsPackage;

  model FixedSpeedTG "Fixed speed turbo-generator rotor"

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));
    parameter SI.AngularVelocity w_start=0 "initial rotor angular velocity"
      annotation(Dialog(tab="Initialization"));
    parameter SI.AngularVelocity w_nom=1 "nominal angular velocity"
      annotation(Dialog(group="Nominal"));
    Interfaces.Rotation_n airgap "to airgap electric machine"
                                           annotation (Placement(transformation(
            extent={{90,50},{110,70}})));
    Modelica.Blocks.Interfaces.RealInput power(
                     final unit="1") "turbine power pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.RealOutput speed(
                               final unit="1") "turbine speed pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    outer System system;
    SI.Angle phi(stateSelect=StateSelect.prefer);
    SI.AngularVelocity w(start=w_start, stateSelect=StateSelect.prefer);

  initial equation
    if dynType <> Types.Dynamics.SteadyInitial then
      w = w_start;
    end if;

  equation
    phi = airgap.phi;
    w = der(phi);
    0 = der(w);
    speed = w/w_nom;
    annotation (defaultComponentName = "rotor1mass",
      Documentation(
              info="<html>
<p>Turbine-rotor and generator-rotor together represent one single rotating object without dynamical properties. The constant rotor velocity is determined at initialisation.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{-100,34},{0,70},{0,-70},{-100,-34},{-100,34}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,90},{100,70}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Rectangle(
            extent={{-100,-70},{100,-90}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,50},{100,-50}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,70},{100,50}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,-50},{100,-70}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid)}));
  end FixedSpeedTG;

  model SingleMassTG "Single mass turbo-generator rotor"

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Initialization"));
    parameter SIpu.AngularVelocity speed_thr(unit="1")=0.5
      "threshold: torque ctrl \\ power ctrl";
    parameter SI.Time H=1 "inertia constant turbine + generator";
    parameter SI.AngularVelocity w_start=0 "initial rotor angular velocity"
      annotation(Dialog(tab="Initialization"));
    parameter SI.AngularVelocity w_nom=1 "nominal angular velocity"
      annotation(Dialog(group="Nominal"));
    parameter SI.Power P_nom=1 "nominal power turbine"
      annotation(Dialog(group="Nominal"));
    Interfaces.Rotation_n airgap "to airgap electric machine"
                                           annotation (Placement(transformation(
            extent={{90,50},{110,70}})));
    Modelica.Blocks.Interfaces.RealInput power(
                     final unit="1") "turbine power pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.RealOutput speed(
                               final unit="1") "turbine speed pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    outer System system;
    final parameter SI.Inertia J=2*H*P_nom/(w_nom*w_nom)
      "inertia turbine + generator";
    final parameter SI.Torque tau_nom=P_nom/w_nom;
    SI.Angle phi(stateSelect=StateSelect.prefer);
    SI.AngularVelocity w(start=w_start, stateSelect=StateSelect.prefer);
    SI.AngularAcceleration a(start=0);
    Real tau_pu(unit="1");

  initial equation
    if dynType == Types.Dynamics.SteadyInitial then
      der(w) = 0;
    else
      w = w_start;
    end if;

  equation
    max(speed_thr, speed)*tau_pu = power;
    phi = airgap.phi;
    w = der(phi);
    a = der(w);
    J*a = tau_pu*tau_nom + airgap.tau;
    speed = w/w_nom;
    annotation (defaultComponentName = "rotor1mass",
      Documentation(
              info="<html>
<p>This model can be regarded as a default model.<br><br>
Turbine-rotor and generator-rotor together represent one single stiff rotating mass, characterised by its mechanical time constant H. The model therefore has one single mechanical degree of freedom. This component does not need an additional torque model.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{-100,34},{0,70},{0,-70},{-100,-34},{-100,34}},
            lineColor={0,0,0},
            fillColor={215,215,215},
            fillPattern=FillPattern.Backward),
          Rectangle(
            extent={{0,50},{100,-50}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Rectangle(
            extent={{-100,90},{100,70}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Rectangle(
            extent={{-100,-70},{100,-90}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,70},{100,50}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{0,-50},{100,-70}},
            lineColor={255,170,85},
            fillColor={255,170,85},
            fillPattern=FillPattern.Solid)}));
  end SingleMassTG;

  model SteamTurboGroup "Steam turbo-group with generator-rotor"
    extends Partials.TurboBase1(final n=size(par.P_nom,1));

    replaceable record Data =
      PowerSystems.Mechanics.TurboGroups.Parameters.SteamTurboGroup
      "turbo-group par"   annotation(choicesAllMatching=true);
    final parameter Data par "turbo-group par"
      annotation (Placement(transformation(extent={{-80,80},{-60,100}})));
    Rotational.ElectricRotor genRotor(
      J=par.J_gen,
      w(start=w_start),
      a(start=0))
      annotation (Placement(transformation(extent={{50,-10},{70,10}})));
    SI.Angle[n] delta "difference angles";
  protected
    Rotational.Rotor aux1(J=par.J_aux[1])
      annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
    Rotational.ShaftNoMass shaft1(stiff=par.stiff[1])
      annotation (Placement(transformation(extent={{-80,-10},{-70,10}})));
    Rotational.ThermalTurbineRotor turbine1(J=par.J_turb[1])
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    Rotational.ShaftNoMass shaft2(stiff=par.stiff[2])
      annotation (Placement(transformation(extent={{-50,-10},{-40,10}})));
    Rotational.ThermalTurbineRotor turbine2(J=par.J_turb[2])
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    Rotational.ShaftNoMass shaft3(stiff=par.stiff[3])
      annotation (Placement(transformation(extent={{-20,-10},{-10,10}})));
    Rotational.ThermalTurbineRotor turbine3(J=par.J_turb[3])
      annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
    Rotational.ShaftNoMass shaft4(stiff=par.stiff[4])
      annotation (Placement(transformation(extent={{10,-10},{20,10}})));
    Rotational.ThermalTurbineRotor turbine4(J=par.J_turb[4])
      annotation (Placement(transformation(extent={{20,-10},{40,10}})));
    Rotational.ShaftNoMass shaft5(stiff=par.stiff[5])
      annotation (Placement(transformation(extent={{40,-10},{50,10}})));
    Rotational.ShaftNoMass shaft6(stiff=par.stiff[6])
      annotation (Placement(transformation(extent={{70,-10},{80,10}})));
    Rotational.Rotor aux2(J=par.J_aux[2])
      annotation (Placement(transformation(extent={{80,-10},{100,10}})));

  initial equation
    aux1.w = turbine1.w;
    aux1.a = turbine1.a;
    turbine1.w = turbine2.w;
    turbine1.a = turbine2.a;
    turbine2.w = turbine3.w;
    turbine2.a = turbine3.a;
    turbine3.w = turbine4.w;
    turbine3.a = turbine4.a;
    turbine4.w = genRotor.w;
    turbine4.a = genRotor.a;
    genRotor.w = aux2.w;
    genRotor.a = aux2.a;

  equation
    delta = {turbine2.flange_a.phi-turbine1.flange_b.phi,turbine3.flange_a.phi -turbine2.flange_b.phi,
      turbine4.flange_a.phi-turbine3.flange_b.phi,genRotor.flange_a.phi -turbine4.flange_b.phi};

    connect(aux1.flange_b,shaft1.flange_a)
      annotation (Line(points={{-80,0},{-80,0}}, color={0,0,0}));
    connect(shaft1.flange_b,turbine1.flange_a)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,0}));
    connect(turbine1.flange_b,shaft2.flange_a)
      annotation (Line(points={{-50,0},{-50,0}}, color={0,0,0}));
    connect(shaft2.flange_b,turbine2.flange_a)
      annotation (Line(points={{-40,0},{-40,0}}, color={0,0,0}));
    connect(turbine2.flange_b,shaft3.flange_a)
      annotation (Line(points={{-20,0},{-20,0}}, color={0,0,0}));
    connect(shaft3.flange_b,turbine3.flange_a)
      annotation (Line(points={{-10,0},{-10,0}}, color={0,0,0}));
    connect(turbine3.flange_b,shaft4.flange_a)
      annotation (Line(points={{10,0},{10,0}}, color={0,0,0}));
    connect(shaft4.flange_b,turbine4.flange_a)
      annotation (Line(points={{20,0},{20,0}}, color={0,0,0}));
    connect(turbine4.flange_b,shaft5.flange_a)
      annotation (Line(points={{40,0},{40,0}}, color={0,0,0}));
    connect(shaft5.flange_b,genRotor.flange_a)
      annotation (Line(points={{50,0},{50,0}}, color={0,0,0}));
    connect(genRotor.flange_b,shaft6.flange_a)
      annotation (Line(points={{70,0},{70,0}}, color={0,0,0}));
    connect(shaft6.flange_b,aux2.flange_a)
      annotation (Line(points={{80,0},{80,0}}, color={0,0,0}));
    connect(blades[1], turbine1.rotor)
                                      annotation (Line(points={{-100,60},{-60,
            60},{-60,6}}, color={0,0,0}));
    connect(blades[2], turbine2.rotor)
                                      annotation (Line(points={{-100,60},{-30,
            60},{-30,6}}, color={0,0,0}));
    connect(blades[3], turbine3.rotor)
                                      annotation (Line(points={{-100,60},{0,60},
            {0,6}}, color={0,0,0}));
    connect(blades[4], turbine4.rotor)
                                      annotation (Line(points={{-100,60},{30,60},
            {30,6}}, color={0,0,0}));
    connect(airgap, genRotor.rotor) annotation (Line(points={{100,60},{60,60},{
            60,6}}, color={0,0,0}));
    annotation (defaultComponentName = "turboGrp",
      Documentation(
              info="<html>
<p>Example model of a large steam turbo-group with generator rotor.<br>
(Aux, HP1, HP2, MP, LP, Generator, Exciter).<br><br>
The rigid massive rotating parts are connected with massless elastic shafts. The model therefore has several mechanical degrees of freedom and allows the study of coupled electrical and mechanical resonances.<br>
An appropriate torque model has to be connected to SteamTurboGroup.blades.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(
        coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}),
        graphics={
          Polygon(
            points={{-100,40},{-60,60},{-60,-60},{-100,-40},{-100,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Polygon(
            points={{-60,40},{-10,70},{-10,-70},{-60,-40},{-60,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Polygon(
            points={{-60,70},{-60,40},{-10,70},{-60,70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-60,-70},{-60,-40},{-10,-70},{-60,-70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
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
                "stator reaction torque- and friction-models may be added here")}));
  end SteamTurboGroup;

  model GasTurbineGear "Gas turbine with gear and generator-rotor"
    extends Partials.TurboBase1(final n=size(par.P_nom,1));

    replaceable record Data =
      PowerSystems.Mechanics.TurboGroups.Parameters.GasTurbineGear
      "turbo-group par"   annotation(choicesAllMatching=true);
    final parameter Data par "turbo-group par"
      annotation (Placement(transformation(extent={{-80,80},{-60,100}})));
    Rotational.ElectricRotor genRotor(
      J=par.J_gen,
      w(start=w_start*par.ratio[end]/par.ratio[1]),
      a(start=0))
      annotation (Placement(transformation(extent={{70,-10},{90,10}})));
  protected
    Rotational.ThermalTurbineRotor turbine(J=par.J_turb)
      annotation (Placement(transformation(extent={{-90,-10},{-70,10}})));
    Rotational.ShaftNoMass shaft1(stiff=par.stiff_sh[1])
      annotation (Placement(transformation(extent={{-70,-10},{-60,10}})));
    Rotational.ThermalTurbineRotor compressor(J=par.J_comp)
      annotation (Placement(transformation(extent={{-40,-10},{-60,10}})));
    Rotational.ShaftNoMass shaft2(stiff=par.stiff_sh[2])
      annotation (Placement(transformation(extent={{-40,-10},{-30,10}})));
    Rotational.Gear gear1(ratio=par.ratio[1:2], J=par.J_gear1)
      annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));
    Rotational.ShaftNoMass shaft3(stiff=par.stiff_sh[3])
      annotation (Placement(transformation(extent={{-10,-10},{0,10}})));
    Rotational.Gear gear2(ratio=par.ratio[2:3], J=par.J_gear2)
      annotation (Placement(transformation(extent={{0,-10},{20,10}})));
    Rotational.ShaftNoMass shaft4(stiff=par.stiff_sh[4])
      annotation (Placement(transformation(extent={{20,-10},{30,10}})));
    Rotational.Rotor accessory(J=par.J_acc)
      annotation (Placement(transformation(extent={{30,-10},{40,10}})));
    Rotational.ShaftNoMass shaft5(stiff=par.stiff_sh[5])
      annotation (Placement(transformation(extent={{40,-10},{50,10}})));
    Rotational.Shaft coupling(J=par.J_cpl, stiff=par.stiff_cpl)
      annotation (Placement(transformation(extent={{50,-40},{60,40}})));
    Rotational.ShaftNoMass shaft6(stiff=par.stiff_sh[6])
      annotation (Placement(transformation(extent={{60,-10},{70,10}})));

  initial equation
    turbine.w = compressor.w;
    turbine.a = compressor.a;
    compressor.w = (par.ratio[1]/par.ratio[2])*gear1.w;
    compressor.a = (par.ratio[1]/par.ratio[2])*gear1.a;
    gear1.w = (par.ratio[2]/par.ratio[3])*gear2.w;
    gear1.a = (par.ratio[2]/par.ratio[3])*gear2.a;
    gear2.w = accessory.w;
    gear2.a = accessory.a;
    accessory.w = coupling.w;
    accessory.a = coupling.a;
    coupling.w = genRotor.w;
    coupling.a = genRotor.a;

  equation
    connect(turbine.flange_b,shaft1.flange_a)
      annotation (Line(points={{-70,0},{-70,0}}, color={0,0,0}));
    connect(compressor.flange_b,shaft1.flange_b)
      annotation (Line(points={{-60,0},{-60,0}}, color={0,0,0}));
    connect(compressor.flange_a,shaft2.flange_a)
      annotation (Line(points={{-40,0},{-40,0}}, color={0,0,0}));
    connect(shaft2.flange_b,gear1.flange_a)
      annotation (Line(points={{-30,0},{-30,0}}, color={0,0,0}));
    connect(gear1.flange_b,shaft3.flange_a)
      annotation (Line(points={{-10,0},{-10,0}}, color={0,0,0}));
    connect(shaft3.flange_b,gear2.flange_a)
      annotation (Line(points={{0,0},{0,0}}, color={0,0,0}));
    connect(gear2.flange_b,shaft4.flange_a)
      annotation (Line(points={{20,0},{20,0}}, color={0,0,0}));
    connect(shaft4.flange_b,accessory.flange_a)
      annotation (Line(points={{30,0},{30,0}}, color={0,0,0}));
    connect(accessory.flange_b,shaft5.flange_a)
      annotation (Line(points={{40,0},{40,0}}, color={0,0,0}));
    connect(shaft5.flange_b,coupling.flange_a)
      annotation (Line(points={{50,0},{50,0}}, color={0,0,0}));
    connect(coupling.flange_b,shaft6.flange_a)
      annotation (Line(points={{60,0},{60,0}}, color={0,0,0}));
    connect(shaft6.flange_b,genRotor.flange_a)
      annotation (Line(points={{70,0},{70,0}}, color={0,0,0}));
    connect(blades[1], turbine.rotor) annotation (Line(points={{-100,60},{-80,
            60},{-80,6}}, color={0,0,0}));
    connect(blades[2], compressor.rotor) annotation (Line(points={{-100,60},{
            -50,60},{-50,6}}, color={0,0,0}));
    connect(airgap, genRotor.rotor) annotation (Line(points={{100,60},{80,60},{
            80,6}}, color={0,0,0}));
    annotation (defaultComponentName = "GTgrp",
      Documentation(
              info="<html>
<p>Example model of a small gas-turbine with gear and generator rotor.
(Turbine, compressor, gear, accessory, generator).<br>
An appropriate torque model has to be connected to GasTurbineGear.blades.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Polygon(
            points={{-100,40},{-60,60},{-60,-60},{-100,-40},{-100,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Polygon(
            points={{-60,40},{-10,70},{-10,-70},{-60,-40},{-60,40}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215}),
          Polygon(
            points={{-60,70},{-60,40},{-10,70},{-60,70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Polygon(
            points={{-60,-70},{-60,-40},{-10,-70},{-60,-70}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-88,10},{-70,10}},
            color={0,0,0},
            thickness=0.5),
          Line(
            points={{-50,10},{-20,10}},
            color={0,0,0},
            thickness=0.5),
          Line(
            points={{-88,-10},{-20,-10}},
            color={0,0,0},
            thickness=0.5)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-60},{100,-80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid,
            textString=
                "stator reaction torque- and friction-models may be added here")}));
  end GasTurbineGear;

  model HydroTurbine "Hydro turbine with generator-rotor"
    extends Partials.TurboBase2(final n=1);

    replaceable record Data =
      PowerSystems.Mechanics.TurboGroups.Parameters.HydroTurbine
      "hydro-turbine par"   annotation(choicesAllMatching=true);
    final parameter Data par "hydro-turbine par"
      annotation (Placement(transformation(extent={{-80,80},{-60,100}})));
    Rotational.ElectricRotor genRotor(
      J=par.J_gen,
      w(start=w_start),
      a(start=0))
      annotation (Placement(transformation(extent={{5,-10},{25,10}})));
  protected
    Rotational.HydroTurbineRotor turbine(J=par.J_turb)
      annotation (Placement(transformation(extent={{-25,-10},{-5,10}})));
    Rotational.Shaft shaft(J=par.J_shaft, stiff=par.stiff)
      annotation (Placement(transformation(extent={{-5,-10},{5,10}})));

  initial equation
    turbine.w = shaft.w;
    der(turbine.w) = der(shaft.w);
    shaft.w = genRotor.w;
    der(shaft.w) = der(genRotor.w);

  equation
    connect(turbine.flange_b,shaft.flange_a)
      annotation (Line(points={{-5,0},{-5,0}}, color={0,0,0}));
    connect(shaft.flange_b,genRotor.flange_a)
      annotation (Line(points={{5,0},{5,0}}, color={0,0,0}));
    connect(blades[1], turbine.rotor) annotation (Line(points={{-100,60},{-15,
            60},{-15,6}}, color={0,0,0}));
    connect(airgap, genRotor.rotor) annotation (Line(points={{100,60},{15,60},{
            15,6}}, color={0,0,0}));
    annotation (defaultComponentName = "hydroGrp",
      Documentation(
              info="<html>
<p>Hydro turbine and generator rotor, coupled with a massive shaft.</p>
<p>Note that name and icon of the model are specific, although the turbine model itself is rather generic. The essential type-specific properties are related with torque generation.<br>
An appropriate torque model has to be connected to HydroTurbine.blades.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(
        coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}),
        graphics={
          Rectangle(
            extent={{-83,50},{-43,-50}},
            lineColor={95,95,95},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-83,70},{-43,30}},
            lineColor={0,0,0},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-83,-30},{-43,-70}},
            lineColor={0,0,0},
            fillColor={135,135,135},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-43,10},{-10,-10}},
            lineColor={0,0,0},
            fillPattern=FillPattern.HorizontalCylinder,
            fillColor={215,215,215})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-60},{100,-80}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid,
            textString=
                "stator reaction torque- and friction-models may be added here")}));
  end HydroTurbine;

  model Diesel "Diesel with generator-rotor"
    extends Partials.TurboBase3(final n=1);                                                                                            // annotation 0;

    replaceable record Data =
      PowerSystems.Mechanics.TurboGroups.Parameters.Diesel "Diesel par"
      annotation(choicesAllMatching=true);
    final parameter Data par "Diesel par"
      annotation (Placement(transformation(extent={{-80,80},{-60,100}})));
    Rotational.ElectricRotor genRotor(
      J=par.J_gen,
      w(start=w_start),
      a(start=0))
      annotation (Placement(transformation(extent={{5,-10},{25,10}})));
  protected
    Rotational.DieselRotor diesel(J=par.J_turb)
      annotation (Placement(transformation(extent={{-25,-10},{-5,10}})));
    Rotational.ShaftNoMass shaft(stiff=par.stiff)
      annotation (Placement(transformation(extent={{-5,-10},{5,10}})));

  initial equation
    diesel.w = genRotor.w;
    der(diesel.w) = der(genRotor.w);

  equation
    connect(diesel.flange_b,shaft.flange_a)
      annotation (Line(points={{-5,0},{-5,0}}, color={0,0,0}));
    connect(shaft.flange_b,genRotor.flange_a)
      annotation (Line(points={{5,0},{5,0}}, color={0,0,0}));
    connect(blades[1], diesel.rotor) annotation (Line(points={{-100,60},{-15,60},
            {-15,6}}, color={0,0,0}));
    connect(airgap, genRotor.rotor) annotation (Line(points={{100,60},{15,60},{
            15,6}}, color={0,0,0}));
    annotation (defaultComponentName = "dieselGrp",
      Documentation(
              info="<html>
<p>Diesel with generator rotor, coupled with a massless shaft.</p>
<p>Note that name and icon of the model are specific, although the 'turbine' model itself is rather generic. The essential type-specific properties are related with torque generation.<br>
An appropriate torque model has to be connected to Diesel.blades (terminology from turbines!).<br><br>
<b>Perhaps somebody can provide a more realistic Diesel-model!</b></p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(
        coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}),
        graphics={
          Rectangle(
            extent={{-90,50},{-20,-70}},
            lineColor={95,95,95},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-80,70},{-30,50}},
            lineColor={95,95,95},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-85,-6},{-25,-66}},
            lineColor={95,95,95},
            fillColor={175,175,175},
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
                "stator reaction torque- and friction-models may be added here")}));
  end Diesel;

  model WindTurbineGear "Wind turbine with gear and generator-rotor"
    extends Partials.TurboBase4(final n=1);

    replaceable record Data =
      PowerSystems.Mechanics.TurboGroups.Parameters.WindTurbineGear
      "turbine par"
      annotation(choicesAllMatching=true);
    final parameter Data par "turbine par"
      annotation (Placement(transformation(extent={{-80,80},{-60,100}})));
    Rotational.ElectricRotor genRotor(
      J=par.J_gen,
      w(start=w_start*par.ratio[end]/par.ratio[1]),
      a(start=0))
      annotation (Placement(transformation(extent={{20,-10},{40,10}})));
  protected
    final parameter Real[3] gr2=diagonal(par.ratio)*par.ratio/par.ratio[end]^2;
    final parameter SI.Inertia J_red=par.J_turb*gr2[1] + par.J_gear*gr2 + par.J_gen
      "gear reduced inertia";
    Rotational.WindTurbineRotor turbine(J=par.J_turb)
      annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
    Rotational.ShaftNoMass shaft1(stiff=par.stiff_sh[1])
      annotation (Placement(transformation(extent={{-20,-10},{-10,10}})));
    Rotational.Gear gear(J=par.J_gear, ratio=par.ratio)
      annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
    Rotational.ShaftNoMass shaft2(stiff=par.stiff_sh[2])
      annotation (Placement(transformation(extent={{10,-10},{20,10}})));

  initial equation
    turbine.w = (par.ratio[1]/par.ratio[end])*gear.w;
    der(turbine.w) = (par.ratio[1]/par.ratio[end])*der(gear.w);
    gear.w = genRotor.w;
    der(gear.w) = der(genRotor.w);

  equation
    connect(turbine.flange_b,shaft1.flange_a)  annotation (Line(points={{-20,0},
            {-20,0}}, color={0,0,0}));
    connect(shaft1.flange_b,gear.flange_a)
      annotation (Line(points={{-10,0},{-10,0}}, color={0,0,0}));
    connect(gear.flange_b,shaft2.flange_a)
      annotation (Line(points={{10,0},{10,0}}, color={0,0,0}));
    connect(airgap, genRotor.rotor) annotation (Line(points={{100,60},{30,60},{
            30,6}}, color={0,0,0}));
    connect(blades[1], turbine.rotor) annotation (Line(points={{-100,60},{-30,
            60},{-30,6}}, color={0,0,0}));
    connect(shaft2.flange_b,genRotor.flange_a)  annotation (Line(points={{20,0},
            {20,0}}, color={0,0,0}));
    annotation (defaultComponentName = "windGrp",
      Documentation(
              info="<html>
<p>Wind turbine, gear and generator rotor, coupled with massless shafts.</p>
<p>Note that name and icon of the model are specific, although the turbine model itself is rather generic. The essential type-specific properties are related with torque generation.<br>
An appropriate torque model has to be connected to WindTurbineGear.blades.</p>
<p><i>
No pole pair reduction of equations of motion is performed.<br>
Therefore phi and w represent the mechanical angle and angular velocity.
</i></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Polygon(
            points={{-55,-120},{-55,120},{-47,80},{-39,40},{-39,20},{-43,6},{-55,
                0},{-67,-6},{-71,-20},{-71,-40},{-65,-80},{-55,-120}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-98,-58},{102,-78}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid,
            textString=
                "stator reaction torque- and friction-models may be added here")}));
  end WindTurbineGear;

  model PcontrolTorque "Turbine torque from power control"

    parameter SI.AngularVelocity w_nom = 314.159265358979323846
      "nom r.p.m. turbine"
      annotation(Evaluate=true, Dialog(group="Nominal"));
    parameter SI.Power[:] P_nom={1} "nom power turbines"
      annotation(Evaluate=true, Dialog(group="Nominal"));

    parameter SIpu.AngularVelocity speed_thr(unit="1")=0.5
      "threshold torque ctrl \\ power ctrl";
    SI.Angle phi;
    SIpu.Torque tau_pu(unit="1");
    Modelica.Blocks.Interfaces.RealInput power(
                    final unit="1") "power pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.RealOutput speed(
                                 final unit="1") "angular velocity pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Interfaces.Rotation_n[     n] blades "to turbine model"
      annotation (Placement(transformation(
          origin={100,60},
          extent={{10,10},{-10,-10}},
          rotation=180)));
  protected
    final parameter Integer n=size(P_nom,1) "number of turbines"
                                                               annotation(Evaluate=true);
    final parameter SI.Torque[n] tau_nom=P_nom/w_nom "nom torque"
    annotation(Evaluate=true);

  equation
    max(speed_thr, speed)*tau_pu = power;
    phi=blades[end].phi;
    for k in 1:n loop
      blades[k].tau = -tau_pu*tau_nom[k];
    end for;
    speed = der(phi)/w_nom;
    annotation (defaultComponentName = "turbTorq",
      Documentation(
              info="<html>
<p>This is a default model. The torque is directly determined by the pu power control-signal.
It does neither contain thermal nor hydraulic forces, but it may be replaced by appropriate physical models.</p>
<p>Power control for speed &gt  speed_thr (speed threshold)
<pre>  speed*torq = power</pre>
torque control for speed &lt  speed_thr (speed threshold)
<pre>  speed_thr*torq = power</pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,30},{100,10}},
            lineColor={0,0,127},
            textString="torque"),
          Text(
            extent={{-100,-10},{100,-30}},
            lineColor={0,0,127},
            textString="gen"),
          Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={176,0,0},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,40},{100,20}},
            lineColor={176,0,0},
            fillColor={255,179,179},
            fillPattern=FillPattern.Solid,
            textString="p control"),
          Text(
            extent={{-100,-20},{100,-40}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid,
            textString="torque"),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Polygon(
            points={{-10,10},{0,-10},{10,10},{-10,10}},
            lineColor={176,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end PcontrolTorque;

  model WindTabTorque "Turbine torque, table {speed, torque pu}"

    parameter SI.AngularVelocity w_nom = 314.159265358979323846
      "nom r.p.m. turbine"
      annotation(Evaluate=true, Dialog(group="Nominal"));
    parameter SI.Power P_nom=1 "nom power turbine"
      annotation(Evaluate=true, Dialog(group="Nominal"));

    parameter String tableName="torque" "table name in file";
    parameter String fileName=TableDir+"WindTorque.tab"
      "name of file containing table";

    Modelica.Blocks.Tables.CombiTable1Ds table(table=[0,0,1; 0,1,1],
      final tableName=tableName,
      final fileName=fileName,
      columns={2},
      final tableOnFile=true) "{wind speed m/s, torque pu}"
      annotation (Placement(transformation(extent={{-20,-20},{20,20}})));
    Interfaces.Rotation_n blades "to turbine model"
      annotation (Placement(transformation(
          origin={100,60},
          extent={{10,10},{-10,-10}},
          rotation=180)));
    Modelica.Blocks.Interfaces.RealInput windSpeed "wind speed"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
  protected
    final parameter SI.Torque tau_nom=P_nom/w_nom "nom torque"
    annotation(Evaluate=true);

  equation
    blades.tau = -table.y[1]*tau_nom;

    connect(windSpeed, table.u)
      annotation (Line(points={{-100,0},{-24,0}}, color={0,0,127}));
    annotation (defaultComponentName = "turbTorq",
      Documentation(
              info="<html>
<p>This is a default model. The torque is directly determined by the pu torque-signal. It does not contain aerodynamic forces, but it may be replaced by appropriate physical models.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,30},{100,10}},
            lineColor={0,0,127},
            textString="torque"),
          Text(
            extent={{-100,-10},{100,-30}},
            lineColor={0,0,127},
            textString="gen"),
          Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={0,0,127},
            fillColor={255,255,170},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-100,40},{100,20}},
            lineColor={0,0,127},
            textString="wind tab"),
          Text(
            extent={{-100,-20},{100,-40}},
            lineColor={0,0,127},
            textString="torque"),
          Text(
            extent={{-100,-100},{100,-140}},
            lineColor={0,0,0},
            textString="%name"),
          Polygon(
            points={{-10,10},{0,-10},{10,10},{-10,10}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end WindTabTorque;

package Parameters "Parameter data for interactive use"
  extends Modelica.Icons.BasesPackage;

record SteamTurboGroup "Steam turbo-group parameters"
  extends Modelica.Icons.Record;

  parameter SI.AngularVelocity w_nom = 314.159265358979323846
        "nom r.p.m. turbines"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Power[:] P_nom={500e6,250e6,250e6,250e6} "nom power turbines"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Inertia[size(P_nom,1)] J_turb={20000,200000,200000,200000}
        "inertia turbines";
  parameter SI.Inertia J_gen=70000 "inertia generator";
  parameter SI.Inertia[:] J_aux={500,1000} "inertia auxiliaries";
  parameter SI.Stiffness[size(J_turb,1)+size(J_aux,1)] stiff={250,350,750,750,750,250}*1e6
        "stiffness shafts";

  annotation (defaultComponentName="data",
    Documentation(
    info="<html>
</html>"));
end SteamTurboGroup;

record GasTurbineGear "Turbo-group parameters"
  extends Modelica.Icons.Record;

  parameter SI.AngularVelocity w_nom = 1576.7653528367 "nom r.p.m. turbine"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Power[:] P_nom={12, -2}*1e6 "nom power {turbine, compressor}"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Inertia J_turb=40 "inertia turbine";
  parameter SI.Inertia J_comp=50 "inertia compressor";
  parameter SI.Inertia[2] J_gear1={0.6,12} "inertias gear1";
  parameter SI.Inertia[2] J_gear2={5,200} "inertias gear2";
  parameter SI.Inertia J_acc=6 "inertia accessory";
  parameter SI.Inertia J_cpl=40 "inertia coupling";
  parameter SI.Inertia J_gen=2500 "inertia generator";
  parameter Real[3] ratio={15057,5067,1500} "gear ratio";
  parameter SI.Stiffness[6] stiff_sh={3,5.5,100,2500,250,200}*1e6
        "stiffness shafts";
  parameter SI.TorsionStiffness stiff_cpl=130*1e6 "stiffness coupling";

  annotation (defaultComponentName="data",
    Documentation(
    info="<html>
</html>"));
end GasTurbineGear;

record HydroTurbine "Turbo-group parameters"
  extends Modelica.Icons.Record;

  parameter SI.AngularVelocity w_nom = 314.159265358979323846
        "nom r.p.m. turbine"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Power P_nom=20e6 "nom power turbine"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Inertia J_turb=1000 "inertia turbines";
  parameter SI.Inertia J_shaft=5 "inertia shaft";
  parameter SI.Inertia J_gen=500 "inertia generator";
  parameter SI.TorsionStiffness stiff=300e6 "stiffness shaft";

  annotation (defaultComponentName="data",
    Documentation(
    info="<html>
</html>"));
end HydroTurbine;

record Diesel "Turbo-group parameters"
  extends Modelica.Icons.Record;

  parameter SI.AngularVelocity w_nom = 157.07963267949 "nom r.p.m. Diesel"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Power P_nom=100e3 "nom power diesel"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Inertia J_turb=20 "inertia diesel";
  parameter SI.Inertia J_gen=20 "inertia generator";
  parameter SI.TorsionStiffness stiff=1e6 "stiffness shaft";

  annotation (defaultComponentName="data",
    Documentation(
    info="<html>
</html>"));
end Diesel;

record WindTurbineGear "Turbo-group parameters"
  extends Modelica.Icons.Record;

  parameter SI.AngularVelocity w_nom=1.0471975511966 "nom r.p.m. turbine"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Power P_nom=1e6 "nom power turbine"
    annotation(Evaluate=true, Dialog(group="Nominal"));
  parameter SI.Inertia J_turb=30 "inertia turbine";
  parameter SI.Inertia[3] J_gear={30,2,0.1} "inertias gear";
  parameter SI.Inertia J_gen=450 "inertia generator";
  parameter Real ratio[3]={10,100,1000} "gear ratio";
  parameter SI.TorsionStiffness[2] stiff_sh={16,1}*1e6 "stiffness shafts";

  annotation (defaultComponentName="data",
    Documentation(
    info="<html>
</html>"));
end WindTurbineGear;
  annotation (preferredView="info",
Documentation(info="<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"));
end Parameters;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model TurboBase "Turbine-generator rotor base "

      parameter SI.AngularVelocity w_start=0 "initial rotor angular velocity";
      parameter Integer n=1 "number of turbines";
      Interfaces.Rotation_p[     n] blades "to turbine torque model"
                                                annotation (Placement(
            transformation(extent={{-110,50},{-90,70}})));
      Interfaces.Rotation_n airgap "to airgap electric machine"
                                             annotation (Placement(
            transformation(extent={{90,50},{110,70}})));
    protected
      outer System system;
          annotation (
            Documentation(
                  info="<html>
</html>
"),         Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-10,10},{10,-10}},
              lineColor={0,0,0},
              fillPattern=FillPattern.HorizontalCylinder,
              fillColor={215,215,215}),
            Rectangle(
              extent={{10,50},{100,-50}},
              lineColor={0,0,0},
              fillPattern=FillPattern.HorizontalCylinder,
              fillColor={215,215,215}),
            Rectangle(
              extent={{10,90},{100,70}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              textString="%name"),
            Rectangle(
              extent={{10,-50},{100,-70}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{10,-70},{100,-90}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-100,90},{-10,70}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-100,-70},{-10,-90}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{10,70},{100,50}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid)}));
    end TurboBase;

    partial model TurboBase1 "Turbine-generator rotor base "
      extends Partials.TurboBase;
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Text(
              extent={{-100,-100},{100,-140}},
              lineColor={0,0,0},
              textString="%name"),
            Polygon(
              points={{-100,70},{-100,40},{-20,70},{-100,70}},
              lineColor={176,0,0},
              fillColor={176,0,0},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,-70},{-100,-40},{-20,-70},{-100,-70}},
              lineColor={176,0,0},
              fillColor={176,0,0},
              fillPattern=FillPattern.Solid)}));
    end TurboBase1;

    partial model TurboBase2 "Turbine-generator rotor base "
      extends Partials.TurboBase;
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-100,70},{-10,-70}},
              lineColor={170,213,255},
              fillColor={170,213,255},
              fillPattern=FillPattern.Solid)}));
    end TurboBase2;

    partial model TurboBase3 "Turbine-generator rotor base "
      extends Partials.TurboBase;
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-100,70},{-10,-70}},
              lineColor={255,128,0},
              fillColor={255,128,0},
              fillPattern=FillPattern.Solid)}));
    end TurboBase3;

    partial model TurboBase4 "Turbine-generator rotor base "
      extends Partials.TurboBase;
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-100,90},{-10,-90}},
              lineColor={255,255,170},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{-100,14},{-10,-14}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid)}));
    end TurboBase4;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Contains a single mass and examples of multi-mass models of turbo groups.</p>
<li>Default torque models</li>
</html>
"));
end TurboGroups;
