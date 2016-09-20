within PowerSystems.Control;
package Setpoints "Setpoints of generators"
  extends Modelica.Icons.VariantsPackage;

block Set_w_p_v "Constant setpoints w, p, v for generation"

  Modelica.Blocks.Interfaces.RealOutput[3] setpts
      "setpoints {speed, power, voltage} pu"
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  protected
  parameter SIpu.AngularVelocity w_set(unit="1", fixed=false, start=1)
      "setpt turbine speed pu";
  parameter SIpu.Power p_set(unit="1", fixed=false, start=1)
      "setpt turbine power pu";
  parameter SIpu.Voltage v_set(unit="1", fixed=false, start=1)
      "setpt exciter voltage pu";

equation
  setpts[1] = w_set;
  setpts[2] = p_set;
  setpts[3] = v_set;
  annotation (defaultComponentName = "setpts1",
    Documentation(
            info="<html>
<p>The speed-, power-, and voltage setpoints setpts[1:3] are determined by the corresponding setpoint-parameters.<br>
With attribute 'fixed=false' these are fixed at steady-state initialisation.<br>
They depend on the chosen (initial) system-frequency f0, the initial terminal voltage and the initial active power.<br>
The setpoints are kept constant during simulation.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{0,80},{80,-80}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-60,-90},{140,-130}},
            lineColor={0,0,0},
            textString=
           "%name"),
          Rectangle(
            extent={{20,76},{60,-76}},
            lineColor={230,250,180},
            fillColor={230,250,180},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{0,80},{80,40}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "w"),
          Text(
            extent={{0,20},{80,-20}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "p"),
          Text(
            extent={{0,-40},{80,-80}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "v")}));
end Set_w_p_v;

block SetVariable_w_p_v "Variable setpoints w, p, v for generation"

  Modelica.Blocks.Interfaces.RealInput setpt_w "setpoint turbine speed pu"
    annotation (Placement(transformation(extent={{-30,50},{-10,70}})));
  Modelica.Blocks.Interfaces.RealInput setpt_p "setpoint turbine power pu"
    annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));
  Modelica.Blocks.Interfaces.RealInput setpt_v "setpoint exciter voltage pu"
    annotation (Placement(transformation(extent={{-30,-70},{-10,-50}})));
  Modelica.Blocks.Interfaces.RealOutput[3] setpts
      "setpoints {speed, power, voltage} pu"
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  protected
  parameter SIpu.AngularVelocity w_set(unit="1", fixed=false, start=1)
      "setpt turbine speed pu";
  parameter SIpu.Power p_set(unit="1", fixed=false, start=1)
      "setpt turbine power pu";
  parameter SIpu.Voltage v_set(unit="1", fixed=false, start=1)
      "setpt exciter voltage pu";

initial equation
  setpts[1] = w_set;
  setpts[2] = p_set;
  setpts[3] = v_set;

equation
  setpts[1] = setpt_w;
  setpts[2] = setpt_p;
  setpts[3] = setpt_v;
  annotation (defaultComponentName = "setpts1",
    Documentation(
            info="<html>
<p>The speed-, power-, and voltage setpoints setpts[1:3] are are taken from inputs set_w, setpt_p, set_v.<br>
The setpoint parameters with attribute 'fixed=false' are fixed at steady-state initialisation and represent the steady-state initial values.<br>
They depend on the chosen (initial) system-frequency f0, the initial terminal voltage and the initial active power.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{0,80},{80,-80}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-60,-90},{140,-130}},
            lineColor={0,0,0},
            textString=
           "%name"),
          Text(
            extent={{0,80},{80,40}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "w"),
          Text(
            extent={{0,20},{80,-20}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "p"),
          Text(
            extent={{0,-40},{80,-80}},
            lineColor={95,95,95},
            lineThickness=0.5,
            textString=
               "v")}));
end SetVariable_w_p_v;

block Set_w_p "Set-points for generation"

  parameter SIpu.AngularVelocity w_set(unit="1", fixed=true)=1
      "setpoint turbine speed pu";
  Modelica.Blocks.Interfaces.RealInput setpt_p "setpoint turbine power pu"
    annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));
  Modelica.Blocks.Interfaces.RealOutput[2] setpts "setpoints {speed, power} pu"
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));

equation
  setpts[1] = w_set;
  setpts[2] = setpt_p;
  annotation (defaultComponentName = "setpts1",
    Documentation(
            info="<html>
<p>The speed setpoint setpts[1] is directly determined by the parameter w_set with attribute 'fixed=true'.<br>
The power setpoint setpts[2] is taken from input setpt_p.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{0,80},{80,-80}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-60,-90},{140,-130}},
            lineColor={0,0,0},
            textString=
           "%name"),
          Text(
            extent={{0,80},{80,40}},
            lineColor={95,95,95},
            textString=
               "w"),
          Text(
            extent={{0,20},{80,-20}},
            lineColor={95,95,95},
            textString=
               "p")}));
end Set_w_p;
  annotation (Documentation(info="<html>
</html>"));
end Setpoints;
