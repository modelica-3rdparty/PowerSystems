within PowerSystems.Control;
package Governors "Turbine Governors "
  extends Modelica.Icons.VariantsPackage;

  block GovernorSimple "Simple governor for constant turbine power"

    parameter Boolean par=true "p parameter or initialised?"   annotation(evaluate=true,
      choices(choice=true "parameter", choice=false "initialised"));
    parameter SIpu.Power p(unit="1", fixed=par)=1 "turbine power"
                                                annotation(Dialog(enable=par));
    Modelica.Blocks.Interfaces.RealInput speed(
                                     final unit="1") "speed of turbine pu"
      annotation (Placement(transformation(
          origin={-60,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput power(
                           final unit="1") "power of turbine pu"
      annotation (Placement(transformation(
          origin={60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  equation
    power = p;
    annotation (defaultComponentName = "governor",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-52,32},{48,-28}},
            lineColor={128,128,128},
            lineThickness=0.5,
            textString=
           "simp"),
          Rectangle(
            extent={{-80,60},{80,-60}},
            lineColor={0,0,127},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-52,32},{48,-28}},
            lineColor={128,128,128},
            lineThickness=0.5,
            textString=
           "simp"),
          Text(
            extent={{-100,140},{100,100}},
            lineColor={0,0,0},
            textString=
             "%name")}),
      Documentation(
              info="<html>
<p>Constant excitation-voltage.</p>
<p><tt>fieldVoltage=1</tt> corresponds to <tt>norm(v)=1 pu</tt> at open generator terminals.</p>
</html>
"),   Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(extent={{0,20},{20,0}}, lineColor={95,95,95}),
          Line(points={{20,10},{60,10},{60,10},{60,-100}}, color={95,95,95}),
          Text(
            extent={{0,20},{20,0}},
            lineColor={95,95,95},
            fillColor={127,0,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "p_set")}));
  end GovernorSimple;

  block GovernorConst "Governor for constant turbine power"
    extends Partials.GovernorBase;

  initial equation
    setptSpeed = speed;

  equation
    connect(setptPower, limiter.u)  annotation (Line(points={{-100,-40},{-26,
            -40},{-26,0},{48,0}}, color={0,0,127}));
    annotation (defaultComponentName = "governor",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-52,32},{48,-28}},
            lineColor={128,128,128},
            lineThickness=0.5,
            textString=
                 "const")}),
      Documentation(
              info="<html>
<p>Constant turbine power (setpoint value).</p>
</html>
"),   Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end GovernorConst;

  block Governor1st "Governor first order"
    extends Partials.GovernorBase;

    parameter Real k=20 "Gain";
    parameter Real t=5 "time constant speed regulator";
  protected
    outer System system;
    final parameter Modelica.Blocks.Types.Init initType=if system.steadyIni then
           Modelica.Blocks.Types.Init.SteadyState else
           Modelica.Blocks.Types.Init.NoInit;
    Modelica.Blocks.Math.Add delta_speed(k2=-1)
                                   annotation (Placement(transformation(extent=
              {{-70,-10},{-50,10}}, rotation=0)));
    Modelica.Blocks.Continuous.TransferFunction speedReg(
      initType=initType,
      a={t,1},
      b={k})  annotation (Placement(transformation(extent={{-30,-10},{-10,10}},
            rotation=0)));
    Modelica.Blocks.Math.Add delta_power
                            annotation (Placement(transformation(extent={{10,
              -10},{30,10}}, rotation=0)));

  equation
    connect(setptSpeed, delta_speed.u1)  annotation (Line(points={{-100,40},{
            -80,40},{-80,6},{-72,6}}, color={0,0,127}));
    connect(speed, delta_speed.u2) annotation (Line(points={{-60,-100},{-60,-80},
            {-80,-80},{-80,-6},{-72,-6}}, color={0,0,127}));
    connect(delta_speed.y, speedReg.u)
      annotation (Line(points={{-49,0},{-32,0}}, color={0,0,127}));
    connect(speedReg.y, delta_power.u1) annotation (Line(points={{-9,0},{0,0},{
            0,6},{8,6}}, color={0,0,127}));
    connect(setptPower, delta_power.u2)  annotation (Line(points={{-100,-40},{0,
            -40},{0,-6},{8,-6}}, color={0,0,127}));
    connect(delta_power.y, limiter.u)
      annotation (Line(points={{31,0},{48,0}}, color={0,0,127}));
    annotation (defaultComponentName = "governor",
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Documentation(
              info="<html>
<p>First order control of turbine power.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,34},{60,-26}},
            lineColor={128,128,128},
            textString=
           "1st")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Governor1st;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

  partial block GovernorBase "Governor base"
    extends PowerSystems.Basic.Icons.Block1;

    parameter SIpu.Power[2] p_minmax(each unit="1")={0,2}
        "{min,max} turbine power";
    protected
    outer System system;
    public
    Modelica.Blocks.Interfaces.RealInput setptPower(
                           final unit="1") "setpoint power pu"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}},
              rotation=0)));
    Modelica.Blocks.Interfaces.RealInput setptSpeed(
                                     final unit="1") "setpoint speed pu"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}},
              rotation=0)));
    Modelica.Blocks.Interfaces.RealInput speed(
                                     final unit="1") "turbine speed pu"
      annotation (Placement(transformation(
            origin={-60,-100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput power(
                           final unit="1") "turbine power pu"
      annotation (Placement(transformation(
            origin={60,-100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    Modelica.Blocks.Nonlinear.Limiter limiter(uMin=p_minmax[1], uMax=p_minmax[2])
      annotation (Placement(transformation(extent={{50,-10},{70,10}}, rotation=
                0)));

  equation
    connect(limiter.y, power)   annotation (Line(points={{71,0},{80,0},{80,-80},
              {60,-80},{60,-100}}, color={0,0,127}));
    annotation (defaultComponentName = "governor",
      Window(
        x=0.45,
        y=0.01,
        width=0.44,
        height=0.65),
      Documentation(info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
  end GovernorBase;
    annotation (Documentation(info="<html>
</html>"));
  end Partials;
  annotation (preferedView="info",
Window(
  x=0.05,
  y=0.44,
  width=0.31,
  height=0.28,
  library=1,
  autolayout=1),
Documentation(info="<html>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics),
    Diagram(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Governors;
