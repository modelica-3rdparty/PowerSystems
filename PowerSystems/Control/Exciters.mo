within PowerSystems.Control;
package Exciters "Generator Exciters "
  extends Modelica.Icons.VariantsPackage;

  block ExciterSimple "Simple exciter for constant field voltage"

    parameter Boolean par=true "v_f parameter or initialised?"   annotation(Evaluate=true,
      choices(choice=true "parameter", choice=false "initialised"));
    parameter SIpu.Voltage v_f(unit="1", fixed=par)=1 "exciter voltage"
                                                              annotation(Dialog(enable=par));
    Modelica.Blocks.Interfaces.RealInput termVoltage[3](each final unit="1")
      "terminal voltage pu"
      annotation (Placement(transformation(
          origin={-60,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput fieldVoltage(final unit="1")
      "field voltage pu"
      annotation (Placement(transformation(
          origin={60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  equation
    fieldVoltage = v_f;
    annotation (defaultComponentName = "exciter",
      Documentation(
              info="<html>
<p>Constant excitation-voltage.</p>
<p><tt>fieldVoltage=1</tt> corresponds to <tt>norm(v)=1 pu</tt> at open generator terminals.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
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
                 "v_f_set")}));
  end ExciterSimple;

  block ExciterConst "Exciter for constant field voltage"
    extends Partials.ExciterBase;

  equation
    connect(setptVoltage, limiter.u)
      annotation (Line(points={{-100,0},{48,0}}, color={0,0,127}));
    annotation (defaultComponentName = "exciter",
      Documentation(
              info="<html>
<p>Constant excitation-voltage (setpoint value).</p>
<p><tt>fieldVoltage=1</tt> corresponds to <tt>norm(v)=1 pu</tt> at open generator terminals.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,34},{60,-26}},
            lineColor={128,128,128},
            textString=
                 "const")}));
  end ExciterConst;

  block Exciter1st "Exciter first order"
    extends Partials.ExciterBase;

    parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
      annotation(Evaluate=true, Dialog(tab="Mode"));
    parameter Real k=50 "gain";
    parameter SI.Time t=0.005 "time constant voltage regulator";
  protected
    outer System system;
    final parameter Modelica.Blocks.Types.Init initType=
      if dynType == Types.Dynamics.SteadyInitial then
           Modelica.Blocks.Types.Init.SteadyState else
           Modelica.Blocks.Types.Init.NoInit;
    Blocks.Math.Norm norm(final n=3, n_eval=3)
      annotation (Placement(transformation(
          origin={-60,-70},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Math.Add delta_voltage(k1=+1, k2=-1)
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    Modelica.Blocks.Continuous.TransferFunction voltageReg(
      initType=initType,
      a={t,1},
      b={k})
      annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));

  equation
    connect(setptVoltage, delta_voltage.u1)  annotation (Line(points={{-100,0},
            {-80,0},{-80,6},{-72,6}}, color={0,0,127}));
    connect(termVoltage, norm.u) annotation (Line(points={{-60,-100},{-60,-80}},
          color={0,0,127}));
    connect(norm.y, delta_voltage.u2) annotation (Line(points={{-60,-60},{-60,
            -40},{-80,-40},{-80,-6},{-72,-6}}, color={0,0,127}));
    connect(delta_voltage.y, voltageReg.u)
      annotation (Line(points={{-49,0},{-32,0}}, color={0,0,127}));
    connect(voltageReg.y, limiter.u)
      annotation (Line(points={{-9,0},{48,0}}, color={0,0,127}));
    annotation (defaultComponentName = "exciter",
      Documentation(
              info="<html>
<p>First order control of excitation-voltage.</p>
<p><tt>fieldVoltage=1</tt> corresponds to <tt>norm(v)=1 pu</tt> at open generator terminals.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,34},{60,-26}},
            lineColor={128,128,128},
            textString=
           "1st")}));
  end Exciter1st;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

  partial block ExciterBase "Exciter base"
    extends PowerSystems.Icons.Block1;

    parameter SIpu.Voltage[2] v_f_minmax(each unit="1")={0, 3}
        "{min,max} exciter voltage";
    Modelica.Blocks.Interfaces.RealInput setptVoltage(
                             final unit="1")
        "setpoint norm of terminal voltage pu"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealInput termVoltage[3](
                             each final unit="1") "terminal voltage pu"
      annotation (Placement(transformation(
            origin={-60,-100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput fieldVoltage(
                             final unit="1") "field voltage pu"
      annotation (Placement(transformation(
            origin={60,-100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    Modelica.Blocks.Nonlinear.Limiter limiter(uMin=v_f_minmax[1],uMax=v_f_minmax[2])
      annotation (Placement(transformation(extent={{50,-10},{70,10}})));

  equation
    connect(limiter.y, fieldVoltage) annotation (Line(points={{71,0},{80,0},{80,
              -80},{60,-80},{60,-100}}, color={0,0,127}));
    annotation (defaultComponentName = "exciter",
      Documentation(
              info="<html>
</html>"));
  end ExciterBase;
    annotation (Documentation(info="<html>
</html>"));
  end Partials;
  annotation (preferredView="info",
Documentation(info="<html>
</html>
"));
end Exciters;
