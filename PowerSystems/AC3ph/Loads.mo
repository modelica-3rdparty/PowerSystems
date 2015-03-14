within PowerSystems.AC3ph;
package Loads "Loads"
  extends Modelica.Icons.VariantsPackage;

  model Zload "Impedance load, 3-phase dq0"
    extends Partials.IndLoadBase;

  equation
    Z = (p0/(p0*p0))*V2_nom;
    annotation (
      defaultComponentName="zLoad",
  Documentation(
          info="<html>
<p>Inductive load with impedance characteristic.<br>
Consumes the desired active and reactive power at <b>nominal</b> voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-98,28},{-18,-32}},
            lineColor={176,0,0},
            lineThickness=0.5,
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
             "Z")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end Zload;

  model PQindLoad "PQ inductive load, 3-phase dq0"
    extends Partials.IndLoadBase;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter SI.Time tcst=0.01 "time constant Z";
  protected
    Real v2 = v*v;

  initial equation
    der(Z) = {0, 0};

  equation
  //  der(Z) = ((p0/(p0*p0))*v2 - Z)/tcst;
    der(Z) = ((p0/(p0*p0))*v2*tanh(imax)/tanh((imax/V2_nom)*v2) - Z)/tcst;
    annotation (
      defaultComponentName="pqLoad",
  Documentation(
          info="<html>
<p>Inductive load with constant characteristic.<br>
Consumes the desired active and reactive power independent of voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-96,36},{44,-24}},
            lineColor={176,0,0},
            lineThickness=0.5,
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
             "p   q")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end PQindLoad;

  model Yload "Admittance load, 3-phase dq0"
    extends Partials.CapLoadBase;

  equation
    Y = (p0/(p0*p0))*I2_nom;
    annotation (
      defaultComponentName="yLoad",
  Documentation(
          info="<html>
<p>Capacitive load with admittance characteristic.<br>
Consumes the desired active and reactive power at <b>nominal</b> voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,28},{-20,-32}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "Y")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-70,0},{-4,0}}),
          Rectangle(
            extent={{-4,21},{-2,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,21},{4,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,5},{-2,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,5},{4,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,-11},{-2,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-11},{4,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-70,16},{-4,16}}),
          Line(points={{-70,-16},{-4,-16}}),
          Line(points={{4,16},{30,16}}),
          Line(points={{4,0},{30,0}}),
          Line(points={{4,-16},{30,-16}}),
          Rectangle(
            extent={{-20,10},{-10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-6},{-10,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-22},{-10,-26}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-30,16},{-30,8},{-20,8}}, color={0,0,255}),
          Line(points={{-30,0},{-30,-8},{-20,-8}}, color={0,0,255}),
          Line(points={{-30,-16},{-30,-24},{-20,-24}}, color={0,0,255}),
          Line(points={{-10,8},{20,8},{20,16}}, color={0,0,255}),
          Line(points={{-10,-8},{20,-8},{20,0}}, color={0,0,255}),
          Line(points={{-10,-24},{20,-24},{20,-16}}, color={0,0,255})}));
  end Yload;

  model PQcapLoad "PQ capacitive load, 3-phase dq0"
    extends Partials.CapLoadBase;

    parameter SIpu.Voltage vmax(unit="1")=2 "maximum voltage";
    parameter SI.Time tcst=0.01 "time constant Y";
  protected
    Real i2 = i*i;

  initial equation
    der(Y) = {0, 0};

  equation
  //  der(Y) = ((p0/(p0*p0))*i2 - Y)/tcst;
    der(Y) = ((p0/(p0*p0))*i2*tanh(vmax)/tanh((vmax/I2_nom)*i2) - Y)/tcst;
    annotation (
      defaultComponentName="pqLoad",
  Documentation(
          info="<html>
<p>Capacitive load with constant characteristic.<br>
Consumes the desired active and reactive power independent of voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,36},{40,-24}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
             "p   q")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-70,0},{-4,0}}),
          Rectangle(
            extent={{-4,21},{-2,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,21},{4,11}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,5},{-2,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,5},{4,-5}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-4,-11},{-2,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{2,-11},{4,-21}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-70,16},{-4,16}}),
          Line(points={{-70,-16},{-4,-16}}),
          Line(points={{4,16},{30,16}}),
          Line(points={{4,0},{30,0}}),
          Line(points={{4,-16},{30,-16}}),
          Rectangle(
            extent={{-20,10},{-10,6}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-6},{-10,-10}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,-22},{-10,-26}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-30,16},{-30,8},{-20,8}}, color={0,0,255}),
          Line(points={{-30,0},{-30,-8},{-20,-8}}, color={0,0,255}),
          Line(points={{-30,-16},{-30,-24},{-20,-24}}, color={0,0,255}),
          Line(points={{-10,8},{20,8},{20,16}}, color={0,0,255}),
          Line(points={{-10,-8},{20,-8},{20,0}}, color={0,0,255}),
          Line(points={{-10,-24},{20,-24},{20,-16}}, color={0,0,255})}));
  end PQcapLoad;

  model ZIPload "ZIP inductive load, 3-phase dq0"
    extends Partials.IndLoadBase;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter Real[2] aZ={1/3,1/3} "weight(power) impedance-dependent";
    parameter Real[2] aI={1/3,1/3} "weight(power) current-dependent";
    parameter Real[2] aP={1,1}-aZ-aI "weight(power) fixed";
    parameter SI.Time tcst=0.01 "time constant Z";
  protected
    SI.Power[2] p(start=p0_set);
    Real v2 = v*v;
    Real v2_pu = v2/V2_nom;

  initial equation
    der(Z) = {0, 0};

  equation
    p =  diagonal(aZ*v2_pu + aI*sqrt(v2_pu) + aP)*p0;
  //  der(Z) = ((p/(p*p))*v2 - Z)/tcst;
    der(Z) = ((p/(p*p))*v2*tanh(imax)/tanh(imax*v2_pu) - Z)/tcst;
    annotation (
      defaultComponentName="zipLoad",
  Documentation(
          info="<html>
<p>Inductive load with characteristic depending on powers 0,1,2 of voltage or current.<br>
Consumes the desired active and reactive power at <b>nominal</b> voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,26},{20,-34}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "ZIP")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end ZIPload;

  model FrequencyLoad "Frequency inductive load, 3-phase dq0"
    extends Partials.IndLoadBase;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter Real[2] af={1,1} "frequency sensitivity";
    parameter Real[2] aV={1,1} "voltage sensitivity";
    parameter SI.Time tcst=0.01 "time constant Z";
  protected
    final parameter Real[2] aw=af/(2*pi);
    SI.Power[2] p(start=p0_set);
    Real v2 = v*v;
    Real v2_pu = v2/V2_nom;

  initial equation
    der(Z) = {0, 0};

  equation
    p = diagonal({1,1} + aV*(sqrt(v2_pu)-1) + aw*(sum(omega) - system.omega_nom))*p0;
  //  der(Z) = ((p/(p*p))*v2 - Z)/tcst;
    der(Z) = ((p/(p*p))*v2*tanh(imax)/tanh(imax*v2_pu) - Z)/tcst;
    annotation (
      defaultComponentName="freqLoad",
  Documentation(
          info="<html>
<p>Inductive load with frequency and voltage sensitive characteristic.<br>
Consumes the desired active and reactive power at <b>nominal</b> voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,26},{-20,-34}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
             "f")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end FrequencyLoad;

  model DynamicLoad "Dynamic inductive load, 3-phase dq0"
    extends Partials.IndLoadBase;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter Real[2] as={0.5,1} "voltage exponent steady-state power";
    parameter Real[2] at={2,2} "voltage exponent transient power";
    parameter SI.Time[2] t_rec={60,60} "power recovery times";
    parameter SI.Time tcst=0.01 "time constant Z";
  protected
    SI.Power[2] p(start=p0_set);
    Real v2 = v*v;
    Real v2_pu = v2/V2_nom;
    Real[2] x;
    Real[2] vs;
    Real[2] vt;
    Real[2] xT;

  initial equation
    if system.steadyIni_t then
      der(x) = {0,0};
    end if;
    der(Z) = {0,0};

  equation
    vs = {v2_pu^(as[1]/2), v2_pu^(as[2]/2)};
    vt = {v2_pu^(at[1]/2), v2_pu^(at[2]/2)};
    xT = {x[1]/t_rec[1], x[2]/t_rec[2]};
    der(x) = diagonal(vs - vt)*p0 - xT;
    p =  diagonal(vt)*p0 + xT;
  //  der(Z) = ((p/(p*p))*v2 - Z)/tcst;
    der(Z) = ((p/(p*p))*v2*tanh(imax)/tanh(imax*v2_pu) - Z)/tcst;
    annotation (
      defaultComponentName="dynLoad",
  Documentation(
          info="<html>
<p>Inductive load with characteristic depending on dynamic state.<br>
Consumes the desired active and reactive power at steady state and <b>nominal</b> voltage.</p>
</html>"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-70,28},{10,-32}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
             "dyn")}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,3},{30,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,3},{-50,-4}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,20},{30,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,20},{-50,13}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-50,-13},{30,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-13},{-50,-20}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid)}));
  end DynamicLoad;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model LoadBase "Load base, 3-phase dq0"
      extends Ports.Yport_p;
      extends Basic.Nominal.Nominal;

      parameter Boolean stIni_en=true "enable steady-state initial equation"
                annotation(Evaluate=true, choices(__Dymola_checkBox=true));

      parameter Boolean scType_par = true
        "= true if p0 defined by parameter p0_set otherwise by input signal p_set"
        annotation(Evaluate=true, choices(__Dymola_checkBox=true));

      parameter SIpu.Power[2] p0_set(each min=0)={sqrt(3),1}/2
        "{active, reactive} power, (start val if signal inp)" annotation(Dialog(enable=scType_par));
      parameter SIpu.Resistance r_n=0 "resistance neutral to grd";
      Modelica.Blocks.Interfaces.RealInput[2] p_set(each min=0) if not scType_par
        "desired {active, reactive} power"                 annotation (Placement(
            transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

    protected
      outer System system;
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;
      final parameter Real S_base=Basic.Precalculation.baseS(      puUnits, S_nom);
      final parameter Real R_base=Basic.Precalculation.baseR(      puUnits, V_nom, S_nom);
      final parameter SI.Resistance R_n=r_n*R_base;
      SI.AngularFrequency[2] omega;
      SI.Power[2] p0(start=p0_set);
      Modelica.Blocks.Interfaces.RealInput[2] p_set_internal
        "Needed to connect to conditional connector";
    equation
      connect(p_set, p_set_internal);

      omega = der(term.theta);
      if scType_par then
        p0 =  p0_set*S_base;
        p_set_internal = {0.0, 0.0};
      else
        p0 = p_set_internal*S_base;
      end if;
      v_n = R_n*i_n "equation neutral to ground";
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{70,20},{76,-20}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid)}),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
              points={{-80,-60},{-80,60},{80,0},{-80,-60}},
              lineColor={0,120,120},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}));
    end LoadBase;

    partial model IndLoadBase "Inductive load base, 3-phase dq0"
      extends LoadBase(v(start={vstart[1],vstart[2],0}), i(start={istart[1],istart[2],0}));

      parameter Real cpl(min=-0.5,max=1)=0
        "phase coupling x_m/x_s, (-1/2 < cpl < 1)";
      SI.MagneticFlux[3] psi(each stateSelect=StateSelect.prefer)
        "magnetic flux";
    protected
      final parameter Real c0=(1 + 2*cpl)/(1 - cpl);
      final parameter Real V2_nom=V_nom*V_nom;
      final parameter Real[2] Zstart=(p0_set/(p0_set*p0_set*S_base))*V2_nom;
      final parameter SI.Voltage[2] vstart={cos(system.alpha0), sin(system.alpha0)}*V_nom;
      final parameter SI.Current[2] istart=[Zstart[1],Zstart[2];-Zstart[2],Zstart[1]]*vstart/(Zstart*Zstart);
      SI.Impedance[2] Z(start=Zstart);

    initial equation
      if steadyIni_t then
        der(psi) = omega[1]*{-psi[2], psi[1], 0};
      end if;

    equation
      psi = Z[2]*{i[1], i[2], c0*i[3]}/system.omega_nom;
      if system.transientSim then
        der(psi) + omega[2]*j_dq0(psi) + Z[1]*i = v;
      else
        omega[2]*j_dq0(psi)  + Z[1]*i = v;
      end if;
      annotation (
        Documentation(info=
        "<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
              points={{-40,-45},{-40,45},{80,0},{-40,-45}},
              lineColor={0,120,120},
              lineThickness=0.5,
              fillColor={0,120,120},
              fillPattern=FillPattern.Solid)}));
    end IndLoadBase;

    partial model CapLoadBase "Capacitive load base, 3-phase dq0"
      extends LoadBase(v(start={vstart[1],vstart[2],0}), i(start={istart[1],istart[2],0}));

      parameter Real beta(min=0)=0 "ratio b_pp/b_pg, (beta > 0)";
      SI.ElectricCharge[3] q(each stateSelect=StateSelect.prefer)
        "electric charge";
    protected
      final parameter Real c0=1/(1+3*beta);
      final parameter Real I2_nom=(S_nom/V_nom)^2;
      final parameter SI.Admittance[2] Ystart=(p0_set/(p0_set*p0_set*S_base))*I2_nom;
      final parameter SI.Voltage[2] vstart={cos(system.alpha0), sin(system.alpha0)}*V_nom;
      final parameter SI.Current[2] istart=[Ystart[1],-Ystart[2];Ystart[2],Ystart[1]]*vstart;
      SI.Admittance[2] Y(start=Ystart);

    initial equation
      if steadyIni_t then
        der(q) = omega[1]*{-q[2], q[1], 0};
      end if;

    equation
      q = Y[2]*{v[1], v[2], c0*v[3]}/system.omega_nom;
      if system.transientSim then
        der(q) + omega[2]*j_dq0(q) + Y[1]*v = i;
      else
        omega[2]*j_dq0(q) + Y[1]*v = i;
      end if;
      annotation (
        Documentation(info=
        "<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Polygon(
              points={{-40,44},{-40,-44},{-20,-36},{-20,36},{-40,44}},
              lineColor={0,0,255},
              pattern=LinePattern.None,
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-50,48},{-50,-48},{-40,-44},{-40,44},{-50,48}},
              lineColor={0,120,120},
              fillColor={0,120,120},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-20,36},{-20,-36},{-10,-33},{-10,33},{-20,36}},
              lineColor={0,120,120},
              fillColor={0,120,120},
              fillPattern=FillPattern.Solid)}));
    end CapLoadBase;

  end Partials;

annotation (preferredView="info",
    Documentation(info="<html>
<p>Load models with an optional input (if scType=signal):</p>
<pre>  p_set:     {active, reactive} power</pre>
</html>
"));
end Loads;
