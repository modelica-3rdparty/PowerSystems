within PowerSystems.AC1ph_DC;
package Loads "Loads"
  model Rload "Resistance load, 1-phase"
    extends Partials.ResLoadBase;

  equation
    R = V2_nom/p0;
  annotation (
    defaultComponentName="rLoad",
      Documentation(
        info="<html>
<p>Resistive load AC or DC with impedance characteristic.<br>
Consumes the desired power at <b>nominal</b> voltage.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,28},{0,-32}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "R")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Rload;
  extends Modelica.Icons.VariantsPackage;

  model ZloadAC "Impedance load AC, 1-phase"
    extends Partials.IndLoadBaseAC;

  equation
    Z = (p0/(p0*p0))*V2_nom;
  annotation (
    defaultComponentName="zLoadAC",
      Documentation(
        info="<html>
<p>Inductive load AC with impedance characteristic.<br>
Consumes the desired active and reactive power at <b>nominal</b> voltage.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-98,28},{-18,-32}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
           "Z"), Text(
            extent={{-28,29},{52,-31}},
            lineColor={255,255,255},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ZloadAC;

  model YloadAC "Admittance load AC, 1-phase"
    extends Partials.CapLoadBaseAC;

  equation
    Y = (p0/(p0*p0))*I2_nom;
  annotation (
    defaultComponentName="yLoadAC",
      Documentation(
        info="<html>
<p>Capacitive load AC with admittance characteristic.<br>
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
               "Y"), Text(
            extent={{-28,29},{52,-31}},
            lineColor={0,0,255},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "~")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end YloadAC;

  model ZloadDC "Impedance load DC"
    extends Partials.IndLoadBaseDC;

  equation
    R = V2_nom/p0;
    L = t_RL*R;
  annotation (
    defaultComponentName="zLoadDC",
      Documentation(
        info="<html>
<p>Resistive-inductive load DC with impedance characteristic.<br>
Consumes the desired power at <b>nominal</b> voltage.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-98,28},{-18,-32}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
           "Z"), Text(
            extent={{-28,29},{52,-31}},
            lineColor={255,255,255},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "=")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end ZloadDC;

  model PindLoadDC "Inductive load DC"
    extends Partials.IndLoadBaseDC;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter SI.Time tcst=0.01 "time constant R";
  protected
    Real v2 = v*v;

  initial equation
    der(R) = 0;

  equation
  //  der(R) = (v2/p0 - R)/tcst;
    der(R) = ((v2/p0)*tanh(imax)/tanh(imax*v2/V2_nom) - R)/tcst;
    L = t_RL*R;
  annotation (
    defaultComponentName="pLoadDC",
      Documentation(
        info="<html>
<p>Resistive-inductive load DC with constant characteristic.<br>
Consumes the desired power independent of voltage.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-98,36},{-18,-24}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "p"), Text(
            extent={{-28,29},{52,-31}},
            lineColor={255,255,255},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "=")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end PindLoadDC;

  model PresLoadDC "P resistive load"
    extends Partials.ResLoadBase;

    parameter SIpu.Current imax(unit="1")=2 "maximum current";
    parameter SI.Time tcst=0.01 "time constant R";
  protected
    Real v2 = v*v;

  initial equation
    der(R) = 0;

  equation
  //  der(R) = (v2/p0 - R)/tcst;
    der(R) = ((v2/p0)*tanh(imax)/tanh(imax*v2/V2_nom) - R)/tcst;
  annotation (
    defaultComponentName="pLoadDC",
      Documentation(
        info="<html>
<p>Resistive load DC with constant characteristic.<br>
Consumes the desired power independent of voltage.</p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,36},{0,-24}},
            lineColor={176,0,0},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "p"), Text(
            extent={{-28,29},{52,-31}},
            lineColor={0,0,255},
            fillColor={128,128,128},
            fillPattern=FillPattern.Solid,
            textString=
                 "=")}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end PresLoadDC;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model LoadBase "Load base, 1-phase"
      extends Basic.Nominal.Nominal;
      extends Ports.Port_p;

      parameter Boolean stIni_en=true "enable steady-state initialization"
        annotation(Evaluate=true, Dialog(tab="Initialization"));
      parameter SI.Voltage v_start = 0
        "start value of voltage drop" annotation(Dialog(tab="Initialization"));
      parameter SI.Current i_start = 0
        "start value of current" annotation(Dialog(tab="Initialization"));

      SI.Voltage v(start = v_start);
      SI.Current i(start = i_start);

    protected
      outer System system;
      final parameter Real S_base=Basic.Precalculation.baseS(puUnits, S_nom);
      final parameter Boolean steadyIni_t=system.steadyIni_t and stIni_en;

    equation
      term.i[1] + term.i[2] = 0;
      v = term.v[1] - term.v[2];
      i = term.i[1];
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
              points={{-80,-60},{-80,60},{80,0},{-80,-60}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}));
    end LoadBase;

    partial model ResLoadBase "Resistive load base, 1-phase"
      extends LoadBase(v(start=V_nom), i(start=V_nom/Rstart));

      parameter Boolean scType_par = true
        "= true if p0 defined by parameter p0_set otherwise by input signal p_set"
        annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      parameter SIpu.Power p0_set(min=0)=1 "power, (start val if signal inp)"    annotation(Dialog(enable=scType_par));
      Modelica.Blocks.Interfaces.RealInput p_set(min=0) if not scType_par
        "desired power"                                    annotation (Placement(
            transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

    protected
      Modelica.Blocks.Interfaces.RealInput p_set_internal
        "Needed to connect to conditional connector";

      final parameter Real V2_nom(unit="V2")=V_nom*V_nom;
      final parameter Real Rstart=V2_nom/(p0_set*S_base);
      SI.Power p0;
      SI.Resistance R(start=Rstart);
    equation
      connect(p_set, p_set_internal);

      if scType_par then
        p0 =  p0_set*S_base;
        p_set_internal = 0.0;
      else
        p0 = p_set_internal*S_base;
      end if;
      R*i = v;
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end ResLoadBase;

    partial model LoadBaseAC "Load base AC, 1-phase"
      extends LoadBase;

      parameter Boolean scType_par = true
        "= true if p0 defined by parameter p0_set otherwise by input signal p_set"
        annotation(Evaluate=true, choices(__Dymola_checkBox=true));

      parameter SIpu.Power[2] p0_set(min=0)={1,1}/sqrt(2)
        "{active, reactive} power, (start val if signal inp)" annotation(Dialog(enable=scType_par));
      Modelica.Blocks.Interfaces.RealInput[2] p_set(min=0) if not scType_par
        "desired {active, reactive} power" annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

    protected
      Modelica.Blocks.Interfaces.RealInput[2] p_set_internal
        "Needed to connect to conditional connector";
      SI.Power[2] p0;

    equation
      connect(p_set, p_set_internal);

      if scType_par then
        p0 =  p0_set*S_base;
        p_set_internal = {0.0, 0.0};
      else
        p0 = p_set_internal*S_base;
      end if;
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end LoadBaseAC;

                 partial model IndLoadBaseAC "Inductive load base AC, 1-phase"
                   extends LoadBaseAC(v(start=vstart), i(start=istart));

                   parameter SI.MagneticFlux psi_start=0 "start value for magnetic flux"
		     annotation(Dialog(tab="Initialization"));
                   SI.MagneticFlux psi(start=psi_start, stateSelect=StateSelect.prefer)
        "magnetic flux";
    protected
                   final parameter Real V2_nom=V_nom*V_nom;
                   final parameter Real[2] Zstart=(p0_set/(p0_set*p0_set*S_base))*V2_nom;
                   final parameter SI.Voltage vstart=cos(system.alpha0)*V_nom;
                   final parameter SI.Current istart=cos(system.alpha0-atan(Zstart[2]/Zstart[1]))*V_nom/sqrt(Zstart*Zstart);
                   SI.Impedance[2] Z(start=Zstart);
                   function atan=Modelica.Math.atan;

                 initial equation
                   if steadyIni_t then
                     der(psi) = 0;
		   elseif not system.steadyIni then
		     psi = psi_start;
                   end if;

                 equation
                   psi = Z[2]*i/system.omega_nom;
                   if system.transientSim then
                     der(psi) + Z[1]*i = v;
                   else
                     Z[1]*i = v;
                   end if;
      annotation (
        Documentation(info=
                     "<html>
</html>
"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
                               points={{-40,-45},{-40,45},{80,0},{-40,-45}},
                               lineColor={0,0,255},
                               fillColor={0,0,255},
                               fillPattern=FillPattern.Solid)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
                 end IndLoadBaseAC;

                 partial model CapLoadBaseAC "Capacitive load base AC, 1-phase"
                   extends LoadBaseAC(v(start=vstart), i(start=istart));

                   parameter SI.ElectricCharge q_start=0 "start value for electric charge"
		     annotation(Dialog(tab="Initialization"));
                   SI.ElectricCharge q(start=q_start, stateSelect=StateSelect.prefer)
        "electric charge";
    protected
                   final parameter Real I2_nom=(S_nom/V_nom)^2;
                   final parameter SI.Admittance[2] Ystart=(p0_set/(p0_set*p0_set*S_base))*I2_nom;
                   final parameter SI.Voltage vstart=cos(system.alpha0)*V_nom;
                   final parameter SI.Current istart=cos(system.alpha0+atan(Ystart[2]/Ystart[1]))*V_nom*sqrt(Ystart*Ystart);
                   SI.Admittance[2] Y(start=Ystart);
                   function atan=Modelica.Math.atan;

                 initial equation
                   if steadyIni_t then
                     der(q) = 0;
		   elseif not system.steadyIni then
		     q = q_start;
                   end if;

                 equation
                   q = Y[2]*v/system.omega_nom;
                   if system.transientSim then
                     der(q) + Y[1]*v = i;
                   else
                     Y[1]*v = i;
                   end if;
                   annotation (
                     Documentation(info=
                     "<html>
</html>
"),                  Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
                               points={{-40,44},{-40,-44},{-20,-36},{-20,36},{-40,44}},
                               lineColor={0,0,255},
                               pattern=LinePattern.None,
                               fillColor={215,215,215},
                               fillPattern=FillPattern.Solid),Polygon(
                               points={{-50,48},{-50,-48},{-40,-44},{-40,44},{-50,48}},
              lineColor={0,0,255},
                               fillColor={0,0,255},
                               fillPattern=FillPattern.Solid),Polygon(
                               points={{-20,36},{-20,-36},{-10,-33},{-10,33},{-20,36}},
              lineColor={0,0,255},
                               fillColor={0,0,255},
                               fillPattern=FillPattern.Solid)}),
                     Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
                 end CapLoadBaseAC;

    partial model LoadBaseDC "Inductive load base DC"
      extends LoadBase;

      parameter Boolean scType_par = true
        "= true if p0 defined by parameter p0_set otherwise by input signal p_set"
        annotation(Evaluate=true, choices(__Dymola_checkBox=true));
      parameter SIpu.Power p0_set(min=0)=1 "power, (start val if signal inp)"    annotation(Dialog(enable=scType_par));
      Modelica.Blocks.Interfaces.RealInput p_set(min=0) if not scType_par
        "desired power"                                    annotation (Placement(
            transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));

    protected
      Modelica.Blocks.Interfaces.RealInput p_set_internal
        "Needed to connect to conditional connector";
      SI.Power p0;

    equation
      connect(p_set, p_set_internal);

      if scType_par then
        p0 =  p0_set*S_base;
        p_set_internal = 0.0;
      else
        p0 = p_set_internal*S_base;
      end if;
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
              points={{-40,-45},{-40,45},{80,0},{-40,-45}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid)}));
    end LoadBaseDC;

    partial model IndLoadBaseDC "Inductive load base DC"
      extends LoadBaseDC;

      parameter SI.Time t_RL=0.1 "R-L time constant";
    protected
      final parameter Real V2_nom(unit="V2")=V_nom*V_nom;
      final parameter Real Rstart=V2_nom/(p0_set*S_base);
      SI.Resistance R(start=Rstart);
      SI.Inductance L(start=t_RL*Rstart);

    initial equation
      if steadyIni_t then
        der(L*i) = 0;
      elseif not system.steadyIni then
	i = i_start;
      end if;

    equation
      der(L*i) + R*i = v;
      annotation (
        Documentation(
      info="<html>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Polygon(
              points={{-40,-45},{-40,45},{80,0},{-40,-45}},
              lineColor={0,0,255},
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid)}));
    end IndLoadBaseDC;

  end Partials;

annotation (preferredView="info",
    Documentation(info="<html>
<p>Different load models with an optional input:</p>
<pre>  p_set:     active or {active, reactive} power</pre>
<p>If p_set is <b>not</b> connected to a corresponding signal, parameter-values are relevant.</p>
</html>
"),
  Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Loads;
