within PowerSystems.Basic;
package Types
    extends Modelica.Icons.Package;

  package SIpu "Additional types for power systems"
    extends Modelica.Icons.Package;

    type AngularVelocity = Real(final quantity="AngularVelocity", unit="rad.s/(rad.s)");
    type Voltage = Real (final quantity="Voltage", unit="V/V");
    type Current = Real (final quantity="Current", unit="A/A");
    type ApparentPower = Real (final quantity="ApparentPower", unit="VA/VA");
    type Resistance = Real (
        final quantity="Resistance",
        unit="Ohm/(V.V/VA)",
        final min=0);
    type Reactance = Real (final quantity="Reactance", unit="Ohm/(V.V/VA)");
    type Impedance = Real (final quantity="Impedance", unit="Ohm/(V.V/VA)");
    type Inductance = Real (final quantity="Inductance", unit="H/H");
    type Conductance = Real (
        final quantity="Conductance",
        unit="S/(VA/(V.V))",
        final min=0);
    type Susceptance = Real (
        final quantity="Susceptance",
        unit="S/(VA/(V.V))",
        min=0);
    type Admittance = Real (
        final quantity="Admittance",
        unit="S/(VA/(V.V))",
        min=0);
    type Resistance_km = Real (
        final quantity="Resistance_per_km",
        unit="Ohm/(km.V.V/VA)",
        min=0);
    type Reactance_km = Real (final quantity="Reactance_per_km",
        unit="Ohm/(km.V.V/VA)",
        min=0);
    type Conductance_km = Real (
        final quantity="Conductance_per_km",
        unit="S/(km.VA/(V.V))",
        min=0);
    type Susceptance_km = Real (
        final quantity="Susceptance_per_km",
        unit="S/(km.VA/(V.V))",
        min=0);
    type MagneticFlux = Real (final quantity="MagneticFlux", unit="Wb/Wb");

    type Energy = Real (final quantity="Energy", unit="J/J");
    type Power = Real (final quantity="Power", unit="W/W");
    type Torque = Real (final quantity="Torque", unit="N.m/(N.m)");

   annotation (
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65,
      library=1,
      autolayout=1),
    Invisible=true,
    Documentation(info="<html>
</html>
"));
  end SIpu;

  package SInotused "Additional types for power systems"
    extends Modelica.Icons.Package;

    type AngularVelocity = Real(final quantity="AngularVelocity", unit="rad/s");
    type Voltage = Real (final quantity="Voltage", unit="V");
    type Current = Real (final quantity="Current", unit="A");
    type ApparentPower = Real (final quantity="ApparentPower", unit="VA");
    type Resistance = Real (
        final quantity="Resistance",
        unit="Ohm",
        final min=0);
    type Reactance = Real (final quantity="Reactance", unit="Ohm");
    type Impedance = Real (final quantity="Impedance", unit="Ohm");
    type Inductance = Real (final quantity="Inductance", unit="H");
    type Conductance = Real (
        final quantity="Conductance",
        unit="S",
        final min=0);
    type Susceptance = Real (
        final quantity="Susceptance",
        unit="S",
        min=0);
    type Admittance = Real (
        final quantity="Admittance",
        unit="S",
        min=0);
    type Resistance_km = Real (
        final quantity="Resistance_per_km",
        unit="Ohm/km",
        min=0);
    type Reactance_km = Real (final quantity="Reactance_per_km",
        unit="Ohm/km",
        min=0);
    type Conductance_km = Real (
        final quantity="Conductance_per_km",
        unit="S/km",
        min=0);
    type Susceptance_km = Real (
        final quantity="Susceptance_per_km",
        unit="S/km",
        min=0);
    type MagneticFlux = Real (final quantity="MagneticFlux", unit="Wb");

    type Energy = Real (final quantity="Energy", unit="J");
    type Power = Real (final quantity="Power", unit="W");
    type Torque = Real (final quantity="Torque", unit="N.m");

   annotation (
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65,
      library=1,
      autolayout=1),
    Invisible=true,
    Documentation(info="<html>
</html>
"));
  end SInotused;

  type Color = Integer[3] (min=0, max=255) "RGB color" annotation (choices(
        choice={255,0,0} "{255, 000, 000 }  red",
        choice={255,255,0} "{255, 255, 000}  yellow",
        choice={0,255,0} "{000, 255, 000}  green",
        choice={0,255,255} "{000, 255, 255}  cyan",
        choice={0,0,255} "{000, 000, 255}  blue",
        choice={255,0,255} "{255, 000, 255}  magenta",
        choice={0,0,0} "{000, 000, 000}  black",
        choice={95,95,95} "{095, 095, 095}  dark grey",
        choice={175,175,175} "{175, 175, 175}  grey",
        choice={255,255,255} "{255, 255, 255}  white"));

/*
  type Units = enumeration(
      SI "SI",
      pu "pu") "unit choice SI or pu"
      annotation(Documentation(info="<html>
<p><pre>
  SI:  SI units for input parameters
  pu:  pu units for input parameters (per unit)
</pre></p>
<p>If <tt>pu</tt> units are chosen, the nominal values (typically <tt>S_nom</tt> and <tt>V_nom</tt>) are used, to determine the units (typically impedance units).</p>
</html>"));
*/
/*
  type Units = String "unit choice" annotation(choices(
    choice=PowerSystems.Basic.Types.SI "SI",
    choice=PowerSystems.Basic.Types.Units.pu "pu"), Documentation(info="<html>
<p><pre>
  SI:  parameters in SI
  pu:  parameters in pu
</pre></p>
</html>"));
    constant PowerSystems.Basic.Types.Units SI=
                      "SI" "SI units";
    constant PowerSystems.Basic.Types.Units pu=
                      "pu" "pu units";
*/

  type FreqType = enumeration(
      par "parameter",
      sig "signal",
      sys "system") "Frequency type"
      annotation(Documentation(info="<html>
<p><pre>
  par:  source has parameter frequency
  sig:  source has signal frequency
  sys:  source has system frequency
</pre></p>
</html>"));
/*
  type FreqType = String "frequency type" annotation(choices(
     choice=PowerSystems.Basic.Types.sys "system",
     choice=PowerSystems.Basic.Types.par "parameter",
     choice=PowerSystems.Basic.Types.Units.sig "signal (omega)"), Documentation(info=
                   "<html>
<p><pre>
  sys:  source has system frequency
  par:  system or source has paramter frequency
  sig:  system or source has signal frequency
  ave:  system has averaged frequency (over involved generators)
</pre></p>
</html>"));


    constant Types.FreqType par=
                          "par" "parameter frequency";
    constant Types.FreqType sig=
                          "sig" "signal frequency";
    constant Types.FreqType ave=
                          "ave" "average frequency";
    constant Types.FreqType sys=
                          "sys" "system frequency";
*/

/*
  type SourceType = enumeration(
      par "parameter",
      sig "signal") "Source type"
      annotation(Documentation(info="<html>
<p><pre>
  par:  parameter
  sig:  signal
</pre></p>
</html>"));
*/
/*
  type SourceType = String "source type" annotation (choices(
       choice=PowerSystems.Basic.Types.par "parameter",
       choice=PowerSystems.Basic.Types.Units.sig "signal"), Documentation(info=
                     "<html>
<p><pre>
  par:  parameter
  sig:  signal
</pre></p>
</html>"));
*/

  type IniType = enumeration(
      v_alpha "v, alpha: voltage and phase angle, 'slack' (steady ini)",
      v_p "v, p: voltage and active power (steady ini)",
      v_q "v, q: voltage and reactive power (steady ini)",
      p_q "p, q: active and reactive power (steady ini)",
      phi_w_el "phi_el, w_el: machine angle and angular velocity electric",
      none "none: no initial condition") "Initialisation type"
      annotation(Documentation(info="<html>
<p><pre>
  v_alpha:  terminal voltage and phase angle ('slack')
  v_p:      terminal voltage and active power
  v_q:      terminal voltage and reactive power
  p_q:      terminal active and reactive power
  phi_w_el: machine angle and angular velocity electric
  none:     no initial condition
</pre></p>
</html>"));
/*
    type IniType = String "initialisation type"
      annotation(choices(
       choice=PowerSystems.Basic.Types.v_alpha
        "voltage and phase angle ('slack')",
       choice=PowerSystems.Basic.Types.v_p "voltage and active power",
       choice=PowerSystems.Basic.Types.v_q "voltage and reactive power",
       choice=PowerSystems.Basic.Types.p_q "active and reactive power",
       choice=PowerSystems.Basic.Types.none "no initial condition"), Documentation(info=
                     "<html>
<p><pre>
  v_alpha:  terminal voltage and phase angle ('slack')
  v_p:      terminal voltage and active power
  v_q:      terminal voltage and reactive power
  p_q:      terminal active and reactive power
  none:     no initial condition
</pre></p>
</html>"));

      constant PowerSystems.Basic.Types.IniType v_alpha=
                               "v_alpha" "voltage and phase";
      constant PowerSystems.Basic.Types.IniType v_p=
                           "v_p" "voltage and active power";
      constant PowerSystems.Basic.Types.IniType v_q=
                           "v_q" "voltage and reactive power";
      constant PowerSystems.Basic.Types.IniType p_q=
                           "p_q" "active and reactive power";
      constant PowerSystems.Basic.Types.IniType none=
                            "none" "no initial condition";
*/

/*
  type Mode = enumeration(
      tr "transient",
      st "steady") "Simulation and initialisation mode"
      annotation (Documentation(info="<html>
<p><pre>
  tr:  transient
  st:  steady state
</pre></p>
</html>"));
*/
/*
    type Mode = String "simulation and initialisation mode" annotation (choices(
       choice=PowerSystems.Basic.Types.tr "transient",
       choice=PowerSystems.Basic.Types.st "steady"), Documentation(info=
                     "<html>
<p><pre>
  tr:  transient
  st:  steady state
</pre></p>
</html>"));

      constant PowerSystems.Basic.Types.Mode tr=
                        "tr" "transient mode";
      constant PowerSystems.Basic.Types.Mode st=
                        "st" "steady state mode";
*/
/*
  type RefFrame = enumeration(
      syn "synchronous",
      inert "inertial") "Reference frame"
      annotation (Documentation(info="<html>
<p><pre>
  syn:   synchronous (rotating)
  inert: inertial (not rotating)
</pre></p>
</html>"));
*/
/*
    type RefFrame = String "reference frame" annotation (choices(
     choice=PowerSystems.Basic.Types.syn "synchronous",
     choice=PowerSystems.Basic.Types.inert "inertial"), Documentation(info=
                   "<html>
<p><pre>
  syn:   synchronous (rotating)
  inert: inertial (not rotating)
</pre></p>
</html>"));

      constant PowerSystems.Basic.Types.RefFrame syn=
                             "syn" "synchronous";
      constant PowerSystems.Basic.Types.RefFrame inert=
                               "inert" "inertial";
*/

    type ReferenceAngle "reference angle"
      extends Modelica.SIunits.Angle;

      function equalityConstraint
        input ReferenceAngle[2] theta_p;
        input ReferenceAngle[2] theta_n;
        output Real[0] residue;
      algorithm
        assert(abs(theta_p[1] - theta_n[1]) < Modelica.Constants.eps, "theta[1] term_p and term_n not equal!");
        assert(abs(theta_p[2] - theta_n[2]) < Modelica.Constants.eps, "theta[2] term_p and term_n not equal!");
      end equalityConstraint;

    annotation (Documentation(info="<html>
<p>Type ReferenceAngle specifies the variable-type that contains relative frequency and angular orientation of a rotating electrical reference system.
<pre>
  theta_p[1]     angle relative to reference-system
  theta_p[2]     reference angle, defining reference-system

  der(theta[1])  relative frequency in reference-system with orientation theta[2]
  der(theta[1] + theta[2])  absolute frequency
</pre></p>
</html>"));
    end ReferenceAngle;

    type AngularVelocity = SI.AngularVelocity(displayUnit = "rpm");
    type Charge_Ah = Real (final quantity="ElectricCharge", final unit="A.h");
    type Length = SI.Length(displayUnit="km");
    type Percent = Real(final quantity="Percent",final unit="%");
    type Stiffness = Real (final quantity="Stiffness", final unit="N", final min=0);
    type TorsionStiffness = Real (final quantity="TorsionStiffness", final unit="N.m/rad", final min=0);

end Types;

