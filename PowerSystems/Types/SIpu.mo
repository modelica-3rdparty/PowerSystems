within PowerSystems.Types;
package SIpu "Additional types for power systems"
  extends Modelica.Icons.Package;

  type AngularVelocity = Real(final quantity="AngularVelocity", unit="rad.s/(rad.s)");
  type Voltage = Real (final quantity="ElectricPotential", unit="V/V");
  type Current = Real (final quantity="ElectricCurrent", unit="A/A");
  type Power = Real (final quantity="Power", unit="W/W");
  type ActivePower = Real (final quantity="Power", unit="W/W");
  type ApparentPower = Real (final quantity="ApparentPower", unit="VA/VA");
  type ReactivePower = Real (final quantity="ReactivePower", unit="var/var");
  type Resistance = Real (
    final quantity="Resistance",
    unit="Ohm/(V.V/VA)",
    final min=0);
  type Reactance = Real (final quantity="Reactance", unit="Ohm/Ohm");
  type Impedance = Real (final quantity="Impedance", unit="Ohm/Ohm");
  type Inductance = Real (final quantity="Inductance", unit="H/H");
  type Conductance = Real (
    final quantity="Conductance",
    unit="S/S",
    final min=0);
  type Susceptance = Real (
    final quantity="Susceptance",
    unit="S/S");
  type Admittance = Real (
    final quantity="Admittance",
    unit="S/S",
    min=0);
  type Resistance_km = Real (
    final quantity="Resistance",
    unit="Ohm/(Ohm.km)",
    min=0);
  type Reactance_km = Real (final quantity="Reactance_per_km",
    unit="Ohm/(Ohm.km)",
    min=0);
  type Conductance_km = Real (
    final quantity="Conductance",
    unit="S/(S.km)",
    min=0);
  type Susceptance_km = Real (
    final quantity="Susceptance",
    unit="S/(S.km)",
    min=0);
  type MagneticFlux = Real (final quantity="MagneticFlux", unit="Wb/Wb");

  type Energy = Real (final quantity="Energy", unit="J/J");
  type Torque = Real (final quantity="Torque", unit="N.m/(N.m)");

  annotation (
    Documentation(info="<html>
</html>
"));
end SIpu;
