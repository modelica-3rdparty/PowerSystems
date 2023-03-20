within PowerSystems.Types;
package SI "SI types with custom attributes, like display units"
  extends Modelica.Icons.Package;
  import MSI = Modelica.Units.SI;

  type Time = MSI.Time;
  type Frequency = MSI.Frequency;

  type Length = MSI.Length(displayUnit = "km");
  type Distance = MSI.Distance;
  type Radius = MSI.Radius;
  type Area = MSI.Area;
  type Velocity = MSI.Velocity;
  type Angle = MSI.Angle;
  type AngularVelocity = MSI.AngularVelocity(displayUnit = "rpm");
  type AngularAcceleration = MSI.AngularAcceleration;
  type AngularFrequency = MSI.AngularFrequency;

  type Force = MSI.Force;
  type Mass = MSI.Mass;
  type Density = MSI.Density;
  type Inertia = MSI.Inertia;
  type Torque = MSI.Torque;
  type Stiffness = Real (
    final quantity = "Stiffness",
    final unit = "N",
    final min=0);
  type TorsionStiffness = Real (
    final quantity="TorsionStiffness",
    final unit="N.m/rad",
    final min=0);

  type Voltage = MSI.Voltage(displayUnit = "kV");
  type Current = MSI.Current;
  type Resistance = MSI.Resistance;
  type Conductance = MSI.Conductance;
  type Inductance = MSI.Inductance;
  type Capacitance = MSI.Capacitance;
  type Admittance = MSI.Admittance;
  type Reactance = MSI.Reactance;
  type Impedance = MSI.Impedance;
  type Power = MSI.Power(displayUnit = "MW");
  type ActivePower = MSI.ActivePower(displayUnit = "MW");
  type ApparentPower = MSI.ApparentPower(displayUnit = "MVA");
  type ReactivePower = MSI.ReactivePower(displayUnit = "Mvar");
  type ElectricFieldStrength = MSI.ElectricFieldStrength;
  type MagneticFlux = MSI.MagneticFlux;
  type ElectricCharge = MSI.ElectricCharge;

  type Temperature = MSI.Temperature;
  type Heat = MSI.Heat;
  type HeatFlowRate = MSI.HeatFlowRate;
  type HeatCapacity = MSI.HeatCapacity;
  type ThermalConductance = MSI.ThermalConductance;

  type Percent = Real (
    final quantity="Percent",
    final unit="1",
    final displayUnit="1/100");

  annotation (
    Documentation(info="<html>
</html>
"));
end SI;
