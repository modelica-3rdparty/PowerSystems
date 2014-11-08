within PowerSystems.Examples.Spot.Data;
package Breakers "Breaker example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record BreakerArc "Breaker, 3-phase, example"
    extends Modelica.Icons.Record;

    SI.Distance D= 50e-3 "contact distance open" annotation(Dialog);
    SI.Time t_opening= 30e-3 "opening duration" annotation(Dialog);
    SI.ElectricFieldStrength Earc= 50e3 "electric field arc" annotation(Dialog);
    Real R0= 1 "small signal resistance arc" annotation(Dialog);

    annotation (defaultComponentName="breakerExpl",
      defaultComponentPrefixes="parameter",
      Documentation(
              info="<html>
</html>
"));
  end BreakerArc;

  annotation (preferredView="info",
    Documentation(info="<html>
</html>"));
end Breakers;
