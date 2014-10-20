within PowerSystems.Examples.Spot.Data;
package Breakers "Breaker example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record BreakerArc "Breaker, 3-phase, example"
    extends Modelica.Icons.Record;

    parameter SI.Distance D= 50e-3 "contact distance open";
    parameter SI.Time t_opening= 30e-3 "opening duration";
    parameter SI.ElectricFieldStrength Earc= 50e3 "electric field arc";
    parameter Real R0= 1 "small signal resistance arc";
    annotation (defaultComponentName="breakerExpl",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end BreakerArc;
  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Breakers;
