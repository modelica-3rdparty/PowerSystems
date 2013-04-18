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
      Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
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
  annotation (preferedView="info",
Documentation(info="<html>
</html>"));
end Breakers;
