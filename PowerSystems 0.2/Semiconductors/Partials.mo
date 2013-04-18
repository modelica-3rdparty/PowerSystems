within PowerSystems.Semiconductors;
package Partials "Partial models"
  extends Modelica.Icons.BasesPackage;

  partial model ComponentBase "Semiconductor component base"

    SI.Voltage v "voltage";
    SI.Current i "current";
    Interfaces.Electric_p term_p "positive terminal"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Interfaces.Electric_n term_n "negative terminal"
  annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));
    Interfaces.Thermal_n heat "source dissipated heat power"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
  protected
    SI.Temperature T "component temperature";

  equation
    term_p.i + term_n.i = 0;
    v = term_p.v - term_n.v;
    i = term_p.i;

    T = heat.T;
    heat.Q_flow = -v*i;
    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
      Documentation(
            info="<html>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
         "%name")}));
  end ComponentBase;

  partial model AC1ph_DC_base "AC(scalar)-DC base"
    extends PowerSystems.Basic.Icons.Inverter;

    Interfaces.Electric_n AC "AC scalar connection"
      annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=
             0)));
    AC1ph_DC.Ports.TwoPin_p DC "DC connection"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}},
            rotation=0)));
    Interfaces.Thermal_n heat      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
      Documentation(
            info="<html>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-120,50},{-80,10}},
            lineColor={0,0,255},
            textString=
                 "="), Text(
            extent={{80,50},{120,10}},
            lineColor={0,0,255},
            textString=
                 "~")}));

  end AC1ph_DC_base;

  annotation (       Window(
x=0.05,
y=0.44,
width=0.31,
height=0.25,
library=1,
autolayout=1));
end Partials;
