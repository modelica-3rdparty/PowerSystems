within PowerSystems.Basic;
package Visualise "Elementary visualisers"
    extends Modelica.Icons.Package;

    model LeftBar "Display a bar on the left side of the icon"

      input PowerSystems.Basic.Types.Color colorL = {0,0,0};
      input Real xL;
      annotation (Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-104,-100},{-94,100}},
              lineColor={95,95,95},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Rectangle(
                extent = [-104,-100; -94, DynamicSelect(0, xL*200-100)],
                lineColor=colorL,
                fillColor=colorL,
                fillPattern=FillPattern.Solid)}));
    end LeftBar;

    model RightBar "Display a bar on the right side of the icon"

      input PowerSystems.Basic.Types.Color colorR = {0,0,0};
      input Real xR;
      annotation (Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{94,-100},{104,100}},
              lineColor={95,95,95},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Rectangle(
                extent = [94,-100; 104, DynamicSelect(0, xR*200-100)],
                lineColor=colorR,
                fillColor=colorR,
                fillPattern=FillPattern.Solid)}));
    end RightBar;

    model Needle "Centered needle"

      input PowerSystems.Basic.Types.Color color = {0,0,0};
      input Real x;
      input Real y;
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(
              points=DynamicSelect({{0,0},{100,0}},{{0,0},{x,y}}*100),
              color=color,
              thickness=0.5)}));
    end Needle;

    model DoubleNeedle "Centered double needle"

      input PowerSystems.Basic.Types.Color color1 = {255,0,0};
      input PowerSystems.Basic.Types.Color color2 = {0,0,255};
      input Real x1;
      input Real y1;
      input Real x2;
      input Real y2;
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Line(
              points=DynamicSelect({{0,0},{50,0}},{{0,0},{x1,y1}}*100),
              color=color1,
              thickness=0.5), Line(
              points=DynamicSelect({{0,0},{50,0}},{{0,0},{x2,y2}}*100),
              color=color2,
              thickness=0.5)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end DoubleNeedle;
    annotation (preferredView="info",
  Documentation(info="<html>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Visualisation\">up users guide</a></p>
</html>
"));
end Visualise;

