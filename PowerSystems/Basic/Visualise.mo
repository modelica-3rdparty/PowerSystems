within PowerSystems.Basic;
package Visualise "Elementary visualisers"
    extends Modelica.Icons.Package;

    model Bar "Bar"

      parameter PowerSystems.Basic.Types.Color color=
                                          {0,0,0};
      input Real x annotation(Hide=false);
      annotation (Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{0,0},{1,1}},
            grid={0.01,0.01}), graphics={Rectangle(
              extent={{0,0},{1,1}},
              lineColor={95,95,95},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid), Rectangle(
              extent=    [0,0; 1, DynamicSelect(
                                               0.5, x)],
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid)}));
    end Bar;

    model Needle "Centered needle"

      parameter PowerSystems.Basic.Types.Color color=
                                          {0,0,0};
      input Real x;
      input Real y;
      final Real[2,2] needle=[0,0; x,y] annotation(Hide=false);
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-1,-1},{1,1}},
            grid={0.02,0.02}), graphics={Line(
              points=DynamicSelect({{0,0},{1,0}},needle),
              color={128,128,128},
              thickness=0.5)}));
    end Needle;

    model DoubleNeedle "Centered double needle"

      parameter PowerSystems.Basic.Types.Color color1=
                                           {255,0,0};
      parameter PowerSystems.Basic.Types.Color color2=
                                           {0,0,255};
      input Real x1;
      input Real y1;
      input Real x2;
      input Real y2;
  protected
      final Real[2,2] needle1=[{0,x1},{0,y1}] annotation(Hide=false);
      final Real[2,2] needle2=[{0,x2},{0,y2}] annotation(Hide=false);
      annotation (
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-1,-1},{1,1}},
            grid={0.02,0.02}), graphics={Line(
              points=DynamicSelect({{0,0},{0.5,0}},needle1),
              color={128,128,128},
              thickness=0.5), Line(
              points=DynamicSelect({{0,0},{0.5,0}},needle2),
              color={128,128,128},
              thickness=0.5)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-1,-1},{1,1}},
            grid={0.02,0.02}), graphics));
    end DoubleNeedle;
    annotation (preferedView="info",
  Documentation(info="<html>
<p><a href=\"PowerSystems.UsersGuide.Introduction.Visualisation\">up users guide</a></p>
</html>
"));
end Visualise;

