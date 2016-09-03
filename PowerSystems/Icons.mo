within PowerSystems;
package Icons "Icons"
    extends Modelica.Icons.IconsPackage;

    partial block Block "Block icon"

      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,0,127},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}));
    end Block;

    partial block Block0 "Block icon 0"
      extends Block;
      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-80},{100,-120}},
              lineColor={0,0,0},
              textString="%name")}));
    end Block0;

    partial block Block1 "Block icon 1"
      extends Block;
      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,120},{100,80}},
              lineColor={0,0,0},
              textString="%name")}));
    end Block1;

    partial block BlockS "Block icon shadowed"

      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,0,127},
              fillColor={192,192,192},
              fillPattern=FillPattern.Solid)}));
    end BlockS;

    partial block BlockS0 "Block icon shadowed 0"
      extends BlockS;
      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-80},{100,-120}},
              lineColor={0,0,0},
              textString="%name")}));
    end BlockS0;

    partial block BlockS1 "Block icon shadowed 1"
      extends BlockS;
      annotation (
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,120},{100,80}},
              lineColor={0,0,0},
              textString="%name")}));
    end BlockS1;

  partial model Adaptor_abc "Adaptor icon abc"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,130,175},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{0,58},{60,18}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{0,18},{60,-22}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{0,-22},{60,-62}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{50,50},{70,30}},
              lineColor={0,0,255},
              textString="a"),
            Text(
              extent={{50,10},{70,-10}},
              lineColor={0,0,255},
              textString="b"),
            Text(
              extent={{50,-30},{70,-50}},
              lineColor={0,0,255},
              textString="c"),
            Text(
              extent={{-70,50},{-50,30}},
              lineColor={0,130,175},
              textString="a"),
            Text(
              extent={{-70,10},{-50,-10}},
              lineColor={0,130,175},
              textString="b"),
            Text(
              extent={{-70,-30},{-50,-50}},
              lineColor={0,130,175},
              textString="c"),
            Text(
              extent={{-60,62},{0,22}},
              lineColor={0,130,175},
              textString="~"),
            Line(points={{-46,30},{-14,30}}, color={0,130,175}),
            Text(
              extent={{-60,22},{0,-18}},
              lineColor={0,130,175},
              textString="~"),
            Line(points={{-46,-10},{-14,-10}}, color={0,130,175}),
            Line(points={{-46,-50},{-14,-50}}, color={0,130,175}),
            Text(
              extent={{-60,-18},{0,-58}},
              lineColor={0,130,175},
              textString="~"),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name")}),
        Documentation(info=""));

  end Adaptor_abc;

  partial model Adaptor_dq0 "Adaptor icon dq0"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,120,120},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{0,58},{60,18}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{0,18},{60,-22}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{0,-22},{60,-62}},
              lineColor={0,0,255},
              textString="~"),
            Text(
              extent={{50,50},{70,30}},
              lineColor={0,0,255},
              textString="a"),
            Text(
              extent={{50,10},{70,-10}},
              lineColor={0,0,255},
              textString="b"),
            Text(
              extent={{50,-30},{70,-50}},
              lineColor={0,0,255},
              textString="c"),
            Text(
              extent={{-70,50},{-50,30}},
              lineColor={0,120,120},
              textString="d"),
            Text(
              extent={{-70,10},{-50,-10}},
              lineColor={0,120,120},
              textString="q"),
            Text(
              extent={{-70,-30},{-50,-50}},
              lineColor={0,120,120},
              textString="o"),
            Text(
              extent={{-60,62},{0,22}},
              lineColor={0,120,120},
              textString="~"),
            Line(points={{-46,30},{-14,30}}, color={0,120,120}),
            Text(
              extent={{-60,22},{0,-18}},
              lineColor={0,120,120},
              textString="~"),
            Line(points={{-46,-10},{-14,-10}}, color={0,120,120}),
            Line(points={{-46,-40},{-14,-40}}, color={0,120,120}),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name")}),
        Documentation(info=""));

  end Adaptor_dq0;

  partial model Inverter "Inverter icon"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,0,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name")}),
        Documentation(info=""));

  end Inverter;

  partial model Inverter_abc "Inverter icon"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,130,175},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,-60},{80,60}}, color={0,130,175}),
            Line(points={{24,-40},{56,-40}}, color={0,130,175}),
            Line(points={{24,-20},{56,-20}}, color={0,130,175}),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name"),
            Text(
              extent={{10,20},{70,-10}},
              lineColor={0,130,175},
              lineThickness=0.5,
              textString="~"),
            Line(points={{24,0},{56,0}}, color={0,130,175}),
            Text(
              extent={{10,0},{70,-30}},
              lineColor={0,130,175},
              lineThickness=0.5,
              textString="~"),
            Text(
              extent={{10,-20},{70,-50}},
              lineColor={0,130,175},
              textString="~"),
            Text(extent={{-80,40},{0,0}}, textString="=")}),
        Documentation(info=""));

  end Inverter_abc;

  partial model Inverter_dq0 "Inverter icon"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,120,120},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,-60},{80,60}}, color={0,120,120}),
            Text(
              extent={{10,20},{70,-10}},
              lineColor={0,120,120},
              lineThickness=0.5,
              textString="~"),
            Text(
              extent={{10,0},{70,-30}},
              lineColor={0,120,120},
              lineThickness=0.5,
              textString="~"),
            Line(points={{24,0},{56,0}}, color={0,120,120}),
            Line(points={{24,-20},{56,-20}}, color={0,120,120}),
            Line(points={{24,-40},{56,-40}}, color={0,120,120}),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString="%name"),
            Text(extent={{-80,40},{0,0}}, textString="=")}),
        Documentation(info=""));

  end Inverter_dq0;

  model LeftBar "Display a bar on the left side of the icon"
    input PowerSystems.Types.Color colorL={0,0,0};
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
    input PowerSystems.Types.Color colorR={0,0,0};
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
    input PowerSystems.Types.Color color={0,0,0};
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
    input PowerSystems.Types.Color color1={255,0,0};
    input PowerSystems.Types.Color color2={0,0,255};
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
</html>
"));
end Icons;
