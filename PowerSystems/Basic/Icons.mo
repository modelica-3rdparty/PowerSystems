within PowerSystems.Basic;
package Icons "Icons"
    extends Modelica.Icons.Package;

    partial block Block "Block icon"

      annotation (
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
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
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-80},{100,-120}},
              lineColor={0,0,0},
              textString=
               "%name")}));
    end Block0;

    partial block Block1 "Block icon 1"
      extends Block;
      annotation (
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
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
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
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
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,-80},{100,-120}},
              lineColor={0,0,0},
              textString=
               "%name")}));
    end BlockS0;

    partial block BlockS1 "Block icon shadowed 1"
      extends BlockS;
      annotation (
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Documentation(info="
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,120},{100,80}},
              lineColor={0,0,0},
              textString=
               "%name")}));
    end BlockS1;

  partial model Adaptor_abc "Adaptor icon abc"

    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
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
              textString=             "~"),
            Text(
              extent={{0,18},{60,-22}},
              lineColor={0,0,255},
              textString=              "~"),
            Text(
              extent={{0,-22},{60,-62}},
              lineColor={0,0,255},
              textString=               "~"),
            Text(
              extent={{50,50},{70,30}},
              lineColor={0,0,255},
              textString=
                   "a"),
            Text(
              extent={{50,10},{70,-10}},
              lineColor={0,0,255},
              textString=
                   "b"),
            Text(
              extent={{50,-30},{70,-50}},
              lineColor={0,0,255},
              textString=
                   "c"),
            Text(
              extent={{-70,50},{-50,30}},
              lineColor={0,130,175},
              textString=
                 "a"),
            Text(
              extent={{-70,10},{-50,-10}},
              lineColor={0,130,175},
              textString=
                 "b"),
            Text(
              extent={{-70,-30},{-50,-50}},
              lineColor={0,130,175},
              textString=
                 "c"),
            Text(
              extent={{-60,62},{0,22}},
              lineColor={0,130,175},
              textString=             "~"),
            Line(points={{-46,30},{-14,30}}, color={0,130,175}),
            Text(
              extent={{-60,22},{0,-18}},
              lineColor={0,130,175},
              textString=              "~"),
            Line(points={{-46,-10},{-14,-10}}, color={0,130,175}),
            Line(points={{-46,-50},{-14,-50}}, color={0,130,175}),
            Text(
              extent={{-60,-18},{0,-58}},
              lineColor={0,130,175},
              textString=              "~"),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
             "%name")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Documentation(info=""));

  end Adaptor_abc;

  partial model Adaptor_dqo "Adaptor icon dqo"

    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
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
              textString =            "~"),
            Text(
              extent={{0,18},{60,-22}},
              lineColor={0,0,255},
              textString =             "~"),
            Text(
              extent={{0,-22},{60,-62}},
              lineColor={0,0,255},
              textString =              "~"),
            Text(
              extent={{50,50},{70,30}},
              lineColor={0,0,255},
              textString=
                   "a"),
            Text(
              extent={{50,10},{70,-10}},
              lineColor={0,0,255},
              textString=
                   "b"),
            Text(
              extent={{50,-30},{70,-50}},
              lineColor={0,0,255},
              textString=
                   "c"),
            Text(
              extent={{-70,50},{-50,30}},
              lineColor={0,120,120},
              textString=
                 "d"),
            Text(
              extent={{-70,10},{-50,-10}},
              lineColor={0,120,120},
              textString=
                 "q"),
            Text(
              extent={{-70,-30},{-50,-50}},
              lineColor={0,120,120},
              textString=
                 "o"),
            Text(
              extent={{-60,62},{0,22}},
              lineColor={0,120,120},
              textString =            "~"),
            Line(points={{-46,30},{-14,30}}, color={0,120,120}),
            Text(
              extent={{-60,22},{0,-18}},
              lineColor={0,120,120},
              textString =             "~"),
            Line(points={{-46,-10},{-14,-10}}, color={0,120,120}),
            Line(points={{-46,-40},{-14,-40}}, color={0,120,120}),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
             "%name")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Documentation(info=""));

  end Adaptor_dqo;

  partial model Inverter "Inverter icon"

    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
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
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Documentation(info=""));

  end Inverter;

  partial model Inverter_abc "Inverter icon"

    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
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
              textString=
             "%name"),
            Text(
              extent={{10,20},{70,-10}},
              lineColor={0,130,175},
              lineThickness=0.5,
              textString=              "~"),
            Line(points={{24,0},{56,0}}, color={0,130,175}),
            Text(
              extent={{10,0},{70,-30}},
              lineColor={0,130,175},
              lineThickness=0.5,
              textString=             "~"),
            Text(
              extent={{10,-20},{70,-50}},
              lineColor={0,130,175},
              textString=             "~"),
            Text(extent={{-80,40},{0,0}}, textString=
                                    "=")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Documentation(info=""));

  end Inverter_abc;

  partial model Inverter_dqo "Inverter icon"

    annotation (
      Window(
        x=
  0.45, y=
  0.01, width=
      0.44,
        height=
       0.65),
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
              textString =             "~"),
            Text(
              extent={{10,0},{70,-30}},
              lineColor={0,120,120},
              lineThickness=0.5,
              textString =            "~"),
            Line(points={{24,0},{56,0}}, color={0,120,120}),
            Line(points={{24,-20},{56,-20}}, color={0,120,120}),
            Line(points={{24,-40},{56,-40}}, color={0,120,120}),
            Text(
              extent={{-100,-90},{100,-130}},
              lineColor={0,0,0},
              textString=
             "%name"),
            Text(extent={{-80,40},{0,0}}, textString=
                                    "=")}),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Documentation(info=""));

  end Inverter_dqo;

  partial record Record "Record icon"

    annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                -100},{100,100}}), graphics={
            Rectangle(
              extent={{-80,80},{80,-40}},
              lineColor={95,95,95},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,40},{80,40}}, color={95,95,95}),
            Line(points={{0,80},{0,-40}}, color={95,95,95}),
            Text(
              extent={{-100,-80},{100,-120}},
              lineColor={0,0,0},
              textString="%name"),
            Line(points={{-80,0},{80,0}}, color={95,95,95})}),
          Documentation(info=""));
  end Record;

  partial function Function "Function icon"

    annotation (
      Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{-100,60},{100,-60}},
              lineColor={255,85,85},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,30},{100,-50}},
              lineColor={255,85,85},
              textString="f"),
            Text(
              extent={{-100,120},{100,80}},
              lineColor={0,0,0},
              textString="%name")}),
      Documentation(
              info="
"),   Window(
  x=0.45,
  y=0.01,
  width=0.44,
  height=0.65),
      Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
  end Function;

  partial class Enumeration "Enumeration icon"

    annotation (Icon(graphics={
            Text(
              extent={{-100,120},{100,80}},
              lineColor={0,0,0},
              textString=                        "%name"),
            Ellipse(
              extent={{-100,60},{100,-60}},
              lineColor={85,85,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,40},{100,-40}},
              lineColor={85,85,255},
              fillColor={223,159,191},
              fillPattern=FillPattern.Solid,
              textString=
                   "e")}),Documentation(info=""));
  end Enumeration;

    partial class Library "Package icon 'Library'"

      annotation (             Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,-100},{80,50}},
              lineColor={0,0,255},
              fillColor={235,235,235},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,50},{-80,70},{100,70},{80,50},{-100,50}},
              lineColor={0,0,255},
              fillColor={235,235,235},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{100,70},{100,-80},{80,-100},{80,50},{100,70}},
              lineColor={0,0,255},
              fillColor={235,235,235},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-120,125},{120,70}},
              lineColor={255,0,0},
              textString="%name"),
            Text(
              extent={{-80,40},{70,-80}},
              lineColor={95,95,95},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid,
              textString="Library")}),
        Documentation(info=""),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end Library;

    partial class SpecialLibrary "Package icon 'Special Library'"

      annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,-100},{80,50}},
              lineColor={95,95,95},
              fillColor={212,231,211},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,50},{-80,70},{100,70},{80,50},{-100,50}},
              lineColor={95,95,95},
              fillColor={212,231,211},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{100,70},{100,-80},{80,-100},{80,50},{100,70}},
              lineColor={95,95,95},
              fillColor={212,231,211},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-80,40},{70,-80}},
              lineColor={95,95,95},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid,
              textString="Library"),
            Text(
              extent={{-120,125},{120,70}},
              lineColor={255,0,0},
              textString="%name")}),
    Window(
      x=0.05,
      y=0.44,
      width=0.29,
      height=0.24,
      library=1,
      autolayout=1),
    Documentation(info=""),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end SpecialLibrary;

    partial class Base "Package icon 'Base'"

      annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,-100},{80,50}},
              lineColor={95,95,95},
              fillColor={253,255,202},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,50},{-80,70},{100,70},{80,50},{-100,50}},
              lineColor={95,95,95},
              fillColor={253,255,202},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{100,70},{100,-80},{80,-100},{80,50},{100,70}},
              lineColor={95,95,95},
              fillColor={253,255,202},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-80,40},{70,-80}},
              lineColor={95,95,95},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid,
              textString="Base")}),
    Window(
      x=0.05,
      y=0.44,
      width=0.29,
      height=0.24,
      library=1,
      autolayout=1),
    Documentation(info=""),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end Base;

    partial class Partials "Package icon 'Partials'"

      annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,-100},{80,50}},
              lineColor={95,95,95},
              fillColor={236,236,236},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,50},{-80,70},{100,70},{80,50},{-100,50}},
              lineColor={95,95,95},
              fillColor={236,236,236},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{100,70},{100,-80},{80,-100},{80,50},{100,70}},
              lineColor={95,95,95},
              fillColor={236,236,236},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-80,40},{70,-80}},
              lineColor={95,95,95},
              fillColor={255,255,170},
              fillPattern=FillPattern.Solid,
              textString="Partials")}),
    Window(
      x=0.05,
      y=0.44,
      width=0.29,
      height=0.24,
      library=1,
      autolayout=1),
    Documentation(info=""),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end Partials;

    partial class Examples "Package icon 'Examples'"

      annotation (
    Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-100,-100},{80,50}},
              lineColor={95,95,95},
              fillColor={255,170,170},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-100,50},{-80,70},{100,70},{80,50},{-100,50}},
              lineColor={95,95,95},
              fillColor={255,170,170},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{100,70},{100,-80},{80,-100},{80,50},{100,70}},
              lineColor={95,95,95},
              fillColor={255,170,170},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-85,41},{65,-79}},
              lineColor={95,95,95},
              textString="Examples")}),
    Window(
      x=0.05,
      y=0.44,
      width=0.29,
      height=0.24,
      library=1,
      autolayout=1),
    Documentation(info=""),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics));
    end Examples;
  annotation (preferedView="info",
    Window(
      x=0.05,
      y=0.41,
      width=0.4,
      height=0.32,
      library=1,
      autolayout=1),
    Documentation(info="<html>
</html>
"));
end Icons;

