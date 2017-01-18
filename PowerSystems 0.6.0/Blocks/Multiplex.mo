within PowerSystems.Blocks;
package Multiplex "Multiplexer and AD converter"
  extends Modelica.Icons.VariantsPackage;

  block R2mux "Real multiplexer 2-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.RealInput u2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
    Modelica.Blocks.Interfaces.RealOutput y[sum(n)]
      "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    parameter Integer n[2]={1,1} "dim of input signals" annotation(Evaluate=true);

  equation
    y = cat(1, u1, u2);
    annotation (defaultComponentName = "r2mux1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-60,60},{-20,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{-60,-20},{-20,-60}},
            lineColor={0,0,127},
            textString=
           "2"),
          Line(points={{-20,40},{0,40},{60,0},{0,-40},{-20,-40}}, color={0,0,
                127})}));
  end R2mux;

  block R3mux "Real multiplexer 3-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.RealInput u2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealInput u3[n[3]] "vector of dimension n[3]"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
    Modelica.Blocks.Interfaces.RealOutput y[sum(n)]
      "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    parameter Integer n[3]={1,1,1} "dim of input signals" annotation(Evaluate=true);

  equation
    y = cat(1,u1, u2, u2);
    annotation (defaultComponentName = "r3mux1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-20,40},{0,40},{60,0},{0,-40},{-20,-40}}, color={0,0,
                127}),
          Text(
            extent={{-60,60},{-20,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{-60,-20},{-20,-60}},
            lineColor={0,0,127},
            textString=
                 "3")}));
  end R3mux;

  block R4mux "Real multiplexer 4-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{-110,50},{-90,70}})));
    Modelica.Blocks.Interfaces.RealInput u2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{-110,10},{-90,30}})));
    Modelica.Blocks.Interfaces.RealInput u3[n[3]] "vector of dimension n[3]"
      annotation (Placement(transformation(extent={{-110,-30},{-90,-10}})));
    Modelica.Blocks.Interfaces.RealInput u4[n[4]] "vector of dimension n[4]"
      annotation (Placement(transformation(extent={{-110,-70},{-90,-50}})));
    Modelica.Blocks.Interfaces.RealOutput y[sum(n)]
      "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    parameter Integer n[4]={1,1,1,1} "dim of input signals" annotation(Evaluate=true);

  equation
    y = cat(1, u1[1:n[1]], u2[1:n[2]], u3[1:n[3]], u4[1:n[4]]);
    annotation (defaultComponentName = "r4mux1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-20,40},{0,40},{60,0},{0,-40},{-20,-40}}, color={0,0,
                127}),
          Text(
            extent={{-60,60},{-20,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{-60,-20},{-20,-60}},
            lineColor={0,0,127},
            textString=
                 "4")}));
  end R4mux;

  block R2demux "Real demultiplexer 2-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u[sum(n)] "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput y1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
    Modelica.Blocks.Interfaces.RealOutput y2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    parameter Integer n[2]={1,1} "dim of output signals" annotation(Evaluate=true);

  equation
    u = cat(1, y1, y2);
    annotation (defaultComponentName = "r2demux1",
      Documentation(
              info="<html>
</html>
"),      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,40},{-20,40},{-60,0},{-20,-40},{0,-40}}, color={0,0,
                127}),
          Text(
            extent={{20,60},{60,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{20,-20},{60,-60}},
            lineColor={0,0,127},
            textString=
           "2")}));
  end R2demux;

  block R3demux "Real demultiplexer 3-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u[sum(n)] "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput y1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
    Modelica.Blocks.Interfaces.RealOutput y2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    Modelica.Blocks.Interfaces.RealOutput y3[n[3]] "vector of dimension n[3]"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    parameter Integer n[3]={1,1,1} "dim of output signals" annotation(Evaluate=true);

  equation
    u = cat(1, y1, y2, y3);
    annotation (defaultComponentName = "r3demux1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,40},{-20,40},{-60,0},{-20,-40},{0,-40}}, color={0,0,
                127}),
          Text(
            extent={{20,60},{60,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{20,-20},{60,-60}},
            lineColor={0,0,127},
            textString=
                 "3")}));
  end R3demux;

  block R4demux "Real demultiplexer 4-fold"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput u[sum(n)] "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput y1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{90,50},{110,70}})));
    Modelica.Blocks.Interfaces.RealOutput y2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{90,10},{110,30}})));
    Modelica.Blocks.Interfaces.RealOutput y3[n[3]] "vector of dimension n[3]"
      annotation (Placement(transformation(extent={{90,-30},{110,-10}})));
    Modelica.Blocks.Interfaces.RealOutput y4[n[4]] "vector of dimension n[4]"
      annotation (Placement(transformation(extent={{90,-70},{110,-50}})));
    parameter Integer n[4]={1,1,1,1} "dim of output signals" annotation(Evaluate=true);

  equation
    u = cat(1, y1, y2, y3, y4);
    annotation (defaultComponentName = "r4demux1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,40},{-20,40},{-60,0},{-20,-40},{0,-40}}, color={0,0,
                127}),
          Text(
            extent={{20,60},{60,20}},
            lineColor={0,0,127},
            textString=
                 "1"),
          Text(
            extent={{20,-20},{60,-60}},
            lineColor={0,0,127},
            textString=
                 "4")}));
  end R4demux;

  block B2mux "Boolean multiplexer 2-fold "
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.BooleanInput u1[n[1]] "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.BooleanInput u2[n[2]] "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
    Modelica.Blocks.Interfaces.BooleanOutput y[sum(n)]
      "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
    parameter Integer n[2]={1,1} "dim of input signals" annotation(Evaluate=true);

  equation
    y = cat(1, u1, u2);
    annotation (defaultComponentName = "b2mux1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-60,60},{-20,20}},
            lineColor={255,0,255},
            textString=
           "1"),
          Text(
            extent={{-60,-20},{-20,-60}},
            lineColor={255,0,255},
            textString=
           "2"),
          Line(points={{-20,40},{0,40},{60,0},{0,-40},{-20,-40}}, color={255,0,
                255})}));
  end B2mux;

  block B2demux "Boolean demultiplexer 2-fold "
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.BooleanInput u[sum(n)]
      "vector of dimension sum(n)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.BooleanOutput y1[n[1]]
      "vector of dimension n[1]"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
    Modelica.Blocks.Interfaces.BooleanOutput y2[n[2]]
      "vector of dimension n[2]"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    parameter Integer n[2]={1,1} "dim of output signals" annotation(Evaluate=true);

  equation
    u = cat(1, y1, y2);
    annotation (defaultComponentName = "b2demux1",
      Documentation(
              info="<html>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,40},{-20,40},{-60,0},{-20,-40},{0,-40}}, color={255,0,
                255}),
          Text(
            extent={{20,60},{60,20}},
            lineColor={255,0,255},
            textString=
                 "1"),
          Text(
            extent={{20,-20},{60,-60}},
            lineColor={255,0,255},
            textString=
           "2")}));
  end B2demux;

  block Gate2demux "Boolean demultiplexer 2-fold"
    extends PowerSystems.Icons.Block;

    Modelica.Blocks.Interfaces.BooleanInput gates[2*n]
      "vector of dimension 2*n"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.BooleanOutput gates_1[n] "vector of dimension n"
      annotation (Placement(transformation(
          origin={-40,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.BooleanOutput gates_2[n] "vector of dimension n"
      annotation (Placement(transformation(
          origin={40,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Integer n=1 "number of pairs out" annotation(Evaluate=true);

  equation
    gates_1[1:n] = gates[1:n];
    gates_2[1:n] = gates[(n + 1):(n + n)];
    annotation (defaultComponentName = "gate2demux1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,50},{-38,12},{-38,-8}}, color={255,0,255}),
          Line(points={{0,50},{40,12},{40,-8}}, color={255,0,255}),
          Text(
            extent={{-50,-10},{-30,-50}},
            lineColor={255,0,255},
            textString=
           "1"),
          Text(
            extent={{30,-8},{50,-48}},
            lineColor={255,0,255},
            textString=
           "2")}));
  end Gate2demux;

  block Gate3demux "Boolean demultiplexer 3-fold "
    extends PowerSystems.Icons.Block;

    Modelica.Blocks.Interfaces.BooleanInput gates[3*n]
      "vector of dimension 3*n"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.BooleanOutput gates_a[n] "vector of dimension n"
      annotation (Placement(transformation(
          origin={-60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.BooleanOutput gates_b[n] "vector of dimension n"
      annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    Modelica.Blocks.Interfaces.BooleanOutput gates_c[n] "vector of dimension n"
      annotation (Placement(transformation(
          origin={60,-100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
    parameter Integer n=1 "number of triples out" annotation(Evaluate=true);

  equation
    gates_a[1:n] = gates[1:n];
    gates_b[1:n] = gates[(n + 1):2*n];
    gates_c[1:n] = gates[(2*n + 1):3*n];
    annotation (defaultComponentName = "gate3demux1",
      Documentation(
              info="<html>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{0,50},{-60,14},{-60,-10}}, color={255,0,255}),
          Line(points={{0,50},{0,-10}}, color={255,0,255}),
          Line(points={{0,50},{60,14},{60,-10}}, color={255,0,255}),
          Text(
            extent={{50,-10},{70,-50}},
            lineColor={255,0,255},
            textString=
           "c"),
          Text(
            extent={{-10,-10},{10,-50}},
            lineColor={255,0,255},
            textString=
           "b"),
          Text(
            extent={{-70,-10},{-50,-50}},
            lineColor={255,0,255},
            textString=
           "a")}));
  end Gate3demux;

  block AD1ph "Analog-Digital converter 1-phase"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput p "scalar analog p-signal"
                              annotation (Placement(transformation(extent={{
              -110,30},{-90,50}})));
    Modelica.Blocks.Interfaces.RealInput n "scalar analog n-signal"
                              annotation (Placement(transformation(extent={{
              -110,-50},{-90,-30}})));
    Modelica.Blocks.Interfaces.BooleanOutput gates[2] "boolean {p, n}-gate"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));

  equation
    gates[1] = if p > 0.5 then true else false;
    gates[2] = if n > 0.5 then true else false;
    annotation (defaultComponentName = "AD1ph",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-60,60},{-20,20}},
            lineColor={0,0,127},
            textString=
                 "p"), Text(
            extent={{-60,-20},{-20,-60}},
            lineColor={0,0,127},
            textString=
                 "n")}),
      Documentation(
              info="<html>
<pre>
  Output:
  gates[1] = true if p > 0.5 else gates[1] = false
  gates[2] = true if n > 0.5 else gates[2] = false
</pre>
</html>
"));
  end AD1ph;

  block DA1ph "Digital-Analog converter 1-phase"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.BooleanInput gates[2] "boolean {p, n}-gate"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput p "scalar analog p-signal"
                               annotation (Placement(transformation(extent={{90,
              30},{110,50}})));
    Modelica.Blocks.Interfaces.RealOutput n "scalar analog n-signal"
                               annotation (Placement(transformation(extent={{90,
              -50},{110,-30}})));

  equation
    p = if gates[1] then 1 else 0;
    n = if gates[2] then 1 else 0;
    annotation (defaultComponentName = "DA1ph",
      Documentation(
              info="<html>
<pre>
  Output:
  p = 1 if gates[1] = true else p = 0
  n = 1 if gates[2] = true else n = 0
</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{20,60},{60,20}},
            lineColor={0,0,127},
            textString=
                 "p"), Text(
            extent={{20,-20},{60,-60}},
            lineColor={0,0,127},
            textString=
                 "n")}));
  end DA1ph;

  block AD3ph "Analog-Digital converter 3-phase"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.RealInput a_p "scalar analog p-signal phase a"
                                annotation (Placement(transformation(extent={{
              -110,90},{-90,110}})));
    Modelica.Blocks.Interfaces.RealInput a_n "scalar analog n-signal phase a"
                                annotation (Placement(transformation(extent={{
              -110,50},{-90,70}})));
    Modelica.Blocks.Interfaces.RealInput b_p "scalar analog p-signal phase b"
                                annotation (Placement(transformation(extent={{
              -110,10},{-90,30}})));
    Modelica.Blocks.Interfaces.RealInput b_n "scalar analog n-signal phase b"
                                annotation (Placement(transformation(extent={{
              -110,-30},{-90,-10}})));
    Modelica.Blocks.Interfaces.RealInput c_p "scalar analog p-signal phase c"
                                annotation (Placement(transformation(extent={{
              -110,-70},{-90,-50}})));
    Modelica.Blocks.Interfaces.RealInput c_n "scalar analog n-signal phase c"
                                annotation (Placement(transformation(extent={{
              -110,-110},{-90,-90}})));
    Modelica.Blocks.Interfaces.BooleanOutput gates[6]
      "boolean {a_p,a_n, b_p,b_n, c_p,c_n,}-gate"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));

  equation
    gates[1] = if a_p > 0.5 then true else false;
    gates[2] = if a_n > 0.5 then true else false;
    gates[3] = if b_p > 0.5 then true else false;
    gates[4] = if b_n > 0.5 then true else false;
    gates[5] = if c_p > 0.5 then true else false;
    gates[6] = if c_n > 0.5 then true else false;
    annotation (defaultComponentName = "AD3ph",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-60,60},{0,20}},
            lineColor={0,0,127},
            textString=
           "a_p"),
          Text(
            extent={{-60,-20},{0,-60}},
            lineColor={0,0,127},
            textString=
           "c_n"),
          Text(
            extent={{-60,20},{0,-20}},
            lineColor={0,0,127},
            textString=
           "."),
          Text(
            extent={{-60,30},{0,-10}},
            lineColor={0,0,127},
            textString=
           ".")}),
      Documentation(
              info="<html>
<pre>
  Output:
  gates[1] = true if a_p > 0.5 else gates[1] = false
  gates[2] = true if a_n > 0.5 else gates[2] = false
  gates[3] = true if b_p > 0.5 else gates[3] = false
  gates[4] = true if b_n > 0.5 else gates[4] = false
  gates[5] = true if c_p > 0.5 else gates[5] = false
  gates[6] = true if c_n > 0.5 else gates[6] = false
</pre>
</html>
"));
  end AD3ph;

  block DA3ph "Digital-Analog converter 3-phase"
    extends PowerSystems.Icons.Block0;

    Modelica.Blocks.Interfaces.BooleanInput gates[6]
      "boolean {a_p,a_n, b_p,b_n, c_p,c_n,}-gate"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
    Modelica.Blocks.Interfaces.RealOutput a_p "scalar analog p-signal phase a"
                                 annotation (Placement(transformation(extent={{
              90,90},{110,110}})));
    Modelica.Blocks.Interfaces.RealOutput a_n "scalar analog n-signal phase a"
                                 annotation (Placement(transformation(extent={{
              90,50},{110,70}})));
    Modelica.Blocks.Interfaces.RealOutput b_p "scalar analog p-signal phase b"
                                 annotation (Placement(transformation(extent={{
              90,10},{110,30}})));
    Modelica.Blocks.Interfaces.RealOutput b_n "scalar analog n-signal phase b"
                                 annotation (Placement(transformation(extent={{
              90,-30},{110,-10}})));
    Modelica.Blocks.Interfaces.RealOutput c_p "scalar analog p-signal phase c"
                                 annotation (Placement(transformation(extent={{
              90,-70},{110,-50}})));
    Modelica.Blocks.Interfaces.RealOutput c_n "scalar analog n-signal phase c"
                                 annotation (Placement(transformation(extent={{
              90,-110},{110,-90}})));

  equation
    a_p = if gates[1] then 1 else 0;
    a_n = if gates[2] then 1 else 0;
    b_p = if gates[3] then 1 else 0;
    b_n = if gates[4] then 1 else 0;
    c_p = if gates[5] then 1 else 0;
    c_n = if gates[6] then 1 else 0;
    annotation (defaultComponentName = "DA3ph",
      Documentation(
              info="<html>
<pre>
  Output:
  a_p = 1 if gates[1] = true else a_p = 0
  a_n = 1 if gates[2] = true else a_n = 0
  b_p = 1 if gates[3] = true else b_p = 0
  b_n = 1 if gates[4] = true else b_n = 0
  c_p = 1 if gates[5] = true else c_p = 0
  c_n = 1 if gates[6] = true else c_n = 0
</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{0,60},{60,20}},
            lineColor={0,0,127},
            textString=
           "a_p"),
          Text(
            extent={{0,-20},{60,-60}},
            lineColor={0,0,127},
            textString=
           "c_n"),
          Text(
            extent={{0,30},{60,-10}},
            lineColor={0,0,127},
            textString=
           "."),
          Text(
            extent={{0,20},{60,-20}},
            lineColor={0,0,127},
            textString=
           ".")}));
  end DA3ph;
  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Multiplex;
