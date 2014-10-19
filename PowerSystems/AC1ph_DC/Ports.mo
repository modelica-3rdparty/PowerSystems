within PowerSystems.AC1ph_DC;
package Ports "Strandard electric ports"
  extends Modelica.Icons.InterfacesPackage;

  connector TwoPin_p "AC1/DC terminal ('positive')"
    extends Interfaces.TerminalDC(redeclare package PhaseSystem =
          PhaseSystems.TwoConductor);
    annotation (defaultComponentName = "term_p",
    Documentation(info="<html>
<p>Electric connector with a vector of 'pin's, positive.</p>
</html>
"), Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Polygon(
            points={{-120,0},{0,-120},{120,0},{0,120},{-120,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-60,60},{60,-60}},
            lineColor={255,255,255},
            pattern=LinePattern.None,
            textString="")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-120,120},{100,60}},
            lineColor={0,0,255},
            textString="%name"),
          Polygon(
            points={{-20,0},{40,-60},{100,0},{40,60},{-20,0}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-10,50},{90,-50}},
            lineColor={255,255,255},
            pattern=LinePattern.None,
            textString="")}));
  end TwoPin_p;

  connector TwoPin_n "AC1/DC terminal ('negative')"
    extends Interfaces.TerminalDC(redeclare package PhaseSystem =
          PhaseSystems.TwoConductor);
    annotation (defaultComponentName = "term_n",
    Documentation(info="<html>
<p>Electric connector with a vector of 'pin's, negative.</p>
</html>"),
    Window(
      x=0.45,
      y=0.01,
      width=0.44,
      height=0.65),
    Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Polygon(
            points={{-120,0},{0,-120},{120,0},{0,120},{-120,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid), Text(
            extent={{-60,60},{60,-60}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            lineThickness=0.5,
            textString="")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,120},{120,60}},
            lineColor={0,0,255},
            textString="%name"),
          Polygon(
            points={{-100,0},{-40,-60},{20,0},{-40,60},{-100,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-90,50},{10,-50}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            lineThickness=0.5,
            textString="")}));
  end TwoPin_n;

  partial model Port_p "One port, 'positive'"

    Ports.TwoPin_p term "positive terminal"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    annotation (
            Icon(graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
             "%name")}),
      Documentation(info="<html></html>"),
                  Diagram(graphics));
  end Port_p;

  partial model Port_n "One port, 'negative'"

    Ports.TwoPin_n term "negative terminal"
  annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));
    annotation (
            Icon(graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
             "%name")}),
      Documentation(info="<html></html>"),
                  Diagram(graphics));
  end Port_n;

  partial model Port_f "One port, 'fault'"

    Ports.TwoPin_p term "fault terminal"
  annotation (Placement(transformation(
          origin={0,-100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    annotation (
            Icon(graphics={Text(
            extent={{-100,130},{100,90}},
            lineColor={0,0,0},
            textString=
             "%name")}),
      Documentation(info="<html></html>"),
                  Diagram(graphics));
  end Port_f;

  partial model Port_p_n "Two port"

    Ports.TwoPin_p term_p "positive terminal"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Ports.TwoPin_n term_n "negative terminal"
  annotation (Placement(transformation(extent={{90,-10},{110,10}}, rotation=0)));
    annotation (
  Icon(graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
             "%name")}),
  Documentation(info="<html>
</html>"),
  Diagram(graphics));

  end Port_p_n;

  partial model Port_pn "Two port, 'current_in = current_out'"
    extends Port_p_n;

  equation
    term_p.i + term_n.i = zeros(2);
    annotation (
  Icon(graphics),
  Documentation(info="<html>
</html>"),
  Diagram(graphics));
  end Port_pn;

  partial model Port_p_n_f "Three port"
    extends Port_p_n;

    Ports.TwoPin_n term_f "fault terminal"
                                         annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    annotation (
  Icon(graphics),
  Documentation(info="<html>
</html>"),
  Diagram(graphics));
  end Port_p_n_f;

  partial model PortTrafo_p_n "Two port for transformers"
    extends Port_p_n;

    SI.Voltage v1 "voltage 1";
    SI.Current i1 "current 1";

    SI.Voltage v2 "voltage 2";
    SI.Current i2 "current 2";
  protected
    Real w1 "1: voltage ratio to nominal";
    Real w2 "2: voltage ratio to nominal";

  equation
    term_p.i[1] + term_p.i[2] = 0;
    term_n.i[1] + term_n.i[2] = 0;

    v1 = (term_p.v[1] - term_p.v[2])/w1;
    term_p.i[1] = i1/w1;
    v2 = (term_n.v[1] - term_n.v[2])/w2;
    term_n.i[1] = i2/w2;
    annotation (
  Icon(graphics),
  Documentation(info="<html>
<p>Contains voltage and current scaling.</p>
<p>Below</p>
<pre>  term, v, i, w</pre>
<p>denote either the primary or secondary side</p>
<pre>
  term_p, v1, i1, w1
  term_n, v2, i2, w2
</pre>
<p>Definitions</p>
<pre>
  v:     scaled voltage across conductor
  i:     scaled current through conductor
  w:     voltage ratio to nominal (any value, but common for primary and secondary)
</pre>
<p>Relations</p>
<pre>
  v = (term.v[1] - term.v[2])/w
  term.i[1] = i/w;
</pre>
</html>
"),
  Diagram(graphics));
  end PortTrafo_p_n;

  partial model PortTrafo_p_n_n "Three port for 3-winding transformers"

    Ports.TwoPin_p term_p "positive terminal"
  annotation (Placement(transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Ports.TwoPin_n term_na "negative terminal a"
  annotation (Placement(transformation(extent={{90,30},{110,50}}, rotation=0)));
    Ports.TwoPin_n term_nb "negative terminal b"
  annotation (Placement(transformation(extent={{90,-50},{110,-30}}, rotation=0)));

    SI.Voltage v1 "voltage 1";
    SI.Current i1 "current 1";

    SI.Voltage v2a "voltage 2a";
    SI.Current i2a "current 2a";

    SI.Voltage v2b "voltage 2b";
    SI.Current i2b "current 2b";

    SI.Voltage v0;
  protected
    Real w1 "1: voltage ratio to nominal";
    Real w2a "2a: voltage ratio to nominal";
    Real w2b "2b: voltage ratio to nominal";

  equation
    term_p.i[1] + term_p.i[2] = 0;
    term_na.i[1] + term_na.i[2] = 0;
    term_nb.i[1] + term_nb.i[2] = 0;

    v1 = (term_p.v[1] - term_p.v[2])/w1;
    term_p.i[1] = i1/w1;
    v2a = (term_na.v[1] - term_na.v[2])/w2a;
    term_na.i[1] = i2a/w2a;
    v2b = (term_nb.v[1] - term_nb.v[2])/w2b;
    term_nb.i[1] = i2b/w2b;
    annotation (
  Icon(graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
             "%name")}),
  Documentation(info="<html>
<p>Contains voltage and current scaling.</p>
<p>Below</p>
<pre>  term, v, i, w</pre>
<p>denote either the primary or secondary_a or secondary_b side</p>
<pre>
  term_p, v1, i1, w1
  term_na, v2a, i2a, w2a
  term_nb, v2b, i2b, w2b
</pre>
<p>Definitions</p>
<pre>
  v:     scaled voltage across conductor
  i:     scaled current through conductor
  w:     voltage ratio to nominal (any value, but common for primary and secondary)
</pre>
<p>Relations</p>
<pre>
  v = (term.v[1] - term.v[2])/w
  term.i[1] = i/w;
</pre>
</html>
"),
  Diagram(graphics));
  end PortTrafo_p_n_n;

    annotation (preferedView="info",
      Window(
        x=0,
        y=0.55,
        width=0.15,
        height=0.41,
        library=1,
        autolayout=1),
      Documentation(info="<html>
<p>Electrical ports with connectors Ports.AC1ph_DC:</p>
<p>The index notation <tt>_p_n</tt> and <tt>_pn</tt> is used for</p>
<pre>
  _p_n:     no conservation of current
  _pn:      with conservation of current
</pre>
</html>
"));
end Ports;
