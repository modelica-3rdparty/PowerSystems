within PowerSystems.AC1ph_DC;
package Nodes "Nodes "
  extends Modelica.Icons.VariantsPackage;

  model Ground "Ground, 1-phase"
    extends Ports.Port_p;

  equation
    term.v = zeros(2);
    annotation (defaultComponentName = "grd1",
      Documentation(
              info="<html>
<p>Zero voltage on both conductors of terminal.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(points={{-90,0},{-4,0}}, color={0,0,255}),
            Rectangle(
            extent={{-4,60},{4,-60}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-80,0},{-60,0}},
            color={0,128,255},
            thickness=0.5), Rectangle(
            extent={{-60,20},{-54,-20}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid)}));
  end Ground;

  model PolarityGround "Polarity grounding, 1-phase"
    extends Ports.Port_p;

    parameter Integer pol(min=-1,max=1)=-1 "grounding scheme" annotation(Evaluate=true,
      choices(choice=1 "plus", choice=0 "symmetrical", choice=-1 "negative"));

  equation
    if pol==1 then
      term.v[1] = 0;
      term.i[2] = 0;
    elseif pol==-1 then
      term.v[2] = 0;
      term.i[1] = 0;
    else
      term.v[1] + term.v[2] = 0;
      term.i[1] = term.i[2];
    end if;
    annotation (defaultComponentName = "polGrd1",
      Documentation(
              info="<html>
<p>Zero voltage depending on polarity choice.</p>
<pre>
  pol =  1     conductor 1 grounded (DC: positive)
  pol =  0     symmetrically grounded
  pol = -1     conductor 2 grounded (DC: negative)
</pre>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-50,0},{-4,0}}, color={0,0,255}),
          Rectangle(
            extent={{-4,60},{4,-60}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-80,10},{-50,0}},
            color={0,0,255},
            pattern=LinePattern.Dot),
          Line(
            points={{-80,0},{-50,0}},
            color={0,0,255},
            pattern=LinePattern.Dot),
          Line(
            points={{-80,-10},{-50,0}},
            color={0,0,255},
            pattern=LinePattern.Dot)}),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(
            points={{-80,-4},{-60,-4}},
            color={0,0,255},
            pattern=LinePattern.Dot,
            thickness=0.5),
          Rectangle(
            extent={{-60,20},{-54,-20}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid),
          Line(
            points={{-80,4},{-60,4}},
            color={0,0,255},
            pattern=LinePattern.Dot,
            thickness=0.5),
          Line(
            points={{-80,0},{-60,0}},
            color={0,0,255},
            pattern=LinePattern.Dot,
            thickness=0.5)}));
  end PolarityGround;

  model GroundOne "Ground, one conductor"

    Interfaces.Electric_p term annotation (Placement(transformation(extent={{
              -110,-10},{-90,10}})));

  equation
    term.v = 0;
    annotation (
      defaultComponentName="grdOne1",
  Documentation(
          info="<html>
<p>Zero voltage on terminal.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
         "%name"),
          Rectangle(
            extent={{-4,50},{4,-50}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid),
          Line(points={{-90,0},{-4,0}}, color={0,0,255})}),
  Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-60,0},{-80,0}},
            color={0,128,255},
            thickness=0.5), Rectangle(
            extent={{-60,20},{-54,-20}},
            lineColor={128,128,128},
            fillColor={160,160,164},
            fillPattern=FillPattern.Solid)}));
  end GroundOne;

  model BusBar "Busbar, 1-phase"

    output SI.Voltage v(stateSelect=StateSelect.never);
    Ports.TwoPin_p term
      annotation (Placement(transformation(extent={{-7,-60},{7,60}})));

  equation
    term.i = zeros(2);
    v = term.v[1] - term.v[2];
    annotation (defaultComponentName = "bus1",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
           "%name"), Rectangle(
            extent={{-10,80},{10,-80}},
            lineColor={0,0,255},
            pattern=LinePattern.None,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}),
      Documentation(
              info="<html>
<p>Calculates difference voltage conductor 1 - conductor 2.</p>
</html>
"));
  end BusBar;

  model Electric_pn_p_n "Adaptor ElectricV[2] (vector) to Electric (scalar)."

    Ports.TwoPin_p term_pn "vector pin {p,n}"
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    Interfaces.Electric_p term_p "scalar p"
        annotation (Placement(transformation(extent={{70,30},{50,50}})));
    Interfaces.Electric_n term_n "scalar n"
        annotation (Placement(transformation(extent={{50,-50},{70,-30}})));

  equation
    term_pn.v = {term_p.v, term_n.v};
    term_pn.i + {term_p.i, term_n.i} = zeros(2);
    annotation (defaultComponentName = "pn_p_n",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
           "%name"),
          Rectangle(
            extent={{-40,60},{40,-60}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,-10},{-10,-10},{-10,-40},{40,-40}}, color={0,0,255}),
          Line(points={{-40,10},{-10,10},{-10,40},{40,40}}, color={0,0,255})}),
      Documentation(
              info="<html>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(points={{-40,5},{0,5},{0,40},{40,40}},
              color={0,0,255}), Line(points={{-40,-5},{0,-5},{0,-40},{40,-40}},
              color={0,0,255})}));
  end Electric_pn_p_n;

/*
  model Electric_abc_a_b_c "Adaptor ElectricV[3] (vector) to Electric (scalar)"

    Basic.Interfaces.ElectricV_p term_abc(
                                         final m=3) "vector {a,b,c}"
      annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
    Interfaces.Electric_n term_a "scalar a"
        annotation (Placement(transformation(extent={{50,30},{70,50}})));
    Interfaces.Electric_n term_b "scalar b"
        annotation (Placement(transformation(extent={{50,-10},{70,10}})));
    Interfaces.Electric_n term_c "scalar c"
        annotation (Placement(transformation(extent={{50,-50},{70,-30}})));

  equation
    {term_a.v,term_b.v,term_c.v} = term_abc.v;
    term_abc.i + {term_a.i,term_b.i,term_c.i} = zeros(3);
      annotation (defaultComponentName = "abc_a_b_c",
          Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-40,60},{40,-60}},
            lineColor={255,255,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{-40,10},{-10,10},{-10,40},{40,40}}, color={0,0,255}),
          Line(points={{-40,-10},{-10,-10},{-10,-40},{40,-40}}, color={0,0,255}),
          Line(points={{-40,0},{40,0}}, color={0,0,255}),
          Text(
            extent={{-100,-90},{100,-130}},
            lineColor={0,0,0},
            textString=
           "%name")}),
          Documentation(info="<html>
</html>
"),       Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Line(points={{-40,5},{0,5},{0,40},{40,40}}, color={0,0,255}),
          Line(points={{-40,-5},{0,-5},{0,-40},{40,-40}}, color={0,0,255}),
          Line(points={{-40,0},{40,0}}, color={0,0,255})}));
  end Electric_abc_a_b_c;
*/
  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Nodes;
