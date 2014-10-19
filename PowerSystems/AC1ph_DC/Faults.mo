within PowerSystems.AC1ph_DC;
package Faults "Line-faults "
  extends Modelica.Icons.VariantsPackage;

  model Short_ABC "a, b, c to ground short, 3-phase abc"
    extends Partials.FaultBase;

    Real[2] s;

  equation
    {v,i} = if on then {epsR*s,s} else {s,epsG*s};
    annotation (
      defaultComponentName="short_ABC",
    Documentation(
      info="<html>
<p>Fault acts on 'term' and connected terminals.</p>
<p>This all-phase short to ground acts directly on the non-transformed variables v and i.<br>
The transformation to inertial abc is only needed to determine the correct phase-angle.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-30,60},{-10,40}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,20},{-10,0}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-40,-70},{0,-80}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Short_ABC;

  model Fault_ab "a to b fault, 1-phase"
    extends Partials.Fault_pp;

    annotation (defaultComponentName = "fault_ab",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-30,50},{-10,30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid), Ellipse(
            extent={{-30,-10},{-10,-30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_ab;

  model Fault_A "a to ground fault, 1-phase"
    extends Partials.Fault_pg(final n_ph=1);

    annotation (defaultComponentName = "fault_A",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-30,-10},{-10,-30}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid), Ellipse(
            extent={{-30,50},{-10,30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_A;

  model Fault_B "b to ground fault, 1-phase"
    extends Partials.Fault_pg(final n_ph=2);

    annotation (defaultComponentName = "fault_B",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-30,50},{-10,30}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid), Ellipse(
            extent={{-30,-10},{-10,-30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_B;

  model Fault_Ab "b to a to ground fault, 1-phase"
    extends Partials.Fault_ppg(final n_ph=1);

    annotation (defaultComponentName = "fault_Ab",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-30,-10},{-10,-30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid), Ellipse(
            extent={{-30,50},{-10,30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_Ab;

  model Fault_aB "a to b to ground fault, 1-phase"
    extends Partials.Fault_ppg(final n_ph=2);

    annotation (defaultComponentName = "fault_aB",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Ellipse(
            extent={{-30,-10},{-10,-30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid), Ellipse(
            extent={{-30,50},{-10,30}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_aB;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model FaultBase "Line fault base, 1-phase"
      extends Ports.Port_f;

      parameter SI.Frequency f=system.f_nom "frequency (when fault occurs)";
      parameter SI.Time t_on=0.1 "approx time fault on";
      parameter SI.Angle phi_on(min=0)=0 "voltage phase angle fault on";
      parameter SI.Resistance epsR=1e-4 "resistance 'fault'";
      parameter SI.Conductance epsG=1e-4 "conductance 'no fault'";
      SI.Voltage[2] v;
      SI.Current[2] i;
      Boolean on(final start=false, fixed=true);
    protected
      outer System system;
      constant Integer[2] pair={2, 1};
      discrete SI.Time t_zero(start=Modelica.Constants.inf, fixed=true);
      Boolean v_pos(start=true, fixed=true);
      Boolean first(start=true, fixed=true);

    equation
      v = term.v;
      term.i = i;

      v_pos = v[1] - v[2] > 0;
      when time > t_on and edge(v_pos) and pre(first) then
        t_zero = time;
        first = false;
      end when;
      on = time > pre(t_zero) + phi_on/(2*pi*f);
      annotation (
        Documentation(
              info="<html>
<p>The small parameter epsG is used to define voltage on the faulted line in particular when the line is disconnected from its sources. For disconnecting switches with zero 'open' conductivity, epsG can not be set to zero.</p>
<p>Terminology:<br>
- lower case p, n:     shorted conductor to conductor<br>
- upper case P, N:     shorted conductor to ground</p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-40,80},{40,-80}},
              lineColor={0,0,0},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid), Line(
              points={{38,78},{2,-12},{38,12},{2,-78}},
              color={255,255,0},
              thickness=0.5)}));
    end FaultBase;

    partial model Fault_pp "Conductor to conductor fault, 1-phase"
      extends FaultBase;

      replaceable Common.Switching.Short fault_pp(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase

    equation
      fault_pp.v = v[1] - v[2];
      fault_pp.i = i[1];
      sum(i) = epsG*sum(v);
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-40,-70},{0,-80}},
              lineColor={0,0,0},
              fillColor={0,255,128},
              fillPattern=FillPattern.Solid)}));
    end Fault_pp;

    partial model Fault_pg "Conductor to ground fault, 1-phase"
      extends FaultBase;

      parameter Integer n_ph(min=1,max=2)=1 "faulted phase" annotation(choices(
        choice=1 "phase a1",
        choice=2 "phase a2"));
      replaceable Common.Switching.Short fault_pg(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pg
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pg
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase

    protected
      final parameter Integer m_ph=pair[n_ph];

    equation
      fault_pg.v=v[n_ph];
      fault_pg.i=i[n_ph];
      i[m_ph] = epsG*v[m_ph];

      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}}), graphics={Rectangle(
              extent={{-40,-70},{0,-80}},
              lineColor={0,0,0},
              fillColor={255,0,0},
              fillPattern=FillPattern.Solid)}));
    end Fault_pg;

    partial model Fault_ppg "Conductor to conductor to ground fault, 1-phase"
      extends FaultBase;

      parameter Integer n_ph(min=1,max=2)=1 "which phase to ground" annotation(choices(
        choice=1 "phase a1",
        choice=2 "phase a2"));
      replaceable Common.Switching.Short fault_pp(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                          choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp
              "fault with arc-model")), Placement(transformation(extent={{-60,
                -20},{-20,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
      replaceable Common.Switching.Short fault_pg(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                        choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pg
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pg
              "fault with arc-model")), Placement(transformation(extent={{20,
                -20},{60,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
    protected
      final parameter Integer m_ph=pair[n_ph];
    equation
      fault_pp.v=v[m_ph] - v[n_ph];
      fault_pp.i=i[m_ph];
      fault_pg.v=v[n_ph];
      fault_pg.i=sum(i);
      annotation (
        Documentation(
              info="<html>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-40,-70},{0,-80}},
              lineColor={0,0,0},
              fillColor={255,0,0},
              fillPattern=FillPattern.Solid)}));
    end Fault_ppg;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p> Contains faults (shorts) conductor to conductor and conductor to ground.</p>
<p> Terminology:</p>
<p><tt><b>Fault_*</b></tt>, example: <tt><b>Fault_Ab</b></tt>:</p>
<p><tt>A B</tt> denote a conductor with (additional) fault to ground,<br>
<tt>a b</tt> denote a conductor with no fault to ground</p>
<p>(The notation <tt>_pp</tt> ('phase-to-phase'), <tt>_pg</tt> ('phase-to-ground') etc is chosen in analogy to three-phase faults.)</p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Faults;
