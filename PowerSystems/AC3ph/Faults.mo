within PowerSystems.AC3ph;
package Faults "Line-faults "
  extends Modelica.Icons.VariantsPackage;

  model Short_ABC "a, b, c to ground short, 3-phase dqo"
    extends Partials.FaultBase;

    Real[3] s;

  equation
    v_pos = v_abc[n_phRef] > 0;

    {v,i} = if on then {epsR*s,s} else {s,epsG*s};
      annotation (
        defaultComponentName="short_ABC",
      Documentation(
        info="<html>
<p>Fault acts on 'term' and connected terminals.</p>
<p>This all-phase short to ground acts directly on the non-transformed variables v and i.<br>
The transformation to inertial abc is only needed to determine the correct phase-angle.</p>
</html>
"),   Icon(coordinateSystem(
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

                model Fault_bc "b to c fault, 3-phase dqo"
    extends Partials.Fault_pp(final n_ph=1);

                annotation (defaultComponentName = "fault_bc",
                  Documentation(
                          info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),               Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(          extent={{-30,60},{-10,40}},
                            lineColor={0,0,0},
                            fillColor={0,255,128},
                            fillPattern=FillPattern.Solid),Ellipse(
                            extent={{-30,20},{-10,0}},
                            lineColor={0,0,0},
                            fillColor={255,0,0},
                            fillPattern=FillPattern.Solid),Ellipse(
                            extent={{-30,-20},{-10,-40}},
                            lineColor={0,0,0},
                            fillColor={255,0,0},
                            fillPattern=FillPattern.Solid)}));
                end Fault_bc;

  model Fault_ca "c to a fault, 3-phase dqo"
    extends Partials.Fault_pp(final n_ph=2);

  annotation (defaultComponentName = "fault_ca",
    Documentation(
            info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_ca;

  model Fault_ab "a to b fault, 3-phase dqo"
    extends Partials.Fault_pp(final n_ph=3);

  annotation (defaultComponentName = "fault_ab",
    Documentation(
            info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid)}));
  end Fault_ab;

  model Fault_abc "a to b to c fault, 3-phase dqo"
    extends Partials.FaultBase;

    parameter Integer n_ph=1 "double connected phase, 2=(1-2,2-3)";
    replaceable Common.Switching.Short fault_pp1(
      final on=on,
      final epsR=epsR,
      final epsG=epsG) "fault model" annotation (                          choices(
         choice(redeclare PowerSystems.Common.Switching.Short fault_pp1
            "short with small resistance"),
         choice(redeclare PowerSystems.Common.Switching.Fault fault_pp1
            "fault with arc-model")), Placement(transformation(extent={{-60,-20},
              {-20,20}}, rotation=0)));
    replaceable Common.Switching.Short fault_pp2(
      final on=on,
      final epsR=epsR,
      final epsG=epsG) "fault model" annotation (                        choices(
         choice(redeclare PowerSystems.Common.Switching.Short fault_pp2
            "short with small resistance"),
         choice(redeclare PowerSystems.Common.Switching.Fault fault_pp2
            "fault with arc-model")), Placement(transformation(extent={{20,-20},
              {60,20}}, rotation=0)));
  protected
    final parameter Integer[2] m_ph=pair[n_ph, :];

  equation
    fault_pp1.v = v_abc[m_ph[1]] - v_abc[n_ph];
    fault_pp1.i = i_abc[m_ph[1]];
    fault_pp2.v = v_abc[m_ph[2]] - v_abc[n_ph];
    fault_pp2.i = i_abc[m_ph[2]];

    v_pos = v_abc[n_phRef] > 0;
    i[3] = epsG*v[3];
  //  sum(i_abc) = epsG*sum(v_abc);
  annotation (defaultComponentName = "fault_abc",
    Documentation(
            info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Fault_abc;

  model Fault_A "a to ground fault, 3-phase dqo"
    extends Partials.Fault_pg(final n_ph=1);

    annotation (defaultComponentName = "fault_A",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid)}));
  end Fault_A;

  model Fault_B "b to ground fault, 3-phase dqo"
    extends Partials.Fault_pg(final n_ph=2);

    annotation (defaultComponentName = "fault_B",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-30,60},{-10,40}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,20},{-10,0}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid)}));
  end Fault_B;

  model Fault_C "c to ground fault, 3-phase dqo"
    extends Partials.Fault_pg(final n_ph=3);

    annotation (defaultComponentName = "fault_C",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-30,60},{-10,40}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,20},{-10,0}},
            lineColor={0,0,0},
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_C;

  model Fault_bC "b to c to ground fault, 3-phase dqo"
    extends Partials.Fault_ppg(final n_ph=1);

    annotation (defaultComponentName = "fault_bC",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-30,60},{-10,40}},
            lineColor={0,0,0},
            fillColor={0,255,128},
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
            fillPattern=FillPattern.Solid)}));
  end Fault_bC;

  model Fault_cA "c to a to ground fault, 3-phase dqo"
    extends Partials.Fault_ppg(final n_ph=2);

    annotation (defaultComponentName = "fault_cA",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-30,-20},{-10,-40}},
            lineColor={0,0,0},
            fillColor={255,0,0},
            fillPattern=FillPattern.Solid)}));
  end Fault_cA;

  model Fault_aB "a to b to ground fault, 3-phase dqo"
    extends Partials.Fault_ppg(final n_ph=3);

    annotation (defaultComponentName = "fault_aB",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
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
            fillColor={0,255,128},
            fillPattern=FillPattern.Solid)}));
  end Fault_aB;

  model Fault_Abc "b to a, c to a, a to ground fault, 3-phase dqo"
    extends Partials.Fault_pppg(final n_ph=1);

    annotation (defaultComponentName = "fault_Abc",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Fault_Abc;

  model Fault_aBc "a to b, c to b, b to ground fault, 3-phase dqo"
    extends Partials.Fault_pppg(final n_ph=2);

    annotation (defaultComponentName = "fault_aBc",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Fault_aBc;

  model Fault_abC "a to c, b to c, c to ground fault, 3-phase dqo"
    extends Partials.Fault_pppg(final n_ph=3);

    annotation (defaultComponentName = "fault_abC",
      Documentation(
              info="<html>
<p>Connect to 'fault'-terminal of faulted line.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Fault_abC;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model FaultBase "Line fault base, 3-phase dqo"
      extends Ports.Port_f;

      parameter SI.Time t_on=0.1 "approx time fault on";
      parameter SI.Angle phi_on(min=0)=0 "voltage phase angle fault on";
      parameter Integer n_phRef(min=1,max=3)=1 "of reference phase"
                                                            annotation(choices(
        choice=1 "phase a",
        choice=2 "phase b",
        choice=3 "phase c"));
      parameter SI.Resistance epsR=1e-4 "resistance 'fault'";
      parameter SI.Conductance epsG=1e-4 "conductance 'no fault'";

      parameter SI.Voltage[3] v_abc_start = zeros(3)
        "start value of voltage phase a, b, c"
        annotation(Dialog(tab="Initialization"));

      SI.Voltage[3] v "voltage";
      SI.Current[3] i "current";
      SI.Voltage[3] v_abc(each stateSelect=StateSelect.never, start = v_abc_start)
        "voltage phase a, b, c";
      SI.Current[3] i_abc(each stateSelect=StateSelect.never)
        "current phase a, b, c";
    protected
      constant Integer[3, 2] pair=[2, 3; 3, 1; 1, 2];
      discrete SI.Angle theta_zero(start=Modelica.Constants.inf, fixed=true);
      Boolean on(final start=false, fixed=true);
      Boolean v_pos(start=true, fixed=true);
      Boolean first(start=true, fixed=true);
      Real[3,3] Park = Basic.Transforms.park(term.theta[2]);

    equation
      v = term.v;
      term.i = i;
      v = Park*v_abc;
      i_abc = transpose(Park)*i;

      when time > t_on and edge(v_pos) and pre(first) then
        theta_zero = sum(term.theta);
        first = false;
      end when;
      on = sum(term.theta) > pre(theta_zero) + phi_on;
      annotation (
        Documentation(
              info="<html>
<p>The small parameter epsG is used to define voltage on the faulted line in particular when the line is disconnected from its sources. For disconnecting switches with zero 'open' conductivity, epsG can not be set to zero.</p>
<p>Terminology:<br>
- lower case a, b, c:     shorted phase to phase<br>
- upper case A, B, C:     shorted phase to ground</p>
</html>
"),     Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-40,80},{40,-80}},
              lineColor={0,0,0},
              lineThickness=0.5,
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid), Line(
              points={{38,78},{2,-12},{38,12},{2,-78}},
              color={255,255,0},
              thickness=0.5)}));
    end FaultBase;

    partial model Fault_pp "Two-phase fault, 3-phase dqo"
      extends FaultBase(final n_phRef=n_ph);

      parameter Integer n_ph(min=1,max=3)=1 "faulted pair"
                                       annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
      replaceable Common.Switching.Short fault_pp(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
    //extends Common.Switching.Partials.FaultBase
    protected
      final parameter Integer[2] m_ph=pair[n_ph, :];

    equation
      fault_pp.v=v_abc[m_ph[1]] - v_abc[m_ph[2]];
      fault_pp.i=i_abc[m_ph[1]];

      v_pos = v_abc[m_ph[1]] - v_abc[m_ph[2]] > 0;
      i_abc[n_ph] = epsG*v_abc[n_ph];
      sum(i_abc[m_ph]) = epsG*sum(v_abc[m_ph]);
    end Fault_pp;

    partial model Fault_pg "One-phase to ground fault, 3-phase dqo"
      extends FaultBase(final n_phRef=n_ph);

      parameter Integer n_ph(min=1,max=3)=1 "faulted phase" annotation(choices(
        choice=1 "phase a",
        choice=2 "phase b",
        choice=3 "phase c"));
      replaceable Common.Switching.Short fault_pg(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model"
           annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pg
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pg
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
    protected
      final parameter Integer[2] m_ph=pair[n_ph, :];

    equation
      fault_pg.v=v_abc[n_ph];
      fault_pg.i=i_abc[n_ph];

      v_pos = v_abc[n_phRef] > 0;
      i_abc[m_ph] = epsG*v_abc[m_ph];

      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}}), graphics={Rectangle(
              extent={{-40,-70},{0,-80}},
              lineColor={0,0,0},
              fillColor={255,0,0},
              fillPattern=FillPattern.Solid)}));
    end Fault_pg;

    partial model Fault_ppg "Two-phase to ground fault, 3-phase dqo"
      extends FaultBase(final n_phRef=n_ph);

      parameter Integer n_ph(min=1,max=3)=1 "faulted pair" annotation(choices(
        choice=1 "phase bc",
        choice=2 "phase ca",
        choice=3 "phase ab"));
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
      final parameter Integer[2] m_ph=pair[n_ph, :];

    equation
      fault_pp.v=v_abc[m_ph[1]] - v_abc[m_ph[2]];
      fault_pp.i=i_abc[m_ph[1]];
      fault_pg.v=v_abc[m_ph[2]];
      fault_pg.i=sum(i_abc[m_ph]);

      v_pos = v_abc[m_ph[1]] - v_abc[m_ph[2]] > 0;
      i_abc[n_ph] = epsG*v_abc[n_ph];
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Rectangle(
              extent={{-40,-70},{0,-80}},
              lineColor={0,0,0},
              fillColor={255,0,0},
              fillPattern=FillPattern.Solid)}));
    end Fault_ppg;

    partial model Fault_pppg "Three-phase to ground fault, 3-phase dqo"
      extends FaultBase;

      parameter Integer n_ph(min=1,max=3)=1 "which phase to ground" annotation(choices(
        choice=1 "phase a",
        choice=2 "phase b",
        choice=3 "phase c"));
      replaceable Common.Switching.Short fault_pp1(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                          choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp1
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp1
              "fault with arc-model")), Placement(transformation(extent={{-80,
                -20},{-40,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
      replaceable Common.Switching.Short fault_pp2(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                         choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pp2
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pp2
              "fault with arc-model")), Placement(transformation(extent={{-20,
                -20},{20,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
      replaceable Common.Switching.Short fault_pg(
        final on=on,
        final epsR=epsR,
        final epsG=epsG) "fault model" annotation (                        choices(
           choice(redeclare PowerSystems.Common.Switching.Short fault_pg
              "short with small resistance"),
           choice(redeclare PowerSystems.Common.Switching.Fault fault_pg
              "fault with arc-model")), Placement(transformation(extent={{40,
                -20},{80,20}}, rotation=0)));
    //  extends Common.Switching.Partials.FaultBase
    protected
      final parameter Integer[2] m_ph=pair[n_ph, :];

    equation
      fault_pp1.v=v_abc[m_ph[1]] - v_abc[n_ph];
      fault_pp1.i=i_abc[m_ph[1]];
      fault_pp2.v=v_abc[m_ph[2]] - v_abc[n_ph];
      fault_pp2.i=i_abc[m_ph[2]];
      fault_pg.v = v_abc[n_ph];
      fault_pg.i = sum(i_abc);

      v_pos = v_abc[n_ph] > 0;
      annotation (
        Documentation(
              info="<html>
</html>"),
        Icon(coordinateSystem(
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
              fillPattern=FillPattern.Solid)}));
    end Fault_pppg;

  end Partials;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Contains faults (shorts) phase to phase and phase to ground.</p>
<p>Terminology:</p>
<p><tt><b>Fault_*</b></tt>, example: <tt><b>Fault_abC</b></tt>:</p>
<p><tt>A B C </tt> denote a phase with (additional) fault to ground,<br>
<tt>a b c </tt> denote a phase with no fault to ground</p>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Faults;
