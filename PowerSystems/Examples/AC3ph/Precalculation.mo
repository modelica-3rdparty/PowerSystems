within PowerSystems.Examples.AC3ph;
package Precalculation "Precalculation of machine matrices"
extends Modelica.Icons.ExamplesPackage;

model Z_matrixTrDat0 "z-matrix from transient data, n=1, 0"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter Boolean use_xtr=true "use t_closed and x_transient?"
    annotation(choices(
  choice=true "t_closed and x_tr",
  choice=false "t_closed and t_open"));
  parameter SI.Time[:] tc_d={0.684058354104098}
      "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
    annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] tc_q=fill(0,0)
      "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
    annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] to_d={4.17178865983301}
      "time constant open-loop d-axis {to_d', to_d'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SI.Time[:] to_q=fill(0,0)
      "time constant open-loop q-axis {to_q', to_q'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SIpu.Reactance[n_d] xtr_d={0.290231213872833}
      "trans reactance d-axis {xtr_d', xtr_d'', ..}"
    annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance[n_q] xtr_q=fill(0,0)
      "trans reactance q-axis {xtr_q', xtr_q'', ..}"
    annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";
  parameter SIpu.Current if0=0 "induced field current at v_s=V_nom/0deg"
    annotation(Dialog(enable=use_trans and n_d>1));
  parameter SI.Angle alpha_if0=0
      "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
                                                         annotation(Dialog(enable=use_trans and n_d>1));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=true;
  final parameter Integer n_d=size(tc_d,1);
  final parameter Integer n_q=size(tc_q,1);
  function T_open = PowerSystems.Utilities.Precalculation.T_open;
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromTransDat;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, omega_nom*tc_d, T_open(x_d, xtr_d, omega_nom*tc_d), x_d, xsig_s, r_s, if0, alpha_if0+pi, 0, true);
  (zr_q, zx_q) := z_matrix(n_q, omega_nom*tc_q, T_open(x_q, xtr_q, omega_nom*tc_q), x_q, xsig_s, r_s, 0, 0, 0, false);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 1 (d-axis), = 0 (q-axis).</p>
<p>Transient data are:
<pre>
  tc_d  = {0.684058354104098} d-axis
  to_d  = {4.171788659833010} d-axis
  xtr_d = {0.290231213872833} d-axis
</pre></p>
<p>Corresponding circuit data are:
<pre>
  {r[1], r_0} = {0.0011, 0.005},      d-axis
  {r_0} = {0.005},                    q-axis
  {xsig_r[1], xsig_0} = {0.13, 0.17}, d-axis
  {xsig_0} = {0.17},                  q-axis
  {xm[1], xm_0} = {0, 1.6} d-axis (no coupling terms in transient order 1)
  {xm_0} = {1.6}           q-axis
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Line(points={{-80,-20},{-40,-20}}, color={0,0,255}),
          Line(points={{-100,-90},{-40,-90}}, color={0,0,255}),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}));
end Z_matrixTrDat0;

model Z_matrixTrDat1 "z-matrix from transient data, n=1"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter Boolean use_xtr=true "use t_closed and x_transient?"
    annotation(choices(
    choice=true "t_closed and x_tr",
    choice=false "t_closed and t_open"));
  parameter SI.Time[:] tc_d={0.684058354104098}
      "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
    annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] tc_q={0.684058354104098}
      "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
    annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] to_d={4.17178865983301}
      "time constant open-loop d-axis {to_d', to_d'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SI.Time[:] to_q={4.17178865983301}
      "time constant open-loop q-axis {to_q', to_q'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SIpu.Reactance[n_d] xtr_d={0.290231213872833}
      "trans reactance d-axis {xtr_d', xtr_d'', ..}"
     annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance[n_q] xtr_q={0.290231213872833}
      "trans reactance q-axis {xtr_q', xtr_q'', ..}"
    annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";
  parameter SIpu.Current if0=0 "induced field current at v_s=V_nom/0deg"
    annotation(Dialog(enable=use_trans and n_d>1));
  parameter SI.Angle alpha_if0=0
      "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
                                                         annotation(Dialog(enable=use_trans and n_d>1));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=true;
  final parameter Integer n_d=size(tc_d,1);
  final parameter Integer n_q=size(tc_q,1);
  function T_open = PowerSystems.Utilities.Precalculation.T_open;
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromTransDat;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, omega_nom*tc_d, T_open(x_d, xtr_d, omega_nom*tc_d), x_d, xsig_s, r_s, if0, alpha_if0+pi, 0, true);
  (zr_q, zx_q) := z_matrix(n_q, omega_nom*tc_q, T_open(x_q, xtr_q, omega_nom*tc_q), x_q, xsig_s, r_s, 0, 0, 0, false);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 1.</p>
<p>Transient data are:
<pre>
  tc_d  = {0.684058354104098} d-axis
  to_d  = {4.171788659833010} d-axis
  xtr_d = {0.290231213872833} d-axis

  tc_q  = {0.684058354104098} q-axis
  to_q  = {4.171788659833010} q-axis
  xtr_q = {0.290231213872833} q-axis
</pre></p>
<p>Corresponding circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 1.6} d- and q-axis (no coupling terms in transient order 1)
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixTrDat1;

model Z_matrixTrDat2 "z-matrix from transient data, n=2"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter Boolean use_xtr=true "use t_closed and x_transient?"
                                                               annotation(choices(
  choice=true "t_closed and x_tr",
  choice=false "t_closed and t_open"));
  parameter SI.Time[:] tc_d={0.859012450972537, 0.024700865536969}
      "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
   annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] tc_q={0.703091060761508, 0.0226730319219496}
      "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
  annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] to_d={4.65929627144818, 0.03185596214872}
      "time constant open-loop d-axis {to_d', to_d'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SI.Time[:] to_q={4.5014019794843, 0.0318010302865584}
      "time constant open-loop q-axis {to_q', to_q'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SIpu.Reactance[n_d] xtr_d={0.32862524982383, 0.253031064823128}
      "trans reactance d-axis {xtr_d', xtr_d'', ..}"
  annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance[n_q] xtr_q={0.27962838258275, 0.197108545894619}
      "trans reactance q-axis {xtr_q', xtr_q'', ..}"
  annotation(Dialog(enable=use_trans and use_xtr));
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";
  parameter SIpu.Current if0=0.834830547142614
      "induced field current at v_s=V_nom/0deg"
  annotation(Dialog(enable=use_trans and n_d>1));
  parameter SI.Angle alpha_if0=-1.7713318143478
      "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
                                                         annotation(Dialog(enable=use_trans and n_d>1));
  parameter Real tol=1e-6 "tolerance precalculation"
    annotation(Dialog(enable=use_trans));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=true;
  final parameter Integer n_d=size(tc_d,1);
  final parameter Integer n_q=size(tc_q,1);
  function T_open = PowerSystems.Utilities.Precalculation.T_open;
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromTransDat;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, omega_nom*tc_d, T_open(x_d, xtr_d, omega_nom*tc_d), x_d, xsig_s, r_s, if0, alpha_if0+pi, tol, true);
  (zr_q, zx_q) := z_matrix(n_q, omega_nom*tc_q, T_open(x_q, xtr_q, omega_nom*tc_q), x_q, xsig_s, r_s, 0, 0, 0, false);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 2.</p>
<p>Transient data are:
<pre>
  tc_d  = {0.859012450972537, 0.024700865536969}  d-axis
  to_d  = {4.65929627144818,  0.03185596214872}   d-axis
  xtr_d = {0.32862524982383,  0.253031064823128}  d-axis

  tc_q  = {0.703091060761508, 0.0226730319219496} q-axis
  to_q  = {4.5014019794843,   0.0318010302865584} q-axis
  xtr_q = {0.27962838258275,  0.197108545894619}  q-axis
</pre></p>
<p>Corresponding circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.06, 1.6}, d-axis (coupling term xm[2])
  {xm[1:n], xm_0} = {0, 0, 1.6},    d-axis (coupling term zero)
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixTrDat2;

model Z_matrixTrDat3 "z-matrix from transient data, n=3"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter Boolean use_xtr=true "use t_closed and x_transient?"
                                                               annotation(choices(
  choice=true "t_closed and x_tr",
  choice=false "t_closed and t_open"));
  parameter SI.Time[:] tc_d={0.869154751730285, 0.028837675377106, 0.00293370131083294}
      "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
   annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] tc_q={0.710852334653427, 0.0267828156988275, 0.00180497772676905}
      "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
  annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] to_d={4.79391852025869, 0.0390156982688014, 0.00331427152151774}
      "time constant open-loop d-axis {to_d', to_d'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SI.Time[:] to_q={4.63399482930977, 0.0387883296526998, 0.00197933074735668}
      "time constant open-loop q-axis {to_q', to_q'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SIpu.Reactance[n_d] xtr_d={0.324237799805601, 0.238193407852477, 0.209956053815068}
      "trans reactance d-axis {xtr_d', xtr_d'', ..}"
  annotation(Dialog(enable=use_xtr));
  parameter SIpu.Reactance[n_q] xtr_q={0.275669319075036, 0.187924853846143, 0.170964423631031}
      "trans reactance q-axis {xtr_q', xtr_q'', ..}"
  annotation(Dialog(enable=use_xtr));
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";
  parameter SIpu.Current if0=0.47426409312074
      "induced field current at v_s=V_nom/0deg"
                                               annotation(Dialog(enable=use_trans and n_d>1));
  parameter SI.Angle alpha_if0=-2.5862242826262
      "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
                                                         annotation(Dialog(enable=use_trans and n_d>1));
  parameter Real tol=1e-6 "tolerance precalculation"
    annotation(Dialog(enable=use_trans));

  Real[n_d + 1] zr_d;
  Real[n_d + 1, n_d + 1] zx_d;
  Real[n_q + 1] zr_q;
  Real[n_q + 1, n_q + 1] zx_q;

  protected
  final parameter Boolean use_trans=true;
  final parameter Integer n_d=size(tc_d,1);
  final parameter Integer n_q=size(tc_q,1);
  function T_open = PowerSystems.Utilities.Precalculation.T_open;
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromTransDat;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, omega_nom*tc_d, T_open(x_d, xtr_d, omega_nom*tc_d), x_d, xsig_s, r_s, if0, alpha_if0+pi, tol, true);
  (zr_q, zx_q) := z_matrix(n_q, omega_nom*tc_q, T_open(x_q, xtr_q, omega_nom*tc_q), x_q, xsig_s, r_s, 0, 0, 0, false);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 3.</p>
<p>Transient data are:
<pre>
  tc_d  = {0.869154751730285, 0.028837675377106,  0.00293370131083294} d-axis
  to_d  = {4.79391852025869,  0.0390156982688014, 0.00331427152151774} d-axis
  xtr_d = {0.324237799805601, 0.238193407852477,  0.209956053815068}   d-axis

  tc_q  = {0.710852334653427, 0.0267828156988275, 0.00180497772676905} q-axis
  to_q  = {4.63399482930977,  0.0387883296526998, 0.00197933074735668} q-axis
  xtr_q = {0.275669319075036, 0.187924853846143,  0.170964423631031}   q-axis
</pre></p>
<p>Corresponding circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.03, 0.005},     d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.001, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.02, 0.04, 1.6}, d-axis (coupling terms xm[2:3])
  {xm[1:n], xm_0} = {0, 0, 0, 1.6},       d-axis (coupling terms zero)
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{10,80},{10,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{10,-20},{10,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{-14,66},{6,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd3"),
          Text(
            extent={{-12,32},{6,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd3"),
          Text(
            extent={{-16,90},{-2,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d3"),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,82},{0,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,70},{12,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,40},{12,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,-30},{12,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,-60},{12,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{-12,-68},{6,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq3"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{-14,-34},{6,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq3"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixTrDat3;

model Z_matrixEqCirc0 "z-matrix from equivalent circuit, n=1, 0"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";

  parameter SIpu.Reactance[:] xsig_rd={0.13}
      "leakage reactance rotor d-axis {f}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[:] xsig_rq=fill(0,0)
      "leakage reactance rotor q-axis{Q}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[size(xsig_rd,1)-1] xm_d=fill(0,0)
      "coupling-reactance d-axis"
                                annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rd,1)] r_rd={0.0011}
      "resistance rotor d-axis{f}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rq,1)] r_rq=fill(0,0)
      "resistance rotor q-axis{Q}"
    annotation(Dialog(enable=not use_trans));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=false;
  final parameter Integer n_d=size(xsig_rd,1);
  final parameter Integer n_q=size(xsig_rq,1);
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromEqCirc;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, x_d, xsig_s, r_s, xm_d, xsig_rd, r_rd);
  (zr_q, zx_q) := z_matrix(n_q, x_q, xsig_s, r_s, zeros(n_q-1), xsig_rq, r_rq);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 1 (d-axis), = 0 (q-axis).</p>
<p>Circuit data are:
<pre>
  {r[1], r_0} = {0.0011, 0.005},    d-axis
  {r_0} = {0.005},                  q-axis
  {xsig[1], xsig_0} = {0.13, 0.17}, d-axis
  {xsig_0} = {0.17},                q-axis
  {xm[1], xm_0} = {0, 1.6} d-axis (no coupling terms transient order 1)
  {xm_0} = {1.6}           q-axis
</pre></p>
<p>Corresponding transient data are:
<pre>
  tc_d  = {0.684058354104098} d-axis
  to_d  = {4.17178865983301}  d-axis
  xtr_d = {0.290231213872833} d-axis
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Line(points={{-80,-20},{-40,-20}}, color={0,0,255}),
          Line(points={{-100,-90},{-40,-90}}, color={0,0,255}),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid)}));
end Z_matrixEqCirc0;

model Z_matrixEqCirc1 "z-matrix from equivalent circuit, n=1"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";

  parameter SIpu.Reactance[:] xsig_rd={0.13}
      "leakage reactance rotor d-axis {f}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[:] xsig_rq={0.13}
      "leakage reactance rotor q-axis{Q}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[size(xsig_rd,1)-1] xm_d=fill(0,0)
      "coupling-reactance d-axis"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rd,1)] r_rd={0.0011}
      "resistance rotor d-axis{f}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rq,1)] r_rq={0.0011}
      "resistance rotor q-axis{Q}"
    annotation(Dialog(enable=not use_trans));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=false;
  final parameter Integer n_d=size(xsig_rd,1);
  final parameter Integer n_q=size(xsig_rq,1);
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromEqCirc;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, x_d, xsig_s, r_s, xm_d, xsig_rd, r_rd);
  (zr_q, zx_q) := z_matrix(n_q, x_q, xsig_s, r_s, zeros(n_q-1), xsig_rq, r_rq);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 1.</p>
<p>Circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 1.6} d- and q-axis (no coupling terms in transient order 1)
</pre></p>
<p>Corresponding transient data are:
<pre>
  tc_d  = {0.684058354104098} d-axis
  to_d  = {4.17178865983301}  d-axis
  xtr_d = {0.290231213872833} d-axis

  tc_q  = {0.684058354104098} q-axis
  to_q  = {4.17178865983301}  q-axis
  xtr_q = {0.290231213872833} q-axis
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixEqCirc1;

model Z_matrixEqCirc2 "z-matrix from equivalent circuit, n=2"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";

  parameter SIpu.Reactance[:] xsig_rd={0.13, 0.035}
      "leakage reactance rotor d-axis {f, D}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[:] xsig_rq={0.13, 0.035}
      "leakage reactance rotor q-axis{Q1, Q2}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[size(xsig_rd,1)-1] xm_d={0.06}
      "coupling-reactance d-axis"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rd,1)] r_rd={0.0011, 0.012}
      "resistance rotor d-axis{f, D}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rq,1)] r_rq={0.0011, 0.012}
      "resistance rotor q-axis{Q1, Q2}"
    annotation(Dialog(enable=not use_trans));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=false;
  final parameter Integer n_d=size(xsig_rd,1);
  final parameter Integer n_q=size(xsig_rq,1);
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromEqCirc;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, x_d, xsig_s, r_s, xm_d, xsig_rd, r_rd);
  (zr_q, zx_q) := z_matrix(n_q, x_q, xsig_s, r_s, zeros(n_q-1), xsig_rq, r_rq);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 2.</p>
<p>Circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.06, 1.6}, d-axis (coupling term xm[2])
  {xm[1:n], xm_0} = {0, 0, 1.6},    d-axis (coupling term zero)
</pre></p>
<p>Corresponding transient data are:
<pre>
  tc_d  = {0.859012450972537, 0.024700865536969}  d-axis
  to_d  = {4.65929627144818,  0.03185596214872}   d-axis
  xtr_d = {0.32862524982383,  0.253031064823128}  d-axis

  to_q  = {4.5014019794843,   0.0318010302865584} q-axis
  tc_q  = {0.703091060761508, 0.0226730319219496} q-axis
  xtr_q = {0.27962838258275,  0.197108545894619}  q-axis
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixEqCirc2;

model Z_matrixEqCirc3 "z-matrix from equivalent circuit, n=3"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";

  parameter SIpu.Reactance[:] xsig_rd={0.13, 0.035, 0.001}
      "leakage reactance rotor d-axis {f, D1, D2}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[:] xsig_rq={0.13, 0.035, 0.001}
      "leakage reactance rotor q-axis{Q1, Q2, Q3}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[size(xsig_rd,1)-1] xm_d={0.02, 0.04}
      "coupling-reactance d-axis"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rd,1)] r_rd={0.0011, 0.012, 0.03}
      "resistance rotor d-axis{f, D1, D2}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rq,1)] r_rq={0.0011, 0.012, 0.03}
      "resistance rotor q-axis{Q1, Q2, Q3}"
    annotation(Dialog(enable=not use_trans));

  Real[n_d+1] zr_d;
  Real[n_d+1, n_d+1] zx_d;
  Real[n_q+1] zr_q;
  Real[n_q+1, n_q+1] zx_q;

  protected
  final parameter Boolean use_trans=false;
  final parameter Integer n_d=size(xsig_rd,1);
  final parameter Integer n_q=size(xsig_rq,1);
  function z_matrix = PowerSystems.Utilities.Precalculation.z_fromEqCirc;

algorithm
  (zr_d, zx_d) := z_matrix(n_d, x_d, xsig_s, r_s, xm_d, xsig_rd, r_rd);
  (zr_q, zx_q) := z_matrix(n_q, x_q, xsig_s, r_s, zeros(n_q-1), xsig_rq, r_rq);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 3.</p>
<p>Circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.03, 0.005},     d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.001, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.02, 0.04, 1.6}, d-axis (coupling terms xm[2:3])
  {xm[1:n], xm_0} = {0, 0, 0, 1.6},       d-axis (coupling terms zero)
</pre></p>
<p>Corresponding transient data are:
<pre>
  tc_d  = {0.869154751730285, 0.028837675377106,  0.00293370131083294} d-axis
  to_d  = {4.79391852025869,  0.0390156982688014, 0.00331427152151774} d-axis
  xtr_d = {0.324237799805601, 0.238193407852477,  0.209956053815068}   d-axis

  tc_q  = {0.710852334653427, 0.0267828156988275, 0.00180497772676905} q-axis
  to_q  = {4.63399482930977,  0.0387883296526998, 0.00197933074735668} q-axis
  xtr_q = {0.275669319075036, 0.187924853846143,  0.170964423631031}   q-axis
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{10,80},{10,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{10,-20},{10,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{-14,66},{6,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd3"),
          Text(
            extent={{-12,32},{6,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd3"),
          Text(
            extent={{-16,90},{-2,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d3"),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-20,82},{0,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,70},{12,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,40},{12,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,-30},{12,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{8,-60},{12,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{-12,-68},{6,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq3"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{-14,-34},{6,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq3"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end Z_matrixEqCirc3;

model TransDatFromEqCirc "Calculates transient data from equivalent circuit"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";

  parameter SIpu.Reactance[:] xsig_rd={0.13, 0.035}
      "leakage reactance rotor d-axis {f, D}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[:] xsig_rq={0.13, 0.035}
      "leakage reactance rotor q-axis{Q1, Q2}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Reactance[size(xsig_rd,1)-1] xm_d={0.06}
      "coupling-reactance d-axis"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rd,1)] r_rd={0.0011, 0.012}
      "resistance rotor d-axis{f, D}"
    annotation(Dialog(enable=not use_trans));
  parameter SIpu.Resistance[size(xsig_rq,1)] r_rq={0.0011, 0.012}
      "resistance rotor q-axis{Q1, Q2}"
    annotation(Dialog(enable=not use_trans));

  output SI.Time[n_d] tc_d "time constant closed-loop d-axis";
  output SI.Time[n_d] to_d "time constant open-loop d-axis";
  output SIpu.Reactance[n_d] xtr_d(each unit="1") "transient reactance d-axis";
  output SI.Time[n_q] tc_q "time constant closed-loop q-axis";
  output SI.Time[n_q] to_q "time constant open-loop q-axis";
  output SIpu.Reactance[n_q] xtr_q(each unit="1") "transient reactance q-axis";
  protected
  final parameter Boolean use_trans=false;
  final parameter Integer n_d=size(xsig_rd,1);
  final parameter Integer n_q=size(xsig_rq,1);
  function transientData=PowerSystems.Utilities.Precalculation.transientData;
  SI.Angle[n_d] Tc_d "Time constant closed-loop d-axis";
  SI.Angle[n_d] To_d "Time constant open-loop d-axis";
  SI.Angle[n_q] Tc_q "Time constant closed-loop q-axis";
  SI.Angle[n_q] To_q "Time constant open-loop q-axis";

algorithm
  (Tc_d, To_d, xtr_d) := transientData(n_d, x_d, xsig_s, r_s, xm_d, xsig_rd, r_rd);
  tc_d := Tc_d/omega_nom;
  to_d := To_d/omega_nom;
  (Tc_q, To_q, xtr_q) := transientData(n_q, x_q, xsig_s, r_s, zeros(n_q-1), xsig_rq, r_rq);
  tc_q := Tc_q/omega_nom;
  to_q := To_q/omega_nom;
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 2.</p>
<p>Circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.06, 1.6}, d-axis (coupling term xm[2])
  {xm[1:n], xm_0} = {0, 0, 1.6},    d-axis (coupling term zero)
</pre></p>
<p>Corresponding transient data are:
<pre>
  tc_d  = {0.859012450972537, 0.024700865536969}  d-axis
  to_d  = {4.65929627144818,  0.03185596214872}   d-axis
  xtr_d = {0.32862524982383,  0.253031064823128}  d-axis

  tc_q  = {0.703091060761508, 0.0226730319219496} q-axis
  to_q  = {4.5014019794843,   0.0318010302865584} q-axis
  xtr_q = {0.27962838258275,  0.197108545894619}  q-axis
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end TransDatFromEqCirc;

model EqCircFromTransDat "Calculates equivalent circuit from transient data"
  extends Modelica.Icons.Example;

  parameter SI.AngularFrequency omega_nom=2*pi*60 "nom ang frequency";
  parameter Boolean use_xtr=true "use t_closed and x_transient?"
                                                               annotation(choices(
  choice=true "t_closed and x_tr",
  choice=false "t_closed and t_open"));
  parameter SI.Time[:] tc_d={0.859012450972537, 0.024700865536969}
      "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
  annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] tc_q={0.703091060761508, 0.0226730319219496}
      "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
  annotation(Dialog(enable=use_trans));
  parameter SI.Time[:] to_d={4.65929627144818, 0.03185596214872}
      "time constant open-loop d-axis {to_d', to_d'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SI.Time[:] to_q={4.5014019794843, 0.0318010302865584}
      "time constant open-loop q-axis {to_q', to_q'', ..}"
  annotation(Dialog(enable=use_trans and (not use_xtr)));
  parameter SIpu.Reactance[n_d] xtr_d={0.32862524982383, 0.253031064823128}
      "trans reactance d-axis {xtr_d', xtr_d'', ..}"
  annotation(Dialog(enable=use_xtr));
  parameter SIpu.Reactance[n_q] xtr_q={0.27962838258275, 0.197108545894619}
      "trans reactance q-axis {xtr_q', xtr_q'', ..}"
   annotation(Dialog(enable=use_xtr));
  parameter SIpu.Reactance x_d=1.77 "syn reactance d-axis";
  parameter SIpu.Reactance x_q=1.77 "syn reactance q-axis";
  parameter SIpu.Reactance xsig_s=0.17 "leakage reactance stator";
  parameter SIpu.Resistance r_s=0.005 "resistance stator";
  parameter SIpu.Current if0=0.834830547142614
      "induced field current at v_s=V_nom/0deg"
    annotation(Dialog(enable=use_trans and n_d>1));
  parameter SI.Angle alpha_if0=-1.7713318143478
      "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
                                                         annotation(Dialog(enable=use_trans and n_d>1));
  parameter Real tol=1e-6 "tolerance precalculation"
    annotation(Dialog(enable=use_trans));

  output SIpu.Resistance[n_d] r_rd(each unit="1");
  output SIpu.Reactance[n_d] xsig_rd(each unit="1");
  output SIpu.Reactance[n_d+1] xm_d(each unit="1");
  output SIpu.Resistance[n_q] r_rq(each unit="1");
  output SIpu.Reactance[n_q] xsig_rq(each unit="1");
  output SIpu.Reactance[n_q+1] xm_q(each unit="1");

  protected
  final parameter Boolean use_trans=true;
  final parameter Integer n_d=size(tc_d,1);
  final parameter Integer n_q=size(tc_q,1);
  function T_open = PowerSystems.Utilities.Precalculation.T_open;
  function equiCircuit = PowerSystems.Utilities.Precalculation.equiCircuit;

algorithm
  (r_rd, xsig_rd, xm_d) := equiCircuit(n_d, omega_nom*tc_d, T_open(x_d, xtr_d, omega_nom*tc_d), x_d, xsig_s, r_s, if0, alpha_if0+pi, tol, true);
  (r_rq, xsig_rq, xm_q) := equiCircuit(n_q, omega_nom*tc_q, T_open(x_q, xtr_q, omega_nom*tc_q), x_q, xsig_s, r_s, 0, 0, 0, false);
  annotation (Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Test generator, transient order = 2.</p>
<p>Transient data are:
<pre>
  tc_d  = {0.859012450972537, 0.024700865536969}  d-axis
  to_d  = {4.65929627144818,  0.03185596214872}   d-axis
  xtr_d = {0.32862524982383,  0.253031064823128}  d-axis

  tc_q  = {0.703091060761508, 0.0226730319219496} q-axis
  to_q  = {4.5014019794843,   0.0318010302865584} q-axis
  xtr_q = {0.27962838258275,  0.197108545894619}  q-axis
</pre></p>
<p>Corresponding circuit data are:
<pre>
  {r[1:n], r_0} = {0.0011, 0.012, 0.005},    d- and q-axis
  {xsig[1:n], xsig_0} = {0.13, 0.035, 0.17}, d- and q-axis
  {xm[1:n], xm_0} = {0, 0.06, 1.6}, d-axis (coupling term xm[2])
  {xm[1:n], xm_0} = {0, 0, 1.6},    d-axis (coupling term zero)
  v_f denotes the field voltage.
</pre></p>
<p>The code uses
<pre>
  xsig_s for xsig_0: armature leakage reactance
  xsig_r for xsig:   rotor leakage reactances
  r_s    for r_0:    armature (stator) resistance
  r_r    for r:      rotor resistances
</pre></p>
<p><a href=\"modelica://PowerSystems.Examples.AC3ph.Precalculation\">up users guide</a></p>
</html>"), Diagram(graphics={
          Line(points={{-40,80},{-40,10}}, color={0,0,255}),
          Line(points={{50,80},{50,10}}, color={0,0,255}),
          Line(points={{90,50},{90,40}}, color={0,0,255}),
          Line(points={{-100,10},{90,10},{90,20}}, color={0,0,255}),
          Line(points={{50,-20},{50,-90}}, color={0,0,255}),
          Line(points={{-40,-20},{-40,-90}}, color={0,0,255}),
          Line(points={{-100,-90},{90,-90},{90,-80}}, color={0,0,255}),
          Line(points={{-80,-20},{90,-20},{90,-30}}, color={0,0,255}),
          Line(points={{90,-50},{90,-60}}, color={0,0,255}),
          Text(
            extent={{24,90},{38,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d2"),
          Text(
            extent={{-66,90},{-52,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-96,90},{-82,84}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-96,-10},{-82,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_s"),
          Text(
            extent={{-66,-10},{-52,-16}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_s"),
          Text(
            extent={{-100,40},{-60,20}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "d-axis"),
          Text(
            extent={{-100,-60},{-60,-80}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "q-axis"),
          Text(
            extent={{-86,-54},{-46,-60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_q = x_q - xsig_s"),
          Text(
            extent={{-86,46},{-46,40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xm_d = x_d - xsig_s"),
          Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
          Text(
            extent={{78,6},{92,0}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "v_f"),
          Line(points={{-80,80},{90,80},{90,70}}, color={0,0,255}),
          Rectangle(
            extent={{-100,82},{-80,78}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,82},{-50,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,60},{-38,30}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,70},{92,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,40},{92,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{20,82},{40,78}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,70},{52,50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,40},{52,20}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-100,-18},{-80,-22}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-70,-18},{-50,-22}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-42,-40},{-38,-70}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-30},{92,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{88,-60},{92,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-30},{52,-50}},
            lineColor={0,0,255},
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{48,-60},{52,-80}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{26,66},{46,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd2"),
          Text(
            extent={{66,66},{86,60}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rd1"),
          Text(
            extent={{28,32},{46,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd2"),
          Text(
            extent={{68,32},{86,26}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rd1"),
          Text(
            extent={{28,-68},{46,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq2"),
          Text(
            extent={{68,-68},{86,-74}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "r_rq1"),
          Text(
            extent={{26,-34},{46,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq2"),
          Text(
            extent={{66,-34},{86,-40}},
            lineColor={0,0,255},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid,
            textString=
                 "xsig_rq1")}));
end EqCircFromTransDat;

annotation (preferredView="info",
    Documentation(info="<html>
<p>These examples illustrate the connection between different ways of specifying a machine.<br>
In particular they show the calculation of
<ul>
<li>impedance matrix from transient data</li>
<li>impedance matrix from equivalent circuit</li>
<li>transient data from equivalent circuit</li>
<li>equivalent circuit from transient data</li>
</ul>
for transient order n = 1 to 3.</p>
<p>The parameter values are chosen in such a way, that the circuit-data<br>
(see 'Diagram layer' of the examples)
<pre>  r_r[1:n], r_s} and {xsig_r[1:n], xsig_s}</pre>
remain the same for d- and q-axis. This facilitates checking.</p>
<p>For machines <b>with</b> field-winding (synchronous), d- and q-axis differ, if n > 1
<pre>
  xm[2:n] > 0, coupling terms exist in d-axis
  xm[2:n] = 0, no coupling terms in q-axis
</pre>
whereas for machines <b>without</b> field-winding (asynchronous), the q-axis diagram is valid for both axes d and q.</p>
<p><a href=\"modelica://PowerSystems.Examples\">up users guide</a></p>
</html>"));
end Precalculation;
