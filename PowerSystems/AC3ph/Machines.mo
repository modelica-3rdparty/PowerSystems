within PowerSystems.AC3ph;
package Machines "AC machines, electric part "
  extends Modelica.Icons.VariantsPackage;

  model Asynchron "Asynchronous machine, cage-rotor, 3-phase dq0"
    extends Partials.AsynchronBase(redeclare replaceable record Data =
      PowerSystems.AC3ph.Machines.Parameters.Asynchron_cage);

  initial equation
    phi_el = phi_el_start;
    annotation (
      defaultComponentName="asynchron",
  Documentation(
          info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>More information see Partials.AsynchronBase.</p>
</html>"));
  end Asynchron;

  model AsynchronY_D "Asynchronous machine Y-Delta, cage-rotor, 3-phase dq0"
    extends Partials.AsynchronBase(redeclare model Topology_p =
      PowerSystems.AC3ph.Ports.Topology.Y_Delta(control=YDcontrol) "Y-Delta",
      redeclare replaceable record Data =
          PowerSystems.AC3ph.Machines.Parameters.Asynchron_cage);
   Modelica.Blocks.Interfaces.BooleanInput YDcontrol "true:Y, false:Delta"
                                              annotation (Placement(
          transformation(extent={{-110,50},{-90,70}})));

  initial equation
    phi_el = phi_el_start;

  equation
  /*
  connect(YDcontrol, top.control) annotation (Line(points={{-100,60},{-80,60},
          {-80,40},{40,40},{40,20}}, color={255,0,255}));
*/
  annotation (defaultComponentName = "asynchron",
    Documentation(
            info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>Switcheable topology Y-Delta. The impedance values are defined with respect to the WINDINGS, i.e. they refer to Y-topology. Terminal impedance in Delta-topology is a factor 3 higher.</p>
<p>More information see Partials.AsynchronBase.</p>
</html>"));
  end AsynchronY_D;

  model Asynchron_ctrl
    "Asynchronous machine, cage-rotor, for field-oriented control, 3-phase dq0"
    extends Partials.AsynchronBase(redeclare replaceable record Data =
      PowerSystems.AC3ph.Machines.Parameters.Asynchron_cage);

    parameter PS.Current[n_r] i_d_start = zeros(n_r)
      "start value of current d_axis"
      annotation(Dialog(tab="Initialization"));
    parameter PS.Current[n_r] i_q_start = zeros(n_r)
      "start value of current q_axis"
      annotation(Dialog(tab="Initialization"));

    Modelica.Blocks.Interfaces.RealOutput[2] i_meas(each final unit="1")
      "measured current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealInput[2] i_act(each final unit="1")
      "actuated current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{10,-10},{-10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput phiRotorflux "rotor-flux angle"
      annotation (Placement(transformation(
          origin={100,100},
          extent={{10,-10},{-10,10}},
          rotation=180)));
    Modelica.Blocks.Interfaces.RealOutput[2] vPhasor
      "desired {abs(v), phase(v)}"
      annotation (Placement(transformation(
          origin={-100,100},
          extent={{-10,-10},{10,10}},
          rotation=180)));
  protected
    constant Real eps=Modelica.Constants.eps;
    final parameter PS.Current I_nom=par.S_nom/par.V_nom;
    SI.Angle alpha_i;
    SI.Angle alpha_psi;
    PS.Voltage[2] v_dq "stator voltage demand in rotor flux-system";
    PS.Current[2] i_dq "stator current demand in rotor flux-system";
    PS.Current[n_r] i_d(start = i_d_start);
    PS.Current[n_r] i_q(start = i_q_start);
    function acos=Modelica.Math.acos;

  initial equation
    phi_el = phi_el_start;
    phiRotorflux = 0;

  equation
    der(phiRotorflux) = w_el - (-diagonal(R_r)*i_rd*psi_rq + diagonal(R_r)*i_rq*psi_rd)/(psi_rd*psi_rd + psi_rq*psi_rq + eps);
    alpha_i = atan2(i[2], i[1]);
    alpha_psi = atan2(R_m*psi_rq, R_m*psi_rd);
    i_meas = {cos(alpha_i - alpha_psi),sin(alpha_i - alpha_psi)}*sqrt(i[1:2]*i[1:2])/I_nom;
    i_dq =  i_act*I_nom;
    (sum(omega) - w_el)*(-L_m*i_dq[2] - L_r*i_q) + diagonal(R_r)*i_d = zeros(n_r);
    (sum(omega) - w_el)*(L_m*i_dq[1] + L_r*i_d) + diagonal(R_r)*i_q = zeros(n_r);
    v_dq = sum(omega)*{-(c.L_s[2]*i_dq[2] + L_m*i_q), c.L_s[1]*i_dq[1] + L_m*i_d} + c.R_s*i_dq;
    vPhasor = {sqrt(v_dq*v_dq)/par.V_nom, atan2(v_dq[2], v_dq[1]) + atan2(sin(alpha_psi - term.theta[1]), cos(alpha_psi - term.theta[1]))};
    annotation (
      defaultComponentName="asynchron",
  Documentation(
          info="<html>
<p>This model is intended for field-oriented control. The input/output current-signals 'i_meas'/'i_act' represent the pu stator current in the rotorflux-fixed reference system:
<pre>
  first component   ('field'): pu current in rotorflux d-axis
  second component ('torque'): pu current in rotorflux q-axis (q 90deg pos vs d)
</pre>
The mapping from current demand to voltage demand is based on the steady-state equations of the machine.</p>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>More information see Partials.AsynchronBase.</p>
</html>
"),
  Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-90,112},{90,88}},
            lineColor={0,0,127},
            fillColor={170,213,255},
            fillPattern=FillPattern.Solid)}));
  end Asynchron_ctrl;

  model Synchron3rd_ee
    "Synchronous machine electrically excited, 3rd order model, 3-phase dq0"
    extends Partials.Synchron3rdBase(final phi_el_start=-pi/2+system.alpha0,
      redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_ee);

    final parameter PS.Voltage Vf_nom=par.V_nom; // to be accessible from 'excitation'.
    output SI.Angle powerAngle(start=0, stateSelect=StateSelect.never)
      "power angle";
    AC1ph_DC.Ports.TwoPin_p field "field winding"
      annotation (Placement(transformation(
          origin={-100,-40},
          extent={{10,10},{-10,-10}},
          rotation=180)));

  initial equation
    w_el = sum(omega);

  equation
    if par.excite==1 then
      psi_e = (field.v[1] - field.v[2])/c.omega_nom;
      field.i = zeros(2);
    else
      assert(false, "machine-parameter must be excite = 1 (el)");
    end if;
    powerAngle = noEvent(mod(phi_el - term.theta[2] - atan2(-v[1], v[2]) + pi, 2*pi)) - pi;
    annotation (defaultComponentName = "synchron",
      Documentation(
              info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>Electric excitation (<tt>excite = 1</tt>).</p>
<p>More information see Partials.Synchron3rdBase.</p>
</html>"));
  end Synchron3rd_ee;

  model Synchron_ee "Synchronous machine electrically excited, 3-phase dq0"
    extends Partials.SynchronBase(final phi_el_start=-pi/2+system.alpha0,
      redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron_ee);
    final parameter PS.Voltage Vf_nom=c.Vf_nom; // to be accessible from 'excitation'.
    output SI.Angle powerAngle(start=0, stateSelect=StateSelect.never)
      "power angle";
    AC1ph_DC.Ports.TwoPin_p field "field winding"
      annotation (Placement(transformation(
          origin={-100,-40},
          extent={{10,10},{-10,-10}},
          rotation=180)));

  initial equation
    w_el = sum(omega);

  equation
    if par.excite==1 then
      psi_e = 0;
      v_rd[1] = (field.v[1] - field.v[2])*c.wf;
      field.i = zeros(2);
    else
      assert(false, "machine-parameter must be excite = 1 (el)");
    end if;
    powerAngle = noEvent(mod(phi_el - term.theta[2] - atan2(-v[1], v[2]) + pi, 2*pi)) - pi;
  annotation (defaultComponentName = "synchron",
    Documentation(
            info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>Electric excitation (<tt>excite = 1</tt>).</p>
<p>More information see Partials.SynchronBase.</p>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Rectangle(
            extent={{-50,-42},{-30,-46}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-30,-42},{30,-46}},
            lineColor={0,0,255},
            lineThickness=0.5,
            fillColor={0,0,255},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-30,-50},{30,-52}},
            lineColor={175,175,175},
            fillColor={175,175,175},
            fillPattern=FillPattern.Solid),
          Line(points={{-50,-44},{-70,-44}}, color={0,0,255}),
          Line(points={{30,-44},{40,-44},{40,-36},{-70,-36}}, color={0,0,255}),
          Text(
            extent={{30,-39},{90,-49}},
            lineColor={0,0,255},
            textString=
                 "field")}));
  end Synchron_ee;

  model Synchron3rd_pm "Synchronous machine pm, 3rd order model, 3-phase dq0"
    extends Partials.Synchron3rdBase(redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_pm);

    Modelica.Blocks.Interfaces.RealOutput phiRotor=
                      phi_el "rotor angle el"
      annotation (Placement(transformation(
          origin={100,100},
          extent={{10,-10},{-10,10}},
          rotation=180)));

  initial equation
    phi_el = phi_el_start;

  equation
    if par.excite==1 then
      assert(false, "machine-parameter must be excite = 2 (pm) or 3 (reluctance)");
    elseif par.excite==2 then
      psi_e = c.Psi_pm; // = par.psi_pm*(par.V_nom/c.omega_nom)
    elseif par.excite==3 then
      psi_e = 0;
    end if;
    annotation (                      Documentation(info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>The model is valid for permanent magnet (<tt>excite = 2</tt>) or reluctance machines (<tt>excite = 3</tt>).</p>
<p>The relation between 'flux induced by permanent magnet' <tt>Psi_pm [Wb]</tt> and 'magnetisation' <tt>psi_pm [pu]</tt> is given by the following relation;
<pre>
  Psi_pm = psi_pm*V_nom/omega_nom
  psi_pm = Psi_pm*omega_nom/V_nom
</pre></p>
<p>More information see Partials.Synchron3rdBase.</p>
</html>"));
  end Synchron3rd_pm;

  model Synchron_pm "Synchronous machine pm, 3-phase dq0"
    extends Partials.SynchronBase(redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron_pm);

    Modelica.Blocks.Interfaces.RealOutput phiRotor=
                      phi_el "rotor angle el"
      annotation (Placement(transformation(
          origin={100,100},
          extent={{10,-10},{-10,10}},
          rotation=180)));

  initial equation
    phi_el = phi_el_start;

  equation
    if par.excite==1 then
      assert(false, "machine-parameter must be excite = 2 (pm) or 3 (reluctance)");
    elseif par.excite==2 then
      psi_e = c.Psi_pm;  // = par.psi_pm*(par.V_nom/c.omega_nom)
      v_rd[1] = 0;
    elseif par.excite==3 then
      psi_e = 0;
      v_rd[1] = 0;
    end if;
  annotation (defaultComponentName = "synchron",
    Documentation(
            info="<html>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>The model is valid for permanent magnet (<tt>excite = 2</tt>) or reluctance machines (<tt>excite = 3</tt>).</p>
<p>The relation between 'flux induced by permanent magnet' <tt>Psi_pm [Wb]</tt> and 'magnetisation' <tt>psi_pm [pu]</tt> is given by the following relation;
<pre>
  Psi_pm = psi_pm*V_nom/omega_nom
  psi_pm = Psi_pm*omega_nom/V_nom
</pre></p>
<p>More information see Partials.SynchronBase.</p>
</html>
"),    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-30,-40},{30,-44}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid)}));
  end Synchron_pm;

  model Synchron3rd_pm_ctrl
    "Synchronous machine, for field-oriented control, 3rd order model, 3-phase dq0"
    extends Partials.Synchron3rdBase(redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_pm);

    Modelica.Blocks.Interfaces.RealOutput[2] i_meas(each final unit="1")
      "measured current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealInput[2] i_act(each final unit="1")
      "actuated current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{10,-10},{-10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput phiRotor=phi_el "rotor angle el"
      annotation (Placement(transformation(
          origin={100,100},
          extent={{10,-10},{-10,10}},
          rotation=180)));
    Modelica.Blocks.Interfaces.RealOutput[2] vPhasor
      "desired {abs(u), phase(u)}"
      annotation (Placement(transformation(
          origin={-100,100},
          extent={{-10,-10},{10,10}},
          rotation=180)));
  protected
    final parameter PS.Current I_nom=par.S_nom/par.V_nom;
    PS.Voltage[2] v_dq "voltage demand {v_d, v_q} pu";
    PS.Current[2] i_dq "current demand {i_d, i_q} pu";
  initial equation
    phi_el = phi_el_start;

  equation
    if par.excite==1 then
      assert(false, "machine-parameter must be excite = 2 (pm) or 3 (reluctance)");
    elseif par.excite==2 then
      psi_e = c.Psi_pm; // = par.psi_pm*(par.V_nom/c.omega_nom)
    elseif par.excite==3 then
      psi_e = 0;
    end if;

    i_meas = i_s[1:2]/I_nom;
    i_dq = i_act*I_nom;
    v_dq = w_el*{-(c.L_s[2]*i_dq[2]),c.L_s[1]*i_dq[1] + psi_e} + c.R_s*i_dq;
    vPhasor = {sqrt(v_dq*v_dq)/par.V_nom,atan2(v_dq[2], v_dq[1])};
    annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics={Rectangle(
            extent={{-90,112},{90,88}},
            lineColor={0,0,127},
            fillColor={170,213,255},
            fillPattern=FillPattern.Solid)}),
                                           Documentation(info="<html>
<p>This model is intended for field-oriented control. The input/output current-signals 'i_meas'/'i_act' represent the pu stator current in the rotor-fixed reference system:
<pre>
  first component    (field'): pu current in rotor d-axis
  second component ('torque'): pu current in rotor q-axis (q 90deg pos vs d)
</pre>
The mapping from current demand to voltage demand is based on the steady-state equations of the machine.</p>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>The model is valid for permanent magnet (<tt>excite=2</tt>) or reluctance machines (<tt>excite=3</tt>).</p>
<p>Limit velocity for pm-excitation without field weakening (d-axis current i_s[1]=0).
<pre>  w_lim = omega_nom/psi_pm</pre></p>
<p>More information see Partials.Synchron3rdBase.</p>
</html>
"));
  end Synchron3rd_pm_ctrl;

  model Synchron_pm_ctrl
    "Synchronous machine, for field-oriented control, 3-phase dq0"
    extends Partials.SynchronBase(redeclare replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron_pm);

    Modelica.Blocks.Interfaces.RealOutput[2] i_meas(
                             final unit="1") "measured current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={-60,100},
          extent={{-10,-10},{10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealInput[2] i_act(
                             final unit="1") "actuated current {i_d, i_q} pu"
      annotation (Placement(transformation(
          origin={60,100},
          extent={{10,-10},{-10,10}},
          rotation=90)));
    Modelica.Blocks.Interfaces.RealOutput phiRotor=
                      phi_el "rotor angle el"
      annotation (Placement(transformation(
          origin={100,100},
          extent={{10,-10},{-10,10}},
          rotation=180)));
    Modelica.Blocks.Interfaces.RealOutput[2] vPhasor
      "desired {abs(u), phase(u)}"
      annotation (Placement(transformation(
          origin={-100,100},
          extent={{-10,-10},{10,10}},
          rotation=180)));
  protected
    final parameter PS.Current I_nom=par.S_nom/par.V_nom;
    PS.Voltage[2] v_dq "voltage demand {v_d, v_q} pu";
    PS.Current[2] i_dq "current demand {i_d, i_q} pu";
  initial equation
    phi_el = phi_el_start;

  equation
    if par.excite==1 then
      assert(false, "machine-parameter must be excite = 2 (pm) or 3 (reluctance)");
    elseif par.excite==2 then
      psi_e = c.Psi_pm;  // = par.psi_pm*(par.V_nom/c.omega_nom)
      v_rd[1] = 0;
    elseif par.excite==3 then
      psi_e = 0;
      v_rd[1] = 0;
    end if;

    i_meas = i_s[1:2]/I_nom;
    i_dq = i_act*I_nom;
    v_dq = w_el*{-c.L_s[2]*i_dq[2],c.L_s[1]*i_dq[1] + c.L_md[1]*i_rd[1] + psi_e}
       + c.R_s*i_dq;
    vPhasor = {sqrt(v_dq*v_dq)/par.V_nom,atan2(v_dq[2], v_dq[1])};
  annotation (defaultComponentName = "synchron",
    Documentation(
            info="<html>
<p>This model is intended for field-oriented control. The input/output current-signals 'i_meas'/'i_act' represent the pu stator current in the rotor-fixed reference system:
<pre>
  first component    (field'): pu current in rotor d-axis
  second component ('torque'): pu current in rotor q-axis (q 90deg pos vs d)
</pre>
The mapping from current demand to voltage demand is based on the steady-state equations of the machine.</p>
<p>Equivalent circuit is on <b>diagram layer</b> of parameter record ACdq0.Machines.Parameters.*</p>
<p>The model is valid for permanent magnet (<tt>excite=2</tt>) or reluctance machines (<tt>excite=3</tt>).</p>
<p>Limit velocity for pm-excitation without field weakening (d-axis current i_s[1]=0).
<pre>  w_lim = omega_nom/psi_pm</pre></p><p>More information see Partials.SynchronBase.</p>
</html>
"), Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-90,112},{90,88}},
            lineColor={0,0,127},
            fillColor={170,213,255},
            fillPattern=FillPattern.Solid)}),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-30,-40},{30,-44}},
            lineColor={176,0,0},
            fillColor={176,0,0},
            fillPattern=FillPattern.Solid)}));
  end Synchron_pm_ctrl;

  package Partials "Partial models"
    extends Modelica.Icons.BasesPackage;

    partial model ACmachine "AC machine base, 3-phase dq0"
      extends Ports.YDport_p(i(start = i_start));

      parameter Types.Dynamics dynType=system.dynType "transient or steady-state model"
        annotation(Evaluate=true, Dialog(tab="Mode"));
      parameter PS.Current[3] i_start = zeros(3)
        "start value of current conductor"
        annotation (Dialog(tab="Initialization"));
      parameter SI.Angle phi_el_start=0 "initial rotor angle electric"
        annotation (Dialog(tab="Initialization"));
      parameter SI.AngularVelocity w_start=0 "initial rotor angular velocity"
        annotation (Dialog(tab="Initialization"));
      parameter Integer pp=1 "pole-pair number";
      SI.Angle phi_el(stateSelect=StateSelect.prefer, start=phi_el_start)
        "rotor angle electric (syn: +pi/2)";
      SI.AngularVelocity w_el(stateSelect=StateSelect.prefer, start=w_el_start)
        "rotor angular velocity el";
      SI.Torque tau_el "electromagnetic torque";
      Interfaces.Rotation_n airgap "electro-mechanical connection"
        annotation (Placement(
            transformation(
            origin={0,60},
            extent={{-10,-10},{10,10}},
            rotation=270)));
      Interfaces.ThermalV_n heat(     m=2) "heat source port {stator, rotor}"
        annotation (Placement(transformation(
            origin={0,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    protected
      outer System system;
      final parameter SI.AngularVelocity w_el_start = w_start*pp
        "initial rotor angular velocity electric";
      SI.AngularFrequency[2] omega;
      function atan2 = Modelica.Math.atan2;

    equation
      omega = der(term.theta);
      pp*airgap.phi = phi_el;
      airgap.tau = -pp*tau_el;
      w_el = der(phi_el);
      annotation (
        Documentation(
              info="<html>
<p>Contains the pole-pair transformation</p>
<pre>
  pp*airgap.phi = phi_el;
  airgap.tau = -pp*tau_el;
</pre>
<p>between the 'electrical' variables phi_el and tau_el and the 'mechanical' variables airgap.phi and airgap.tau.<br>
The connector 'airgap' transfers the electromagnetic rotor-torque to the mechanical system.</p>
<p>The electric reference frame can specified by the machine rotor. Use this choice ONLY if really necessary and care about restrictions. Choosing
<pre>  isRef = true</pre> induces
<pre>  term.theta = {-phi_el_start, phi_el}</pre>
and therefore
<pre>  omega = {0, w_el}</pre>
not allowing steady-state initialisation for asynchronous machines. Note that
<pre>  phi_el = pole_pair_number*phi_mechanical</pre>
More info see at 'Machines.Asynchron' and 'Machines.Synchron'.</p>
</html>
"),     Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Ellipse(
              extent={{90,90},{-90,-90}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{70,70},{-70,-70}},
              lineColor={255,170,85},
              fillColor={255,170,85},
              fillPattern=FillPattern.Solid),
            Ellipse(
              extent={{50,50},{-50,-50}},
              lineColor={0,0,0},
              fillPattern=FillPattern.Sphere,
              fillColor={215,215,215}),
            Polygon(
              points={{-64,-10},{-59,10},{-54,-10},{-64,-10}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{55,10},{59,-10},{65,10},{55,10}},
              lineColor={255,255,255},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid)}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{70,20},{76,-20}},
              lineColor={128,128,128},
              fillColor={128,128,128},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,18},{-30,14}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,18},{30,14}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,9},{30,7}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,2},{30,-2}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-7},{30,-9}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-14},{30,-18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,25},{30,23}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,2},{-30,-2}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,-14},{-30,-18}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Line(points={{-80,-16},{-50,-16}}, color={0,0,255}),
            Line(points={{-80,0},{-50,0}}, color={0,0,255}),
            Line(points={{-80,16},{-50,16}}, color={0,0,255}),
            Text(
              extent={{-40,40},{40,30}},
              lineColor={0,0,255},
              textString="stator"),
            Text(
              extent={{-40,-90},{40,-100}},
              lineColor={0,0,255},
              textString="rotor")}));
    end ACmachine;

    partial model AsynchronBase "Asynchronous machine base, 3-phase dq0"
      extends ACmachine(final pp=par.pp, v(start={cos(system.alpha0),sin(system.alpha0),0}*par.V_nom));

      output Real slip "<0: motor, >0: generator";
      replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Asynchron(f_nom=system.f_nom)
        "machine parameters" annotation(choicesAllMatching=true);
      final parameter Data par "machine parameters"
        annotation (Placement(transformation(extent={{-60,60},{-40,80}})));
    protected
      final parameter Integer n_r = par.n_r
        "number of rotor circuits d- and q-axis";
      final parameter Coefficients.Asynchron c = Basic.Precalculation.machineAsyn(par, top.scale);
      parameter SI.Inductance L_m[n_r] = c.L_m;
      parameter SI.Resistance R_r[n_r] = c.R_r;
      parameter SI.Resistance R_m[n_r] = c.R_m;
      parameter SI.Inductance L_r[n_r,n_r] = c.L_r;

      parameter PS.Current[n_r] i_rd_start = zeros(n_r)
        "start value of rotor current d_axis"
        annotation(Dialog(tab="Initialization"));
      parameter PS.Current[n_r] i_rq_start = zeros(n_r)
        "start value of rotor current q_axis"
        annotation(Dialog(tab="Initialization"));

      PS.Voltage[n_r] v_rd=zeros(n_r) "rotor voltage d_axis, cage-rotor = 0";
      PS.Voltage[n_r] v_rq=zeros(n_r) "rotor voltage q_axis, cage-rotor = 0";
      PS.Current[n_r] i_rd(start = i_rd_start) "rotor current d_axis";
      PS.Current[n_r] i_rq(start = i_rq_start) "rotor current q_axis";
      SI.MagneticFlux[2] psi_s "magnetic flux stator dq";
      SI.MagneticFlux[n_r] psi_rd "magnetic flux rotor d";
      SI.MagneticFlux[n_r] psi_rq "magnetic fluxrotor q";

    initial equation
      if dynType == Types.Dynamics.SteadyInitial then
        der(psi_s) = omega[1]*{-psi_s[2], psi_s[1]};
        der(i[3]) = 0;
        der(psi_rd) = omega[1]*(-psi_rq);
        der(psi_rq) = omega[1]*psi_rd;
      elseif dynType <> Types.Dynamics.SteadyState then
        der(psi_rd) = omega[1]*(-psi_rq);
        der(psi_rq) = omega[1]*psi_rd;
      end if;

    equation
      psi_s = diagonal(c.L_s[1:2])*i[1:2] + {L_m*i_rd, L_m*i_rq};
      psi_rd = L_m*i[1] + L_r*i_rd;
      psi_rq = L_m*i[2] + L_r*i_rq;

      if dynType <> Types.Dynamics.SteadyState then
        der(psi_s) + omega[2]*{-psi_s[2], psi_s[1]} + c.R_s*i[1:2] = v[1:2];
        c.L_s[3]*der(i[3]) + c.R_s*i[3] = v[3];
        der(psi_rd) + (omega[2] - w_el)*(-psi_rq) + diagonal(R_r)*i_rd = v_rd;
        der(psi_rq) + (omega[2] - w_el)*psi_rd + diagonal(R_r)*i_rq = v_rq;
      else
        omega[2]*{-psi_s[2], psi_s[1]} + c.R_s*i[1:2] = v[1:2];
        c.R_s*i[3] = v[3];
        (omega[2] - w_el)*(-psi_rq) + diagonal(R_r)*i_rd = v_rd;
        (omega[2] - w_el)*psi_rd + diagonal(R_r)*i_rq = v_rq;
      end if;

      if par.neu_iso then
        i_n = zeros(top.n_n);
      else
        v_n = c.R_n*i_n "equation neutral to ground (relevant if Y-topology)";
      end if;

      slip = (w_el/sum(omega) - 1);
      tau_el = i[1:2]*{-psi_s[2], psi_s[1]};
      heat.ports.Q_flow = -{c.R_s*i*i, diagonal(R_r)*i_rd*i_rd + diagonal(R_r)*i_rq*i_rq};
    annotation (
      defaultComponentName="asynchron",
        Documentation(
          info="<html>
<p>The stator contains one winding each in d-axis, q-axis, o-axis.<br>
The rotor contains n_r windings each in d-axis and q-axis (at least one).<br>
See also equivalent circuit on 'Diagram layer' of
<a href=\"modelica://PowerSystems.AC3ph.Machines.Parameters.Asynchron\">Parameters.Asynchron</a> !</p>
<pre>
  v, i:                  stator-voltage and -current dq0
  v_rd[n_r], i_rd[n_r]:  rotor-voltage and -current d-axis
  v_rq[n_r], i_rq[n_r]:  rotor-voltage and -current q-axis
</pre>
<p>The equations are valid for <i>all</i> dq0 reference systems with arbitrary angular orientation.<br>
Special choices are</p>
<pre>
  omega[2] = omega  defines 'stator' system, rotating with stator frequency
  omega[2] = w_el   defines 'rotor' system, rotating with rotor speed el (el = mec*pp)
  omega[2] = 0      defines the inertial system, not rotating.
  with
  omega[2] = der(term.theta[2])
</pre></p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,10},{100,-10}},
              lineColor={255,255,255},
              textString="asyn")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-50,-58},{-30,-62}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-58},{30,-62}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,-74},{-30,-78}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-74},{30,-78}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-67},{30,-69}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,-42},{-30,-46}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-42},{30,-46}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-29},{30,-31}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-51},{30,-53}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-83},{30,-85}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Line(points={{-50,-86},{-60,-86},{-60,-40},{-50,-40},{-50,-42}},
                color={0,0,255}),
            Line(points={{30,-86},{40,-86},{40,-40},{30,-40},{30,-42}}, color={
                  0,0,255}),
            Line(
              points={{30,-42},{30,-86}},
              color={0,0,255},
              thickness=0.5),
            Line(
              points={{-50,-42},{-50,-86}},
              color={0,0,255},
              thickness=0.5)}));
    end AsynchronBase;

    partial model SynTransform "Rotation transform dq"
      extends ACmachine;
      import Modelica.Math.Matrices.inv;

      PS.Voltage[3] v_s "stator voltage dq0 in rotor-system";
      PS.Current[3] i_s(each stateSelect=StateSelect.prefer, start = i_s_start)
        "stator current dq0 in rotor-system";

    protected
      parameter PS.Current[3] i_s_start = cat(1, inv(Basic.Transforms.rotation_dq(phi_el_start))*i_start[1:2], {i_start[3]})
        "start value of stator current dq0 in rotor-system";
      SI.MagneticFlux psi_e "excitation flux";
      Real[2,2] Rot_dq "Rotation reference-dq0 to rotor-dq0 system";

    initial equation
      if dynType == Types.Dynamics.FixedInitial then
        // initialize i_s from i_start
        i_start = cat(1, Rot_dq*i_s[1:2], {i_s[3]});
      end if;

    equation
      Rot_dq = Basic.Transforms.rotation_dq(phi_el - term.theta[2]);
      v_s = cat(1, transpose(Rot_dq)*v[1:2], {v[3]});
      i = cat(1, Rot_dq*i_s[1:2], {i_s[3]});
      annotation (
        Documentation(
              info="<html>
<p>Contains the transformation of stator voltage and current from the dq0 reference-frame to the dq0 rotor-frame.<br>
The transformation angle is the (electric) rotor-angle relative to the reference frame.</p>
<p>If 'rotorSys = true', the reference frame is specified by the rotor. This allows to avoid the transformation. In this case, the system choice ('synchronous', 'inertial') has no influence. Note that this choice is not generally possible (for example several machines coupled to one common source).
<pre>
  v_s, i_s:    stator-voltage and -current dq0 in the rotor frame of the machine.
</pre></p>
</html>"),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{10,40},{70,30}},
              lineColor={0,0,255},
              textString="(armature)")}));
    end SynTransform;

    partial model Synchron3rdBase "Synchronous machine 3rd base, 3-phase dq0"
      extends SynTransform(final pp=par.pp,  v(start={cos(system.alpha0),sin(system.alpha0),0}*par.V_nom));

      replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron3rd(f_nom=system.f_nom)
        "machine parameters" annotation(choicesAllMatching=true);
      final parameter Data par "machine parameters"
        annotation (Placement(transformation(extent={{-60,60},{-40,80}})));
    protected
      final parameter Coefficients.Synchron3rd c = Basic.Precalculation.machineSyn3rd(
                                                                                     par,top.scale);
      SI.MagneticFlux[2] psi_s "magnetic flux stator dq";

    initial equation
      if dynType == Types.Dynamics.SteadyInitial then
        der(psi_s) = zeros(2);
        der(c.L_s[3]*i_s[3]) = 0;
      end if;

    equation
      psi_s = {c.L_s[1]*i_s[1] + psi_e, c.L_s[2]*i_s[2]};

      if dynType <> Types.Dynamics.SteadyState then
        der(psi_s) + w_el*{-psi_s[2], psi_s[1]} + c.R_s*i_s[1:2] = v_s[1:2];
        c.L_s[3]*der(i_s[3]) + c.R_s*i_s[3] = v_s[3];
      else
        w_el*{-psi_s[2], psi_s[1]} + c.R_s*i_s[1:2] = v_s[1:2];
        c.R_s*i_s[3] = v_s[3];
      end if;

      if par.neu_iso then
        i_n = zeros(top.n_n);
      else
        v_n = c.R_n*i_n "equation neutral to ground (relevant if Y-topology)";
      end if;

      tau_el = i_s[1:2]*{-psi_s[2], psi_s[1]};
      heat.ports.Q_flow = -{c.R_s*i_s*i_s, 0};
      annotation (
        Documentation(
              info="<html>
<p>'Voltage behind synchronous reactance', simplified model of synchronous machine.<br>
One winding in d-axis, q-axis, o-axis.<br>
See also equivalent circuit on 'Diagram layer' of
<a href=\"modelica://PowerSystems.AC3ph.Machines.Parameters.Synchron3rd\">Parameters.Synchron3rd</a> !</p>
<pre>
  v, i:          stator-voltage and -current dq0 reference-system
  v_s, i_s:      stator-voltage and -current dq0 rotor-system<br>
</pre>
<p>The model is valid for reference systems with arbitrary angular orientation theta[2].</p>
<p>Voltage of field-winding:</p>
<p>Machine- and excitation model must use the same value for the (unscaled) field voltage <tt>Vf_nom</tt>,<br>
as the machine model is stator-scaled with
<pre>  Vf_nom = V_nom</pre>
i.e. choose for both values <tt>V_nom</tt>.</p>
<p>The magnetic flux Psi_pm of the permanent magnet (if present) is defined by
<pre>
  Psi_pm = psi_pm*Psi_nom
  Psi_nom = V_nom/omega_nom = V_nom/(pp*w_nom)
</pre>
where <tt>psi_pm</tt> relates to the induced armature voltage <tt>v_op</tt> at open-terminal and <tt>omega_nom</tt> as
<pre>  psi_pm = v_op/V_nom</pre></p>
<p>The power angle is calculated if so desired. Note that for an inverter driven machine the power angle signal is oscillating with the source voltage.
<pre>
  powerAngle:   difference (angle of rotor) - (angle of terminal_voltage)
                (&gt 0: generator, &lt 0: motor)
</pre></p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,10},{100,-10}},
              lineColor={255,255,255},
              textString="syn3")}));
    end Synchron3rdBase;

    partial model SynchronBase "Synchronous machine base, 3-phase dq0"
      extends SynTransform(final pp=par.pp,v(start={cos(system.alpha0),sin(system.alpha0),0}*par.V_nom));

      replaceable record Data =
        PowerSystems.AC3ph.Machines.Parameters.Synchron(f_nom=system.f_nom)
        "machine parameters" annotation(choicesAllMatching=true);
      final parameter Data par "machine parameters"
        annotation (Placement(transformation(extent={{-60,60},{-40,80}})));
    protected
      final parameter Integer n_d = par.n_d "number of rotor circuits d-axis";
      final parameter Integer n_q = par.n_q "number of rotor circuits q-axis";
      final parameter Coefficients.Synchron c = Basic.Precalculation.machineSyn(par, top.scale);
      parameter SI.Inductance[n_d, n_d] L_rd = c.L_rd "L matrix rotor";
      parameter SI.Inductance[n_q, n_q] L_rq = c.L_rq "L matrix rotor";
      parameter SI.Inductance[n_d] L_md = c.L_md "L matrix mutual d-axis";
      parameter SI.Inductance[n_q] L_mq = c.L_mq "L matrix mutual q-axis";
      parameter SI.Resistance[n_d] R_rd = c.R_rd "R matrix rotor";
      parameter SI.Resistance[n_q] R_rq = c.R_rq "R matrix rotor";

      PS.Voltage[n_d] v_rd(start=ones(n_d))
        "rotor voltage d-axis in rotor-system";
      PS.Voltage[n_q] v_rq "rotor voltage q-axis in rotor-system";
      PS.Current[n_d] i_rd(each stateSelect=StateSelect.prefer)
        "rotor current d-axis in rotor-system";
      PS.Current[n_q] i_rq(each stateSelect=StateSelect.prefer)
        "rotor current q-axis in rotor-system";
      SI.MagneticFlux[2] psi_s "magnetic flux stator dq";
      SI.MagneticFlux[n_d] psi_rd "magnetic flux rotor d";
      SI.MagneticFlux[n_q] psi_rq "magnetic fluxrotor q";
      PS.Voltage v_f "field voltage (not scaled to stator units)";
      PS.Current i_f "field current (not scaled to stator units)";

    initial equation
      if dynType == Types.Dynamics.SteadyInitial then
        der(psi_s) = zeros(2);
        der(c.L_s[3]*i_s[3]) = 0;
        der(psi_rd) = zeros(n_d);
        der(psi_rq) = zeros(n_q);
      elseif dynType <> Types.Dynamics.SteadyState then
        der(psi_rd) = zeros(n_d);
        der(psi_rq) = zeros(n_q);
      end if;

    equation
      v_rd[2:end] = zeros(n_d - 1);
      v_rq = zeros(n_q);
      psi_s = diagonal(c.L_s[1:2])*i_s[1:2] + {L_md*i_rd + psi_e, L_mq*i_rq};
      psi_rd = L_md*i_s[1] + L_rd*i_rd;
      psi_rq = L_mq*i_s[2] + L_rq*i_rq;
      v_f = v_rd[1]/c.wf;
      i_f = i_rd[1]*c.wf;

      if dynType <> Types.Dynamics.SteadyState then
        der(psi_s) + w_el*{-psi_s[2], psi_s[1]} + c.R_s*i_s[1:2] = v_s[1:2];
        c.L_s[3]*der(i_s[3]) + c.R_s*i_s[3] = v_s[3];
        der(psi_rd) + diagonal(R_rd)*i_rd = v_rd;
        der(psi_rq) + diagonal(R_rq)*i_rq = v_rq;
      else
        w_el*{-psi_s[2], psi_s[1]} + c.R_s*i_s[1:2] = v_s[1:2];
        c.R_s*i_s[3] = v_s[3];
        diagonal(R_rd)*i_rd = v_rd;
        diagonal(R_rq)*i_rq = v_rq;
      end if;

      if par.neu_iso then
        i_n = zeros(top.n_n);
      else
        v_n = c.R_n*i_n "equation neutral to ground (relevant if Y-topology)";
      end if;

      tau_el = i_s[1:2]*{-psi_s[2], psi_s[1]};
      heat.ports.Q_flow = -{c.R_s*i_s*i_s, diagonal(R_rd)*i_rd*i_rd + diagonal(R_rq)*i_rq*i_rq};
      annotation (
        Documentation(
              info="<html>
<p>General model of synchronous machine.<br>
Stator: one winding each in d-axis, q-axis, o-axis.<br>
Rotor: n_d windings in d-axis (field f, (n_d-1) damper D1, ..), n_q windings in q-axis (damper Q1, ..).<br>
See also equivalent circuit on 'Diagram layer' of
<a href=\"modelica://PowerSystems.AC3ph.Machines.Parameters.Synchron\">Parameters.Synchron</a> !</p>
<pre>
  v, i:          stator-voltage and -current dq0 reference-system
  v_s, i_s:      stator-voltage and -current dq0 rotor-system
  v_rd, i_rd:    rotor-voltage and -current d-axis rotor-system
  v_rq, i_rq:    rotor-voltage and -current q-axis rotor-system
</pre>
<p>The model is valid for reference systems with arbitrary angular orientation theta[2].</p>
<p>Voltage of field-winding:</p>
<p>Machine- and excitation model should use the same value for the (unscaled) field voltage <tt>Vf_nom</tt>.<br>
This value is calculated from <tt>If_nom</tt>, defined through <tt>V=V_nom</tt> at open terminal.</p>
<p>As the machine model is stator-scaled, the default values
<pre>
  If_nom = I_nom = S_nom/V_nom
  Vf_nom = V_nom
</pre>
are sufficient when unscaled values for the field-winding are not of interest.</p>
<p>The magnetic flux Psi_pm of the permanent magnet (if present) is defined by
<pre>
  Psi_pm = psi_pm*Psi_nom
  Psi_nom = V_nom/omega_nom = V_nom/(pp*w_nom)
</pre>
where <tt>psi_pm</tt> relates to the induced armature voltage <tt>v_op</tt> at open-terminal and <tt>omega_nom</tt> as
<pre>  psi_pm = v_op/V_nom</pre></p>
<p>The power angle is calculated if so desired. Note that for an inverter driven machine the power angle signal is oscillating with the source voltage.
<pre>
  powerAngle:   difference (angle of rotor) - (angle of terminal_voltage)
                (&gt 0: generator, &lt 0: motor)
</pre></p>
</html>"),
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={Text(
              extent={{-100,10},{100,-10}},
              lineColor={255,255,255},
              textString="syn")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-50,-58},{-30,-62}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-58},{30,-62}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-50,-74},{-30,-78}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-74},{30,-78}},
              lineColor={0,0,255},
              lineThickness=0.5,
              fillColor={0,0,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-66},{30,-68}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-30,-29},{30,-31}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid),
            Line(points={{-50,-60},{-60,-60},{-60,-54},{40,-54},{40,-60},{30,-60}},
                color={0,0,255}),
            Line(points={{-50,-76},{-60,-76},{-60,-70},{40,-70},{40,-76},{30,-76}},
                color={0,0,255}),
            Rectangle(
              extent={{-30,-82},{30,-84}},
              lineColor={175,175,175},
              fillColor={175,175,175},
              fillPattern=FillPattern.Solid)}));
    end SynchronBase;

  end Partials;

  package Control
    extends Modelica.Icons.Package;

  model Excitation "Excitation (electric part)"
    extends Ports.Port_p;

    parameter PS.Voltage V_nom=1 "nom voltage armature"
      annotation(Evaluate=true, Dialog(group="Nominal"));
    parameter PS.Voltage Vf_nom=1 "nom voltage field-winding"
      annotation(Evaluate=true, Dialog(group="Nominal"));

    Modelica.Blocks.Interfaces.RealOutput[3] termVoltage(each final unit="1")
        "terminal voltage pu to exciter control"
      annotation (Placement(transformation(
            origin={-60,100},
            extent={{-10,-10},{10,10}},
            rotation=90)));
    Modelica.Blocks.Interfaces.RealInput fieldVoltage(final unit="1")
        "field voltage pu from exciter control"
      annotation (Placement(transformation(
            origin={60,100},
            extent={{-10,-10},{10,10}},
            rotation=270)));
    AC1ph_DC.Ports.TwoPin_n field "to generator field-winding"
      annotation (Placement(transformation(extent={{-90,-50},{-110,-30}})));

  equation
    term.i = zeros(3);
    termVoltage = term.v/V_nom;
    field.v = {fieldVoltage*Vf_nom, 0};
  annotation (defaultComponentName = "excitation",
    Documentation(
            info="<html>
<p>This is a default model. The excitation-voltage is directly determined by the pu field-voltage control-signal.<br>
It does not contain any electronic component.</p>
</html>
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Text(
              extent={{-100,30},{100,10}},
              lineColor={0,0,127},
              textString="torque"),
            Text(
              extent={{-100,-10},{100,-30}},
              lineColor={0,0,127},
              textString="gen"),
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,120,120},
              fillColor={215,215,215},
              fillPattern=FillPattern.Solid),
            Text(
              extent={{-100,30},{100,10}},
              lineColor={0,0,255},
              textString="field"),
            Text(
              extent={{-100,-10},{100,-30}},
              lineColor={0,0,255},
              textString="voltage"),
            Line(points={{-80,-40},{40,-40}}, color={0,0,255})}));
  end Excitation;

  model PowerAngle "Direct determination of generator power angle"
    extends Ports.Port_p;
    outer System system;

    parameter Integer pp=1 "pole-pair number";
    parameter SI.Angle delta=0 "power angle";
    parameter SI.AngularFrequency gamma=1 "inverse time constant";
    SI.Angle phi_el(start=delta-pi/2+system.alpha0) "rotor angle";
    Interfaces.Rotation_n airgap(     phi(start=(delta-pi/2+system.alpha0)/pp))
        "to airgap of generator"
    annotation (Placement(transformation(extent={{90,50},{110,70}})));
    protected
    function atan2 = Modelica.Math.atan2;

  equation
    term.i = zeros(3);
    pp*airgap.phi = phi_el;

    phi_el = delta + atan2(-term.v[1], term.v[2]) + term.theta[2]; // steady state!
  //  der(phi_el) = gamma*(delta + atan2(-term.v[1], term.v[2]) + term.theta[2] - phi_el);
  annotation (defaultComponentName = "powerAngle",
    Documentation(
            info="<html>
<p>
Auxiliary control device for test purposes.<br>
Generator rotates at given power angle delta. Replaces turbine and generator-rotor (mechanical part).</p>
<p>Connector 'airgap' to be connected to 'generator.airgap'.<br>
Connector 'term' to be connected to 'generator.term'.</p>
<p>Note: initial equations in synchronous machine must be omitted.</p>
</html>
"), Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2}), graphics={
            Rectangle(
              extent={{-80,60},{80,-60}},
              lineColor={0,0,127},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),
            Polygon(
              points={{-20,0},{12,0},{8,14},{-20,0}},
              lineColor={255,170,170},
              pattern=LinePattern.None,
              fillColor={255,170,170},
              fillPattern=FillPattern.Solid),
            Line(points={{80,0},{-20,0},{60,40}}, color={95,95,95})}));
  end PowerAngle;

    annotation (Documentation(info="<html>
txt
</html>"));
  end Control;

package Parameters "Parameter data for interactive use"

  extends Modelica.Icons.MaterialPropertiesPackage;

  record Asynchron3rd "Asynchronous machine 3rd order parameters"
    extends PowerSystems.Basic.Nominal.NominalDataAC;

    Boolean neu_iso "isolated neutral if Y" annotation(Dialog);
    Integer pp "pole-pair number" annotation(Dialog);

    SIpu.Reactance x "total reactance d- and q-axis" annotation(Dialog);
    SIpu.Reactance x_o "reactance o-axis" annotation(Dialog);
    SIpu.Resistance r_s "resistance stator" annotation(Dialog);
    SIpu.Resistance r_n "resistance neutral to grd (if Y)" annotation(Dialog(enable=not neu_iso));

    annotation (defaultComponentName="asyn3rdPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
</html>"),
      Diagram(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}},
              grid={2,2}), graphics={
              Line(points={{-40,80},{-40,10}}, color={0,0,255}),
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
                     "xm = x - xsig_s"),
              Text(
                extent={{-86,46},{-46,40}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                     "xm = x - xsig_s"),
              Rectangle(
                extent={{-42,60},{-38,30}},
                lineColor={0,0,255},
                fillColor={0,0,255},
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
              Line(points={{-80,-20},{-40,-20}}, color={0,0,255}),
              Line(points={{-100,10},{-40,10}}, color={0,0,255}),
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
                fillPattern=FillPattern.Solid),
              Line(points={{-100,-90},{-40,-90}}, color={0,0,255}),
              Line(points={{-100,80},{-40,80}}, color={0,0,255}),
              Rectangle(
                extent={{-100,82},{-80,78}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-70,82},{-50,78}},
                lineColor={0,0,255},
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid)}));
  end Asynchron3rd;

  record Asynchron "Asynchronous machine parameters"
    extends Asynchron3rd;

    parameter Integer n_r(min=0) "# rotor circuits d- and q-axis";
    Boolean transDat "use transient data"  annotation(Dialog, choices(
      choice=true "transient data",
      choice=false "equivalent circuit data"));
    Boolean use_xtr "use x_transient and t_closed"
      annotation(Dialog(enable=transDat), choices(
      choice=true "x_tr and t_closed",
      choice=false "t_closed and t_open"));
    SIpu.Reactance[n_r] xtr "transient reactance {xtr', xtr'', ..}"
      annotation(Dialog(enable=transDat and use_xtr));
    SI.Time[n_r] tc "time constant closed-loop {tc', tc'', ..}"
      annotation(Dialog(enable=transDat));
    SI.Time[n_r] to "time constant open-loop {to', to'', ..}"
      annotation(Dialog(enable=transDat and not use_xtr));

    SIpu.Reactance xsig_s "leakage reactance stator" annotation(Dialog);
    SIpu.Reactance[n_r] xsig_r "leakage reactance rotor"
      annotation(Dialog(enable=not transDat));
    SIpu.Resistance[n_r] r_r "resistance rotor"
      annotation(Dialog(enable=not transDat));

    annotation (defaultComponentName="asynPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Specifying standard transient data, an example:</p>
<p>&nbsp; - for first order per axis write</p>
<pre>
  xtr = {0.4}  for  xtr' = 0.4,  no xtr'', ..
  tc  = {1.3}  for   tc' = 1.3,   no tc'', ..
</pre>
<p>&nbsp; - for second order per axis write</p>
<pre>
  xtr = {0.4, 0.24}  for  xtr' = 0.4,  xtr'' = 0.24
  tc  = {1.3, 0.04}  for   tc' = 1.3,   tc'' = 0.04
</pre>
<p>and analogous for higher order.</p>
<p>Specifying equivalent circuit data:</p>
<p>&nbsp; &nbsp; <tt>xsig_r, r_r</tt> correspond to a stator-based equivalent circuit.<br>
&nbsp; &nbsp; The number of components of <tt>xsig_r, r_r</tt> depends on the order of the model.<br>
&nbsp; &nbsp; For pu-input refer to stator base value <tt>R_base</tt>.</p>
</html>"),
      Diagram(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}},
              grid={2,2}), graphics={
              Line(points={{50,80},{50,10}}, color={0,0,255}),
              Line(points={{90,50},{90,40}}, color={0,0,255}),
              Line(points={{-40,10},{90,10},{90,20}}, color={0,0,255}),
              Line(points={{50,-20},{50,-90}}, color={0,0,255}),
              Line(points={{-40,-90},{90,-90},{90,-80}}, color={0,0,255}),
              Line(points={{90,-50},{90,-60}}, color={0,0,255}),
              Line(points={{-40,80},{90,80},{90,70}}, color={0,0,255}),
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
                   "xsig_r2"),
              Text(
                extent={{66,66},{86,60}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "xsig_r1"),
              Text(
                extent={{28,32},{46,26}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "r_r2"),
              Text(
                extent={{68,32},{86,26}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "r_r1"),
              Text(
                extent={{28,-68},{46,-74}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "r_r2"),
              Text(
                extent={{68,-68},{86,-74}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "r_r1"),
              Text(
                extent={{26,-34},{46,-40}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "xsig_r2"),
              Text(
                extent={{66,-34},{86,-40}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                   "xsig_r1"),
              Line(points={{-40,-20},{90,-20},{90,-30}}, color={0,0,255})}));
  end Asynchron;

  record Asynchron3rd_cage "Asynchronous machine 3rd order parameters"
    extends Asynchron3rd(
      neu_iso=false,
      pp=1,
      x=4,
      x_o=0.1,
      r_s=0.04,
      r_n=1);

    annotation (defaultComponentName="asyn3rdPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
</html>"));
  end Asynchron3rd_cage;

  record Asynchron_cage "Asynchronous machine parameters"
    extends Asynchron(
      neu_iso=false,
      pp=1,
      x=4,
      x_o=0.1,
      r_s=0.04,
      r_n=1,
      n_r=1,
      transDat=true,
      use_xtr=true,
      xtr={0.4844},
      tc={0.03212},
      to={0.2653},
      xsig_s=0.25,
      xsig_r={0.25},
      r_r={0.04});

    annotation (defaultComponentName="asynPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Specifying standard transient data, an example:</p>
<p>&nbsp; - for first order per axis write</p>
<pre>
  xtr = {0.4}  for  xtr' = 0.4,  no xtr'', ..
  tc  = {1.3}  for   tc' = 1.3,   no tc'', ..
</pre>
<p>&nbsp; - for second order per axis write</p>
<pre>
  xtr = {0.4, 0.24}  for  xtr' = 0.4,  xtr'' = 0.24
  tc  = {1.3, 0.04}  for   tc' = 1.3,   tc'' = 0.04
</pre>
<p>and analogous for higher order.</p>
<p>Specifying equivalent circuit data:</p>
<p>&nbsp; &nbsp; <tt>xsig_r, r_r</tt> correspond to a stator-based equivalent circuit.<br>
&nbsp; &nbsp; The number of components of <tt>xsig_r, r_r</tt> depends on the order of the model.<br>
&nbsp; &nbsp; For pu-input refer to stator base value <tt>R_base</tt>.</p>
</html>"));
  end Asynchron_cage;

  record Synchron3rd "Synchronous machine 3rd order parameters"
    extends PowerSystems.Basic.Nominal.NominalDataAC;

    Boolean neu_iso "isolated neutral if Y" annotation(Dialog);
    Integer pp "pole-pair number" annotation(Dialog);
    Integer excite(min=0,max=3) "excitation (1:el, 2:pm, 3:reluctance)"
      annotation(Evaluate=true, Dialog, choices(
      choice=1 "electric excitation",
      choice=2 "permanent magnet",
      choice=3 "reluctance machine"));
    SIpu.MagneticFlux psi_pm
        "magnetisation (V/V_nom at open term at omega_nom)"
      annotation(Dialog(enable=excite==2));

    SIpu.Reactance x_d "syn reactance d-axis" annotation(Dialog);
    SIpu.Reactance x_q "syn reactance q-axis" annotation(Dialog);
    SIpu.Reactance x_o "reactance o-axis" annotation(Dialog);
    SIpu.Resistance r_s "resistance armature" annotation(Dialog);
    SIpu.Resistance r_n "resistance neutral to grd (if Y)" annotation(Dialog(enable=not neu_iso));

    annotation (defaultComponentName="syn3rdPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>This simplified model uses only main but no transient reactances.</p>
</html>"),
      Diagram(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}},
              grid={2,2}), graphics={
              Line(points={{-40,80},{-40,10}}, color={0,0,255}),
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
              Rectangle(
                extent={{-42,60},{-38,30}},
                lineColor={0,0,255},
                fillColor={0,0,255},
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
              Line(points={{-80,-20},{-40,-20}}, color={0,0,255}),
              Line(points={{-100,10},{-40,10}}, color={0,0,255}),
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
                fillPattern=FillPattern.Solid),
              Line(points={{-100,-90},{-40,-90}}, color={0,0,255}),
              Line(points={{-100,80},{-40,80}}, color={0,0,255}),
              Rectangle(
                extent={{-100,82},{-80,78}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-70,82},{-50,78}},
                lineColor={0,0,255},
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid)}));
  end Synchron3rd;

  record Synchron "Synchronous machine parameters"
    extends Synchron3rd;

    parameter Integer n_d(min=1) "# rotor circuits d-axis";
    parameter Integer n_q(min=0) "# rotor circuits q-axis";

    Boolean transDat "use transient data?" annotation(Dialog, choices(
      choice=true "transient data",
      choice=false "equivalent circuit data"));
    Boolean use_xtr "use x_transient and t_closed?"
      annotation(Dialog(enable=transDat), choices(
      choice=true "x_tr and t_closed",
      choice=false "t_closed and t_open"));
    SIpu.Reactance[n_d] xtr_d "trans reactance d-axis {xtr_d', xtr_d'', ..}"
      annotation(Dialog(enable=transDat and use_xtr));
    SIpu.Reactance[n_q] xtr_q "trans reactance q-axis {xtr_q', xtr_q'', ..}"
      annotation(Dialog(enable=transDat and use_xtr));
    SI.Time[n_d] tc_d "time constant closed-loop d-axis {tc_d', tc_d'', ..}"
      annotation(Dialog(enable=transDat));
    SI.Time[n_q] tc_q "time constant closed-loop q-axis {tc_q', tc_q'', ..}"
      annotation(Dialog(enable=transDat));
    SI.Time[n_d] to_d "time constant open-loop d-axis {to_d', to_d'', ..}"
      annotation(Dialog(enable=transDat and not use_xtr));
    SI.Time[n_q] to_q "time constant open-loop q-axis {to_q', to_q'', ..}"
      annotation(Dialog(enable=transDat and not use_xtr));
    Boolean use_if0 "induced field current and phase available?"
      annotation(Dialog(enable=transDat and size(tc_d,1)>1 and not pm_exc), choices(
      choice=true "d-axis with xm_d",
      choice=false "d-axis omitting xm_d"));
    SIpu.Current if0 "induced field current at v_s=Vnom/0deg"
     annotation(Dialog(enable=transDat and size(tc_d,1)>1 and use_if0 and not pm_exc));
    SI.Angle alpha_if0 "angle(if0) at v_s=Vnom/0deg (sign: i_f behind v_s)"
      annotation(Dialog(enable=transDat and size(tc_d,1)>1 and use_if0 and not pm_exc));
    Real tol "tolerance precalculation"
      annotation(Dialog(enable=transDat and size(tc_d,1)>1 and use_if0 and not pm_exc));

    SIpu.Reactance xsig_s "leakage reactance armature" annotation(Dialog);
    SIpu.Reactance[n_d] xsig_rd "leakage reactance rotor d-axis {f, D, ..}"
      annotation(Dialog(enable=not transDat));
    SIpu.Reactance[n_q] xsig_rq "leakage reactance rotor q-axis {Q1, ..}"
      annotation(Dialog(enable=not transDat));
    SIpu.Reactance[n_d-1] xm_d "coupling-reactance d-axis {xm1, ..}"
      annotation(Dialog(enable=not transDat));
    SIpu.Resistance[n_d] r_rd "resistance rotor d-axis {f, D, ..}"
      annotation(Dialog(enable=not transDat));
    SIpu.Resistance[n_q] r_rq "resistance rotor q-axis {Q1, ..}"
      annotation(Dialog(enable=not transDat));

    SI.Current If_nom "nom field current (V=V_nom at open term)"
      annotation(Dialog(group="Nominal", enable=not pm_exc));

    annotation (defaultComponentName="synPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Specifying standard transient data both for _d and _q axis:</p>
<p>&nbsp; - for first order write</p>
<pre>
  xtr = {0.4}   for  xtr'  = 0.4,  no xtr''
  tc  = {1.3}   for   tc'  = 1.3,   no tc''
  and
  xtr = {0.26}  for  xtr'' = 0.26,  no xtr'
  tc  = {0.06}  for   tc'' = 0.06,   no tc'
</pre>
<p>&nbsp; - for second order write</p>
<pre>
  xtr = {0.4, 0.24}  for  xtr' = 0.4, xtr'' = 0.24
  tc  = {1.3, 0.04}  for   tc' = 1.3,  tc'' = 0.04
</pre>
<p>and analogous for higher order.</p>
<p>Sign of field current i_f:<br>
Mathematical conventions (Z-matrix) are used for formulas in package 'Precalculation'.<br>
Experimental conventions (if0_deg) choose the inverse sign for the field-current.<br>
Therefore we have to use the following definition for the phase-angle of i_f:
<pre>  alpha_if0 = (if0_deg + 180)*pi/180</pre></p>
<p>If the induced field-current values are not available and for pm-excitation the d-axis is treated according to the q-axis scheme (without xm_d).</p>
<p>Specifying equivalent circuit data:</p>
<p>&nbsp; &nbsp; <tt>xsig_f, r_f, xsig_Q, r_Q</tt> correspond to a stator-based equivalent circuit.<br>
&nbsp; &nbsp; The number of components of <tt>xsig_r, r_r</tt> depends on the order of the model.<br>
&nbsp; &nbsp; For pu-input refer to stator base value <tt>R_base</tt>.</p>
<p>Relation rotor resistance of field winding to stator-based equivalent circuit data:</p>
<pre>
  If_base = (x_d - xsig_s)*If_nom, (x_d, xsig_s in pu)
  Rf_base = P_nom/If_base^2
  rf =  Rf/Rf_base                (in pu, stator-based).
  rf = (Rf/Rf_base)*R_base        (in SI, stator-based).
  Rf = resistance field winding   (in Ohm, true value, not scaled)
</pre>
</html>"),
      Diagram(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}},
              grid={2,2}), graphics={
              Line(points={{10,80},{10,10}}, color={0,0,255}),
              Line(points={{50,80},{50,10}}, color={0,0,255}),
              Line(points={{90,50},{90,40}}, color={0,0,255}),
              Line(points={{-40,10},{90,10},{90,20}}, color={0,0,255}),
              Line(points={{50,-20},{50,-90}}, color={0,0,255}),
              Line(points={{10,-20},{10,-90}}, color={0,0,255}),
              Line(points={{-40,-90},{90,-90},{90,-80}}, color={0,0,255}),
              Line(points={{-40,-20},{90,-20},{90,-30}}, color={0,0,255}),
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
              Ellipse(extent={{64,16},{76,4}}, lineColor={0,0,255}),
              Text(
                extent={{78,6},{92,0}},
                lineColor={0,0,255},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString=
                     "v_f / pm"),
              Line(points={{-40,80},{90,80},{90,70}}, color={0,0,255}),
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
  end Synchron;

  record Synchron3rd_ee "Synchronous machine 3rd order parameters"
    extends Synchron3rd(
      neu_iso=false,
      pp=1,
      final excite=1,
      final psi_pm=0,
      x_d=1.9,
      x_q=1.77,
      x_o=0.1,
      r_s=0.005,
      r_n=1);

    annotation (defaultComponentName="syn3rd_elPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>This simplified model uses only main but no transient reactances.</p>
</html>"));
  end Synchron3rd_ee;

  record Synchron_ee "Synchronous machine parameters"
    extends Synchron(
      neu_iso=false,
      pp=1,
      final excite=1,
      final psi_pm=0,
      x_d=1.9,
      x_q=1.77,
      x_o=0.1,
      r_s=0.005,
      r_n=1,
      n_d=2,
      n_q=2,
      transDat=true,
      use_xtr=true,
      xtr_d={0.33,0.25},
      xtr_q={0.44,0.27},
      tc_d={0.86, 0.025},
      tc_q={0.25, 0.04},
      to_d={4.9898, 0.032747},
      to_q={1.0867, 0.060327},
      use_if0=true,
      if0=0.85,
      alpha_if0=-1.7453292519943,
      tol=1e-6,
      xsig_s=0.17,
      xsig_rd={0.135194,0.0365214},
      xsig_rq={0.407386,0.144502},
      xm_d={0.0555125},
      r_rd={1.32139e-3, 14.376e-3},
      r_rq={7.38411e-3, 19.7148e-3},
      If_nom=1);

  //  example: V_nom=20e3, S_nom=500e6, If_nom=1500.
    annotation (defaultComponentName="syn_elPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Specifying standard transient data both for _d and _q axis:</p>
<p>&nbsp; - for first order write</p>
<pre>
  xtr = {0.4}   for  xtr'  = 0.4,  no xtr''
  tc  = {1.3}   for   tc'  = 1.3,   no tc''
  and
  xtr = {0.26}  for  xtr'' = 0.26,  no xtr'
  tc  = {0.06}  for   tc'' = 0.06,   no tc'
</pre>
<p>&nbsp; - for second order write</p>
<pre>
  xtr = {0.4, 0.24}  for  xtr' = 0.4, xtr'' = 0.24
  tc  = {1.3, 0.04}  for   tc' = 1.3,  tc'' = 0.04
</pre>
<p>and analogous for higher order.</p>
<p>Sign of field current i_f:<br>
Mathematical conventions (Z-matrix) are used for formulas in package 'Precalculation'.<br>
Experimental conventions (if0_deg) choose the inverse sign for the field-current.<br>
Therefore we have to use the following definition for the phase-angle of i_f:
<pre>  alpha_if0 = (if0_deg + 180)*pi/180</pre></p>
<p>If the induced field-current values are not available and for pm-excitation the d-axis is treated according to the q-axis scheme (without xm_d).</p>
<p>Specifying equivalent circuit data:</p>
<p>&nbsp; &nbsp; <tt>xsig_f, r_f, xsig_Q, r_Q</tt> correspond to a stator-based equivalent circuit.<br>
&nbsp; &nbsp; The number of components of <tt>xsig_r, r_r</tt> depends on the order of the model.<br>
&nbsp; &nbsp; For pu-input refer to stator base value <tt>R_base</tt>.</p>
<p>Relation rotor resistance of field winding to stator-based equivalent circuit data:</p>
<pre>
  If_base = (x_d - xsig_s)*If_nom, (x_d, xsig_s in pu)
  Rf_base = P_nom/If_base^2
  rf =  Rf/Rf_base                (in pu, stator-based).
  rf = (Rf/Rf_base)*R_base        (in SI, stator-based).
  Rf = resistance field winding   (in Ohm, true value, not scaled)
</pre>
</html>"));
  end Synchron_ee;

  record Synchron3rd_pm "Synchronous machine pm 3rd order parameters"
    extends Synchron3rd(
      neu_iso=false,
      pp=2,
      final excite=2,
      psi_pm=1.2,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.05,
      r_n=1);
    annotation (defaultComponentName="syn3rd_pmPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>The relation between 'flux induced by permanent magnet' <tt>Psi_pm [Wb]</tt> and 'magnetisation' <tt>psi_pm [pu]</tt> is given by the following relation;
<pre>
  Psi_pm = psi_pm*V_nom/omega_nom
  psi_pm = Psi_pm*omega_nom/V_nom
</pre></p>
</html>
"));
  end Synchron3rd_pm;

  record Synchron_pm "Synchronous machine pm parameters"
    extends Synchron(
      neu_iso=false,
      pp=2,
      final excite=2,
      psi_pm=1.2,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.05,
      r_n=1,
      n_d=1,
      n_q=1,
      transDat=true,
      use_xtr=true,
      xtr_d={0.142857},
      xtr_q={0.142857},
      tc_d={0.00994718},
      tc_q={0.00994718},
      to_d={0.0278521},
      to_q={0.0278521},
      use_if0=false,
      if0=0,
      alpha_if0=0,
      tol=1e-6,
      xsig_s=0.1,
      xsig_rd={0.05},
      xsig_rq={0.05},
      xm_d=fill(0, 0),
      r_rd={0.04},
      r_rq={0.04},
      If_nom=0);

    annotation (defaultComponentName="syn_pmPar",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
<p>Specifying standard transient data both for _d and _q axis:</p>
<p>&nbsp; - for first order write</p>
<pre>
  xtr = {0.4}   for  xtr'  = 0.4,  no xtr''
  tc  = {1.3}   for   tc'  = 1.3,   no tc''
  and
  xtr = {0.26}  for  xtr'' = 0.26,  no xtr'
  tc  = {0.06}  for   tc'' = 0.06,   no tc'
</pre>
<p>&nbsp; - for second order write</p>
<pre>
  xtr = {0.4, 0.24}  for  xtr' = 0.4, xtr'' = 0.24
  tc  = {1.3, 0.04}  for   tc' = 1.3,  tc'' = 0.04
</pre>
<p>and analogous for higher order.</p>
<p>Sign of field current i_f:<br>
Mathematical conventions (Z-matrix) are used for formulas in package 'Precalculation'.<br>
Experimental conventions (if0_deg) choose the inverse sign for the field-current.<br>
Therefore we have to use the following definition for the phase-angle of i_f:
<pre>  alpha_if0 = (if0_deg + 180)*pi/180</pre></p>
<p>If the induced field-current values are not available and for pm-excitation the d-axis is treated according to the q-axis scheme (without xm_d).</p>
<p>Specifying equivalent circuit data:</p>
<p>&nbsp; &nbsp; <tt>xsig_f, r_f, xsig_Q, r_Q</tt> correspond to a stator-based equivalent circuit.<br>
&nbsp; &nbsp; The number of components of <tt>xsig_r, r_r</tt> depends on the order of the model.<br>
&nbsp; &nbsp; For pu-input refer to stator base value <tt>R_base</tt>.</p>
<p>The relation between 'flux induced by permanent magnet' <tt>Psi_pm [Wb]</tt> and 'magnetisation' <tt>psi_pm [pu]</tt> is given by the following relation;
<pre>
  Psi_pm = psi_pm*V_nom/omega_nom
  psi_pm = Psi_pm*omega_nom/V_nom
</pre></p>
</html>"));
  end Synchron_pm;

  record Synchron3rd_reluctance "Synchronous machine pm 3rd order parameters"
    extends Synchron3rd(
      neu_iso=false,
      pp=2,
      final excite=3,
      final psi_pm=0,
      x_d=2.0,
      x_q=0.6,
      x_o=0.1,
      r_s=0.05,
      r_n=1);
    annotation (defaultComponentName="syn_reluctPar",
      defaultComponentPrefixes="parameter",
      Documentation(info="<html>
<p>Equivalent circuit on <b>diagram layer</b>!</p>
</html>"));
  end Synchron3rd_reluctance;

  annotation (preferredView="info",
    Documentation(info="<html>
<p>Records containing parameters of the corresponding components.</p>
</html>"));
end Parameters;

package Coefficients "Coefficient matrices of machine equations"
  extends Modelica.Icons.MaterialPropertiesPackage;

record Asynchron "Coefficient matrices of asynchronous machine"
  extends Modelica.Icons.Record;

  parameter Integer n_r "number of rotor circuits";
  SI.Inductance[3] L_s "L matrix stator dq0, d=q";
  SI.Inductance[n_r, n_r] L_r "L matrix rotor";
  SI.Inductance[n_r] L_m "L matrix mutual";
  SI.Resistance R_s "R matrix stator";
  SI.Resistance[n_r] R_r "R matrix rotor";
  SI.Resistance R_n "resistance neutral to grd (if Y)";
  SI.Resistance[n_r] R_m "= diagonal(R_r)*inv(L_r)*L_m";

  annotation (defaultComponentPrefixes="final parameter",
    Documentation(info="<html>
</html>"));
end Asynchron;

record Synchron3rd "Coefficient matrices of synchronous machine, 3rd order"
  extends Modelica.Icons.Record;

  SI.Inductance[3] L_s "L matrix stator dq0";
  SI.Resistance R_s "R stator (armature)";
  SI.Resistance R_n "resistance neutral to grd (if Y)";
  SI.MagneticFlux Psi_pm "flux permanent magnet";
  SI.AngularFrequency omega_nom;

  annotation (defaultComponentPrefixes="final parameter",
    Documentation(info="<html>
</html>"));
end Synchron3rd;

record Synchron "Coefficient matrices of synchronous machine"
  extends Modelica.Icons.Record;

  parameter Integer n_d "number of rotor circuits d-axis";
  parameter Integer n_q "number of rotor circuits q-axis";
  SI.Inductance[3] L_s "L matrix stator dq0";
  SI.Inductance[n_d, n_d] L_rd "L matrix rotor";
  SI.Inductance[n_q, n_q] L_rq "L matrix rotor";
  SI.Inductance[n_d] L_md "L matrix mutual d-axis";
  SI.Inductance[n_q] L_mq "L matrix mutual q-axis";
  SI.Resistance R_s "R stator (armature)";
  SI.Resistance[n_d] R_rd "R matrix rotor";
  SI.Resistance[n_q] R_rq "R matrix rotor";
  SI.Resistance R_n "resistance neutral to grd (if Y)";
  SI.MagneticFlux Psi_pm "flux permanent magnet";
  Real wf "ratio field winding";
  SI.Voltage Vf_nom "nom voltage field winding";
  SI.AngularFrequency omega_nom;

  annotation (defaultComponentPrefixes="final parameter",
    Documentation(info="<html>
</html>"));
end Synchron;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Records containing the result of precalculation, and used in the dynamical equations of the corresponding components.</p>
</html>"));
end Coefficients;

annotation (preferredView="info",
  Documentation(info="<html>
<p> This package contains the <b>electrical part</b> (electrical equations) of AC synchronous and asynchronous machines (generators or motors).<br>
Complete drives or generators are found in package Drives or Generation.</p>
<p>The models in this package can be used both for Y- and for Delta-topology, if the impedance parameters are defined 'as seen from the terminals', directly relating terminal voltage and terminal current.</p>
<p>

.</p>
</html>"));
end Machines;
