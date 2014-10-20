within PowerSystems.Blocks;
package Transforms "Auxiliary blocks"
  extends Modelica.Icons.VariantsPackage;

  block Park "Park-transform of input signal-vector"
    extends Partials.MIMO(final nin=3, final nout=3);

    Modelica.Blocks.Interfaces.RealInput theta "transformation angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
    constant Real s13=sqrt(1/3);
    constant Real s23=sqrt(2/3);
    constant Real dph_b=2*pi/3;
    constant Real dph_c=4*pi/3;
    Real[3] c;
    Real[3] s;

  equation
    c =  cos({theta, theta - dph_b, theta - dph_c});
    s =  sin({theta, theta - dph_b, theta - dph_c});
    y = transpose([s23*c, -s23*s, {s13, s13, s13}])*u;
    annotation (defaultComponentName = "park",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,40},{80,-40}},
            lineColor={128,128,128},
            textString=
                 "park")}),
      Documentation(info="<html>
<p>The block <tt>Park</tt> transforms abc variables (u) into dq0 variables (y) with arbitrary angular orientation
<pre>  y = P*u</pre>
<tt>P</tt> can be factorised into a constant, angle independent orthogonal matrix <tt>P0</tt> and an angle-dependent rotation <tt>R</tt></p>
<pre>
  P(theta) = R'(theta)*P0
</pre>
<p>Using the definition</p>
<pre>
  c_k = cos(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
  s_k = sin(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
</pre>
<p>it takes the form
<pre>
                       [ c_0,  c_1, c_2]
  P(theta) = sqrt(2/3)*[-s_0, -s_1,-s_2]
                       [ w,    w,   w  ]
</pre>
with
<pre>
                        [ 1,      -1/2,       -1/2]
  P0 = P(0) = sqrt(2/3)*[ 0, sqrt(3)/2, -sqrt(3)/2]
                        [ w,         w,          w]
</pre>
and
<pre>
             [c_0, -s_0, 0]
  R(theta) = [s_0,  c_0, 0]
             [  0,  0,   1]
</pre></p>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Park;

  block Rotation_dq "Rotation of input signal-vector"
    extends Partials.MIMO(final nin=2, final nout=2);

    Modelica.Blocks.Interfaces.RealInput theta "rotation angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
    Real c;
    Real s;

  equation
    c =   cos(theta);
    s =   sin(theta);
    y = [c, -s; s, c]*u;
    annotation (defaultComponentName = "rot_dq",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-75,40},{75,-40}},
            lineColor={128,128,128},
            textString=
                 "rot_dq")}),
      Documentation(info="<html>
<p>The block <tt>Rotation_dq</tt> rotates u by an arbitrary angle <tt>theta</tt> into y according to
<pre>  y = R_dq*u</pre>
<tt>R_dq</tt> is the restriction of <tt>R_dq0</tt> from dq0 to dq.</p>
<p>The matrix <tt>R_dq0</tt> rotates dq0 variables around the o-axis in dq0-space with arbitrary angle <tt>theta</tt>.
<p>It takes the form
<pre>
                 [cos(theta), -sin(theta), 0]
  R_dq0(theta) = [sin(theta),  cos(theta), 0]
                 [  0,           0,        1]
</pre>
and has the real eigenvector
<pre>  {0, 0, 1}</pre>
in the dq0 reference-frame.</p>
<p>Coefficient matrices of the form (symmetrical systems)
<pre>
      [x, 0, 0 ]
  X = [0, x, 0 ]
      [0, 0, xo]
</pre>
are invariant under transformations R_dq0</p>
<p>The connection between R_dq0 and R_abc is the following
<pre>  R_dq0 = P0*R_abc*P0'.</pre>
with P0 the orthogonal transform 'Transforms.P0'.</p>
</html>
"),   Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end Rotation_dq;

  block Rotation_abc "Rotation of input signal-vector"
    extends Partials.MIMO(final nin=3, final nout=3);

    Modelica.Blocks.Interfaces.RealInput theta "rotation angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
    constant Real q13=1/3;
    constant Real q23=2/3;
    constant Real dph_b=2*pi/3;
    constant Real dph_c=4*pi/3;
    Real[3] g;

  equation
    g =  {q13, q13, q13} + q23*cos({theta, theta - dph_b, theta - dph_c});
    y =  [g[{1,2,3}], g[{3,1,2}], g[{2,3,1}]]*u;
    annotation (defaultComponentName = "rot_abc",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-75,40},{75,-40}},
            lineColor={128,128,128},
            textString=
                 "rot_abc")}),
      Documentation(info="<html>
<p>The block <tt>Rotation_abc</tt> rotates u by an arbitrary angle <tt>theta</tt> into y according to
<pre>  y = R_abc*u</pre>
<p>The matrix <tt>R_abc</tt> rotates abc variables around the {1,1,1}-axis in abc-space with arbitrary angle <tt>theta</tt>.
<p>Using the definition
<pre>
  g_k = 1/3 + (2/3)*cos(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
</pre>
it takes the form
<pre>
                 [g_0, g_2, g_1]
  R_abc(theta) = [g_1, g_0, g_2]
                 [g_2, g_1, g_0]
</pre>
and has the real eigenvector
<pre>  {1, 1, 1}/sqrt(3)</pre>
in the abc reference-frame.</p>
<p>Coefficient matrices of the form (symmetrical systems)
<pre>
      [x,  xm, xm]
  X = [xm,  x, xm]
      [xm, xm, x ]
</pre>
are invariant under transformations R_abc</p>
<p>The connection between R_abc and R_dq0 is the following
<pre>  R_abc = P0'*R_dq0*P0.</pre>
with P0 the orthogonal transform 'Transforms.P0'.</p>
</html>"));
  end Rotation_abc;

  block RotationPhasor "Rotation of input signal-vector"
    extends Partials.MIMO(final nin=2, final nout=2);

    Modelica.Blocks.Interfaces.RealInput theta "rotation angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));

  equation
    y = {u[1], u[2] + theta};
    annotation (defaultComponentName = "rot_Phasor",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-75,40},{75,-40}},
            lineColor={128,128,128},
            textString=
                 "rot_Ph")}),
      Documentation(info="<html>
<p>Rotates phasor u in polar representation by angle theta.</p>
<p>Input u:
<pre>
  u[1]     absolute value
  u[2]     argument, phase
</pre></p>
<p>Output y:
<pre>
  y[1] = u[1]            absolute value
  y[2] = u[2] + theta    argument, phase
</pre></p>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end RotationPhasor;

  block PhasorToAlphaBeta "Rotation of input signal-vector"
    extends Partials.MIMO(final nin=2, final nout=2);

    Modelica.Blocks.Interfaces.RealInput theta "rotation angle"
      annotation (Placement(transformation(
          origin={0,100},
          extent={{-10,-10},{10,10}},
          rotation=270)));
  protected
    constant Real s23=sqrt(2/3);

  equation
    y = sqrt(2/3)*u[1]*{cos(u[2] + theta), sin(u[2] + theta)};
    annotation (defaultComponentName = "phToAlphaBeta",
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{-70,18},{-10,-18}},
            lineColor={128,128,128},
            textString=
                 "uPh"),
          Text(
            extent={{-10,40},{80,10}},
            lineColor={128,128,128},
            textString=
                 "alpha"),
          Text(
            extent={{-10,-10},{80,-40}},
            lineColor={128,128,128},
            textString=
                 "beta")}),
      Documentation(info="<html>
<p>Transforms phasor u to amplitudes {alpha, beta}.<br>
i.e. rotates phasor in polar representation by angle theta and transforms to Euclidean {y[1], y[2]} coordinates.</p>
<p>Input u:
<pre>
  u[1]     absolute value
  u[2]     argument, phase
</pre></p>
<p>Output y:
<pre>
  y = sqrt(2/3)*u[1]*{cos(u[2] + theta), sin(u[2] + theta)}    amplitudes alpha, beta
</pre></p>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end PhasorToAlphaBeta;
  annotation (preferredView="info",
Documentation(info="<html>
</html>"),
    Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end Transforms;
