within PowerSystems;
package Types
  extends Modelica.Icons.TypesPackage;

  type SourceFrequency = enumeration(
    Parameter   "Parameter f",
    Signal   "Signal omega_in",
    System   "System defined") "Options for specification of frequency"
      annotation(Documentation(info="<html>
<p><pre>
  Parameter: parameter f
  Signal:    signal input omega_in
  System:    system defined frequency
</pre></p>
</html>"));

  type SystemFrequency = enumeration(
    Parameter   "Parameter f",
    Signal   "Signal omega_in",
    Average   "Average generators")
    "Options for specification of frequency in system object"
      annotation(Documentation(info="<html>
<p><pre>
  Parameter: parameter f
  Signal:    signal input omega_in
  Average:   average frequency over involved generators
</pre></p>
</html>"));

  type ReferenceFrame = enumeration(
    Synchron   "Synchronously rotating",
    Inertial   "Inertial (signals oscillate)")
    "Options for specification of reference frame"
      annotation(Documentation(info="<html>
<p><pre>
  Synchron: synchronously rotating (stationary signals are constant)
  Inertial: inertial (stationary signals are oscillating)
</pre></p>
</html>"));

  type Dynamics = enumeration(
    FreeInitial   "Free initial conditions",
    FixedInitial   "Fixed initial conditions",
    SteadyInitial   "Steady-state initial conditions",
    SteadyState   "Steady-state (quasi-stationary)") "Options for specification of simulation"
      annotation(Documentation(info="<html>
<p><pre>
  FreeInitial:     Transient simulation with free initial conditions
  FixedInitial:    Transient simulation with fixed initial conditions
  SteadyInitial:   Transient simulation with steady-state initial conditions
  SteadyState:     Quasi-stationary simulation using steady-state model
</pre></p>
</html>"));

  type Init = enumeration(
    v_alpha   "v, alpha: voltage and phase angle, 'slack' (steady init)",
    v_p   "v, p: voltage and active power (steady init)",
    v_q   "v, q: voltage and reactive power (steady init)",
    p_q   "p, q: active and reactive power (steady init)",
    phi_w_el   "phi_el, w_el: machine angle and angular velocity electric",
    none   "none: no initial condition") "Initialization type"
      annotation(Documentation(info="<html>
<p><pre>
  v_alpha:  terminal voltage and phase angle ('slack')
  v_p:      terminal voltage and active power
  v_q:      terminal voltage and reactive power
  p_q:      terminal active and reactive power
  phi_w_el: machine angle and angular velocity electric
  none:     no initial condition
</pre></p>
</html>"));

  type ReferenceAngle "Reference angle"
    extends SI.Angle;

    function equalityConstraint
      input ReferenceAngle theta1[:];
      input ReferenceAngle theta2[:];
      output Real[0] residue "No constraints";
    algorithm
      for i in 1:size(theta1, 1) loop
        assert(abs(theta1[i] - theta2[i]) < Modelica.Constants.eps, "angles theta1 and theta2 not equal over connection!");
      end for;
    end equalityConstraint;

    annotation (Documentation(info="<html>
<p>Type ReferenceAngle specifies the variable-type that contains relative frequency and angular orientation of a rotating electrical reference system.
In the case of three phase AC models we have:</p>
<pre>
  theta[1]       angle relative to reference-system
  theta[2]       reference angle, defining reference-system

  der(theta[1])  relative frequency in reference-system with orientation theta[2]
  der(theta[1] + theta[2])  absolute frequency
</pre>
</html>"));
  end ReferenceAngle;

  type Color = Integer[3] (each min=0, each max=255) "RGB color" annotation (choices(
        choice={255,0,0} "{255, 000, 000 }  red",
        choice={255,255,0} "{255, 255, 000}  yellow",
        choice={0,255,0} "{000, 255, 000}  green",
        choice={0,255,255} "{000, 255, 255}  cyan",
        choice={0,0,255} "{000, 000, 255}  blue",
        choice={255,0,255} "{255, 000, 255}  magenta",
        choice={0,0,0} "{000, 000, 000}  black",
        choice={95,95,95} "{095, 095, 095}  dark grey",
        choice={175,175,175} "{175, 175, 175}  grey",
        choice={255,255,255} "{255, 255, 255}  white"));

end Types;
