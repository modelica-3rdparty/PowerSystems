within PowerSystems.Common;
package IronSaturation "Iron saturation properties"
  extends Modelica.Icons.MaterialPropertiesPackage;

  function saturationAnalytic "Analytic iron saturation function"

    input Real[:] psi0 "unsaturated flux pu";
    input Real[3] c;
    output Real[size(psi0,1)] psi
      "n=0: saturated flux, n=1: derivative d_psi/d_psi0";

  algorithm
    for k in 1:size(psi0,1) loop
    psi[k] := (c[1]/c[2])*tanh(c[2]*psi0[k]) + c[3]*psi0[k];
    end for;
    annotation (
      Documentation(
              info="<html>
<p>Returns analytic saturation function.
<pre>
  Input: psi0     unsaturated flux pu
  Output: psi     saturating flux as function of psi0
</pre>
The analytic expression for the saturating flux function is
<pre>
  psi = (c[1]/c[2])*tanh(c[2]*psi0) + c[3]*psi0
</pre>
For <tt>xratio &lt &lt  1</tt> the coefficients <tt>c</tt> are related to
<pre>
  xratio      ratio saturated/unsaturated coupling impedance
  psi_sat     saturation value of flux pu
</pre>
in the following way
<pre>
  c[1] = 1-xratio
  c[2] = (1-xratio)/(psi_sat-xratio)
  c[3] = xratio
</pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end saturationAnalytic;

  function der_saturationAnalytic
    "Derivative of analytic iron saturation function"

    input Real[:] psi0 "unsaturated flux pu";
    input Real[3] c;
    output Real[size(psi0,1)] der_psi "derivative d_psi/d_psi0";

  algorithm
    for k in 1:size(psi0,1) loop
    der_psi[k] := c[1]/(cosh(c[2]*psi0[k]))^2 + c[3];
    end for;
    annotation (
      Documentation(
              info="<html>
<p>Returns  the derivative of analytic saturation function.
<pre>
  Input: psi0         unsaturated flux pu
  Output: der_psi     derivative of saturating flux with respect to psi0
</pre>
The analytic expression for the saturating flux function and derivative is
<pre>
  psi = (c[1]/c[2])*tanh(c[2]*psi0) + c[3]*psi0
  d_psi/d_psi0 = c[1]/cosh(c[2]*psi0)^2 + c[3]
</pre>
For <tt>xratio &lt &lt  1</tt> the coefficients <tt>c</tt> are related to
<pre>
  xratio      ratio saturated/unsaturated coupling impedance
  psi_sat     saturation value of flux pu
</pre>
in the following way
<pre>
  c[1] = 1-xratio
  c[2] = (1-xratio)/(psi_sat-xratio)
  c[3] = xratio
</pre></p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end der_saturationAnalytic;

  record SaturationTab "Saturating flux table"
    extends Modelica.Icons.Record;
  // psi0:          unsaturated flux pu, 1st column
  // psi:           saturating flux pu,  2nd column
  // d_psi/d_psi0:  derivative of psi with respect to psi0, 3rd column
  //
  // psi0             psi               d_psi/d_psi0

    Real psi[102,3] = [
    0.0000000e+000,  0.0000000e+000,  1.0000000e+000;
    5.0000000e-002,  4.9981521e-002,  9.9889156e-001;
    1.0000000e-001,  9.9852361e-002,  9.9557607e-001;
    1.5000000e-001,  1.4950282e-001,  9.9008280e-001;
    2.0000000e-001,  1.9882514e-001,  9.8245996e-001;
    2.5000000e-001,  2.4771442e-001,  9.7277376e-001;
    3.0000000e-001,  2.9606950e-001,  9.6110722e-001;
    3.5000000e-001,  3.4379377e-001,  9.4755869e-001;
    4.0000000e-001,  3.9079586e-001,  9.3224012e-001;
    4.5000000e-001,  4.3699033e-001,  9.1527512e-001;
    5.0000000e-001,  4.8229815e-001,  8.9679696e-001;
    5.5000000e-001,  5.2664715e-001,  8.7694646e-001;
    6.0000000e-001,  5.6997236e-001,  8.5586980e-001;
    6.5000000e-001,  6.1221619e-001,  8.3371650e-001;
    7.0000000e-001,  6.5332858e-001,  8.1063732e-001;
    7.5000000e-001,  6.9326699e-001,  7.8678238e-001;
    8.0000000e-001,  7.3199635e-001,  7.6229941e-001;
    8.5000000e-001,  7.6948886e-001,  7.3733216e-001;
    9.0000000e-001,  8.0572380e-001,  7.1201899e-001;
    9.5000000e-001,  8.4068720e-001,  6.8649173e-001;
    1.0000000e+000,  8.7437149e-001,  6.6087466e-001;
    1.0500000e+000,  9.0677511e-001,  6.3528381e-001;
    1.1000000e+000,  9.3790209e-001,  6.0982636e-001;
    1.1500000e+000,  9.6776160e-001,  5.8460025e-001;
    1.2000000e+000,  9.9636746e-001,  5.5969405e-001;
    1.2500000e+000,  1.0237377e+000,  5.3518688e-001;
    1.3000000e+000,  1.0498940e+000,  5.1114853e-001;
    1.3500000e+000,  1.0748613e+000,  4.8763966e-001;
    1.4000000e+000,  1.0986676e+000,  4.6471219e-001;
    1.4500000e+000,  1.1213430e+000,  4.4240962e-001;
    1.5000000e+000,  1.1429196e+000,  4.2076756e-001;
    1.5500000e+000,  1.1634312e+000,  3.9981424e-001;
    1.6000000e+000,  1.1829129e+000,  3.7957100e-001;
    1.6500000e+000,  1.2014004e+000,  3.6005295e-001;
    1.7000000e+000,  1.2189304e+000,  3.4126945e-001;
    1.7500000e+000,  1.2355397e+000,  3.2322472e-001;
    1.8000000e+000,  1.2512652e+000,  3.0591839e-001;
    1.8500000e+000,  1.2661437e+000,  2.8934600e-001;
    1.9000000e+000,  1.2802119e+000,  2.7349957e-001;
    1.9500000e+000,  1.2935056e+000,  2.5836802e-001;
    2.0000000e+000,  1.3060604e+000,  2.4393764e-001;
    2.0500000e+000,  1.3179108e+000,  2.3019252e-001;
    2.1000000e+000,  1.3290908e+000,  2.1711492e-001;
    2.1500000e+000,  1.3396331e+000,  2.0468562e-001;
    2.2000000e+000,  1.3495698e+000,  1.9288424e-001;
    2.2500000e+000,  1.3589316e+000,  1.8168951e-001;
    2.3000000e+000,  1.3677485e+000,  1.7107954e-001;
    2.3500000e+000,  1.3760490e+000,  1.6103206e-001;
    2.4000000e+000,  1.3838607e+000,  1.5152455e-001;
    2.4500000e+000,  1.3912100e+000,  1.4253451e-001;
    2.5000000e+000,  1.3981224e+000,  1.3403952e-001;
    2.5500000e+000,  1.4046219e+000,  1.2601741e-001;
    2.6000000e+000,  1.4107316e+000,  1.1844637e-001;
    2.6500000e+000,  1.4164737e+000,  1.1130502e-001;
    2.7000000e+000,  1.4218690e+000,  1.0457250e-001;
    2.7500000e+000,  1.4269374e+000,  9.8228503e-002;
    2.8000000e+000,  1.4316980e+000,  9.2253356e-002;
    2.8500000e+000,  1.4361686e+000,  8.6628033e-002;
    2.9000000e+000,  1.4403663e+000,  8.1334185e-002;
    2.9500000e+000,  1.4443072e+000,  7.6354161e-002;
    3.0000000e+000,  1.4480066e+000,  7.1671012e-002;
    3.0500000e+000,  1.4514790e+000,  6.7268499e-002;
    3.1000000e+000,  1.4547379e+000,  6.3131088e-002;
    3.1500000e+000,  1.4577963e+000,  5.9243945e-002;
    3.2000000e+000,  1.4606662e+000,  5.5592924e-002;
    3.2500000e+000,  1.4633593e+000,  5.2164556e-002;
    3.3000000e+000,  1.4658862e+000,  4.8946029e-002;
    3.3500000e+000,  1.4682572e+000,  4.5925175e-002;
    3.4000000e+000,  1.4704818e+000,  4.3090446e-002;
    3.4500000e+000,  1.4725691e+000,  4.0430894e-002;
    3.5000000e+000,  1.4745277e+000,  3.7936152e-002;
    3.5500000e+000,  1.4763653e+000,  3.5596407e-002;
    3.6000000e+000,  1.4780897e+000,  3.3402381e-002;
    3.6500000e+000,  1.4797079e+000,  3.1345309e-002;
    3.7000000e+000,  1.4812264e+000,  2.9416913e-002;
    3.7500000e+000,  1.4826516e+000,  2.7609382e-002;
    3.8000000e+000,  1.4839892e+000,  2.5915350e-002;
    3.8500000e+000,  1.4852449e+000,  2.4327872e-002;
    3.9000000e+000,  1.4864237e+000,  2.2840407e-002;
    3.9500000e+000,  1.4875305e+000,  2.1446794e-002;
    4.0000000e+000,  1.4885698e+000,  2.0141237e-002;
    4.0500000e+000,  1.4895460e+000,  1.8918278e-002;
    4.1000000e+000,  1.4904629e+000,  1.7772787e-002;
    4.1500000e+000,  1.4913245e+000,  1.6699941e-002;
    4.2000000e+000,  1.4921341e+000,  1.5695204e-002;
    4.2500000e+000,  1.4928951e+000,  1.4754317e-002;
    4.3000000e+000,  1.4936105e+000,  1.3873279e-002;
    4.3500000e+000,  1.4942833e+000,  1.3048331e-002;
    4.4000000e+000,  1.4949162e+000,  1.2275945e-002;
    4.4500000e+000,  1.4955117e+000,  1.1552810e-002;
    4.5000000e+000,  1.4960723e+000,  1.0875819e-002;
    4.5500000e+000,  1.4966000e+000,  1.0242055e-002;
    4.6000000e+000,  1.4970971e+000,  9.6487843e-003;
    4.6500000e+000,  1.4975655e+000,  9.0934416e-003;
    4.7000000e+000,  1.4980071e+000,  8.5736213e-003;
    4.7500000e+000,  1.4984235e+000,  8.0870684e-003;
    4.8000000e+000,  1.4988163e+000,  7.6316686e-003;
    4.8500000e+000,  1.4991871e+000,  7.2054403e-003;
    4.9000000e+000,  1.4995373e+000,  6.8065263e-003;
    4.9500000e+000,  1.4998682e+000,  6.4331862e-003;
    5.0000000e+000,  1.5001810e+000,  6.0837891e-003;
    5.0500000e+000,  1.5005000e+000,  6.0837891e-003];

    annotation (
      Documentation(
              info="<html>
<p>Example table of saturating flux. The tabulated values correspond to the function values of 'saturationAnalytic' and 'der_saturationAnalytic', psi(psi0) and d_psi/d_psi0(psi0).</p>
<p>For use with 'Base.Math.interpolateTable':
<pre>
  y = interpolateTable(psi0, saturationTab)
  psi = y[1]
  der_psi = y[2]
</pre></p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end SaturationTab;
  annotation (preferredView="info",
Documentation(info="<html>
<p>Iron saturation function and table.</p>
</html>
"), Icon(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-100,-100},{100,100}},
        grid={2,2}), graphics));
end IronSaturation;
