within PowerSystems.Examples.Spot.Data;
package Machines "Machine example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

record DCser1500V_1p5MVA "DC machine series excited, example"
  extends PowerSystems.AC1ph_DC.Machines.Parameters.DCser(
    pp=2,
    l_fd=0.15,
    r_fd=0.01,
    l_q=0.5,
    r_q=0.05,
    puUnits=true,
    V_nom=1500,
    S_nom=1.5e6,
    w_nom=157.079632679489661923);
  annotation (defaultComponentPrefixes="parameter");
end DCser1500V_1p5MVA;

record DCpar1500V_1p5MVA "DC machine parallel excited, example"
  extends PowerSystems.AC1ph_DC.Machines.Parameters.DCpar(
    pp=2,
    l_fd=100*pi,
    r_fd=100,
    l_q=0.5,
    r_q=0.05,
    puUnits=true,
    V_nom=1500,
    S_nom=1.5e6,
    w_nom=157.079632679489661923,
    Vf_nom=1500);
  annotation (defaultComponentPrefixes="parameter");
end DCpar1500V_1p5MVA;

record DCpm100V_1kVA "DC machine permanent magnet excited, example"
  extends PowerSystems.AC1ph_DC.Machines.Parameters.DCpm(
    pp=2,
    l_aq=0.5,
    r_aq=0.05,
    puUnits=true,
    V_nom=100,
    S_nom=1e3,
    w_nom=157.079632679489661923);
  annotation (defaultComponentPrefixes="parameter");
end DCpm100V_1kVA;

record BLDC100V_1kVA
    "BLDC machine (= synchronous pm, 3rd order model), example pu-units"
  extends Basic.Nominal.NominalDataAC(
    puUnits=true,
    V_nom=100*sqrt(3/2)/2,
    S_nom=1e3,
    f_nom=60);

  Boolean neu_iso=false "isolated neutral if Y" annotation(Dialog);
  Integer pp=2 "pole-pair number" annotation(Dialog);
  final Integer excite=2 "excitation (2:pm)" annotation(Evaluate=true);
  SIpu.MagneticFlux psi_pm(unit="1")=1.2
      "magnetisation (V/V_nom at open term at omega_nom)" annotation(Dialog);
  SIpu.Reactance x_d=0.4 "syn reactance d-axis" annotation(Dialog);
  SIpu.Reactance x_q=0.4 "syn reactance q-axis" annotation(Dialog);
  SIpu.Reactance x_o=0.1 "reactance 0-axis" annotation(Dialog);
  SIpu.Resistance r_s=0.05 "resistance armature" annotation(Dialog);
  SIpu.Resistance r_n=1 "resistance neutral to grd (if Y)" annotation(Dialog(enable=not neu_iso));

  annotation (defaultComponentName="bldc100_1k",
    defaultComponentPrefixes="parameter",
    Documentation(
          info="<html>
<p>The relation between source DC voltage V_dc and nominal 3-phase voltage of the synchronous machine V_nom is given by
<pre>  V_nom = V_dc*sqrt(3/2)/2</pre>
Note that V_nom is only used, if impedance values x and r are given in pu.<br>
f_nom is needed to relate impedance x and inductance L values.</p>
</html>
"));
end BLDC100V_1kVA;

record BLDC100V_1kVA_SI
    "BLDC machine (= synchronous pm, 3rd order model), example SI-units"
  extends Basic.Nominal.NominalDataAC(
    puUnits=true,
    V_nom=100*sqrt(3/2)/2,
    S_nom=1e3,
    f_nom=60);

  Boolean neu_iso=false "isolated neutral if Y" annotation(Dialog);
  Integer pp=2 "pole-pair number" annotation(Dialog);
  final Integer excite=2 "excitation (2:pm)" annotation(Evaluate=true);
  SIpu.MagneticFlux psi_pm(unit="1")=1.2
      "magnetisation (V/V_nom at open term at omega_nom)" annotation(Dialog);
  SIpu.Reactance x_d=1.5 "syn reactance d-axis" annotation(Dialog);
  SIpu.Reactance x_q=1.5 "syn reactance q-axis" annotation(Dialog);
  SIpu.Reactance x_o=0.375 "reactance 0-axis" annotation(Dialog);
  SIpu.Resistance r_s=0.1875 "resistance armature" annotation(Dialog);
  SIpu.Resistance r_n=1 "resistance neutral to grd (if Y)" annotation(Dialog(enable=not neu_iso));

  annotation (defaultComponentName="bldc100_1k_SI",
    defaultComponentPrefixes="parameter",
    Documentation(
          info="<html>
<p>The relation between source DC voltage V_dc and nominal 3-phase voltage of the synchronous machine V_nom is given by
<pre>  V_nom = V_dc*sqrt(3/2)/2</pre>
Note that V_nom is only used, if impedance values x and r are given in pu.<br>
f_nom is needed to relate impedance x and inductance L values.</p>
</html>
"));
end BLDC100V_1kVA_SI;

  record Asynchron400V_30kVA "Asynchronous machine, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Asynchron(
      neu_iso=false,
      pp=8,
      x=3,
      x_o=0.1,
      r_s=0.04,
      r_n=1,
      n_r=1,
      transDat=true,
      use_xtr=true,
      xtr={0.196667},
      tc={0.0130419},
      to={0.198944},
      xsig_s=0.1,
      xsig_r={0.1},
      r_r={0.04},
      puUnits=true,
      V_nom=400,
      S_nom=30e3,
      f_nom=50);
    annotation (defaultComponentPrefixes="parameter");
  end Asynchron400V_30kVA;

  record Asynchron3kV_1p5MVA "Asynchronous machine, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Asynchron(
      neu_iso=false,
      pp=2,
      x=2.8,
      x_o=0.1,
      r_s=0.02,
      r_n=1,
      n_r=2,
      transDat=true,
      use_xtr=true,
      xtr={0.1, 0.075},
      tc={0.014, 0.4e-3},
      to={0.4, 2.8e-3},
      xsig_s=0.05,
      xsig_r={0.0529633, 0.0481803},
      r_r={0.0234304, 0.580595},
      puUnits=true,
      V_nom=3000,
      S_nom=1.5e6,
      f_nom=50);
    annotation (defaultComponentPrefixes="parameter");
  end Asynchron3kV_1p5MVA;

  record Synchron3rd_pm400V_30kVA
    "Synchronous machine pm, 3rd order model, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_pm(
      neu_iso=false,
      pp=2,
      psi_pm=1.1,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.03,
      r_n=1,
      puUnits=true,
      V_nom=400,
      S_nom=30e3,
      f_nom=50);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron3rd_pm400V_30kVA;

  record Synchron_pm400V_30kVA "Synchronous machine pm, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron_pm(
      neu_iso=false,
      pp=2,
      psi_pm=1.1,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.03,
      r_n=1,
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
      puUnits=true,
      V_nom=400,
      S_nom=30e3,
      f_nom=50,
      If_nom=0);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron_pm400V_30kVA;

  record Synchron3rd_pm560V_100kVA "Synchronous machine 3rd order pm, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_pm(
      neu_iso=false,
      pp=2,
      psi_pm=1.1,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.03,
      r_n=1,
      puUnits=true,
      V_nom=560,
      S_nom=100e3,
      f_nom=400);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron3rd_pm560V_100kVA;

  record Synchron_pm560V_100kVA "Synchronous machine pm, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron_pm(
      neu_iso=false,
      pp=2,
      psi_pm=1.1,
      x_d=0.4,
      x_q=0.4,
      x_o=0.1,
      r_s=0.03,
      r_n=1,
      transDat=true,
      use_xtr=true,
      xtr_d={0.142857},
      xtr_q={0.142857},
      tc_d={0.0132629},
      tc_q={0.0132629},
      to_d={0.0371362},
      to_q={0.0371362},
      use_if0=false,
      if0=0,
      alpha_if0=0,
      tol=1e-6,
      xsig_s=0.1,
      xsig_rd={0.05},
      xsig_rq={0.05},
      xm_d=fill(0, 0),
      r_rd={0.03},
      r_rq={0.03},
      puUnits=true,
      V_nom=560,
      S_nom=100e3,
      f_nom=400,
      If_nom=0);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron_pm560V_100kVA;

  record Synchron3rd_ee20kV_1200MVA
    "Synchronous machine, 3rd order model, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_ee(
      neu_iso=false,
      pp=1,
      x_d=2.29,
      x_q=1.95,
      x_o=0.1,
      r_s=0.004,
      r_n=1,
      puUnits=true,
      V_nom=20e3,
      S_nom=1200e6,
      f_nom=50);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron3rd_ee20kV_1200MVA;

  record Synchron_ee20kV_1200MVA "Synchronous machine, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron_ee(
      neu_iso=false,
      pp=1,
      x_d=2.29,
      x_q=1.95,
      x_o=0.1,
      r_s=0.004,
      r_n=1,
      transDat=true,
      use_xtr=true,
      xtr_d={0.361123, 0.283696},
      xtr_q={0.464817, 0.303593},
      tc_d={0.93133, 0.0253919},
      tc_q={0.270197, 0.041458},
      to_d={5.94309, 0.0321197},
      to_q={1.207780, 0.0595723},
      use_if0=true,
      if0=0.286,
      alpha_if0=-0.97738438111682,
      tol=1e-6,
      xsig_s=0.2,
      xsig_rd={0.1294, 0.03498},
      xsig_rq={0.3948, 0.1527},
      xm_d={0.05965},
      r_rd={1.321e-3, 14.38e-3},
      r_rq={7.037e-3, 20.38e-3},
      puUnits=true,
      V_nom=20e3,
      S_nom=1200e6,
      f_nom=50,
      If_nom=8000);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron_ee20kV_1200MVA;

  record Synchron3rd_ee60Hz_26kV_720MVA
    "Synchronous machine, 3rd order model, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_ee(
      neu_iso=false,
      pp=1,
      x_d=1.9,
      x_q=1.77,
      x_o=0.1,
      r_s=0.005,
      r_n=1,
      puUnits=true,
      V_nom=26e3,
      S_nom=720e6,
      f_nom=60);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron3rd_ee60Hz_26kV_720MVA;

  record Synchron_ee60Hz_26kV_720MVA "Synchronous machine, example"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron_ee(
      neu_iso=false,
      pp=1,
      x_d=1.9,
      x_q=1.77,
      x_o=0.1,
      r_s=0.005,
      r_n=1,
      transDat=true,
      use_xtr=true,
      xtr_d={0.3290, 0.253},
      xtr_q={0.436, 0.2730},
      tc_d={0.7158, 0.02058},
      tc_q={0.2134, 0.03320},
      to_d={4.164, 0.02658},
      to_q={0.9307, 0.04936},
      use_if0=true,
      if0=0.835428,
      alpha_if0=-1.7910219718115,
      tol=1e-6,
      xsig_s=0.17,
      xsig_rd={0.1294, 0.03498},
      xsig_rq={0.3948, 0.1527},
      xm_d={0.05965},
      r_rd={1.321e-3, 14.38e-3},
      r_rq={7.037e-3, 20.38e-3},
      puUnits=true,
      V_nom=26e3,
      S_nom=720e6,
      f_nom=60,
      If_nom=1800);
    annotation (defaultComponentPrefixes="parameter");
  end Synchron_ee60Hz_26kV_720MVA;

  record SynchronIso20kV_500MVA
    "Synchronous machine (isotropic), 3rd order model"
    extends PowerSystems.AC3ph.Machines.Parameters.Synchron3rd_ee(
      neu_iso=false,
      pp=1,
      x_d=1.6,
      x_q=1.6,
      x_o=0.1,
      r_s=0.01,
      r_n=1,
      puUnits=true,
      V_nom=20e3,
      S_nom=500e6,
      f_nom=50);
    annotation (defaultComponentPrefixes="parameter");
  end SynchronIso20kV_500MVA;

  annotation (preferredView="info",
Documentation(info="<html>
<p>Note: a correct value for S_nom is only needed, if you choose input in pu-units. In this case the 'nominal' values are chosen as base-values. For SI-units S_nom is not used. Nevertheless it must be defined. V_nom however is used to define voltage start values.</p>
</html>"));
end Machines;
