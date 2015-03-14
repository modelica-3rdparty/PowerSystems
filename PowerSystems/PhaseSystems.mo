within PowerSystems;
package PhaseSystems "Phase systems used in power connectors"
  extends Modelica.Icons.Package;
  import SI = Modelica.SIunits;
  import Modelica.Constants.pi;

  partial package PartialPhaseSystem "Base package of all phase systems"
    extends Modelica.Icons.Package;
    constant String phaseSystemName = "UnspecifiedPhaseSystem";
    constant Integer n "Number of independent voltage and current components";
    constant Integer m "Number of reference angles";

    type Voltage = Real(unit = "V", quantity = "Voltage." + phaseSystemName)
      "voltage for connector";
    type Current = Real(unit = "A", quantity = "Current." + phaseSystemName)
      "current for connector";
    type ReferenceAngle = Basic.Types.ReferenceAngle "Reference angle for connector";

    replaceable partial function j "Return vector rotated by 90 degrees"
      extends Modelica.Icons.Function;
      input Real x[n];
      output Real y[n];
    end j;

    replaceable function jj "Vectorized version of j"
      input Real[:,:] xx "array of voltage or current vectors";
      output Real[size(xx,1),size(xx,2)] yy "array of rotated vectors";
    algorithm
      //yy := {j(xx[:,k]) for k in 1:size(xx,2)};
      // Note: Dymola 2013 fails to expand
      for k in 1:size(xx,2) loop
        yy[:,k] := j(xx[:,k]);
      end for;
    end jj;

    replaceable partial function thetaRel
      "Return absolute angle of rotating system as offset to thetaRef"
      input SI.Angle theta[m];
      output SI.Angle thetaRel;
    end thetaRel;

    replaceable partial function thetaRef
      "Return absolute angle of rotating reference system"
      input SI.Angle theta[m];
      output SI.Angle thetaRef;
    end thetaRef;

    replaceable partial function phase "Return phase"
      extends Modelica.Icons.Function;
      input Real x[n];
      output SI.Angle phase;
    end phase;

    replaceable partial function phaseVoltages
      "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    end phaseVoltages;

    replaceable partial function phaseCurrents "Return phase currents"
      extends Modelica.Icons.Function;
      input SI.Current I "system current";
      input SI.Angle phi = 0 "phase angle";
      output SI.Current i[n] "phase currents";
    end phaseCurrents;

    replaceable partial function phasePowers "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.ActivePower P "active system power";
      input SI.Angle phi = 0 "phase angle";
      output SI.Power p[n] "phase powers";
    end phasePowers;

    replaceable partial function phasePowers_vi "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.Power p[n] "phase powers";
    end phasePowers_vi;

    replaceable partial function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    end systemVoltage;

    replaceable partial function systemCurrent
      "Return system current as function of phase currents"
      extends Modelica.Icons.Function;
      input SI.Current i[n];
      output SI.Current I;
    end systemCurrent;

    replaceable partial function activePower
      "Return total power as function of phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.ActivePower P "active system power";
    end activePower;

    annotation (Icon(graphics));
  end PartialPhaseSystem;

  package DirectCurrent "DC system"
    extends PartialPhaseSystem(phaseSystemName="DirectCurrent", n=1, m=0);

    redeclare function j "Direct current has no complex component"
      extends Modelica.Icons.Function;
      input Real x[n];
      output Real y[n];
    algorithm
      y := zeros(n);
    end j;

    redeclare function thetaRel
      "Return absolute angle of rotating system as offset to thetaRef"
      input SI.Angle theta[m];
      output SI.Angle thetaRel;
    algorithm
      thetaRel := 0;
    end thetaRel;

    redeclare function thetaRef
      "Return absolute angle of rotating reference system"
      input SI.Angle theta[m];
      output SI.Angle thetaRef;
    algorithm
      thetaRef := 0;
    end thetaRef;

    redeclare function phase "Return phase"
      extends Modelica.Icons.Function;
      input Real x[n];
      output SI.Angle phase;
    algorithm
      phase := 0;
    end phase;

    redeclare replaceable function phaseVoltages
      "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    algorithm
      v := {V};
    end phaseVoltages;

    redeclare function phaseCurrents "Return phase currents"
      extends Modelica.Icons.Function;
      input SI.Current I "system current";
      input SI.Angle phi = 0 "phase angle";
      output SI.Current i[n] "phase currents";
    algorithm
      i := {I};
    end phaseCurrents;

    redeclare function phasePowers "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.ActivePower P "active system power";
      input SI.Angle phi = 0 "phase angle";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {P};
    end phasePowers;

    redeclare function phasePowers_vi "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {v*i};
    end phasePowers_vi;

    redeclare replaceable function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    algorithm
      V := v[1];
    end systemVoltage;

    redeclare function systemCurrent
      "Return system current as function of phase currents"
      extends Modelica.Icons.Function;
      input SI.Current i[n];
      output SI.Current I;
    algorithm
      I := i[1];
    end systemCurrent;

    redeclare function activePower
      "Return total power as function of phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.ActivePower P "active system power";
    algorithm
      P := v*i;
    end activePower;

    annotation (Icon(graphics={Line(
            points={{-70,-10},{50,-10}},
            color={95,95,95},
            smooth=Smooth.None)}));
  end DirectCurrent;

  package TwoConductor "Two conductors for Spot DC_AC1ph components"
    extends PartialPhaseSystem(phaseSystemName="TwoConductor", n=2, m=0);

    redeclare function j "Direct current has no complex component"
      extends Modelica.Icons.Function;
      input Real x[n];
      output Real y[n];
    algorithm
      y := zeros(n);
    end j;

    redeclare function thetaRel
      "Return absolute angle of rotating system as offset to thetaRef"
      input SI.Angle theta[m];
      output SI.Angle thetaRel;
    algorithm
      thetaRel := 0;
    end thetaRel;

    redeclare function thetaRef
      "Return absolute angle of rotating reference system"
      input SI.Angle theta[m];
      output SI.Angle thetaRef;
    algorithm
      thetaRef := 0;
    end thetaRef;

    redeclare function phase "Return phase"
      extends Modelica.Icons.Function;
      input Real x[n];
      output SI.Angle phase;
    algorithm
      phase := 0;
    end phase;

    redeclare replaceable function phaseVoltages
      "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    algorithm
      v := 0.5*{V, -V};
    end phaseVoltages;

    redeclare function phaseCurrents "Return phase currents"
      extends Modelica.Icons.Function;
      input SI.Current I "system current";
      input SI.Angle phi = 0 "phase angle";
      output SI.Current i[n] "phase currents";
    algorithm
      i := {I, -I};
    end phaseCurrents;

    redeclare function phasePowers "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.ActivePower P "active system power";
      input SI.Angle phi = 0 "phase angle";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {P, 0};
    end phasePowers;

    redeclare function phasePowers_vi "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.Power p[n] "phase powers";
    algorithm
      p := v.*i;
    end phasePowers_vi;

    redeclare replaceable function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    algorithm
      V := v[1] - v[2];
    end systemVoltage;

    redeclare function systemCurrent
      "Return system current as function of phase currents"
      extends Modelica.Icons.Function;
      input SI.Current i[n];
      output SI.Current I;
    algorithm
      I := (i[1] - i[2])/2;
    end systemCurrent;

    redeclare function activePower
      "Return total power as function of phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.ActivePower P "active system power";
    algorithm
      P := v*i;
    end activePower;

    annotation (Icon(graphics={Line(
            points={{-70,-28},{50,-28}},
            color={95,95,95},
            smooth=Smooth.None),
                               Line(
            points={{-70,6},{50,6}},
            color={95,95,95},
            smooth=Smooth.None)}));
  end TwoConductor;

  package ThreePhase_d
    "AC system covering only resistive loads with three symmetric phases"
    extends DirectCurrent(phaseSystemName="ThreePhase_d");

    redeclare function phaseVoltages "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    algorithm
      v := {V}/sqrt(3);
    end phaseVoltages;

    redeclare function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    algorithm
      V := sqrt(3)*v[1];
    end systemVoltage;

    annotation (Icon(graphics={
          Line(
            points={{-70,-10},{-58,10},{-38,30},{-22,10},{-10,-10},{2,-30},{22,
                -50},{40,-30},{50,-10}},
            color={95,95,95},
            smooth=Smooth.Bezier)}));
  end ThreePhase_d;

  package ThreePhase_dq "AC system, symmetrically loaded three phases"
    extends PartialPhaseSystem(phaseSystemName="ThreePhase_dq", n=2, m=1);

    redeclare function j "Return vector rotated by 90 degrees"
      extends Modelica.Icons.Function;
      input Real x[n];
      output Real y[n];
    algorithm
      y := {-x[2], x[1]};
    end j;

    redeclare function thetaRel
      "Return absolute angle of rotating system as offset to thetaRef"
      input SI.Angle theta[m];
      output SI.Angle thetaRel;
    algorithm
      thetaRel := 0;
    end thetaRel;

    redeclare function thetaRef
      "Return absolute angle of rotating reference system"
      input SI.Angle theta[m];
      output SI.Angle thetaRef;
    algorithm
      thetaRef := theta[1];
    end thetaRef;

    redeclare function phase "Return phase"
      extends Modelica.Icons.Function;
      input Real x[n];
      output SI.Angle phase;
    algorithm
      phase := atan2(x[2], x[1]);
    end phase;

    redeclare function phaseVoltages "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    algorithm
      v := {V*cos(phi), V*sin(phi)}/sqrt(3);
    end phaseVoltages;

    redeclare function phaseCurrents "Return phase currents"
      extends Modelica.Icons.Function;
      input SI.Current I "system current";
      input SI.Angle phi = 0 "phase angle";
      output SI.Current i[n] "phase currents";
    algorithm
      i := {I*cos(phi), I*sin(phi)};
    end phaseCurrents;

    redeclare function phasePowers "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.ActivePower P "active system power";
      input SI.Angle phi = 0 "phase angle";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {P, P*tan(phi)};
    end phasePowers;

    redeclare function phasePowers_vi "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {v*i, -j(v)*i};
    end phasePowers_vi;

    redeclare function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    algorithm
      V := sqrt(3*v*v);
    end systemVoltage;

    redeclare function systemCurrent
      "Return system current as function of phase currents"
      extends Modelica.Icons.Function;
      input SI.Current i[n];
      output SI.Current I;
    algorithm
      I := sqrt(i*i);
    end systemCurrent;

    redeclare function activePower
      "Return total power as function of phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.ActivePower P "active system power";
    algorithm
      P := v[1]*i[1];
    end activePower;

    annotation (Icon(graphics={
          Line(
            points={{-70,12},{-58,32},{-38,52},{-22,32},{-10,12},{2,-8},{22,-28},
                {40,-8},{50,12}},
            color={95,95,95},
            smooth=Smooth.Bezier),
          Line(
            points={{-70,-70},{50,-70}},
            color={95,95,95},
            smooth=Smooth.None),
          Line(
            points={{-70,-46},{50,-46}},
            color={95,95,95},
            smooth=Smooth.None)}));
  end ThreePhase_dq;

  package ThreePhase_dq0 "AC system in dq0 representation"
    extends PartialPhaseSystem(phaseSystemName="ThreePhase_dq0", n=3, m=2);

    redeclare function j
      "Rotation(pi/2) of vector around {0,0,1} and projection on orth plane"
      extends Modelica.Icons.Function;
      input Real x[:];
      output Real y[size(x,1)];
    algorithm
      y := cat(1, {-x[2], x[1]}, zeros(size(x,1)-2));
    end j;

    redeclare function jj "Vectorized version of j"
      input Real[:,:] xx "array of voltage or current vectors";
      output Real[size(xx,1),size(xx,2)] yy "array of rotated vectors";
    algorithm
      yy := cat(1, {-xx[2,:], xx[1,:]}, zeros(size(xx,1)-2, size(xx,2)));
    end jj;

    redeclare function thetaRel
      "Return absolute angle of rotating system as offset to thetaRef"
      input SI.Angle theta[m];
      output SI.Angle thetaRel;
    algorithm
      thetaRel := theta[1];
    end thetaRel;

    redeclare function thetaRef
      "Return absolute angle of rotating reference system"
      input SI.Angle theta[m];
      output SI.Angle thetaRef;
    algorithm
      thetaRef := theta[2];
    end thetaRef;

    redeclare function phase "Return phase"
      extends Modelica.Icons.Function;
      input Real x[n];
      output SI.Angle phase;
    algorithm
      phase := atan2(x[2], x[1]);
    end phase;

    redeclare function phaseVoltages "Return phase to neutral voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage V "system voltage";
      input SI.Angle phi = 0 "phase angle";
      output SI.Voltage v[n] "phase to neutral voltages";
    protected
      Voltage neutral_v = 0;
    algorithm
      v := {V*cos(phi), V*sin(phi), sqrt(3)*neutral_v}/sqrt(3);
    end phaseVoltages;

    redeclare function phaseCurrents "Return phase currents"
      extends Modelica.Icons.Function;
      input Current I "system current";
      input SI.Angle phi = 0 "phase angle";
      output SI.Current i[n] "phase currents";
    algorithm
      i := {I*cos(phi), I*sin(phi), 0};
    end phaseCurrents;

    redeclare function phasePowers "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.ActivePower P "active system power";
      input SI.Angle phi = 0 "phase angle";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {P, P*tan(phi), 0};
    end phasePowers;

    redeclare function phasePowers_vi "Return phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.Power p[n] "phase powers";
    algorithm
      p := {v[1:2]*i[1:2], -j(v[1:2])*i[1:2], v[3]*i[3]};
    end phasePowers_vi;

    redeclare function systemVoltage
      "Return system voltage as function of phase voltages"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n];
      output SI.Voltage V;
    algorithm
      V := sqrt(v*v);
    end systemVoltage;

    redeclare function systemCurrent
      "Return system current as function of phase currents"
      extends Modelica.Icons.Function;
      input SI.Current i[n];
      output SI.Current I;
    algorithm
      I := sqrt(i*i);
    end systemCurrent;

    redeclare function activePower
      "Return total power as function of phase powers"
      extends Modelica.Icons.Function;
      input SI.Voltage v[n] "phase voltages";
      input SI.Current i[n] "phase currents";
      output SI.ActivePower P "active system power";
    algorithm
      P := v[1]*i[1];
    end activePower;

    annotation (Icon(graphics={
          Line(
            points={{-70,28},{-58,48},{-38,68},{-22,48},{-10,28},{2,8},{22,-12},
                {40,8},{50,28}},
            color={95,95,95},
            smooth=Smooth.Bezier),
          Line(
            points={{-70,-54},{50,-54}},
            color={95,95,95},
            smooth=Smooth.None),
          Line(
            points={{-70,-78},{50,-78}},
            color={95,95,95},
            smooth=Smooth.None),
          Line(
            points={{-70,-28},{50,-28}},
            color={95,95,95},
            smooth=Smooth.None)}));
  end ThreePhase_dq0;

  annotation (Icon(graphics={Line(
          points={{-70,-52},{50,-52}},
          color={95,95,95},
          smooth=Smooth.None), Line(
          points={{-70,8},{-58,28},{-38,48},{-22,28},{-10,8},{2,-12},{22,-32},{
              40,-12},{50,8}},
          color={95,95,95},
          smooth=Smooth.Bezier)}));
end PhaseSystems;
