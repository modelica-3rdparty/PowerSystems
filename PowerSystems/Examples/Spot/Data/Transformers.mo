within PowerSystems.Examples.Spot.Data;
package Transformers "Transformer example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

record TrafoIdeal1ph "Ideal trafo, 1-phase, example"
  extends PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoIdeal1ph(
    puUnits=true,
    V_nom={15000,1400},
    S_nom=5e6,
    f_nom=50/3,
    tap_neutral={3,3},
    dv_tap={0.025,0.025});
  annotation (defaultComponentName="trafo15k_1400",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"));
end TrafoIdeal1ph;

record TrafoStray1ph "Trafo with ideal magnetic coupling, 1-phase, example"
  extends PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoStray1ph(
    puUnits=true,
    V_nom={15000,1400},
    S_nom=5e6,
    f_nom=50/3,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r={0.03,0.03},
    x={0.05,0.05});
  annotation (defaultComponentName="trafo15k_1400",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Rectangle(
            extent={{-26,60},{-20,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid), Rectangle(
            extent={{20,60},{26,-60}},
            lineColor={215,215,215},
            fillColor={215,215,215},
            fillPattern=FillPattern.Solid)}));
end TrafoStray1ph;

record TrafoMag1ph "Trafo with magnetic coupling, 1-phase, example"
  extends PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoMag1ph(
    puUnits=true,
    V_nom={15000,1400},
    S_nom=5e6,
    f_nom=50/3,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r={0.03,0.03},
    x={0.05,0.05},
    redc=500,
    xm=500);
  annotation (defaultComponentName="trafo15k_1400",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-30,60},{-30,-60}},
            color={0,0,255},
            pattern=LinePattern.Dot), Line(
            points={{30,60},{30,-60}},
            color={0,0,255},
            pattern=LinePattern.Dot)}));
end TrafoMag1ph;

record TrafoSat1ph "Trafo with saturation, 1-phase, example"
  extends PowerSystems.AC1ph_DC.Transformers.Parameters.TrafoSat1ph(
    puUnits=true,
    V_nom={15000,1400},
    S_nom=5e6,
    f_nom=50/3,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r={0.03,0.03},
    x={0.05,0.05},
    redc=500,
    xm=500,
    psi_sat(unit="1")=1.5,
    xm_sat=1);
  annotation (defaultComponentName="trafo15k_1400",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"),
    Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Line(
            points={{-15,-40},{-11,-10},{-7,10},{-5,20},{-1,30},{5,36},{15,40}},
            color={0,0,0},
            thickness=0.5)}));

end TrafoSat1ph;

record TrafoIdeal "Ideal trafo, 3-phase, example"
  extends PowerSystems.AC3ph.Transformers.Parameters.TrafoIdeal(
    puUnits=true,
    V_nom={20,400}*1e3,
    S_nom=1000e6,
    f_nom=50,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r_n={1,1});
  annotation (defaultComponentName="trafo20k_400k",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"));
end TrafoIdeal;

record TrafoStray "Trafo with ideal magnetic coupling, 3-phase, example"
  extends PowerSystems.AC3ph.Transformers.Parameters.TrafoStray(
    puUnits=true,
    V_nom={20,400}*1e3,
    S_nom=1000e6,
    f_nom=50,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r_n={1,1},
    r={0.005,0.005},
    x={0.05,0.05},
    x0={x[1],x[2]});
  annotation (defaultComponentName="trafo20k_400k",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoStray;

record TrafoMag "Trafo with magnetic coupling, 3-phase, example"
  extends PowerSystems.AC3ph.Transformers.Parameters.TrafoMag(
    puUnits=true,
    V_nom={20,400}*1e3,
    S_nom=1000e6,
    f_nom=50,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r_n={1,1},
    r={0.005,0.005},
    x={0.05,0.05},
    x0={x[1],x[2]},
    redc=500,
    xm=500,
    xm0=50);
  annotation (defaultComponentName="trafo20k_400k",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoMag;

record TrafoSat "Trafo with saturation, 3-phase, example"
  extends PowerSystems.AC3ph.Transformers.Parameters.TrafoSat(
    puUnits=true,
    V_nom={20,400}*1e3,
    S_nom=1000e6,
    f_nom=50,
    tap_neutral={3,3},
    dv_tap={0.025,0.025},
    r_n={1,1},
    r={0.005,0.005},
    x={0.05,0.05},
    x0={x[1],x[2]},
    redc=500,
    xm=500,
    xm0=50,
    psi_sat(unit="1")=1.5,
    xm_sat=1);
  annotation (defaultComponentName="trafo20k_400k",
    defaultComponentPrefixes="parameter",
    Documentation(
    info="<html>
</html>"));
end TrafoSat;

  annotation (preferredView="info",
    Documentation(info="<html>
<p>Note: a correct value for S_nom is only needed, if you choose input in pu-units. In this case the 'nominal' values are chosen as base-values. For SI-units S_nom is not used. Nevertheless it must be defined. V_nom however is used to define the winding ratio and voltage start values.</p>
</html>"));
end Transformers;
