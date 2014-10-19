within PowerSystems.Examples.Spot.Data;
package Semiconductors "Breaker example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record IdealSC100V_10A "Ideal semiconductor parameters, example"
    extends Modelica.Icons.Record;

    parameter Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}";
    parameter SI.Voltage Vf=1 "forward threshold-voltage"       annotation(Evaluate=true);
    parameter SI.Heat Hsw_nom=2e-3 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true);
    parameter Real[:] cT_loss=fill(0,0) "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true);
    parameter SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Dialog(enable=size(cT_loss,1)>0), Evaluate=true);

    parameter SI.Voltage V_nom=100 "nom Voltage"
      annotation(Evaluate=true);
    parameter SI.Current I_nom=10 "nom Current"
      annotation(Evaluate=true);
    annotation (defaultComponentName="idealSC100_10",
      Documentation(
            info="<html>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end IdealSC100V_10A;

  record IdealSC1kV_100A "Ideal semiconductor parameters, example"
    extends Modelica.Icons.Record;

    parameter Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}";
    parameter SI.Voltage Vf=2.5 "forward threshold-voltage"     annotation(Evaluate=true);
    parameter SI.Heat Hsw_nom=0.25 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true);
    parameter Real[:] cT_loss=fill(0,0) "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true);
    parameter SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Dialog(enable=size(cT_loss,1)>0), Evaluate=true);

    parameter SI.Voltage V_nom=100 "nom Voltage"
      annotation(Evaluate=true);
    parameter SI.Current I_nom=10 "nom Current"
      annotation(Evaluate=true);
    annotation (defaultComponentName="idealSC1k_100",
      Documentation(
            info="<html>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end IdealSC1kV_100A;

  record IdealSC3kV_500A "Ideal semiconductor parameters, example"
    extends Modelica.Icons.Record;

    parameter Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}";
    parameter SI.Voltage Vf=3 "forward threshold-voltage"       annotation(Evaluate=true);
    parameter SI.Heat Hsw_nom=5 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true);
    parameter Real[:] cT_loss={0.005} "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true);
    parameter SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Dialog(enable=size(cT_loss,1)>0), Evaluate=true);

    parameter SI.Voltage V_nom=3e3 "nom Voltage"
      annotation(Evaluate=true);
    parameter SI.Current I_nom=500 "nom Current"
      annotation(Evaluate=true);
    annotation (defaultComponentName="idealSC3k_500",
      Documentation(
            info="<html>
</html>"),
      Diagram(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics));
  end IdealSC3kV_500A;

  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Semiconductors;
