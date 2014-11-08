within PowerSystems.Examples.Spot.Data;
package Semiconductors "Breaker example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record IdealSC100V_10A "Ideal semiconductor parameters, example"
    extends Basic.Nominal.NominalDataVI(
      V_nom=100,
      I_nom=10);

    Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}"
      annotation(Dialog);
    SI.Voltage Vf=1 "forward threshold-voltage"
      annotation(Evaluate=true, Dialog);
    SI.Heat Hsw_nom=2e-3 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true, Dialog);
    Real[:] cT_loss=fill(0,0) "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true, Dialog);
    SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Evaluate=true, Dialog(enable=size(cT_loss,1)>0));

    annotation (defaultComponentName="idealSC100_10",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
</html>"));
  end IdealSC100V_10A;

  record IdealSC1kV_100A "Ideal semiconductor parameters, example"
    extends Basic.Nominal.NominalDataVI(
      V_nom=100,
      I_nom=10);

    Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}"
      annotation(Dialog);
    SI.Voltage Vf=2.5 "forward threshold-voltage"
      annotation(Evaluate=true, Dialog);
    SI.Heat Hsw_nom=0.25 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true, Dialog);
    Real[:] cT_loss=fill(0,0) "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true, Dialog);
    SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Evaluate=true, Dialog(enable=size(cT_loss,1)>0));

    annotation (defaultComponentName="idealSC1k_100",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
</html>"));
  end IdealSC1kV_100A;

  record IdealSC3kV_500A "Ideal semiconductor parameters, example"
    extends Basic.Nominal.NominalDataVI(
      V_nom=3e3,
      I_nom=500);

    Real[2] eps={1e-4,1e-4} "pu {resistance 'on', conductance 'off'}"
      annotation(Dialog);
    SI.Voltage Vf=3 "forward threshold-voltage"
       annotation(Evaluate=true, Dialog);
    SI.Heat Hsw_nom=5 "switching loss at V_nom, I_nom (av on off)"
      annotation(Evaluate=true, Dialog);
    Real[:] cT_loss={0.005} "{cT1,cT2,...} T-coef thermal losses"
      annotation(Evaluate=true, Dialog);
    SI.Temp_K T0_loss=300 "reference T for cT_loss expansion"
      annotation(Evaluate=true, Dialog(enable=size(cT_loss,1)>0));

    annotation (defaultComponentName="idealSC3k_500",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
</html>"));
  end IdealSC3kV_500A;

  annotation (preferredView="info",
    Documentation(info="<html>
</html>"));
end Semiconductors;
