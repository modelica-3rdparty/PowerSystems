within PowerSystems.Examples.Data;
package Semiconductors "Breaker example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record IdealSC100V_10A "Ideal semiconductor parameters, example"
    extends PowerSystems.Semiconductors.Ideal.SCparameter(
      V_nom=100,
      I_nom=10,
      eps={1e-4,1e-4},
      Vf=1,
      Hsw_nom=2e-3,
      cT_loss=fill(0,0),
      T0_loss=300);
    annotation (defaultComponentName="idealSC100_10",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
</html>"));
  end IdealSC100V_10A;

  record IdealSC1kV_100A "Ideal semiconductor parameters, example"
    extends PowerSystems.Semiconductors.Ideal.SCparameter(
      V_nom=100,
      I_nom=10,
      eps={1e-4,1e-4},
      Vf=2.5,
      Hsw_nom=0.25,
      cT_loss=fill(0,0),
      T0_loss=300);
    annotation (defaultComponentName="idealSC1k_100",
      defaultComponentPrefixes="parameter",
      Documentation(
            info="<html>
</html>"));
  end IdealSC1kV_100A;

  record IdealSC3kV_500A "Ideal semiconductor parameters, example"
    extends PowerSystems.Semiconductors.Ideal.SCparameter(
      V_nom=3e3,
      I_nom=500,
      eps={1e-4,1e-4},
      Vf=3,
      Hsw_nom=5,
      cT_loss={0.005},
      T0_loss=300);
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
