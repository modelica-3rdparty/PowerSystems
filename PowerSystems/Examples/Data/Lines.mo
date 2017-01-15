within PowerSystems.Examples.Data;
package Lines "Line example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record OHline15kV1ph "Overhead RX-line 15kV 1-phase rail, example"
    extends PowerSystems.AC1ph_DC.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=15e3,
      S_nom=100e6,
      f_nom=50/3,
      r={3e-3,3e-3},
      x=2e-3,
      x0=5e-3);
    annotation (defaultComponentName="OH15kV1ph",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>
"));
  end OHline15kV1ph;

  record OHline_15kV1ph "Overhead PI-line 15kV 1-phase rail, example"
    extends PowerSystems.AC1ph_DC.Lines.Parameters.Line(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50/3,
      r={3e-3,3e-3},
      x=2e-3,
      x0=5e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.02e-3,
      b_pp=0.02e-3);
    annotation (defaultComponentName="OH_15kV1ph",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>
"));
  end OHline_15kV1ph;

record OHline132kV1ph "Overhead RX-line 132kV 1-phase rail, example"
  extends PowerSystems.AC1ph_DC.Lines.Parameters.RXline(
    puUnits=true,
    V_nom=132e3,
    S_nom=100e6,
    f_nom=50/3,
    r={1.2e-3,1.2e-3},
    x=1e-3,
    x0=3e-3);
  annotation (defaultComponentName="OH132kV1ph",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"));
end OHline132kV1ph;

record OHline_132kV1ph "Overhead PI-line 132kV 1-phase rail, example"
  extends PowerSystems.AC1ph_DC.Lines.Parameters.Line(
    puUnits=true,
    V_nom=132e3,
    S_nom=100e6,
    f_nom=50/3,
    r={1.2e-3,1.2e-3},
    x=1e-3,
    x0=3e-3,
    g_pg=1e-10,
    g_pp=1e-10,
    b_pg=0.05e-3,
    b_pp=0.05e-3);
  annotation (defaultComponentName="OH_132kV1ph",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"));
end OHline_132kV1ph;

  record Cable132kVDC "Cable RX-line 132kV DC, example"
    extends PowerSystems.AC1ph_DC.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r={0.65e-3,0.65e-3},
      x=0.8e-3,
      x0=3e-3);
    annotation (defaultComponentName="C132kVDC",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable132kVDC;

  record Cable_132kVDC "Cable PI-line 132kV DC, example"
    extends PowerSystems.AC1ph_DC.Lines.Parameters.Line(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r={0.65e-3,0.65e-3},
      x=0.08e-3,
      x0=3e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.003,
      b_pp=0.003);
    annotation (defaultComponentName="C_132kVDC",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable_132kVDC;

  record OHline132kV "Overhead RX-line 132kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r=1.2e-3,
      x=3e-3,
      x0=9e-3);
  annotation (defaultComponentName="OH132kV",
    defaultComponentPrefixes="parameter",
    Documentation(
      info="<html>
</html>"));
  end OHline132kV;

  record OHline_132kV "Overhead PI-line 132kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.Line(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r=1.2e-3,
      x=3e-3,
      x0=9e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.05e-3,
      b_pp=0.05e-3);
    annotation (defaultComponentName="OH_132kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end OHline_132kV;

  record OHline400kV "Overhead RX-line 400kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=400e3,
      S_nom=100e6,
      f_nom=50,
      r=0.02e-3,
      x=0.25e-3,
      x0=0.75e-3);
    annotation (defaultComponentName="OH400kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end OHline400kV;

  record OHline_400kV "Overhead PI-line 400kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.Line(
      puUnits=true,
      V_nom=400e3,
      S_nom=100e6,
      f_nom=50,
      r=0.02e-3,
      x=0.25e-3,
      x0=0.75e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.003e-3,
      b_pp=0.003e-3);
    annotation (defaultComponentName="OH_400kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end OHline_400kV;

  record Cable132kV "Cable RX-line 132kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r=0.65e-3,
      x=0.8e-3,
      x0=3e-3);
    annotation (defaultComponentName="C132kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable132kV;

  record Cable_132kV "Cable PI-line 132kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.Line(
      puUnits=true,
      V_nom=132e3,
      S_nom=100e6,
      f_nom=50,
      r=0.65e-3,
      x=0.08e-3,
      x0=3e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.003,
      b_pp=0.003);
    annotation (defaultComponentName="C_132kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable_132kV;

  record Cable400kV "Cable RX-line 400kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.RXline(
      puUnits=true,
      V_nom=400e3,
      S_nom=100e6,
      f_nom=50,
      r=0.05e-3,
      x=0.085e-3,
      x0=3e-3);
    annotation (defaultComponentName="C400kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable400kV;

  record Cable_400kV "Cable PI-line 400kV 3-phase, example"
    extends PowerSystems.AC3ph.Lines.Parameters.Line(
      puUnits=true,
      V_nom=400e3,
      S_nom=100e6,
      f_nom=50,
      r=0.05e-3,
      x=0.085e-3,
      x0=3e-3,
      g_pg=1e-10,
      g_pp=1e-10,
      b_pg=0.025,
      b_pp=0.025);
    annotation (defaultComponentName="C_400kV",
      defaultComponentPrefixes="parameter",
      Documentation(
        info="<html>
</html>"));
  end Cable_400kV;

  annotation (preferredView="info",
    Documentation(info="<html>
<p>Some examples of standard-types.<br>
Exact data for specific cases still have to be determined.</p>
<p>Note: correct values for V_nom and S_nom are only needed, if you choose input in pu-units. In this case the 'nominal' values are chosen as base-values. For SI-units the values are not used. Nevertheless they must be defined.</p>
</html>"));
end Lines;
