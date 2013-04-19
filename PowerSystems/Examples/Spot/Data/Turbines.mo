within PowerSystems.Examples.Spot.Data;
package Turbines "Turbine example data"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record SteamTurboGroup1200MW "Steam turbo-group, example"
    extends PowerSystems.Mechanics.TurboGroups.Parameters.SteamTurboGroup(
     w_nom(displayUnit="rpm")=314.15926535898,
     P_nom={480e6,240e6,240e6,240e6},
     J_turb={20000,200000,200000,200000},
     J_gen=62000,
     J_aux={460,830},
     stiff={260,355,750,750,750,220}*1e6);
  end SteamTurboGroup1200MW;

  record GasTurbineGear "Small GT with gear, example"
    extends PowerSystems.Mechanics.TurboGroups.Parameters.GasTurbineGear(
      w_nom=1576.7653528367,
      P_nom={12, -2}*1e6,
      J_turb=40,
      J_comp=50,
      J_gear1={0.6,12},
      J_gear2={5,200},
      J_acc=6,
      J_cpl=40,
      J_gen=2500,
      ratio={15057,5067,1500},
      stiff_sh={3,5.5,100,2500,250,200}*1e6,
      stiff_cpl=130*1e6);
  end GasTurbineGear;

  record HydroTurbine "Hydro turbine, example"
    extends PowerSystems.Mechanics.TurboGroups.Parameters.HydroTurbine(
      w_nom=314.15926535898,
      P_nom=20e6,
      J_turb=1000,
      J_shaft=5,
      J_gen=500,
      stiff=300e6);
  end HydroTurbine;

  record Diesel "Diesel, example"
    extends PowerSystems.Mechanics.TurboGroups.Parameters.Diesel(
      w_nom=157.07963267949,
      P_nom=100e3,
      J_turb=20,
      J_gen=20,
      stiff=1e6);
  end Diesel;

  record WindTurbineGear "Wind turbine with gear, example"
    extends PowerSystems.Mechanics.TurboGroups.Parameters.WindTurbineGear(
      w_nom=1.0471975511966,
      P_nom=30e3,
      J_turb=10,
      J_gear={0.3,0.1,0.03},
      J_gen=0.5,
      ratio={1,6,42},
      stiff_sh={16,1}*1e4);
  end WindTurbineGear;
   annotation (preferedView="info",
 Documentation(info="<html>
</html>"));
end Turbines;
