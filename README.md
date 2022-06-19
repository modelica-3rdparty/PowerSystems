# PowerSystems

The library is intended to model electrical power systems at different levels of detail both in transient and steady-state mode.

## Library description

`PowerSystems` provides a framework and examples for the flexible modeling of electical power systems. Generic component models adapt to different application needs by using a replaceable `PhaseSystem`. Moreover the library contains the extensive sets of detailed one phase and transient three phase component models of the former [SPOT](https://github.com/modelica-3rdparty/SPOT) library.

In particular this shall cover systems like:

 * AC power systems, including dc power flow, steady-state, transient, and unsymmetric,
 * Variable frequency systems, e.g. in wind turbines or for drive control, and
 * DC power systems, like HVDC

See also the publication [Franke, Wiesmann: Flexible modeling of electrical power systems - the Modelica PowerSystems library, Modelica conference 2014](https://doi.org/10.3384/ECP14096515).

<img src="PowerSystems/Examples/PowerWorld/Resources/PowerWorld.png" width="500">
<img src="PowerSystems/Examples/Wind/Resources/WindTurbine_DFIG.png" width="300" align="top">

## Current release

Download [PowerSystems v1.0.0 (2018-11-12)](../../archive/v1.0.0.zip)

#### Revisions

 * [Version v1.0.1  (2021-06-30)](../../archive/v1.0.1.zip)
   * Make semiconductor model in Semiconductors.PhaseModules.SwitchModule replaceable.
   * Fix unit in AC3ph.Impedances.Inductor (#43).
   * Added missing 'each' to AC3ph.Machines.Synchron_pm_ctrl (#44).
   * Small clean-ups

 * [Version v1.0.0  (2018-11-12)](../../archive/v1.0.0.zip)
   * Added missing 'each'.
   * Enhance sensors with scaling of output signals.
   * Add steady-state AC3ph PQload.

 * [Version v0.6.0  (2017-01-18)](../../archive/v0.6.0.zip)
   * Enhance AC3ph with replaceable PhaseSystem.
     Many components work with ThreePhase_dq as well, besides the default ThreePhase_dq0.
   * Introduce own SI sub-package to get appropriate display units.
   * Treat PerCent as display unit, 0..1 internally.
   * Implement inertial reference frame for Generic components (#18).
   * Fix some HTML warnings (#20).
   * Fix measurement units in PWM control models (#21).
   * Make phasor of PVImeter unconditional (#26).
   * Unify naming of start values for Loads.
   * Reorganize Examples to better outline the Introductory examples and
     to better reflect the three component sub-packages AC3ph, AC1ph_DC and Generic.

 * [Version v0.5.0  (2016-09-20)](../../archive/v0.5.0.zip)

   This release introduces a couple of changes that improve the modeling experience
   and unify the library with other Modelica libraries.
   * Introduce replaceable model and record types, instead of replaceable
     model and record instances.
   * Simplify initialization
     * Introduce enumeration Types.Dynamics dynType, replacing Booleans stIni_en, steadyIni_t and transientSim
     * Unify naming of start parameters from *_ini to *_start
     * Simplify initialization of machine rotors and line models
   * Rework AC3ph and AC1ph_DC line models: rename PIline to Tline and introduce new PIline.
   * Rework tap changers of AC3ph and AC1ph_DC trafo models: treat effect of tap changers in replaceable Topology, enabling more arrangements such as phase angle regulating.
   * Add examples for electrical drive trains of wind turbines
   * Reorganize Basic package to Utilities.
   * Add Inline=true annotations to functions that shall be inlined.
   * Introduce PhaseSystem Voltage and Current types, including nominal values of 1000.
   * Upgrade Phasor model to standard Modelica graphics.
   * Base on Modelica 3.2.2 instead of 3.2.1 (without changing anything).

 * [Version v0.4.0  (2015-03-14)](../../archive/v0.4.0.zip)
   * fix Generic components to work with simple ThreePhase_d again (was broken in v0.3)
   * rework parameter records (move parameter qualifiers from record members to whole records to permit their construction with functions)
   * remove ambiguous start values
   * lot of clean-up

 * [Version v0.3  (2014-10-20)](../../archive/v0.3.zip)
   * add initial equations to Generic models and related examples
   * add start parameters to AC1phDC and extend transient initialization
   * add start parameters to AC3ph to improve steady-state initialization
   * fix use of condionally declared variables
   * clean up annotations
   * rename dqo to dq0

 * Version v0.2.1  (2014-08-15)
   * replace deprecated classDirectory() with loadResource()
   * fix references to Connections package

 * [Version v0.2 (2013-04-18)](../../archive/v0.2.zip)
   * Clean-up Examples and Resources

 * Version v0.1.3  (2013-02-28)
   * Generic: change connector units from MW to W

 * Version v0.1.2  (2012-12-22)
   * Rework Basic.Types to using SI units
   * Adapt parameter records to SI units

 * Version v0.1.1 (2012-12-15)
   * Rename Utilities package to Basic
   * Remove BasePU and BaseSI sub-packages

 * Version v0.1 (2012-12-06)
   * Initial version

## License

Copyright &copy; 2007-2015, Modelica Association.

This Modelica package is free software and the use is completely at your own risk;
it can be redistributed and/or modified under the terms of the [Modelica License 2](https://modelica.org/licenses/ModelicaLicense2).

## Development and contribution

Contributors:

 * Hansj&uuml;rg Wiesmann (&dagger; 2015): Wrote the original [SPOT](https://github.com/modelica-3rdparty/SPOT) library and supported the creation of the `PowerSystems` library.
 * [Martin Otter](http://www.robotic.dlr.de/Martin.Otter): Converted the original Spot library from Modelica 2 to Modelica 3.
 * [R&uuml;diger Franke](mailto:Ruediger.Franke@de.abb.com): Created the `PowerSystems` library out of the `PowerFlow` concept library and the [SPOT](https://github.com/modelica-3rdparty/SPOT) library.

You may report any issues with using the [Issues](../../issues) button.

Contributions in shape of [Pull Requests](../../pulls) are always welcome.

# Acknowledgement

This work was in parts supported by the ITEA2 MODRIO project by funding of BMBF under contract number ITEA 2 - 11004. Work on the predecessor PowerFlow library was in parts supported by the ITEA2 EUROSYSLIB project by funding of BMBF under contract number ITEA 2 - 06020. Work on the predecessor Spot library was in parts supported by the RealSim project by funding of the IST Programme, Contract No. IST-1999-11979.
