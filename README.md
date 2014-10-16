# PowerSystems

The library is intended to model electrical power systems at different levels of detail both in transient and steady-state mode.

## Library description

`PowerSystems` combines the interface concept of the `PowerFlow` library (developed as part of [Eurosyslib](http://www.eurosyslib.com/) project) with the component models of the [SPOT](https://github.com/modelica-3rdparty/SPOT) library.

`PowerSystems` uses replaceable PhaseSystems to define the voltage and current variables as well as optional supporting reference angles in the connectors. The aim is to support different single and polyphase systems and different mathematical formulations in one framework.

In particular this shall cover systems like:

 * AC systems, including steady-state, transient, and unsymmetric,
 * Variable frequency systems, e.g. in wind turbines or for drive control, and
 * DC power systems, like HVDC

See also the publication [Franke, Wiesmann: Flexible modeling of electrical power systems -- the Modelica PowerSystems library, Modelica conference 2014](https://www.modelica.org/events/modelica2014/proceedings/html/submissions/ECP14096515_FrankeWiesmann.pdf).

![PowerWorld.png](PowerSystems/Examples/PowerWorld/Resources/PowerWorld.png)


## Current release

Download [PowerSystems v0.2 (2013-04-18)](../../archive/v0.2.zip)

#### Revisions

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

Copyright &copy; 2007-2014, Modelica Association.

This Modelica package is free software and the use is completely at your own risk;
it can be redistributed and/or modified under the terms of the [Modelica License 2](https://modelica.org/licenses/ModelicaLicense2).

## Development and contribution

Contributors:

 * [Hansj&uuml;rg Wiesmann](mailto:hj.wiesmann@bluewin.ch): Wrote the original [SPOT](https://github.com/modelica-3rdparty/SPOT) library and supported the creation of the `PowerSystems` library.
 * [Martin Otter](http://www.robotic.dlr.de/Martin.Otter): Converted the original Spot library from Modelica 2 to Modelica 3.
 * [R&uuml;diger Franke](mailto:Ruediger.Franke@de.abb.com): Created the `PowerSystems` library out of the `PowerFlow` concept library and the [SPOT](https://github.com/modelica-3rdparty/SPOT) library.

You may report any issues with using the [Issues](../../issues) button.

Contributions in shape of [Pull Requests](../../pulls) are always welcome.

# Acknowledgement

This work was in parts supported by the ITEA2 MODRIO project by funding of BMBF under contract number ITEA 2 - 11004. Work on the predecessor PowerFlow library was in parts supported by the ITEA2 EUROSYSLIB project by funding of BMBF under contract number ITEA 2 - 06020. Work on the predecessor Spot library was in parts supported by the RealSim project by funding of the IST Programme, Contract No. IST-1999-11979.
