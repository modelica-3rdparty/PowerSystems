within ;
package PowerSystems "Library for electrical power systems"
  extends Modelica.Icons.Package;

  import Modelica.Constants.pi;
  import PowerSystems.Types.SI "SI types with custom display units";
  import PowerSystems.Types.SIpu "per-unit types for user interface";

  constant String TableDir=Modelica.Utilities.Files.loadResource("modelica://PowerSystems/Resources/Tables/")
    "Directory of example tables";

  replaceable package PackagePhaseSystem =
      PhaseSystems.ThreePhase_dq "Default phase system"
    annotation (choicesAllMatching=true);

  annotation (preferredView="info",
  version="1.0.1",
  versionDate="2021-06-30",
  Documentation(info="<html>
<p>The Modelica PowerSystems library is intended for the modeling of electrical <b>power systems</b> at different <b>levels of detail</b> both in <b>transient</b> and <b>steady-state</b> mode.</p>
<p>The Users Guide to the library is <a href=\"modelica://PowerSystems.UsersGuide\"><b>here</b></a>.</p>
<p><br/>Copyright &copy; 2007-2018, Modelica Association. </p>
<p>Copyright &copy; 2004-2008, H.J. Wiesmann (&dagger; 2015).</p>
<p><i>This Modelica package is <b>Open Source</b> software; it can be redistributed and/or modified
under the terms of the <b>Modelica license, version 2.0, see the license conditions and
the accompanying disclaimer <a href=\"modelica://Modelica.UsersGuide.ModelicaLicense2\">here</a>.</b></i> </p>
<p><i>This work was in parts supported by the ITEA2 MODRIO project by funding of BMBF under contract
number ITEA 2 - 11004. Work on the predecessor PowerFlow library was in parts supported by
the ITEA2 EUROSYSLIB project by funding of BMBF under contract number ITEA 2 - 06020.
Work on the predecessor Spot library was in parts supported by the RealSim project
by funding of the IST Programme, Contract No. IST-1999-11979. </i></p>
<p/>
</html>
"),
  uses(Modelica(version="3.2.3")),
  Icon(graphics={
      Line(
        points={{-60,-16},{38,-16}},
        color={0,0,0},
        smooth=Smooth.None),
      Line(
        points={{-60,-16},{-60,-42}},
        color={0,0,0},
        smooth=Smooth.None),
      Line(
        points={{38,-16},{38,-42}},
        color={0,0,0},
        smooth=Smooth.None),
      Line(
        points={{-10,10},{-10,-16}},
        color={0,0,0},
        smooth=Smooth.None),
      Ellipse(extent={{-20,30},{0,10}}, lineColor={0,0,0}),
      Ellipse(extent={{-20,42},{0,22}}, lineColor={0,0,0}),
      Ellipse(extent={{-70,-42},{-50,-62}}, lineColor={0,0,0}),
      Ellipse(extent={{28,-42},{48,-62}}, lineColor={0,0,0}),
      Line(
        points={{-10,52},{-10,42}},
        color={0,0,0},
        smooth=Smooth.None)}));
end PowerSystems;
