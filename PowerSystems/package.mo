within ;
package PowerSystems "Library for electrical power systems"
  extends Modelica.Icons.Package;

  import Modelica.Constants.pi;
  import SI = Modelica.SIunits;
  import PowerSystems.Types.SIpu       "Per-Unit types for user interface";
  import PowerSystems.Types;


  package UsersGuide "User's Guide"
    extends Modelica.Icons.Information;

    package Overview "Overview"
      extends Modelica.Icons.Information;
      annotation (
        Documentation(info="<html>
<p>PowerSystems combines a generic concept for the modeling of electrical power systems at different levels of detail with the extensive component models of the former SPOT library. </p>
<p>PowerSystems uses replaceable PhaseSystems to define the voltage and current variables as well as optional supporting reference angles in the connectors. The aim is to have different single and polyphase systems and different mathematical formulations in one framework. In particular this shall cover systems like: </p>
<p><ul>
<li>AC power systems, including dc power flow, steady-state, transient, and unsymmetric,</li>
<li>Variable frequency systems, e.g. in wind turbines or for drive control, and </li>
<li>DC power systems, like HVDC </li>
</ul></p>
<p>A general terminal for electrical power systems can be defined as:</p>
<pre>connector Terminal &QUOT;General power terminal&QUOT;
  replaceable package PhaseSystem = PhaseSystems.PartialPhaseSystem &QUOT;Phase system&QUOT;;
  PhaseSystem.Voltage v[PhaseSystem.n] &QUOT;voltage vector&QUOT;;
  flow PhaseSystem.Current i[PhaseSystem.n] &QUOT;current vector&QUOT;;
  PhaseSystem.ReferenceAngle theta[PhaseSystem.m] &QUOT;optional vector of phase angles&QUOT;;
end Terminal;</pre>
<p>The replaceable PhaseSystem defines the number <code><b>n</b></code> of independent voltage and current components and their representation in the connector. Moreover it defines types for the physical quantities so that terminals of different phase systems cannot be directly connected. </p>
<p>The vector of reference angles <code><b>theta[m]</b></code> allows the definition of a rotating reference system for the description of AC systems with modal components. It is known from the Spot library that this enables the treatment of modal quantities in the time domain, covering transient and unsymmetric systems as well. </p>
<p>The power Terminal is overdetermined with the reference angles though. The operators Connections.root, Connections.potentialRoot, Connections.isRoot and Connections.branch are used for their implementation. A Modelica tool needs to analyze connection graphs and eliminate redundant equations. </p>
The following table summerizes the PhaseSystems that are predefined in the PowerSystems library:
<p/>
<table border=1 cellspacing=0 cellpadding=1>
<tr>
<th>PhaseSystem</th>         <th>n</th> <th>m</th> <th>Comment</th>
</tr>
<tr>
<td>DirectCurrent</td>       <td>1</td> <td>0</td> <td>One voltage and one current component in natural coordinates</td>
</tr>
<tr>
<td>TwoConductor</td>       <td>2</td> <td>0</td> <td>Two voltage and two current components for Spot AC1ph_DC components</td>
</tr>
<tr>
<td>ThreePhase_d</td> <td>1</td> <td>0</td> <td>One modal component for active power &mdash; like DirectCurrent, but converting voltage values to three phase</td>
</tr>
<tr>
<td>ThreePhase_dq</td> <td>2</td> <td>1</td> <td>Two modal components for active and reactive power; one reference angle for frequency &mdash; cf. complex phasors with variable frequency</td>
</tr>
<tr>
<td>ThreePhase_dq0</td>      <td>3</td> <td>2</td> <td>Three modal components for active, reactive and dc power; two reference angles for Spot dq0 components</td>
</tr>
</table>
<p/>
</html>"));
    end Overview;

    package Examples "Examples"
      extends Modelica.Icons.Information;
      annotation (
        Documentation(info="<html>
<p><a href=\"modelica://PowerSystems.Examples.Network\">Examples.Network</a>: The examples NetworkLoop and NetworkOpened are taken from the textbook Oeding, Oswald: Elektrische Kraftwerke und Netze, section 14.2.5: Leistungsfluss in Ringnetzen. The example NetworkControlled additionally investigates frequency/power control in conjunction with the Modelica.Rotational library and a basic EMF (Electro-Motoric Force). </p>
<p><a href=\"modelica://PowerSystems.Examples.PowerWorld\">Examples.PowerWorld</a> models a control area for power distribution in island mode. It was used to demonstrate &QUOT;Stabilization of wind power&QUOT; in the Eurosyslib work package 5.3. See . </p>
<p><a href=\"modelica://PowerSystems.Examples.Spot\">Examples.Spot</a> serve as tutorial and interactive documentation for the detailed component models in AC1ph_DC and AC3ph. </p>
</html>"));
    end Examples;

    package ReleaseNotes "Release notes"
      extends Modelica.Icons.ReleaseNotes;
      annotation (
        Documentation(info="<html>
<p><b>Contributors</b></p>
<ul>
<li>Hansj&uuml;rg Wiesmann (&dagger; 2015):<br>
   Wrote the original Spot library and supported the creation of the PowerSystems library.
</li>
<li><a href=\"mailto:Martin.Otter@dlr.de\">Martin Otter</a>:<br>
   Converted the original Spot library from Modelica 2 to Modelica 3.
</li>
<li><a href=\"mailto:Ruediger.Franke@de.abb.com\">R&uuml;diger Franke</a>:<br>
   Created the PowerSystems library out of the PowerFlow concept library and the Spot library.
</li>
</ul>
</html>", revisions=
            "<html>
<ul>
<li><i>11 Jun 2016</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.5 dev
  <ul>
  <li>Introduce replaceable model and record types, instead of replaceable
      model and record instances.
      This shall unify the look and feel with other Modelica libraries.</li>
  <li>Rework AC3ph and AC1ph_DC line models
  <ul><li>Rename PIline to Tline and introduce new PIline.</li>
      <li>Simplify initialization.</li></ul>
  <li>Upgrade Phasor model to standard Modelica graphics.</li>
  <li>Base on Modelica 3.2.2 instead of 3.2.1 (without changing anything).</li>
  </ul>
</li>
<li><i>14 Mar 2015</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.4.0
  <ul>
  <li>fix Generic components to work with simple ThreePhase_d again (was broken in v0.3)</li>
  <li>rework parameter records (move parameter qualifiers from record members to whole records to permit their construction with functions)</li>
  <li>remove ambiguous start values</li>
  <li>lot of clean-up</li>
  </ul>
</li>
<li><i>20 Oct 2014</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.3
  <ul>
  <li>add initial equations to Generic models and related examples</li>
  <li>add start parameters to AC1phDC and extend transient initialization</li>
  <li>add start parameters to AC3ph to improve steady-state initialization</li>
  <li>fix use of condionally declared variables</li>
  <li>clean up annotations</li>
  <li>rename dqo to dq0</li>
  </ul>
</li>
<li><i>15 Aug 2014</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.2.1
  <ul>
  <li>use Modelica.Utilities.Files.loadResource() instead of deprecated classDirectory()</li>
  <li>fix references to Connections package</li>
  </ul>
</li>
<li><i>18 Apr 2013</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.2
  <ul>
  <li>Clean-up Examples and Resources</li>
  </ul>
</li>
<li><i>28 Feb 2013</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.1.3
  <ul>
  <li>Generic: change connector units from MW to W</li>
  </ul>
</li>
<li><i>22 Dec 2012</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.1.2
  <ul>
  <li>Rework Basic.Types to using SI units</li>
  <li>Adapt parameter records to SI units</li>
  </ul>
</li>
<li><i>15 Dec 2012</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.1.1
  <ul>
  <li>Rename Utilities package to Basic</li>
  <li>Remove BasePU and BaseSI sub-packages</li>
  </ul>
</li>
<li><i>6 Dec 2012</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
     Version 0.1
  <ul>
  <li>Initial version</li>
  </ul>
</li>
</ul>
</html>"));
    end ReleaseNotes;

    annotation(DocumentationClass=true, preferredView="info",
      Documentation(info="<html>
<p>See the subsections below and the linked examples.</p>
<p>See also the publication <a href=\"https://www.modelica.org/events/modelica2014/proceedings/html/submissions/ECP14096515_FrankeWiesmann.pdf\">Franke, Wiesmann: Flexible modeling of electrical power systems -- the Modelica PowerSystems library, Modelica conference 2014</a>.</p>
</html>"));
  end UsersGuide;


  replaceable package PackagePhaseSystem =
      PhaseSystems.ThreePhase_dq "Default phase system"
    annotation (choicesAllMatching=true);

  constant String TableDir=Modelica.Utilities.Files.loadResource("modelica://PowerSystems/Resources/Tables/")
  "Directory of example tables";


  annotation (preferredView="info",
  version="0.5 dev",
  versionDate="2016-06-11",
  Documentation(info="<html>
<p>The Modelica PowerSystems library is intended for the modeling of electrical <b>power systems</b> at different <b>levels of detail</b> both in <b>transient</b> and <b>steady-state</b> mode.</p>
<p>The Users Guide to the library is <a href=\"modelica://PowerSystems.UsersGuide\"><b>here</b></a>.</p>
<p><br/>Copyright &copy; 2007-2016, Modelica Association. </p>
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
  uses(Modelica(version="3.2.2")),
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
