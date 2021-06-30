within PowerSystems;
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
<pre>connector Terminal &quot;General power terminal&quot;
  replaceable package PhaseSystem = PhaseSystems.PartialPhaseSystem &quot;Phase system&quot;;
  PhaseSystem.Voltage v[PhaseSystem.n] &quot;voltage vector&quot;;
  flow PhaseSystem.Current i[PhaseSystem.n] &quot;current vector&quot;;
  PhaseSystem.ReferenceAngle theta[PhaseSystem.m] &quot;optional vector of phase angles&quot;;
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
<p><a href=\"modelica://PowerSystems.Examples.PowerWorld\">Examples.PowerWorld</a> models a control area for power distribution in island mode. It was used to demonstrate &quot;Stabilization of wind power&quot; in the Eurosyslib work package 5.3. See . </p>
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
</html>", revisions="<html>
<ul>
<li><i>30 Jun 2021</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
    Version 1.0.1
  <ul>
  <li>Make semiconductor model in Semiconductors.PhaseModules.SwitchModule replaceable.</li>
  <li>Fix unit in AC3ph.Impedances.Inductor (#43).</li>
  <li>Added missing 'each' to AC3ph.Machines.Synchron_pm_ctrl (#44).</li>
  <li>Small clean-ups</li>
  </ul>
</li>
<li><i>12 Nov 2018</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
    Version 1.0.0
  <ul>
  <li>Added missing 'each'.</li>
  <li>Enhance sensors with scaling of output signals.</li>
  <li>Add steady-state AC3ph PQload.</li>
  </ul>
</li>
<li><i>18 Jan 2017</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
    Version 0.6.0
  <ul>
  <li>Enhance AC3ph with replaceable PhaseSystem.
      Many components work with ThreePhase_dq as well, besides the default ThreePhase_dq0.</li>
  <li>Introduce own SI sub-package to get appropriate display units.</li>
  <li>Treat PerCent as display unit, 0..1 internally.</li>
  <li>Implement inertial reference frame for Generic components (#18)</li>
  <li>Fix some HTML warnings (#20)</li>
  <li>Fix measurement units in PWM control models (#21)</li>
  <li>Make phasor of PVImeter unconditional (#26)</li>
  <li>Unify naming of start values for Loads</li>
  <li>Reorganize Examples to better outline the Introductory examples and
      to better reflect the three component sub-packages AC3ph, AC1ph_DC and Generic.</li>
  </ul>
</li>
<li><i>20 Sep 2016</i>
    by <a href=\"mailto:Ruediger.Franke@de.abb.com\">Ruediger Franke</a>:<br>
    Version 0.5.0<br>
    This release introduces a couple of changes that improve the modeling experience
    and unify the library with other Modelica libraries.
  <ul>
  <li>Introduce replaceable model and record types, instead of replaceable
      model and record instances.</li>
  <li>Simplify initialization
  <ul><li>Introduce enumeration Types.Dynamics dynType, replacing Booleans stIni_en, steadyIni_t and transientSim</li>
      <li>Unify naming of start parameters from *_ini to *_start</li>
      <li>Simplify initialization of machine rotors and line models</li>
  </ul></li>
  <li>Rework AC3ph and AC1ph_DC line models: rename PIline to Tline and introduce new PIline.</li>
  <li>Rework tap changers of AC3ph and AC1ph_DC trafo models: treat effect of tap changers in replaceable Topology, enabling more arrangements such as phase angle regulating.</li>
  <li>Add examples for electrical drive trains of wind turbines</li>
  <li>Reorganize Basic package to Utilities.</li>
  <li>Add Inline=true annotations to functions that shall be inlined.</li>
  <li>Introduce PhaseSystem Voltage and Current types, including nominal values of 1000.</li>
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
