within PowerSystems.Control;
package Relays "Relays"
  extends Modelica.Icons.VariantsPackage;

  block SwitchRelay "Relay for sequential switching "
    extends PowerSystems.Basic.Icons.Block0;

    parameter Integer n(min=1)=3 "number of signals";
    parameter Integer switched[:]=1:n "switched signals";
    parameter Boolean ini_state=true "initial state (closed true, open false)"
      annotation(choices(choice=true "closed", choice=false "open"));
    parameter SI.Time t_switch[:]={1} "switching time vector";
    Modelica.Blocks.Interfaces.BooleanOutput[n] y(start=fill(ini_state, n), each fixed=true)
      "boolean state of switch (closed:true, open:false)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  protected
    Integer cnt(start=1,fixed=true);

  algorithm
    when time > t_switch[cnt] then
      cnt := min(cnt + 1, size(t_switch, 1));
      for k in switched loop
        y[k] := not y[k];
      end for;
    end when;
    annotation (defaultComponentName = "relay1",
      Documentation(
              info="<html>
<p>Allows choosing the phases that will be switched at defined time-events t_switch (finite length vector).</p>
<p>The switching sequence is one of
<pre>
  closed - open - closed - ...
  open - closed - open - ...
</pre></p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "switch")}));
  end SwitchRelay;

  block TapChangerRelay "Relay for setting tap-changer "
    extends PowerSystems.Basic.Icons.Block0;

    parameter Integer preset_1[:]={1}
      "1: positions tap changer";
    parameter Integer preset_2[:]={1}
      "2: positions tap changer";
    parameter SI.Time t_switch_1[:]={0.5} "1: switching times";
    parameter SI.Time t_switch_2[:]={0.5} "2: switching times";
    Modelica.Blocks.Interfaces.IntegerOutput tap_1(start = preset_1[1])
      "positions of tap changer 1"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    Modelica.Blocks.Interfaces.IntegerOutput tap_2(start = preset_2[1])
      "positions of tap changer 2"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
  protected
    Integer cnt_1(start=1,fixed=true);
    Integer cnt_2(start=1,fixed=true);

  algorithm
    when time > t_switch_1[min(cnt_1, size(t_switch_1, 1))] then
      cnt_1 := cnt_1 + 1;
      tap_1 := preset_1[min(cnt_1, size(preset_1, 1))];
    end when;
    when time > t_switch_2[min(cnt_2, size(t_switch_2, 1))] then
      cnt_2 := cnt_2 + 1;
      tap_2 := preset_2[min(cnt_2, size(preset_2, 1))];
    end when;
    annotation (defaultComponentName = "tapRelay1",
      Documentation(
              info="<html>
<p>The tap changer positions are pre-selected. The voltage change per position 
and the neutral position for nominal voltage are defined in the transformer data.</p>
<p>The switching times can be chosen arbitrarily.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{50,50},{70,30}},
            lineColor={255,128,0},
            textString=
                 "2"),
          Text(
            extent={{50,-30},{70,-50}},
            lineColor={255,128,0},
            textString=
                 "1"),
          Text(
            extent={{-80,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "tap")}));
  end TapChangerRelay;

  block TapChanger3Relay "Relay for setting tap-changer 3-winding transformer"
    extends PowerSystems.Basic.Icons.Block0;

    parameter Integer preset_1[:]={1}
      "1: positions tap changer";
    parameter Integer preset_2a[:]={1}
      "2a: positions tap changer";
    parameter Integer preset_2b[:]={1}
      "2b: positions tap changer";
    parameter SI.Time t_switch_1[:]={0.5} "1: switching times";
    parameter SI.Time t_switch_2a[:]={0.5} "2a: switching times";
    parameter SI.Time t_switch_2b[:]={0.5} "2b: switching times";
    Modelica.Blocks.Interfaces.IntegerOutput tap_1(start = preset_1[1])
      "positions of tap changer 1"
      annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
    Modelica.Blocks.Interfaces.IntegerOutput[2] tap_2(
      start = {preset_2a[1], preset_2b[1]})
      "positions of tap changers {2a,2b}"
      annotation (Placement(transformation(extent={{90,30},{110,50}})));
  protected
    Integer cnt_1(start=1,fixed=true);
    Integer cnt_2a(start=1,fixed=true);
    Integer cnt_2b(start=1,fixed=true);

  algorithm
    when time > t_switch_1[min(cnt_1, size(t_switch_1, 1))] then
      cnt_1 := cnt_1 + 1;
      tap_1 := preset_1[min(cnt_1, size(preset_1, 1))];
    end when;
    when time > t_switch_2a[min(cnt_2a, size(t_switch_2a, 1))] then
      cnt_2a := cnt_2a + 1;
      tap_2[1] := preset_2a[min(cnt_2a, size(preset_2a, 1))];
    end when;
    when time > t_switch_2b[min(cnt_2b, size(t_switch_2b, 1))] then
      cnt_2b := cnt_2b + 1;
      tap_2[2] := preset_2b[min(cnt_2b, size(preset_2b, 1))];
    end when;
    annotation (defaultComponentName = "tapRelay1",
      Documentation(
              info="<html>
<p>The tap changer positions are pre-selected. The voltage change per position 
and the neutral position for nominal voltage are defined in the transformer data.</p>
<p>The switching times can be chosen arbitrarily.</p>
</html>
"),   Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Text(
            extent={{50,50},{70,30}},
            lineColor={255,128,0},
            textString=
                 "2"),
          Text(
            extent={{50,-30},{70,-50}},
            lineColor={255,128,0},
            textString=
                 "1"),
          Text(
            extent={{-80,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "tap")}));
  end TapChanger3Relay;

  block Y_DeltaControl "Relay for Y-Delta topology switching "
    extends PowerSystems.Basic.Icons.Block0;

    parameter Boolean ini_state=true "initial state (Y true, D false)"
      annotation(choices(choice=true "Y", choice=false "Delta"));
    parameter SI.Time t_switch[:]={1} "switching time vector";
    Modelica.Blocks.Interfaces.BooleanOutput y(start=ini_state, fixed=true)
      "boolean state (Y-top: true, Delta-top: false)"
      annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  protected
    Integer cnt(start=1,fixed=true);

  algorithm
    when time > t_switch[cnt] then
      cnt := min(cnt + 1, size(t_switch, 1));
      y := not y;
    end when;
    annotation (defaultComponentName = "relay1",
      Documentation(
              info="<html>
<p>Allows choosing Y or Delta-topology at defined time-events t_switch (finite length vector).</p>
<p>The switching sequence is one of
<pre>
  Y - Delta - Y - ...
  Delta - Y - Delta - ...
</pre></p>
</html>"),
      Icon(coordinateSystem(
          preserveAspectRatio=false,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={Text(
            extent={{-80,20},{80,-20}},
            lineColor={128,128,128},
            textString=
                 "Y - D")}));
  end Y_DeltaControl;
  annotation (preferredView="info",
Documentation(info="<html>
</html>"));
end Relays;
