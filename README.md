# PowerSystems - Ceraolo's fork

A few words about the rationale of this fork. As a professor of Electric power Systems and a heavy Modelica user, I consider Power System Library a very important achievement of the Modelica community. However, personally I've had several problems in understanding how to use this library, and I think that also other users may encounter similar problems.

My the difficulties in understanding this library were due to both the documentation (that I consider too short) and the library whose structure and organization, in my opinion, can be improved.

Therefore I consider this fork as a lab to test my ideas and what I think are improvements. I will continue to contribute to the "official" Power Systems Library with tichets and comments to tickets.


NOTES ABOUT COMMITS

1)  ELIMINATION OF GENERIC SUB-LIBRARY.
Power systems has two kinds of models: Generic and Other. The latter come from the original SPOT library. I think that for the purpose of my fork, having two kinds of models is too much. I see  that the Generic models are much simpler than the others, and are too simple, in my opinion, for the majority of needs a  Power System Engineer usually has. Therefore I chose to concentrate on the other models (i.e. those that come from the SPOT library)

2) ADAPTATION OF EXAMPLES' SHEET SIZE TO THE MODEL SIZE
 Tools use the size of the model's sheet to choose the display default size of the models. Tools (e.g. Dymola and OpenModelica) as default arrange things so that everything in the shett is visible. So, If models have large blank parts, then the important part off the display is smaller tnan necessary, often tiny. This is particular important when in Dymola's simulation mode the model diagram is displayed.
  Blank parts will be thus progressively eliminated to enhance model's display in tools, at least those mentioned above.
