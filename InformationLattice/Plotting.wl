(* ::Package:: *)

BeginPackage["InformationLattice`Plotting`"]


(*-|-|-|---Definitions---|-|-|-*)
PiecewiseMonotoneCubicInterpolation::usage = "Returns an interpolation-function which is piecewise monotonic.";
InformationLatticePlot::usage = "";


Begin["`Private`"]


(*---------PiecewiseMonotoneCubicInterpolation---------*)

(*
For reference see
Monotone Piecewise Cubic Interpolation
F. N. Fritsch and R. E. Carlson
https://doi.org/10.1137/0717021
*)

PiecewiseMonotoneCubicInterpolation[f_,xs_]:=
With[{\[CapitalDelta]=(f[[2;;]]-f[[;;-2]])/(xs[[2;;]]-xs[[;;-2]])},
With[{d={0.}~Join~MapThread[G,{\[CapitalDelta][[;;-2]],\[CapitalDelta][[2;;]]}]~Join~{0.}},
	Interpolation[Table[{{xs[[i]]},f[[i]],d[[i]]},{i,Length[f]}]]
]
]

(*Helper function*)
G[S1_,S2_]:=
If[S1<0&&S2<0,
	0.
,
	If[Abs[S2]<Abs[S1],
		Sign[S1] (3Abs[S1 S2])/(Abs[S1]+2Abs[S2])
	,
		If[S1 S2>0,
		(2S1 S2)/(S1+S2)
		,
		0.	
		]
	]
];


InformationLatticePlot[ilv_]:=Show[Plot[PiecewiseMonotoneCubicInterpolation[ilv,Range[Length[ilv]]-1][l],{l,0,Length[ilv]-1},PlotRange->{-0.05 Max[ilv],1.05 Max[ilv]}],
ListPlot[ilv,DataRange->{0,Length[ilv]-1},PlotMarkers->{"\[Cross]",Large},PlotStyle->Red]]


End[];
EndPackage[]
