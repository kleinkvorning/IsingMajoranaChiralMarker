(* ::Package:: *)

BeginPackage["IsingMajoranaChain`DataBaseGeneration`AddOrAppenToDeltaValue`CalculateDerivedData`"]
Needs["SingleParticleDensityMatrixAndChiralMarker`"];
Needs["InformationLattice`"];
Needs["TensorProductPauliMatricesEtc`"]


ExportDerivedDataNewIteration::usage = "Given a HDF5 file at location fl and a path to an iteration ExportDerivedDataNewIteration[fl,path] exports the derived data for that iteration.";
ExportUpdatedDisorderAverage::usage = "Given a HDF5 file at location fl and a path to a delta value ExportUpdatedDisorderAverage[fl,path] exports the disordered averaged data for that delta value.";


Begin["`Private`"]


ExportDerivedDataNewIteration[fl_,path_]:=
Module[{AddedKeys={}},
If[StringContainsQ[path,"PeriodicBC"],
	Export[fl,
		path<>"SingleParticleDensityMatrix"->NumericArray[SingleParticleDensityMatrix[Import[fl,path<>"MinAbsEigenvector"],Import[fl,path<>"z2"]]]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"SingleParticleDensityMatrix"];
	Export[fl,
		path<>"MeanChiralMarker"->Mean[ChiralMarker[Import[fl,path<>"SingleParticleDensityMatrix"]]]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"MeanChiralMarker"];
	Export[fl,
		path<>"InformationPerScalePB"->NumericArray[InformationPerScalePB[Import[fl,path<>"MinAbsEigenvector"]]]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"InformationPerScalePB"];
,
	Export[fl,
		path<>"InformationPerScale"->NumericArray[InformationPerScale[Import[fl,path<>"MinAbsEigenvector"]]]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"InformationPerScale"];
];
AddedKeys
];


ExportUpdatedDisorderAverage[fl_,path_]:=
Module[{AddedKeys={}},
If[StringContainsQ[path,"PeriodicBC"],
	(*DisorderAverageMarker and DisorderStandardDeviationMarker*)
	With[{MCM=Import[fl,path<>"Iterations/"<>#<>"/MeanChiralMarker/"]&/@(ToString/@Range[Import[fl,path<>"NbrOfIterations"]])},
		(*DisorderAverageMarker*)
		Export[fl,path<>"DisorderAverageMarker"->Mean[MCM],OverwriteTarget->"Append","AppendMode"->"Overwrite"];
		AppendTo[AddedKeys,path<>"DisorderAverageMarker"];
		(*DisorderStandardDeviationMarker*)
		With[{StandardDeviation=Function[{x},If[Length[x]>1,StandardDeviation[x],0.]]},(*Avoiding crash if an iteration only has one element*)
			Export[fl,path<>"DisorderStandardDeviationMarker"->StandardDeviation[MCM],OverwriteTarget->"Append","AppendMode"->"Overwrite"];
			AppendTo[AddedKeys,path<>"DisorderStandardDeviationMarker"];
		];
	];
	(*DisorderAverageInformationPerScalePB*)
	Export[fl,path<>"DisorderAverageInformationPerScalePB"->
		Mean[Import[fl,path<>"Iterations/"<>#<>"/InformationPerScalePB/"]&/@(ToString/@Range[Import[fl,path<>"NbrOfIterations"]])]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"DisorderAverageInformationPerScalePB"];
	(*DisorderAverageInformationDecayLengthPB*)
	With[
	{
		(*
		Here we bin the information into bins with two scales and then 
		calculate the decay lengt as a linear least square fit in a LogPlot
		*)
		DecayLengthFunction=
			Function[{ilvin},
				With[
				{
					ilv=
						(
							Table[Total[ilvin[[2n-1;;2n]]],{n,1,Quotient[Length[ilvin],2]}]+
							If[OddQ[Length[ilvin]],
								Table[Total[ilvin[[2n;;2n+1]]],{n,1,Quotient[Length[ilvin],2]}]
							,
								0.
							]
						)
				},
					With[{m=Table[{1,i},{i,0,Length[ilv]-1}]},
						-2(Inverse[m\[Transpose] . m] . m\[Transpose] . Log[ilv])[[2]]^-1
					]
				]
			]
	},
		Export[fl,path<>"DisorderAverageInformationDecayLengthPB"->
			DecayLengthFunction[Import[fl,path<>"DisorderAverageInformationPerScalePB"]]
		,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
		AppendTo[AddedKeys,path<>"DisorderAverageInformationDecayLengthPB"];
	];
,(*OpenBC*)
	(*DisorderAverageInformationPerScale*)
	Export[fl,path<>"DisorderAverageInformationPerScale"->
		Mean[Import[fl,path<>"Iterations/"<>#<>"/InformationPerScale/"]&/@(ToString/@Range[Import[fl,path<>"NbrOfIterations"]])]
	,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,path<>"DisorderAverageInformationPerScale"];
	(*DisorderAverageInformationAtLargeScale*)
	With[
	{
		ILS=
			Function[{ilv},
				With[{l=Length[ilv]},
					Total[ilv[[-Quotient[l,3];;]]]+ilv[[-Quotient[l,3]-1]](Mod[l,3]/3)
				]
			]
	},
	Export[fl,path<>"DisorderAverageInformationAtLargeScale"->
		ILS[Normal[Import[fl,path<>"DisorderAverageInformationPerScale"]]]
			,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	];
	AppendTo[AddedKeys,path<>"DisorderAverageInformationAtLargeScale"];
];
AddedKeys
];


End[];
EndPackage[]
