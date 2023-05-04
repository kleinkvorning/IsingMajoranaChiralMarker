(* ::Package:: *)

BeginPackage["IsingMajoranaChain`DataBaseGeneration`AddOrAppenToDeltaValue`"]
Needs["IsingMajoranaChain`DataBaseGeneration`AddOrAppenToDeltaValue`CalculateDerivedData`"];


AddOrAppenToDeltaValue::usage = "Helper function for AddNewIteration";
AddOrAppenToDeltaValueSystemSizeDoesNotExists::usage = "Helper function for AddNewIteration";
AddOrAppenToDeltaValue::hashalreadypresent = "Hash value already present, no data added.";


Begin["`Private`"]


AddOrAppenToDeltaValue[fl_,path_,data_]:=
Module[{AddedKeys={}},		
With[
	{
		\[Delta]Ti=If[Import[fl,path<>"NbrOfdeltaValues"]==0,
				{}
			,
				MapIndexed[
					Round[10#1]->#2[[1]]&,Import[fl,path<>"deltaValues/"<>#<>"/delta"]&/@(ToString/@Range[Import[fl,path<>"NbrOfdeltaValues"]])
				]
			]
	},
	If[MemberQ[\[Delta]Ti[[;;,1]],Round[10 data["delta"]]],(*If delta value already exists the iteration should be added to existing delta value*)
		AddedKeys=Join[AddedKeys,
			AddNewIteration0[fl,path<>"deltaValues/"<>ToString[(Round[10 data["delta"]]/.\[Delta]Ti)]<>"/",data]
		];
	,(*If delta value does not exists new delta value is added*)
		Export[fl,path<>"NbrOfdeltaValues"->Import[fl,path<>"NbrOfdeltaValues"]+1,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
		AppendTo[AddedKeys,path<>"NbrOfdeltaValues"];
		Export[fl,path<>"deltaValues/"<>ToString[Import[fl,path<>"NbrOfdeltaValues"]]<>"/NbrOfIterations"->0,OverwriteTarget->"Append"];
		AppendTo[AddedKeys,path<>"deltaValues/"<>ToString[Import[fl,path<>"NbrOfdeltaValues"]]<>"/NbrOfIterations"];
		Export[fl,path<>"deltaValues/"<>ToString[Import[fl,path<>"NbrOfdeltaValues"]]<>"/delta"->data["delta"],OverwriteTarget->"Append"];
		AppendTo[AddedKeys,path<>"deltaValues/"<>ToString[Import[fl,path<>"NbrOfdeltaValues"]]<>"/delta"];
		AddedKeys=Join[AddedKeys,
			AddNewIteration0[fl,path<>"deltaValues/"<>ToString[Import[fl,path<>"NbrOfdeltaValues"]]<>"/",data]
		];
	];
];
AddedKeys
];			


(*WHen systemsize  not exist we set "NbrOfdeltaValues"->0 and "l" and update "NbrOfSystemSizes" then AddNewDelta*)
AddOrAppenToDeltaValueSystemSizeDoesNotExists[fl_,data_]:=
Module[{AddedKeys={}},
	Export[fl,"/NbrOfSystemSizes"->Import[fl,"/NbrOfSystemSizes"]+1,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
	AppendTo[AddedKeys,"/NbrOfSystemSizes"];
	Export[fl,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"OpenBC"<>"/NbrOfdeltaValues"->0,OverwriteTarget->"Append"];
	AppendTo[AddedKeys,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"OpenBC"<>"/NbrOfdeltaValues"];
	Export[fl,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"PeriodicBC"<>"/NbrOfdeltaValues"->0,OverwriteTarget->"Append"];
	AppendTo[AddedKeys,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"PeriodicBC"<>"/NbrOfdeltaValues"];
	Export[fl,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"l"->data["l"],OverwriteTarget->"Append"];
	AppendTo[AddedKeys,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>"l"];
	AddedKeys=Join[AddedKeys,
		AddOrAppenToDeltaValue[fl,"SystemSizes/"<>ToString[Import[fl,"/NbrOfSystemSizes"]]<>"/"<>data["BC"]<>"/",data]
	];
	AddedKeys
];


AddNewIteration0[fl_,path0_,data_]:=
Module[{AddedKeys={}},
If[!MemberQ[Import[fl,path0<>"Iterations/"<>#<>"/Hash"]&/@(ToString/@Range[Import[fl,path0<>"NbrOfIterations"]]),data["Hash"]],(*Check if Hash value is present (i.e., if the iteration is added a second time by mistake)*)
	With[{nit=Import[fl,path0<>"NbrOfIterations"]},With[{path=path0<>"Iterations/"<>ToString[nit+1]<>"/"},
		(*Updating the number of iterations*)
		Export[fl,
			path0<>"NbrOfIterations"->nit+1
		,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
		AppendTo[AddedKeys,path0<>"NbrOfIterations"];
		Export[fl,path<>#->data[#],OverwriteTarget->"Append"]&/@{"MinAbsEigenvector","z2","t","m","Hash"};
		AppendTo[AddedKeys,path<>#]&/@{"MinAbsEigenvector","z2","t","m","Hash"};
		AddedKeys=Join[AddedKeys,
			ExportDerivedDataNewIteration[fl,path]
		];
		AddedKeys=Join[AddedKeys,
			ExportUpdatedDisorderAverage[fl,path0]
		];
	];];
,
	Message[AddOrAppenToDeltaValue::hashalreadypresent];
	AddedKeys={$Failed};
];
	AddedKeys
];


End[];
EndPackage[]
