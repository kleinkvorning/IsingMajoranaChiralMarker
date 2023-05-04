(* ::Package:: *)

BeginPackage["IsingMajoranaChain`DataBaseGeneration`"]


Needs["IsingMajoranaChain`DataBaseGeneration`Hash`"];
Needs["IsingMajoranaChain`DataBaseGeneration`DataDescription`"];
Needs["IsingMajoranaChain`DataBaseGeneration`AddOrAppenToDeltaValue`"];


AddNewIteration::usage = "AddNewIteration[file,data] adds data to hdf5 file.";


AddNewIteration::keysmissing = 
"When calling AddNewIteration[fl,data] data needs to have the Keys
\"l\",\"BC\",\"MinAbsEigenvector\",\"z2\",\"m\",\"t\",\"delta\",\"g\"
and they should have the right properties.
";
AddNewIteration::conflictingconvetions =
"When calling AddNewIteration[fl,data] there was different convetions in the file `1` and in
the association data.";
AddNewIteration::databasefilenotwritable = "The database file `1` is not writable. It could be open in another application, or it could be corrupt. Aborting.";


Begin["`Private`"]


AddNewIteration[fl_,data0_]:=
Module[{AddedKeys={}},
If[DataTest[data0],
	With[
		{
			data=
				If[MemberQ[Keys[data0],"Hash"],
					data0
				,
					Append[data0,"Hash"->HamToHash[data0["t"],data0["m"]]]
				]
		},
			(*Adds keys /g and /NbrOfSystemSizes if not already present*)
			Quiet[(*Turns off Export::h5dsae*)
				Export[fl,"/g"->data["g"],OverwriteTarget->"Append","AppendMode"->"Preserve"];
				AppendTo[AddedKeys,"/g"];
				Export[fl,"/NbrOfSystemSizes"->0,OverwriteTarget->"Append","AppendMode"->"Preserve"];
				AppendTo[AddedKeys,"/NbrOfSystemSizes"];
			];
			(*Test to see if fl is writable*)
			If[FailureQ[Export[fl,"/g"->Import[fl,"/g"],OverwriteTarget->"Append","AppendMode"->"Overwrite"]],
				Message[AddNewIteration::databasefilenotwritable,fl];
				AddedKeys={};
			,
				(*Add or overwrite /DataDescription*)
				Export[fl,"/DataDescription"->DataDescription,OverwriteTarget->"Append","AppendMode"->"Overwrite"];
				AppendTo[AddedKeys,"/DataDescription"];
				If[Import[fl,"/g"]==data["g"],(*Check if database file is for same interaction term -- if not throw error*)
					With[
						{
							SSTi=
								MapIndexed[#1->#2[[1]]&,
									Import[fl,"/SystemSizes/"<>#<>"/l"]&/@(ToString/@Range[Import[fl,"/NbrOfSystemSizes"]])
								]
						},
						If[MemberQ[SSTi[[;;,1]],data["l"]],(*If systemsize exists*)
							AddedKeys=Join[AddedKeys,
								AddOrAppenToDeltaValue[fl,"SystemSizes/"<>ToString[data["l"]/.SSTi]<>"/"<>data["BC"]<>"/",data]
							];
						,
							AddedKeys=Join[AddedKeys,
								AddOrAppenToDeltaValueSystemSizeDoesNotExists[fl,data]
							];
						];
					];
				,
					Message[AddNewIteration::conflictingconvetions,fl];
					AddedKeys={};
				];
			];
		];
];
If[Or@@(FailureQ/@AddedKeys),
	{}
,
	DeleteDuplicates[AddedKeys]
]
];


DataTest[data_]:=
Module[{out=False},
	If[
	(And@@(MemberQ[Keys[data],#]&/@{"l","BC","MinAbsEigenvector","z2","m","t","delta","g"}))
	&&
	Log2[Length[data["MinAbsEigenvector"]]]==data["l"]
	&&
	NumericQ[data["g"]]
	&&
	Length[data["m"]]>=data["l"]
	&&
	Length[data["t"]]>=(data["l"]-1)
	&&
	(data["BC"]=="PeriodicBC"||data["BC"]=="OpenBC")
	&&
	NumericQ[data["delta"]]
	,
		out=True;
	];
	If[out==False,
		Message[AddNewIteration::keysmissing];
	];
	out
];


End[];
EndPackage[]
