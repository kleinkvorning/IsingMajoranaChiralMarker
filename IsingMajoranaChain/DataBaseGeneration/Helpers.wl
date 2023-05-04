(* ::Package:: *)

BeginPackage["DataBaseGeneration`Helpers`"]


AppendJoinIfNotFailed::usage = "
AppendJoinIfNotFailed[lst1, lst2]

The function takes two lists as input, lst1 and lst2. It checks if lst2 is a Failure object. If lst2 is a Failure object, the function returns $Failed. Otherwise, it joins lst1 and lst2 and returns the combined list.

Parameters:
lst1 (List): The first list to be combined.
lst2 (List): The second list to be combined. If it is a Failure object, the function will return $Failed.

Returns:
List: The combined list if lst2 is not a Failure object.
$Failed: If lst2 is a Failure object.
";


Begin["`Private`"]


AppendJoinIfNotFailed[lst1_,lst2_]:=
If[FailureQ[lst2],
	$Failed
,
	Join[lst1,lst2]
];


End[];
EndPackage[]
