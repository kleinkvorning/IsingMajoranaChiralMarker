(* ::Package:: *)

BeginPackage["CostumHDF5Tools`"]


CopyHDF5WithExcludedKey::usage = "CopyHDF5WithExcludedKey[sourceFile, destinationFile, excludedKey] is a function that copies an HDF5 file, sourceFile, to a new file, destinationFile, while excluding a specific key, excludedKey, from the copying process. By default, if destinationFile already exists, it will not be overwritten. However, this behavior can be changed by setting the optional argument OverwriteTarget -> True. If OverwriteTarget -> False is specified and the destination file already exists, a warning message will be displayed, and the function will not proceed with copying.";
CopyHDF5SelectKeys::usage = 
"CopyHDF5SelectKeys[sourceFile, destinationFile, selectKeys] \
copies the specified keys from an HDF5 file (sourceFile) to another HDF5 file (destinationFile). \
The function only copies the data associated with the keys listed in the selectKeys list. \
If any import or export operation fails, an appropriate error message will be displayed.";
CopyHDF5SelectKeys::ImportFailed= "Import failed when trying to import one or more of the keys from `1`.";
CopyHDF5SelectKeys::databasefilenotwritable = "The database file `1` is not writable. It could be open in another application, or it could be corrupt."


Begin["`Private`"]


CopyHDF5SelectKeys[sourceFile_, destinationFile_, selectKeys_]:=
If[Length[selectKeys]>0,
With[{values=Import[sourceFile, #]&/@selectKeys},
	(* Check if any of the imported values have failed *)
	If[Or@@(FailureQ/@values),
  
		(* If any import failed, display a message *)
		Message[CopyHDF5SelectKeys::ImportFailed,sourceFile];
  
	(* If all imports succeeded, continue *)
	, 
  
		(* Export the values to the destination file, overwriting the existing values if necessary *)
		If[Or@@(FailureQ/@MapThread[Export[destinationFile,#1->#2,OverwriteTarget->"Append","AppendMode"->"Overwrite"]&,{selectKeys,values}]),
    
			(* If the export fails, display a message *)
			Message[CopyHDF5SelectKeys::databasefilenotwritable,destinationFile];
		];
  
	(* Close 'Import failure If' statement *)
	];
	
(* Close 'With' statement *)	
];
];


CopyHDF5WithExcludedKey[sourceFile_, destinationFile_, excludedKey_, 
   OverwriteTarget -> True] :=
  With[{
    (* Filters out the excluded keys from the source file *)
    keys = Select[
      Import[sourceFile], ! StringMatchQ[#, "*/*/" <> excludedKey] &]
    },
   (* If the destination file exists, delete it *)
   If[FileExistsQ[destinationFile],
    DeleteFile[destinationFile];
    ];
   (* Export the filtered keys to the destination file *)
   Export[destinationFile, # -> Import[sourceFile, #], 
      OverwriteTarget -> "Append"] & /@ keys;
   ];

CopyHDF5WithExcludedKey[sourceFile_, destinationFile_, excludedKey_] :=
  (* Check if the destination file exists *)
  If[FileExistsQ[destinationFile],
   (* If it exists, display a warning message *)
   Message[CopyFile::filex, destinationFile];
   ,
   (* If it doesn't exist, proceed with copying and overwriting *)
   CopyHDF5WithExcludedKey[sourceFile, destinationFile, excludedKey, 
     OverwriteTarget -> True];
   ];

CopyHDF5WithExcludedKey[sourceFile_, destinationFile_, excludedKey_, 
   OverwriteTarget -> False] :=
  (* If OverwriteTarget is set to False, just call the function without the OverwriteTarget option *)
  CopyHDF5WithExcludedKey[sourceFile, destinationFile, excludedKey];


End[];
EndPackage[]
