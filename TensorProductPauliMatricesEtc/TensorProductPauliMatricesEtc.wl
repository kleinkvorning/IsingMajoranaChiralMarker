(* ::Package:: *)

BeginPackage["TensorProductPauliMatricesEtc`"]


(*-|-|-|---Definitions---|-|-|-*)
Id::usage = "I[d] Returns a d\[Cross]d dimensional sparse identiy matrix.";
\[Sigma]::usage = "Subscript[\[Sigma],i] is the sparse i'th PauliMatrix.";
TensorPower::usage = "E.g., TensorPower[x,3]=x\[CircleTimes]x\[CircleTimes]x.";
BandFlatten::usage = "Changes the spectrum of a matrix, such that all eigenvalues larger than 0.5->1 and all values smaller than 0.5->0 ";


Begin["`Private`"]


bf=If[#>0.5,1,0]&;
BandFlatten[\[Rho]in_]:=With[{\[Rho]=Normal[\[Rho]in]},#[[2]]\[Transpose] . (bf/@#[[1]] #[[2]])&@{#[[1]],Orthogonalize[#[[2]]]}&@Eigensystem[\[Rho]]];


(*-|-|-|---Definitions---|-|-|-*)
(*The tensor product*)
CircleTimes[{},y_]:=y;
CircleTimes[x_,{}]:=x;
CircleTimes[x_,y_]/;(MatrixQ[x]&&MatrixQ[y]):=ArrayFlatten[TensorProduct[y,x]];
CircleTimes[x_,y_]/;(VectorQ[x]&&VectorQ[y]):=Flatten[TensorProduct[y,x]];
(*Emposing associativity*)
CircleTimes[x_,y_,z_]:=CircleTimes[CircleTimes[x,y],z];
CircleTimes[x__,y_]:=CircleTimes[CircleTimes[x],y];

(*Pauli-Matrices*)
\[Sigma]/:Subscript[\[Sigma], 0]=Id[2];
\[Sigma]/:Subscript[\[Sigma], 1]=SparseArray[PauliMatrix[1]];
\[Sigma]/:Subscript[\[Sigma], 2]=SparseArray[PauliMatrix[2]];
\[Sigma]/:Subscript[\[Sigma], 3]=SparseArray[PauliMatrix[3]];


(*The sparse identit matrix*)
Id[d_]:=SparseArray[Band[{1,1}]->1.,{d,d}];


TensorPower[x_,0]:={};
TensorPower[x_,1]:=x;
TensorPower[x_,n_]:=TensorPower[x,n-1]\[CircleTimes]x;


End[];
EndPackage[]
