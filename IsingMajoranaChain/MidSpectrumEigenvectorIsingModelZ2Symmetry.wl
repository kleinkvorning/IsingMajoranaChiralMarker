(* ::Package:: *)

BeginPackage["IsingMajoranaChain`MidSpectrumEigenvectorIsingModelZ2Symmetry`"]
Needs["TensorProductPauliMatricesEtc`"];
Needs["MinAbsEigenvector`"];


(*-|-|-|---Definitions---|-|-|-*)
MapMatrixToPositiveSpace::usage = "MapMatrixToPositiveSpace[H,1] returns H projected to the eigenvalue 1 Z2 symmetry eigen-space where the z2 symmetry operator is assumed to be \!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\)\[CircleTimes]\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\)\[CircleTimes]\[CenterEllipsis].
MapMatrixToPositiveSpace[H,3] is the same but with the symmetry operator assumed to be \!\(\*SubscriptBox[\(\[Sigma]\), \(3\)]\)\[CircleTimes]\!\(\*SubscriptBox[\(\[Sigma]\), \(3\)]\)\[CircleTimes]\[CenterEllipsis].";
PositiveSpaceToFullSpace::usage = "Takes a vector in the positive space and returns a vector in full space.";
MidSpectrumEigenvectorIsingMajoranaChainModel::usage = "Given an association <|\"t\"->parameter_vector, \"m\"->parameter_vector, \"g\"->real_scalar, \"z2\"->1 or 3|> it returns a list of two midspectrum eigenvalue of the IsingMajoranaChainModel with specified parameters and open and periodic boundary conditions respectivly ";
MidSpectrumEigenvector::usage = "MidSpectrumEigenvector[H,z2] returns midspectrum eigen vector given a hamiltonian H and z2=1,3 specifies the Z2 symmetry";
MidSpectrumEigenvectorIsingMajoranaChainModelOBC::usage = "Like MidSpectrumEigenvectorIsingMajoranaChainModel but only open boundary conditions";
MidSpectrumEigenvectorIsingMajoranaChainModelPBC::usage = "Like MidSpectrumEigenvectorIsingMajoranaChainModel but only preiodic boundary conditions";


MidSpectrumEigenvectorIsingMajoranaChainModel::keysmissing="When calling MidSpectrumEigenvectorIsingMajoranaChainModelPBC[data] data needs to have the Keys
\"l\",\"z2\",\"m\",\"t\",\"g\"
and they should have the right properties.
";


Begin["`Private`"]


MapMatrixToPositiveSpace[H_,1]:=With[{dh=Quotient[Length[H],2]},
	0.5(#[[;;dh,;;]]+Reverse[#[[dh+1;;,;;]]]&[H[[;;,;;dh]]+SparseArray[Reverse/@H[[;;,dh+1;;]]]])
]
PositiveSpaceToFullSpace[xP_,1]:=Sqrt[0.5](xP~Join~Reverse[xP]);

Z23M[l_]:=Z23M[l]=SparseArray[SparseArray[#,2^l]&/@(1/2(Nest[Join[#,-#]&,{1,-1},l-1]+1)//SparseArray//ArrayRules)[[;;-2]]];
MapMatrixToPositiveSpace[H_,3]:=Z23M[Log2[Length[H]]] . H . Z23M[Log2[Length[H]]]\[Transpose];
PositiveSpaceToFullSpace[xP_,3]:=Z23M[Log2[Length[xP]]+1]\[Transpose] . xP;


MidSpectrumEigenvectorIsingMajoranaChainModel[data_]:=
If[CheckParameters[data,0],
With[{t=data["t"],\[Mu]=data["m"],l=data["l"],g=data["g"]},
(*I use different basis depending on the sign of \[Delta]. This is not strictly necessary but for large |\[Delta] there will be more nummerical noise otherwise.The basises are realted through the local transformation u=1/Sqrt[2](Id[2]+\[ImaginaryI] Subscript[\[Sigma], 2]).Subscript[\[Sigma], 1] on every site. *)
With[
{
z2=data["z2"](*The pauli-matrix generating the z2 symmetry opertator is Subscript[\[Sigma], 1] or Subscript[\[Sigma], 3]*)
,
o=If[data["z2"]==1,3,1] (*The other Pauli-matrix*)
},
With[{terms=Table[\[Mu][[i]]Subscript[\[Sigma], z2]\[CircleTimes]Id[2^If[i>l-2,l-i,2]]+If[i<l,(t[[i]]Subscript[\[Sigma], o]\[CircleTimes]Subscript[\[Sigma], o]+g Subscript[\[Sigma], z2]\[CircleTimes]Subscript[\[Sigma], z2])\[CircleTimes]Id[If[i>l-2,l-i,2]],0]+If[i<l-1,g Subscript[\[Sigma], o]\[CircleTimes]Id[2]\[CircleTimes]Subscript[\[Sigma], o],0],{i,1,l}]},
With[{Hp=MapMatrixToPositiveSpace[\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(l\)]\(Id[
\*SuperscriptBox[\(2\), \(i - 1\)]]\[CircleTimes]terms[\([i]\)]\[CircleTimes]Id[
\*SuperscriptBox[\(2\), \(l - i - If[i > l - 2, l - i, 2]\)]]\)\),z2]},
With[{HpPBC=Hp+MapMatrixToPositiveSpace[t[[l]]Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-2)]\[CircleTimes]Subscript[\[Sigma], o]+g Subscript[\[Sigma], z2]\[CircleTimes]Id[2^(l-2)]\[CircleTimes]Subscript[\[Sigma], z2]+g Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-3)]\[CircleTimes]Subscript[\[Sigma], o]\[CircleTimes]Id[2]+g Id[2]\[CircleTimes]Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-3)]\[CircleTimes]Subscript[\[Sigma], o],z2]},
{PositiveSpaceToFullSpace[MinAbsEigenvector[Hp],z2],PositiveSpaceToFullSpace[MinAbsEigenvector[HpPBC],z2]}
]]]]]];


MidSpectrumEigenvectorIsingMajoranaChainModelOBC[data_]:=
If[CheckParameters[data,1],
With[{t=data["t"],\[Mu]=data["m"],l=data["l"],g=data["g"]},
(*I use different basis depending on the sign of \[Delta]. This is not strictly necessary but for large |\[Delta] there will be more nummerical noise otherwise.The basises are realted through the local transformation u=1/Sqrt[2](Id[2]+\[ImaginaryI] Subscript[\[Sigma], 2]).Subscript[\[Sigma], 1] on every site. *)
With[
{
z2=data["z2"](*The pauli-matrix generating the z2 symmetry opertator is Subscript[\[Sigma], 1] or Subscript[\[Sigma], 3]*)
,
o=If[data["z2"]==1,3,1] (*The other Pauli-matrix*)
},
With[{terms=Table[\[Mu][[i]]Subscript[\[Sigma], z2]\[CircleTimes]Id[2^If[i>l-2,l-i,2]]+If[i<l,(t[[i]]Subscript[\[Sigma], o]\[CircleTimes]Subscript[\[Sigma], o]+g Subscript[\[Sigma], z2]\[CircleTimes]Subscript[\[Sigma], z2])\[CircleTimes]Id[If[i>l-2,l-i,2]],0]+If[i<l-1,g Subscript[\[Sigma], o]\[CircleTimes]Id[2]\[CircleTimes]Subscript[\[Sigma], o],0],{i,1,l}]},
With[{Hp=MapMatrixToPositiveSpace[\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(l\)]\(Id[
\*SuperscriptBox[\(2\), \(i - 1\)]]\[CircleTimes]terms[\([i]\)]\[CircleTimes]Id[
\*SuperscriptBox[\(2\), \(l - i - If[i > l - 2, l - i, 2]\)]]\)\),z2]},
PositiveSpaceToFullSpace[MinAbsEigenvector[Hp],z2]
]]]]];


MidSpectrumEigenvectorIsingMajoranaChainModelPBC[data_]:=
If[CheckParameters[data,0],
With[{t=data["t"],\[Mu]=data["m"],l=data["l"],g=data["g"]},
(*I use different basis depending on the sign of \[Delta]. This is not strictly necessary but for large |\[Delta] there will be more nummerical noise otherwise.The basises are realted through the local transformation u=1/Sqrt[2](Id[2]+\[ImaginaryI] Subscript[\[Sigma], 2]).Subscript[\[Sigma], 1] on every site. *)
With[
{
z2=data["z2"](*The pauli-matrix generating the z2 symmetry opertator is Subscript[\[Sigma], 1] or Subscript[\[Sigma], 3]*)
,
o=If[data["z2"]==1,3,1] (*The other Pauli-matrix*)
},
With[{terms=Table[\[Mu][[i]]Subscript[\[Sigma], z2]\[CircleTimes]Id[2^If[i>l-2,l-i,2]]+If[i<l,(t[[i]]Subscript[\[Sigma], o]\[CircleTimes]Subscript[\[Sigma], o]+g Subscript[\[Sigma], z2]\[CircleTimes]Subscript[\[Sigma], z2])\[CircleTimes]Id[If[i>l-2,l-i,2]],0]+If[i<l-1,g Subscript[\[Sigma], o]\[CircleTimes]Id[2]\[CircleTimes]Subscript[\[Sigma], o],0],{i,1,l}]},
With[{Hp=MapMatrixToPositiveSpace[\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(l\)]\(Id[
\*SuperscriptBox[\(2\), \(i - 1\)]]\[CircleTimes]terms[\([i]\)]\[CircleTimes]Id[
\*SuperscriptBox[\(2\), \(l - i - If[i > l - 2, l - i, 2]\)]]\)\),z2]},
With[{HpPBC=Hp+MapMatrixToPositiveSpace[t[[l]]Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-2)]\[CircleTimes]Subscript[\[Sigma], o]+g Subscript[\[Sigma], z2]\[CircleTimes]Id[2^(l-2)]\[CircleTimes]Subscript[\[Sigma], z2]+g Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-3)]\[CircleTimes]Subscript[\[Sigma], o]\[CircleTimes]Id[2]+g Id[2]\[CircleTimes]Subscript[\[Sigma], o]\[CircleTimes]Id[2^(l-3)]\[CircleTimes]Subscript[\[Sigma], o],z2]},
	PositiveSpaceToFullSpace[MinAbsEigenvector[HpPBC],z2]
]]]]]];


CheckParameters[data_,BC_(*0 for PBC 1 for OBC*)]:=
Module[{out=False},
	If[
	(And@@(MemberQ[Keys[data],#]&/@{"l","z2","m","t","g"}))
	&&
	NumericQ[data["g"]]
	&&
	Length[data["m"]]>=data["l"]
	&&
	Length[data["t"]]>=data["l"]-BC
	,
		out=True;
	];
	If[out==False,
		Message[MidSpectrumEigenvectorIsingMajoranaChainModel::keysmissing];
	];
	out
];


MidSpectrumEigenvector[H_,z2_:3]:=PositiveSpaceToFullSpace[MinAbsEigenvector[MapMatrixToPositiveSpace[H,z2]],z2];


End[];
EndPackage[]
