(* ::Package:: *)

BeginPackage["InformationLattice`"]


(*-|-|-|---Definitions---|-|-|-*)
InformationLattice::usage = "Given a state \[Psi] InfoLattice[\[Psi]] returns a pyramid shaped array of the information lattice values.";
InformationPerScale::usage = "Sum the information lattice, i.e., returns a list of I^l";
InformationPerScalePB::usage = "The same as InformationPerScale but for periodic BC";


Begin["`Private`"]


(*---------------InformationLattice---------------*)
InformationLattice[\[Psi]in_]:=
With[{\[Psi]=Normal[\[Psi]in]},
With[{L=Log2[Length[\[Psi]]]},
With[{vnis=Table[Table[VNI[\[Psi],n,l],{n,L-l+1}],{l,L}]},
Table[Table[
	If[l==1,
		vnis[[l,n]]
	,
		If[l==2,
			vnis[[l,n]]-vnis[[l-1,n]]-vnis[[l-1,n+1]]
		,
			vnis[[l,n]]-vnis[[l-1,n]]-vnis[[l-1,n+1]]+vnis[[l-2,n+1]]
		]
	]
,{n,L-l+1}],{l,L}]
]]];

(*Helper functions*)
VNE[\[Rho]_]:=-Abs[#] . Log2[Abs[#]]&[Eigenvalues[\[Rho]]];
VNI[\[Psi]_,n_,l_]:=
With[{L=Log2[Length[\[Psi]]]},
If[l>L/2,
	l-VNE[TO[\[Psi],n,l]]
,
	l-VNE[RDM[\[Psi],n,l]]
]
]

(*RDM gives the density matrix of sites [n,n+l]*)
RDM[\[Psi]_,n_,l_]:=
With[{dC=2^(n-1),dB=2^l},
With[{dA=Quotient[Length[\[Psi]],dC dB]},
Flatten[Transpose[#,{1,3,2}] . ConjugateTranspose[#],{{1,2},{3,4}}]&[
	ArrayReshape[#,{dB,dA dC,1}]&[
		Transpose[#,{2,1,3}]&[
			ArrayReshape[\[Psi],{dA,dB,dC}]
		]
	]
]
]]

(*TO traces out the sites [n,n+l]*)
TO[\[Psi]_,n_,l_]:=
With[{dC=2^(n-1),dB=2^l},
With[{dA=Quotient[Length[\[Psi]],dC dB]},
With[{P=ArrayReshape[\[Psi],{dA,dB,dC}]},Flatten[Transpose[P,{1,3,2}] . ConjugateTranspose[P],{{1,2},{3,4}}]
]]]


InformationPerScale[\[Psi]_]:=Total/@InformationLattice[\[Psi]];


(*--Periodic boundary conditions--*)

InformationPerScalePB[\[Psi]_]:=
With[{ilv=InformationPerScale[\[Psi]]},
With[{l=Length[ilv]},
If[OddQ[l],
ilv[[;;(l+1)/2]]+{0}~Join~Reverse[ilv[[(l+1)/2+1;;]]]
,
ilv[[;;l/2+1]]+{0}~Join~Reverse[ilv[[l/2+2;;]]]~Join~{0}
]
]
]


End[];
EndPackage[]
