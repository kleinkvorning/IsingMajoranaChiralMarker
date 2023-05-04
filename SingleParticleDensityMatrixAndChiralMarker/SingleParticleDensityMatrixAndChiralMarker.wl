(* ::Package:: *)

BeginPackage["SingleParticleDensityMatrixAndChiralMarker`"]
Needs["TensorProductPauliMatricesEtc`"];


SingleParticleDensityMatrix::usage = "Returns the BdG single-particle density matrix with chiral symmetry S=Id[l]\[CircleTimes]\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\). By default the fermion parity operator is assumed to bed \[Sigma]3\[CircleTimes]\[Sigma]3... to instead use the convention \[Sigma]1\[CircleTimes]\[Sigma]1... give secondargument, as SingleParticleDensityMatrix[\[Psi],1]";
ChiralMarker::usage = "ChiralMarker[n,\[Rho]SP], gives the chiral marker at position n given a BandFlattened single particle density matrix";


Begin["`Private`"]


\[Psi][J_,l_,z2_]:=
With[{j=If[J<=l,J,J-l],s=If[J<=l,1,-1]},
op[j,l,s,z2]
]
\[Psi]d[J_,l_,z2_]:=
With[{j=If[J<=l,J,J-l],s=If[J<=l,-1,1]},
-op[j,l,s,z2]
]
op[j_,l_,s_,z2_]:=op[j,l,s,z2]=1/2 (TensorPower[Subscript[\[Sigma], z2],j-1]\[CircleTimes](I (-1)^((z2+1)/2) Subscript[\[Sigma], 2]+s Subscript[\[Sigma], If[z2==1,3,1]])\[CircleTimes]Id[2^(l-j)]);

\[Rho]SP[II_,J_,z2_][x_]:=
With[{l=Log2[Length[x]]},
With[{i=If[II<=l,II,II-l],j=If[J<=l,J,J-l]},
(x . \[Psi]d[II,l,z2]) . (\[Psi][J,l,z2] . x)
]]

SingleParticleDensityMatrix[\[Psi]_,z2_:3]:=
With[{x=Normal[\[Psi]]},
With[{l=Log2[Length[x]]},
	Table[\[Rho]SP[II,J,z2][x],{II,2l},{J,2l}]
]
];


ChiralMarker[xx_,\[Rho]SPin_]:=
With[{\[Rho]SP=BandFlatten[Normal[\[Rho]SPin]]},
With[{l=Length[\[Rho]SP]/2},
With[{xop=Join[#,#]&[Table[Mod[x+(l-1)/2-xx,l],{x,0,l-1}]],S=Id[l]\[CircleTimes]Subscript[\[Sigma], 1]},
-2\[Rho]SP[[xx+1]] . S . (xop \[Rho]SP[[xx+1]])-2\[Rho]SP[[xx+l+1]] . S . (xop \[Rho]SP[[xx+l+1]])
]]];

ChiralMarker0[xx_,\[Rho]SP_]:=
With[{l=Length[\[Rho]SP]/2},
With[{xop=Join[#,#]&[Table[Mod[x+(l-1)/2-xx,l],{x,0,l-1}]],S=Id[l]\[CircleTimes]Subscript[\[Sigma], 1]},
-2\[Rho]SP[[xx+1]] . S . (xop \[Rho]SP[[xx+1]])-2\[Rho]SP[[xx+l+1]] . S . (xop \[Rho]SP[[xx+l+1]])
]];

ChiralMarker[\[Rho]SP_]:=ChiralMarker0[#,BandFlatten[Normal[\[Rho]SP]]]&/@(Range[Length[\[Rho]SP]/2]-1);


End[];
EndPackage[]
