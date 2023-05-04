(* ::Package:: *)

BeginPackage["MinAbsEigenvector`Lanczos`"]


(*-|-|-|---Definitions---|-|-|-*)
LanczosMidSpectrumEigenVector::usage = "LanczosMidSpectrumEigenVector[A,H] with A[x]\[TildeTilde]\!\(\*SuperscriptBox[\(H\), \(-1\)]\).x gives an approximation to the mid-spectrum eigenvector";


Begin["`Private`"]


LanczosMidSpectrumEigenVector[A_,H_,tol_:10^-13]:=(*A[x]=H^-1.x*)
Module[{
continue=True,
v={}, (*The Krylov basis*)
Av,(*A applied to the latest Krylov basis*)
r=SparseArray[Ordering[Normal[Abs[Diagonal[H]]],1]->1.,Length[H]],
(*
What is (after orthogonalization etc) is going to be the new addition to the Krylov basis.
The initial guess is given by the correct vector if the off-diagonal elements where 0.
*)
ro,(*The old r*)
Akb,(*A projected to the Krylov basis*)
\[Beta]={}, (*Off-diagonal elements if Akb*)
\[Alpha]={}, (*Diagonal elements of Akb*)
n=0,(*Counting the number of Lanczos steps*)
res=0,(*Residue norm--measures how good the eigenvalue is*)
reso=1,(*old res*)
res2=1,
relimp=1,(*Relative improvement in res*)
vec,(*Best approximation to smallest eigenvector*)
CF=Function[x,{Sqrt[# . #],Max[Abs[#]]}&[H . x-x . H . x x]]
},
While[continue&&n<100&&res2>tol,(*Run at most 100 steps of if improvement is less than 5%*)
	n++;
	AppendTo[v,r];
	r=A[v[[-1]]];
	Av=r;
	AppendTo[\[Alpha],v[[-1]] . r];
	If[Length[v]>1,
		r-=v[[-2]]\[Beta][[-1]];
	];
	r-=\[Alpha][[-1]]v[[-1]];
	ro=r;
	r=Reorthogonalize[v,r];
	Akb=If[Length[\[Alpha]]>1,SparseArray[{Band[{1,1}]->\[Alpha],Band[{2,1}]->\[Beta],Band[{1,2}]->\[Beta]}],{\[Alpha]}];

	r/=Sqrt[r . r];
	AppendTo[\[Beta],Av . r];
vec=Total[Eigenvectors[Akb][[1]]v];
reso=res;
{res,res2}=CF[vec];
relimp=Abs[reso-res]/(2^-53+Abs[reso]);If[n>10&&relimp<0.05,continue=False;];
];
vec
];



(*Helpers*)
(*I do full-reorthogonalization, since the orthoginalization anyway is not the most time-consuimg part of the algorithm*)
Reorthogonalize[v_,r_]:=
Block[{h,out=r},
With[{rr=r/Sqrt[r . r]},
	Do[
		h=r . vc;
		out-=h vc;
	,{vc,v}];
out
]];


End[];
EndPackage[]
