(* ::Package:: *)

BeginPackage["MinAbsEigenvector`"]
Needs["MinAbsEigenvector`Lanczos`"];


(*-|-|-|---Definitions---|-|-|-*)
MinAbsEigenvector::usage = "Returns the eigenvector corresponding to eigenvalue with smallest magnitude";


Begin["`Private`"]


MinAbsEigenvector[H_,tol_:1. 10^-12]:=
With[{
	Hif=LinearSolve[H],
	CF=Function[x,{Sqrt[# . #],Max[Abs[#]]}&[H . x-x . H . x x]]
	},
	Module[
	{
	x=#/Sqrt[# . #]&[LanczosMidSpectrumEigenVector[Hif,H,0.1 tol]],
	n=0,
	cfo,
	cf,
	xo,
	cf2=1.
	},
	xo=x;
	{cf,cf2}=CF[x];
	If[cf2>tol,
		cfo=cf;
		x=MinimizeResidueFunction[Hif,H,x];
		{cf,cf2}=CF[x];
		While[cf<cfo&&n<100&&cf2>tol,
			n++;
			xo=x;
			cfo=cf;
			x=MinimizeResidueFunction[Hif,H,x];
			{cf,cf2}=CF[x];
		];
	];
	xo
]]

(*
After gotten an approximation to vec to an eigenvalue, we minimize the residue function
CF[x_]:=((x.H.x)x-H.x)^2=x.H.H.x+x.x(x.H.x)^2-2(x.H.X)^2
Without loss of generality we can take x.x=1 and \[Epsilon]\[Perpendicular]x and we have 
CF[x+\[Epsilon]]=
x.H.H.x - (x.H.x)^2+(*zeroth order*)
\[Epsilon].H.(H.x-2(x.H.x)x)+(*first order*)
\[Epsilon].H.H.\[Epsilon]+ (x.H.x)((x.H.x)\[Epsilon].\[Epsilon]-2\[Epsilon].H.\[Epsilon])-4(\[Epsilon].H.x)^2+(*etc*)

We define Subscript[P, \[Perpendicular]x]=(#-#.x x)&
g=Subscript[P, \[Perpendicular]x][\.08\.08\.08\.08\.08\.08\.08\.08\.08\.08\.08\.08\.08H.(H.x-2(x.H.x)x)]
When we are close to a small eigenvalue the terms (x.H.x) will be small and so will \[Epsilon].H.x,
so we approximate CF to second order as CFa[x+\[Epsilon]]=const.+ \[Epsilon].g+\[Epsilon].H.H.\[Epsilon]. 
This function has a minima at \[Epsilon]=\.08-PseudoInverse[Subscript[P, \[Perpendicular]x].H.H.Subscript[P, \[Perpendicular]x]].g\[TildeTilde]-Subscript[P, \[Perpendicular]x][A.A.g]
*)
MinimizeResidueFunction[Hif_,H_,x_]:=
With[{hx=H . x},
With[{g=#-# . x x&[H . (hx-2 x . hx x)]},
With[{\[Epsilon]=-(#-# . x x&[Hif[Hif[g]]])},
With[{h\[Epsilon]=H . \[Epsilon]},
With[
{
xhx=x . hx,
\[Epsilon]hx=\[Epsilon] . hx,
\[Epsilon]h\[Epsilon]=\[Epsilon] . h\[Epsilon],
\[Epsilon]2=\[Epsilon] . \[Epsilon],
xh2x=hx . hx,
xh2\[Epsilon]=h\[Epsilon] . hx,
\[Epsilon]h2\[Epsilon]=h\[Epsilon] . h\[Epsilon]
},
#/Sqrt[# . #]&[x+\[Epsilon](t/.FindRoot[(xh2\[Epsilon]+t \[Epsilon]h2\[Epsilon]+2 (-1+t^2 \[Epsilon]2) (\[Epsilon]hx+t \[Epsilon]h\[Epsilon]) (xhx+t (2 \[Epsilon]hx+t \[Epsilon]h\[Epsilon]))+t \[Epsilon]2 (xhx+t (2 \[Epsilon]hx+t \[Epsilon]h\[Epsilon]))^2),{t,1}])]
]]]]];
 \.08 \.08 \.08 \.08 \.08 \.08 \.1f \.1f \.1e \.1e \.1e \.1e \.1e \.08 \.08 \.08


End[];
EndPackage[]
