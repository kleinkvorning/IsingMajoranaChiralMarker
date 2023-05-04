(* ::Package:: *)

BeginPackage["IsingMajoranaChain`DataBaseGeneration`DataDescription`"]


DataDescription::usage = "The data description as a string.";


Begin["`Private`"]


DataDescription=
"Enclosed are eigenvectors corresponding to min abs eigenvalues of the Ising-
Majorana model together with various data derived from these vectors.

INTERACTION PARAMETER
All data is calculated with the interaction parameter g=\"g\".

GROUP STRUCTURE
All data can is in the group \"SystemSizes\\\" which contain subgroups
\"SystemSizes\\1\\\",\"SystemSizes\\2\\\" etc.These groups contain the the
Key \"l\" which specifies the number of sites in that system-size and two sub-
groups OpenBC and PeriodicBC.These containing data for open and periodic
Hamiltonians.They contain the key \"NbrOfdeltaValues\" which specify how many
Delta values there are for the given systemize and boundary condition.All
data is in the group \"deltaValues\" which contain subgroups \"1\",\"2\",etc.
They in turn contain the key \"delta\" which specify the delta value and
\"NbrOfIterations\" which specify the number of iterations for the given delta
value.It also contain disordered averaged data which is specific to each
Boundary condition and should be self-explained by the Key.Finally,it
Contains the group Iteration which contains the subgroups \"1\",\"2\",etc,
one for each iteration. Each of these subgroups contain data specific to the
iteration.

MORE INFORMATION
More details about the model and conventions used are found in the 
\"ModelAndConventions.pdf\".
";


End[];
EndPackage[]
