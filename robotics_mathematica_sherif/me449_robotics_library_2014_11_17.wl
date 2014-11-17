(* ::Package:: *)

BeginPackage["RoboticLibrarySM`"]; 

RotInv::usage=
 "Takes a rotation matrix as input and returns the inverse, by transposing it; a commonly used porperty for rotation matrices.";

VecToSkew::usage=
"Takes a 3-vector q and returns the 3x3 skew-symmetric representation [q]";

SkewToVec::usage=
"Takes a 3x3 skew-symmetric matrix [q] and returns the corresponding 3-vector q";

ExpToRot::usage="Takes exponential coordinates \[Omega] and \[Theta] (sometimes written \[Omega]\[Theta]) and returns
the corresponding rotation matrix R = exp [\[Omega]] according to Rodrigues' formula. ExpToRot 
also normalizes \[Omega] before evaluating. If \[Theta] is ommited, the default value for \[Theta] is assumed to be 1.";

RotToExp::usage=
"Takes a rotation matrix R \[Element] SO(3) and returns the exponential coordinates \[Omega]\[Theta] where \[Omega] is the normalized rotation axis and \[Theta] 
is the time traveled to complete the rotation. Calculations are based on the matrix logarithm [\[Omega]] = log R. If the calculated
\[Theta] is equal to zero, then there is no rotation. The input is not a rotation matrix and could ie. be the identity matrix.";

\[Omega]bTo\[Omega]a::usage="Takes an angular velocity \[Omega]_b in {b} and a rotation matrix R_ab representing
{b} in {a} and returns \[Omega]_a in {a}";

TransfInv::usage="Takes a 4x4 transformation matrix as input and returns the inverse ";

TwistToMat::usage="Takes a spatial velocity (twist) V = [\[Omega], v] T and returns [V]";

MatToTwist::usage="Takes [V] and returns V";

ExpToTrans::usage="Takes exponential coordinates V (sometimes written S\[Theta]) and returns the
corresponding transformation matrix T = exp[V]. TwistToExp also normalizes V before evaluating.
Normalizing is computed according to the rotational component (or according to the translational 
compontent if there is no rotation. If \[Theta] is ommited, the default value for \[Theta] is assumed to be 1";

TransToExp::usage="Takes a transformation matrix T \[Element] SE(3) and calculates the exponential
coordinates V based on the matrix logarithm [V] = log T. The exponential coordinates V=S\[Theta] 
are returned in terms of the normalized screw axis S, as well as the net rotation \[Theta] about the
screw axis";

AdjTrans::usage="Takes a transformation matrix T and returns the matrix form of the adjoint map [AdT]";

VbToVa::usage="Takes a twist Vb in {b} and a transformation matrix Tab representing {b}
in {a} and returns Va in {a}";

SiToTsb::usage="Takes the home configuration M = Tsb(0) of a manipulator\[CloseCurlyQuote]s end-effector,
a list of world-fixed screw axes Si in the space frame corresponding to
the joint motions, and a list of joint displacements \[Theta] = (\[Theta] 1, . . . \[Theta]n), and
calculates the configuration of the end-effector when the robot is at these
joint coordinates Tsb(\[Theta]) (i.e., the forward kinematics).";

BiToTsb::usage="Takes the home configuration M = Tsb(0) of a manipulator\[CloseCurlyQuote]s end-effector,
a list of end-effector-fixed screw axes Bi in the end-effector body frame
corresponding to the joint motions, and a list of joint displacements \[Theta] =
(\[Theta] 1, . . . \[Theta]n), and calculates the configuration of the end-effector when the
robot is at these joint coordinates Tsb(\[Theta]) (i.e., the forward kinematics)";

SiToJs\[Theta]::usage="Calculates the space Jacobian, given the joint screw axes Si represented in 
the space frame, as well as joint angles.";

BiToJb\[Theta]::usage="Calculates the body Jacobian, given the joint screw axes Bi represented in 
the body frame, as well as joint angles.";

Jb\[Theta]ToJs\[Theta]::usage="Transforms a body Jacobian into a space jacobian. 
Required inputs: Home configuration M, space frame joint screw-axes Si, 
joint angles \[Theta] and a body Jacobian Jb\[Theta]";

InvKinemBody::usage="Given a home configuration 'M', screw axes 'Bi' (represented in the 
body Frame), a goal configuration 'Tsd'(represented as a transformation Tsd \[Epsilon] SE(3) in 
the space frame), an initial joint-angle guess '\[Theta]guess', and a maximum total error '\[Epsilon]', 
InvKinemBody performs numerical inverse kinemtics using the body frame Jacobian until the
total error requirements are met.

After each iteration of the numerical method, it prints the joint angle guess and the (x, y) 
position of the end-effector. Finally, it returns the first joiint angle guess, that 
fulfills the total error requirements. The computation is stopped.
";




Begin["`Public`"];

RotInv[R_]:= 
	Module[{RInv},
		If[R\[Transpose].R==IdentityMatrix[Length[R]] && Det[R]==1, 
			RInv=R\[Transpose]; (*Is only used if R is truly a member of a special orthogonal group SO. See above requirements*)
			Return[RInv];
		];
		If[R\[Transpose].R!=IdentityMatrix[Length[R]] || Abs[Det[R]]!= 1,
			Print["The matrix entered is not a member of a special orthogonal group and therefor cannot be a rotation matrix!"];
			Return[Null];
		];
	]

VecToSkew[q_]:= 
	Module[{qSkew},
		qSkew= {{0,-q[[3]], q[[2]]},{q[[3]],0, -q[[1]]},{-q[[2]],q[[1]],0}};
		Return[qSkew];
	]

SkewToVec[qSkew_]:= 
	Module[{q},
		q= {qSkew[[3,2]],qSkew[[1,3]],qSkew[[2,1]]};
		Return[q];
	]

ExpToRot[\[Omega]_,\[Theta]_:1]:= 
	Module[{R,\[Omega]norm,\[Theta]norm},
		\[Omega]norm=Normalize[\[Omega]]; (*\[Omega]=\[Omega]/|\[Omega]|*)
		\[Theta]norm=Norm[\[Omega]]*\[Theta]; (*Adjust the original \[Theta] from the function arguments*)
		(*Integrate the rotation with a normalized angular velocity \[Omega]norm and angle \[Theta]norm using Rodrigues' formula*)
		R=IdentityMatrix[3]+Sin[\[Theta]norm]VecToSkew[\[Omega]norm]+(1-Cos[\[Theta]norm])VecToSkew[\[Omega]norm].VecToSkew[\[Omega]norm]; 
		Return[R];
	];

RotToExp[R_]:=
	Module[{\[Theta],\[Omega]},
		\[Theta]=ArcCos[(Tr[R]-1)/2];
		If[\[Theta]==0 ,
			Print["The angle of this rotation is zero! Did you input the identity matrix?"];
			Return[Null];
		];

	   If[\[Theta]<0 || \[Theta]>\[Pi],
			Print["The calculated \[Theta] seems to be outside the range of [0,\[Pi]]"];
			Return[Null];
		];

	If[0 < \[Theta] <\[Pi],
			\[Omega]={R[[3,2]]-R[[2,3]],R[[1,3]]-R[[3,1]],R[[2,1]]-R[[1,2]]}/(2*Sin[\[Theta]]);
			Return[{\[Omega],\[Theta]}];
		];

	If[\[Theta] ==\[Pi],
		If [R[[3,3]]!=-1,
			\[Omega]=1/Sqrt[2(1+R[[3,3]])] {R[[1,3]],R[[2,3]],1+R[[3,3]]};
			Return[{\[Omega],\[Theta]}]; 
				];

		If [R[[1,1]]!=-1,
			\[Omega]=1/Sqrt[2(1+R[[1,1]])] {1+R[[1,1]],R[[2,1]],R[[3,1]]};
				Return[{\[Omega],\[Theta]}];
			];

		If [R[[2,2]]!=-1,
			\[Omega]=1/Sqrt[2(1+R[[2,2]])] {R[[1,2]],1+R[[2,2]],R[[3,2]]};
				Return[{\[Omega],\[Theta]}];
			];
		];
	]

\[Omega]bTo\[Omega]a[R_,\[Omega]b_]:=
	Module[{\[Omega]a},
		\[Omega]a=R.\[Omega]b;
		Return[\[Omega]a];
	]

TransfInv[T_]:=
	Module[{R,p,TInv},
		R=T[[1;;3,1;;3]];
		p=T[[1;;3,4]];
		TInv=Insert[Insert[R,-R\[Transpose].p,4]\[Transpose],{0,0,0,1},4];
		Return[TInv];
	]

TwistToMat[V_]:=
	Module[{VMat},
		VMat=Insert[Insert[VecToSkew[V[[1;;3]]]\[Transpose],V[[4;;6]],4]\[Transpose],{0,0,0,0},4];
		Return[VMat];
	]

MatToTwist[VMat_]:=
	Module[{V},
		V=Join[SkewToVec[VMat[[1;;3,1;;3]]],VMat[[1;;3,4]]];
		Return[V];
	]

ExpToTrans[V_,\[Theta]_:1]:=
	Module[{\[Theta]norm, \[Theta]new, \[Omega]norm, vnorm, \[Omega]Skew, ExpV},
		If[V[[1;;3]]!={0,0,0}, (*If \[Omega] is a non-zero vector*)
			\[Theta]norm=Norm[V[[1;;3]]]; (*Set a variable '\[Theta]norm' equal to the magnitude of the angular velocity \[Omega]*)
			\[Theta]new=\[Theta]norm*\[Theta]; (*Adjust the original \[Theta] from the function arguments*)
			\[Omega]norm=V[[1;;3]]/\[Theta]norm; (*Normalize \[Omega]. \[Omega]norm = \[Omega]/|\[Omega]|*)
			vnorm=V[[4;;6]]/\[Theta]norm; (*Normalize v. vnorm=v/|\[Omega]|*)
			\[Omega]Skew=VecToSkew[\[Omega]norm]; (*Skew-symmetric representation of the normalized angular velocity*)
			(*Evaluate the equation from the lecture notes with the normalized velocities, as well as the adjusted \[Theta]*)
			ExpV=Insert[Insert[ExpToRot[\[Omega]norm,\[Theta]new]\[Transpose],(IdentityMatrix[3]\[Theta]new+(1-Cos[\[Theta]new])\[Omega]Skew+(\[Theta]new-Sin[\[Theta]new])\[Omega]Skew.\[Omega]Skew).vnorm,4]\[Transpose],{0,0,0,1},4];
			Return[ExpV];
		];
		If[V[[1;;3]]=={0,0,0},(*If \[Omega] is a zero-vector*)
			\[Theta]norm=Norm[V[[4;;6]]]; (*Set a variable '\[Theta]norm' equal to the magnitude of the translational velocity v*)
			\[Theta]new=\[Theta]norm*\[Theta]; (*Adjust the original \[Theta] from the function arguments*)
			\[Omega]norm=V[[1;;3]]/\[Theta]norm; (*Normalize \[Omega]. \[Omega]norm = \[Omega]/|v|.  Returns {0,0,0}!*)
			vnorm=V[[4;;6]]/\[Theta]norm; (*Normalize v. vnorm=v/|v|*)
			\[Omega]Skew=VecToSkew[\[Omega]norm]; (*Skew-symmetric representation of the normalized angular velocity. Retutns a 0 matrix*)
			(*ExpV=Insert[Insert[ExpToRot[\[Omega]norm,\[Theta]new]\[Transpose],(IdentityMatrix[3]\[Theta]new+(1-Cos[\[Theta]new])\[Omega]Skew+(\[Theta]new-Sin[\[Theta]new])\[Omega]Skew.\[Omega]Skew).vnorm,4]\[Transpose],{0,0,0,1},4];*)
			ExpV=Insert[Insert[IdentityMatrix[3],vnorm*\[Theta]new,4]\[Transpose],{0,0,0,1},4]; (*The previous commented equation can be simplified, given that \[Omega]={0,0,0}.*)
			Return[ExpV];
		];
	]

TransToExp[T_]:=
	Module[{R,p,\[Omega]norm,\[Theta]norm,\[Omega]skew,v,vnorm,S},
		R=T[[1;;3,1;;3]];
		p=T[[1;;3,4]];
		If[R!= IdentityMatrix[3],
			\[Omega]norm=RotToExp[R][[1]]; (*The indexing is because RotToExp (using Rodriguez Formula) returns a vector {\[Omega],\[Theta]}   ...*)
			(*with a normalized angular velocity \[Omega] (|\[Omega]|=1) and the corresponding angle or rotation \[Theta]*)
			\[Theta]norm=RotToExp[R][[2]]; (*Set a variable '\[Theta]norm' equal to \[Theta] from Rodriguez formula *)
			\[Omega]skew=VecToSkew[\[Omega]norm]; 
			v=(1/\[Theta]norm*IdentityMatrix[3]-1/2*\[Omega]skew+(1/\[Theta]norm-1/2*Cot[\[Theta]norm/2])*(\[Omega]skew.\[Omega]skew)).p; (*Equation for the matrix log of T from the lecture notes*)
			S=Join[\[Omega]norm,v];
			Return[{S,\[Theta]norm}];(*Return a normalized screww axis S and a corresponding travel time \[Theta]*)
		];

		If [R==IdentityMatrix[3], (*If R is equal to the Identity, which implies |\[Omega]|=0*)
			\[Omega]norm={0,0,0}; (*then \[Omega] is a zero vector, and so is the normalized \[Omega]*)
			\[Theta]norm=Norm[p]; (*Set a variable \[Theta]norm as the magnitude of the translation p*)
			vnorm=p/\[Theta]norm; (*Normalize the translation: vnorm=p/|p|*)
			S=Join[\[Omega]norm,vnorm];
			Return[{S,\[Theta]norm}]; (*Return a normalized screww axis S and a corresponding travel time \[Theta]*)
		];
	]

AdjTrans[T_]:=
	Module[{pSkew,R,AdT},
		pSkew=VecToSkew[T[[1;;3,4]]];
		R=T[[1;;3,1;;3]];
		AdT=Join[Join[R,pSkew.R]\[Transpose],Join[ConstantArray[0,{3,3}],R]\[Transpose]]\[Transpose];
		Return[AdT];
	]

VbToVa[Tab_,Vb_]:=
	Module[{Va},
		Va=AdjTrans[Tab].Vb;
		Return[Va];
	]

SiToTsb[M_,Si_,\[Theta]_]:=
	Module[{n,temp,Tsb,i},
		n=Length[Si]; (*Set a variable n equal to the number of input screw axes S, represented in the space frame*)
		Array[temp,n+1]; (*Create a 1-based array 'temp' with the length n+1. It looks like this: temp ={temp[1],temp[2],...,temp[n+1]} *)
		temp[n+1]=M; (*Store the input M (description in notes) into the array slot temp[n+1]*)
			For[i=n,i>=1,i--, (*Start a loop with a counter-variable 'i' with the initial value of i=n, decreasing by 1 after teach iteration until i=1*)
				temp[i]=ExpToTrans[Si[[i]],\[Theta][[i]]].temp[i+1]; (*At each iteration 'temp[i]' is set to be equal to exp([Si],\[Theta]i])*exp([Si+1],\[Theta]i+1)*...*exp([Sn,\[Theta]n])*M*)
			];
		Tsb=temp[1]; (*As a result of the above iterations: temp[1]= exp([S1],\[Theta]1])*exp([S2],\[Theta]2)*...*exp([Sn,\[Theta]n])*M which is equal to Tsb*)
		Return[Tsb]; (*Return Tsb, the transformation matrix from the body frame {b} to the space frame {s}*)
	];

BiToTsb[M_,Bi_,\[Theta]_]:= 
	Module[{n,temp,Tsb,i},
		n=Length[Bi];(*Set a variable n equal to the number of input screw axes B, represented in the body frame*)
		Array[temp,n+1,0]; (*Create a 0-based array 'temp' with the length n+1. It looks like this: temp ={temp[0],temp[1],...,temp[n]}*)
		temp[0]=M; (*Store the input M (description in notes) into the array slot temp[0]*)
			For [i=1,i<= n,i++, (*Start a loop with a counter-variable 'i' with the initial value of i=1, increasing by 1 after teach iteration until i=n*)
			temp[i]= temp[i-1].ExpToTrans[Bi[[i]],\[Theta][[i]]]; (*At each iteration 'temp[i]' is set to be equal to M*exp([B1],\[Theta]1)*...*exp([Bi-1],\[Theta]i-1)*exp([Bi],\[Theta]i)*)
			];
		Tsb=temp[n]; (*As a result of the above iterations: temp[n]= M*exp([B1],\[Theta]1)*exp([B2],\[Theta]2)*...*exp([Bn],\[Theta]n) which is equal to Tsb*)
		Return[Tsb]; (*Return Tsb, the transformation matrix from the body frame {b} to the space frame {s}*)
	];

SiToJs\[Theta][Si_,\[Theta]_]:=
	Module[{n,Js,Jsi,temp,i},
		n=Length[Si]; (*Set a variable n equal to the number of input screw axes Si, represented in the space frame*)
		Array[temp,n]; (*Create a 1-based array 'temp' with the length n. It looks like this: temp ={temp[1],temp[2],...,temp[n].*)
		Js=Array[Jsi,n]; (*Prepare an array, where the individual columns of the Jacobian will be stored*)
		temp[1]=IdentityMatrix[6];
		Jsi[1]=Si[[1]]; (*The first column of the space Jacobian is equal to the screw-axis S1 of the first joint, represented in the space-frame*)
		For[i=2,i<=n,i++, (*Loop*)
		temp[i]=temp[i-1].AdjTrans[ExpToTrans[Si[[i-1]],\[Theta][[i-1]]]];
		Jsi[i]=temp[i].Si[[i]]; (*Calculate columns of the space Jacobian individually*)
		];
		Return[Js\[Transpose]]; (*Return the array that contains all the columns of the space Jacobian*)
	];

BiToJb\[Theta][Bi_,\[Theta]_]:= 
	Module[{n,Jb,Jbi,temp,i},
		n=Length[Bi];(*Set a variable n equal to the number of input screw axes Bi, represented in the body frame*)
		Array[temp,n]; (*Create a 1-based array 'temp' with the length n. It looks like this: temp ={temp[1],temp[2],...,temp[n].*)
		Jb = Array [Jbi,n]; (*Prepare an array, where the individual columns of the Jacobian will be stored*)
		temp[n]=IdentityMatrix[6];
		Jbi[n]=Bi[[n]]; (*The last column of the body Jacobian is equal to the screw-axis Bn of the last joint, represented in the body-frame*)
		For[i=n-1,i>= 1,i--,(*Loop*)
		temp[i]=temp[i+1].AdjTrans[ExpToTrans[Bi[[i+1]],-\[Theta][[i+1]]]]; 
		Jbi[i]=temp[i].Bi[[i]]; (*Calculate columns of the body Jacobian individually*)
	];
Return[Jb\[Transpose]]; (*Return the array that contains all the columns of the body Jacobian*)
];

Jb\[Theta]ToJs\[Theta][M_,Si_,\[Theta]_,Jb\[Theta]_]:=
	Module[{Tsb,Js\[Theta]},
	Tsb = SiToTsb[M,Si,\[Theta]]; (*Find the transformation matrix from the space frame to the body frame*)
	Js\[Theta]=AdjTrans[Tsb].Jb\[Theta]; (*Transfom the body Jacobian to the space Jacobian*)
	Return[Js\[Theta]]; (*Return the space Jacobian*)
	];

InvKinemBody[M_,Bi_,Tsd_,\[Theta]guess_,\[Epsilon]_]:= 
	Module[{i,Tsb,TsbInv,Vb,Jb,\[Theta]},
		i=0; (*Initialize a counter variable i and set it to zero*)
		\[Theta][0]=\[Theta]guess; (*Read initial guess from the input variables of the function*)
		Tsb[0]=N[BiToTsb[M,Bi,\[Theta][0][[1]]]]; (*Numerically calculate the transformation matrix from the space-frame to the body-frame, that corresponds to the initial joint angle guess*)

		While[Abs[Norm[Tsd[[1;;2,4]]-Tsb[i][[1;;2,4]]]]>\[Epsilon], (*Run loop until error requirements are met*)
			Print["Iteration number ",i+1,":"];

			TsbInv[i]=TransfInv[Tsb[i]]; (*Find the inverse of the numerically calculated transformation that goes from the space-frame to the current body-frame; the current body 
			frame being the one that corresponds to the current joint-angle guess*)

			Vb[i] = TransToExp[TsbInv[i].Tsd][[1]]*TransToExp[TsbInv[i].Tsd][[2]]; (*TsbInv.Tsd is equal to Tbd; Tbd being the transformation that takes you from the current 
			 body frame to the goal configuration of the end-effector. Calculate Vb[i], which is the spacial velocity that results in Tbd, if followed for unit time*)

			Jb[i]=N[BiToJb\[Theta][Bi,\[Theta][i][[1]]]]; (*Numerically calculate the body Jacobian, that corresponds to the current joint-angle guess*)

			\[Theta][i+1] = {\[Theta][i][[1]]+PseudoInverse[Jb[i]].Vb[i]};  (*Update the joint-angle guess in preparation for the next iteration*)
			Print["The joint angle guess for iteration number ",i+1," is \[Theta]=",MatrixForm[\[Theta][i+1]]];

			Tsb[i+1]=N[BiToTsb[M,Bi,\[Theta][i+1][[1]]]]; (*Numerically calculate the transformation matrix, that goes from the space-frame to the updated body-frame; the updated body-frame 
			being the one that correspond to the updated joint-angle guess*)
			Print["The (x,y) position of the end-effector is: ",MatrixForm[Tsb[i+1][[1;;2,4]]],"\n"];
			i=i+1;
		];
	Return[Print[\[Theta][i]]];
	];


End[];

EndPackage[];



