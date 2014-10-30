function Tt=tranformation_inverse(T)
%   Function to calculate the inverse of a 4x4 SE(3) tranformation matrix
%   Tt = transformation_inverse(T)
%Inputs:  T - 4x4 Transformation matrix
%Outputs: Tt - 4x4 Inverse of T
R=[T(1,1) T(1,2) T(1,3);T(2,1) T(2,2) T(2,3);T(3,1) T(3,2) T(3,3)];
Rt=rotation_inverse(R);
Pt= -Rt*[T(1,4); T(2,4); T(3,4)];
Tt=[Rt(1,1) Rt(1,2) Rt(1,3) Pt(1,1);Rt(2,1) Rt(2,2) Rt(2,3) Pt(2,1);Rt(3,1) Rt(3,2) Rt(3,3) Pt(3,1);0 0 0 1];
end