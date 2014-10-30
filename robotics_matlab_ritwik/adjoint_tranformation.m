function A=adjoint_tranformation(T)
% Function to generate the adjoint map of the transformation
% A = adjoint_transformation(T)
% Input - T - 4x4 Transformation matrix
% Output - A - 6x6 Adjoint map matrix
R=[T(1,1) T(1,2) T(1,3);T(2,1) T(2,2) T(2,3);T(3,1) T(3,2) T(3,3)];
p=[T(1,4);T(2,4);T(3,4)];
A=[R zeros(3);vector2skew(p)*R R];
end