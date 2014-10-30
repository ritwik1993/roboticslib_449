function [InvT] = Transformation_Inverse (T)
% Problem 1-(vii).
% find the inverse of a Transformation Matrix.

n = 4;
% the size of the rotation matrix, as stated in the problem statement.

R = T(1:3,1:3);
p = T(1:3,4);

InvT11 = transpose(R);
InvT12 = (-1)*transpose(R)*p;
InvT21 = [0,0,0];
InvT22 = [1];

InvT=[InvT11,InvT12;InvT21,InvT22];

disp ( ' Inverse of T =' )
disp ( InvT )

