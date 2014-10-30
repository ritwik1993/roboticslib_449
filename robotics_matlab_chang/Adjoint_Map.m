function [AdT_Bracket] = Adjoint_Map (T)
% Problem 1-(xii).
% given a transformation T, find the adjoint map AdT.
% [AdT]=[R,0;[p]R,R], which is a 6x6 matrix.

R = T(1:3,1:3);
p = T(1:3,4);
% p = 3x1 column vector. 
p_transpose = transpose(p);
% since I need to use 'Skew-Symmetric' function
% I need a 1x3 row matrix, so take transpose of p

AdT_Bracket11 = R;
% get [AdT](1,1).

AdT_Bracket12 = zeros(3);
% get [AdT](1,2), should be a 3x3 zero matrix.

fHandle = @Skew_Symmetric;
[p_Bracket] = fHandle(p_transpose);
% get [p], the skew-symmetric of p. 
% should use p_transpose instead of p,
% since p here is a 3x1 column vector,
% and in function Skew_Symmetric, I use a 1x3 row vector to represent p.
AdT_Bracket21 = p_Bracket * R; 
% get [AdT](2,1).

AdT_Bracket22 = R;
% get [AdT](2,2).

AdT_Bracket = [AdT_Bracket11, AdT_Bracket12; AdT_Bracket21, AdT_Bracket22];
% get the [AdT]=[R,0;[p]R,R], which is a 6x6 matrix.

disp (' Adjoint Map [AdT] =')
disp ( AdT_Bracket )