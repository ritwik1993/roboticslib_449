function [V_Bracket] = Given_V_Spatial_Velocity_Find_V_Bracket (V_Spatial_Velocity)
% Problem 1-(viii).
% given V_Spatial_Velocity=transpose[w_transpose; v_transpose],
% V_Spatial_Velocity is a 6x1 column vector.
% find [V]=[[w],v_transpose;0,0,0,0].
% w_transpose is a 3x1 column vector,
% v_transpose is a 3x1 column vector,
% w is a 1x3 row vector.
% v is a 1x3 row vector.

w_transpose = V_Spatial_Velocity (1:3,1);
v_transpose = V_Spatial_Velocity (4:6,1);
w = transpose (w_transpose);
v = transpose (v_transpose);
% find the transpose vector of w & v.
% w_transpose is a 3x1 column vector,
% v_transpose is a 3x1 column vector,
% w is a 1x3 row vector.
% v is a 1x3 row vector.

fHandle = @Skew_Symmetric;
[wss] = fHandle (w);
% find the skew-symmetric matrix of w.
% wss = the skew-symmetric of row vector w.

V_Bracket = [wss, v_transpose; 0, 0, 0, 0];
% wss = [w] = 3x3 matrix.
% v_transpose = 3x1 column vector.
% get [V]=[[w],v;0,0,0,0].

disp ('[V] = ')
disp ( V_Bracket )