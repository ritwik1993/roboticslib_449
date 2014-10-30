function [V_Spatial_Velocity] = Given_V_Bracket_Find_V_Spatial_Velocity (V_Bracket)
% Problem 1-(ix).
% given a [V], find what is V_Spatial_Velocity.
% V_Spatial_Velocity is a 6x1 column vector.
% V_Bracket is a 4x4 matrix.

wss = V_Bracket(1:3,1:3);
% find the skew-symmetric of w.
fHandle = @Given_w_Bracket_Find_w;
[w] = fHandle (wss);
% find angular velocity w, 1x3 row vector.
w_transpose = transpose(w);
% w_transpose is a 3x1 column vector.

v_transpose = V_Bracket(1:3,4);
% find the transpose of v, which v is assumed to be a 1x3 row vector.
% which is the first three numbers of the last column.

V_Spatial_Velocity = [w_transpose; v_transpose];
% get a 6x1 column vector.

disp ( ' Spatial Velocity V =' )
disp ( V_Spatial_Velocity )

