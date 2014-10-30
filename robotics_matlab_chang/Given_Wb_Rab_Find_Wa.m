function [wa] = Given_Wb_Rab_Find_Wa (wb,Rab)
% Problem 1-(vi).
% given wb in frame {b} and Rab, find wa in frame{a}.
% wb is a 1x3 row vector.
% input wb is a row vector and Rab is a 3x3 matrix.

wa_transpose = Rab * transpose(wb);
% using Rab*wb=wa, wa is a column vector.
% so we need to transpose wa so as to get a row vector.

wa = transpose(wa_transpose);

disp ( ' wa = ' )
disp ( wa )