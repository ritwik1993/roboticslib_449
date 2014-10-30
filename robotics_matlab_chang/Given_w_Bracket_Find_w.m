function [w] = Given_w_Bracket_Find_w (wss)
% Problem 1-(iii).
% given skew-symmetric wss , [w], find its corresponding 1x3 vector w.

w1 = wss(3,2);
w2 = wss(1,3);
w3 = wss(2,1);
% get the three numbers in the 1x3 vector w.

w = [w1, w2, w3];
% represent the 1x3 vector w.

disp ( ' w = ' )
disp ( w )