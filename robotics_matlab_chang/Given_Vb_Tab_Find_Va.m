function [Va] = Given_Vb_Tab_Find_Va (Vb,Tab)
% Problem 1-(xiii).
% given TWIST Vb and transformation Tab, find TWIST Va.
% Va and Vb are twists.

fHandle = @Adjoint_Map;
[AdTab_Bracket] = fHandle(Tab);
% find adjoint map of transformation Tab.

Va = AdTab_Bracket * Vb;
% twist Va = [AdTab] * (twist Vb).

disp ( ' twist Va ')
disp ( Va )