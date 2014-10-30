function [Tsb_theta_SpaceFrame] = Calculate_Tsb_SpaceFrame (Tsb_zero, S, theta)
% Problem 1-(xiv).
% given Tsb[0], a list of screw axes S, the ith screw axis is called S(i);
% and a list of joint displacement theta, the ith theta is called theta(i). 
% S is a group of 6x1 column vectors.

n = length(theta);
% get how many joint coordinates.

i = n;

Tsb_theta_SpaceFrame = Tsb_zero;

while i >= 1
    
    Si = S(:,i)
    
    Vi = Si * theta(i)
    
    fHandle = @Given_V_Exp_Coords_Find_T;
    [ T ] = fHandle(Vi);
    
    Tsb_theta_SpaceFrame = T * Tsb_theta_SpaceFrame;
    
    i = i - 1;
    
    disp ( 'Tsb(theta) =' )
    disp ( Tsb_theta_SpaceFrame )

end

