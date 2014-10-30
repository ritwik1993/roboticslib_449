function [Tsb_theta_BodyFrame] = Calculate_Tsb_BodyFrame (Tsb_zero, B, theta)
% Problem 1-(xv).
% given Tsb[0], a list of screw axes B, the ith screw axis is called B(i);
% and a list of joint displacement theta, the ith theta is called theta(i). 
% B is a group of 6x1 column vectors.

n = length(theta);
% get how many joint coordinates.

i = 1;

Tsb_theta_BodyFrame = Tsb_zero;

while i <= n
    
    Bi = B(:,i);
    
    Vi = Bi * theta(i);
    
    fHandle = @Given_V_Exp_Coords_Find_T;
    [ T ] = fHandle(Vi);
    
    Tsb_theta_BodyFrame = Tsb_theta_BodyFrame * T;
    
    i = i + 1;
    
    disp ( 'Tsb(theta) =' )
    disp ( Tsb_theta_BodyFrame )
    
end
