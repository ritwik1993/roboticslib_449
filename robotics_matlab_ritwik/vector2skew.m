function X=vector2skew(x) 
%Function to generate the Skew symmetric matrix given a 3-vector q ,
%   X = vector2skew(x1)
%   Inputs:   x1 - (3x1) vector q
%   Output:   X  - Skew symmetric matrix of q
X=[0 -x(3) x(2);x(3) 0 -x(1);-x(2) x(1) 0];
end