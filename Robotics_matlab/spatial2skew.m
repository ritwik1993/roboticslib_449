function V=spatial2skew(v)
%Function to generate the skew symmetric representation of a Twist vector,
%   V = spatial2skew(v)
%   Inputs:   v - a 6x1 twist vector 
%   Output:   V - 4x4 skew symmetric matrix
W=vector2skew([v(1,1);v(2,1);v(3,1)]);
V=[W [v(4,1);v(5,1);v(6,1)];0 0 0 0];
end