function v=skew2spatial(V)
%Function to generate a 6x1 twist vector given its skew symmetric representation,
%   v = skew2spatial(V)
%   Inputs:   V - Skew symmetric matrix 
%   Output:   v - (3x1) vector
W=[V(1,1) V(1,2) V(1,3);V(2,1) V(2,2) V(2,3);V(3,1) V(3,2) V(3,3)];
w=skew2vector(W);
v=[w;V(1,4);V(2,4);V(3,4)];
end