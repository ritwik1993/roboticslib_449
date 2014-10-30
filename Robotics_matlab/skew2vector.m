function x=skew2vector(X)
%Function to generate a 3-vector q given its skew symmetric representation,
%   x = skew2vector(X)
%   Inputs:   X - Skew symmetric matrix 
%   Output:   x - (3x1) vector
x=[X(3,2);X(1,3);X(2,1)];  % returns a 1 X 3 vector
end