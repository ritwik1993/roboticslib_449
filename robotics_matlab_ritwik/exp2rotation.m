function R=exp2rotation(w,theta)
%Function to generate a rotation matrix given its exponential coordinates,
%   R=exp2rotation(w,theta)
%   Inputs:   w- (3x1)exponential coordinates 
%             theta - angle in radians
%   Output:   R - Rotation matrix
if norm(w)==0
    R=eye(3);
else
    W=vector2skew(w/norm(w));
    R=eye(3)+((sin(theta))*W)+((1-cos(theta))*W^2);
end
end