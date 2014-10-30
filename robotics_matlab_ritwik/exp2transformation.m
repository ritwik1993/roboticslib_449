function S=exp2transformation(s,theta)
%Function to generate a rotation matrix given its exponential coordinates,
%   S=exp2transformation(s,theta)
%   Inputs:   s- (6x1)exponential coordinates 
%             theta - angle in radians
%   Output:   S - transformation matrix matrix
w=[s(1,1);s(2,1);s(3,1)];
v=[s(4,1);s(5,1);s(6,1)];
if norm(w)==0
    v=v/norm(v);
    S=[eye(3) v*theta;0 0 0 1];
else
    v=v/norm(w);
    w=w/norm(w);
    W=vector2skew(w);
    W1=exp2rotation(w,theta);
    T=((eye(3)*theta)+((1-cos(theta))*W)+((theta-sin(theta))*W^2))*v;
    S=[W1,T;0 0 0 1];
end

end