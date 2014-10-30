function x=angvel_rotation(w,R)  
%Function to calculate the angular velocity vector in a different frame,
%   x = angvel_rotation(w,R)
%   Inputs:   w - (3x1) angular velocity vector
%             R - Rotation matrix
%   Output:   x - w after undergoing rotation described by R
x=R*w;
end
