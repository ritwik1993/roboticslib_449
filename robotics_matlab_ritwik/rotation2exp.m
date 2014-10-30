function [w,theta]=rotation2exp(R)
%Function to generate the exponential coordinates given its rotation matrix,
%   [w,theta] = rotation2exp(R)
%   Inputs:   R - Rotation matrix 
%   Output:   w - (3x1) vector
%             theta - Angle in radians 
if R==eye(3)
    theta=0;
    w=zeros(3,1);
elseif trace(R)==-1
    theta=pi
    w=(1/sqrt(2*(1+R(3,3))))*[R(1,3);R(2,3);1+R(33)];
else
    theta=acos((trace(R)-1)/2);
    W=(R-transpose(R))/(2*sin(theta));
    w=skew2vector(W);
end
end
