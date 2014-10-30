function [s,theta]=tranformation2exp(T)
%Function to generate the spatial velocity coordinates given its transformation,
%   [s,theta]=transformation2exp(T)
%   Input:   T - 4x4 Transformation matrix   
%   Outputs:   s- (6x1)spatial velocity coordinates 
%             theta - angle in radians

R=[T(1,1) T(1,2) T(1,3);T(2,1) T(2,2) T(2,3);T(3,1) T(3,2) T(3,3)];
p=[T(1,4);T(2,4);T(3,4)];
magp=norm(p);
[w,theta]=rotation2exp(R);
if R==eye(3)
    theta=magp;
    v=p/magp;
else 
    W=vector2skew(w);
    v=(eye(3)/theta-W/2+(1/theta-cot(theta/2)/2)*W*W)*p;
end
s=[w;v];
end
