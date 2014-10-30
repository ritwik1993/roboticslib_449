function [R] = Given_w_Exp_Coords_Find_R (w_Exp_Coords)
% Problem 1-(iv).
% w_Exp_Coords is a 1x3 row vector.
% w_Exp_Coords is not angular velocity !!!.
% w_Exp_Coords is exponential coordinates !!!.
% w_Exp_Coords do not need to be a unit vector.
% w_Exp_Coords is a constant theta times angular velocity unit vector.
% get rotation matrix using R=I+sin(theta)*[w]+(1-cos(theta))*[w]*[w].

% first we need to find angular velocity w from w_ExpCoords.
% w_ExpCoords = w * theta.
% [w_ExpCoords] = [w] * theta.
% ||w_ExpCoords|| = ||w|| * theta   &  ||w|| = 1 since w is a unit vector.
% this means: theta = ||w_ExpCoords||.
w_EC1 = w_Exp_Coords(1);
w_EC2 = w_Exp_Coords(2);
w_EC3 = w_Exp_Coords(3);

if w_EC1 ~= 0 || w_EC2 ~= 0 || w_EC3 ~= 0 
    
    theta = sqrt(w_EC1^2 + w_EC2^2 + w_EC3^2);
   
    w = w_Exp_Coords / theta;
    % find the angular velocity w, which is a unit vector.
    % w = w_Exp_Coords / theta.
    % w is also a 1x3 row vector.
    
    n = length(w_Exp_Coords);
    % find the size of vector w_ExpCoords.
    
    I = eye(n);
    % define a nxn identity matrix.
    
    fHandle = @Skew_Symmetric;
    wss = fHandle(w);
    % call the 'Skew_Symmetric' function that is written before.
    % w is a 1x3 row vector.
    % get the [w] of the angular velocity w (unit vector).
    R = I + sin(theta) * wss + (1-cos(theta))* wss * wss;
    % get rotation matrix using R=I+sin(theta)*[w]+(1-cos(theta))*[w]*[w].

else 
    
    n = length(w_Exp_Coords);
    % find the size of vector w_ExpCoords.
    w = [0, 0, 0];
    R = eye(n);
    
end


disp ( ' R = ')
disp ( R )
