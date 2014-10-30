function [T] = Given_V_Exp_Coords_Find_T (V_Exp_Coords)
% Problem 1-(x).
% given a 6x1 exponential coordinate V, find transformation T=[R,p;0,1].
% V_Exp_Coords = 6x1 column vector = exponential coordinates.

% first we need to find theta using the given exponential coordinates V.
% w_ExpCoords = w * theta.
% w_ExpCoords is the first three numbers in the exponential coordinates V.
% w_ExpCoords = V_Exp_Coords(1:3,1).
% w is the first three numbers in the twist S.
% w = S(1:3,1).
% V_Exp_Coords = S * theta.
% w_ExpCoords = w * theta.
% [w_ExpCoords] = [w] * theta.
% ||w_ExpCoords|| = ||w|| * theta   &  ||w|| = 1 since w is a unit vector.
% this means: theta = ||w_ExpCoords||.
% which means: theta = ||V_Exp_Coords(1:3,1)||
w_EC1 = V_Exp_Coords(1);
w_EC2 = V_Exp_Coords(2);
w_EC3 = V_Exp_Coords(3);
v_EC1 = V_Exp_Coords(4);
v_EC2 = V_Exp_Coords(5);
v_EC3 = V_Exp_Coords(6);

if w_EC1 ~= 0 || w_EC2 ~= 0 || w_EC3 ~= 0 
    
    theta = sqrt(w_EC1^2 + w_EC2^2 + w_EC3^2);
    
    S = V_Exp_Coords / theta;
    % V_Exp_Coords = S * theta,
    
else if v_EC1 ~= 0 || v_EC2 ~= 0 || v_EC3 ~= 0
        
        theta = sqrt(v_EC1^2 + v_EC2^2 + v_EC3^2);
        
        S = V_Exp_Coords / theta;
        % V_Exp_Coords = S * theta,
        
    else
        
        disp ( ' please provide a correct V_Exp_Coords' )
        
    end
end

w_3x1 = S (1:3);
% w_3x1 = angular velocity in Twist S.
% w_3x1 = 3x1 column vector.
v_3x1 = S (4:6);
% v_3x1 = linear velocity in Twist S.
% v_3x1 = 3x1 column vector.
    
w_Exp_Coords = transpose (V_Exp_Coords(1:3,1));
fHandle = @Given_w_Exp_Coords_Find_R;
[R] = fHandle ( w_Exp_Coords );
T11 = R;
% get T(1,1), which is R, AKA expm(w_Exp_Coords)
% this is written in another function named 'Given_w_Exp_Coords_Find_R'.
% now, w_Exp_Coords is a 1x3 row vector.

n = length (w_Exp_Coords);
I = eye (n);

w_1x3 = transpose(w_3x1);
% in [w], we use angular velocity w.
% w is NOT exponential coordinates w_Exp_Coords.
fHandle = @Skew_Symmetric; 
% function 'Skew_Symmetric' use w which is Angular Velocity.
[wss] = fHandle (w_1x3);
% w_1x3 = 1x3 row vector = angular velocity, in twist S.

G_theta = I*theta + (1-cos(theta)) * wss + (theta-sin(theta))*wss*wss;
% G(theta)=I*theta+(1-cos(theta))*[w]+(theta-cos(theta))*[w]*[w].

T12 = G_theta * v_3x1;
% get T(1,2)=G(theta)*v.
% v_3x1 = linear velocity in Twist S.


T21 = [0,0,0];
T22 = [1];

T = [T11, T12; T21, T22];

disp ( 'Transportation T = ' )
disp ( T )