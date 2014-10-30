function [V_Exp_Coords] = Given_T_Find_V_Exp_Coords (T)
% Problem 1-(xi).
% given transformation T, find exponential coordinates V.
% V_Exp_Coords=6x1 column matrix = [w_Exp_Coords; v_Exp_Coords],
% w_Exp_Coords & v_Exp_Coords are 3x1 column vectors.
% w_Exp_Coords & v_Exp_Coords DO NOT have to be unit vectors
% w_Exp_Coords=(angular velocity w in twist S)*theta.
% v_Exp_Coords=(linear velocity v in twist S)*theta.

% T=[R,p;0,1].
R = T(1:3, 1:3);
% find rotation matrix R in transportation T=[R,p;0,1].
p = T(1:3, 4);
% p = 3x1 column vector.
p1 = T(1,1);
p2 = T(2,1);
p3 = T(3,1);

trR = R(1,1) + R(2,2) + R(3,3);
% find theta.

n = length (R);
I = eye(n);
% define a 3x3 identity matrix I.

if R == I
    
    w = [0; 0; 0];
    % w = angular velocity in twist S.
    % w = 3x1 zero column vector. 
    v = p / sqrt( p1^2 + p2^2 + p3^2);
    % v = linear velocity in twist S = 3x1 column vector.
    theta = sqrt( p1^2 + p2^2 + p3^2);
    % theta = ||p||.
    
    V_Exp_Coords = [w; v] * theta;
    % get V = 6x1 column vector.
    
else if trR == -1
        
       theta = pi;
        
        w_1 = 1/sqrt(2*(1+R(3,3)))*transpose([R(1,3),R(2,3),1+R(3,3)]);
        w_2 = 1/sqrt(2*(1+R(2,2)))*transpose([R(1,2),1+R(2,2),R(3,2)]);
        w_3 = 1/sqrt(2*(1+R(1,1)))*transpose([1+R(1,1),R(2,1),R(3,1)]);
        
        if w_1 ~= Inf
            
            w = w_1;
            w_Exp_Coords = w_1 * theta;
            
        else if w_2 ~= Inf
                
                w = w_2;
                w_Exp_Coords = w_2 * theta;
                
            else 
                
                w = w_3;
                w_Exp_Coords = w_3 * theta;
                
            end
        end
        
        v = p / sqrt( p1^2 + p2^2 + p3^2);
        % v = linear velocity in twist S = 3x1 column vector.
        % NOT SURE, never mentioned in the book!!!
        
        V_Exp_Coords = [w; v] * theta;
        % get V = 6x1 column vector.
        
    else 
        
        theta = acos((trR-1)/2);
        % find theta.
        wss = (R-transpose(R))./2./sin(theta);
        % find skew-symmetrics [w].
        fHandle = @Given_w_Bracket_Find_w;
        [w_1x3] = fHandle(wss);
        w = transpose(w_1x3);
        % find w.
        % w = angular velocity in twist S.
        % w = 3x1 column vector.
        
        n = length(p);
        I = eye(n);
        Inv_G_theta=1/theta*I-1/2*wss+(1/theta-1/2*cot(theta/2))*wss*wss;
        % eq (3.116) in book P72.
        v = Inv_G_theta * p;
        % find v, should be a 3x1 column vector.
        
        V_Exp_Coords = [w; v] * theta;
        % get V = 6x1 column vector.
    end
end
        
disp ( ' Exponential Coordinates V = ' )
disp ( V_Exp_Coords )