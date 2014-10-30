function [w_Exp_Coords] = Given_R_Find_w_Exp_Coords (R)
% Problem 1-(v).
% given a rotation matrix R, SO(3), 3x3 matrix.
% find the Exponential Coordinates 1x3 row vector w.

n = length (R);
I = eye (n);

trR = R(1,1) + R(2,2) + R(3,3);

if R == I
    
    theta = 0;
    w_Exp_Coords = 0;
     
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
        
    else
        
        theta = acos((trR-1)/2);
        % find theta.
        wss = (R-transpose(R))./2./sin(theta);
        % find skew-symmetrics [w].
        fHandle = @Given_w_Bracket_Find_w;
        [w] = fHandle(wss);
        % find w.
        w_Exp_Coords = w * theta;
        
    end
end

disp ( ' w_Exp_Coords = ' )
disp ( w_Exp_Coords )