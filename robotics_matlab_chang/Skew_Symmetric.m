function [wss] = Skew_Symmetric (w)
% Problem 1-(ii).
% define wss as the skew-symmetric of 3-vector w.
% w is a 1x3 row vector.
% wss = the skew-symmetric of row vector w.

[rows, columns] = size(w);

% assuming the ideal w is a 1x3 vector, one rows and three columns. 

if rows ~= 1
    disp ('wrong w, need a 1x3 vector')
end

if columns ~= 3
    disp ('wrong w, need a 1x3 vector')
end

w1 = w(1,1);
w2 = w(1,2);
w3 = w(1,3);

wss = zeros(3);
% let skew-symmetric [w] be a 3x3 zero matrix first, 
% then we can change the number for specific positions.

wss(1,2) = (-1) * w3;
wss(1,3) = w2;
wss(2,1) = w3;
wss(2,3) = (-1) * w1;
wss(3,1) = (-1) * w2;
wss(3,2) = w1;
    
disp ( ' [w] =' )
disp ( wss )
% output the skew-symmetric of 3-vector w.