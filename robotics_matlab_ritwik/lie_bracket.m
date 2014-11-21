function out=lie_bracket(V1,V2)
%Function to calculate the lie bracket of 2 6X1 vectors
out=[vector2skew([V1(1,1);V1(2,1);V1(3,1)]) zeros(3,3);vector2skew([V1(4,1);V1(5,1);V1(6,1)]) vector2skew([V1(1,1);V1(2,1);V1(3,1)])]*V2;
end