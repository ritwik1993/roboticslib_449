function Va=twist_rotation(Vb,T)
%Function to calculate the Twist vector in a different frame,
%   x = twist_rotation(T,vb)
%   Inputs:   Vb - (6x1) Twist vector
%             T - Transformation matrix
%   Output:   Va - Vb represented after Tranformation T
A=adjoint_tranformation(T);
Va=A*Vb;
end