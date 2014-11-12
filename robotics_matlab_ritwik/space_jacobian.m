function J=space_jacobian(angles,varargin)
%Function to calculate the Space jacobian given a home configuration,
%angle displacements and the world fixed screw axes for each joint.
%   T = space_jacobian([t1,t2,....,tn],S1,S2,....,Sn)
%   Inputs:   tn - nth angular/translational displacement
%             Sn - (6X1) space axis vector of nth joint
%   Output:   (6xn) Space Jacobian 
n=(nargin)-1;
J=[varargin{1}];
T=eye(4);
for i = 2:1:n
    T=T*exp2transformation(varargin{i-1},angles(i-1));
    adj=adjoint_tranformation(T)*varargin{i};
    J=[J adj];
end
end
