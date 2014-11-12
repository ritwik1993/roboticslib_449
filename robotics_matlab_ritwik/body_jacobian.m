function J=body_jacobian(angles,varargin)
%Function to calculate the body frame jacobian given a home configuration,
%angle displacements and the body fixed screw axes for each joint.
%   T = body_jacobian([t1,t2,....,tn],B1,B2,....,Bn)
%   Inputs:   tn - nth angular/translational displacement
%             Sn - (6X1) space axis vector of nth joint
%   Output:   (6xn) Space Jacobian 
n=(nargin)-1;
J=[varargin{n}];
T=eye(4);
for i = n:-1:2
    T=exp2transformation(varargin{i},-angles(i))*T;
    adj=adjoint_tranformation(T)*varargin{i-1};
    J=[adj J];
end
end
