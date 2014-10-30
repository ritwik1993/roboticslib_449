function T=fwd_body_poe(M,angles,varargin)
%Function to calculate the Forward Kinematics given a home configuration,
%angle displacements and the body fixed screw axes for each joint.
%   T = fwd_space_poe(M,[t1,t2,....,tn] ,B1,B2,....,Bn)
%   Inputs:   M - (4X4) Home Configuration 
%             tn - nth angular/translational displacement
%             Bn - (6X1) space axis vector of nth joint
%   Output:   Forward Kinematics (Configuration of the end effector)
n=(nargin)-2;
T=M;
for i = 1:n
    T=T*exp2transformation(varargin{i},angles(i));
end
end