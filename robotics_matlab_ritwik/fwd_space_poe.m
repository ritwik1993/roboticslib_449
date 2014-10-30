function T=fwd_space_poe(M,angles,varargin)
%Function to calculate the Forward Kinematics given a home configuration,
%angle displacements and the world fixed screw axes for each joint.
%   T = fwd_space_poe(M,[t1,t2,....,tn] ,S1,S2,....,Sn)
%   Inputs:   M - (4X4) Home Configuration 
%             tn - nth angular/translational displacement
%             Sn - (6X1) space axis vector of nth joint
%   Output:   Forward Kinematics (Configuration of the end effector)
n=(nargin)-2;
T=eye(4);
for i = 1:1:n
    T=T*exp2transformation(varargin{i},angles(i));
end
T=T*M;
end
