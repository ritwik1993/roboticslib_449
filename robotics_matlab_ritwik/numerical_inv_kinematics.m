function thetaf=numerical_inv_kinematics(T,M,in_angles,varargin)
% Function to calculate the numerical inverse kinematics given an initial
% guess and the goal configuration
%        thetaf=numerical_inv_kinematics(T,M,[theta1;theta2;....thetan],B1,B2,...Bn);
%        Output -> the inverse kinematics values (angles in radians for revolute! and meters for prismatic)
%        Inputs -> theta1,theta2 etc are the initial guesses in degrees!
%                  M is the home configuration of the robot
%                  T is the 4X4 matrix describing the end configuration
%                  Bn (6X1) are the body screw axes for the nth joint
i=1;
thetaf=in_angles;
n=nargin-3;
while norm(T-fwd_body_poe(M,thetaf,varargin{1:n}))>0.01
    [V,y]=tranformation2exp(tranformation_inverse(fwd_body_poe(M,thetaf,varargin{1:n}))*T);
    thetaf=thetaf+(pinv(body_jacobian(thetaf,varargin{1:n}))*(V*y));
    disp(['At iteration # ',num2str(i),' the joint angle estimates are (in radians)'])
    disp(thetaf)
    disp('and the x,y,z position of the end effector is')
    TSB=fwd_body_poe(M,thetaf,varargin{1:n});
    disp([TSB(1,4),TSB(2,4),TSB(3,4)])
    i=i+1;
    end
thetaf=thetaf;
end
