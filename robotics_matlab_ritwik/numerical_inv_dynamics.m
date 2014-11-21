% Code to calculate the numerical inverse dynamics
%Please modify line 68 to specify the configuration of end-effector in the last frame
%--------------------------------------------------------------------------------------------------
%Preliminaries
n=input('Please enter number of links in the robot: ');
for i=1:n
    %
    disp(['Please enter M (4x4) of frame ',num2str(i),...
        ' in terms of frame: ',num2str(i-1)]);
    Mlink(:,:,i)=input('');     %Input M (4X4) in frame (i) wrt frame (i-1)
end
M(:,:,1)=Mlink(:,:,1);
for i=2:n    %Calculate configuration of each frame in frame 0  
    M(:,:,i)=M(:,:,i-1)*Mlink(:,:,i);
end
for i=1:n
    disp(['Please enter S (6X1) of frame: ',num2str(i)]);
    S(:,:,i)=input('');     %Input S (4X4) of frame (i) 
end
for i=1:n
    A(:,:,i)=adjoint_tranformation...
        (tranformation_inverse(M(:,:,i)))*S(:,:,i);
end
for i=1:n
    disp(['Please enter G (6X6 inertia tensor) of frame: ',num2str(i)]);
    G(:,:,i)=input('');     %Input G (6X6) of frame (i) 
end
g=input('Enter the 3X1 gravity vector described in base frame: ');
Ftip=input...
    ('Enter the 6X1 spatial force vector exerted by the tip of the robot: ..');
theta=input...
    ('Please enter the nX1 displacement vector :');    
thetad=input...
    ('Please enter the nX1 displacement derivative vector :');   
thetadd=input...
    ('Please enter the nX1 displacement double derivative vector :');   
%---------------------------------------------------------------------------------------------------


%Initialization
V0=[0;0;0;0;0;0];
V0dot=[0;0;0;-g];
F(:,:,n+1)=Ftip;


%---------------------------------------------------------------------------------------------------


%Forward Iteration
for i=1:n
    if i==1
        Tlink(:,:,i)=Mlink(:,:,i)*exp2transformation(A(:,:,i),theta(i));
        V(:,:,i)=adjoint_tranformation...
            (tranformation_inverse(Tlink(:,:,i)))*V0+A(:,:,i)*thetad(i);
        Vdot(:,:,i)=adjoint_tranformation...
            (tranformation_inverse(Tlink(:,:,i)))*V0dot+lie_bracket...
            (V(:,:,i),A(:,:,i))*thetad(i)+A(:,:,i)*thetadd(i);
    else 
         Tlink(:,:,i)=Mlink(:,:,i)*exp2transformation(A(:,:,i),theta(i));
        V(:,:,i)=adjoint_tranformation...
            (tranformation_inverse(Tlink(:,:,i)))*...
            V(:,:,i-1)+A(:,:,i)*thetad(i);
        Vdot(:,:,i)=adjoint_tranformation...
            (tranformation_inverse(Tlink(:,:,i)))*Vdot(:,:,i-1)+...
            lie_bracket(V(:,:,i),A(:,:,i))*thetad(i)+A(:,:,i)*thetadd(i);
    end
end
Tlink(:,:,n+1)=[1 0 0 0;0 1 0 0;0 0 1 0;0 0 0 1];

%---------------------------------------------------------------------------------------------------


%Backward Iteration

for i=n:-1:1
    F(:,:,i)=transpose(adjoint_tranformation(tranformation_inverse...
        (Tlink(:,:,i+1))))*F(:,:,i+1)+G(:,:,i)*Vdot(:,:,i)-transpose...
        (ad_prelie(V(:,:,i)))*(G(:,:,i)*V(:,:,i));
    torque(i)=transpose(F(:,:,i))*A(:,:,i);
end
disp('The joint torques are: ');
disp(torque);
