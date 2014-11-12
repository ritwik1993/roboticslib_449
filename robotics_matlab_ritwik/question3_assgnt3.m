function question3_assgnt3(theta1,theta2)
%Function to plot the polygon of feasible velocities for a 2R arm
% question3_assgnt3[theta1,theta2];
theta1dot=[-1,1];
theta2dot=[-1,1];
J=[(-2*sin(theta1))-sin(theta1+theta2) -sin(theta1+theta2);...
        (2*cos(theta1))+cos(theta1+theta2) cos(theta1+theta2)];
if rank(J)>1
    xtip=J*[theta1dot;theta2dot];
    xtip
    [maxx,imaxx]=max(xtip(1,:));
    if xtip(1,1)==xtip(1,2)
        [minx,iminx]=min(xtip(1,:));
        iminx=iminx+1;
    else
        [minx,iminx]=min(xtip(1,:));
    end
    [maxy,imaxy]=max(xtip(2,:));
    if xtip(2,1)==xtip(2,2)
        [miny,iminy]=min(xtip(2,:));
        iminy=iminy+1;
    else
        [miny,iminy]=min(xtip(2,:));
    end
    plot([-1,1],xtip(1,:),'b');
    hold on;
    plot([-1,1],xtip(2,:),'b');
    %fill([[-1,1]xtip(2,:)],'r')
    plot([iminx-1 iminy-2],[minx miny],'g');
    plot([iminx-1 imaxy-1],[minx maxy],'g');
    plot([imaxx-2 iminy-2],[maxx miny],'g');
    plot([imaxx-2 imaxy-1],[maxx maxy],'g');
    axis equal;
else
    disp('This configuration leads to a singularity!!')
end
end
