function check=obtuse_check(A,B,C)
%Function to calculate if any one angle at [x,y] and [x1,y1] is obtuse given three points
d1=norm(A-B);
d2=norm(B-C);
d3=norm(C-A);
anglea=acos((d1^2+d3^2-d2^2)/(2*d1*d3));
angleb=acos((d1^2+d2^2-d3^2)/(2*d1*d2));
if anglea>pi/2 || angleb>pi/2
    check=0;
    return
else
    check=1;
    return
end