function E=visibility_generator(N,obs)
%Generate the visibility graph given the set of Node vertices and
%obstacle...coordinates along with the radius.
e=flip(combnk(1:length(N),2));
d=ones(length(N));


for i=1:length(e(:,1))
    v1=N(e(i,1),:);
    v2=N(e(i,2),:);
    d(e(i,1),e(i,2))=norm(v1-v2);
    for j=1:length(obs(:,1))
        oc=[obs(j,1),obs(j,2)]; % Obstacle center
        ro=obs(j,3); % Obstacle radius(inflated)
        od=abs((v1-oc)*[0 1;-1 0]*(v2-oc)'/norm(v1-v2));  %orthogonal distance from obstacle center to line edge
        %check if obstacle is between nodes in any one axis
        check = obtuse_check(v1,v2,oc); 
        if od<ro & check==1
             d(e(i,1),e(i,2))=0;
        end
    end   
end
for i=1:length(N)
    for j=1:length(obs(:,1))
        v1=N(i,:);
        oc=[obs(j,1),obs(j,2)]; % Obstacle center
        ro=obs(j,3); % Obstacle radius(inflated)
        d1=norm(oc-v1);
        if d1<ro
            d(:,i)=0;
        end
    end
end 

       
E=triu(d)+transpose(triu(d));
E(logical(eye(size(E)))) = 0; %Make diagonals as 0
end