function path=astar_planner(N,E,start,stop)
%Function implementation of the Astar search algorithm
%   astar_planner(N,E,start,stop)
% Inputs ->   N - nX2 matrix with n nodes, each row describing (x,y)
%                 position of the node
%             E - nXn symmetric matrix which describes edge connectivity
%             start - the start node
%             stop  - the stop node
n=size(E,1);
open=[start 0];
closed=[];
for i=1:n
    past_cost(i)=10000;
end
past_cost(start)=0;
while size(open,1)~=0
    current=open(1,1);
    open(1,:)=[];
    closed=[closed current];
    if current==stop
        disp('Shortest path successfully computed');
        i=1;
        path(i)=stop;
        i=2;
        path(i)=parent(stop);
        while path(i)~=start
            i=i+1;
            path(i)=parent(path(i-1));
        end
        path=flip(path);
        disp(['The shortest path is through nodes ',num2str(path)]);
        return 
    end
    for i=1:n            
        if E(current,i)~=0 & isempty(find(closed==i)) 
            tentative_past_cost=past_cost(current)+E(current,i);
            if tentative_past_cost<past_cost(i)
                past_cost(i)=tentative_past_cost;
                parent(i)=current;
                est_total_cost(i)=past_cost(i)+sqrt((N(i,1)-N(stop,1))^2+(N(i,2)-N(stop,2))^2);
                if isempty(find(open(:,1)==i))
                    open=[open;i est_total_cost(i)];
                    open=sortrows(open,2);
                else
                    open=sortrows(open,2);
                end
            end
        end
    end
end
disp('Failed to find a path between Start and Goal nodes');
path=0;
return
end
            
        
    
