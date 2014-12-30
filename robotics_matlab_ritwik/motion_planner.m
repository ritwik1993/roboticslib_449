function motion_planner(r,obs1,N,start,stop)
% Function to plan motion from start node to stop node
close all;
obs=obs1;
obs(:,3)=obs1(:,3)+r;   % Inflate obstacle by robot's radius
E=visibility_generator(N,obs)
path=astar_planner(N,E,start,stop);
%---------------------------------------------------------------------------------------
% Visualization
hold on;
scatter(N(:,1),N(:,2),45,'o','filled');
scatter(N(start,1),N(start,2),60,'d','filled','MarkerFaceColor',[0 1 0]);
scatter(N(stop,1),N(stop,2),60,'d','filled','MarkerFaceColor',[1 0 0]);
legend('Nodes','Start Node','Goal Node');
axis([0 100 0 100]);
axis equal;
ax=gca;
ax.XTick=[0:10:100];
ax.YTick=[0:10:100];
xlim([0,100]);
ylim([0,100]);
for i=1:size(obs(:,1))  %Plot obstacles
    rectangle('Position',[obs1(i,1)-obs1(i,3),obs1(i,2)-obs1(i,3),2*obs1(i,3),2*obs1(i,3)],...
  'Curvature',[1,1], 'FaceColor','m');
end
for i=2:length(path)
    plot([N(path(i-1),1) N(path(i),1)],[N(path(i-1),2) N(path(i),2)],'k','LineWidth',2);
end
end