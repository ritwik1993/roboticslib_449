function [N,obs]=random_world(n,o)
%Function to create n random nodes and o random obstacles
close all;
N = round(100*rand(n,2));
obs=round(100*rand(o,2));
obs(:,3)= round(4+(6)*rand(o,1));
hold on;
scatter(N(:,1),N(:,2),45,'o','filled');
axis([0 100 0 100]);
axis equal;
ax=gca;
ax.XTick=[0:10:100];
ax.YTick=[0:10:100];
xlim([0,100]);
ylim([0,100]);
for i=1:size(obs(:,1))  %Plot obstacles
    rectangle('Position',[obs(i,1)-obs(i,3),obs(i,2)-obs(i,3),2*obs(i,3),2*obs(i,3)],...
  'Curvature',[1,1], 'FaceColor','m');
end
end
