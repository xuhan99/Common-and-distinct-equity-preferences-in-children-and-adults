
%% 1. load the data
[design,~,~] = xlsread('data_adult.xlsx');
subs         = unique(design(:,1),'stable'); %%%VERY IMPORTANT,not to change the original orders. Need to check manually!!!!!!!!!!!!!
nsub         = length(subs); % number of subjects.

%% 2. load the parameters
[par,~,~]=xlsread('IndPars_fitm4a_adult.xlsx');
par = par(:,2:end); %excluding the subid column
paras = struct('alpha11',par(:,1)','alpha12',par(:,2)','alpha21',par(:,3)','alpha22',par(:,4)','beta11',par(:,5)','beta12',par(:,6)','beta21',par(:,7)','beta22',par(:,8)','tau',par(:,9)');

%% 3. simulation for each subject: absolute fit methods
for n=1:nsub
    clear design_sub
    design_sub = design((design(:,1)==subs(n)),:); %data for a given subj. There is no missing trials in this exp, so we do not need to exclude missing trials
    if n==1
        sim_data   = abs_fit_m4(paras,design_sub,n,1);
    else
        sim_data   = [sim_data;abs_fit_m4(paras,design_sub,n,1)];
    end
    
end

%% 4. save data
names={'subid',	'gender','trial','pool','rate_s','rate_o','choice_raw','outcome_s','outcome_o','conds','party','frame','choice ','choice_sim','choice_allo'};
commaheader = [names;repmat({','},1,numel(names))];
commaheader=commaheader(:)';
textheader=cell2mat(commaheader);

fid = fopen('sim_data_m4_abs_fit_adult.csv','w');
fprintf(fid,'%s\n',textheader);
dlmwrite('sim_data_m4_abs_fit_all_adult(1).csv',sim_data,'-append');

fclose('all');

