function data = abs_fit_m4(param,design,subn,sampn)
rng('shuffle')
%% parameters
alpha11       = param.alpha11(sampn,subn);       
alpha12       = param.alpha12(sampn,subn);       
alpha21       = param.alpha21(sampn,subn);       
alpha22       = param.alpha22(sampn,subn);       
beta11          = param.beta11(sampn,subn);       
beta12          = param.beta12(sampn,subn);       
beta21          = param.beta21(sampn,subn);       
beta22          = param.beta22(sampn,subn);       
tau            = param.tau(sampn,subn);       
%% initialisation
data    = zeros(length(design),15);
%% initial setup
choice_sim     = zeros(length(design),1);%simulated choice
choice_allo     = zeros(length(design),1);%actual choice
trial                = design(:,3);
pool               = design(:,4);
rate_s            = design(:,5);
rate_o            = design(:,6);
party             = design(:,11);
frame            = design(:,12);

for nt=1:length(design) % loop over each trial of the block
    utility = zeros(pool(nt)+1,1);
    fenzi = zeros(pool(nt)+1,1);
    for cn = 1:pool(nt)+1
        Xself    = (cn-1) *rate_s(nt); 
        Xother   = (pool(nt)-(cn-1)) *rate_o(nt);
        if frame(nt) == 1 && party(nt) == 1
            utility(cn) = Xself - alpha11*max(Xself-Xother,0) - beta11*max(Xother-Xself,0);
        elseif party(nt) == 1 && frame(nt) == 2
            utility(cn) = Xself - alpha12*max(Xself-Xother,0) - beta12*max(Xother-Xself,0);
        elseif party(nt) == 2 && frame(nt) == 1
            utility(cn) = Xself - alpha21*max(Xself-Xother,0) - beta21*max(Xother-Xself,0);
        elseif party(nt) == 2 && frame(nt) == 2
            utility(cn) = Xself - alpha22*max(Xself-Xother,0) - beta22*max(Xother-Xself,0);
        end
         fenzi(cn)=exp(tau*utility(cn));
    end
    fenmu=sum(fenzi);
    prob= zeros(pool(nt)+1,1);
    for i=1:pool(nt)+1
        prob(i)=fenzi(i)/fenmu;
    end
    c(nt) = find(rand < cumsum(prob(:)),1); 
    choice_sim(nt) =  c(nt);
    choice_allo(nt) =  c(nt)-1;
    clear utility fenzi prob fenmu
    
    
end % nt

%% write data
data(:,1:13)  = design;
data(:,14)   = choice_sim;
data(:,15)   = choice_allo;
