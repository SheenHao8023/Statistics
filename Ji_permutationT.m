function CorrectedP=Ji_permutationT(Residual)

    LoadingDis = pdist(Residual,'Euclidean');
 
    Z = linkage(LoadingDis,'ward');
  
    T = cluster(Z,'maxclust',2);
    Cluster1=find(T==1);
    Cluster2=find(T==2);
    CovForCl1=CovAll(Cluster1,2);
    CovForCl2=CovAll(Cluster2,2);
  
[~,Rep,~ ,Restats]=ttest2(CovForCl1,CovForCl2);

%for categorical variate
% Gender = [CovForCl1;CovForCl2];
% group = [ones(length(CovForCl1),1);2*ones(length(CovForCl2),1)];
% group = nominal(group);
% fitglm(group,Gender,'distribution','binomial');
n1=length(find(CovForCl1==1));
n2=length(find(CovForCl2==1));
N1=size(CovForCl1,1);
N2=size(CovForCl2,1);
p0 = (n1+n2) / (N1+N2);
n10 = N1 * p0;
n20 = N2 * p0;
observed = [n1 N1-n1 n2 N2-n2];
expected = [n10 N1-n10 n20 N2-n20];
[h,p,stats] = chi2gof([1 2 3 4],'freq',observed,'expected',expected,'ctrs',[1 2 3 4],'nparams',2);
RefChi=stats.chi2stat;

%for more than two groups (ANOVAs)
Groups=mat2cell(X5ClusterLabelF,size(X5ClusterLabelF,1),[ones(1,size(X5ClusterLabelF,2))]); 

[P,tbl,stats]=anovan(CovAll(:,2),Groups,'display','off','sstype',3);

Fstats=tbl{2,5};

%for more than two groups catogorical (chi-sqaure)

n1=length(find(CovForCl1==1));
n2=length(find(CovForCl2==1));
n3=length(find(CovForCl3==1));
n4=length(find(CovForCl4==1));
n5=length(find(CovForCl5==1));
N1=size(CovForCl1,1);
N2=size(CovForCl2,1);
N3=size(CovForCl3,1);
N4=size(CovForCl4,1);
N5=size(CovForCl5,1);
p0 = (n1+n2+n3+n4+n5) / (N1+N2+N3+N4+N5);
n10 = N1 * p0;
n20 = N2 * p0;
n30 = N3 * p0;
n40 = N4 * p0;
n50 = N5 * p0;
observed = [n1 N1-n1 n2 N2-n2 n3 N3-n3 n4 N4-n4 n5 N5-n5];
expected = [n10 N1-n10 n20 N2-n20 n30 N3-n30 n40 N4-n40 n50 N5-n50];
[h,p,stats] = chi2gof([1 2 3 4 5 6 7 8 9 10],'freq',observed,'expected',expected,'ctrs',[1 2 3 4 5 6 7 8 9 10],'nparams',5);
RefChi=stats.chi2stat;

for sample=1:10000
    
 Rand=randperm(size(Residual,1));
 
 ExtractedT=T(Rand',:);
 
    RandCluster1=find(ExtractedT==1);
    RandCluster2=find(ExtractedT==2);
    RandCovForCl1=CovAll(RandCluster1,2);
    RandCovForCl2=CovAll(RandCluster2,2);
    
    [~,p,~ ,stats]=ttest2(RandCovForCl1,RandCovForCl2);
    
    AllP(sample,1)=p;
    AllT(sample,1)=stats.tstat;
    
    
    %modified for post-hoc two sample t after ANOVA
    
     TAll=X5ClusterLabelF;
     idx=find(X5ClusterLabelF==3|X5ClusterLabelF==4|X5ClusterLabelF==5);
     TAll(idx,:)=[];
     ClusterAll=[Cluster1',Cluster2']';
     ExtractedCov=CovAll(ClusterAll,4);
     Rand=randperm(size(TAll,1));
     ExtractedT=TAll(Rand',:);
     RandCluster1=find(ExtractedT==1);
     RandCluster2=find(ExtractedT==2);
     RandCovForCl1=ExtractedCov(RandCluster1,1);
     RandCovForCl2=ExtractedCov(RandCluster2,1);
     [~,p,~ ,stats]=ttest2(RandCovForCl1,RandCovForCl2);
     AllP(sample,1)=p;
     AllT(sample,1)=stats.tstat;
    
    
    %for more than two groups (ANOVAs)
    
     ExtractedT=X5ClusterLabelF(Rand',:);
 
     Groups=mat2cell(ExtractedT,size(ExtractedT,1),[ones(1,size(ExtractedT,2))]); 

    [P,tbl,stats]=anovan(CovAll(:,2),Groups,'display','off','sstype',3);

    AllF(sample,1)=tbl{2,5};

        
    %chi-square test
    
    n1=length(find(RandCovForCl1==1));
    n2=length(find(RandCovForCl2==1));
    N1=size(RandCovForCl1,1);
    N2=size(RandCovForCl2,1);
    p0 = (n1+n2) / (N1+N2);
    n10 = N1 * p0;
    n20 = N2 * p0;
    observed = [n1 N1-n1 n2 N2-n2];
    expected = [n10 N1-n10 n20 N2-n20];
    [h,p,stats] = chi2gof([1 2 3 4],'freq',observed,'expected',expected,'ctrs',[1 2 3 4],'nparams',2);
    AllChi(sample,1)=stats.chi2stat;
     clear p0 n10 n20 observed expected
     
      %chi-square test for 5 groups
    
    n1=length(find(RandCovForCl1==1));
    n2=length(find(RandCovForCl2==1));
    n3=length(find(RandCovForCl3==1));
    n4=length(find(RandCovForCl4==1));
    n5=length(find(RandCovForCl5==1));
    N1=size(RandCovForCl1,1);
    N2=size(RandCovForCl2,1);
    N3=size(RandCovForCl3,1);
    N4=size(RandCovForCl4,1);
    N5=size(RandCovForCl5,1);
    
    p0 = (n1+n2+n3+n4+n5) / (N1+N2+N3+N4+N5);
    n10 = N1 * p0;
    n20 = N2 * p0;
    n30 = N3 * p0;
    n40 = N4 * p0;
    n50 = N5 * p0;
    observed = [n1 N1-n1 n2 N2-n2 n3 N3-n3 n4 N4-n4 n5 N5-n5];
    expected = [n10 N1-n10 n20 N2-n20 n30 N3-n30 n40 N4-n40 n50 N5-n50];
    [h,p,stats] = chi2gof([1 2 3 4 5 6 7 8 9 10],'freq',observed,'expected',expected,'ctrs',[1 2 3 4 5 6 7 8 9 10],'nparams',5);
    AllChi(sample,1)=stats.chi2stat;
     clear p0 n10 n20 n30 n40 n50 observed expected
    fprintf(1,'%s\n',['sample ' int2str(sample) ' done'])
end

CorrectedP=(length(find(AllT>=Restats.tstat))+1)/10001;%upper-tailed p-value
CorrectedP1=(length(find(AllT<=Restats.tstat))+1)/10001;%lower-tailed p-value
if ((2*CorrectedP)<1 | (2*CorrectedP1)<1)==1
TwoTailedP=min(2.*CorrectedP,2.*CorrectedP1);
else
TwoTailedP=1;
end

%for more than two groups using ANOVA

CorrectedP=(length(find(AllF>=Fstats))+1)/10001;%upper-tailed p-value
CorrectedP1=(length(find(AllF<=Fstats))+1)/10001;%lower-tailed p-value
if ((2*CorrectedP)<1 | (2*CorrectedP1)<1)==1
TwoTailedP=min(2.*CorrectedP,2.*CorrectedP1);
else
TwoTailedP=1;
end

%chi-square test
CorrectedP=(length(find(AllChi>=RefChi))+1)/10001;%upper-tailed p-value
% CorrectedP1=(length(find(AllChi<=RefChi))+1)/10001;%lower-tailed p-value
% if ((2*CorrectedP)<1 | (2*CorrectedP1)<1)==1
% TwoTailedP=min(2.*CorrectedP,2.*CorrectedP1);
% else
% TwoTailedP=1;
% end

   