
% DiNuzzo, M., Giove, F., Maraviglia, B. et al. 
% Computational Flux Balance Analysis Predicts that Stimulation of Energy Metabolism in Astrocytes 
% and their Metabolic Interactions with Neurons Depend on Uptake of K+ Rather than Glutamate. 
% Neurochem Res 42, 202-216 (2017). 
% https://doi.org/10.1007/s11064-016-2048-0
%
% https://link.springer.com/article/10.1007/s11064-016-2048-0

function plot_summary(what)

fprintf('CLPFBA (DiNuzzo et al, Neurochem Res 2017)\n\n');

if (nargin==0)
    fprintf('Figures\n');
    fprintf('1: Flux domains\n');
    fprintf('2: Core model\n');
    fprintf('3: [OLD: 3 panels] Model tuning (assumptions on ionic currents) [*]\n');
    fprintf('4: Glucose/Lactate uptake\n');
    fprintf('5: Pearson correlation matrix\n');
    fprintf('6: HK vs PDH\n');
    fprintf('7: GDH and ammonia homeostasis\n');
    fprintf('8: Distribution histograms (extended)\n'); 
    fprintf('9: Distribution histograms (selected, no objective-function)\n'); 
    fprintf('10: Model tuning PC (suppl)\n');
    fprintf('11: [NEW: 15 panels] Model tuning (assumptions on ionic currents) [*]\n');
    fprintf('\n[*] Automatic data loading.\n\n');
    return
end

default_path = '../projects/brain';
screensize = [1 41, 1600, 781]; 


global n labels lowerbounds upperbounds

if (what==3 || what==6 || what==10 || what==11)
    data = single(importdata([default_path, '/1_core_model_ions_60_60/output_solutions.dat']));  % datatype: single
    domains = importdata([default_path, '/1_core_model_ions_60_60/output_domains.dat']);
    labels = domains.colheaders;
    n = length(labels);
    lowerbounds = domains.data(1, :);
    upperbounds = domains.data(2, :);    
    data_2020 = single(importdata([default_path, '/1_core_model_ions_20_20/output_solutions.dat']));  % datatype: single
    data_4040 = single(importdata([default_path, '/1_core_model_ions_40_40/output_solutions.dat']));  % datatype: single
    data_6060 = single(importdata([default_path, '/1_core_model_ions_60_60/output_solutions.dat']));  % datatype: single
    data_8080 = single(importdata([default_path, '/1_core_model_ions_80_80/output_solutions.dat']));  % datatype: single
    data_100100 = single(importdata([default_path, '/1_core_model_ions_100_100/output_solutions.dat']));  % datatype: single
    data_6044 = single(importdata([default_path, '/1_core_model_ions_60_44/output_solutions.dat']));  % datatype: single
    data_6052 = single(importdata([default_path, '/1_core_model_ions_60_52/output_solutions.dat']));  % datatype: single
    data_6068 = single(importdata([default_path, '/1_core_model_ions_60_68/output_solutions.dat']));  % datatype: single
    data_6076 = single(importdata([default_path, '/1_core_model_ions_60_76/output_solutions.dat']));  % datatype: single
    data_025 = single(importdata([default_path, '/1_core_model_aros_025/output_solutions.dat']));  % datatype: single
    data_050 = single(importdata([default_path, '/1_core_model_aros_050/output_solutions.dat']));  % datatype: single
    data_100 = single(importdata([default_path, '/1_core_model_aros_100/output_solutions.dat']));  % datatype: single
    data_200 = single(importdata([default_path, '/1_core_model_aros_200/output_solutions.dat']));  % datatype: single
else
    directory = uigetdir(default_path);

    domains = importdata([directory, '/output_domains.dat']);
    labels = domains.colheaders;
    n = length(labels);
    lowerbounds = domains.data(1, :);
    upperbounds = domains.data(2, :);
    data = single(importdata([directory, '/output_solutions.dat']));  % datatype: single
    data0 = single(importdata([default_path, '/0_awake/output_solutions.dat']));  % datatype: single
    udata = single(importdata([directory, '/output_user.dat']));  % datatype: single
        
end


ALL = length(data);
bins = ceil(2*ALL^(1/3));

   
plot_col = [
    0    0.4470    0.7410; ...
    0.8500    0.3250    0.0980; ...
    0.9290    0.6940    0.1250; ...
    0.4940    0.1840    0.5560; ...
    0.4660    0.6740    0.1880; ...
    0.3010    0.7450    0.9330; ...
    0.6350    0.0780    0.1840; ...
    0.4 0.4 0.4
    ];    

    osize = 0.1;
    n_col = plot_col(1, :); %[0.4470    0.4470    0.4470]; %[0 172/255 248/255]; %[0    0.4470    0.7410];
    a_col = plot_col(2, :); %[0.4660    0.6740    0.1880]; %[0.8 0.1 0]; %[0.8500    0.3250    0.0980];
    x_col = [0 0 0];
    p_col = [0.85 0.85 0.85];    
    s_marker = 'o';

%%
% --------------------------------
% domains

if (~isempty(find(what==1)))

    figure('Color','w','Position',screensize);
    subplot(2,1,1);
    %scatter(1:n,lowerbounds,'Marker','none'); hold on;
    %scatter(1:n,upperbounds,'Marker','none');
    annotate_domains;
    %panel('A',0.21);
    
    hold on;
    
    domains = importdata([default_path, '/0_awake/output_domains.dat']);
    labels = domains.colheaders;
    n = length(labels);
    lowerbounds = domains.data(1, :);
    upperbounds = domains.data(2, :);    
    
    %subplot(2,1,2);
    %scatter(1:n,lowerbounds,'Marker','none'); hold on;
    %scatter(1:n,upperbounds,'Marker','none');    
    annotate_domains([0    0.4470    0.7410   ]);
    %('B',0.21);
    
    legend({['    0 ',char(8804),' V_{cyc} ',char(8804),' 0.51 {\mu}mol g^{-1} min^{-1}'], ...
        '    V_{cyc} = 0.51 {\mu}mol g^{-1} min^{-1} (awake)'}, 'fontsize', 10);
    legend boxoff;
    
end

%%
% --------------------------------
% pearson's

if (~isempty(find(what==5)))

    figure('Color','w','Position',screensize);
    [ccorr, ~] = corr(data);
    subplot(1,8,2:8);
    ccorr(n+1, :) = 0;
    ccorr(:, n+1) = 0;
    pcolor(1:n+1, n+1:-1:1, ccorr);

    annotate_pearson;

end

%%
% --------------------------------
% distributions (before)


if (~isempty(find(what==8)))
    
    letter = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};    
    
    plots = [4,6];
    t = 1;
    figure('Color','w','Position',screensize);
    for i=1:n
        d = data(:,i);  
        d0 = data0(:,i); 
        u = udata(:, i);
        mind=min(d);
        maxd=max(d);
        range=maxd-mind;
        d_mean = nanmean(d);
        d_std = nanstd(d);
        d_mean0 = nanmean(d0);
        d_std0 = nanstd(d0);         
        if (range > 0)    
            subplot(plots(1),plots(2),t); 
            hist(d, bins);
            bins0 = round(bins*(max(d0)-min(d0))/range);
            hold on;
            hist(d0, bins0);            
            from=mind-range/5;
            to=maxd+range/5;        
            annotate_distr(labels(i),from,to, d_mean, d_std, lowerbounds(i), upperbounds(i), u, d_mean0, d_std0); 
            panel(letter{t},-0.1);
            t = t+1;          
            if (rem(t-1,plots(1)*plots(2)) == 0)
                figure('Color','w','Position',screensize);
                t = 1;
            end
        end
    end
    
end


%%
if (~isempty(find(what==9)))
    
    letter = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};
    
    plots = [4,6];
    t = 1;
    figure('Color','w','Position',screensize);
    for i=1:n
        d = data(:,i);   
        d0 = data0(:,i); 
        mind=min(d);
        maxd=max(d);
        range=maxd-mind;
        d_mean = nanmean(d);
        d_std = nanstd(d);
        d_mean0 = nanmean(d0);
        d_std0 = nanstd(d0);        
        if (range > 0)    
            subplot(plots(1),plots(2),t); 
            hist(d, bins);
            bins0 = round(bins*(max(d0)-min(d0))/range);
            hold on;
            hist(d0, bins0);
            from=mind-range/5;
            to=maxd+range/5;        
            annotate_distr2(labels(i),from,to, d_mean, d_std, d_mean0, d_std0, lowerbounds(i), upperbounds(i));                 
            panel(letter{t},-0.1);            
            t = t+1;          
            if (rem(t-1,plots(1)*plots(2)) == 0)
                figure('Color','w','Position',screensize);
                t = 1;
            end
        end
    end
    
end


%%
% --------------------------------
% misc plots

cmrglc_idx = find(strcmp(labels,'J_201_be_GLUT'));
cmro2_idx = find(strcmp(labels,'J_204_b_O2'));

ntn_idx = find(strcmp(labels,'J_101_n_NT'));
%vcyc_idx = find(strcmp(labels,'J_108_a_GS')); 
vcyc_idx = find(strcmp(labels,'J_109_ae_SN')); 

pdhn_idx = find(strcmp(labels,'J_601_n_PDH'));
pdha_idx = find(strcmp(labels,'J_701_a_PDH'));
pca_idx = find(strcmp(labels,'J_514_a_PC'));

jglcen_idx = find(strcmp(labels,'J_202_en_GLUT'));
jglcea_idx = find(strcmp(labels,'J_203_ea_GLUT'));

jgphos_idx = find(strcmp(labels,'J_301_a_GPhos'));

jlacne_idx = find(strcmp(labels,'J_210_ne_MCT'));
jlacae_idx = find(strcmp(labels,'J_211_ae_MCT'));

cmro2n_idx = find(strcmp(labels,'J_205_bn_O2'));
cmro2a_idx = find(strcmp(labels,'J_206_ba_O2'));

ogimin = lowerbounds(cmro2_idx)/upperbounds(cmrglc_idx)
ogimax = upperbounds(cmro2_idx)/lowerbounds(cmrglc_idx)


cmro2 = data(:, cmro2_idx);
cmrglc = data(:, cmrglc_idx);
ogi = cmro2./cmrglc;

ntn = data(:, ntn_idx);
vcyc = data(:, vcyc_idx);

pdhn = data(:, pdhn_idx);
pdha = data(:, pdha_idx);
pca = data(:, pca_idx);

jglcen = data(:, jglcen_idx);
jglcea = data(:, jglcea_idx);

jgphos = data(:, jgphos_idx);

%figure; scatter(jglcea, jgphos); xlabel('J-GLUT-ea'); ylabel('J-GPhos');

jlacne = data(:, jlacne_idx);
jlacae = data(:, jlacae_idx);

cmro2n = data(:, cmro2n_idx);
cmro2a = data(:, cmro2a_idx);

f_glc = [length(jglcen(jglcen>=jglcea)); length(jglcea(jglcea>jglcen))];
f_lac = [length(jlacne(jlacne>=0)); length(jlacne(jlacne<0))]; % note that jlacae is the opposite of jlacne (jlacae+jlacne)=0

[m_vcyc, m_jpdhn] = compute_mean(vcyc, pdhn, bins);
[~, m_jpdha] = compute_mean(vcyc, pdha, bins);
[~, m_jpca] = compute_mean(vcyc, pca, bins);
[~, m_jglcen] = compute_mean(vcyc, jglcen, bins);
[~, m_jglcea] = compute_mean(vcyc, jglcea, bins);
[~, m_jlacne] = compute_mean(vcyc, jlacne, bins);
[~, m_jlacae] = compute_mean(vcyc, jlacae, bins);



% info
fprintf('JnPDH (awake): %f\n', (m_jpdhn(end)));
fprintf('JaPDH (awake): %f\n', (m_jpdha(end)));
fprintf('JaPC (awake): %f\n', (m_jpca(end)));
fprintf('JenGLUT (awake): %f\n', (m_jglcen(end)));
fprintf('JeaGLUT (awake): %f\n', (m_jglcea(end)));
fprintf('JneMCT (awake): %f\n', (m_jlacne(end)));
fprintf('JaeMCT (awake): %f\n', (m_jlacae(end)));
%
fprintf('JnPDH (overall): %f +/- %f\n', (mean(pdhn(:))), (std(pdhn(:))));
fprintf('JaPDH (overall): %f +/- %f\n', (mean(pdha(:))), (std(pdha(:))));
fprintf('JaPC (overall): %f +/- %f\n', (mean(pca(:))), (std(pca(:))));
fprintf('JenGLUT (overall): %f +/- %f\n', (mean(jglcen(:))), (std(jglcen(:))));
fprintf('JeaGLUT (overall): %f +/- %f\n', (mean(jglcea(:))), (std(jglcea(:))));
fprintf('JneMCT (overall): %f +/- %f\n', (mean(jlacne(:))), (std(jlacne(:))));
fprintf('JaeMCT (overall): %f +/- %f\n', (mean(jlacae(:))), (std(jlacae(:))));


bins3 = 2*bins;

global units xl
units = ' ({\mu}mol g^{-1} min^{-1})';
xl = ['Vcyc', units];

global vcyc_0
vcyc_0 = 0.51;

lwidth = 1.5;

%%

if (~isempty(find(what==2)))

    figure('Color','w','Position',screensize);

    
    subplot(2,5,2); scatter(vcyc, cmrglc, osize, 'Marker', s_marker, 'MarkerEdgeColor', [0 0 0]);  box on; xlabel(xl); ylabel(['CMRGlc',units]); 
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'k','LineStyle',':'); 
    xlim([0,vcyc_0]);
    %hold on; line(get(gca,'XLim'), [0.4,0.4], 'LineStyle',':');
    panel('A',-0.0);

    subplot(2,5,3); scatter(vcyc, cmro2, osize, 'Marker', s_marker, 'MarkerEdgeColor', [0 0 0]);  box on; xlabel(xl); ylabel(['CMRO_2',units]); 
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'k','LineStyle',':'); 
    xlim([0,vcyc_0]);
    %hold on; line(get(gca,'XLim'), [0.4,0.4], 'LineStyle',':');
    panel('B',-0.05);
    
    subplot(2,5,4); [ne,ce]=hist(ogi, bins); bar(ce,ne,'FaceColor',[1 1 1],'BarWidth',1); 
    xlabel('Oxygen-Glucose Index (OGI)'); 
    ylabel('Counts');
    xlim([5.0, 5.6]);
    %scale = 0.5;
    %hp = get(gca,'Position');
    %set(gca,'Position',[hp(1),hp(2),scale*hp(3),scale*hp(4)]);
    %set(gca,'xaxislocation','top');
    %set(gca,'yaxislocation','right');
    %box off;
    panel('C',-0.05);
    
    subplot(2,5,7); [cc, ~]=hist3([vcyc, pdhn], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(pdhn), max(pdhn), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Neuronal PDH Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jpdhn, 'w', 'LineWidth', lwidth);
    x_vcyc = linspace(min(vcyc), max(vcyc), 100);
    y_cmrglcoxn = 2.*(0.87.*x_vcyc + 0.10); % Hyder et al, PNAS 2013 (2* takes into account pdh vs cmrglconx)
    hold on; plot(x_vcyc, y_cmrglcoxn, 'r:', 'LineWidth', lwidth); % experimental
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('D');

    subplot(2,5,8); [cc, ~]=hist3([vcyc, pdha], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(pdha), max(pdha), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Astrocytic PDH Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jpdha, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('E');
    
    subplot(2,5,9); [cc, ~]=hist3([vcyc, pca], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(pca), max(pca), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Astrocytic PC Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jpca, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('F',-0.05);
    

    
    
    colormap jet
    h = colorbar('eastoutside');
    hp = get(h, 'Position');
    ap = get(gca, 'Position');
    set(h, 'Position', [ap(1)+ap(3).*1.65, ap(2), hp(3)./2, ap(4)]);
    ylabel(h, 'Counts', 'FontSize', 12);
    set(h,'TickLength', 0.005);        
    
    %{
    colormap jet
    h = colorbar('southoutside');
    hp = get(h, 'Position');
    ap = get(gca, 'Position');
    set(h, 'Position', [ap(1), hp(2).*0.2, ap(3), hp(4)./3]);
    ylabel(h, 'Counts', 'FontSize', 12);
    set(h,'TickLength', 0.005);
    %}

end



if (~isempty(find(what==10)))
    
    ddx = 0;
    
    vcyc_idx = find(strcmp(labels,'J_109_ae_SN')); 
    pdha_idx = find(strcmp(labels,'J_701_a_PDH'));
    pca_idx = find(strcmp(labels,'J_514_a_PC'));
    
    vcyc_6044 = data_6044(:, vcyc_idx);
    pdha_6044 = data_6044(:, pdha_idx);
    pca_6044 = data_6044(:, pca_idx);    
    vcyc_6052 = data_6052(:, vcyc_idx);
    pdha_6052 = data_6052(:, pdha_idx);
    pca_6052 = data_6052(:, pca_idx); 
    vcyc_6060 = data_6060(:, vcyc_idx);
    pdha_6060 = data_6060(:, pdha_idx);
    pca_6060 = data_6060(:, pca_idx);        
    vcyc_6068 = data_6068(:, vcyc_idx);
    pdha_6068 = data_6068(:, pdha_idx);
    pca_6068 = data_6068(:, pca_idx); 
    vcyc_6076 = data_6076(:, vcyc_idx);
    pdha_6076 = data_6076(:, pdha_idx);
    pca_6076 = data_6076(:, pca_idx);       
    
    [mvcyc_6044, mpdha_6044, spdha_6044] = compute_mean(vcyc_6044, pdha_6044, bins);
    [~, mpca_6044, spca_6044] = compute_mean(vcyc_6044, pca_6044, bins);
    [mvcyc_6052, mpdha_6052, spdha_6052] = compute_mean(vcyc_6052, pdha_6052, bins);
    [~, mpca_6052, spca_6052] = compute_mean(vcyc_6052, pca_6052, bins);
    [mvcyc_6060, mpdha_6060, spdha_6060] = compute_mean(vcyc_6060, pdha_6060, bins);
    [~, mpca_6060, spca_6060] = compute_mean(vcyc_6060, pca_6060, bins);    
    [mvcyc_6068, mpdha_6068, spdha_6068] = compute_mean(vcyc_6068, pdha_6068, bins);   
    [~, mpca_6068, spca_6068] = compute_mean(vcyc_6068, pca_6068, bins);
    [mvcyc_6076, mpdha_6076, spdha_6076] = compute_mean(vcyc_6076, pdha_6076, bins);
    [~, mpca_6076, spca_6076] = compute_mean(vcyc_6076, pca_6076, bins);    
    
    subplot(2,5,2);
    myshadedplot(mvcyc_6044, mpdha_6044, spdha_6044, plot_col(1,:)); hold on;
    myshadedplot(mvcyc_6052, mpdha_6052, spdha_6052, plot_col(2,:)); hold on;
    myshadedplot(mvcyc_6060, mpdha_6060, spdha_6060, plot_col(3,:)); hold on;
    myshadedplot(mvcyc_6068, mpdha_6068, spdha_6068, plot_col(4,:)); hold on;
    myshadedplot(mvcyc_6076, mpdha_6076, spdha_6076, plot_col(5,:)); hold on;    
    xlim([0-ddx,vcyc_0+ddx]);
    xlabel(xl); ylabel(['Astrocytic PDH Rate',units]);
    legend({'60/44 (Na^+/K^+)', '60/52', '60/60', '60/68', '60/76'}, 'location','northwest');
    legend boxoff;
    panel('A');    
    ylim([0,1]);
    
    subplot(2,5,3);
    myshadedplot(mvcyc_6044, mpca_6044, spca_6044, plot_col(1,:)); hold on;
    myshadedplot(mvcyc_6052, mpca_6052, spca_6052, plot_col(2,:)); hold on;
    myshadedplot(mvcyc_6060, mpca_6060, spca_6060, plot_col(3,:)); hold on;
    myshadedplot(mvcyc_6068, mpca_6068, spca_6068, plot_col(4,:)); hold on;
    myshadedplot(mvcyc_6076, mpca_6076, spca_6076, plot_col(5,:)); hold on;    
    xlim([0-ddx,vcyc_0+ddx]);
    xlabel(xl); ylabel(['Astrocytic PC Rate',units]);
    legend({'60/44 (Na^+/K^+)', '60/52', '60/60', '60/68', '60/76'}, 'location','northwest');
    legend boxoff;
    panel('B');    
    ylim([0,0.4]);    
    
end




%-------------------------------------------------- NEW
if (~isempty(find(what==11)))
    
    vcyc_idx = find(strcmp(labels,'J_109_ae_SN')); 
    pdhn_idx = find(strcmp(labels,'J_601_n_PDH'));
    pdha_idx = find(strcmp(labels,'J_701_a_PDH'));
    pca_idx = find(strcmp(labels,'J_514_a_PC'));
    
    vcyc_2020 = data_2020(:, vcyc_idx);
    pdhn_2020 = data_2020(:, pdhn_idx);
    pdha_2020 = data_2020(:, pdha_idx);
    pca_2020 = data_2020(:, pca_idx);
    vcyc_4040 = data_4040(:, vcyc_idx);
    pdhn_4040 = data_4040(:, pdhn_idx);
    pdha_4040 = data_4040(:, pdha_idx);
    pca_4040 = data_4040(:, pca_idx);
    vcyc_6060 = data_6060(:, vcyc_idx);
    pdhn_6060 = data_6060(:, pdhn_idx);
    pdha_6060 = data_6060(:, pdha_idx);
    pca_6060 = data_6060(:, pca_idx);    
    vcyc_8080 = data_8080(:, vcyc_idx);
    pdhn_8080 = data_8080(:, pdhn_idx);
    pdha_8080 = data_8080(:, pdha_idx);
    pca_8080 = data_8080(:, pca_idx);    
    vcyc_100100 = data_100100(:, vcyc_idx);
    pdhn_100100 = data_100100(:, pdhn_idx);
    pdha_100100 = data_100100(:, pdha_idx);
    pca_100100 = data_100100(:, pca_idx);  
    
    vcyc_6044 = data_6044(:, vcyc_idx);
    pdhn_6044 = data_6044(:, pdhn_idx);
    pdha_6044 = data_6044(:, pdha_idx);
    pca_6044 = data_6044(:, pca_idx);    
    vcyc_6052 = data_6052(:, vcyc_idx);
    pdhn_6052 = data_6052(:, pdhn_idx);
    pdha_6052 = data_6052(:, pdha_idx);
    pca_6052 = data_6052(:, pca_idx); 
    vcyc_6068 = data_6068(:, vcyc_idx);
    pdhn_6068 = data_6068(:, pdhn_idx);
    pdha_6068 = data_6068(:, pdha_idx);
    pca_6068 = data_6068(:, pca_idx); 
    vcyc_6076 = data_6076(:, vcyc_idx);
    pdhn_6076 = data_6076(:, pdhn_idx);
    pdha_6076 = data_6076(:, pdha_idx);
    pca_6076 = data_6076(:, pca_idx);     
    
    [mvcyc_2020, mpdhn_2020, spdhn_2020] = compute_mean(vcyc_2020, pdhn_2020, bins);
    [~, mpdha_2020, spdha_2020] = compute_mean(vcyc_2020, pdha_2020, bins);
    [~, mpca_2020, spca_2020] = compute_mean(vcyc_2020, pca_2020, bins);
    [mvcyc_4040, mpdhn_4040, spdhn_4040] = compute_mean(vcyc_4040, pdhn_4040, bins);
    [~, mpdha_4040, spdha_4040] = compute_mean(vcyc_4040, pdha_4040, bins);
    [~, mpca_4040, spca_4040] = compute_mean(vcyc_4040, pca_4040, bins);
    [mvcyc_6060, mpdhn_6060, spdhn_6060] = compute_mean(vcyc_6060, pdhn_6060, bins);
    [~, mpdha_6060, spdha_6060] = compute_mean(vcyc_6060, pdha_6060, bins);
    [~, mpca_6060, spca_6060] = compute_mean(vcyc_6060, pca_6060, bins);
    [mvcyc_8080, mpdhn_8080, spdhn_8080] = compute_mean(vcyc_8080, pdhn_8080, bins);
    [~, mpdha_8080, spdha_8080] = compute_mean(vcyc_8080, pdha_8080, bins);
    [~, mpca_8080, spca_8080] = compute_mean(vcyc_8080, pca_8080, bins);
    [mvcyc_100100, mpdhn_100100, spdhn_100100] = compute_mean(vcyc_100100, pdhn_100100, bins);
    [~, mpdha_100100, spdha_100100] = compute_mean(vcyc_100100, pdha_100100, bins);
    [~, mpca_100100, spca_100100] = compute_mean(vcyc_100100, pca_100100, bins);
    
    [mvcyc_6044, mpdhn_6044, spdhn_6044] = compute_mean(vcyc_6044, pdhn_6044, bins);
    [~, mpdha_6044, spdha_6044] = compute_mean(vcyc_6044, pdha_6044, bins);
    [~, mpca_6044, spca_6044] = compute_mean(vcyc_6044, pca_6044, bins);
    [mvcyc_6052, mpdhn_6052, spdhn_6052] = compute_mean(vcyc_6052, pdhn_6052, bins);
    [~, mpdha_6052, spdha_6052] = compute_mean(vcyc_6052, pdha_6052, bins);
    [~, mpca_6052, spca_6052] = compute_mean(vcyc_6052, pca_6052, bins);
    [mvcyc_6068, mpdhn_6068, spdhn_6068] = compute_mean(vcyc_6068, pdhn_6068, bins);
    [~, mpdha_6068, spdha_6068] = compute_mean(vcyc_6068, pdha_6068, bins);
    [~, mpca_6068, spca_6068] = compute_mean(vcyc_6068, pca_6068, bins);
    [mvcyc_6076, mpdhn_6076, spdhn_6076] = compute_mean(vcyc_6076, pdhn_6076, bins);
    [~, mpdha_6076, spdha_6076] = compute_mean(vcyc_6076, pdha_6076, bins);
    [~, mpca_6076, spca_6076] = compute_mean(vcyc_6076, pca_6076, bins);    
    
    
    vcyc_025 = data_025(:, vcyc_idx);
    pca_025 = data_025(:, pca_idx);
    vcyc_050 = data_050(:, vcyc_idx);
    pca_050 = data_050(:, pca_idx);
    vcyc_100 = data_100(:, vcyc_idx);
    pca_100 = data_100(:, pca_idx);
    vcyc_200 = data_200(:, vcyc_idx);
    pca_200 = data_200(:, pca_idx);    
    
    [mvcyc_025, mpca_025, spca_025] = compute_mean(vcyc_025, pca_025, bins);
    [mvcyc_050, mpca_050, spca_050] = compute_mean(vcyc_050, pca_050, bins);
    [mvcyc_100, mpca_100, spca_100] = compute_mean(vcyc_100, pca_100, bins);
    [mvcyc_200, mpca_200, spca_200] = compute_mean(vcyc_200, pca_200, bins);
    [mvcyc_250, mpca_250, spca_250] = compute_mean(vcyc, pca, bins);
    
    % experimental (see Table 2 in the paper)
    % first point is to widen errorbar tick lenth
    em_vcyc = [-1,     0.01 0.15 0.40 0.04 0.31 0.22 0.51 0.02 0.58 0.27 0.49 0.16 0.28 0.12 0.40 0.12];
    em_pdhn = [0,       0.16 0.40 0.90 0.41 0.80 0.52 1.16 0.34 1.22 0.47 0.98 0.50 0.56 0.36 0.82 0.44];
    es_pdhn = [0,       0.10 0.08 0.28 0.05 0.16 0.04 0.17 0.12 0.04 0.04 0.06 0.05 0.07 0.04 0.08 0.01];
    em_pdha = [0,       nan, nan, nan, 0.28 nan, nan, 0.30 nan, nan, 0.14 nan, nan, nan, nan, nan, 0.23];
    es_pdha = [0,       nan, nan, nan, 0.07 nan, nan, 0.11 nan, nan, 0.03 nan, nan, nan, nan, nan, 0.02];
    em_pca =  [0,       nan, nan, nan, 0.04 nan, 0.06 0.18 nan, nan, nan, nan, nan, nan, 0.09 0.17 0.07];
    es_pca =  [0,       nan, nan, nan, 0.01 nan, 0.01 0.04 nan, nan, nan, nan, nan, nan, 0.01 0.02 0.004];
    
   
    subplot(3,5,1); myshadedplot(mvcyc_2020, mpdhn_2020, spdhn_2020, plot_col(1,:)); ann11a(1,1,em_vcyc, em_pdhn, es_pdhn);
    subplot(3,5,2); myshadedplot(mvcyc_4040, mpdhn_4040, spdhn_4040, plot_col(2,:)); ann11a(1,2,em_vcyc, em_pdhn, es_pdhn);
    subplot(3,5,3); myshadedplot(mvcyc_6060, mpdhn_6060, spdhn_6060, plot_col(3,:)); ann11a(1,3,em_vcyc, em_pdhn, es_pdhn);
    subplot(3,5,4); myshadedplot(mvcyc_8080, mpdhn_8080, spdhn_8080, plot_col(4,:)); ann11a(1,4,em_vcyc, em_pdhn, es_pdhn);
    subplot(3,5,5); myshadedplot(mvcyc_100100, mpdhn_100100, spdhn_100100, plot_col(5,:));     ann11a(1,5,em_vcyc, em_pdhn, es_pdhn);
   
   
    subplot(3,5,6); myshadedplot(mvcyc_6044, mpdha_6044, spdha_6044, plot_col(1,:)); ann11a(2,1,em_vcyc, em_pdha, es_pdha);
    subplot(3,5,7); myshadedplot(mvcyc_6052, mpdha_6052, spdha_6052, plot_col(2,:)); ann11a(2,2,em_vcyc, em_pdha, es_pdha);
    subplot(3,5,8); myshadedplot(mvcyc_6060, mpdha_6060, spdha_6060, plot_col(3,:)); ann11a(2,3,em_vcyc, em_pdha, es_pdha);
    subplot(3,5,9); myshadedplot(mvcyc_6068, mpdha_6068, spdha_6068, plot_col(4,:)); ann11a(2,4,em_vcyc, em_pdha, es_pdha);
    subplot(3,5,10); myshadedplot(mvcyc_6076, mpdha_6076, spdha_6076, plot_col(5,:));     ann11a(2,5,em_vcyc, em_pdha, es_pdha);
   
    
    subplot(3,5,11);    myshadedplot(mvcyc_025, mpca_025, spca_025, plot_col(1,:)); ann11a(3,1,em_vcyc, em_pca, es_pca);
    subplot(3,5,12);myshadedplot(mvcyc_050, mpca_050, spca_050, plot_col(2,:)); ann11a(3,2,em_vcyc, em_pca, es_pca);
    subplot(3,5,13);myshadedplot(mvcyc_100, mpca_100, spca_100, plot_col(3,:)); ann11a(3,3,em_vcyc, em_pca, es_pca);
    subplot(3,5,14);myshadedplot(mvcyc_200, mpca_200, spca_200, plot_col(4,:));    ann11a(3,4,em_vcyc, em_pca, es_pca);
    subplot(3,5,15);myshadedplot(mvcyc_250, mpca_250, spca_250, plot_col(5,:));    ann11a(3,5,em_vcyc, em_pca, es_pca);
   
    
end
%--------------------------------------------------
%--------------------------------------------------

if (~isempty(find(what==3)))
    
    vcyc_idx = find(strcmp(labels,'J_109_ae_SN')); 
    pdhn_idx = find(strcmp(labels,'J_601_n_PDH'));
    pdha_idx = find(strcmp(labels,'J_701_a_PDH'));
    pca_idx = find(strcmp(labels,'J_514_a_PC'));
    
    vcyc_2020 = data_2020(:, vcyc_idx);
    pdhn_2020 = data_2020(:, pdhn_idx);
    pdha_2020 = data_2020(:, pdha_idx);
    pca_2020 = data_2020(:, pca_idx);
    vcyc_4040 = data_4040(:, vcyc_idx);
    pdhn_4040 = data_4040(:, pdhn_idx);
    pdha_4040 = data_4040(:, pdha_idx);
    pca_4040 = data_4040(:, pca_idx);
    vcyc_6060 = data_6060(:, vcyc_idx);
    pdhn_6060 = data_6060(:, pdhn_idx);
    pdha_6060 = data_6060(:, pdha_idx);
    pca_6060 = data_6060(:, pca_idx);    
    vcyc_8080 = data_8080(:, vcyc_idx);
    pdhn_8080 = data_8080(:, pdhn_idx);
    pdha_8080 = data_8080(:, pdha_idx);
    pca_8080 = data_8080(:, pca_idx);    
    vcyc_100100 = data_100100(:, vcyc_idx);
    pdhn_100100 = data_100100(:, pdhn_idx);
    pdha_100100 = data_100100(:, pdha_idx);
    pca_100100 = data_100100(:, pca_idx);  
    
    vcyc_6044 = data_6044(:, vcyc_idx);
    pdhn_6044 = data_6044(:, pdhn_idx);
    pdha_6044 = data_6044(:, pdha_idx);
    pca_6044 = data_6044(:, pca_idx);    
    vcyc_6052 = data_6052(:, vcyc_idx);
    pdhn_6052 = data_6052(:, pdhn_idx);
    pdha_6052 = data_6052(:, pdha_idx);
    pca_6052 = data_6052(:, pca_idx); 
    vcyc_6068 = data_6068(:, vcyc_idx);
    pdhn_6068 = data_6068(:, pdhn_idx);
    pdha_6068 = data_6068(:, pdha_idx);
    pca_6068 = data_6068(:, pca_idx); 
    vcyc_6076 = data_6076(:, vcyc_idx);
    pdhn_6076 = data_6076(:, pdhn_idx);
    pdha_6076 = data_6076(:, pdha_idx);
    pca_6076 = data_6076(:, pca_idx);     
    
    [mvcyc_2020, mpdhn_2020, spdhn_2020] = compute_mean(vcyc_2020, pdhn_2020, bins);
    [~, mpdha_2020, spdha_2020] = compute_mean(vcyc_2020, pdha_2020, bins);
    [~, mpca_2020, spca_2020] = compute_mean(vcyc_2020, pca_2020, bins);
    [mvcyc_4040, mpdhn_4040, spdhn_4040] = compute_mean(vcyc_4040, pdhn_4040, bins);
    [~, mpdha_4040, spdha_4040] = compute_mean(vcyc_4040, pdha_4040, bins);
    [~, mpca_4040, spca_4040] = compute_mean(vcyc_4040, pca_4040, bins);
    [mvcyc_6060, mpdhn_6060, spdhn_6060] = compute_mean(vcyc_6060, pdhn_6060, bins);
    [~, mpdha_6060, spdha_6060] = compute_mean(vcyc_6060, pdha_6060, bins);
    [~, mpca_6060, spca_6060] = compute_mean(vcyc_6060, pca_6060, bins);
    [mvcyc_8080, mpdhn_8080, spdhn_8080] = compute_mean(vcyc_8080, pdhn_8080, bins);
    [~, mpdha_8080, spdha_8080] = compute_mean(vcyc_8080, pdha_8080, bins);
    [~, mpca_8080, spca_8080] = compute_mean(vcyc_8080, pca_8080, bins);
    [mvcyc_100100, mpdhn_100100, spdhn_100100] = compute_mean(vcyc_100100, pdhn_100100, bins);
    [~, mpdha_100100, spdha_100100] = compute_mean(vcyc_100100, pdha_100100, bins);
    [~, mpca_100100, spca_100100] = compute_mean(vcyc_100100, pca_100100, bins);
    
    [mvcyc_6044, mpdhn_6044, spdhn_6044] = compute_mean(vcyc_6044, pdhn_6044, bins);
    [~, mpdha_6044, spdha_6044] = compute_mean(vcyc_6044, pdha_6044, bins);
    [~, mpca_6044, spca_6044] = compute_mean(vcyc_6044, pca_6044, bins);
    [mvcyc_6052, mpdhn_6052, spdhn_6052] = compute_mean(vcyc_6052, pdhn_6052, bins);
    [~, mpdha_6052, spdha_6052] = compute_mean(vcyc_6052, pdha_6052, bins);
    [~, mpca_6052, spca_6052] = compute_mean(vcyc_6052, pca_6052, bins);
    [mvcyc_6068, mpdhn_6068, spdhn_6068] = compute_mean(vcyc_6068, pdhn_6068, bins);
    [~, mpdha_6068, spdha_6068] = compute_mean(vcyc_6068, pdha_6068, bins);
    [~, mpca_6068, spca_6068] = compute_mean(vcyc_6068, pca_6068, bins);
    [mvcyc_6076, mpdhn_6076, spdhn_6076] = compute_mean(vcyc_6076, pdhn_6076, bins);
    [~, mpdha_6076, spdha_6076] = compute_mean(vcyc_6076, pdha_6076, bins);
    [~, mpca_6076, spca_6076] = compute_mean(vcyc_6076, pca_6076, bins);    
    
    
    vcyc_025 = data_025(:, vcyc_idx);
    pca_025 = data_025(:, pca_idx);
    vcyc_050 = data_050(:, vcyc_idx);
    pca_050 = data_050(:, pca_idx);
    vcyc_100 = data_100(:, vcyc_idx);
    pca_100 = data_100(:, pca_idx);
    vcyc_200 = data_200(:, vcyc_idx);
    pca_200 = data_200(:, pca_idx);    
    
    [mvcyc_025, mpca_025, spca_025] = compute_mean(vcyc_025, pca_025, bins);
    [mvcyc_050, mpca_050, spca_050] = compute_mean(vcyc_050, pca_050, bins);
    [mvcyc_100, mpca_100, spca_100] = compute_mean(vcyc_100, pca_100, bins);
    [mvcyc_200, mpca_200, spca_200] = compute_mean(vcyc_200, pca_200, bins);
    [mvcyc_250, mpca_250, spca_250] = compute_mean(vcyc, pca, bins);
    
    % experimental (see Table 2 in the paper)
    % first point is to widen errorbar tick lenth
    em_vcyc = [-1,     0.01 0.15 0.40 0.04 0.31 0.22 0.51 0.02 0.58 0.27 0.49 0.16 0.28 0.12 0.40 0.12];
    em_pdhn = [0,       0.16 0.40 0.90 0.41 0.80 0.52 1.16 0.34 1.22 0.47 0.98 0.50 0.56 0.36 0.82 0.44];
    es_pdhn = [0,       0.10 0.08 0.28 0.05 0.16 0.04 0.17 0.12 0.04 0.04 0.06 0.05 0.07 0.04 0.08 0.01];
    em_pdha = [0,       nan, nan, nan, 0.28 nan, nan, 0.30 nan, nan, 0.14 nan, nan, nan, nan, nan, 0.23];
    es_pdha = [0,       nan, nan, nan, 0.07 nan, nan, 0.11 nan, nan, 0.03 nan, nan, nan, nan, nan, 0.02];
    em_pca =  [0,       nan, nan, nan, 0.04 nan, 0.06 0.18 nan, nan, nan, nan, nan, nan, 0.09 0.17 0.07];
    es_pca =  [0,       nan, nan, nan, 0.01 nan, 0.01 0.04 nan, nan, nan, nan, nan, nan, 0.01 0.02 0.004];
    
    ddx = 0.1; % extend xlim to display all experimental data
    eosize = 8;
    
    subplot(2,5,2);
    myshadedplot(mvcyc_2020, mpdhn_2020, spdhn_2020, plot_col(1,:)); hold on;
    myshadedplot(mvcyc_4040, mpdhn_4040, spdhn_4040, plot_col(2,:)); hold on;
    myshadedplot(mvcyc_6060, mpdhn_6060, spdhn_6060, plot_col(3,:)); hold on;
    myshadedplot(mvcyc_8080, mpdhn_8080, spdhn_8080, plot_col(4,:)); hold on;
    myshadedplot(mvcyc_100100, mpdhn_100100, spdhn_100100, plot_col(5,:)); hold on;    
    xlim([0-ddx,vcyc_0+ddx]);
    xlabel(xl); ylabel(['Neuronal PDH Rate',units]);
    legend({'20/20 (Na^+/K^+)', '40/40', '60/60', '80/80', '100/100'}, 'location','northwest');
    legend boxoff;
    panel('A');
    h=errorbar(em_vcyc, em_pdhn, es_pdhn, 'Marker', 's','MarkerSize',eosize,'MarkerEdgeColor','k', 'MarkerFaceColor','none','LineStyle','none','Color','k');
    %errorbar_tick(h, 0.03, 'units');
    ylim([0,2]);
    
   
    subplot(2,5,3);
    myshadedplot(mvcyc_6044, mpdha_6044, spdha_6044, plot_col(1,:)); hold on;
    myshadedplot(mvcyc_6052, mpdha_6052, spdha_6052, plot_col(2,:)); hold on;
    myshadedplot(mvcyc_6060, mpdha_6060, spdha_6060, plot_col(3,:)); hold on;
    myshadedplot(mvcyc_6068, mpdha_6068, spdha_6068, plot_col(4,:)); hold on;
    myshadedplot(mvcyc_6076, mpdha_6076, spdha_6076, plot_col(5,:)); hold on;    
    xlim([0-ddx,vcyc_0+ddx]);
    xlabel(xl); ylabel(['Astrocytic PDH Rate',units]);
    legend({'60/44 (Na^+/K^+)', '60/52', '60/60', '60/68', '60/76'}, 'location','northwest');
    legend boxoff;
    panel('B');    
    h=errorbar(em_vcyc, em_pdha, es_pdha, 'Marker', 's','MarkerSize',eosize,'MarkerEdgeColor','k', 'MarkerFaceColor','none','LineStyle','none','Color','k');
    %errorbar_tick(h, 0.03, 'units');    
    ylim([0,1]);
    
    
    subplot(2,5,4);
    myshadedplot(mvcyc_025, mpca_025, spca_025, plot_col(1,:)); hold on;
    myshadedplot(mvcyc_050, mpca_050, spca_050, plot_col(2,:)); hold on;
    myshadedplot(mvcyc_100, mpca_100, spca_100, plot_col(3,:)); hold on;
    myshadedplot(mvcyc_200, mpca_200, spca_200, plot_col(4,:)); hold on;   
    myshadedplot(mvcyc_250, mpca_250, spca_250, plot_col(5,:)); hold on;   
    xlim([0-ddx,vcyc_0+ddx]);
    xlabel(xl); ylabel(['Astrocytic PC Rate',units]);
    legend({'1:1 (nROS:aROS)', '1:2', '1:4', '1:8', '1:10'}, 'location','northwest');
    legend boxoff;
    panel('C',-0.03);    
    h=errorbar(em_vcyc, em_pca, es_pca, 'Marker', 's','MarkerSize',eosize,'MarkerEdgeColor','k', 'MarkerFaceColor','none','LineStyle','none','Color','k');
    %errorbar_tick(h, 0.03, 'units');    
    ylim([0,0.4]);    
    
end


% ------------------
if (~isempty(find(what==4)))

    figure('Color','w','Position',screensize);
    
    subplot(2,5,2); [cc, ~]=hist3([vcyc, jglcen], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(jglcen), max(jglcen), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Neuronal GLUT Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jglcen, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('A',-0.05);
    
    subplot(2,5,3); [cc, ~]=hist3([vcyc, jglcea], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(jglcea), max(jglcea), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Astrocytic GLUT Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jglcea, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('B',-0.05);
    
    colormap jet
    h = colorbar('eastoutside');
    hp = get(h, 'Position');
    ap = get(gca, 'Position');
    set(h, 'Position', [ap(1)+ap(3).*1.65, ap(2), hp(3)./2, ap(4)]);
    ylabel(h, 'Counts', 'FontSize', 12);
    set(h,'TickLength', 0.005);       

    subplot(2,5,7); [cc, ~]=hist3([vcyc, jlacne], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(jlacne), max(jlacne), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Neuronal MCT Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jlacne, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('D',-0.05);
         
    
    subplot(2,5,8); [cc, ~]=hist3([vcyc, jlacae], [bins3,bins3]); imagesc(linspace(min(vcyc), max(vcyc), bins3), linspace(min(jlacae), max(jlacae), bins3), cc');  climdb([0,bins/4]);
    xlabel(xl); ylabel(['Astrocytic MCT Rate',units]); set(gca,'YDir','normal'); hold on; plot(m_vcyc, m_jlacae, 'w', 'LineWidth', lwidth);
    %hold on; line([vcyc_0, vcyc_0], get(gca,'YLim'), 'Color', 'w','LineStyle',':', 'LineWidth', lwidth);
    xlim([0,vcyc_0]);
    panel('E',-0.05);


    subplot(2,10,8); 
    bar([1,2],f_glc,0.8,'FaceColor', [0 0 0]); 
    hold on;
    bar([3,4],f_lac,0.8,'FaceColor', [1 1 1]);
    set(gca,'XTick',[1,2,3,4]);
    set(gca,'XTickLabel',{'blood-to-neuron','blood-to-astrocyte','neuron-to-astrocyte','astrocyte-to-neuron'}); 
    yl=get(gca,'YLim');
    set(gca,'YLim',[yl(1),1.2*yl(2)]);
    xlabel('');
    ylabel('Counts');
    set(gca,'yaxislocation','right');
    xlim([0,5]);
    legend({'glucose', 'lactate'});
    legend boxoff;
    box off;
    rotateXLabels(gca,45);
    hp = get(gca,'Position'); 
    set(gca,'Position',[hp(1).*0.95,hp(2),hp(3),hp(4)]);
    panel('C',-0.05);
    
    

    subplot(2,5,9);
    data_1 = data(:, find(strcmp(labels,'J_202_en_GLUT')));
    data_2 = data(:, find(strcmp(labels,'J_201_be_GLUT')));
    data_3 = data_1./data_2;
    data_4 = data(:, find(strcmp(labels,'J_210_ne_MCT')));
    scatter(data_3, data_4, osize, 'MarkerEdgeColor', x_col);
    xlabel('J_{e{\rightarrow}n}GLUT/J_{b{\rightarrow}e}GLUT');    
    ylabel(['J_{n{\rightarrow}a}MCT',units]);
    box on;    
    xlim([0,1]);
    panel('F',-0.05);

end
    
%% --------------------------------------------------------------------------------------------
% subsample around awake

    %data(vcyc<vcyc_0-0.01, :) = nan;
    %data(jglcen<0.32, :) = nan;
    %data(jglcen>0.38, :) = nan;
    
    %data(isnan(data)) = [];



    
    
%% --------------------------------------------------------------------------------------------


if (~isempty(find(what==7)))
    
    s_alpha = 0.05;
    
    figure('Color','w','Position',screensize);
    
    subplot(2,12,1:3);
    data_1 = data(:, find(strcmp(labels,'J_624_n_ALAT'))); 
    data_2 = data(:, find(strcmp(labels,'J_626_n_BCAT'))); 
    data_3 = data(:, find(strcmp(labels,'J_623_n_GDH')));
    scatter3(data_1, data_2, data_3, osize, 'MarkerEdgeColor', n_col);
    xl = xlim; yl = ylim; zl = zlim;
    hold on; scatter3(data_1, data_2, 0.*data_3+zl(1), osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    hold on; scatter3(data_1, 0.*data_2+yl(2), data_3, osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    hold on; scatter3(0.*data_1+xl(2), data_2, data_3, osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    xlim(xl); ylim(yl); zlim(zl);
    xlabel(['J_{n}ALAT', 10, units(2:end)]);
    ylabel(['J_{n}BCAT', 10, units(2:end)]);
    zlabel(['J_{n}GDH', units]);
    box off;
    panel('A',0.05);
    
    subplot(2,12,5:7);
    data_1 = data(:, find(strcmp(labels,'J_724_a_ALAT'))); 
    data_2 = data(:, find(strcmp(labels,'J_726_a_BCAT'))); 
    data_3 = data(:, find(strcmp(labels,'J_723_a_GDH')));    
    scatter3(data_1, data_2, data_3, osize, 'MarkerEdgeColor', a_col);
    xl = xlim; yl = ylim; zl = zlim;
    hold on; scatter3(data_1, data_2, 0.*data_3+zl(1), osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    hold on; scatter3(data_1, 0.*data_2+yl(2), data_3, osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    hold on; scatter3(0.*data_1+xl(2), data_2, data_3, osize, 'Marker', s_marker, 'MarkerEdgeColor', p_col);
    xlim(xl); ylim(yl); zlim(zl);
    xlabel(['J_{a}ALAT', 10, units(2:end)]);
    ylabel(['J_{a}BCAT', 10, units(2:end)]);
    zlabel(['J_{a}GDH', units]);
    box off;
    panel('B',0.05);

    
    subplot(2,5,6);
    data_1 = data(:, find(strcmp(labels,'J_623_n_GDH')));
    data_2 = data(:, find(strcmp(labels,'J_723_a_GDH')));   
    scatter(vcyc, data_1, osize, 'Marker', s_marker, 'MarkerEdgeColor', n_col);
    hold on;
    scatter(vcyc, data_2, osize, 'Marker', s_marker, 'MarkerEdgeColor', a_col);
    xlabel(['J_{n}GDH', units]);    
    ylabel(['J_{a}GDH', units]);
    box on;
    set(gca, 'Layer', 'top')
    xlim([0,vcyc_0]);
    panel('C',-0.05);
    hp=get(gca,'Position'); set(gca,'Position',[hp(1),0.8.*hp(2),hp(3),hp(4)]);
    
    subplot(2,5,7);
    data_1 = data(:, find(strcmp(labels,'J_624_n_ALAT')));
    data_2 = data(:, find(strcmp(labels,'J_724_a_ALAT')));
    [P,S] = polyfit(vcyc,data_1,1);
    [Y,DELTA] = polyconf(P,vcyc,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    %yfit = P(1)*vcyc+P(2);
    myshadedplot2(vcyc,Y,DELTA,n_col);
    hold on;       
    scatter(vcyc, data_1, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);    
    hold on;
    [P,S] = polyfit(vcyc,data_2,1);
    [Y,DELTA] = polyconf(P,vcyc,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    %yfit = P(1)*vcyc+P(2);
    myshadedplot2(vcyc,Y,DELTA,a_col);
    hold on;       
    scatter(vcyc, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);    
    xlabel(['J_{n}ALAT', units]);    
    ylabel(['J_{a}ALAT', units]);
    box on;
    set(gca, 'Layer', 'top')
    xlim([0,vcyc_0]);
    panel('D',-0.05);
    hp=get(gca,'Position'); set(gca,'Position',[hp(1),0.8.*hp(2),hp(3),hp(4)]);
    
    h=legend({'neuron','linear regression','astrocyte','linear regression'},'fontsize',10,'location','northwest','orientation','horizontal');
    hp=get(h,'Position');
    %set(h,'Position',[hp(1),0.83*hp(2),hp(3),hp(4)]);
    set(h,'Position',[0.65.*hp(1),-0.01*hp(2),hp(3),hp(4)]);
    legend boxoff;      
    
    subplot(2,5,8);
    data_1 = data(:, find(strcmp(labels,'J_626_n_BCAT')));
    data_2 = data(:, find(strcmp(labels,'J_726_a_BCAT')));
    [P,S] = polyfit(vcyc,data_1,1);
    [Y,DELTA] = polyconf(P,vcyc,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    %yfit = P(1)*vcyc+P(2);
    myshadedplot2(vcyc,Y,DELTA,n_col);
    hold on;       
    scatter(vcyc, data_1, osize, 'Marker', s_marker, 'MarkerEdgeColor', n_col);   
    hold on;
    [P,S] = polyfit(vcyc,data_2,1);
    [Y,DELTA] = polyconf(P,vcyc,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    %yfit = P(1)*vcyc+P(2);
    myshadedplot2(vcyc,Y,DELTA,n_col);
    hold on;       
    scatter(vcyc, data_2, osize, 'Marker', s_marker, 'MarkerEdgeColor', a_col);    
    xlabel(['J_{n}BCAT', units]);    
    ylabel(['J_{a}BCAT', units]);
    box on;    
    set(gca, 'Layer', 'top')
    xlim([0,vcyc_0]);
    panel('E',-0.05);
    hp=get(gca,'Position'); set(gca,'Position',[hp(1),0.8.*hp(2),hp(3),hp(4)]);
    
end



if (~isempty(find(what==6)))    
    
    figure('Color','w','Position',screensize);
    
   
    s_alpha = 0.001;
    
    %---------------------------------------------------
    data = data_6044;
    
    subplot(2,5,2);
    data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    %data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));
    [data_1, ui, ~] = unique(data_1); data_2 = data_2(ui); data_4 = data_4(ui);
    [P,S] = polyfit(data_1,data_2,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit1_pn_m = P(1);
    fit1_pn_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    myshadedplot2(data_1,Y,DELTA,n_col);
    hold on;    
    scatter(data_1, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col); 
    hold on;
    [P,S] = polyfit(data_1,data_4,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit1_pa_m = P(1);
    fit1_pa_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    myshadedplot2(data_1,Y,DELTA,a_col);
    hold on;    
    scatter(data_1, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);  
    hold on;
    xlabel(['J_{n}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;    
    set(gca, 'Layer', 'top')
    panel('A',-0.03);
    xlim([min(data_1),max(data_1)]);
    ylim([0,1.5]);
    text(0.2, 1.4, '60/44 (Na^+/K^+)');
   

    
    %{
    subplot(2,5,7);
    %data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));    
    [P,S] = polyfit(data_3,data_2,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit2_pn_m = P(1);
    fit2_pn_s = max(DELTA)./(max(data_3)-min(data_3));    
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,n_col);
    hold on;
    scatter(data_3, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);
    hold on;
    [P,S] = polyfit(data_3,data_4,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit2_pa_m = P(1);
    fit2_pa_s = max(DELTA)./(max(data_3)-min(data_3));    
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,a_col);
    hold on;    
    scatter(data_3, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);
    hold on;
    xlabel(['J_{a}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;  
    set(gca, 'Layer', 'top')
    panel('F',-0.03);    
    xlim([min(data_3),max(data_3)]);
    ylim([0,1.5]);
    text(0.2, 1.4, '60/44 (Na^+/K^+)');
    %}
    
    %---------------------------------------------------
    data = data_6060;
    
    subplot(2,5,3);
    data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    %data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));    
    [data_1, ui, ~] = unique(data_1); data_2 = data_2(ui); data_4 = data_4(ui);
    [P,S] = polyfit(data_1,data_2,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit3_pn_m = P(1);
    fit3_pn_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    [~,hc] = myshadedplot2(data_1,Y,DELTA,n_col);
    set(get(get(hc, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off'); 
    hold on;    
    scatter(data_1, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);   
    hold on;
    [P,S] = polyfit(data_1,data_4,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit3_pa_m = P(1);
    fit3_pa_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    [~,hc] = myshadedplot2(data_1,Y,DELTA,a_col);
    set(get(get(hc, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off');     
    hold on;       
    scatter(data_1, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);   
    hold on;
    xlabel(['J_{n}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;    
    set(gca, 'Layer', 'top')
    panel('B',-0.03);
    xlim([min(data_1),max(data_1)]);
    ylim([0,1.5]);
    text(0.2, 1.4, '60/60 (Na^+/K^+)');
   
    %{
    subplot(2,5,8);
    %data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));    
    [P,S] = polyfit(data_3,data_2,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit4_pn_m = P(1);
    fit4_pn_s = max(DELTA)./(max(data_3)-min(data_3));    
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,n_col);
    hold on;       
    scatter(data_3, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);
    hold on;
    [P,S] = polyfit(data_3,data_4,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit4_pa_m = P(1);
    fit4_pa_s = max(DELTA)./(max(data_3)-min(data_3));       
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,a_col);
    hold on;      
    scatter(data_3, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);   
    hold on;
    xlabel(['J_{a}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;  
    set(gca, 'Layer', 'top')
    panel('G',-0.03);    
    xlim([min(data_3),max(data_3)]);
    ylim([0,1.5]);
    text(0.2, 1.4, '60/60 (Na^+/K^+)');
    %}

    h=legend({'neuron','astrocyte'},'fontsize',10,'location','northoutside','orientation','horizontal');
    hp=get(h,'Position');
    set(h,'Position',[0.99*hp(1),0.54*hp(2),hp(3),hp(4)]);
    %set(h,'Position',[0.75.*hp(1),0.01*hp(2),hp(3),hp(4)]);
    legend boxoff;   
        
    
    %---------------------------------------------------
    data = data_6076;
    
    subplot(2,5,4);
    data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    %data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));
    [data_1, ui, ~] = unique(data_1); data_2 = data_2(ui); data_4 = data_4(ui);
    
    % remove some points outside bounds
    where = (data_1>0.9);
    data_1(where) = [];
    data_2(where) = [];
    data_4(where) = [];
    
    [P,S] = polyfit(data_1,data_2,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit5_pn_m = P(1);
    fit5_pn_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    myshadedplot2(data_1,Y,DELTA,n_col);
    hold on;      
    scatter(data_1, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);  
    hold on;
    [P,S] = polyfit(data_1,data_4,1);
    [Y,DELTA] = polyconf(P,data_1,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit5_pa_m = P(1);
    fit5_pa_s = max(DELTA)./(max(data_1)-min(data_1));
    %yfit = P(1)*data_1+P(2);
    myshadedplot2(data_1,Y,DELTA,a_col);
    hold on;      
    scatter(data_1, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col);
    hold on;
    xlabel(['J_{n}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;    
    set(gca, 'Layer', 'top')
    panel('C',-0.03);
    xlim([min(data_1),max(data_1)]);
    ylim([0,1.5]);
    text(0.2, 1.4, '60/76 (Na^+/K^+)');
   
    %{
    subplot(2,5,9);
    %data_1 = data(:, find(strcmp(labels,'J_401_n_HK')));
    data_2 = data(:, find(strcmp(labels,'J_601_n_PDH')));
    data_3 = data(:, find(strcmp(labels,'J_501_a_HK')));
    data_4 = data(:, find(strcmp(labels,'J_701_a_PDH')));
    [P,S] = polyfit(data_3,data_2,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit6_pn_m = P(1);
    fit6_pn_s = max(DELTA)./(max(data_3)-min(data_3));       
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,n_col);
    hold on;      
    scatter(data_3, data_2, osize, 'Marker', s_marker, 'MarkerFaceColor', n_col, 'MarkerEdgeColor', n_col);
    hold on;
    [P,S] = polyfit(data_3,data_4,1);
    [Y,DELTA] = polyconf(P,data_3,S,'predopt','curve','alpha',s_alpha,'simopt','on');
    fit6_pa_m = P(1);
    fit6_pa_s = max(DELTA)./(max(data_3)-min(data_3));       
    %yfit = P(1)*data_3+P(2);
    myshadedplot2(data_3,Y,DELTA,a_col);
    hold on;      
    scatter(data_3, data_4, osize, 'Marker', s_marker, 'MarkerFaceColor', a_col, 'MarkerEdgeColor', a_col); 
    hold on;
    xlabel(['J_{a}HK',units]);    
    ylabel(['J_{x}PDH',units]);
    box on;  
    set(gca, 'Layer', 'top')
    panel('H',-0.03);    
    xlim([min(data_3),max(data_3)]);
    ylim([0,1.5]);   
    text(0.2, 1.4, '60/76 (Na^+/K^+)');
    %}
    
    % -------------------
    % -----
    
    xxn = 1:3;
    yyn = [fit1_pn_m, fit3_pn_m, fit5_pn_m];
    een = [fit1_pn_s, fit3_pn_s, fit5_pn_s];
    xxa = 1:3;
    yya = [fit1_pa_m, fit3_pa_m, fit5_pa_m];
    eea = [fit1_pa_s, fit3_pa_s, fit5_pa_s];
    
    subplot(4,5,5);
    bar(xxn, yyn, 'facecolor', n_col, 'edgecolor', n_col);
    hold on;
    errorbar([-15,xxn], [0,yyn], [0,een], 'color', n_col, 'linestyle', 'none'); 
    box off;
    ylabel(['Slope']);
    set(gca, 'XTickLabel', {'60/44 (Na^+/K^+)', '60/60 (Na^+/K^+)', '60/76 (Na^+/K^+)'});
    rotateXLabels(gca, 20);
    set(gca, 'Layer', 'top')  
    panel('D',-0.03,0.1); 
    hp=get(gca,'Position');
    set(gca,'Position',[1.01.*hp(1),1.048*hp(2),hp(3),0.75*hp(4)]);
    xlim([0,4]);
    
    subplot(4,5,10);
    bar(xxa, yya, 'facecolor', a_col, 'edgecolor', a_col);
    hold on;
    errorbar([-15,xxa], [0,yya], [0,eea], 'color', a_col, 'linestyle', 'none');  
    box off;
    ylabel(['Slope']);
    set(gca, 'XTickLabel', {'60/44 (Na^+/K^+)', '60/60 (Na^+/K^+)', '60/76 (Na^+/K^+)'});    
    rotateXLabels(gca, 20);
    set(gca, 'Layer', 'top')    
    panel('E',-0.03,0.1);
    hp=get(gca,'Position');
    set(gca,'Position',[1.01.*hp(1),1.067*hp(2),hp(3),0.75*hp(4)]);
    xlim([0,4]);

    % -----
    
    %{
    xxn = 1:3;
    yyn = [fit2_pn_m, fit4_pn_m, fit6_pn_m];
    een = [fit2_pn_s, fit4_pn_s, fit6_pn_s];
    xxa = 1:3;
    yya = [fit2_pa_m, fit4_pa_m, fit6_pa_m];
    eea = [fit2_pa_s, fit4_pa_s, fit6_pa_s];
    
    subplot(4,5,15);
    bar(xxn, yyn, 'facecolor', n_col, 'edgecolor', n_col);
    hold on;
    h=errorbar(xxn, yyn, een, 'color', n_col, 'linestyle', 'none'); 
    errorbar_tick(h,0.25,'units');
    box off;
    ylabel(['Slope']);
    set(gca, 'XTickLabel', {'60/44 (Na^+/K^+)', '60/60 (Na^+/K^+)', '60/76 (Na^+/K^+)'});
    rotateXLabels(gca, 20);
    set(gca, 'Layer', 'top')  
    panel('D',-0.03,0.1); 
    hp=get(gca,'Position');
    set(gca,'Position',[hp(1),1.048*hp(2),hp(3),0.75*hp(4)]);
    
    subplot(4,5,20);
    bar(xxa, yya, 'facecolor', a_col, 'edgecolor', a_col);
    hold on;
    h=errorbar(xxa, yya, eea, 'color', a_col, 'linestyle', 'none'); 
    errorbar_tick(h,0.25,'units');    
    box off;
    ylabel(['Slope']);
    set(gca, 'XTickLabel', {'60/44 (Na^+/K^+)', '60/60 (Na^+/K^+)', '60/76 (Na^+/K^+)'});    
    rotateXLabels(gca, 20);
    set(gca, 'Layer', 'top')    
    panel('E',-0.03,0.1);
    hp=get(gca,'Position');
    set(gca,'Position',[hp(1),1.067*hp(2),hp(3),0.75*hp(4)]);
    %}
    
    % --------
    
    %subplot(2,5,5);
    %hold on;
    %scatter(cmro2n, cmro2a, osize, 'MarkerFaceColor', [0,0,0], 'MarkerEdgeColor', [0,0,0]);
    %xlabel(['CMR_{O_2,n}',units]);    
    %ylabel(['CMR_{O_2,a}',units]);
    %box on;  
    %h=legend({'neuron','astrocyte'},'fontsize',10,'location','northwest');
    %hp=get(h,'Position');
    %set(h,'Position',[hp(1),0.83*hp(2),hp(3),hp(4)]);
    %legend boxoff;    
    %panel('D',-0.02);      
    
    %{
    subplot(2,5,9);
    data_1 = data(:, find(strcmp(labels,'J_412_nc_ME')));
    data_2 = data(:, find(strcmp(labels,'J_413_nm_ME')));    
    scatter(data_1, data_2, osize, 'MarkerEdgeColor', n_col);
    xlabel(['J_{x}cME', units]);    
    ylabel(['J_{x}mME', units]);
    box on;    
    
    subplot(2,5,10);
    data_1 = data(:, find(strcmp(labels,'J_513_am_ME')));
    data_2 = data(:, find(strcmp(labels,'J_514_a_PC')));
    scatter(data_1, data_2, osize, 'MarkerEdgeColor', a_col);
    xlabel(['J_{a}mME', units]);    
    ylabel(['J_{a}PC', units]);
    box on;    
    %}
    
end






return
end


function [mx, my, me] = compute_mean(x, y, bins)
mx = linspace(min(x), max(x), bins);
my = nan(1, bins);
me = nan(1, bins);
for i=1:bins
    if (i==1)
        yi = y(x<mx(2));
    else
        yi = y(x>=mx(i-1) & x<mx(i));
    end
    my(i) = mean(yi);
    me(i) = std(yi);
end
return
end


function annotate_distr(title,from,to,m,s,lo,up,u,m0,s0)
LabelFontSize = 10;
LegendFontSize = 9;
TextFontSize = 8;
set(gca,'FontSize',LegendFontSize); 
set(gca, 'XMinorTick', 'on');
set(gca, 'YMinorTick', 'on');
set(gca, 'ActivePositionProperty', 'position');
set(gca,'TickDir','out');
box off;
h = findobj(gca,'Type','patch');
%set(h,'FaceColor','w','EdgeColor','k');
set(h(1),'FaceColor','w','EdgeColor','r','facealpha',0.25,'edgealpha',0.75);
set(h(2),'FaceColor','w','EdgeColor','k','facealpha',1);
set(gca,'XLim',[from, to]);    
yl=get(gca,'YLim');
yl(2)=2.5*yl(2);
set(gca,'YLim',yl);
%pos=get(gca,'Position');
%pos(3)=1.04*pos(3);
%pos(4)=1.03*pos(4);
%set(gca,'Position',pos);
%xl=get(gca,'XLim');
%line(xl,[0,0],'Color','k');
xlabel('Rate ({\mu}mol/g/min)','FontSize',LabelFontSize);
ylabel('Counts','FontSize',LabelFontSize);
ntitle = add_space_to_labels(title, 0);
text(0.05,0.95,[ntitle{1}, ' {\in} [', num2str(lo,'%.3f'), ', ', num2str(up,'%.3f'), ']'],'Units', 'normalized','FontSize',TextFontSize,'Color',[0.5,0.5,0.5]); 
text(0.05,0.85,[ntitle{1}, ' = ', num2str(m,'%.3f'), ' \pm ', num2str(s,'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[0,0,0]); 
text(0.05,0.75,[ntitle{1}, ' = ', num2str(m0,'%.3f'), ' \pm ', num2str(s0,'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[1,0,0]); 
% opt
text(0.05,0.65,[ntitle{1}, '*(NT) = ', num2str(u(1),'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[0,0.5,0]); 
text(0.05,0.55,[ntitle{1}, '*(ATP) = ', num2str(u(2),'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[0,0.5,0]); 
text(0.05,0.45,[ntitle{1}, '*(Redox) = ', num2str(u(3),'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[0,0.5,0]); 
% points
hold on; plot(m, 0, 'o','linestyle','none','markersize',8,'markeredgecolor', [0,0,0],'markerfacecolor', [0,0,0]);
hold on; plot(m0, 0, 'o','linestyle','none','markersize',7,'markeredgecolor', [1,0,0],'markerfacecolor', [1,0,0]);
hold on; plot(u(1), 0, 'o','linestyle','none','markersize',4,'markeredgecolor', [0,0.5,0],'markerfacecolor', [0,0.5,0]);
hold on; plot(u(2), 0, 'o','linestyle','none','markersize',4,'markeredgecolor', [0,0.5,0],'markerfacecolor', [0,0.5,0]);
hold on; plot(u(3), 0, 'o','linestyle','none','markersize',4,'markeredgecolor', [0,0.5,0],'markerfacecolor', [0,0.5,0]);
set(gca, 'Layer', 'top');
end






function annotate_distr2(title,from,to,m,s,m0,s0,lo,up)
LabelFontSize = 12;
LegendFontSize = 10;
TextFontSize = 9;
set(gca,'FontSize',LegendFontSize); 
set(gca, 'XMinorTick', 'on');
set(gca, 'YMinorTick', 'on');
set(gca, 'ActivePositionProperty', 'position');
set(gca,'TickDir','out');
box off;
h = findobj(gca,'Type','patch');
set(h(1),'FaceColor','w','EdgeColor','r','facealpha',0.25,'edgealpha',0.75);
set(h(2),'FaceColor','w','EdgeColor','k','facealpha',1);
set(gca,'XLim',[from, to]);    
yl=get(gca,'YLim');
yl(2)=1.6*yl(2);
set(gca,'YLim',yl);
%pos=get(gca,'Position');
%pos(3)=1.04*pos(3);
%pos(4)=1.03*pos(4);
%set(gca,'Position',pos);
%xl=get(gca,'XLim');
%line(xl,[0,0],'Color','k');
xlabel('Rate ({\mu}mol/g/min)','FontSize',LabelFontSize);
ylabel('Counts','FontSize',LabelFontSize);
ntitle = add_space_to_labels(title, 0);
%text(0.10,0.9,[ntitle{1}, ' = ', num2str(m,'%.3f'), ' \pm ', num2str(s,'%.3f')],'Units', 'normalized','FontSize',TextFontSize); 
%text(0.10,0.8,[ntitle{1}, ' = ', num2str(m0,'%.3f'), ' \pm ', num2str(s0,'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color','r'); 
text(0.05,0.92,[ntitle{1}, ' {\in} [', num2str(lo,'%.3f'), ', ', num2str(up,'%.3f'), ']'],'Units', 'normalized','FontSize',TextFontSize,'Color',[0.5,0.5,0.5]); 
text(0.05,0.81,[ntitle{1}, ' = ', num2str(m,'%.3f'), ' \pm ', num2str(s,'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[0,0,0]); 
text(0.05,0.70,[ntitle{1}, ' = ', num2str(m0,'%.3f'), ' \pm ', num2str(s0,'%.3f')],'Units', 'normalized','FontSize',TextFontSize,'Color',[1,0,0]); 
hold on; plot(m, 0, 'o','linestyle','none','markersize',8,'markeredgecolor', [0,0,0],'markerfacecolor', [0,0,0]);
hold on; plot(m0, 0, 'o','linestyle','none','markersize',6,'markeredgecolor', [1,0,0],'markerfacecolor', [1,0,0]);
set(gca, 'Layer', 'top');
end





function annotate_domains(col)
if (nargin==0)
    col=[0.3 0.3 0.3];
    ww=6; % width
else
    ww=3; % width
end
global n labels lowerbounds upperbounds
set(gca,'XLim',[0 n+1]);
set(gca,'YLim',[-5, 15]);
ylimits=get(gca,'YLim');
d=0.5; % gap between lines and axis
dm=0.0001; % minimum height
for j=1:n
    if (upperbounds(j)-lowerbounds(j) < dm)
        hh=line([j-0.3,j+0.3],[lowerbounds(j),upperbounds(j)],'Color',col,'LineStyle','-','LineWidth',1.5);
    else
        hh=line('XData',[j,j],'YData',[lowerbounds(j),upperbounds(j)],'ZData',[0,0],'Color',col,'LineStyle','-','LineWidth',ww);
    end
    if (j>1)
       set(get(get(hh, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off');
    end
        %line('XData',[j,j],'YData',[ylimits(1)+d,lowerbounds(j)-d],'ZData',[0,0],'Color',[0.8 0.8 0.8],'LineStyle',':');
        %line('XData',[j,j],'YData',[upperbounds(j)+d,ylimits(2)-d],'ZData',[0,0],'Color',[0.8 0.8 0.8],'LineStyle',':');
end
grid on;
xlabel('Flux','FontSize',12);
ylabel('Rate ({\mu}mol/g/min)','FontSize',12);

nlabels = add_space_to_labels(labels, 0);

set(gca, 'XTick', 1:n);
set(gca, 'XTickLabel', nlabels, 'FontSize', 8);
set(gca,'TickDir','out');
set(gca,'TickLength',[0.002 0]);

rotateXLabels(gca,90);
box on;
set(gca, 'Layer', 'top');
return
end




function annotate_pearson
global n labels

shading flat;
%set(gca,'CLim',[-1 1]);
xlabel('Flux','FontSize',12);
h = ylabel('Pathway','FontSize',12);
hp = get(h, 'Position');
set(h,'Position',[-35, hp(2), hp(3)]);  % <<<<<<

colormap jet
ap = get(gca, 'Position');
h = colorbar('eastoutside');
hp = get(h, 'Position');
set(h, 'Position', [hp(1)+3.*hp(3), ap(2), hp(3)./3, ap(4)]);
ylabel(h, 'Pearson''s correlation coefficient', 'FontSize', 12);
set(h,'TickLength', 0.005);

set(gca,'XLim',[1, n+1]);
set(gca,'YLim',[1, n+1]);

nlabels = add_space_to_labels(labels, 0);

set(gca, 'XTick',1.5:1:n+0.5);
set(gca, 'XTickLabel', nlabels, 'FontSize', 8);
set(gca, 'YTick',1.5:1:n+0.5);
set(gca, 'YTickLabel', ''); %flip(nlabels));

set(gca,'TickDir','out');
set(gca,'TickLength',[0.002 0]);

rotateXLabels(gca,90);
box on;
set(gca, 'Layer', 'top')

sep = [12, 21, 23, 34, 36, 47, 50, 63, 67, 68, 73, 79, 92, 96, 97, 102, 108, 110, 114, 116, 120];
sep_label = {'Glutamatergic Activity', ...
    'Blood-Brain Glucose/O_2/CO_2 Exchanges    ', 'Intercellular Lactate Trafficking',...
    'Neuronal Glycolysis ', 'Neuronal Pyruvate Recycling  ',...
    'Astrocytic Glycolysis', 'Astrocytic Pyruvate Recycling + Carboxylation',...
    'Neuronal TCA Cycle    ', 'Neuronal Mitochondrial Shuttles',...
    'Neuronal Fatty Acids Synthesis','Neuronal NADH Shuttles    ', 'Neuronal Ammonia Homeostasis    ',...
    'Astrocytic TCA Cycle  ', 'Astrocytic Mitochondrial Shuttles',...
    'Astrocytic Fatty Acids Synthesis','Astrocytic NADH Shuttles ', 'Astrocytic Ammonia Homeostasis   ',... 
    'Neuronal Respiration + Housekeeping  ', 'Neuronal PPP + Antioxidant System  ',...
    'Astrocytic Respiration + Housekeeping', 'Astrocytic PPP + Antioxidant System'
    };

lw = 1;
for i=1:numel(sep)
    line([0, n+1], [2+n-sep(i), 2+n-sep(i)], 'Color', 'w', 'LineWidth', lw);
    line([sep(i), sep(i)], [0, n+1],  'Color', 'w', 'LineWidth', lw);    
end

hp = get(gca, 'position');
axes('position', [hp(1)-hp(3), hp(2), hp(3), hp(4)]);
set(gca, 'Color','none', 'Visible', 'off');
set(gca,'XLim',[1, n+1]);
set(gca,'YLim',[1, n+1]);

xs = 85;
xe0 = 119;
xe_disp = 1;
lw2 = 0.5;
for i=1:numel(sep)
    if (i==1)
        pos = 2+n-(sep(i)./2)-0.5;
        q = sep(i)-1;
    else
        pos = 2+n-(sep(i-1)+(sep(i)-sep(i-1))./2);
        q = sep(i)-sep(i-1);
    end
    text(xs, pos, sep_label{i}, 'FontSize', 8);
    xf = xs+0.62*length(sep_label{i});
    if (q==1)
        xe = xe0;
    else
        xe = xe0-xe_disp;
        delta = q./2-0.5;
        line([xe, xe], [pos-delta, pos+delta], 'LineWidth',lw2);
        line([xe, xe0], [pos-delta, pos-delta], 'LineWidth',lw2);
        line([xe, xe0], [pos+delta, pos+delta], 'LineWidth',lw2);
    end
    line([xf, xe], [pos, pos], 'LineWidth',lw2);
end


return
end

function l = add_space_to_labels(l, s)
for i=1:length(l)
    l{i}(1:6) = [];
    l{i} = strrep(l{i}, '_', '');
    l{i} = ['J', l{i}];
    l{i} = [l{i}, repmat(' ', 1, s)];
end
return
end


function panel(letter, dispx, dispy)
if (nargin<3)
    dispy = 0;
end
if (nargin<2)
    dispx = 0;
end
text(-0.25+dispx,1.05+dispy,letter,'Units','normalized','FontSize',18);
return
end


function [ha, hc] = myshadedplot2(qx, qm, qe, c)

x = linspace(nanmin(qx), nanmax(qx), 100);
m = interp1(qx, qm, x);
e = interp1(qx, qe, x);

y1 = m-e;
y2 = m+e;

% plot the shaded area
ha = fill([x, fliplr(x)], [y1, fliplr(y2)], 'k');

for i=1:length(ha)
    set(get(get(ha(i), 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off');  
    set(ha(i), 'LineStyle', 'none');
    set(ha(i), 'FaceAlpha', 0.25); 
    set(ha(i), 'FaceColor', c);
end

hold on;

% plot the curve
hc = plot(x, m, 'Color', c);


set(gca, 'Layer', 'top')

return
end


function ann11a(p,i,x,y,e)
global vcyc_0 units xl
    switch p
        case 1
            letter = {'A','B','C','D','E'};
            str = {'20/20 (Na^+/K^+)', '40/40 (Na^+/K^+)', '60/60 (Na^+/K^+)', '80/80 (Na^+/K^+)', '100/100 (Na^+/K^+)'};
            yl = 2;
            lab = 'Neuronal PDH Rate';
            if (i==3)
                col = [0.4,0.4,0.4];%[0,0,0];
            else
                col = [0.4,0.4,0.4];
            end
        case 2
            letter = {'F','G','H','I','J'};
            str = {'60/44 (Na^+/K^+)', '60/52 (Na^+/K^+)', '60/60 (Na^+/K^+)', '60/68 (Na^+/K^+)', '60/76 (Na^+/K^+)'};
            yl = 1;
            lab = 'Astrocytic PDH Rate';
            if (i==3)
                col = [0.4,0.4,0.4];%[0,0,0];
            else
                col = [0.4,0.4,0.4];
            end            
        case 3
            letter = {'K','L','M','N','O'};
            str = {'1:1 (nROS:aROS)', '1:2 (nROS:aROS)', '1:4 (nROS:aROS)', '1:8 (nROS:aROS)', '1:10 (nROS:aROS)'};
            yl = 0.4;
            lab = 'Astrocytic PC Rate';
            if (i==5)
                col = [0.4,0.4,0.4];%[0,0,0];
            else
                col = [0.4,0.4,0.4];
            end            
    end
    ddx_inf = 0.1; % extend xlim to display all experimental data
    ddx_sup = 0.09;
    eosize = 8;
    xlim([0-ddx_inf,vcyc_0+ddx_sup]);
    xlabel(xl); ylabel([lab,10,units]);

    panel(letter{i},-0.15);
    h=errorbar(x,y,e, 'Marker', 's','MarkerSize',eosize,'MarkerEdgeColor',col, 'MarkerFaceColor','none','LineStyle','none','Color',col);
    %errorbar_tick(h, 0.03, 'units');
    ylim([0,yl]);    
    
    legend({str{i}, 'experimental'}, 'location','northwest');
    legend boxoff;    
    
    %box off;
    return
end

function outrange = climdb(range)
% CLIMDB Get axis color limits or set to a specified dB range
%    RANGE = CLIMDB returns the color limits of the current axes.
%
%    CLIMDB(RANGE) sets the color limits of the current axis to the desired
%    range. RANGE can be a scalar, in which case the color limits will be
%    set to [MAX-RANGE, MAX]. Or RANGE can be a 2 element vector, which
%    specifies the color range directly. Since colorbars are not
%    automatically updated, it is best to call this function before
%    displaying a colorbar.

% return color limits if no inputs given
if nargin==0 && nargout<=1,
    outrange = get(gca,'clim');
    return
end

% determine clim if not fully specified
switch prod(size(range)),
case 1,
    clim = get(gca,'clim');
    clim(1) = clim(2)-range;
case 2,
    clim = range;
otherwise,
    error('RANGE must be a scalar or a 2 element vector');
end;

% set new color limits on current axis 
set(gca,'clim',clim);
end

function climmatch(figs)
% CLIMMATCH Match color limits on figures
%    CLIMDB(FIGS) matches the color limits on the specified figures. For
%    example, CLIMDB([1 2]) matches the color limits of figures 1 and 2.

axch = findobj(figs,'type','axes','tag','');
clims = get(axch,'clim');
if length(clims)==0, return; end;
clim = clims{1};
for i=2:length(clims),
    nlim = clims{i};
    clim = [min([clim(1) nlim(1)]) max([clim(2) nlim(2)])];
end;

set(axch,'clim',clim);
end

function hh = rotateXLabels( ax, angle, varargin )
%rotateXLabels: rotate any xticklabels
%
%   hh = rotateXLabels(ax,angle) rotates all XLabels on axes AX by an angle
%   ANGLE (in degrees). Handles to the resulting text objects are returned
%   in HH.
%
%   hh = rotateXLabels(ax,angle,param,value,...) also allows one or more
%   optional parameters to be specified. Possible parameters are:
%     'MaxStringLength'   The maximum length of label to show (default inf)
%
%   Examples:
%   >> bar( hsv(5)+0.05 )
%   >> days = {'Monday','Tuesday','Wednesday','Thursday','Friday'};
%   >> set( gca(), 'XTickLabel', days )
%   >> rotateXLabels( gca(), 45 )
%
%   See also: GCA, BAR

%   Copyright 2006-2013 The MathWorks Ltd.

error( nargchk( 2, inf, nargin ) );
if ~isnumeric( angle ) || ~isscalar( angle )
    error( 'RotateXLabels:BadAngle', 'Parameter ANGLE must be a scalar angle in degrees' )
end
angle = mod( angle, 360 );

% From R2014b, rotating labels is built-in using 'XTickLabelRotation'
if ~verLessThan('matlab','8.4.0')
    set(ax, 'XTickLabelRotation', angle)
    if nargout > 1
        hh = [];
    end
    return;
end

[maxStringLength] = parseInputs( varargin{:} );

% Get the existing label texts and clear them
[vals, labels] = findAndClearExistingLabels( ax, maxStringLength );

% Create the new label texts
h = createNewLabels( ax, vals, labels, angle );

% Reposition the axes itself to leave space for the new labels
repositionAxes( ax );

% If an X-label is present, move it too
repositionXLabel( ax );

% Store angle
setappdata( ax, 'RotateXLabelsAngle', angle );

% Only send outputs if requested
if nargout
    hh = h;
end

%-------------------------------------------------------------------------%
    function [maxStringLength] = parseInputs( varargin )
        % Parse optional inputs
        maxStringLength = inf;
        if nargin > 0
            params = varargin(1:2:end);
            values = varargin(2:2:end);
            if numel( params ) ~= numel( values )
                error( 'RotateXLabels:BadSyntax', 'Optional arguments must be specified as parameter-value pairs.' );
            end
            if any( ~cellfun( 'isclass', params, 'char' ) )
                error( 'RotateXLabels:BadSyntax', 'Optional argument names must be specified as strings.' );
            end
            for pp=1:numel( params )
                switch upper( params{pp} )
                    case 'MAXSTRINGLENGTH'
                        maxStringLength = values{pp};
                        
                    otherwise
                        error( 'RotateXLabels:BadParam', 'Optional parameter ''%s'' not recognised.', params{pp} );
                end
            end
        end
    end % parseInputs
%-------------------------------------------------------------------------%
    function [vals,labels] = findAndClearExistingLabels( ax, maxStringLength )
        % Get the current tick positions so that we can place our new labels
        vals = get( ax, 'XTick' );
        
        % Now determine the labels. We look first at for previously rotated labels
        % since if there are some the actual labels will be empty.
        ex = findall( ax, 'Tag', 'RotatedXTickLabel' );
        if isempty( ex )
            % Store the positions and labels
            labels = get( ax, 'XTickLabel' );
            if isempty( labels )
                % No labels!
                return
            else
                if ~iscell(labels)
                    labels = cellstr(labels);
                end
            end
            % Clear existing labels so that xlabel is in the right position
            set( ax, 'XTickLabel', {}, 'XTickMode', 'Manual' );
            setappdata( ax, 'OriginalXTickLabels', labels );
        else
            % Labels have already been rotated, so capture them
            labels = getappdata( ax, 'OriginalXTickLabels' );
            set(ex, 'DeleteFcn', []);
            delete(ex);
        end
        % Limit the length, if requested
        if isfinite( maxStringLength )
            for ll=1:numel( labels )
                if length( labels{ll} ) > maxStringLength
                    labels{ll} = labels{ll}(1:maxStringLength);
                end
            end
        end
        
    end % findAndClearExistingLabels
%-------------------------------------------------------------------------%
    function restoreDefaultLabels( ax )
        % Restore the default axis behavior
        removeListeners( ax );
        
        % Try to restore the tick marks and labels
        set( ax, 'XTickMode', 'auto', 'XTickLabelMode', 'auto' );
        rmappdata( ax, 'OriginalXTickLabels' );
        
        % Try to restore the axes position
        if isappdata( ax, 'OriginalAxesPosition' )
            set( ax, 'Position', getappdata( ax, 'OriginalAxesPosition' ) );
            rmappdata( ax, 'OriginalAxesPosition' );
        end
    end
%-------------------------------------------------------------------------%
    function textLabels = createNewLabels( ax, vals, labels, angle )
        % Work out the ticklabel positions
        zlim = get(ax,'ZLim');
        z = zlim(1);
        
        % We want to work in normalised coords, but this doesn't print
        % correctly. Instead we have to work in data units even though it
        % makes positioning hard.
        y = getYPositionToUse( ax );
        
        % Now create new text objects in similar positions.
        textLabels = -1*ones( numel( vals ), 1 );
        for ll=1:numel(vals)
            textLabels(ll) = text( ...
                'Units', 'Data', ...
                'Position', [vals(ll), y, z], ...
                'String', labels{ll}, ...
                'Parent', ax, ...
                'Clipping', 'off', ...
                'Rotation', angle, ...
                'Tag', 'RotatedXTickLabel', ...
                'UserData', vals(ll));
        end
        % So that we can respond to CLA and CLOSE, attach a delete
        % callback. We only attach it to one label to save massive numbers
        % of callbacks during axes shut-down.
        set(textLabels(end), 'DeleteFcn', @onTextLabelDeleted);
        
        % Now copy font properties into the texts
        updateFont();
        % Update the alignment of the text
        updateAlignment();
        
    end % createNewLabels

%-------------------------------------------------------------------------%
    function repositionAxes( ax )
        % Reposition the axes so that there's room for the labels
        % Note that we only do this if the OuterPosition is the thing being
        % controlled
        if ~strcmpi( get( ax, 'ActivePositionProperty' ), 'OuterPosition' )
            return;
        end
        
        % Work out the maximum height required for the labels
        labelHeight = getLabelHeight(ax);
        
        % Remove listeners while we mess around with things, otherwise we'll
        % trigger redraws recursively
        removeListeners( ax );
        
        % Change to normalized units for the position calculation
        oldUnits = get( ax, 'Units' );
        set( ax, 'Units', 'Normalized' );
        
        % Not sure why, but the extent seems to be proportional to the height of the axes.
        % Correct that now.
        set( ax, 'ActivePositionProperty', 'Position' );
        pos = get( ax, 'Position' );
        axesHeight = pos(4);
        % Make sure we don't adjust away the axes entirely!
        heightAdjust = min( (axesHeight*0.9), labelHeight*axesHeight );
        
        % Move the axes
        if isappdata( ax, 'OriginalAxesPosition' )
            pos = getappdata( ax, 'OriginalAxesPosition' );
        else
            pos = get(ax,'Position');
            setappdata( ax, 'OriginalAxesPosition', pos );
        end
        if strcmpi( get( ax, 'XAxisLocation' ), 'Bottom' )
            % Move it up and reduce the height
            set( ax, 'Position', pos+[0 heightAdjust 0 -heightAdjust] )
        else
            % Just reduce the height
            set( ax, 'Position', pos+[0 0 0 -heightAdjust] )
        end
        set( ax, 'Units', oldUnits );
        set( ax, 'ActivePositionProperty', 'OuterPosition' );
        
        % Make sure we find out if axes properties are changed
        addListeners( ax );
        
    end % repositionAxes

%-------------------------------------------------------------------------%
    function repositionXLabel( ax )
        % Try to work out where to put the xlabel
        removeListeners( ax );
        labelHeight = getLabelHeight(ax);
        
        % Use the new max extent to move the xlabel. We may also need to
        % move the title
        xlab = get(ax,'XLabel');
        titleh = get( ax, 'Title' );
        set( [xlab,titleh], 'Units', 'Normalized' );
        if strcmpi( get( ax, 'XAxisLocation' ), 'Top' )
            titleExtent = get( xlab, 'Extent' );
            set( xlab, 'Position', [0.5 1+labelHeight-titleExtent(4) 0] );
            set( titleh, 'Position', [0.5 1+labelHeight 0] );
        else
            set( xlab, 'Position', [0.5 -labelHeight 0] );
            set( titleh, 'Position', [0.5 1 0] );
        end
        addListeners( ax );
    end % repositionXLabel

%-------------------------------------------------------------------------%
    function height = getLabelHeight(ax)
        height = 0;
        textLabels = findall( ax, 'Tag', 'RotatedXTickLabel' );
        if isempty(textLabels)
            return;
        end
        oldUnits = get( textLabels(1), 'Units' );
        set( textLabels, 'Units', 'Normalized' );
        for ll=1:numel(vals)
            ext = get( textLabels(ll), 'Extent' );
            if ext(4) > height
                height = ext(4);
            end
        end
        set( textLabels, 'Units', oldUnits );
    end % getLabelExtent

%-------------------------------------------------------------------------%
    function updateFont()
        % Update the rotated text fonts when the axes font changes
        properties = {
            'FontName'
            'FontSize'
            'FontAngle'
            'FontWeight'
            'FontUnits'
            };
        propertyValues = get( ax, properties );
        textLabels = findall( ax, 'Tag', 'RotatedXTickLabel' );
        set( textLabels, properties, propertyValues );
    end % updateFont

    function updateAlignment()
        textLabels = findall( ax, 'Tag', 'RotatedXTickLabel' );
        angle = get( textLabels(1), 'Rotation' );
        % Depending on the angle, we may need to change the alignment. We change
        % alignments within 5 degrees of each 90 degree orientation.
        if strcmpi( get( ax, 'XAxisLocation' ), 'Top' )
            if 0 <= angle && angle < 5
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Bottom' );
            elseif 5 <= angle && angle < 85
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Bottom' );
            elseif 85 <= angle && angle < 95
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Middle' );
            elseif 95 <= angle && angle < 175
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Top' );
            elseif 175 <= angle && angle < 185
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Top' );
            elseif 185 <= angle && angle < 265
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Top' );
            elseif 265 <= angle && angle < 275
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Middle' );
            elseif 275 <= angle && angle < 355
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Bottom' );
            else % 355-360
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Bottom' );
            end
        else
            if 0 <= angle && angle < 5
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Top' );
            elseif 5 <= angle && angle < 85
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Top' );
            elseif 85 <= angle && angle < 95
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Middle' );
            elseif 95 <= angle && angle < 175
                set( textLabels, 'HorizontalAlignment', 'Right', 'VerticalAlignment', 'Bottom' );
            elseif 175 <= angle && angle < 185
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Bottom' );
            elseif 185 <= angle && angle < 265
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Bottom' );
            elseif 265 <= angle && angle < 275
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Middle' );
            elseif 275 <= angle && angle < 355
                set( textLabels, 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Top' );
            else % 355-360
                set( textLabels, 'HorizontalAlignment', 'Center', 'VerticalAlignment', 'Top' );
            end
        end
    end

%-------------------------------------------------------------------------%
    function onAxesFontChanged( ~, ~ )
        updateFont();
        repositionAxes( ax );
        repositionXLabel( ax );
    end % onAxesFontChanged

%-------------------------------------------------------------------------%
    function onAxesPositionChanged( ~, ~ )
        % We need to accept the new position, so remove the appdata before
        % redrawing
        if isappdata( ax, 'OriginalAxesPosition' )
            rmappdata( ax, 'OriginalAxesPosition' );
        end
        if isappdata( ax, 'OriginalXLabelPosition' )
            rmappdata( ax, 'OriginalXLabelPosition' );
        end
        repositionAxes( ax );
        repositionXLabel( ax );
    end % onAxesPositionChanged

%-------------------------------------------------------------------------%
    function onXAxisLocationChanged( ~, ~ )
        updateAlignment();
        repositionAxes( ax );
        repositionXLabel( ax );
    end % onXAxisLocationChanged

%-------------------------------------------------------------------------%
    function onAxesLimitsChanged( ~, ~ )
        % The limits have moved, so make sure the labels are still ok
        textLabels = findall( ax, 'Tag', 'RotatedXTickLabel' );
        xlim = get( ax, 'XLim' );
        pos = [0 getYPositionToUse( ax )];
        for tt=1:numel( textLabels )
            xval = get( textLabels(tt), 'UserData' );
            pos(1) = xval;
            % If the tick is off the edge, make it invisible
            if xval<xlim(1) || xval>xlim(2)
                set( textLabels(tt), 'Visible', 'off', 'Position', pos )
            elseif ~strcmpi( get( textLabels(tt), 'Visible' ), 'on' )
                set( textLabels(tt), 'Visible', 'on', 'Position', pos )
            else
                % Just set the position
                set( textLabels(tt), 'Position', pos );
            end
        end
        
        repositionXLabel( ax );
    end % onAxesPositionChanged

%-------------------------------------------------------------------------%
    function onTextLabelDeleted( ~, ~ )
        % The final text label has been deleted. This is likely from a
        % "cla" or "close" call, so we should remove all of our dirty
        % hacks.
        restoreDefaultLabels(ax);
    end

%-------------------------------------------------------------------------%
    function addListeners( ax )
        % Create listeners. We store the array of listeners in the axes to make
        % sure that they have the same life-span as the axes they are listening to.
        axh = handle( ax );
        listeners = [
            handle.listener( axh, findprop( axh, 'FontName' ), 'PropertyPostSet', @onAxesFontChanged )
            handle.listener( axh, findprop( axh, 'FontSize' ), 'PropertyPostSet', @onAxesFontChanged )
            handle.listener( axh, findprop( axh, 'FontWeight' ), 'PropertyPostSet', @onAxesFontChanged )
            handle.listener( axh, findprop( axh, 'FontAngle' ), 'PropertyPostSet', @onAxesFontChanged )
            handle.listener( axh, findprop( axh, 'FontUnits' ), 'PropertyPostSet', @onAxesFontChanged )
            handle.listener( axh, findprop( axh, 'OuterPosition' ), 'PropertyPostSet', @onAxesPositionChanged )
            handle.listener( axh, findprop( axh, 'XLim' ), 'PropertyPostSet', @onAxesLimitsChanged )
            handle.listener( axh, findprop( axh, 'YLim' ), 'PropertyPostSet', @onAxesLimitsChanged )
            handle.listener( axh, findprop( axh, 'XAxisLocation' ), 'PropertyPostSet', @onXAxisLocationChanged )
            ];
        setappdata( ax, 'RotateXLabelsListeners', listeners );
    end % addListeners

%-------------------------------------------------------------------------%
    function removeListeners( ax )
        % Rempove any property listeners whilst we are fiddling with the axes
        if isappdata( ax, 'RotateXLabelsListeners' )
            delete( getappdata( ax, 'RotateXLabelsListeners' ) );
            rmappdata( ax, 'RotateXLabelsListeners' );
        end
    end % removeListeners

%-------------------------------------------------------------------------%
    function y = getYPositionToUse( ax )
        % Use the direction and XAxisLocation properties to work out which
        % Y-value to draw the labels at.
        whichYLim = 1;
        % If YDir is reversed, switch where we position
        if strcmpi( get( ax, 'YDir' ), 'Reverse' )
            whichYLim = 3-whichYLim;
        end
        % If set to "top", then switch (again?)
        if strcmpi( get( ax, 'XAxisLocation' ), 'Top' )
            whichYLim = 3-whichYLim;
        end
        % Now get the value
        ylim = get( ax, 'YLim' );
        y = ylim(whichYLim);
    end % getYPositionToUse

end % EOF

function s = spectral(m)
%SPECTRAL Black-purple-blue-green-yellow-red-white color map.
%
%         map = spectral(num_colors)
%
% SPECTRAL(M) returns an M-by-3 matrix containing a "spectral" colormap.
% SPECTRAL, by itself, is the same length as the current colormap.
%
% For example, to reset the colormap of the current figure:
%
%           colormap(spectral)
%
% See also HSV, GRAY, PINK, HOT, COOL, BONE, COPPER, FLAG,
%          COLORMAP, RGBPLOT.

if nargin < 1, m = size(get(gcf,'colormap'),1); end
base = [
  0.0000 0.0000 0.0000
  0.4667 0.0000 0.5333
  0.5333 0.0000 0.6000
  0.0000 0.0000 0.6667
  0.0000 0.0000 0.8667
  0.0000 0.4667 0.8667
  0.0000 0.6000 0.8667
  0.0000 0.6667 0.6667
  0.0000 0.6667 0.5333
  0.0000 0.6000 0.0000
  0.0000 0.7333 0.0000
  0.0000 0.8667 0.0000
  0.0000 1.0000 0.0000
  0.7333 1.0000 0.0000
  0.9333 0.9333 0.0000
  1.0000 0.8000 0.0000
  1.0000 0.6000 0.0000
  1.0000 0.0000 0.0000
  0.8667 0.0000 0.0000
  0.8000 0.0000 0.0000
  0.8000 0.8000 0.8000
];
n = length(base);
X0 = linspace (1, n, m);
s = interp1(1:n,base,X0);

return
end

