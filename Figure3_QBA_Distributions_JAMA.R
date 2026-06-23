suppressMessages({library(ggplot2); library(dplyr); library(patchwork)})
FONT<-"sans"
jama_navy<-"#003366"; jama_red<-"#c0392b"; jama_gray<-"#5d6d7e"; jama_light<-"#dfe4e8"

simulate_qba<-function(log_estimate,log_se,prior_median=1.40,prior_sd_log=0.12,n_sim=50000,seed=42){
  set.seed(seed)
  log_obs<-rnorm(n_sim,log_estimate,log_se); observed<-exp(log_obs)
  log_bias<-rnorm(n_sim,log(prior_median),prior_sd_log)
  adjusted<-exp(log_obs-log_bias)
  list(observed=observed,adjusted=adjusted,
       prob=mean(adjusted>1)*100, median_adj=median(adjusted),
       si_lower=quantile(adjusted,.025), si_upper=quantile(adjusted,.975),
       obs_pt=exp(log_estimate))
}

qba_panel<-function(s,title,subt,eff="HR",xr=c(0.3,2.6)){
  df<-data.frame(value=c(s$observed,s$adjusted),
                 type=rep(c("Observed","Adjusted"),each=length(s$observed)))
  # densidades com o MESMO bandwidth do geom_density (adjust=1.5), p/ alinhar sombra e calcular headroom
  da<-density(s$adjusted,n=512,from=xr[1],to=xr[2],adjust=1.5); dfa<-data.frame(x=da$x,y=da$y)
  do<-density(s$observed,n=512,from=xr[1],to=xr[2],adjust=1.5)
  shade<-dfa[dfa$x>=1,]
  ymax<-max(c(do$y,da$y)); top<-ymax*1.33
  ptxt<-sprintf("P(adjusted %s >1.0) = %.1f%%",eff,s$prob)
  mtxt<-sprintf("Median adjusted: %.2f\n95%% SI: %.2f\u2013%.2f",s$median_adj,s$si_lower,s$si_upper)
  ggplot(df,aes(value,fill=type,color=type))+
    geom_area(data=shade,aes(x,y),inherit.aes=FALSE,fill=jama_red,alpha=0.13)+
    geom_density(alpha=0.30,linewidth=0.8,adjust=1.5)+
    geom_vline(xintercept=1,linetype="dashed",color="black",linewidth=0.5)+
    geom_vline(xintercept=s$obs_pt,linetype="dotted",color=jama_navy,linewidth=0.6)+
    scale_fill_manual(values=c(Observed=jama_navy,Adjusted=jama_red))+
    scale_color_manual(values=c(Observed=jama_navy,Adjusted=jama_red))+
    scale_x_continuous(breaks=seq(0.5,5,0.5))+
    scale_y_continuous(expand=expansion(mult=c(0,0)))+
    coord_cartesian(xlim=xr, ylim=c(0,top))+
    annotate("text",x=xr[2]*0.985,y=ymax*1.25,label=ptxt,hjust=1,vjust=1,size=3.15,
             fontface="bold",family=FONT,color=jama_red)+
    annotate("text",x=xr[2]*0.985,y=ymax*1.10,label=mtxt,hjust=1,vjust=1,size=2.6,
             family=FONT,color=jama_gray,lineheight=0.95)+
    labs(title=title,subtitle=subt,x=eff,y="Density")+
    theme_minimal(base_size=10)+
    theme(text=element_text(family=FONT),
          plot.title=element_text(face="bold",size=11,hjust=0,margin=margin(b=2)),
          plot.subtitle=element_text(size=8.5,color=jama_gray,margin=margin(b=8)),
          axis.title.x=element_text(size=9,margin=margin(t=5)),
          axis.title.y=element_text(size=9,margin=margin(r=5)),
          axis.text=element_text(size=8,color="black"),
          axis.line=element_line(color="black",linewidth=0.3),
          axis.ticks=element_line(color="black",linewidth=0.25),
          panel.grid=element_blank(),
          legend.position="none",
          plot.margin=margin(t=5,r=12,b=5,l=10))
}

sh<-simulate_qba(log(1.34),(log(1.46)-log(1.22))/(2*1.96),seed=42)
shy<-simulate_qba(log(1.17),(log(1.28)-log(1.07))/(2*1.96),seed=43)
st<-simulate_qba(log(1.78),(log(2.75)-log(1.16))/(2*1.96),seed=44)

pa<-qba_panel(sh,"A  Hypothyroidism","6 reports; pooled HR, 1.34 (95% CI, 1.22-1.46)","HR",c(0.3,2.6))
pb<-qba_panel(shy,"B  Hyperthyroidism","4 reports; pooled HR, 1.17 (95% CI, 1.07-1.28)","HR",c(0.3,2.6))
pc<-qba_panel(st,"C  TPOAb positivity (Brown 2015)","1 report; OR, 1.78 (95% CI, 1.16-2.75)","OR",c(0.4,4.5))

legend_plot<-ggplot(data.frame(x=1:2,type=factor(c("Observed","Adjusted"),levels=c("Observed","Adjusted"))),
                    aes(x,y=0,fill=type))+geom_col(width=0)+
  scale_fill_manual(values=c(Observed=jama_navy,Adjusted=jama_red),
    labels=c(Observed="Observed distribution",Adjusted="Adjusted for familial confounding"),name=NULL)+
  theme_void()+theme(legend.position="bottom",legend.direction="horizontal",
    legend.text=element_text(size=9,family=FONT),legend.key.size=unit(0.4,"cm"))+
  guides(fill=guide_legend(override.aes=list(alpha=0.5)))

fig3<-(pa | pb | pc)/legend_plot + plot_layout(heights=c(1,0.12))+
  plot_annotation(caption=paste0(
    "Distributions of observed (navy) and bias-adjusted (red) effect estimates for the principal exposure domains. Adjustment used a log-normal prior for the\n",
    "familial-confounding bias factor (median 1.40; 95% prior interval 1.11\u20131.77), from cross-condition sibling-comparison attenuation (Khachadourian et al, 2025).\n",
    "The dashed line marks the null (1.0); the dotted navy line marks the observed pooled estimate; shading is the adjusted mass above the null. 50 000 iterations per domain."),
    theme=theme(plot.caption=element_text(size=7.5,color=jama_gray,hjust=0,family=FONT,lineheight=1.3)))

ggsave("Figure3_QBA_Distributions_JAMA.png",fig3,width=13.6,height=5.6,dpi=600,bg="white")  # 600 dpi raster (JAMA min. 350)
ggsave("Figure3_QBA_Distributions_JAMA.pdf",fig3,width=13.6,height=5.6,device=cairo_pdf,bg="white")  # vector: preferred
cat("fig3 ok\n")
