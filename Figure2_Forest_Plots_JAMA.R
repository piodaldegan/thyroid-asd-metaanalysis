# Figure 2 — forest plots (maternal hypothyroidism / hyperthyroidism vs offspring ASD)
# Changes vs the previous version of this script:
#   - panel A subtitle: tau^2 = 0.005 (was 0.00; rounded display of DL estimate 0.0047)
#   - output filenames corrected to "Figure2_*" (were "Figure1_*")
#   - internal object renamed fig2 (was fig1) for consistency with manuscript numbering

suppressMessages({library(ggplot2); library(patchwork); library(grid)})
FONT<-"sans"
navy<-"#003366"; gray<-"#5d6d7e"; ink<-"#1f2a36"; bdr<-"#c4ced6"

# eixo log compartilhado + posições das colunas (em coords de dado, log)
XLIM<-c(0.5,5.3); BREAKS<-c(0.5,1,2,3,5); LAB_X<-0.47
HR_X<-8.4; WT_X<-17.5

forest <- function(d, title, subt, show_xtitle=FALSE){
  n<-nrow(d); d$y<-n - seq_len(n) + 1          # 1ª linha (topo) ... pooled por último (base y=1)
  ind<-d[!d$is_pooled,]; pl<-d[d$is_pooled,]
  mw<-max(ind$weight,na.rm=TRUE); ind$sq<-2.6 + (ind$weight/mw)*3.4
  # diamante do pooled
  dh<-0.30
  diam<-data.frame(x=c(pl$lo,pl$est,pl$hi,pl$est), y=c(pl$y,pl$y+dh,pl$y,pl$y-dh))
  d$hr<-sprintf("%.2f (%.2f-%.2f)",d$est,d$lo,d$hi)
  d$wt<-ifelse(d$is_pooled,"",sprintf("%.1f",d$weight))

  ggplot()+
    geom_vline(xintercept=1,linetype="dashed",color=gray,linewidth=0.5)+
    # separador acima do pooled
    annotate("segment",x=XLIM[1],xend=WT_X*1.15,y=1.5,yend=1.5,color=bdr,linewidth=0.4)+
    # CIs + quadrados (estudos)
    geom_segment(data=ind,aes(x=lo,xend=hi,y=y,yend=y),linewidth=0.6,color=navy)+
    geom_point(data=ind,aes(x=est,y=y,size=sq),shape=15,color=navy)+ scale_size_identity()+
    # diamante (pooled)
    geom_polygon(data=diam,aes(x=x,y=y),fill=navy,color=navy)+
    # rótulos dos estudos (margem esquerda)
    geom_text(data=d,aes(x=LAB_X,y=y,label=study),hjust=1,family=FONT,size=3.15,color=ink)+
    # colunas à direita
    geom_text(data=d,aes(x=HR_X,y=y,label=hr),hjust=0.5,family=FONT,size=3.1,color=ink)+
    geom_text(data=d,aes(x=WT_X,y=y,label=wt),hjust=0.5,family=FONT,size=3.1,color=ink)+
    # cabeçalhos das colunas
    annotate("text",x=HR_X,y=n+1.15,label="HR (95% CI)",hjust=0.5,fontface="bold",family=FONT,size=3.1,color=ink)+
    annotate("text",x=WT_X,y=n+1.15,label="Weight, %",hjust=0.5,fontface="bold",family=FONT,size=3.1,color=ink)+
    # favors
    annotate("text",x=0.82,y=0.12,label="\u2190 Favours no association",hjust=1,family=FONT,size=2.7,color=gray)+
    annotate("text",x=1.22,y=0.12,label="Favours association \u2192",hjust=0,family=FONT,size=2.7,color=gray)+
    scale_x_log10(breaks=BREAKS,labels=sprintf("%.1f",BREAKS))+
    coord_cartesian(xlim=XLIM, ylim=c(-0.25,n+1.7), clip="off")+
    labs(title=title, subtitle=subt, x=if(show_xtitle)"Hazard Ratio (95% CI, log scale)" else NULL, y=NULL)+
    theme_minimal(base_size=11)+
    theme(text=element_text(family=FONT,color=ink),
          plot.title=element_text(face="bold",size=13,hjust=0,margin=margin(b=3)),
          plot.subtitle=element_text(size=9.5,color=gray,margin=margin(b=10)),
          axis.title.x=element_text(size=10,margin=margin(t=8)),
          axis.text.x=element_text(size=9.5,color=ink),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          axis.line.x=element_line(color=ink,linewidth=0.4),
          axis.ticks.x=element_line(color=ink,linewidth=0.3),
          axis.ticks.length.x=unit(0.14,"cm"),
          axis.ticks.y=element_blank(),
          panel.grid=element_blank(),
          plot.margin=margin(t=10,r=210,b=8,l=158))
}

hypo<-data.frame(
  study=c("Andersen et al, 2014","Andersen et al, 2018","Getahun et al, 2018",
          "Rotem et al, 2020","Wu et al, 2025","Elbedour et al, 2025","Pooled (random-effects)"),
  est=c(1.34,1.75,1.31,1.23,1.34,2.61,1.34),
  lo =c(1.14,1.12,1.13,1.10,1.19,1.44,1.22),
  hi =c(1.59,2.73,1.53,1.37,1.51,4.74,1.46),
  # CORRIGIDO: pesos de efeitos aleatorios DerSimonian-Laird (tau2 = 0.0047), derivados do modelo
  weight=c(18.6,3.9,20.7,28.2,26.3,2.3,NA),
  is_pooled=c(F,F,F,F,F,F,T), stringsAsFactors=FALSE)
hyper<-data.frame(
  study=c("Andersen et al, 2014","Andersen et al, 2018","Rotem et al, 2020",
          "Wu et al, 2025","Pooled (random-effects)"),
  est=c(1.18,1.16,1.44,1.14,1.17),
  lo =c(0.96,0.62,1.06,1.03,1.07),
  hi =c(1.45,2.17,1.95,1.27,1.28),
  # CORRIGIDO: com tau2 = 0 / I2 = 0%, os pesos = inverso da variancia (efeito fixo)
  weight=c(18.4,2.0,8.4,71.2,NA),
  is_pooled=c(F,F,F,F,T), stringsAsFactors=FALSE)

# ---- BLOCO DE VERIFICACAO (nao altera a figura; so confirma no console) ----
# Confirma que os pesos exibidos vem do mesmo modelo que gera o HR/tau2/I2 do cabecalho.
check_weights <- function(d, method=c("DL","FE")){
  method<-match.arg(method)
  s<-d[!d$is_pooled,]
  yi<-log(s$est); sei<-(log(s$hi)-log(s$lo))/(2*qnorm(.975)); vi<-sei^2
  wf<-1/vi; muF<-sum(wf*yi)/sum(wf); Q<-sum(wf*(yi-muF)^2); df<-length(yi)-1
  C<-sum(wf)-sum(wf^2)/sum(wf); tau2<-if(method=="DL") max(0,(Q-df)/C) else 0
  w<-1/(vi+tau2); mu<-sum(w*yi)/sum(w); se<-sqrt(1/sum(w))
  data.frame(study=s$study,
             peso_modelo=round(100*w/sum(w),1),
             peso_figura=s$weight)|>print()
  cat(sprintf("  -> HR combinado=%.3f (%.2f-%.2f); I2=%.0f%%; tau2=%.4f; Q=%.2f (df=%d)\n\n",
              exp(mu),exp(mu-qnorm(.975)*se),exp(mu+qnorm(.975)*se),
              max(0,(Q-df)/Q)*100, tau2, Q, df))
}
cat("Painel A (hipotireoidismo, DerSimonian-Laird):\n"); check_weights(hypo,"DL")
cat("Painel B (hipertireoidismo, tau2=0 -> inverso da variancia):\n"); check_weights(hyper,"FE")
# ---------------------------------------------------------------------------

sa<-expression(paste("6 reports; pooled HR, 1.34 (95% CI, 1.22-1.46);  ",I^2," = 39%;  ",tau^2," = 0.005;  Q = 8.24 (df = 5);  ",italic(P)," = .14"))
sb<-expression(paste("4 reports; pooled HR, 1.17 (95% CI, 1.07-1.28);  ",I^2," = 0%;  ",tau^2," = 0.00;  Q = 2.03 (df = 3);  ",italic(P)," = .57"))

pA<-forest(hypo,"A  Maternal hypothyroidism",sa,FALSE)
pB<-forest(hyper,"B  Maternal hyperthyroidism",sb,TRUE)

fig2<-pA/pB + plot_layout(heights=c(1.25,1)) +
  plot_annotation(caption=paste0(
    "Squares are individual study estimates sized by inverse-variance weight; horizontal lines are 95% CIs.\n",
    "The diamond is the random-effects pooled estimate (its width is the 95% CI).\n",
    "Both panels share a common logarithmic x-axis; the dashed line marks the null (HR = 1.0).\n",
    "Andersen et al, 2018 used laboratory-diagnosed manifest dysfunction with week-specific reference intervals."),
    theme=theme(plot.caption=element_text(size=7.6,color=gray,hjust=0,lineheight=1.3,family=FONT,margin=margin(t=8))))

ggsave("Figure2_Forest_Plots_JAMA.png", fig2, width=9.6, height=6.6, dpi=600, bg="white")  # 600 dpi raster (JAMA min. 350)
ggsave("Figure2_Forest_Plots_JAMA.pdf", fig2, width=9.6, height=6.6, device=cairo_pdf, bg="white")  # vector: preferred
cat("fig2 ok\n")
