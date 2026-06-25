suppressMessages({library(ggplot2); library(dplyr); library(grid)})
FONT <- "sans"

# paleta de risco (robvis-like, refinada)
col_lvl <- c(Low="#1a9850","Some concerns"="#f0c000",High="#e8763a","Very high"="#8c1515")
gly_lvl <- c(Low="+","Some concerns"="\u2212",High="\u2715","Very high"="!")
gcol_lvl<- c(Low="white","Some concerns"="#4a3b00",High="white","Very high"="white")
slate<-"#5d6d7e"; ink<-"#1f2a36"; bdr<-"#c4ced6"

# ---- matriz final (ROBINS-E) -----------------------------------------------
L<-"Low"; S<-"Some concerns"; H<-"High"; V<-"Very high"
robe <- tibble::tribble(
 ~study,~grp, ~D1,~D2,~D3,~D4,~D5,~D6,~D7,~Ov,
 "Andersen et al, 2014","A", S,L,L,L,L,L,S,S,
 "Andersen et al, 2018","A", S,L,S,L,L,L,L,S,
 "Getahun et al, 2018","A",  S,L,S,S,L,L,S,S,
 "Rotem et al, 2020","A",    S,S,S,S,S,L,S,S,
 "Wu et al, 2025","A",       S,S,L,S,L,L,S,S,
 "Elbedour et al, 2025","A", S,L,S,S,S,S,L,S,
 "Brown et al, 2015","B",    S,L,L,L,L,L,S,S,
 "Yau et al, 2015","C",      S,S,S,L,S,L,L,S,
 "M\u00f8llehave et al, 2025","C",S,L,H,L,S,L,L,H,
 "Cromie et al, 2020","D",   S,S,L,S,S,L,S,S,
 "Ge et al, 2022","E",       S,L,S,S,L,L,S,S,
 "George et al, 2014","F",   H,H,H,S,S,H,H,H,
 "Qutranji et al, 2021","F", H,H,H,S,S,S,H,H,
 "Hankus et al, 2020","F",V,V,H,S,S,L,S,V
)
grp_lab <- c(A="A  Clinical thyroid dysfunction (pooled domains)",
             B="B  Thyroid autoimmunity (TPOAb)",
             C="C  Biomarker-defined dysfunction (TSH / FT4)",
             D="D  Iodine status",
             E="E  Thyroid-related treatment",
             F="F  Excluded from synthesis \u2014 high / very high risk (case-control)")
dom_lab <- c(D1="Confounding",D2="Exposure measurement",D3="Participant selection",
             D4="Post-exposure interventions",D5="Missing data",D6="Outcome measurement",
             D7="Reported result",Ov="Overall")
dom_codes <- names(dom_lab)

# ---- layout vertical: sequência de linhas (cabeçalho de grupo + estudos) ----
seqr <- list(); ord<-0; rows<-list(); bands<-list()
for(g in names(grp_lab)){
  ord<-ord+1; seqr[[length(seqr)+1]]<-list(type="gh",grp=g,ord=ord)
  studs<-robe$study[robe$grp==g]
  y0<-ord
  for(s in studs){ ord<-ord+1; seqr[[length(seqr)+1]]<-list(type="st",study=s,ord=ord) }
  bands[[g]]<-c(top=y0+0.5, bot=ord+0.5)  # faixa atrás dos estudos do grupo
}
N<-ord
yof <- function(o) N-o+1   # converte ordem (1=topo) em y (maior=topo)

st_ord <- sapply(seqr, function(e) if(e$type=="st") e$ord else NA)
st_nm  <- sapply(seqr, function(e) if(e$type=="st") e$study else NA)
study_y <- setNames(yof(st_ord[!is.na(st_ord)]), st_nm[!is.na(st_nm)])
gh <- Filter(function(e) e$type=="gh", seqr)

# ---- posições de coluna ----------------------------------------------------
xcol <- c(D1=46,D2=53,D3=60,D4=67,D5=74,D6=81,D7=88, Ov=97)
labx <- 1.0; lab_right <- 44.5

# ---- células ---------------------------------------------------------------
cells <- do.call(rbind, lapply(seq_len(nrow(robe)), function(i){
  data.frame(study=robe$study[i],
             code=dom_codes,
             x=as.numeric(xcol[dom_codes]),
             y=study_y[[robe$study[i]]],
             lvl=as.character(robe[i, dom_codes]),
             stringsAsFactors=FALSE)
}))
cells$fill<-col_lvl[cells$lvl]; cells$gly<-gly_lvl[cells$lvl]; cells$gcol<-gcol_lvl[cells$lvl]
cells$ov <- cells$code=="Ov"

# zebra por estudo
zeb <- data.frame(y=unname(study_y))
zeb$fill<-ifelse(seq_len(nrow(zeb))%%2==0,"#f3f5f7","white")

# bandas e cabeçalhos de grupo
band_df<-do.call(rbind,lapply(names(bands),function(g) data.frame(grp=g,
            ymin=yof(bands[[g]]["bot"]), ymax=yof(bands[[g]]["top"]))))
gh_df<-do.call(rbind,lapply(gh,function(e) data.frame(grp=e$grp,y=yof(e$ord),lab=grp_lab[e$grp])))

# ---- RoB 2 / Hales panel removed -------------------------------------------
# The single randomized trial (Hales 2020, CATS II) was excluded at full text:
# offspring outcome ascertained by the Social Communication Questionnaire
# (screening), not clinically diagnosed ASD. All 14 included studies are
# observational and assessed with ROBINS-E (matrix above).

# ---- legenda ---------------------------------------------------------------
leg_y <- -3.0
leg <- data.frame(lvl=names(col_lvl), x=c(40,52,66,80))
leg$fill<-col_lvl[leg$lvl]; leg$gly<-gly_lvl[leg$lvl]; leg$gcol<-gcol_lvl[leg$lvl]
leg_txt<-c(Low="Low risk","Some concerns"="Some concerns",High="High risk","Very high"="Very high risk")

PT <- 7.2   # tamanho do ponto
p <- ggplot() +
  # zebra
  geom_rect(data=zeb, aes(xmin=lab_right-43.5, xmax=99.8, ymin=y-0.5, ymax=y+0.5),
            fill=zeb$fill) +
  # banda à esquerda marcando grupos (faixa fina colorida)
  geom_rect(data=band_df, aes(xmin=0.2, xmax=0.7, ymin=ymin, ymax=ymax), fill=slate) +

  # cabeçalhos de domínio (rotacionados)
  annotate("text", x=as.numeric(xcol), y=N+0.9, label=dom_lab[dom_codes],
           angle=35, hjust=0, vjust=0, family=FONT, size=2.55, color=ink) +
  annotate("segment", x=xcol["Ov"]-3.3, xend=xcol["Ov"]-3.3, y=0.5, yend=N+0.5,
           color=bdr, linewidth=0.4) +  # separador antes de Overall

  # cabeçalhos de grupo
  geom_text(data=gh_df, aes(x=labx, y=y, label=lab), hjust=0, vjust=0.4,
            family=FONT, fontface="bold", size=2.75, color=ink) +

  # nomes dos estudos
  annotate("text", x=lab_right, y=unname(study_y), label=names(study_y),
           hjust=1, family=FONT, size=2.6, color=ink) +

  # círculos ROBINS-E
  geom_point(data=cells, aes(x=x,y=y), shape=21, fill=cells$fill,
             color=ifelse(cells$ov,"#2b3640","white"),
             size=ifelse(cells$ov,PT+0.6,PT), stroke=ifelse(cells$ov,0.9,0.5)) +
  geom_text(data=cells, aes(x=x,y=y,label=gly), color=cells$gcol,
            family=FONT, fontface="bold", size=3.2) +

  # ----- separador (matriz / legenda) -----
  annotate("segment", x=0.2, xend=99.8, y=-1.0, yend=-1.0, color=bdr, linewidth=0.4) +

  # ----- legenda -----
  annotate("text", x=labx, y=leg_y+0.9, hjust=0, family=FONT, fontface="bold",
           size=2.6, color=ink, label="Risk-of-bias judgement") +
  geom_point(data=leg, aes(x=x,y=leg_y-0.4), shape=21, fill=leg$fill, color="white", size=PT, stroke=0.5) +
  geom_text(data=leg, aes(x=x,y=leg_y-0.4,label=gly), color=leg$gcol, family=FONT, fontface="bold", size=3.2) +
  geom_text(data=leg, aes(x=x+1.8,y=leg_y-0.4,label=leg_txt[lvl]), hjust=0, family=FONT, size=2.55, color=ink) +

  scale_x_continuous(limits=c(0,103)) +
  scale_y_continuous(limits=c(leg_y-1.8, N+6)) +
  coord_cartesian(clip="off") +
  labs(title="Risk-of-bias assessment of included studies, grouped by maternal thyroid exposure domain",
       caption=paste0(
       "All included studies were observational and assessed with ROBINS-E. Studies informing more than one exposure domain appear once, under their primary domain. ",
       "Judgements are the\nconsensus of two independent reviewers after adjudication. ROBINS-E domains: 1, confounding; 2, measurement of the exposure; 3, selection of participants; ",
       "4, post-\nexposure interventions; 5, missing data; 6, measurement of the outcome; 7, selection of the reported result. Groups A\u2013E met eligibility and informed ",
       "synthesis (A\u2013B pooled or triangulated;\nC\u2013E narrative); group F met eligibility but was excluded from all synthesis for high or very high risk of bias.")) +
  theme_void(base_size=10) +
  theme(text=element_text(family=FONT),
        plot.title=element_text(face="bold", size=11.5, hjust=0, margin=margin(b=2)),
        plot.caption=element_text(size=6.6, color=slate, hjust=0, lineheight=1.35, margin=margin(t=12)),
        plot.margin=margin(12,14,8,12))

ggsave("eFigure1_RiskOfBias_byDomain.png", p, width=11, height=8.0, dpi=600, bg="white")  # 600 dpi raster (JAMA min. 350)
ggsave("eFigure1_RiskOfBias_byDomain.pdf", p, width=11, height=8.0, device=cairo_pdf, bg="white")  # vector: preferred for line art
cat("rob ok\n")
