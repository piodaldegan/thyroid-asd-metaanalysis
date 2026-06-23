# Analysis Code — Maternal Thyroid Dysfunction and Offspring ASD

**Manuscript:** The Impact of Maternal Thyroid Dysfunction on Autism Risk in Offspring: A Systematic Review and Meta-analysis  
**Journal:** JAMA Psychiatry (under review)  
**Authors:** Vitor Pio Daldegan, Ana Carolina Gomes Pereira, Patrícia Borges Botelho, Lício A. Velloso  
**PROSPERO registration:** CRD420261290024

---

## Overview

This repository contains the annotated R code used to produce all figures in the manuscript. The meta-analytic estimates (pooled hazard ratios, 95% confidence intervals, heterogeneity statistics) were computed in **Review Manager 5.4.1** (Cochrane Collaboration) and are supplied as hard-coded inputs to the figure scripts. The R scripts handle visualization, quantitative bias analysis (QBA), and risk-of-bias display only.

---

## Repository Contents

| File | Figure in manuscript | Description |
|---|---|---|
| `Figure1_PRISMA_FlowDiagram.R` | Figure 1 | PRISMA 2020 flow diagram of study selection |
| `Figure2_Forest_Plots_JAMA.R` | Figure 2 | Two-panel forest plot (maternal hypothyroidism / hyperthyroidism vs offspring ASD) |
| `Figure3_QBA_Distributions_JAMA.R` | Figure 3 | Probabilistic quantitative bias analysis (QBA) density panels |
| `eFigure1_RiskOfBias_byDomain.R` | eFigure 1 (Supplement) | ROBINS-E and Cochrane RoB 2 traffic-light risk-of-bias figure |

Each script saves both a **vector PDF** (`cairo_pdf` device, preferred for publication) and a **600 dpi PNG** (JAMA minimum 350 dpi) to the working directory.

---

## Dependencies

All scripts require **R ≥ 4.2.0**. Install the required packages with:

```r
install.packages(c("ggplot2", "patchwork", "dplyr"))
```

> `grid` ships with base R and does not need to be installed separately.  
> `ggchicklet` is mentioned in `Figure1_PRISMA_FlowDiagram.R` as an optional extension for rounded box corners and is **not required** to run the script.

### Package versions used in the original analysis

| Package | Version |
|---|---|
| ggplot2 | 3.5.1 |
| patchwork | 1.2.0 |
| dplyr | 1.1.4 |

---

## Reproducing the Figures

Clone the repository and run each script from its root directory. No additional data files are needed — all inputs (study-level estimates, pooled results, risk-of-bias judgements) are embedded in the scripts.

```bash
git clone https://github.com/piodaldegan/thyroid-asd-metaanalysis.git
cd <repo-name>
Rscript Figure1_PRISMA_FlowDiagram.R
Rscript Figure2_Forest_Plots_JAMA.R
Rscript Figure3_QBA_Distributions_JAMA.R
Rscript eFigure1_RiskOfBias_byDomain.R
```

Each script prints a confirmation message when complete (e.g., `Figure 1 rendered.`) and writes the output files to the working directory.

---

## Key Meta-analytic Estimates (from Review Manager 5.4.1)

These are the canonical pooled estimates reported in the manuscript:

| Exposure domain | Pooled estimate | 95% CI | I² |
|---|---|---|---|
| Maternal hypothyroidism | HR 1.34 | 1.22–1.46 | 39% |
| Maternal hyperthyroidism | HR 1.17 | 1.07–1.28 | 0% |
| Thyroid peroxidase antibodies (TPOAb) | OR 1.78 | 1.16–2.75 | — |

QBA-adjusted probability that the effect exceeds the null (primary prior: familial confounding median 1.40, 95% interval 1.11–1.77):

| Domain | P(adjusted estimate > 1.0) |
|---|---|
| Hypothyroidism | 36.8% |
| Hyperthyroidism | 8.1% |
| TPOAb | 83.1% |

---

## Use of AI Tools

R code in this repository was drafted with assistance from Claude (Anthropic), under the direction of and with iterative review by the authors, as described in eAppendix 4 of the Supplementary Online Content. All code was executed, verified against extracted study data, and edited by the authors, who take responsibility for its integrity.

---

## License

This code is shared under the [MIT License](LICENSE). The underlying study data are owned by the original study authors; this repository does not redistribute individual participant data.

---

## Citation

If you use this code, please cite the manuscript:

> Daldegan VP, Gomes Pereira AC, Borges Botelho P, Velloso LA. The Impact of Maternal Thyroid Dysfunction on Autism Risk in Offspring: A Systematic Review and Meta-analysis. *JAMA Psychiatry*. [In review]

A citable DOI for this repository will be available via Zenodo upon publication.
