# VarWiz
An interactive genomic dashboard written in [Shiny](https://shiny.posit.co/) and [React](https://react.dev/).

> Checkout the dashboard at [https://vojtam-varwiz.hf.space/](https://vojtam-varwiz.hf.space/).

## Introduction & motivation
The idea for this project was to create a part of an interactive dashboard for visualization of genomic DNA variants, such as insertions, deletions, etc. The motivation was that such a visualization could be helpful to medical geneticists, who might use it to gain insights into somatic or germline mutations of a specific patient for specifically targeted gene panels (e.g. BRONCO). As a data source, the dashboard is meant to visualize Mutation Annotation Format (MAF) files. These files aggregate the results of variant-calling pipelines. Specifically, I have used [breast cancer BRCA TCGA dataset](https://portal.gdc.cancer.gov/projects/TCGA-BRCA).

The visualization is dominated by an interactive Sankey diagram that hierarchically aggregates the information over a selected set of features. This diagram creates an implicit hierarchy of pathways -> genes -> variants. The user selection spawns a more detail-oriented plot of variants found in the gene.

The Sankey is interactive - the nodes may be dragged and rearranged, hovering on the nodes in the variants column shows the chromosome and location of the variant, together with the change caused by it. The most important aspect of the interactivity is the ability to select any link by clicking on it. This action will select that particular gene and draw a lollipop diagram, which shows the genomic view of the gene and visualizes the mutations of the gene.

![dash_whole](https://github.com/user-attachments/assets/f62255ab-fca0-4253-831f-4696a84174b1)

![dash_detail](https://github.com/user-attachments/assets/04165a3e-e9b9-44a0-a0be-7c0c29bb254e)

