library(tidyverse)
library(scales)

all = nrow(dfall)
inc = nrow(dfinc)
exc = nrow(dfall) - nrow(dfinc)
ipd    = nrow(filter(dfinc, patient_type == 1))
opd    = nrow(filter(dfinc, patient_type == 2))
ptmiss = nrow(filter(dfinc, is.na(patient_type)))
ipdsn = nrow(filter(dfipd, afb == 0))
ipdsp = nrow(filter(dfipd, afb == 1))
ipdsm = nrow(filter(dfipd, is.na(afb)))
ipdsntb    = nrow(filter(dfipd, afb == 0 & tb == 'Yes'))
ipdsnno    = nrow(filter(dfipd, afb == 0 & tb == 'No'))
ipdsnpt    = nrow(filter(dfipd, afb == 0 & tb_diag %in% c(1,3)))
ipdsnet    = nrow(filter(dfipd, afb == 0 & tb_diag == 2))
ipdsnptpos = nrow(filter(dfipdsnpt, lam == 'Positive'))
ipdsnptneg = nrow(filter(dfipdsnpt, lam == 'Negative'))

# Texts for figures
l1 <- paste0('All PLHIV in the database',
             '\n',
             '(n=', all, ')')
l2 <- paste0('Excluded',
             '\n',
             '(n=', exc, ')')
l3 <- paste0('Included in analysis',
             '\n',
             '(n=', inc, ')')
l4 <- paste0('OPD', 
             '\n',
             '(n=', opd, ')')
l5 <- paste0('IPD', 
             '\n',
             '(n=', ipd, ')')
l6 <- paste0('Missing patient type', 
             '\n',
             '(n=', ptmiss, ')')
l7 <- paste0('Smear +', 
             '\n',
             '(n=', ipdsp, ')')
l8 <- paste0('Smear -', 
             '\n',
             '(n=', ipdsn, ')')
l9 <- paste0('Missing', 
             '\n',
             '(n=', ipdsm, ')')
l10 <- paste0('TB', 
             '\n',
             '(n=', ipdsntb, ')')
l11 <- paste0('Not TB', 
             '\n',
             '(n=', ipdsnno, ')')
l12 <- paste0('Pulmonary & Disseminated TB', 
             '\n',
             '(n=', ipdsnpt, ')')
l13 <- paste0('Extrapulmonary TB', 
             '\n',
             '(n=', ipdsnet, ')')
l14 <- paste0('LAM -', 
             '\n',
             '(n=', ipdsnptneg, ')')
l15 <- paste0('LAM +', 
             '\n',
             '(n=', ipdsnptpos, ')')

d <- DiagrammeR::grViz(
  "digraph flowchart {

    graph [layout = dot,
           #splines=ortho,
           nodesep = 1;
           compound = true]

    node [shape = box,
          fixedsize = t,
          width = 3,
          height = 0.8,
          style = filled,
          color = gray,
          fillcolor = WhiteSmoke,
          fontname = Helvetica,
          fontsize = 20]

    all [label = '@@1']
    exc [label = '@@2']
    inc [label = '@@3']
    opd [label = '@@4']
    ipd [label = '@@5', color = 'red']
    ptmiss [label = '@@6']
    ipdsp [label = '@@7']
    ipdsn [label = '@@8']
    ipdsm [label = '@@9']
    ipdsntb [label = '@@10']
    ipdsnno [label = '@@11']
    ipdsnpt [label = '@@12', color = 'blue']
    ipdsnet [label = '@@13']
    ipdsnptneg [label = '@@14']
    ipdsnptpos [label = '@@15']
    blank1 [label = '', width = 0.01, height = 0.01]
    blank2 [label = '', width = 0.01, height = 0.01]
    blank3 [label = '', width = 0.01, height = 0.01]
    blank4 [label = '', width = 0.01, height = 0.01]
    blank5 [label = '', width = 0.01, height = 0.01]
    blank6 [label = '', width = 0.01, height = 0.01]

    all     -> blank1 [dir = none];
    blank1  -> exc;
    {rank = same; blank1 exc};
    blank1 -> inc;
    inc -> blank2 [dir = none];
    blank2 -> opd;
    blank2 -> ipd;
    blank2 -> ptmiss;
    {rank = same; opd ipd ptmiss};
    ipd  -> blank3 [dir = none];
    blank3 -> ipdsp;
    blank3  -> ipdsn;
    blank3  -> ipdsm;
    {rank = same; ipdsp ipdsn ipdsm};
    ipdsn  -> blank4 [dir = none];
    blank4 -> ipdsntb;
    blank4 -> ipdsnno;
    {rank = same; ipdsntb ipdsnno};
    ipdsntb -> blank5 [dir = none];
    blank5  -> ipdsnpt;
    blank5  -> ipdsnet;
    {rank = same; ipdsnpt ipdsnet};
    ipdsnpt  -> ipdsnptneg;
    ipdsnpt  -> ipdsnptpos;
    {rank = same; ipdsnptneg ipdsnptpos};
    
  }

  [1]: l1
  [2]: l2
  [3]: l3
  [4]: l4
  [5]: l5
  [6]: l6
  [7]: l7
  [8]: l8
  [9]: l9
  [10]: l10
  [11]: l11
  [12]: l12
  [13]: l13
  [14]: l14
  [15]: l15

")


