original_n <- nrow(mesa)

# exclude those outside the age range at baseline
mesa <- filter(mesa, age1c >= 40 & age1c <= 79)

exclude_age <- original_n - nrow(mesa)

# exclude those with pre-existing CVD at baseline or no follow up data
mesa <- filter(mesa, prebase == ""  & !is.na(cvdatt))

exclude_prebase <- original_n - exclude_age - nrow(mesa)

# exclude those on statins at baseline
mesa <- filter(mesa, lipid1c != 1)

exclude_liprxx <- original_n - exclude_age - exclude_prebase - nrow(mesa)

sample_size <- original_n - exclude_age - exclude_prebase - exclude_liprxx

a1 <-
  paste0(
    'Participants who completed\nMESA exam 1 \n(N = ',
    format(original_n, big.mark = ","),
    ')'
  )
b1 <- ''
c1 <- ''
d1 <- ''
e1 <- paste0('Eligible sample \n(N = ',
             format(sample_size, big.mark = ","),
             ')')
f1 <- paste0('Initiators \n(N = ',
             format(table(mesa$primary_lipmed)[2], big.mark = ","),
             ')')
  
a2 <- ''
b2 <-
  paste0('Age outside of\n40-79 years\n(N = ',
         format(exclude_age, big.mark = ","),
         ')')
c2 <-
  paste0('Pre-existing CVD \n(N = ',
         format(exclude_prebase, big.mark = ","),
         ')')
d2 <-
  paste0(
    'Already on lipid-lowering\ntherapy\n(N = ',
    format(exclude_liprxx, big.mark = ","),
    ')'
  )

e2 <- ''
f2 <- paste0('Non-initiators \n(N = ',
             format(table(mesa$primary_lipmed)[1], big.mark = ","),
             ')')

ndf <- create_node_df(
  n = 12,
  label = c(a1, b1, c1, d1, e1, f1,  # Column 1
            a2, b2, c2, d2, e2, f2), # Column 2
  style = c('solid', 'invis', 'invis', 'invis', 'solid', 'solid',  # Column 1
            'invis', 'solid', 'solid', 'solid', 'invis', 'solid'), # Column 2
  shape = c('box', 'point', 'point', 'point', 'box', 'box',  # Column 1 
            'plaintext', 'box', 'box', 'box', 'point', 'box'), # Column 2
  width = c(3, 0.001, 0.001, 0.001, 3, 3, # Column 1
            3, 3, 3, 3, 0.001, 3), # Column 2
  height = c(1, 0.001, 0.001, 0.001, 1, 1, # Column 1
             1, 1, 1, 1, 0.001, 1), # Column 2
  color = 'black',
  fontcolor = 'black',
  fontsize = c(rep(14, 12)),
  fontname = c(rep('Helvetica', 12)),
  penwidth = 1.5,
  fixedsize = 'true',
  x = c(0, 0, 0, 0, 0, -1.75,
        3.5, 3.5, 3.5, 3.5, 3.5, 1.75),
  y = c(6.25, 5, 3.75, 2.5, 1.25, -0.5, 
        6.25, 5, 3.75, 2.5, 1.25, -0.5)
)

# Create an edge dataframe
edf <- create_edge_df(
  from = c(1, 2, 3, 4, 5, 5, # Column 1
           7, 8, 9, 10, 11,  # Column 2
           2, 3, 4        # Horizontals
  ),
  to = c(2, 3, 4, 5, 6, 12, # Column 1
         8, 9, 10, 11, 12,  # Column 2
         8, 9, 10        # Horizontals
  ),
  arrowhead = c('none', 'none', 'none', 'none', 'normal', 'normal', # Column 1
                'none', 'none', 'none', 'none', 'none',   # Column 2
                'normal', 'normal', 'normal'     # Horizontals
  ),
  color = c('black', 'black', 'black', 'black', 'black', 'black', # Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', '#00000000', # Column 2
            'black', 'black', 'black'  # Horizontals
  ),
  constraint = c(rep('true', 11), # Columns
                 rep('false', 3)  # Horizontals
  )
)

g <- create_graph(ndf,
                  edf)

g

render_graph(g, layout = "neato")

export_graph(graph = g,
             file_type = "pdf",
             file_name = "3_figures/figS_STROBE.pdf")


