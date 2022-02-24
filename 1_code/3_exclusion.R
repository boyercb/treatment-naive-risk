original_n <- nrow(mesa)

# exclude those outside the age range at baseline
mesa <- filter(mesa, age1c >= 40 & age1c <= 79)

exclude_age <- original_n - nrow(mesa)

# exclude those with pre-existing CVD at baseline
mesa <- filter(mesa, prebase == "")

exclude_prebase <- original_n - exclude_age - nrow(mesa)

# exclude those on statins at baseline
mesa <- filter(mesa, lipid1c != 1)

exclude_liprxx <- original_n - exclude_age - exclude_prebase - nrow(mesa)

sample_size <- original_n - exclude_age - exclude_prebase - exclude_liprxx


a1 <-
  paste0(
    'Participants from MESA exam 1 \n(N = ',
    format(original_n, big.mark = ","),
    ')'
  )
b1 <- ''
c1 <- ''
d1 <- ''
e1 <- paste0('Final sample \n(N = ',
             format(sample_size, big.mark = ","),
             ')')
a2 <- ''
b2 <-
  paste0('Baseline age outside of\n 40-79 year target\n(N = ',
         format(exclude_age, big.mark = ","),
         ')')
c2 <-
  paste0('Pre-existing CVD at baseline \n(N = ',
         format(exclude_prebase, big.mark = ","),
         ')')
d2 <-
  paste0(
    'Already on lipid-lowering\ntherapy at baseline\n(N = ',
    format(exclude_liprxx, big.mark = ","),
    ')'
  )

e2 <- ''


# Create a node dataframe
ndf <- create_node_df(
  n = 10,
  label = c(a1, b1, c1, d1, e1, # Column 1
            a2, b2, c2, d2, e2), # Column 2
  style = c('solid', 'invis', 'invis', 'invis', 'solid', # Column 1
            'invis', 'solid', 'solid', 'solid', 'invis'), # Column 2
  shape = c('box', 'point', 'point', 'point', 'box', # Column 1 
            'plaintext', 'box', 'box', 'box', 'point'), # Column 2
  width = c(3, 0.001, 0.001, 0.001, 3, # Column 1
            3, 3, 3, 3, 3), # Column 2
  height = c(1, 0.001, 0.001, 0.001, 1, # Column 1
             1, 1, 1, 1, 1), # Column 2
  color = 'black',
  fontcolor = 'black',
  fontsize = c(rep(14, 10)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true',
  x = c(0, 0, 0, 0, 0,
        3.5, 3.5, 3.5, 3.5, 3.5),
  y = c(5, 3.75, 2.5, 1.25, 0, 
        5, 3.75, 2.5, 1.25, 0)
)

# Create an edge dataframe
edf <- create_edge_df(
  from = c(1, 2, 3, 4, # Column 1
           6, 7, 8, 9, # Column 2
           2, 3, 4 # Horizontals
  ),
  to = c(2, 3, 4, 5, # Column 1
         7, 8, 9, 10, # Column 2
         7, 8, 9 # Horizontals
  ),
  arrowhead = c('none', 'none', 'none', 'normal', # Column 1
                'none', 'none', 'none', 'none', # Column 2
                'normal', 'normal', 'normal' # Horizontals
  ),
  color = c('black', 'black', 'black', 'black', # Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', # Column 2
            'black', 'black', 'black' # Horizontals
  ),
  constraint = c(rep('true', 8), # Columns
                 rep('false', 3) # Horizontals
  )
)

g <- create_graph(ndf,
                  edf)

g

render_graph(g, layout = "neato")

export_graph(graph = g,
             file_type = "pdf",
             file_name = "3_figures/figS_STROBE.pdf")


