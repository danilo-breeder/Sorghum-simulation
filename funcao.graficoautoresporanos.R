authorProdOverTime <- function(M, k = 11, graph = TRUE) { 
  
  # Sua lista de ordem desejada (agora será a ordem exata de exibição no eixo Y, de cima para baixo):
  ordem_autores_k11 <- c(
    "HOOGENBOOM G", "MELCHINGER A", "WANG X",       "CHAPMAN S",    
    "HAMMER G",     "PODLICH D",    "HICKEY J",     "WANG J",       
    "GORJANC G",    "COOPER M",     "PIEPHO H"
  )
  
  if (!("DI" %in% names(M))) {
    M$DI <- "NA"
  }
  M$TC <- as.numeric(M$TC)
  M$PY <- as.numeric(M$PY)
  M <- M[!is.na(M$PY), ] 
  
  Y <- as.numeric(substr(Sys.time(), 1, 4))
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  df <- data.frame(AU = trimws(unlist(listAU)), SR = rep(M$SR, nAU))
  
  AU_full <- df %>%
    group_by(AU) %>%
    count() %>%
    ungroup()
  
  # Força AU a ter a sua ordem exata
  AU <- data.frame(AU = ordem_autores_k11) %>%
    left_join(AU_full, by = "AU") %>%
    filter(!is.na(n))
  
  k <- nrow(AU) 
  
  if (k < 11) {
    warning(paste("Apenas", k, "dos 11 autores desejados foram encontrados no dataset M."))
  }
  
  df <- df %>%
    right_join(AU, by = "AU") %>%
    left_join(M, by = "SR") %>%
    select("AU.x", "PY", "TI", "SO", "DI", "TC") %>%
    mutate(TCpY = TC / (Y - PY + 1)) %>%
    group_by(AU.x) %>%
    mutate(n = length(AU.x)) %>%
    ungroup() %>%
    rename(
      Author = AU.x,
      year = PY,
      DOI = DI
    ) %>%
    arrange(desc(n), desc(year)) %>%
    select(-n)
  
  df2 <- dplyr::group_by(df, Author, year) %>%
    dplyr::summarise(freq = length(year), TC = sum(TC), TCpY = sum(TCpY)) %>%
    as.data.frame()
  
  # Define os níveis do fator na ordem desejada
  df2$Author <- factor(df2$Author, levels = AU$AU) 
  
  x <- c(0.5, 1.5 * k / 10)
  y <- c(min(df$year), min(df$year) + diff(range(df2$year)) * 0.125)
  
  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  
  g <- ggplot(df2, aes(x = Author, y = year, text = paste("Author: ", Author, "\nYear: ", year, "\nN. of Articles: ", freq, "\nTotal Citations per Year: ", round(TCpY, 2)))) +
    geom_point(aes(alpha = TCpY, size = freq), color = "dodgerblue4") +
    scale_size(range = c(2, 6)) +
    scale_alpha(range = c(0.3, 1)) +
    scale_y_continuous(breaks = seq(min(df2$year), max(df2$year), by = 2)) +
    guides(size = guide_legend(order = 1, "N.Artigos"), alpha = guide_legend(order = 2, "Cit./ano")) +
    theme(
      legend.position = "bottom",
      text = element_text(color = "#000000", size = 16),
      panel.background = element_rect(fill = "#FFFFFF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 90)
      , axis.title.x = element_text(hjust = .95)
      , axis.text.x = element_text(face = "bold", angle = 90),
      axis.text.y = element_text(face = "bold")
      , axis.line.x = element_line(color = "grey50", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = .2, color = "grey90")
    ) +
    coord_fixed(ratio = 1/1) +
    labs(
      title = NULL,
      x = "Autor",
      y = "Ano"
    ) +
    geom_line(data = df2, aes(x = Author, y = year, group = Author), size = 1.0, color = "firebrick4", alpha = 0.3) +
    # AJUSTE DA ORDEM: Removido o 'rev()'
    scale_x_discrete(limits = levels(df2$Author)) +
    coord_flip() +
    annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2])
  
  
  df$DOI <- as.character(df$DOI)
  res <- list(dfAU = df2, dfPapersAU = df, graph = g)
  if (isTRUE(graph)) {
    plot(g)
  }
  return(res)
}
