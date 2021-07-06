library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)
library(patchwork)
library(ggplot2)

source("../acesso_oport/R/fun/setup.R")

theme_mapa <- function(base_size) {
  
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.7)),
      legend.text=element_text(size=unit(7, "cm")),
      # legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}


compare_access <- function(sigla_muni) {
  
  city_code <- munis_list$munis_df[abrev_muni == sigla_muni]$code_muni
  
  # abrir shape da linha
  linha_shape <- st_read("../../data-raw/avaliacao_intervencoes/for/linha_leste_kaue_gearth.gpkg")
  
  # abrir city limits
  city_shape <- geobr::read_municipality(city_code)
  
  
  # abrir access
  access <- read_rds(sprintf("../../data/avaliacao_intervencoes/output_access/acess_%s.rds", sigla_muni))
  
  
  # compare each indicator
  access_comp <- setDT(access)
  access_comp <- access_comp[!is.na(quintil)]
  access_comp <- access_comp %>% dplyr::select(city, origin, pop_total, quintil, decil, tipo, geometry, CMATT30:CMAEM120) %>%
    gather("ind", "valor", CMATT30:CMAEM120) %>%
    spread(tipo, valor) %>%
    # calculate abs diffs
    mutate(dif_abs = depois - antes,
           dif_rel = depois/antes) %>%
    st_sf(crs = 4326)
  
  
  
  # variaveiss <- "CMATT60"
  # variaveiss <- "CMASA60" checar os zeros desse indicador
  # TODO: 
  # - criar graficos (boxplot + pontos) com desigualdades
  # - colocar as outras linhas de alta capacidade nos mapas
  # - mapas: antes e depois (em cima) e dif abs e dif rel (em baixo)
  # - calcular indicador gravitacional
  # - colocar porcentagem nos valores relativos
  
  library(scales)
  
  ks <- function (x) { scales::number_format(accuracy = 1,
                                             scale = 1/1000,
                                             suffix = "k",
                                             big.mark = ",")(x)
    
  }
  
  fazer_plots_acess_comp <- function(variaveiss) {
    
    
    
    labelss <- if (grepl("TT", variaveiss)) ks else number_format(accuracy = 1)
    
    go <- access_comp %>%
      filter(quintil != 0) %>%
      filter(ind == variaveiss)
    # mutate(dif_abs = ifelse(dif_abs < 0, 0, dif_abs),
    #        dif_rel = ifelse(dif_rel < 0, 0, dif_rel))
    
    map1 <- ggplot()+
      geom_sf(data = go, aes(fill = dif_abs), color = NA)+
      geom_sf(data = linha_shape, size = 0.2)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Diferença absoluta",
           subtitle = variaveiss,
           fill = "")
    
    # plot1 <- ggplot()+
    #   geom_boxplot(data = go, aes(x = 1, y = dif_abs), outlier.size = 0.5) +
    #   # coord_flip()+
    #   theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+    
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         axis.text.x = element_text(size = 5))
    
    ggplot()+
      geom_point(data = go, aes(x = depois, y = antes, color = as.factor(quintil)), size = 2)+
      scale_color_brewer(palette = "RdBu")
    # coord_flip()+
    theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
      theme(axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 5))
    
    map2 <- ggplot()+
      geom_sf(data = go, aes(fill = dif_rel), color = NA)+
      geom_sf(data = linha_shape, size = 0.2)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(option = "inferno")+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Diferença relativa",
           subtitle = variaveiss,
           fill = "")
    
    # plot1 <- ggplot()+
    #   geom_boxplot(data = go, aes(x = 1, y = dif_abs), outlier.size = 0.5) +
    #   # coord_flip()+
    #   theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+    
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         axis.text.x = element_text(size = 5))
    
    
    # arrange layout
    plot <- map1 + map2 + 
      plot_layout(nrow = 1) & 
      # plot_layout(nrow = 2, heights = c(3, 1)) & 
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7))
    
    # out
    filename <- sprintf("figures/%s/fig1_mapdif_%s_%s", sigla_muni, sigla_muni, variaveiss)
    ggsave(plot = plot, filename = paste0(filename, ".png"), height = 10, width = 16, units = "cm", device = "png")
    ggsave(plot = plot, filename = paste0(filename, ".pdf"), height = 12, width = 16, units = "cm", device = "pdf")
    
  }
  
  variaveis <- grep(pattern = "TT|ET|EI|EF|EM|ST|SB|SM|SA", x = colnames(access), value = TRUE)
  
  lapply(variaveis, fazer_plots_acess_comp)
  
  
  
}



