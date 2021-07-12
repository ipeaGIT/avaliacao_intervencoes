
source("../acesso_oport/R/fun/setup.R")

theme_mapa <- function(base_size) {
  
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      # legend.title=element_text(size=rel(0.7)),
      # legend.text=element_text(size=unit(7, "cm")),
      # legend.title=element_blank(),
      # plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}


compare_access <- function(sigla_muni) {
  
  city_code <- munis_list$munis_df[abrev_muni == sigla_muni]$code_muni
  
  # abrir shape da linha
  linha_leste_shape <- st_read("../../data-raw/avaliacao_intervencoes/for/linha_leste_kaue_gearth.gpkg")
  # abrir outras linhas
  gtfs <- gtfstools::read_gtfs("../../data/avaliacao_intervencoes/r5/graph/for_depois/gtfs_for_metrofor_2021-01_new.zip")
  linhas_shape <- gtfstools::get_trip_geometry(gtfs) %>%
    left_join(gtfs$trips %>% select(trip_id, route_id), by = "trip_id") %>%
    left_join(gtfs$routes %>% select(route_id, route_long_name), by = "route_id") %>%
    filter(!(route_id == "LL" & origin_file == "stop_times")) %>%
    count(route_long_name)
  
  
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
           dif_rel = round((depois-antes)/antes, 2)) %>%
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
  
  ks <- function (x) { scales::label_number(accuracy = 1,
                                            scale = 1/1000,
                                            suffix = "k",
                                            big.mark = ",")(x)
    
  }
  
  
  
  # variaveiss <- "CMATT60"
  
  fazer_plots_acess_comp <- function(variaveiss) {
    
    
    
    labelss <- if (grepl("TT", variaveiss)) ks else label_number(accuracy = 1)
    
    go <- access_comp %>%
      filter(quintil != 0) %>%
      filter(ind == variaveiss)
    # mutate(dif_abs = ifelse(dif_abs < 0, 0, dif_abs),
    #        dif_rel = ifelse(dif_rel < 0, 0, dif_rel))
    
    # map with access antes
    map1 <- ggplot()+
      geom_sf(data = go, aes(fill = antes), color = NA)+
      geom_sf(data = linhas_shape, size = 0.8)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Acessibilidade TP antes da intervencao",
           subtitle = variaveiss,
           fill = "",
           color = "Linha")
    
    # map with access antes
    map2 <- ggplot()+
      geom_sf(data = go, aes(fill = depois), color = NA)+
      geom_sf(data = linhas_shape, size = 0.6)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Acessibilidade TP depois da intervencao",
           # subtitle = variaveiss,
           fill = "")
    
    # diferenca absoluta
    map3 <- ggplot()+
      geom_sf(data = go, aes(fill = dif_abs), color = NA)+
      geom_sf(data = linhas_shape, size = 0.6)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Diferença absoluta",
           # subtitle = variaveiss,
           fill = "")
    
    # plot1 <- ggplot()+
    #   geom_boxplot(data = go, aes(x = 1, y = dif_abs), outlier.size = 0.5) +
    #   # coord_flip()+
    #   theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+    
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         axis.text.x = element_text(size = 5))
    
    # ggplot()+
    #   geom_point(data = go, aes(x = depois, y = antes, color = as.factor(quintil)), size = 2)+
    #   scale_color_brewer(palette = "RdBu")
    # # coord_flip()+
    # theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         axis.text.x = element_text(size = 5))
    
    # diferenca relativa
    map4 <- ggplot()+
      geom_sf(data = go, aes(fill = dif_rel), color = NA)+
      geom_sf(data = linhas_shape, size = 0.6)+
      geom_sf(data= city_shape, fill = NA)+
      scale_fill_viridis_c(option = "inferno", label = label_percent(accuracy = 1))+
      # scale_fill_distiller(palette = "PuBu", direction = 1
      #                      # limits = c(-1,1)*max(abs(go$dif1))
      #                      # breaks = c(-30000, 0, 30000),
      #                      # labels = c("-30 mil", 0, "30 mil")
      # )+
      theme_mapa()+
      labs(title = "Diferença relativa",
           # subtitle = variaveiss,
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
    plot <- 
      # map2 + map3 + map4 &
      map2 + (map3 |map4) +
      # plot_layout(nrow = 2) & 
      # plot_layout(nrow = 2, heights = c(3, 1)) & 
      plot_layout(heights = c(1, 1)) &
      theme(plot.title = element_text(size = 7),
            plot.subtitle = element_text(size = 6),
            legend.text = element_text(size = 5))
    
    # out
    filename <- sprintf("figures/%s/access_comparison/fig1_mapdif_%s_%s", sigla_muni, sigla_muni, variaveiss)
    ggsave(plot = plot, filename = paste0(filename, ".png"), height = 12, width = 16, units = "cm", device = "png")
    # ggsave(plot = plot, filename = paste0(filename, ".pdf"), height = 12, width = 16, units = "cm", device = "pdf")
    
  }
  
  variaveis <- grep(pattern = "TT|ET|EI|EF|EM|ST|SB|SM|SA", x = colnames(access), value = TRUE)
  
  lapply(variaveis, fazer_plots_acess_comp)
  
  
  
}

# apply function
compare_access('for')




# plot access inequalities
go <- access_comp %>%
  filter(quintil != 0) %>%
  filter(ind %in% c("CMATT60", "CMASA60", "CMAET30", "CMAET60"))

boxplot_inequalities1 <- ggplot()+
  geom_boxplot(data = go, 
               aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
               outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
  # coord_flip()+
  facet_wrap(~ind, scale = "free_y")+
  scale_y_continuous(label = percent)+
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
  theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
  guides(color=guide_legend(nrow=1)) +
  theme( 
    panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 5, face ="bold")
    ,legend.text = element_text(size = 5)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    , axis.text.y = element_text(size = 5),
    axis.title.x = element_text(size = 5),
    axis.title.y = element_text(size = 5, face="bold"),
    legend.title = element_text(size = 5)
    
  )


ggsave(plot = boxplot_inequalities1, 
       filename = sprintf("figures/%s/access_inequalities/fig1_ineq1_%s.png", sigla_muni, sigla_muni), 
       height = 12, width = 16, units = "cm", device = "png")




# teste: pegar locais que tiveram pelo menos 2% de melhoria no acesso
boxplot_inequalities2 <- go %>%
  filter(dif_rel >= 0.02) %>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
               outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
  # coord_flip()+
  facet_wrap(~ind, scale = "free_y")+
  scale_y_continuous(label = percent)+
  theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
  
  coord_cartesian(ylim = c(0, NA))+
  geom_hline(yintercept = 0)+
  guides(color=guide_legend(nrow=1)) +
  theme( 
    panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 5, face ="bold")
    ,legend.text = element_text(size = 5)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    , axis.text.y = element_text(size = 5),
    axis.title.x = element_text(size = 5),
    axis.title.y = element_text(size = 5, face="bold"),
    legend.title = element_text(size = 5)
    
  )


ggsave(plot = boxplot_inequalities2, 
       filename = sprintf("figures/%s/access_inequalities/fig1_ineq2_%s.png", sigla_muni, sigla_muni), 
       height = 12, width = 16, units = "cm", device = "png")





baseplot1 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold", size=9)
    ,axis.text.x  = element_text(face="bold", size=9)
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
  )

scatterplot_inequalities <- ggplot()+
  geom_point(data = go, 
             aes(x = antes, y = depois, color = as.factor(quintil), size = pop_total/100),
             alpha = 0.4)+
  geom_abline()+
  scale_color_brewer(palette = "RdBu")+
  # coord_flip()+
  # theme_ipsum_rc(base_family = 'Helvetica')+
  facet_wrap(~ind, scale = "free")+
  labs(size = "Pop. total, milhares",
       color = "Quintil de renda")+
  baseplot1+
  guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=5))) +
  # guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
  theme(
    # Legends
    legend.position="bottom", # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=7),
    legend.key.size = unit(.2, "cm"),
    legend.text.align=0.5,
    # axis
    axis.ticks=element_blank())





ggsave(plot = scatterplot_inequalities, 
       filename = sprintf("figures/%s/access_inequalities/fig2_scatterplot_%s.png", sigla_muni, sigla_muni), 
       height = 12, width = 16, units = "cm", device = "png")
