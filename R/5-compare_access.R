
source("../acesso_oport/R/fun/setup.R")
library(ggnewscale)

theme_mapa <- function(base_size) {
  
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      # legend.title=element_text(size=rel(0.7)),
      # legend.text=element_text(size=unit(7, "cm")),
      # legend.title=element_blank(),
      # plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}


compare_access <- function(sigla_muni, modo_acesso) {
  
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
  
  # paradas <- gtfs$stops %>% 
  #   mutate(linha = ifelse(stop_id %in% c("XDS", "CMT", "NVT", "PAP"), "Linha Leste", "Outra")) %>%
  #   select(stop_name, linha, stop_lon, stop_lat) %>% 
  #   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
  # 
  # st_write(linhas_shape, "linhas_metrofor_fortaleza.gpkg")
  # st_write(paradas, "paradas_metrofor_fortaleza.gpkg")
  
  
  # rename
  linhas_shape <- linhas_shape %>%
    mutate(route_long_name1 = case_when(route_long_name == "Metro Linha Leste" ~ "Linha Leste",
                                        route_long_name == "VLT Parangaba Papicu" ~ "VLT",
                                        TRUE ~ route_long_name ))
  
  
  # abrir city limits
  city_shape <- geobr::read_municipality(city_code)
  
  
  # abrir access
  access <- read_rds(sprintf("../../data/avaliacao_intervencoes/output_access/acess_%s_%s.rds", sigla_muni, modo_acesso))
  
  
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
  # variaveiss <- "CMAET60"
  # variaveiss <- "CMASM60"
  
  fazer_plots_acess_comp <- function(variaveiss) {
    
    
    
    labelss <- if (grepl("TT", variaveiss)) ks else label_number(accuracy = 1)
    
    go <- access_comp %>%
      filter(quintil != 0) %>%
      filter(ind == variaveiss)
    # mutate(dif_abs = ifelse(dif_abs < 0, 0, dif_abs),
    #        dif_rel = ifelse(dif_rel < 0, 0, dif_rel))
    
    
    # go %>%
    #   filter(dif_rel >= 0.02) %>%
    #   pull(dif_abs) %>% mean()
    # 
    # go %>%
    #   filter(dif_rel >= 0.02) %>%
    #   pull(dif_rel) %>% mean()
    # 
    # hex_ben2 <- go %>%
    #   filter(dif_rel >= 0.02) %>% pull(origin)
    # 
    # a <- unique(c(hex_ben1, hex_ben2))
    # 
    # go %>% filter(origin %in% a) %>% pull(pop_total) %>% sum()
    # 
    # %>%
    #   
    #   pull(pop_total) %>% sum()
    
    library(ggtext)
    
    # abrir tiles
    # abrir tiles
    map_tiles <- read_rds(sprintf("../../data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_muni))
    
    
    # transform map elements to UTM
    linhas_shape_map <- st_transform(linhas_shape, 3857)
    city_shape_map <- st_transform(city_shape, 3857)
    go_map <- st_transform(go, 3857)
    
    # create fun
    create_map_acess <- function(var) {
      
      ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = go_map, aes(fill = {{ var }} ), color = NA)+
        geom_sf(data = linhas_shape_map, size = 0.5, alpha = 0.7)+
        # geom_sf(data = linhas_shape, aes(color = route_long_name1), size = 0.5, alpha = 0.7)+
        geom_sf(data= city_shape_map, fill = NA)+
        theme_mapa()
    }
    
    
    
    
    # map with access antes
    map1 <- create_map_acess(antes) +
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      labs(title = "Acessibilidade TP ***antes***",
           # subtitle = variaveiss,
           fill = "")
    
    # map with acess depois
    map2 <- create_map_acess(depois)+
      scale_fill_viridis_c(labels = labelss, option = "inferno")+
      labs(title = "Acessibilidade TP ***depois***",
           # subtitle = variaveiss,
           fill = "")
    
    map3 <- create_map_acess(dif_abs)+
      scale_fill_distiller(palette = "PuBu", direction = 1, labels = labelss
                           # limits = c(-1,1)*max(abs(go$dif1))
                           # breaks = c(-30000, 0, 30000),
                           # labels = c("-30 mil", 0, "30 mil")
      )+
      labs(title = "Diferença ***absoluta***",
           # subtitle = variaveiss,
           fill = "")
    
    map4 <- create_map_acess(dif_abs)+
      scale_fill_distiller(palette = "PuBu", direction = 1
                           # limits = c(-1,1)*max(abs(go$dif1))
                           # breaks = c(-30000, 0, 30000),
                           # labels = c("-30 mil", 0, "30 mil")
                           , label = label_percent(accuracy = 1)
      )+
      labs(title = "Diferença ***relativa***",
           # subtitle = variaveiss,
           fill = "")
    
    
    # arrange plot
    plot_conditions <- 
      # map2 + map3 + map4 &
      # (map1 | map2) / (map3 |map4) +
      (map1 | map2) +
      # plot_layout(nrow = 2) & 
      # plot_layout(nrow = 2, heights = c(3, 1)) & 
      plot_layout(heights = c(1, 1)) &
      theme_mapa() +
      theme(plot.title = element_markdown(size = 9),
            plot.subtitle = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.key.width = unit(0.8, "cm"))
    
    # out
    filename <- sprintf("figures/%s/access_conditions/map1_conditions_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
    ggsave(plot = plot_conditions, filename = paste0(filename, ".png"),
           height = 10, width = 16,
           # height = 14, width = 16,
           units = "cm", device = "png", dpi = 300)
    
    plot_comparison <- 
      # map2 + map3 + map4 &
      # (map1 | map2) / (map3 |map4) +
      (map3 | map4) +
      # plot_layout(nrow = 2) & 
      # plot_layout(nrow = 2, heights = c(3, 1)) & 
      plot_layout(heights = c(1, 1)) &
      theme(plot.title = element_markdown(size = 9),
            plot.subtitle = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.key.width = unit(0.8, "cm"))
    
    # out
    filename <- sprintf("figures/%s/access_comparison/map2_comparison_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
    ggsave(plot = plot_comparison, filename = paste0(filename, ".png"),
           height = 10, width = 16,
           # height = 14, width = 16,
           units = "cm", device = "png", dpi = 300)
    
    
    
    
    # plot access inequalities --------------------------------------------------------------
    
    # 1) access inequalities boxplot 1
    go_long <- go %>%
      pivot_longer(cols = dif_abs:dif_rel,
                   names_to = "tipo_indicador",
                   values_to = "valor_indicador"
      )
    # 
    # # change label
    # go_long <- go_long %>%
    #   mutate(valor_indicador1 = ifelse(tipo_indicador == "dif_rel", 
    #                                    paste0(valor_indicador * 100, " %"), valor_indicador))
    
    # calcular palma ratio
    acess_palma <- go_long %>%
      select(city, decil, pop_total, tipo_indicador, valor_indicador) %>%
      # pegar so decis 4 e 9
      filter(decil %in% c(1, 2, 3, 4, 10)) %>%
      # definir ricos e pobres
      mutate(classe = ifelse(decil %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
      group_by(city, classe, tipo_indicador) %>%
      summarise(acess_media = weighted.mean(valor_indicador, pop_total)) %>%
      ungroup() %>%
      spread(classe, acess_media) %>%
      # calcular palma ratio
      group_by(city, tipo_indicador) %>%
      mutate(palma_ratio = rico/pobre) %>%
      ungroup()
    
    # definir valor pra truncar eixo y
    valor_trunc_abs <- ifelse(variaveiss == "CMATT60", 20000, 10)
    valor_trunc_rel <- ifelse(variaveiss == "CMATT60", 0.1, 0.1)
    
    
    boxplot_inequalities11 <- ggplot()+
      geom_boxplot(data = go, 
                   aes(x = as.factor(decil), y = dif_abs, weight = pop_total, color = as.factor(decil)), 
                   outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05),
                   show.legend = FALSE) +
      # coord_flip()+
      # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
      # scale_y_continuous(label = percent)+
      # scale_y_continuous(labels = function(x) ifelse(x <= 2, paste0(x * 10, "%"), x))+
      scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
      theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
      guides(color=guide_legend(nrow=1)) +
      labs(y = "Ganho absoluto de acess.", x = "",
           subtitle = sprintf("**Razão de Palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_abs")$palma_ratio, 2)))+
      coord_cartesian(ylim = c(NA, valor_trunc_abs)) +
      theme( 
        panel.grid.minor = element_blank()
        ,strip.text = element_markdown(size = 7, face ="bold")
        ,legend.text = element_text(size = 5)
        , legend.position = "bottom"
        , axis.text.x = element_blank()
        , axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7, face="bold"),
        legend.title = element_text(size = 7)
        
      )
    
    boxplot_inequalities12 <- ggplot()+
      geom_boxplot(data = go, 
                   aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
                   outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
      # coord_flip()+
      # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
      # scale_y_continuous(label = percent)+
      scale_y_continuous(labels = percent)+
      scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
      theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
      guides(color=guide_legend(nrow=1)) +
      labs(y = "Ganho relativo de acess.", x = "",
           subtitle = sprintf("**Razão de palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_rel")$palma_ratio, 2)))+
      coord_cartesian(ylim = c(NA, valor_trunc_rel)) +
      theme( 
        panel.grid.minor = element_blank()
        ,strip.text = element_markdown(size = 5, face ="bold")
        ,legend.text = element_text(size = 5)
        , legend.position = "bottom"
        , axis.text.x = element_blank()
        , axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7, face="bold"),
        legend.title = element_text(size = 7)
        
      )
    
    # juntar
    
    boxplot_inequalities1 <- 
      # map2 + map3 + map4 &
      # (map1 | map2) / (map3 |map4) +
      (boxplot_inequalities11 / boxplot_inequalities12) +
      # plot_layout(nrow = 2) & 
      # plot_layout(nrow = 2, heights = c(3, 1)) & 
      plot_layout(heights = c(1, 1)) &
      theme(plot.title = element_markdown(size = 7),
            plot.subtitle = element_markdown(size = 8),
            legend.text = element_text(size = 6),
            plot.margin=unit(c(0,0,0,0),"mm"))
    
    
    ggsave(plot = boxplot_inequalities1, 
           filename = sprintf("figures/%s/access_inequalities/fig1_ineq1_%s_%s_%s.png", 
                              sigla_muni, sigla_muni, modo_acesso, variaveiss), 
           height = 10, width = 16, units = "cm", device = "png")
    
    
    
    # 
    # # 2) access inequalities boxplot 2
    # # teste: pegar locais que tiveram pelo menos 2% de melhoria no acesso
    # boxplot_inequalities2 <- go %>%
    #   filter(dif_rel >= 0.02) %>%
    #   ggplot()+
    #   geom_boxplot(aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
    #                outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
    #   # coord_flip()+
    #   facet_wrap(~ind, scale = "free_y", nrow = 3)+
    #   scale_y_continuous(label = percent)+
    #   theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
    #   scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
    #   
    #   coord_cartesian(ylim = c(0, NA))+
    #   geom_hline(yintercept = 0)+
    #   guides(color=guide_legend(nrow=1)) +
    #   theme( 
    #     panel.grid.minor = element_blank()
    #     ,strip.text = element_text(size = 5, face ="bold")
    #     ,legend.text = element_text(size = 5)
    #     , legend.position = "bottom"
    #     , axis.text.x = element_blank()
    #     , axis.text.y = element_text(size = 5),
    #     axis.title.x = element_text(size = 5),
    #     axis.title.y = element_text(size = 5, face="bold"),
    #     legend.title = element_text(size = 5)
    #     
    #   )
    # 
    # 
    # ggsave(plot = boxplot_inequalities2, 
    #        filename = sprintf("figures/%s/access_inequalities/fig1_ineq2_%s.png", sigla_muni, sigla_muni), 
    #        height = 12, width = 16, units = "cm", device = "png")
    # 
    # 
    # 
    # 
    # # 3) access inequalities scatterplot
    # 
    # baseplot1 <- theme_minimal() +
    #   theme( 
    #     axis.text.y  = element_text(face="bold", size=9)
    #     ,axis.text.x  = element_text(face="bold", size=9)
    #     ,panel.grid.minor = element_blank()
    #     ,strip.text = element_text(size = 11, face ="bold")
    #     ,legend.text = element_text(size = 11)
    #   )
    # 
    # scatterplot_inequalities <- ggplot()+
    #   geom_point(data = go, 
    #              aes(x = antes, y = depois, color = as.factor(quintil), size = pop_total/100),
    #              alpha = 0.4)+
    #   geom_abline()+
    #   scale_color_brewer(palette = "RdBu")+
    #   # coord_flip()+
    #   # theme_ipsum_rc(base_family = 'Helvetica')+
    #   facet_wrap(~ind, scale = "free")+
    #   labs(size = "Pop. total, milhares",
    #        color = "Quintil de renda")+
    #   baseplot1+
    #   guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=5))) +
    #   # guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
    #   theme(
    #     # Legends
    #     legend.position="bottom", # horz vert
    #     legend.direction='horizontal',
    #     legend.box='horizontal',
    #     legend.title=element_text(size=8),
    #     legend.text=element_text(size=7),
    #     legend.key.size = unit(.2, "cm"),
    #     legend.text.align=0.5,
    #     # axis
    #     axis.ticks=element_blank())
    # 
    # 
    # 
    # 
    # 
    # ggsave(plot = scatterplot_inequalities, 
    #        filename = sprintf("figures/%s/access_inequalities/fig2_scatterplot_%s.png", sigla_muni, sigla_muni), 
    #        height = 12, width = 16, units = "cm", device = "png")
    
    
  }
  
  # variaveis <- grep(pattern = "TT|ET|EI|EF|EM|ST|SB|SM|SA", x = colnames(access), value = TRUE)
  variaveis <- c("CMATT60", "CMAET60", "CMASB60", "CMASM60", "CMAEI60", "CMAEF60", "CMAEM60")
  
  lapply(variaveis, fazer_plots_acess_comp)
  
  
  
}





# apply function
compare_access('for', "WALK")
compare_access('for', "BICYCLE")

