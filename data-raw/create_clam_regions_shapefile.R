## code to prepare `DATASET` dataset goes here


create_clam_regions_shapefile <- function() {

  polygonsSean <- sf::st_read(dsn=here::here("data-raw","gis","shellfish_strata.shp"))

  df <- data.frame(STRATUM=c(6010:6080, 6800, 6810),Region=rep("SVA",length(SVA)))
  df <- rbind(df,data.frame(STRATUM=c(6090:6160, 6820:6860),Region=rep("DMV",length(DMV))))
  df <- rbind(df,data.frame(STRATUM=c(6170:6200, 6870),Region=rep("SNJ",length(SNJ))))
  df <- rbind(df,data.frame(STRATUM=c(6210:6280, 6880:6900),Region=rep("NNJ",length(NNJ))))
  df <- rbind(df,data.frame(STRATUM=c(6290:6360, 6910:6930),Region=rep("LI",length(LI))))
  df <- rbind(df,data.frame(STRATUM=c(6370:6520, 6940:6960),Region=rep("SNE",length(SNE))))
  df <- rbind(df,data.frame(STRATUM=c(6530:6740),Region=rep("GB",length(GB))))

  shellfish_strata <- dplyr::left_join(polygonsSean,df,by="STRATUM")

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data=shellfish_strata, ggplot2::aes(fill = Region))
  p

  sf::st_write(shellfish_strata,dsn=here::here("data-raw","shellfish_strata.shp"),driver="ESRI Shapefile")

}




