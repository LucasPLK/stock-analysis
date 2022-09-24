check_nans_by_indicator_and_year <- function(df, indicators, year_cols){
  
  qtd_nans_list = c()
  indicator_list = c()
  year_list = c()
  total_rows = c()
  
  for (ind in indicators){
    for (year in year_cols){
      total_rows <- nrow(df[df$Indicador == ind,])
      qtd_nans_list <- c(qtd_nans_list, c(sum(is.na(df[df$Indicador == ind, year]))))
      indicator_list <- c(indicator_list, ind)
      year_list <- c(year_list, year)
    }
  }
  
  return (data.frame (indicador  = indicator_list,
                      anos = year_list,
                      qtd_nulos = qtd_nans_list,
                      qtd_total = total_rows
  ))
}