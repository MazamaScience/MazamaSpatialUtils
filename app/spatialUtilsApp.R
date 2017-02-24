
library(jug)
library(MazamaSpatialUtils)

setSpatialDataDir("~/data/Spatial")
initializeSpatialData()

jug() %>%
  
  # Countries
  get("/spatialUtils/getCountryCode", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getCountryCode(lons,lats, allData=TRUE)
    df$longitude <- round(lons,digits=2)
    df$latitude <- round(lats,digits=2)
    df <- df[,c('longitude','latitude','countryName','countryCode','ISO3','FIPS')]
    result <- readr::format_csv(df)
    
    res$content_type("text/plain")
    
    return(result)
    
  }) %>%
  
  # # READ MW - PLOT
  # get("/items/plot", function(req, res, err){
  #   db_plot<-
  #     db_list() %>%
  #     ggplot() +
  #     aes(x="", fill=checked) + 
  #     geom_bar(position="stack") +
  #     coord_flip() +
  #     xlab("") +
  #     scale_y_continuous(breaks=seq(0,100,1)) +
  #     theme(legend.position="none")
  #   
  #   res$plot(db_plot, height=100, width=300)
  #   
  # }) %>%
  # 
  # # READ MW - SPECIFIC items
  # get("/items/(?<description>.*)", function(req, res, err){
  #   
  #   res$json(db_get(req$params$description))
  #   
  # }) %>%
  # # CREATE/UPDATE MW
  # post("/items/(?<description>.*)", function(req, res, err){
  #   
  #   db_save(req$params$description, req$params$checked)
  #   return(TRUE)
  #   
  # }) %>%
  # # DELETE MW
  # delete("/items", function(req, res, err){
  #   
  #   db_delete(req$params$description)
  #   return(TRUE)
  #   
  # }) %>%
  # serve_static_files(root_path="public/") %>%

  simple_error_handler() %>%

  serve_it(verbose=TRUE)

# ----- END -------------------------------------------------------------------

