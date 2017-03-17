## Create a Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Dockerfile` in this directory:

```
$ docker build -t spatialutils:v0.4.9 .
$ docker tag spatialutils:v0.4.9 spatialutils:latest
$ docker images
REPOSITORY                   TAG                 IMAGE ID            CREATED             SIZE
spatialutils                 latest              be5b4f8f8ca5        27 seconds ago      1.19 GB
spatialutils                 v0.4.9              be5b4f8f8ca5        27 seconds ago      1.19 GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/spatialutils
```

In the example below, you should replace ```spatialutils``` with ```mazamascience/spatialutils``` if you pulled from DockerHub.

## Docker Run ##

Having built the docker image we can now test it with:

```
docker run -ti spatialutils R --vanilla
...
> getCountryName(1:50,1:50)
 [1] NA                       NA                       NA                      
 [4] NA                       NA                       "Nigeria"               
 [7] "Nigeria"                "Nigeria"                "Nigeria"               
[10] "Nigeria"                "Nigeria"                "Nigeria"               
[13] "Nigeria"                "Chad"                   "Chad"                  
[16] "Chad"                   "Chad"                   "Chad"                  
[19] "Chad"                   "Chad"                   "Libyan Arab Jamahiriya"
[22] "Libyan Arab Jamahiriya" "Libyan Arab Jamahiriya" "Libyan Arab Jamahiriya"
[25] "Egypt"                  "Egypt"                  "Egypt"                 
[28] "Egypt"                  "Egypt"                  "Egypt"                 
[31] "Egypt"                  NA                       NA                      
[34] NA                       NA                       "Turkey"                
[37] "Turkey"                 "Turkey"                 "Turkey"                
[40] "Turkey"                 "Turkey"                 "Georgia"               
[43] "Georgia"                "Russia"                 "Russia"                
[46] "Russia"                 "Russia"                 "Kazakhstan"            
[49] "Kazakhstan"             "Kazakhstan"            
> setSpatialDataDir('/home/mazama/data/Spatial')
> loadSpatialData('NaturalEarthAdm1')
> getStateName(1:50,1:50)
 [1] NA                       NA                       NA                      
 [4] NA                       NA                       "Edo"                   
 [7] "Kogi"                   "Nassarawa"              "Nassarawa"             
[10] "Bauchi"                 "Gombe"                  "Yobe"                  
[13] "Borno"                  "Lac"                    "Kanem"                 
[16] "Kanem"                  "Borkou"                 "Borkou"                
[19] "Borkou"                 "Borkou"                 "Al Kufrah"             
[22] "Al Kufrah"              "Al Kufrah"              "Al Kufrah"             
[25] "Al Wadi at Jadid"       "Al Wadi at Jadid"       "Al Wadi at Jadid"      
[28] "Al Jizah"               "Matruh"                 "Matruh"                
[31] "Al Gharbiyah"           NA                       NA                      
[34] NA                       NA                       "Hatay"                 
[37] "Gaziantep"              "Malatya"                "Tunceli"               
[40] "Erzincan"               "Rize"                   "Guria"                 
[43] "Samegrelo-Zemo Svaneti" "Stavropol'"             "Stavropol'"            
[46] "Kalmyk"                 "Astrakhan'"             "Atyrau"                
[49] "West Kazakhstan"        "West Kazakhstan"       
```

