## Create a Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

> _NOTE:_ Issue on new Macs with M1 chips: "failed to solve with frontend dockerfile.v0", 
> Partial solution involves issuing the following commands at the command line:
> `export DOCKER_BUILDKIT=0`
> `export COMPOSE_DOCKER_CLI_BUILD=0`
>
> For now, don't build on machines with M1 chips.

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make production_build
```

You should then be able to see something like the following:

```
$ docker images | grep spatialutils
mazamascience/spatialutils        0.8.6     4742c9a4aaff   2 minutes ago   4.73GB
mazamascience/spatialutils        latest    4742c9a4aaff   2 minutes ago   4.73GB
...
```

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was obtained on July 28, 22020:

```
docker run -ti mazamascience/spatialutils R --vanilla
...
> library(MazamaSpatialUtils)
Loading required package: sp
> getCountryName(1:50,1:50)
 [1] NA           NA           "Nigeria"    "Nigeria"    "Nigeria"   
 [6] "Nigeria"    "Nigeria"    "Nigeria"    "Nigeria"    "Nigeria"   
[11] "Nigeria"    "Nigeria"    "Nigeria"    "Chad"       "Chad"      
[16] "Chad"       "Chad"       "Chad"       "Chad"       "Chad"      
[21] "Chad"       "Libya"      "Libya"      "Libya"      "Egypt"     
[26] "Egypt"      "Egypt"      "Egypt"      "Egypt"      "Egypt"     
[31] "Egypt"      "Egypt"      "Cyprus"     "Cyprus"     "Syria"     
[36] "Turkey"     "Turkey"     "Turkey"     "Turkey"     "Turkey"    
[41] "Turkey"     "Georgia"    "Georgia"    "Russia"     "Russia"    
[46] "Russia"     "Russia"     "Kazakhstan" "Kazakhstan" "Kazakhstan"
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

## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/spatialutils:0.8.6
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/spatialutils:0.8.6
```


