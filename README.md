### ClusterChar

\#\#Aim Univariate and multivariate characterization of a clustering
process with ggplot2. Calculate partition evaluation measures.

\#\#Installation After unzipping the project. For install package in
local:

    setwd("C:/users/paupa")
    if(!require(devtools)){install.packages("devtools")}

    ## Loading required package: devtools

    ## Loading required package: usethis

    if(!require(roxygen2)){devtools::install_github("klutometis/roxygen")}

    ## Loading required package: roxygen2

    library(devtools)
    devtools::install("ClusterChar")

    ##          checking for file 'C:\Users\paupa\ClusterChar/DESCRIPTION' ...  v  checking for file 'C:\Users\paupa\ClusterChar/DESCRIPTION'
    ##       -  preparing 'ClusterChar': (782ms)
    ##    checking DESCRIPTION meta-information ...  v  checking DESCRIPTION meta-information
    ##       -  checking for LF line-endings in source and make files and shell scripts
    ##       -  checking for empty or unneeded directories
    ##       -  building 'ClusterChar_0.0.0.9000.tar.gz'
    ##      
    ## Running "C:/PROGRA~1/R/R-36~1.3/bin/x64/Rcmd.exe" INSTALL \
    ##   "C:\Users\paupa\AppData\Local\Temp\RtmpaA2ned/ClusterChar_0.0.0.9000.tar.gz" \
    ##   --install-tests 
    ## * installing to library 'D:/OneDrive/Documents/R/win-library/3.6'
    ## * installing *source* package 'ClusterChar' ...
    ## ** using staged installation
    ## ** R
    ## ** byte-compile and prepare package for lazy loading
    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when loading 'ClusterChar'
    ## Warning: replacing previous import 'creditmodel::log_trans' by 'scales::log_trans' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading 'ClusterChar'
    ## ** help
    ## *** installing help indices
    ##   converting help for package     finding HTML links ...'ClusterChar'
    ##  fini
    ##     Perform_UniChar                         html  
    ##     barplt.UniChar                          html  
    ##     circleFun                               html  
    ##     correlation                             html  
    ##     correlation.UniChar                     html  
    ##     correlation.default                     html  
    ##     correlation_circle                      html  
    ##     d                                       html  
    ##     effectsize                              html  
    ##     effectsize.UniChar                      html  
    ##     effectsize.default                      html  
    ##     indexes                                 html  
    ##     multivariate_charac                     html  
    ##     plot.UniChar                            html  
    ##     plot.list.corr                          html  
    ##     plot.measures                           html  
    ##     print.confusion.matrix                  html  
    ##     radar                                   html  
    ##     radar.UniChar                           html  
    ##     radar.default                           html  
    ##     sep_data                                html  
    ##     summary.multivariate                    html  
    ##     valuetest                               html  
    ##     valuetest.UniChar                       html  
    ##     valuetest.default                       html  
    ##     vcramer.UniChar                         html  
    ##     vtest.UniChar                           html  
    ## ** building package indices
    ## ** testing if installed package can be loaded from temporary location
    ## *** arch - i386
    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when loading 'ClusterChar'
    ## Warning: replacing previous import 'creditmodel::log_trans' by 'scales::log_trans' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading 'ClusterChar'
    ## *** arch - x64
    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when loading 'ClusterChar'
    ## Warning: replacing previous import 'creditmodel::log_trans' by 'scales::log_trans' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading 'ClusterChar'
    ## ** testing if installed package can be loaded from final location
    ## *** arch - i386
    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when loading 'ClusterChar'
    ## Warning: replacing previous import 'creditmodel::log_trans' by 'scales::log_trans' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading 'ClusterChar'
    ## *** arch - x64
    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when loading 'ClusterChar'
    ## Warning: replacing previous import 'creditmodel::log_trans' by 'scales::log_trans' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when loading 'ClusterChar'
    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading 'ClusterChar'
    ## ** testing if installed package keeps a record of temporary installation path
    ## * DONE (ClusterChar)

For install with git hub:

    if(!require(devtools)){install.packages("devtools")}
    devtools::install_github("PaulineLne/ClusterChar")

    ## Downloading GitHub repo PaulineLne/ClusterChar@HEAD

    ##          checking for file 'C:\Users\paupa\AppData\Local\Temp\RtmpaA2ned\remotes135c7ded3698\PaulineLne-ClusterChar-af3206a/DESCRIPTION' ...  v  checking for file 'C:\Users\paupa\AppData\Local\Temp\RtmpaA2ned\remotes135c7ded3698\PaulineLne-ClusterChar-af3206a/DESCRIPTION' (401ms)
    ##       -  preparing 'ClusterChar':
    ##    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
    ##       -  checking for LF line-endings in source and make files and shell scripts
    ##       -  checking for empty or unneeded directories
    ##       -  building 'ClusterChar_0.0.0.9000.tar.gz'
    ##      
    ## 

    ## Installing package into 'D:/OneDrive/Documents/R/win-library/3.6'
    ## (as 'lib' is unspecified)

    library(ClusterCharPlot)

    ## Warning: replacing previous import 'dplyr::combine' by 'gridExtra::combine' when
    ## loading 'ClusterCharPlot'

    ## Warning: replacing previous import 'creditmodel::log_trans' by
    ## 'scales::log_trans' when loading 'ClusterCharPlot'

    ## Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when
    ## loading 'ClusterCharPlot'

    ## Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading
    ## 'ClusterCharPlot'

    library(readxl)
    library(stats)

Préparation données
===================

    #Getting data from the sample in the package
    df<- read_excel('census_clustering.xlsx')


    #Traitement données
    y <- as.vector(t(df[1:1000,'cluster']))
    y_true <- as.vector(t(df[1:1000,'classe']))
    X<-df[0:1000,3:15]

    # #Recodage
    y[y=="cluster_1"] <- 1
    y[y=="cluster_2"] <- 2
    y[y=="cluster_3"] <- 3
    y[y=="cluster_4"] <- 4
    y[y=="cluster_5"] <- 5
    y <- as.numeric(y)
    #unclass(y)

Caractérisation univarié
------------------------

    #Création de l'objet de la classe UniChar à partir de notre jeu de données complet
    obj <- Perform_UniChar(X,y)
    plot(obj)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    #Analyse pour les données qualitative
    vCramer <- vcramer.UniChar(obj,y)
    print(vCramer)

    ##                  vCramer
    ## workclass      0.1832874
    ## education      0.4443688
    ## marital_status 0.5482172
    ## occupation     0.3529333
    ## relationship   0.5605546
    ## race           0.4324314
    ## sex            0.5538446
    ## native_country 0.4534297

    vCramer["label"] <- obj$catnames
    colnames(vCramer[1])

    ## [1] "vCramer"

    barplt.UniChar(obj, vCramer, TRUE)

    ##                  vCramer          label
    ## workclass      0.1832874      workclass
    ## education      0.4443688      education
    ## marital_status 0.5482172 marital_status
    ## occupation     0.3529333     occupation
    ## relationship   0.5605546   relationship
    ## race           0.4324314           race
    ## sex            0.5538446            sex
    ## native_country 0.4534297 native_country

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    vval <- vtest.UniChar(obj,y, 'education')
    vtest <- as.data.frame(t(vval))
    vtest["min",] <- min(vtest[1,])
    vtest["max",] <- max(vtest[1,])
    print(vtest)

    ##           Masters   7th-8th   HS-grad Bachelors Some-college Assoc-voc
    ## v.test  0.1004957  2.748384  1.841315 -1.007910    -2.118943 -2.651200
    ## min    -2.8282427 -2.828243 -2.828243 -2.828243    -2.828243 -2.828243
    ## max     4.6776699  4.677670  4.677670  4.677670     4.677670  4.677670
    ##        Assoc-acdm Doctorate Prof-school       11th      10th Preschool
    ## v.test  -1.803100  1.254853  -0.7438429 -0.7472182 -1.120019 -1.648238
    ## min     -2.828243 -2.828243  -2.8282427 -2.8282427 -2.828243 -2.828243
    ## max      4.677670  4.677670   4.6776699  4.6776699  4.677670  4.677670
    ##             12th   1st-4th       9th   5th-6th
    ## v.test -2.828243 -1.263315 -1.354979  4.677670
    ## min    -2.828243 -2.828243 -2.828243 -2.828243
    ## max     4.677670  4.677670  4.677670  4.677670

    radar(vtest)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    #Analyse pour les données 
    #Correlation, caracterisation des partitions
    cor <- correlation(obj,y)
    print(cor)

    ##             age       fnlwgt capital_gain capital_loss hours_per_week
    ## mk G 1 42.57586 181653.05172   439.234483    67.086207      44.389655
    ## mk G 2 44.18617 193328.39894  2821.505319   151.430851      43.638298
    ## mk G 3 43.88739 184465.71171   188.441441    69.774775      39.225225
    ## mk G 4 26.65233 199031.44803   176.494624    36.286738      35.666667
    ## mk G 5 37.61905 135529.80952   193.523810   283.047619      39.047619
    ## epl%   30.03268      1.00131     3.823688     1.681175       8.534209

    #Barplot de la correlation
    tmp <- as.data.frame(cor)
    (correlations <- t(tmp[6,]))

    ##                     epl%
    ## age            30.032679
    ## fnlwgt          1.001310
    ## capital_gain    3.823688
    ## capital_loss    1.681175
    ## hours_per_week  8.534209

    ClusterCharPlot::barplt.UniChar(obj, correlations)

    ##                     epl%          label
    ## age            30.032679            age
    ## fnlwgt          1.001310         fnlwgt
    ## capital_gain    3.823688   capital_gain
    ## capital_loss    1.681175   capital_loss
    ## hours_per_week  8.534209 hours_per_week

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    #Valeur test, caracterisation des groupes
    vt <- valuetest(obj,y)
    print(vt)

    ##                        age     fnlwgt capital_gain capital_loss hours_per_week
    ## Test value G 1   5.8184554 -1.2821505   -1.2367326   -0.6583550      6.2333549
    ## Test value G 2   6.1652328  0.7169947    6.1393232    2.8770059      3.7732406
    ## Test value G 3   6.4768039 -0.6217325   -1.8600321   -0.4309211     -1.8085488
    ## Test value G 4 -17.1505937  1.9893095   -2.2118884   -2.2329889     -7.7378998
    ## Test value G 5  -0.3386551 -2.3167220   -0.5053874    2.4776429     -0.5620414

    radar(vt)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    #Effect size, caracterisation des groupes
    es <- effectsize(obj,y)
    print(es)

    ##                                  age      fnlwgt capital_gain capital_loss
    ## Effect size G 1 vs other  0.41254685 -0.08942698  -0.08625424  -0.04589085
    ## Effect size G 2 vs other  0.50877323  0.05804579   0.50655099   0.23382561
    ## Effect size G 3 vs other  0.50352261 -0.04731743  -0.14177773  -0.03279229
    ## Effect size G 4 vs other -1.43989932  0.14053845  -0.15633650  -0.15783534
    ## Effect size G 5 vs other -0.07469325 -0.51232279  -0.11147533   0.54812209
    ##                          hours_per_week
    ## Effect size G 1 vs other      0.4431151
    ## Effect size G 2 vs other      0.3075937
    ## Effect size G 3 vs other     -0.1378404
    ## Effect size G 4 vs other     -0.5627145
    ## Effect size G 5 vs other     -0.1239755

    radar(es)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Carastérisation multivarié
--------------------------

    ### Sans y_true (ie sans confusionMatrix)

    y <- as.vector(t(df[1:1000,'cluster']))
    y_true <- as.vector(t(df[1:1000,'classe']))
    X <- df[1:1000,c("capital_gain", "capital_loss", "hours_per_week", "age","fnlwgt")]

    #Variables reelles
    multivariate=multivariate_charac(X,y)
    measures=indexes(X,y)

    print(multivariate$distance_matrix)

    ##              1            2            3            4            5
    ## 1      0.00000                                                    
    ## 2   2703.61731      0.00000                                       
    ## 3   2817.51815    269.45135      0.00000                          
    ## 4   2817.53342    270.37075     10.42232      0.00000             
    ## 5 401184.04646 402720.03458 402878.27109 402881.62471      0.00000

    print(multivariate$confusion_matrix)

    ## This confusion matrix is NULL. This may be due to the fact that you did not give y_true.

    print(multivariate$correlation)

    ## $All
    ##                      Comp.1       Comp.2
    ## capital_gain    0.001876903  0.999998239
    ## capital_loss   -0.007962398 -0.999968300
    ## hours_per_week  0.014602892  0.999893372
    ## age            -0.088004559  0.996120072
    ## fnlwgt          0.999998239 -0.001876933
    ## 
    ## $cluster_1
    ##                       Comp.1        Comp.2
    ## capital_gain    0.0002615324  0.9999999658
    ## capital_loss   -0.0075030491 -0.9999718517
    ## hours_per_week  0.0981396234 -0.9951726555
    ## age             0.0850168965 -0.9963795097
    ## fnlwgt         -0.9999999657  0.0002620929
    ## 
    ## $cluster_2
    ##                      Comp.1       Comp.2
    ## capital_gain    0.007817989  0.999969439
    ## capital_loss    0.073724504 -0.997278646
    ## hours_per_week  0.185903278  0.982568049
    ## age            -0.188221012  0.982126698
    ## fnlwgt          0.999969448 -0.007816829
    ## 
    ## $cluster_3
    ##                       Comp.1        Comp.2
    ## capital_gain    0.0004748323  0.9999998873
    ## capital_loss   -0.0285741040 -0.9995916769
    ## hours_per_week  0.0173366064  0.9998497097
    ## age            -0.0313509084 -0.9995084395
    ## fnlwgt          0.9999998855 -0.0004785955
    ## 
    ## $cluster_4
    ##                       Comp.1        Comp.2
    ## capital_gain    0.0004921804  0.9999998789
    ## capital_loss    0.0131998327 -0.9999128784
    ## hours_per_week -0.0101514468 -0.9999484727
    ## age            -0.0104064519 -0.9999458514
    ## fnlwgt          0.9999998789 -0.0004921491
    ## 
    ## $cluster_5
    ##                      Comp.1       Comp.2
    ## capital_gain    0.001122572  0.999999370
    ## capital_loss    0.001152801 -0.999999336
    ## hours_per_week  0.104105080 -0.994566304
    ## age             0.013918634  0.999903131
    ## fnlwgt         -0.999999477  0.001022555
    ## 
    ## attr(,"class")
    ## [1] "list.corr"

    print(multivariate$squared_correlation)

    ## $All
    ##                        Comp.1                  CTR1         Comp.2
    ## capital_gain   0.000003522765 0.0000000000003165358 0.999996477235
    ## capital_loss   0.000063399779 0.0000000000056967466 0.999936600221
    ## hours_per_week 0.000213244454 0.0000000000191609442 0.999786755546
    ## age            0.007744802446 0.0000000006959042773 0.992255197554
    ## fnlwgt         0.999996477124 0.0000000898540447713 0.000003522876
    ##                              CTR2
    ## capital_gain   0.0000381788447820
    ## capital_loss   0.0000381765587387
    ## hours_per_week 0.0000381708378220
    ## age            0.0000378832906255
    ## fnlwgt         0.0000000001344998
    ## 
    ## $cluster_1
    ##                          Comp.1                    CTR1           Comp.2
    ## capital_gain   0.00000006839918 0.000000000000006479321 0.99999993160082
    ## capital_loss   0.00005629574556 0.000000000005332786382 0.99994370425444
    ## hours_per_week 0.00963138568454 0.000000000912362415832 0.99036861431546
    ## age            0.00722787268529 0.000000000684682308493 0.99277212731471
    ## fnlwgt         0.99999993130732 0.000000094728046725828 0.00000006869268
    ##                              CTR2
    ## capital_gain   0.0002906583792386
    ## capital_loss   0.0002906420362881
    ## hours_per_week 0.0002878589559750
    ## age            0.0002885575572156
    ## fnlwgt         0.0000000000199661
    ## 
    ## $cluster_2
    ##                       Comp.1                 CTR1        Comp.2
    ## capital_gain   0.00006112095 0.000000000005210114 0.99993887905
    ## capital_loss   0.00543530245 0.000000000463319771 0.99456469755
    ## hours_per_week 0.03456002883 0.000000002945989625 0.96543997117
    ## age            0.03542714931 0.000000003019905303 0.96457285069
    ## fnlwgt         0.99993889719 0.000000085237475683 0.00006110281
    ##                              CTR2
    ## capital_gain   0.0000083027265736
    ## capital_loss   0.0000082581034867
    ## hours_per_week 0.0000080162740662
    ## age            0.0000080090741618
    ## fnlwgt         0.0000000005073509
    ## 
    ## $cluster_3
    ##                         Comp.1                   CTR1          Comp.2
    ## capital_gain   0.0000002254657 0.00000000000002044044 0.9999997745343
    ## capital_loss   0.0008164794168 0.00000000007402100112 0.9991835205832
    ## hours_per_week 0.0003005579202 0.00000000002724820453 0.9996994420798
    ## age            0.0009828794586 0.00000000008910662045 0.9990171205414
    ## fnlwgt         0.9999997709464 0.00000009065872652187 0.0000002290536
    ##                             CTR2
    ## capital_gain   0.000898178274660
    ## capital_loss   0.000897445132929
    ## hours_per_week 0.000897908522513
    ## age            0.000897295675993
    ## fnlwgt         0.000000000205731
    ## 
    ## $cluster_4
    ##                         Comp.1                   CTR1          Comp.2
    ## capital_gain   0.0000002422415 0.00000000000002105765 0.9999997577585
    ## capital_loss   0.0001742355838 0.00000000001514600594 0.9998257644162
    ## hours_per_week 0.0001030518724 0.00000000000895812576 0.9998969481276
    ## age            0.0001082942420 0.00000000000941383612 0.9998917057580
    ## fnlwgt         0.9999997577893 0.00000008692829523191 0.0000002422107
    ##                              CTR2
    ## capital_gain   0.0002341671398321
    ## capital_loss   0.0002341263962989
    ## hours_per_week 0.0002341430651890
    ## age            0.0002341418375980
    ## fnlwgt         0.0000000000567178
    ## 
    ## $cluster_5
    ##                        Comp.1                  CTR1         Comp.2
    ## capital_gain   0.000001260168 0.0000000000002856821 0.999998739832
    ## capital_loss   0.000001328951 0.0000000000003012754 0.999998671049
    ## hours_per_week 0.010837867706 0.0000000024569623555 0.989162132294
    ## age            0.000193728378 0.0000000000439185406 0.999806271622
    ## fnlwgt         0.999998954382 0.0000002267014004141 0.000001045618
    ##                             CTR2
    ## capital_gain   0.001323195566739
    ## capital_loss   0.001323195475725
    ## hours_per_week 0.001308856597617
    ## age            0.001322940893335
    ## fnlwgt         0.000000001383559
    ## 
    ## attr(,"class")
    ## [1] "list.corr_carr"

    plot(multivariate$correlation)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    #Printing and plotting the results of indexes
    print(measures$internal)

    ## $Silhouette
    ## [1] 0.003615629
    ## 
    ## $Dunn
    ## [1] 0.00000223986
    ## 
    ## $eta_sq
    ## [1] 0.01007956
    ## 
    ## attr(,"class")
    ## [1] "internal_ind"

    print(measures$external)

    ## [1] 0
    ## attr(,"class")
    ## [1] "external_ind"

    plot(measures)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-11-2.png)

    # ACP
    y <- as.vector(t(df[1:1000,'cluster']))
    y_true <- as.vector(t(df[1:1000,'classe']))
    #Seulement les premières variables
    X <- df[1:1000, 16:20]

    multivariate=multivariate_charac(X,y)
    measures=indexes(X,y)

    print(multivariate$distance_matrix)

    ##          1        2        3        4        5
    ## 1 0.000000                                    
    ## 2 3.899361 0.000000                           
    ## 3 3.844664 3.852766 0.000000                  
    ## 4 7.929744 6.259506 8.578183 0.000000         
    ## 5 4.095641 3.807393 3.035782 9.469178 0.000000

    print(multivariate$confusion_matrix)

    ## This confusion matrix is NULL. This may be due to the fact that you did not give y_true.

    print(multivariate$correlation)

    ## $All
    ##        Comp.1     Comp.2
    ## Z1  0.9674688  0.2529903
    ## Z2  0.2669393 -0.9637133
    ## Z3 -0.4028098  0.9152837
    ## Z4 -0.1148171 -0.9933866
    ## Z5 -0.0565012 -0.9984025
    ## 
    ## $cluster_1
    ##        Comp.1     Comp.2
    ## Z1  0.8888125  0.4582710
    ## Z2 -0.7197095  0.6942753
    ## Z3  0.8677796 -0.4969492
    ## Z4  0.9098918  0.4148456
    ## Z5  0.6031092  0.7976586
    ## 
    ## $cluster_2
    ##         Comp.1     Comp.2
    ## Z1  0.90925720  0.4162347
    ## Z2  0.66359901 -0.7480885
    ## Z3  0.56023081 -0.8283366
    ## Z4 -0.18904963 -0.9819675
    ## Z5  0.01192676 -0.9999289
    ## 
    ## $cluster_3
    ##        Comp.1     Comp.2
    ## Z1  0.9904263 0.13804268
    ## Z2 -0.6612549 0.75016133
    ## Z3  0.6937809 0.72018609
    ## Z4  0.9976004 0.06923453
    ## Z5  0.6162001 0.78758964
    ## 
    ## $cluster_4
    ##        Comp.1     Comp.2
    ## Z1  0.9945849  0.1039275
    ## Z2 -0.8841010 -0.4672958
    ## Z3  0.3651661 -0.9309424
    ## Z4  0.6673663 -0.7447297
    ## Z5  0.4083724 -0.9128154
    ## 
    ## $cluster_5
    ##        Comp.1     Comp.2
    ## Z1  0.3830959  0.9237086
    ## Z2 -0.3631439 -0.9317331
    ## Z3  0.5560502  0.8311487
    ## Z4 -0.9533220  0.3019556
    ## Z5  0.2057992 -0.9785942
    ## 
    ## attr(,"class")
    ## [1] "list.corr"

    print(multivariate$squared_correlation)

    ## $All
    ##         Comp.1       CTR1     Comp.2      CTR2
    ## Z1 0.935995914 312.957354 0.06400409  26.54903
    ## Z2 0.071256595  23.825185 0.92874340 385.24479
    ## Z3 0.162255702  54.251428 0.83774430 347.49816
    ## Z4 0.013182966   4.407825 0.98681703 409.33386
    ## Z5 0.003192385   1.067398 0.99680761 413.47798
    ## 
    ## $cluster_1
    ##       Comp.1     CTR1    Comp.2     CTR2
    ## Z1 0.7899877 361.2626 0.2100123 323.8647
    ## Z2 0.5179818 236.8739 0.4820182 743.3310
    ## Z3 0.7530415 344.3671 0.2469585 380.8401
    ## Z4 0.8279031 378.6014 0.1720969 265.3945
    ## Z5 0.3637407 166.3392 0.6362593 981.1896
    ## 
    ## $cluster_2
    ##          Comp.1         CTR1    Comp.2     CTR2
    ## Z1 0.8267486555 428.57036873 0.1732513 163.7502
    ## Z2 0.4403636505 228.27592256 0.5596363 528.9459
    ## Z3 0.3138585567 162.69815073 0.6861414 648.5135
    ## Z4 0.0357397619  18.52679509 0.9642602 911.3802
    ## Z5 0.0001422476   0.07373837 0.9998578 945.0256
    ## 
    ## $cluster_3
    ##       Comp.1     CTR1     Comp.2       CTR2
    ## Z1 0.9809442 647.1777 0.01905578  18.261413
    ## Z2 0.4372580 288.4808 0.56274202 539.283266
    ## Z3 0.4813320 317.5587 0.51866801 497.046547
    ## Z4 0.9952066 656.5873 0.00479342   4.593599
    ## Z5 0.3797026 250.5087 0.62029745 594.439408
    ## 
    ## $cluster_4
    ##       Comp.1     CTR1     Comp.2      CTR2
    ## Z1 0.9891991 760.1094 0.01080092  12.18614
    ## Z2 0.7816346 600.6150 0.21836538 246.37070
    ## Z3 0.1333463 102.4645 0.86665371 977.80187
    ## Z4 0.4453777 342.2322 0.55462226 625.75244
    ## Z5 0.1667680 128.1460 0.83323198 940.09381
    ## 
    ## $cluster_5
    ##        Comp.1       CTR1     Comp.2      CTR2
    ## Z1 0.14676245  31.853089 0.85323755 204.56486
    ## Z2 0.13187351  28.621618 0.86812649 208.13450
    ## Z3 0.30919188  67.106515 0.69080812 165.62218
    ## Z4 0.90882284 197.249468 0.09117716  21.85985
    ## Z5 0.04235331   9.192296 0.95764669 229.59709
    ## 
    ## attr(,"class")
    ## [1] "list.corr_carr"

    plot(multivariate$correlation)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    #Printing and plotting the results of indexes
    print(measures$internal)

    ## $Silhouette
    ## [1] 0.3469309
    ## 
    ## $Dunn
    ## [1] 0.0206825
    ## 
    ## $eta_sq
    ## [1] 0.5966502
    ## 
    ## attr(,"class")
    ## [1] "internal_ind"

    print(measures$external)

    ## [1] 0
    ## attr(,"class")
    ## [1] "external_ind"

    plot(measures)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-12-2.png)

    #Avec y=y_true
    y <- as.vector(t(df[1:1000,'cluster']))
    y_true <- as.vector(t(df[1:1000,'classe']))
    X <- df[1:1000,c("capital_gain", "capital_loss", "hours_per_week", "age","fnlwgt")]
    multivariate=multivariate_charac(X,y_true)
    measures=indexes(X,y_true)

    print(multivariate$distance_matrix)

    ##        1      2
    ## 1      0       
    ## 2 270368      0

    print(multivariate$confusion_matrix)

    ## This confusion matrix is NULL. This may be due to the fact that you did not give y_true.

    print(multivariate$correlation)

    ## $All
    ##                      Comp.1       Comp.2
    ## capital_gain    0.001876903  0.999998239
    ## capital_loss   -0.007962398 -0.999968300
    ## hours_per_week  0.014602892  0.999893372
    ## age            -0.088004559  0.996120072
    ## fnlwgt          0.999998239 -0.001876933
    ## 
    ## $less
    ##                       Comp.1        Comp.2
    ## capital_gain    0.0001317375  0.9999999913
    ## capital_loss   -0.0136068029 -0.9999074232
    ## hours_per_week  0.0007143242 -0.9999997449
    ## age            -0.0547833093  0.9984982669
    ## fnlwgt          0.9999999913 -0.0001321543
    ## 
    ## $more
    ##                      Comp.1       Comp.2
    ## capital_gain    0.005016727  0.999987416
    ## capital_loss    0.023921766 -0.999713834
    ## hours_per_week -0.013659046  0.999906711
    ## age            -0.458010781  0.888946638
    ## fnlwgt          0.999987418 -0.005016295
    ## 
    ## attr(,"class")
    ## [1] "list.corr"

    print(multivariate$squared_correlation)

    ## $All
    ##                        Comp.1                  CTR1         Comp.2
    ## capital_gain   0.000003522765 0.0000000000003165358 0.999996477235
    ## capital_loss   0.000063399779 0.0000000000056967466 0.999936600221
    ## hours_per_week 0.000213244454 0.0000000000191609442 0.999786755546
    ## age            0.007744802446 0.0000000006959042773 0.992255197554
    ## fnlwgt         0.999996477124 0.0000000898540447713 0.000003522876
    ##                              CTR2
    ## capital_gain   0.0000381788447820
    ## capital_loss   0.0000381765587387
    ## hours_per_week 0.0000381708378220
    ## age            0.0000378832906255
    ## fnlwgt         0.0000000001344998
    ## 
    ## $less
    ##                          Comp.1                    CTR1           Comp.2
    ## capital_gain   0.00000001735478 0.000000000000001611651 0.99999998264522
    ## capital_loss   0.00018514508562 0.000000000017193490156 0.99981485491438
    ## hours_per_week 0.00000051025903 0.000000000000047385182 0.99999948974097
    ## age            0.00300121097736 0.000000000278707324158 0.99699878902264
    ## fnlwgt         0.99999998253523 0.000000092864953977790 0.00000001746477
    ##                                CTR2
    ## capital_gain   0.000508697723883619
    ## capital_loss   0.000508603549826679
    ## hours_per_week 0.000508697473144347
    ## age            0.000507171023492383
    ## fnlwgt         0.000000000008884287
    ## 
    ## $more
    ##                       Comp.1                 CTR1        Comp.2
    ## capital_gain   0.00002516755 0.000000000002048755 0.99997483245
    ## capital_loss   0.00057225088 0.000000000046583877 0.99942774912
    ## hours_per_week 0.00018656955 0.000000000015187627 0.99981343045
    ## age            0.20977387515 0.000000017076566670 0.79022612485
    ## fnlwgt         0.99997483678 0.000000081402591039 0.00002516322
    ##                              CTR2
    ## capital_gain   0.0000091970573834
    ## capital_loss   0.0000091920257000
    ## hours_per_week 0.0000091955729226
    ## age            0.0000072679379323
    ## fnlwgt         0.0000000002314334
    ## 
    ## attr(,"class")
    ## [1] "list.corr_carr"

    plot(multivariate$correlation)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    #Printing and plotting the results of indexes
    print(measures$internal)

    ## $Silhouette
    ## [1] 0.0742224
    ## 
    ## $Dunn
    ## [1] 0
    ## 
    ## $eta_sq
    ## [1] 0.003239003
    ## 
    ## attr(,"class")
    ## [1] "internal_ind"

    print(measures$external)

    ## [1] 0
    ## attr(,"class")
    ## [1] "external_ind"

    plot(measures)

![](tutorial_census_files/figure-markdown_strict/unnamed-chunk-13-2.png)

    # ### Avec y et y_true
    # y <- as.vector(t(df[1:1000,'cluster']))
    # y_true <- as.vector(t(df[1:1000,'classe']))
    # X <- df[1:1000,16:25]
    # 
    # #  Erreur : `data` and `reference` should be factors with the same levels.
    # multivariate=multivariate_charac(X,y,y_true)
    # measures=indexes(X,y,y_true)
