options(stringsAsFactors = F) #necessary to convert the names in "method" column

bone_compr_s513_data <- read.csv("s513.csv",header = T)

bone_compr_s513_data$method[bone_compr_s513_data$method == "C" | bone_compr_s513_data$method == "G"] = "Experimental"
bone_compr_s513_data$method[bone_compr_s513_data$method == "D" | bone_compr_s513_data$method == "H"] = "Idealized"
bone_compr_s513_data$method[bone_compr_s513_data$method == "E" | bone_compr_s513_data$method == "I"] = "Generic"
bone_compr_s513_data$method[bone_compr_s513_data$method == "F" | bone_compr_s513_data$method == "J"] = "Specific"

attach(bone_compr_s513_data)
m1 <- mean(difference[which(method == "Experimental" & type == "CF")])
m2 <- mean(difference[which(method == "Idealized" & type == "CF")])
m3 <- mean(difference[which(method == "Generic" & type == "CF")])
m4 <- mean(difference[which(method == "Specific" & type == "CF")])
m5 <- mean(difference[which(method == "Experimental" & type == "vM")])
m6 <- mean(difference[which(method == "Idealized" & type == "vM")])
m7 <- mean(difference[which(method == "Generic" & type == "vM")])
m8 <- mean(difference[which(method == "Specific" & type == "vM")])

difference.m <- c(m1, m2, m3, m4, m5, m6, m7, m8)
type.m <- c("CF","CF","CF","CF","vM","vM","vM","vM")
method.m <- c("Experimental","Idealized","Generic","Specific","Experimental","Idealized","Generic","Specific")
cbind(type.m, method.m, difference.m )

fit1 <- aov(difference.m ~ type.m*method.m)
summary(fit1)




# Df Sum Sq Mean Sq F value   Pr(>F)    
# type            1    0.9   0.879   34.80 3.71e-09 ***
#   method          3   12.9   4.300  170.32  < 2e-16 ***
#   type:method     3    3.3   1.092   43.26  < 2e-16 ***
#   Residuals   19936  503.4   0.025                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

posthoc <- TukeyHSD(fit1, conf.level=0.95)
posthoc
#  Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = difference ~ type * method)

#$type
#diff         lwr        upr p adj
#vM-CF 0.01327555 0.008864718 0.01768639     0

#$method
#diff          lwr         upr     p adj
#Generic-Experimental    0.071459888  0.063283390  0.07963639 0.0000000
#Idealized-Experimental  0.035878379  0.027701882  0.04405488 0.0000000
#Specific-Experimental   0.042535058  0.034358561  0.05071156 0.0000000
#Idealized-Generic      -0.035581508 -0.043758006 -0.02740501 0.0000000
#Specific-Generic       -0.028924830 -0.037101327 -0.02074833 0.0000000
#Specific-Idealized      0.006656679 -0.001519819  0.01483318 0.1557889 non-significant

#$`type:method`
#diff          lwr          upr     p adj
#vM:Experimental-CF:Experimental  0.0437298034  0.030087393  0.057372214 0.0000000
#CF:Generic-CF:Experimental       0.1043163257  0.090673915  0.117958736 0.0000000
#vM:Generic-CF:Experimental       0.0823332531  0.068690843  0.095975664 0.0000000
#CF:Idealized-CF:Experimental     0.0424348576  0.028792447  0.056077268 0.0000000
#vM:Idealized-CF:Experimental     0.0730517048  0.059409294  0.086694115 0.0000000
#CF:Specific-CF:Experimental      0.0640306458  0.050388235  0.077673056 0.0000000
#vM:Specific-CF:Experimental      0.0647692740  0.051126864  0.078411684 0.0000000
#CF:Generic-vM:Experimental       0.0605865223  0.046944112  0.074228933 0.0000000
#vM:Generic-vM:Experimental       0.0386034497  0.024961039  0.052245860 0.0000000
#CF:Idealized-vM:Experimental    -0.0012949458 -0.014937356  0.012347465 0.9999921 non-significant
#vM:Idealized-vM:Experimental     0.0293219013  0.015679491  0.042964312 0.0000000
#CF:Specific-vM:Experimental      0.0203008424  0.006658432  0.033943253 0.0001752
#vM:Specific-vM:Experimental      0.0210394705  0.007397060  0.034681881 0.0000805
#vM:Generic-CF:Generic           -0.0219830726 -0.035625483 -0.008340662 0.0000287
#CF:Idealized-CF:Generic         -0.0618814681 -0.075523879 -0.048239058 0.0000000
#vM:Idealized-CF:Generic         -0.0312646209 -0.044907031 -0.017622210 0.0000000
#CF:Specific-CF:Generic          -0.0402856799 -0.053928090 -0.026643269 0.0000000
#vM:Specific-CF:Generic          -0.0395470517 -0.053189462 -0.025904641 0.0000000
#CF:Idealized-vM:Generic         -0.0398983955 -0.053540806 -0.026255985 0.0000000
#vM:Idealized-vM:Generic         -0.0092815483 -0.022923959  0.004360862 0.4397326 non-significant
#CF:Specific-vM:Generic          -0.0183026073 -0.031945018 -0.004660197 0.0012416
#vM:Specific-vM:Generic          -0.0175639791 -0.031206390 -0.003921569 0.0024253
#vM:Idealized-CF:Idealized        0.0306168472  0.016974437  0.044259258 0.0000000
#CF:Specific-CF:Idealized         0.0215957882  0.007953378  0.035238199 0.0000440
#vM:Specific-CF:Idealized         0.0223344164  0.008692006  0.035976827 0.0000193
#CF:Specific-vM:Idealized        -0.0090210590 -0.022663469  0.004621351 0.4787993 non-significant
#vM:Specific-vM:Idealized        -0.0082824308 -0.021924841  0.005359980 0.5923414 non-significant
#vM:Specific-CF:Specific          0.0007386282 -0.012903782  0.014381039 0.9999998 non-significant

plot(posthoc)

# Average/mean
# Interaction
# Main effects(type)
# Main effects(model)
# Tykey
