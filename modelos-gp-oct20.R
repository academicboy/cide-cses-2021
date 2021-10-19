#####################################
#Presentación en Política Y Gobierno#
#####################################


######3Paquetes
library(pacman)
p_load(foreign, haven,dplyr,ggplot2, lme4)##Cargar con un solo comando todas las demÃ¡s librerias
rm(list=ls(all=TRUE))
options(scipen = 999)
base <- read_dta("cses-2021.dta")
#######Manejo de etiquetas en datos en formato dta (STATA)
datos <- base

base<- base %>% 
  mutate_at(vars(-SATDEM_2021,-info_escala, -votoprev, 
                 -gustopart_imput, -confianza_total, -SEXO, -EVALUADEM, -APOYODEM),haven::as_factor) %>%            
  purrr::set_names(base %>%      
                     sjlabelled::get_label() %>% 
                     tibble::enframe() %>%               
                     na_if("") %>%             
                     mutate(value = coalesce(value, name)) %>%  
                     pull(value)) %>%            
  janitor::clean_names()
names(base) <- names(datos)
head(base)
rm(base)

####Con los datos en la forma correcta procedemos a ajustar los modelos
######################################################################
####Priemra parte: modelos logísticos simples
modelo_1<- glm(SATDEM_2021~gustopart_imput+P19+P21+confianza_total+Edad+ESCOLARIDAD, family=binomial("logit"), na.action=na.omit,data=base)
modelo_2<- glm(APOYODEM~gustopart_imput+P19+P21+confianza_total+Edad+ESCOLARIDAD,family=binomial("logit"), na.action=na.omit,data=base)
modelo_3<- glm(EVALUADEM~gustopart_imput+P19+P21+confianza_total+Edad+ESCOLARIDAD, family=binomial("logit"), na.action=na.omit,data=base)
modelo_4<- glm(confianza_INE~gustopart_imput+P19+P21+APOYODEM+Edad+ESCOLARIDAD, family=binomial("logit"), na.action=na.omit,data=base)

##Obtenemos un reporte con los resultados de los cuatro modelos y 
###creamos algunas gráficas interesantes 
export_summs(modelo_1, modelo_2,modelo_3,modelo_4, robust=T,to.file = "docx", digits=3,file.name = "simpler-logistic-models-robust.docx")

plot_model(modelo_1, transform = "plogis",ci.lvl = 0.95,
           colors='black',show.values=F,dot.size =5) +
  coord_flip()+
  scale_y_continuous(limits = c(0,1),breaks = c(0.0,0.5,1))+
  labs(title= " ",
       subtitle="PROBABILIDADES ESTIMADAS POR CATEGORÍA EN CADA VARIABLE [95% CI]",
       x="", y="Pr(y=1)", caption = "\nFuente: Encuesta CIDE-CSES 2021") 
dev.off()
ggsave(filename = "plot.png",width = 15, height = 10, dpi = 100)
#####################################################################
#####Parte 2: modelos multinivel

modelo_1= glmer(SATDEM_2021 ~ gustopart_imput+P19+P21+confianza_total +(1|S2)+(1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

modelo_2= glmer(APOYODEM ~ gustopart_imput+P19+P21+confianza_total +(1|S2)+(1|Edad) +(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

modelo_3= glmer(EVALUADEM ~ gustopart_imput+P19+P21+ confianza_total +(1|S2)+ (1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)


modelo_4= glmer(confianza_INE~gustopart_imput+P19+P21+(1|S2) + (1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

##Reporte con los resultados de los cuatro modelos
export_summs(modelo_1, modelo_2,modelo_3,modelo_4, robust=T,to.file = "docx", digits=3,file.name = "multilevel-logistic-models-robust.docx")


###Gráficas para diferentes estimados
# table of estimates with 95% CI
(tab <- cbind(LL = fixef(modelo_1) - 1.96 * se,Est = fixef(modelo_1),UL = fixef(modelo_1) + 1.96 *se))

##
plot_model(modelo_1, transform = "plogis",ci.lvl = 0.95,
           colors='black',show.values=F,dot.size =5) +
  coord_flip()+
  scale_y_continuous(limits = c(0,1),breaks = c(0.0,0.5,1))+
  labs(title= "SATISFACCIÓN CON LA DEMCORACIA",
       subtitle="PROBABILIDADES ESTIMADAS POR CATEGORÍA EN CADA VARIABLE [95% CI]",
       x="", y="Pr(y=1)", caption = "\nFuente: Encuesta CIDE-CSES 2021")


##################################
####Parte 3: modelo smultinivel con post estratificación (MrP)
###La idea de este técnica consiste en modelar las respuestas individuales como 
##una función de sus características individuales (efectso aleatorios)

####Aquí se decidió incluir algo de contexto (efectos fijos) 
modelo_1= glmer(SATDEM_2021 ~ P19+(1|S2)+(1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

modelo_2= glmer(APOYODEM ~ P19 +(1|S2)+(1|Edad) +(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

modelo_3= glmer(EVALUADEM ~ P19 +(1|S2)+ (1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)


modelo_4= glmer(confianza_INE~P19 +(1|S2) + (1|Edad)+(1|ESCOLARIDAD)+(1|estado),
                data= base, family=binomial("logit"), nAGQ=1,na.action=na.omit)

#########Aquí obtenemos los efectos aleatorios por estado. 
###Como no todos los estados est+an en la muetsra de la entrevista, volvemos cero los efectos de lo estados que no están. 
lev.estados <- levels(base$estado)
lev.estados <- as.data.frame(lev.estados)
lev.estados$order <- rep(1:32)

state.ranefs <- array(NA,c(32,1))
dimnames(state.ranefs) <- list(c(lev.estados$lev.estados),"effect")
for(i in lev.estados$lev.estados){
  state.ranefs[i,1] <- ranef(modelo_4)$estado[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0
state.ranefs

#######Extraemos los efectos aleatorios del resto de sociodemográficos
re.sexo = ranef(modelo_1)$S2[[1]]
re.edad = ranef(modelo_1)$Edad[[1]]
re.educ=ranef(modelo_1)$ESCOLARIDAD[[1]]

sex.re <- rep(re.sexo,16)
edad.re <- rep(kronecker(re.edad,c(1,1)), 4)
educ.re <- kronecker(re.educ,rep(1, 8))
ind.re <- rowSums(cbind(sex.re, edad.re, educ.re))
ind.re <- ind.re+fixef(modelo_4)[1]
beta1 <- fixef(modelo_4)[2:5]

######Extraemos las probabilidades individuales
y.lat2 <- rep(NA,1800)
for (i in 1:32){
  a <- ((i-1)*32)+1
  b <- a + 31
  y.lat2[a:b] <- ind.re + beta1*P_19[i]+state.ranefs[i]
}

p3 <- invlogit(y.lat2)
####Para realizar la post estratificación se necesitan las cantidades 
###por grupos sociodemográficos (aquí tegno el % de cada grupo)
estratos <- readxl::read_xlsx("estratos nacionales.xlsx")

estratos = as.matrix.data.frame(estratos)
a<- c(estratos)
# Estimados por estado
mrp.datos <- rep(NA,32)
for(i in 1:32){
  a1 <- ((i-1)*32)+1
  a2 <- a1 + 31
  p3 <- invlogit(y.lat2[a1:a2])
  a <- estratos[,i]
  mrp.datos[i] <- sum(p3*a)/sum(a)
}

mrp.datos###Probabilidades del modelo 1 (Satisfacción con la democracia)
satdem.prb <- as.data.frame(mrp.datos)
satdem.prb$estado <- lev.estados$lev.estados
write.csv(satdem.prb, "confine-probabilidades-estados-cont.csv")
