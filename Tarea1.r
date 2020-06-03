library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

pob_chi <- 18730000
pob_chi_minsal <- 19458310
pob_arg <- 44490000
pob_bol <- 11350000
pob_per <- 31990000

df_covid_time <- read.csv("time_series_covid19_confirmed_global.csv",sep=",",check.names=FALSE)
df_covid_deaths <- read.csv("time_series_covid19_deaths_global.csv",sep=",",check.names=FALSE)
df_covid_recovered <- read.csv("time_series_covid19_recovered_global.csv",sep=",",check.names=FALSE)

head(df_covid_time,n=5)

df_chi <- df_covid_time[df_covid_time$"Country/Region"=='Chile',]
df_chi_deaths <- df_covid_deaths[df_covid_deaths$"Country/Region"=='Chile',]
df_chi_recovered <- df_covid_recovered[df_covid_recovered$"Country/Region"=='Chile',]

df_arg <- df_covid_time[df_covid_time$"Country/Region"=='Argentina',]
df_arg_deaths <-df_covid_deaths[df_covid_deaths$"Country/Region"=='Argentina',]
df_arg_recovered <-df_covid_recovered[df_covid_recovered$"Country/Region"=='Argentina',]

df_bol <- df_covid_time[df_covid_time$"Country/Region"=='Bolivia',]
df_bol_deaths <-df_covid_deaths[df_covid_deaths$"Country/Region"=='Bolivia',]
df_bol_recovered <-df_covid_recovered[df_covid_recovered$"Country/Region"=='Bolivia',]

df_per <- df_covid_time[df_covid_time$"Country/Region"=='Peru',]
df_per_deaths <-df_covid_deaths[df_covid_deaths$"Country/Region"=='Peru',]
df_per_recovered <-df_covid_recovered[df_covid_recovered$"Country/Region"=='Peru',]

sort_df <- function(df_c,df_d,df_r){
    df_c <-select(df_c, -c("Province/State","Lat","Long"))
    df_d <- select(df_d, -c("Province/State","Lat","Long"))
    df_r <-select(df_r, -c("Province/State","Lat","Long"))
    colnames(df_c)[1]<- 'Pais'
    colnames(df_d)[1]<- 'Pais'
    colnames(df_r)[1]<- 'Pais'
    df_c <- gather(df_c,c(2:ncol(df_c)),key='Fecha',value='Casos')
    df_d <- gather(df_d,c(2:ncol(df_d)),key='Fecha',value='Muertos')
    df_r <- gather(df_r,c(2:ncol(df_r)),key='Fecha',value='Recuperados')
    
    return(cbind(df_c,df_d[3],df_r[3]))
}

add_nc <- function(df){
    new_cases <- rep(0,length(df$Casos))
    for(i in c(2:length(df$Casos))){
        new_cases[i] <- df$Casos[i]-df$Casos[i-1]
        }
    return(new_cases)
    }

add_params <- function(df,pob){
    #Consideramos desde el 2do caso en adelante
    df <- df[df$Casos>=2,]
    df <- cbind(c(1:nrow(df)),df)
    colnames(df)[1] <- 'Dias'
    df$"Casos_Nuevos" <- add_nc(df)
    #Agregamos la tasa de incidencia
    df$"Tasa_Incidencia" <- df$Casos*(100000/pob)
    df$"Tasa_Incidencia_dia" <- df$Casos_Nuevos*(100000/pob)
    df$"Tasa_Mortalidad" <- df$Muertos*(100000/pob)
    #Convertimos la fecha a Date, para trabajarla de mejor manera
    df$Fecha <- as.Date(df$Fecha, format = "%m/%d/%y")
    
    return(df)
}

df_chi <- sort_df(df_chi,df_chi_deaths,df_chi_recovered)
df_chi <- add_params(df_chi,pob_chi)

df_arg <- sort_df(df_arg,df_arg_deaths,df_arg_recovered)
df_arg <- add_params(df_arg,pob_arg)

df_bol <- sort_df(df_bol,df_bol_deaths,df_bol_recovered)
df_bol <- add_params(df_bol,pob_bol)

df_per <- sort_df(df_per,df_per_deaths,df_per_recovered)
df_per <- add_params(df_per,pob_per)

df_final <- rbind(df_chi,df_arg,df_bol,df_per)
df_final <- df_final[order(df_final$Dias),]
head(df_final,n=5)

graph1 <- ggplot(df_final) + geom_line(aes(x = Dias, y = Casos, col = Pais), size=1, alpha = 1) + ggtitle("Evolución de casos COVID19 por País desde el 2do caso") + theme_bw()


graph1

# graph1 <- ggplot(df_final) + geom_line(aes(x = Dias, y = Casos, col = Pais),size=1, alpha = 1) + 
#                                                                                     ggtitle("Evolución de casos COVID19 desde el 2do caso") + theme_bw()
# graph2 <- ggplot(df_final) + geom_line(aes(x= Dias, y = Tasa_Incidencia, col = Pais),size = 1, alpha = 1) + 
#                                                                                     ggtitle("Tasa de Incidencia") + theme_bw()
# ggsave(plot = graph1, width = w*7, height = 8, dpi = 300, filename = "graph.pdf")
# ggsave(plot = graph2, width = w*7, height = 8, dpi = 300, filename = "incidencia.pdf")

# grid.arrange(graph1,graph2,nrow=2)

#Tomamos el último valor de nuestros df
acum_chi <- tail(df_chi$Casos, n=1) 
acum_arg <- tail(df_arg$Casos, n=1) 
acum_bol <- tail(df_bol$Casos, n=1)
acum_per <- tail(df_per$Casos, n=1)

ti_chi <- (acum_chi/pob_chi)*100000
ti_arg <- (acum_arg/pob_arg)*100000
ti_bol <- (acum_bol/pob_bol)*100000
ti_per <- (acum_per/pob_per)*100000

print(paste('Tasa de incidencia Chile: ',ti_chi))
print(paste('Tasa de incidencia Argentina: ',ti_arg))
print(paste('Tasa de incidencia Bolivia: ',ti_bol))
print(paste('Tasa de incidencia Peru: ',ti_per))









add_tc <- function(df,pob){
    #Agregamos la tasa de contagio al data frame
    df$"Tasa_Contagio" <- df$Casos-df$Muertos-df$Recuperados*(100000/pob)
    return(df)
}

week <- function(n,w,df){
    df <- df[df$Casos>n,]
    df <- df[df$Fecha>=df$Fecha[1] & df$Fecha<(df$Fecha+(w*7))[1],]
    return(df)
}

n = 200
w = 2

df_chi2 <- week(n,w,df_chi) #Filtramos el df a w semanas desde que se producen n casos
df_chi2 <- add_tc(df_chi2,pob_chi) #Agregmaos la tasa de contagio en el periodo mencionado
df_chi2$Dias <- c(1:nrow(df_chi2)) #Agregamos una nueva columna con la tasa de contagio

df_arg2 <- week(n,w,df_arg) 
df_arg2 <- add_tc(df_arg2,pob_arg)
df_arg2$Dias <- c(1:nrow(df_arg2))

df_bol2 <- week(n,w,df_bol) 
df_bol2 <- add_tc(df_bol2,pob_bol)
df_bol2$Dias <- c(1:nrow(df_bol2))

df_per2 <- week(n,w,df_per) 
df_per2 <- add_tc(df_per2,pob_per)
df_per2$Dias <- c(1:nrow(df_per2))

df_final2 <- rbind(df_chi2,df_arg2,df_bol2,df_per2)
df_final2 <- df_final2[order(df_final2$Dias),]

head(df_final2,n=5)

graph2 <- ggplot(df_final2) + geom_line(aes(x = Dias, y = Tasa_Contagio, col = Pais), size=1, alpha = 1) + 
                                                                                    ggtitle("Tasa de contagio acumulada desde que se superan 200 confirmados") + theme_bw()
ggsave(plot = graph2, width = w*7, height = 8, dpi = 300, filename = "graph3.pdf")


graph2

n_cont_chi <- sum(df_chi2$Casos_Nuevos)
n_fall_chi <- tail(df_chi2$Muertos, n=1) - df_chi2$Muertos[1]
n_rec_chi <- tail(df_chi2$Recuperados, n=1) - df_chi2$Recuperados[1]

n_cont_arg <- sum(df_arg2$Casos_Nuevos)
n_fall_arg <- tail(df_arg2$Muertos, n=1) - df_arg2$Muertos[1]
n_rec_arg <- tail(df_arg2$Recuperados, n=1) - df_arg2$Recuperados[1]

n_cont_bol <- sum(df_bol2$Casos_Nuevos)
n_fall_bol <- tail(df_bol2$Muertos, n=1) - df_bol2$Muertos[1]
n_rec_bol <- tail(df_bol2$Recuperados, n=1) - df_bol2$Recuperados[1]

n_cont_per <- sum(df_per2$Casos_Nuevos)
n_fall_per <- tail(df_per2$Muertos, n=1) - df_per2$Muertos[1]
n_rec_per <- tail(df_per2$Recuperados, n=1) - df_per2$Recuperados[1]

tc_chi <- (n_cont_chi-n_fall_chi-n_rec_chi)/(pob_chi)*100000
tc_arg <- (n_cont_arg-n_fall_arg-n_rec_arg)/(pob_arg)*100000
tc_bol <- (n_cont_bol-n_fall_bol-n_rec_bol)/(pob_bol)*100000
tc_per <- (n_cont_per-n_fall_per-n_rec_per)/(pob_per)*100000

print(paste('Tasa contagio Chile:',tc_chi))
print(paste('Tasa contagio Argentina:',tc_arg))
print(paste('Tasa contagio Bolivia:',tc_bol))
print(paste('Tasa contagio Peru:',tc_per))

z_alpha2 <- qnorm(1- 0.05/2)
print(z_alpha2)

int_chi <- c(tc_chi-z_alpha2*sqrt((tc_chi*(100000-tc_chi))/pob_chi), tc_chi+z_alpha2*sqrt((tc_chi*(100000-tc_chi))/pob_chi))
int_arg <- c(tc_arg-z_alpha2*sqrt((tc_arg*(100000-tc_arg))/pob_arg), tc_arg+z_alpha2*sqrt((tc_arg*(100000-tc_arg))/pob_arg))
int_bol <- c(tc_bol-z_alpha2*sqrt((tc_bol*(100000-tc_bol))/pob_bol), tc_bol+z_alpha2*sqrt((tc_bol*(100000-tc_bol))/pob_bol))
int_per <- c(tc_per-z_alpha2*sqrt((tc_per*(100000-tc_per))/pob_per), tc_per+z_alpha2*sqrt((tc_per*(100000-tc_per))/pob_per))


print(paste('Intervalo de Confianza de Tasa de Contagio para Argentina:',int_arg[1],int_arg[2]))
print(paste('Intervalo de Confianza de Tasa de Contagio para Chile:',int_chi[1],int_chi[2]))
print(paste('Intervalo de Confianza de Tasa de Contagio para Bolivia:',int_bol[1],int_bol[2]))
print(paste('Intervalo de Confianza de Tasa de Contagio para Peru:',int_per[1],int_per[2]))








ggplot(data = df_final[df_final$Pais == "Chile" | df_final$Pais=='Peru',], aes(x= Pais, y= Tasa_Incidencia, color = Pais)) + 
  geom_boxplot() +
  theme_bw()

t.test(x = df_final[df_final$Pais == "Peru",]$Tasa_Incidencia,
       y = df_final[df_final$Pais == "Chile", ]$Tasa_Incidencia,
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95 )

alpha = 0.05
t_alpha = qnorm(1-alpha/2) #Como es un test de dos colas dividimos alpha en dos
print(t_alpha)

ggplot(data = df_final[df_final$Pais == "Argentina" | df_final$Pais=='Bolivia',], aes(x= Pais, y= Tasa_Incidencia, color = Pais)) + 
  geom_boxplot() +
  theme_bw()

t.test(x = df_final[df_final$Pais == "Argentina",]$Tasa_Incidencia,
       y = df_final[df_final$Pais == "Bolivia", ]$Tasa_Incidencia,
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)

alpha = 0.05
t_alpha = qnorm(1-alpha/2) #Como es un test de dos colas dividimos alpha en dos
print(t_alpha)

shapiro.test(df_per$Tasa_Incidencia)

par(mfcol=c(2,2))
qqnorm(df_chi$Tasa_Incidencia, pch = 1 , frame = FALSE,main='Q-Q Plot TI Chile') 
qqline(df_chi$Tasa_Incidencia, col = "steelblue", lwd = 2)

qqnorm(df_per$Tasa_Incidencia, pch = 1 , frame = FALSE,main='Q-Q Plot TI Perú')
qqline(df_per$Tasa_Incidencia, col = "steelblue", lwd = 2)

qqnorm(df_arg$Tasa_Incidencia, pch = 1 , frame = FALSE,main='Q-Q Plot TI Argentina')
qqline(df_arg$Tasa_Incidencia, col = "steelblue", lwd = 2)

qqnorm(df_bol$Tasa_Incidencia, pch = 1 , frame = FALSE,main='Q-Q Plot TI Bolivia')
qqline(df_bol$Tasa_Incidencia, col = "steelblue", lwd = 2)

ggsave(plot = last_plot(), width = w*7, height = 8, dpi = 300, filename = "qqplots.pdf")

qqnorm(df_chi$Tasa_Mortalidad, pch = 1 , frame = FALSE)
qqline(df_chi$Tasa_Mortalidad, col = "steelblue", lwd = 2)

df_final %>% 
  group_by(Pais) %>% 
  summarize(mean_mortalidad= mean(Tasa_Mortalidad),
            std_mortalidad = sd(Tasa_Mortalidad))

m_chi <-  df_chi$Tasa_Mortalidad
m_per <-  df_per$Tasa_Mortalidad

n_chi <- length(m_chi)
n_per <- length(m_per)

print(paste("n Chile: ", n_chi))
print(paste("n Peru: ", n_per))

p_chi <- tail(df_chi$Tasa_Mortalidad,n=1)
n_chi <- pob_chi

p_per <- tail(df_per$Tasa_Mortalidad,n=1)
n_per <- pob_per

print(paste('P_chi: ',p_chi))
print(paste('P_per: ',p_per))

p_hat <- 100000*((p_chi+p_per)/(n_chi+n_per))

SE_p <- sqrt(p_hat*(100000-p_hat)*((1/n_chi)+(1/n_per)))
print(paste("SE de p_hat: ", SE_p))

Z <- (p_chi-p_per)/SE_p
print(paste("Z : ", Z))

print(paste('n_chi:',n_chi))
print(paste('n_per:',n_per))

alpha = 0.05
z_alpha = qnorm(1- alpha/2)
print(paste("Z_alpha :", z_alpha))







p_arg <- tail(df_arg$Tasa_Mortalidad,n=1)
n_arg <- pob_arg

p_bol <- tail(df_bol$Tasa_Mortalidad,n=1)
n_bol <- pob_bol

print(paste('P_arg: ',p_arg))
print(paste('P_bol: ',p_bol))

p_hat2 <- 100000*((p_arg+p_bol)/(n_arg+n_bol))

print(paste('P_hat2: ',p_hat2))

SE_p2 <- sqrt(p_hat2*(100000-p_hat2)*((1/n_arg) + (1/n_bol)))
print(paste("SE de p_hat: ", SE_p2))

Z2 <- (p_arg-p_bol)/SE_p2
print(paste("Z : ", Z2))

print(paste('n_arg:',n_arg))
print(paste('n_bol:',n_bol))

alpha = 0.05
z_alpha = qnorm(1 - alpha/2)
print(paste("Z_alpha :", z_alpha))

ggplot(data = df_final, aes(x= Pais, y= Tasa_Mortalidad, color = Pais))+ geom_boxplot()+ theme_bw()





df_anova <- df_final[df_final$Dias < 46, ]
ggplot(data = df_anova, aes(x= Pais, y= Tasa_Incidencia, color = Pais)) + 
  geom_boxplot() +
  theme_bw()

anova_df <- aov(df_anova$Tasa_Incidencia~df_anova$Pais)
summary(anova_df)

t.test(x = df_final[df_final$Pais == "Bolivia",]$Tasa_Incidencia,
       y = df_final[df_final$Pais == "Chile", ]$Tasa_Incidencia,
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)

t.test(x = df_final[df_final$Pais == "Peru",]$Tasa_Incidencia,
       y = df_final[df_final$Pais == "Argentina", ]$Tasa_Incidencia,
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)
