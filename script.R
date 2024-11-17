
library(countrycode)
library(lubridate)
library(ggrepel)
library(patchwork)
library(tidytext)
library(tidyverse)
library(ggridges)
theme_set(theme_bw(base_size = 21))
options(scipen = 999)



# ===== PROCESANDO INFLACIÓN =============
argipc <- readxl::read_excel("ipc argentina.xlsx")

arg <- argipc %>% 
  mutate(anio = year(dmy(Fecha))) %>% 
  group_by(anio) %>% 
  summarise(ipc = mean(Valor)) %>% 
  as.data.frame() %>% 
  mutate(pais = "Argentina",
         iso = countrycode(pais, 
                           origin = "country.name",
                           destination = "iso3c"))

tempipc <- readxl::read_excel("INFLACION.xls", sheet = 1, skip = 2)

temipc2 <- tempipc %>% 
  select(-c(`Indicator Name`, `Indicator Code`)) %>% 
  gather("anio", "ipc", 3:ncol(.)) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  filter(anio >= 2015,
         !is.na(ipc))

colnames(temipc2)[c(1, 2)] <- c("pais", "iso")

consoipc <- bind_rows(temipc2, arg) %>% 
  mutate(region = countrycode(iso, origin="iso3c",
                              destination = "region")) %>% 
  filter(!is.na(region))


rm(arg, argipc, temipc2, tempipc)


# ================ PROCESANDO VIOLENCIA ===========

tempviol = readxl::read_excel("data_cts_intentional_homicide.xlsx",
                              skip = 2)

temp <- tempviol %>%
  filter(Iso3_code == "BOL",
         Indicator == "Victims of intentional homicide",
         `Unit of measurement` == "Counts",
         Year == 2008) %>% 
  select(Country, Iso3_code, Year, VALUE) %>% 
  group_by(Country, Iso3_code, Year) %>% 
  summarise(kill = (sum(VALUE)/9880593)*100000)

colnames(temp)[1:3] <- c("pais", "iso", "anio")


homicidio <- tempviol %>% 
  filter(Sex == "Total",
         Indicator == "Victims of intentional homicide",
         Dimension == "Total",
         Category == "Total",
         `Unit of measurement` == "Rate per 100,000 population") %>% 
  select(Country, Iso3_code, Year, VALUE) %>% 
  mutate(region = countrycode(Iso3_code, origin = "iso3c",
                              destination = "region")) %>% 
  filter(!is.na(region))

colnames(homicidio)[1:4] <- c("pais", "iso", "anio", "kill")

homicidio <- bind_rows(homicidio, temp)


# otros paises
otrosla <- data.frame(
  pais = c("Chile", "Argentina", "Peru", "Peru", "Brazil",
           "Uruguay", "Paraguay", "Colombia", "Ecuador",
           "Mexico", "Panama", "El Salvador","Honduras",
           "Trinidad and Tobago", "Dominican Republic"),
  iso = c("CHL", "ARG", "PER", "PER", "BRA", "URY", "PRY","COL",
          "ECU", "MEX", "PAN", "SLV", "HND", "TTO", "DOM"),
  anio = c(2023, 2023, 2023, 2022, 2023, 2023, 2023, 2023, 2023,
           2023, 2023, 2023, 2023, 2023,2023),
  kill = c(6.3 , 4.4, 3.2, 4.5, 22.8, 10.2, 6.2, 25.8, 44.5,
           24, 13.7, 2.4, 34.4, 42.2, 11.6)
) %>% 
  mutate(region = countrycode(iso, origin = "iso3c",
                              destination = "region"))


otrosaf <- data.frame(
  pais = c("Australia", "Spain"),
  iso = c("AUS", "ESP"),
  anio = c(2023, 2023),
  kill = c(0.87, 0.68)
)

homicidio <- bind_rows(homicidio, otrosla, otrosaf)

rm(tempviol, otrosaf, otrosla, temp)


# ================= PROCESANDO POBREZA =====================

tempov <- read.csv("pip.csv")

pobreza <- tempov %>% 
  group_by(country_name, reporting_year) %>% 
  arrange(welfare_type == "income", .by_group = TRUE) %>% 
  slice_tail(n = 1) %>%                             
  ungroup() %>% 
  group_by(country_name, reporting_year) %>% 
  arrange(survey_coverage == "national", .by_group = TRUE) %>% 
  slice_tail(n = 1) %>%                            
  ungroup() %>% 
  select(country_name, country_code, reporting_year, headcount)

colnames(pobreza) <- c("pais", "iso", "anio", "poverty3_75")

# pobreza <- pobreza %>% filter(anio >= 2015)

rm(tempov)

# ========== ANALISIS ==================


g1 <- consoipc %>%
  group_by(anio) %>%
  summarise(q1 = quantile(ipc, .25),
            q2 = quantile(ipc, .5),
            q3 = quantile(ipc, .75)) %>%
  ggplot() +
  aes(anio, ymin = q1, y = q2, ymax = q3) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(alpha = .3) +
  geom_line(
    data = consoipc %>%
      filter(pais == "Chile") %>%
      group_by(anio) %>%
      summarise(q1 = quantile(ipc, .25),
                q2 = quantile(ipc, .5),
                q3 = quantile(ipc, .75)),
    mapping = aes(x=anio, y = q2),
    color = "red"
  ) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(2015, 2023, 1),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title ="Inflación",
       subtitle = "Variación anual IPC en el mundo",
       caption = "Recta es IPC mediano, bandas son cuantil .25 y .75")


g2 <- homicidio %>% 
  filter(anio >= 2015)
  group_by(anio) %>% 
  summarise(q1 = quantile(kill, .25),
            q2 = quantile(kill, .5),
            q3 = quantile(kill, .75)) %>% 
  ggplot() +
  aes(anio, ymin = q1, y = q2, ymax = q3) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(alpha = .3) +
  geom_line(
    data = homicidio %>% 
      filter(pais == "Chile") %>% 
      group_by(anio) %>% 
      summarise(q1 = quantile(kill, .25),
                q2 = quantile(kill, .5),
                q3 = quantile(kill, .75)),
    mapping = aes(x=anio, y = q2),
    color = "red"
  ) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(2015, 2023, 1),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title ="Homicidios",
       subtitle = "Víctimas de homicidio cada 1000 personas",
       caption = "Recta es homicidio mediano, bandas son cuantil .25 y .75")


g3 <- pobreza %>% 
  group_by(anio) %>% 
  summarise(q1 = quantile(poverty3_75, .25),
            q2 = quantile(poverty3_75, .5),
            q3 = quantile(poverty3_75, .75)) %>% 
  ggplot() +
  aes(anio, ymin = q1, y = q2, ymax = q3) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(alpha = .3) +
  geom_line(
    data = pobreza %>% 
      filter(pais == "Chile") %>% 
      group_by(anio) %>% 
      summarise(q1 = quantile(poverty3_75, .25),
                q2 = quantile(poverty3_75, .5),
                q3 = quantile(poverty3_75, .75)),
    mapping = aes(x=anio, y = q2),
    color = "red"
  ) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(2015, 2023, 1),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title ="Pobreza",
       subtitle = "% de personas que viven con menos de U$3.75 al día (precios del 2017)",
       caption = "Recta es pobreza mediana, bandas son cuantil .25 y .75")

g1

g2

g3

# inflación
consoipc %>% 
  filter(anio == 2022) %>% 
  group_by(anio) %>%
  summarise(q1 = quantile(ipc, .25),
            q2 = quantile(ipc, .5),
            q3 = quantile(ipc, .75)) 


# homicidio
homicidio$conti <- countrycode(homicidio$iso,
                               origin = "iso3c",
                               destination = "continent")

homicidio %>% 
  filter(anio == 1995) %>% 
  # mutate(anio = paste0("y", anio)) %>% 
  group_by(conti) %>% 
  summarise(n=n(),
            avg = mean(kill))

homicidio %>% 
  filter(anio == 1995,
         region == "Latin America & Caribbean") %>% 
  # mutate(anio = paste0("y", anio)) %>% 
  group_by(conti) %>% 
  summarise(n=n(),
            avg = mean(kill))


homicidio %>% 
  filter(anio %in% c(2015, 2019)) %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, kill) %>% 
  mutate(dif1 = (y2019-y2015)) -> temp1
  # group_by(conti) %>% 
  # summarise(avg = mean(dif, na.rm = T),
  #           sube = sum(dif > 0,na.rm = T),
  #           caemantiene = sum(dif <= 0,na.rm = T)) %>% 
  # mutate(tasasube1 = sube/(sube+caemantiene),
  #        periodo = "uno") 

homicidio %>% 
  filter(anio %in% c(2019, 2022)) %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, kill) %>% 
  mutate(dif2 = (y2022-y2019)) -> temp2
  # group_by(conti) %>% 
  # summarise(avg = mean(dif, na.rm = T),
  #           sube = sum(dif > 0,na.rm = T),
  #           caemantiene = sum(dif <= 0,na.rm = T)) %>% 
  # mutate(tasasube2 = sube/(sube+caemantiene)) 


temp1 %>% 
  select(iso, dif1, conti) %>% 
  inner_join(temp2 %>% select(iso, dif2)) %>% 
  filter(conti != "Oceania") %>% 
  ggplot() +
  aes(dif1, dif2) +
  geom_jitter() +
  facet_wrap(.~conti, scales = "free") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 9))


homicidio %>% 
  filter(anio %in% c(2019, 2022),
         conti != "Oceania") %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, kill) %>% 
  mutate(dif = (y2022-y2019)) %>% 
  filter(!is.na(dif)) %>% 
  ggplot() +
  aes(reorder_within(iso, dif, conti), dif) +
  geom_col() +
  facet_wrap(.~conti, scales = "free") +
  scale_x_reordered() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   size = 9))

homicidio %>% 
  filter(anio %in% c(2019, 2022),
         conti != "Oceania") %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, kill) %>% 
  mutate(dif = (y2022-y2019)) %>% 
  filter(!is.na(dif)) %>% 
  ggplot() +
  aes(y2019, dif) +
  geom_jitter() +
  geom_text_repel(aes(label = iso)) +
  facet_wrap(.~conti, scales = "free") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 9)) +
  labs(x="Homicide rates in 2019",
       y="2019 to 2022 difference")

tiene23 <- homicidio %>% 
  filter(anio == 2023,
         conti == "Americas") %>% 
  select(iso) %>% 
  unlist()


homicidio %>% 
  filter(region == "Latin America & Caribbean",
         anio >= 2015) %>%
  group_by(anio) %>% 
  summarise(med = median(kill, na.rm = T))


homicidio %>% 
  filter(anio %in% c(2019, 2023),
         conti == "Americas") %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, kill) %>% 
  mutate(dif = (y2023-y2019)) %>% 
  filter(!is.na(dif)) %>% 
  left_join(homicidio %>% 
              filter(anio >=1990,
                     anio <= 2015,
                     iso %in% tiene23) %>% 
              group_by(iso) %>% 
              summarise(med = mean(kill, na.rm = T))) %>% 
  ggplot() +
  aes(y2019, dif, color = med) +
  scale_color_gradient(low = "green", high = "red") +
  geom_jitter() +
  geom_text_repel(aes(label = iso)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-40, 40, 5)) +
  scale_x_continuous(breaks = seq(0, 40, 3)) +
  theme(axis.text.x = element_text(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size = 10)) +
  labs(x="Tasa homicidios en 2019",
       y="Diferencia 2019 a 2023",
       color = "Media \nhomicidios \n1990-2015",
       caption = "Bolivia solo tiene datos en 2008 y 2015")



homicidio %>% 
  filter(iso %in% c("CHL", "ARG", "URY", "CRI"),
         anio >= 2005) %>% 
  ggplot() +
  aes(anio, kill, color = reorder(iso, -kill)) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.2,.8)) +
  labs(title = "Homicidios en países pacíficos",
       subtitle = "Tasa homicidios en países con tasas más bajas de LATAM")
  
homicidio %>% 
  filter(iso %in% c("BRA", "MEX"),
         anio >= 2005) %>% 
  ggplot() +
  aes(anio, kill, color = reorder(iso, -kill)) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.2,.8)) +
  labs(title = "Homicidios en países pacíficos",
       subtitle = "Tasa homicidios en países con tasas más bajas de LATAM")


homicidio %>% 
  filter(iso %in% c("CHL"),
         anio >= 2015,
         anio <= 2019) %>% 
  select(kill) %>% 
  unlist() %>% 
  {
    sd(.)/mean(.)
  }

homicidio %>% 
  filter(iso %in% c("CHL"),
         anio >= 2019) %>% 
  select(kill) %>% 
  unlist() %>% 
  {
    sd(.)/mean(.)
  }


homicidio %>% 
  filter(iso %in% c("CHL"),
         anio >= 2014) %>% 
  ggplot() +
  aes(anio, kill) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  geom_rect(aes(xmin = 2014, xmax = 2018, 
                ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = .03) +
  annotate(geom = "text", 
           x = 2016, y = 7, label = "Bachelet 2",
           size = 10) +
  geom_rect(aes(xmin = 2018, xmax = 2022, 
                ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = .03) +
  annotate(geom = "text", 
           x = 2020, y = 7, label = "Piñera 2",
           size = 10) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.2,.8)) +
  labs(title = "Tasa de Homicidios en Chile")



homicidio %>% 
  filter(iso %in% c("CHL"),
         anio >= 2005) %>% 
  ggplot() +
  aes(anio, log(kill/lag(kill)), color = reorder(iso, -kill)) +
  geom_line(linewidth = 1.2) +
  # facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.2,.8)) +
  labs(title = "Homicidios en países pacíficos",
       subtitle = "Tasa homicidios en países con tasas más bajas de LATAM")





homicidio %>% 
  filter(region == "Latin America & Caribbean") %>%
  group_by(anio) %>% 
  summarise(cv = sd(kill, na.rm = T)/mean(kill, na.rm = T)) %>% 
  ggplot() +
  aes(anio, cv) +
  geom_line() +
  # facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  # scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


homicidio %>% 
  filter(conti != "Oceania") %>%
  group_by(anio, conti) %>% 
  summarise(cv = sd(kill, na.rm = T)/mean(kill, na.rm = T)) %>% 
  ggplot() +
  aes(anio, cv) +
  geom_line(linewidth = 1.2) +
  facet_wrap(.~conti) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 2),
                     expand = c(0,0)) +
  # scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Convergencia",
       subtitle = "Coeficiente de variación en tasas de homicidios")


homicidio %>%
  filter(conti != "Oceania") %>%
  group_by(anio, conti) %>% 
  summarise(avg = median(kill, na.rm = T)) %>%
  ggplot() +
  aes(anio, avg) +
  geom_line(linewidth = 1.2) +
  facet_wrap(.~conti, scales = "free_y") +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(breaks = seq(1990, 2023, 2),
                     expand = c(0,0)) +
  # scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Homicidio global")

homicidio %>%
  filter(region == "Latin America & Caribbean",
         anio %in% c(2015, 2019, 2022)) %>% 
  ggplot() +
  aes(kill, fill = as.factor(anio)) +
  geom_density(alpha = .3)

pobreza$conti <- countrycode(pobreza$iso, 
                             origin = "iso3c",
                             destination = "continent")

pobreza  %>% 
  filter(anio %in% c(2019, 2021),
         conti %in% c("Americas", "Asia", "Europe")) %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, poverty3_75) %>% 
  ggplot() +
  aes(y2019, y2021-y2019) +
  geom_jitter() +
  geom_text_repel(aes(label = iso)) +
  facet_wrap(.~conti, scales = "free_x") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size = 10)) +
  labs(x="Pobreza en el 2019",
       y="Diferencia 2019 a 2021")

pobreza %>% 
  filter(anio >= 2019) %>% 
  group_by(conti, anio) %>% 
  summarise(n = length(unique(iso)))


pobreza %>% 
  filter(iso == "CHL")


0.0111 * 100
0.0281 * 100

pobreza %>%
  filter(anio %in% c(2015, 2019, 2021),
         !is.na(conti),
         conti != "Oceania") %>% 
  group_by(conti, anio) %>% 
  summarise(m = median(poverty3_75, na.rm = T))
  

pobreza %>% 
  filter(anio %in% c(2015, 2019)) %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, poverty3_75) %>% 
  mutate(dif = (y2019-y2015)) %>% 
group_by(conti) %>%
summarise(avg = mean(dif, na.rm = T),
          sube = sum(dif > 0,na.rm = T),
          caemantiene = sum(dif <= 0,na.rm = T)) %>%
mutate(tasasube1 = sube/(sube+caemantiene),
       periodo = "uno")

pobreza %>% 
  filter(anio %in% c(2019, 2021)) %>% 
  mutate(anio = paste0("y", anio)) %>% 
  spread(anio, poverty3_75) %>% 
  mutate(dif = (y2021-y2019)) %>% 
  group_by(conti) %>%
  summarise(avg = mean(dif, na.rm = T),
            sube = sum(dif > 0,na.rm = T),
            caemantiene = sum(dif <= 0,na.rm = T)) %>%
  mutate(tasasube1 = sube/(sube+caemantiene),
         periodo = "uno")


chl <- pobreza %>% 
  filter(iso == "CHL") %>% 
  mutate(dify = anio - lag(anio),
         difpov = (poverty3_75 - lag(poverty3_75))*100) %>% 
  mutate(avgy = difpov/dify)

chl %>% 
  ggplot() +
  aes(x = anio) +
  geom_line(aes(y = avgy/100, color = "Variación anual media")) +
  geom_line(aes(y = poverty3_75, color = "Pobreza")) +
  geom_point(aes(y = avgy/100, color = "Variación anual media")) +
  geom_point(aes(y = poverty3_75, color = "Pobreza")) 



chl <- pobreza %>% 
  filter(iso == "CHL") %>% 
  mutate(dify = anio - lag(anio),
         difpov = (poverty3_75 - lag(poverty3_75))*100) %>% 
  mutate(avgy = difpov/dify)


ury <- pobreza %>% 
  filter(iso == "URY") %>% 
  mutate(dify = anio - lag(anio),
         difpov = (poverty3_75 - lag(poverty3_75))*100) %>% 
  mutate(avgy = difpov/dify)


ury <- pobreza %>% 
  filter(iso == "CRI") %>% 
  mutate(dify = anio - lag(anio),
         difpov = (poverty3_75 - lag(poverty3_75))*100) %>% 
  mutate(avgy = difpov/dify)

tresgrandes <- pobreza %>% 
  filter(iso %in% c("CRI", "URY", "CHL")) %>% 
  group_by(pais) %>% 
  mutate(dify = anio - lag(anio),
         difpov = (poverty3_75 - lag(poverty3_75))*100) %>% 
  mutate(avgy = difpov/dify)


tresgrandes %>% 
  filter(anio >= 1990) %>% 
  ggplot() +
  aes(anio, avgy, color = pais) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  geom_hline(yintercept = 0, size = .9) +
  scale_x_continuous(breaks = seq(1990, 2023, 1),
                     expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8,.8)) +
  labs(y = "← Fall - Rise →",
       title = "Poverty acceleration",
       subtitle = "Avg year variation by year of measurement")




