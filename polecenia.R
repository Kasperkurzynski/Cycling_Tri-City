# Wczytanie danych
load('przejazdy.RData')
load('punkty_pomiarowe.RData')

# Wczytanie potrzebnych bibliotek
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)
library(RColorBrewer)
library(ggcorrplot)
library(hexbin)
library(ggridges)
library(gganimate)
library(RColorBrewer)
library(viridis)
library(calendR)
library(gifski)
library(sf)
library(tmap)
library(knitr)
library(kableExtra)

# Utworzenie nowych zmiennych

przejazdy$Month <- month(ymd(przejazdy$Data)) 
przejazdy$Year <- year(ymd(przejazdy$Data))
przejazdy$Day <- day(ymd(przejazdy$Data))
przejazdy$Dzien_tyg <- as.character(przejazdy$Data, format = "%A")
przejazdy$Czy_Weekend <- ifelse(przejazdy$Dzien_tyg %in% c("sobota", "niedziela"),"weekend", "dzień powszedni")

przejazdy <- przejazdy %>%
  mutate(Month = factor(Month), Year = factor(Year), Day = factor(Day), 
         Dzien_tyg = factor(Dzien_tyg), Czy_Weekend = factor(Czy_Weekend))

przejazdy$Pora_Roku <- ifelse(przejazdy$Month %in% c("12", '1', '2'), 'Zima', 
                              ifelse(przejazdy$Month %in% c("3", '4', '5'), 'Wiosna',
                              ifelse(przejazdy$Month %in% c("6", '7', '8'), 'Lato', 'Jesień')))

przejazdy$Pora_Roku <- as.factor(przejazdy$Pora_Roku)

levels(przejazdy$Month) <- c('1' = 'Styczeń', '2' = 'Luty', '3' = 'Marzec',
                              '4' = 'Kwiecień', '5' = 'Maj', '6' = 'Czerwiec',
                              '7' = 'Lipiec', '8' = 'Sierpień', '9' = 'Wrzesień',
                              '10' = 'Październik', '11' = 'Listopad', '12' = 'Grudzień')


# Polecenia wykonanie za pomocą R graphics

# Polecenie 1.
zad1 <- przejazdy[,c(1,3)]

zad1 <- zad1 %>%
  group_by(Stacja) %>%
  summarise(Liczba_dni = n()) %>%
  arrange(desc(Liczba_dni))


par(mar=c(5, 11, 4, 2))
barplot(sort(zad1$Liczba_dni),
        names.arg = zad1$Stacja[order(zad1$Liczba_dni)],
        main = "Rozkład liczby dni według punktu pomiarowego",
        xlab = "Liczba dni",
        space = 0.3,
        col = rev(viridis(35)),
        horiz=T,
        cex.names=0.8,
        las=1,
        font.axis = 3,
        font.lab = 2)


# Polecenie 2.
zad2 <- przejazdy[,c(1,2,3)]

zad2 <- zad2 %>%
  filter(Stacja == "Pas Nadmorski")

par(mar=c(5, 5, 4, 2))
hist(zad2$Licznik,
     main="Rozkład liczby przejazdów w punkcie pomiarowym Pas Nadmorski",
     xlab="Liczba przejazdów",
     ylab = "Częstość",
     col="darkorange1",
     border = "black",
     breaks = seq(0,12000,500),
     xlim = c(0,12500),
     las = 1,
     font.axis = 3,
     font.lab = 2)


# Polecenia wykonanie za pomocą pakietu 'ggplot2'

# Polecenie 3.

# Porównanie względem natężenia
zad3 <- przejazdy %>%
  group_by(Stacja) %>%
  summarise(Liczba_dni = n(), 
            Suma_przejazdow = sum(Licznik), 
            Natezenie = round(Suma_przejazdow / Liczba_dni,1)) %>%
  arrange(desc(Natezenie))
  
zad3

ggplot(zad3, aes(x = reorder(Stacja, -Natezenie), y = Natezenie)) + geom_bar(stat="identity", fill = heat.colors(27), alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        plot.title = element_text(color="grey40", face = "bold", size=19, family="serif")) +
  scale_y_continuous(breaks = seq(0, 3000, 500)) +
  annotate(geom = 'rect',
           xmin = 0.5, xmax = 3.5,
           ymin = 0.5, ymax = 2600,
           fill = 'green',
           alpha = 0.4) +
  annotate("text", x = 7.5, y = 2400, label = "Stacje o największym natężeniu") +
  annotate(geom = 'rect',
           xmin = 24.5, xmax = 27.5,
           ymin = 0.5, ymax = 400,
           fill = 'pink',
           alpha = 0.6) +
  annotate("text", x = 24, y = 500, label = "Stacje o najniższym natężeniu") +
  xlab("Stacja") +
  ylab("Natężenie") +
  ggtitle('Natężenie ruchu rowerowego w zależności od stacji')


# Porównanie względem rozkładów dla wybranych stacji
zad3.1 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski" | Stacja == "ul. Kartuska" | Stacja == "al. Hallera" |
         Stacja == "ul. Jaśkowa Dolina" | Stacja == "ul. Wita Stwosza" | Stacja == "ul. Sucharskiego" |
         Stacja == "al. Havla" | Stacja == "al. Żołnierzy Wyklętych" | Stacja == "ul. 3 Maja")

# Za pomocą histogramu
ggplot(zad3.1, aes(x = Licznik, fill = Stacja)) + geom_histogram(binwidth = 600, show.legend = F, alpha = 0.8) + 
  facet_wrap(~Stacja) +
  scale_x_continuous(breaks = seq(0, 12000, 1500)) +  
  xlab("Liczba przejazdów") +
  ylab("Liczebność") +
  ggtitle('Rozkłady liczby przejazdów dla wybranych stacji') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 12),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

# Za pomocą wykresu z pakietu ggridges
ggplot(zad3.1, aes(x = Licznik, y = Stacja, fill = Stacja)) +
  geom_density_ridges(scale = 2, show.legend = F) +  
  theme_ridges()

# Za pomocą wykresu skrzynkowego
ggplot(zad3.1, aes(x = Stacja, y = Licznik, fill = Stacja)) + geom_boxplot(show.legend = F) +
  xlab("Stacja") + ylab("Licznik") + ggtitle("Rozkład liczby przejazdów względem wybranej stacji") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        plot.title = element_text(color="grey40", face = "bold", size=19, family="serif"))
  

# Polecenie 4.

# Natężenie a miesiąc
zad4 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski") %>%
  group_by(Month) %>%
  summarise(Liczba_dni = n(), 
            Suma_przejazdow = sum(Licznik), 
            Natezenie = round(Suma_przejazdow / Liczba_dni,1))


ggplot(zad4, aes(x = reorder(Month, -Natezenie), y = Natezenie)) + geom_bar(stat="identity", fill = 'slateblue', alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        plot.title = element_text(color="grey40", face = "bold", size=19, family="serif")) +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  annotate(geom = 'rect',
           xmin = 0.5, xmax = 3.5,
           ymin = 0.5, ymax = 5900,
           fill = 'green',
           alpha = 0.4) +
  annotate("text", x = 5, y = 5400, label = "Miesiące o największym natężeniu") +
  annotate(geom = 'rect',
           xmin = 9.5, xmax = 12.5,
           ymin = 0.5, ymax = 800,
           fill = 'pink',
           alpha = 0.6) +
  annotate("text", x = 11, y = 1150, label = "Miesiące o najniższym natężeniu") +
  xlab("Miesiąc") +
  ylab("Natężenie") +
  ggtitle('Natężenie ruchu rowerowego w zależności od miesiąca')

# Natężenie a typ dnia
zad4.1 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski") %>%
  group_by(Czy_Weekend, Pora_Roku) %>%
  summarise(Liczba_dni = n(), 
            Suma_przejazdow = sum(Licznik), 
            Natezenie = round(Suma_przejazdow / Liczba_dni,1))

ggplot(zad4.1, aes(x = Czy_Weekend, y = Natezenie, fill = Czy_Weekend)) + geom_col(alpha = 0.8) +
  geom_label(aes(x = Czy_Weekend, y = Natezenie, label = Natezenie)) +
  scale_fill_manual(values = c('orangered3', 'royalblue2')) +
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(color="Grey40", size=16),
        strip.background = element_rect(colour = "black", fill = 'lightyellow2'),
        strip.text = element_text(face = "italic", size = 10),
        legend.position = "none",
        plot.title = element_text(color="grey40", face = "bold", size=19, family="serif")) +
  scale_y_continuous(breaks = seq(0, 8000, 1000)) + facet_wrap(~Pora_Roku)



# Rozkład a miesiąc
zad4.2 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski")

ggplot(zad4.2, aes(x = Licznik, fill = Month)) + geom_histogram(binwidth = 800, show.legend = F, alpha = 0.8) + 
  facet_wrap(~Month) +
  scale_x_continuous(breaks = seq(0, 12000, 2000)) +  
  xlab("Liczba przejazdów") +
  ylab("Liczebność") +
  theme_bw() +
  ggtitle('Rozkłady liczby przejazdów w zależności od miesiąca') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

# Rozkład a dzień tygodnia
ggplot(zad4.2, aes(x = Licznik, fill = Dzien_tyg)) + geom_histogram(binwidth = 800, show.legend = F, alpha = 0.8) + 
  facet_wrap(~Dzien_tyg) +
  scale_x_continuous(breaks = seq(0, 12000, 2000)) +  
  xlab("Liczba przejazdów") +
  ylab("Liczebność") +
  ggtitle('Rozkłady liczby przejazdów w zależności od dnia tygodnia') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))


# Rozkład a weekend
ggplot(zad4.2, aes(x = Licznik, fill = Czy_Weekend)) + geom_histogram(binwidth = 800, show.legend = F, alpha = 0.8) + 
  facet_wrap(~Czy_Weekend) +
  scale_x_continuous(breaks = seq(0, 12000, 2000)) +  
  xlab("Liczba przejazdów") +
  ylab("Liczebność") +
  ggtitle('Rozkłady liczby przejazdów w zależności od typu dnia') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

# Rozkład a pora roku
ggplot(zad4.2, aes(x = Licznik, fill = Pora_Roku)) + geom_histogram(binwidth = 800, show.legend = F, alpha = 0.8) + 
  facet_wrap(~Pora_Roku) +
  scale_x_continuous(breaks = seq(0, 12000, 2000)) +  
  xlab("Liczba przejazdów") +
  ylab("Liczebność") +
  ggtitle('Rozkłady liczby przejazdów w zależności od pory roku') +
  scale_fill_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

# Boxplot (w raporcie od razu połączony z histogramem)

ggplot(zad4.2, aes(y = Licznik, fill = Czy_Weekend)) + geom_boxplot(show.legend = F, alpha = 0.8) + 
  facet_wrap(~Czy_Weekend) +
  scale_y_continuous(breaks = seq(0, 12000, 2000)) +
  ylab("Liczebność") +
  ggtitle('Rozkłady liczby przejazdów w zależności od dnia powszedniego lub weekendu') +
  theme_bw() +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        axis.text.x=element_blank(),
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))


# Polecenie 5.

# Porównanie wszystkich stacji
ggplot(data = przejazdy) +
  geom_line(stat = "summary",
            fun = mean,
            mapping =aes(x = Month, 
                         y = Licznik,
                         group = Dzien_tyg,
                         color = Dzien_tyg,
                         linetype = Czy_Weekend)) + 
  facet_wrap(facets = vars(Stacja),
             scales = "free_y") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        legend.title = element_text(size=10.5),
        legend.text = element_text(size=8.5),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="darkslateblue"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold")) +
  ylab("Liczebność") + xlab("Miesiąc") +
  ggtitle('Porównanie stacji w zależności od średniej liczby przejazdów')

# Porównanie wybranych stacji

zad5 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski" | Stacja == 'al. Grunwaldzka (UG)' |  
           Stacja == "al. Hallera" | Stacja == "ul. Jaśkowa Dolina")

ggplot(zad5, aes(x = Pora_Roku, y = Licznik, fill=Pora_Roku)) + geom_boxplot(size=1.2, alpha=0.5) + facet_grid(~Stacja) +
  theme_bw() + ylab("Licznik") + 
  ggtitle("Wybrane stacje ze względu na porę roku") +
  scale_fill_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.title = element_text(color="grey30", size=22, family="serif"))

ggplot(zad5, aes(x = Czy_Weekend, y = Licznik, fill=Czy_Weekend)) + geom_boxplot(size=1.2, alpha=0.5) + facet_grid(~Stacja) +
  theme_bw() + ylab("Licznik") + 
  ggtitle("Wybrane stacje ze względu na dzień tygodnia") +
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.title = element_text(color="grey30", size=22, family="serif"))

# Polecenie 6.
zad6 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski")

ggplot(zad6, aes(x = Temperatura, y = Licznik, color = Pora_Roku, shape = Czy_Weekend)) + geom_point(size = 3, alpha=0.7) +
  theme_minimal() +
  scale_color_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  ggtitle("Zależność liczby przejazdów i temperatury") + xlab("Temperatura [C]") + ylab("Liczba przejazdów") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="grey40"),
        plot.title = element_text(color="grey40", size=25, family="serif"))

ggplot(zad6, aes(x = Wilgotność, y = Licznik, color = Pora_Roku, shape = Czy_Weekend)) + geom_point(size = 3, alpha=0.7) +
  theme_minimal() +
  scale_color_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  ggtitle("Zależność liczby przejazdów i wilgotności") + xlab("Wilgotność [%]") + ylab("Liczba przejazdów") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="grey40", size=25, family="serif"))

ggplot(zad6, aes(x = Ciśnienie_stacja, y = Licznik, color = Pora_Roku, shape = Czy_Weekend)) + geom_point(size = 3, alpha=0.7) +
  theme_minimal() +
  scale_color_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  ggtitle("Zależność liczby przejazdów i ciśnienia powietrza") + xlab("Ciśnienie powietrza [hPa]") + ylab("Liczba przejazdów") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="grey40", size=25, family="serif"))

ggplot(zad6, aes(x = Zachmurzenie, y = Licznik, color = Pora_Roku, shape = Czy_Weekend)) + geom_point(size = 3, alpha=0.7) +
  theme_minimal() +
  scale_color_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  ggtitle("Zależność liczby przejazdów i zachmurzenia") + xlab("Zachmurzenie [oktanty]") + ylab("Liczba przejazdów") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="grey40", size=25, family="serif"))

ggplot(zad6, aes(x = Wiatr, y = Licznik, color = Pora_Roku, shape = Czy_Weekend)) + geom_point(size = 3, alpha=0.7) +
  theme_minimal() +
  scale_color_manual(values = c('sandybrown', 'firebrick3', 'palegreen3', 'lavender')) +
  ggtitle("Zależność liczby przejazdów i siły wiatru") + xlab("Wiatr [m/s]") + ylab("Liczba przejazdów") +
  theme(axis.title.y = element_text(color="Grey23", size=15),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="Grey23", size=15),
        axis.text.x = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="grey40", size=25, family="serif"))



# Polecenie 7.
zad7 <- przejazdy %>%
  filter(Stacja == "Pas Nadmorski" | Stacja == "ul. Kartuska" | Stacja == "al. Hallera" |
           Stacja == "ul. Jaśkowa Dolina" | Stacja == "ul. Wita Stwosza" | Stacja == "ul. Sucharskiego")

chartZad7 <- ggplot(zad7, aes(x = Temperatura, y = Licznik, color = Stacja, size = Wiatr)) + 
  geom_point(alpha = 0.7, stroke = 1) + 
  theme_minimal() +
  labs(title = "Zależność temperatury od liczby przejazdów w czasie",
       x = "Temperatura",
       y = "Liczba przejazdów") +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        legend.title = element_text(size=10.5),
        legend.text = element_text(size=8.5),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey98",
                                         size=0.5, linetype="solid", 
                                         colour ="grey50"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold")) +
  scale_color_brewer(palette = 'Set2')

mojaAnimacja <- chartZad7 +
  transition_time(Data) +
  labs(subtitle = "Dzień: {frame_time}") +
  shadow_wake(wake_length = 0.1)

animate(mojaAnimacja, height = 750, width = 900, nframes= 150, fps = 15, duration = 40, end_pause = 30, res = 100)
anim_save("mojaAnimacja.gif")

# Polecenie 8.

statystyki <- przejazdy %>%
  group_by(Stacja) %>%
  summarise("Srednia" = mean(Licznik),
            "Odchylenie standardowe" = sd(Licznik),
            "Minimum" = min(Licznik),
            "Kwartyl 1" = quantile(Licznik, 0.25),
            "Mediana" = median(Licznik),
            "Kwartyl 3" = quantile(Licznik, 0.75),
            "Maksimum" = max(Licznik))

statystyki %>%
  kbl(caption = "Podstawowe statystyki dla stacji pomiarowych") %>%
  kable_styling(bootstrap_options = c("hover", "condensed")) %>%
  kable_paper()

punkty <- rename(punkty, Stacja = stacja)
punkty <- left_join(x = punkty,
                    y = statystyki,
                    by = "Stacja")

tmap_mode("view")
tm_shape(punkty) +
  tm_symbols(size = "Srednia", 
             scale = 6,
             col = "royalblue3",
             alpha = 0.7) +
  tm_basemap(providers$OpenStreetMap)

tmap_mode("view")
tm_shape(punkty) +
  tm_symbols(size = "Mediana", 
             scale = 6,
             col = "salmon3",
             alpha = 0.7) +
  tm_basemap(providers$OpenStreetMap)

tmap_mode("view")
tm_shape(punkty) +
  tm_symbols(size = "Maksimum", 
             scale = 6,
             col = "darkorchid2",
             alpha = 0.7) +
  tm_basemap(providers$OpenStreetMap)


# Polecenie 9.
zad9 <- przejazdy[, c(4:12)]
pogodaKor <- round(cor(zad9),2)
ggcorrplot(pogodaKor, hc.order = TRUE, type = "lower",
           outline.col = "white", colors = c('coral2', 'azure1', 'palegreen2'), lab = T)


a <- ggplot(zad9, aes(x = Zachmurzenie, y = Wilgotność)) + geom_hex(bins = 60) + 
  scale_fill_gradient(low = "lightsteelblue1", high = "springgreen3") +
  theme_minimal() + 
  ylab("Ciśnienie wody [hPa]") + xlab("Wilgotność [%]") +
  ggtitle('Zależność pomiędzy wilgotnością i zachmurzeniem') + 
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))


b <- ggplot(zad9, aes(x = Ciśnienie_stacja, y = Ciśnienie_morze)) + geom_hex(bins = 80) + 
  scale_fill_gradient(low = "khaki3", high = "darkorchid2") + 
  theme_minimal() + 
  ylab("Ciśnienie morza [hPa]") + xlab("Ciśnienie powietrza [hPa]") +
  ggtitle('Zależność pomiędzy ciśnienem morza i powietrza') +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

c <- ggplot(zad9, aes(x = Temperatura, y = Wilgotność)) + geom_hex(bins = 70) + 
  scale_fill_gradient(low = "deepskyblue2", high = "firebrick3") +
  theme_minimal() + 
  ylab("Wilgotność [%]") + xlab("Temperatura [C]") +
  ggtitle('Zależność pomiędzy wilgotnością i temperaturą') +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))


d <- ggplot(zad9, aes(x = Temperatura, y = Ciśnienie_woda)) + geom_hex(bins = 70)  + 
  scale_fill_gradient(low = "cadetblue2", high = "goldenrod3") +
  theme_minimal() + 
  ylab("Ciśnienie wody [hPa]") + xlab("Temperatura [C]") +
  ggtitle('Zależność pomiędzy ciśnienem wody i temperaturą') +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

grid.arrange(a,b,c,d)


a1 <- ggplot(przejazdy, aes(x = Month, y = Temperatura, fill = Month)) + geom_boxplot(alpha = 0.8) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold")) +
  ylab("Temperatura") + xlab("Miesiąc") +
  ggtitle('Rozkład wartości temperatur względem miesiąca')

b1 <- ggplot(przejazdy, aes(x = Month, y = Zachmurzenie, fill = Month)) + geom_boxplot(alpha = 0.8) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold")) +
  ylab("Zachmurzenie") + xlab("Miesiąc") +
  ggtitle('Rozkład wartości zachmurzenia względem miesiąca')

c1 <- ggplot(przejazdy, aes(x = Month, y = Wiatr, fill = Month)) + geom_boxplot(alpha = 0.8) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),                            
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "italic", size = 9),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold")) +
  ylab("Wiatr") + xlab("Miesiąc") +
  ggtitle('Rozkład wartości siły wiatru względem miesiąca')

grid.arrange(a1,b1,c1)

ggplot(przejazdy, aes(x = Temperatura, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "B") +  
  theme_ridges() + labs(title = 'Temperatury w zależności od miesiąca') +
  xlab('Temparatura') + ylab("Miesiąc")

ggplot(przejazdy, aes(x = Ciśnienie_stacja, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Ciś. [hPa]", option = "E") +  
  theme_ridges() + labs(title = 'Ciśnienie powietrza w zależności od miesiąca') + xlab("Ciśnienie powietrza") +
  ylab("Miesiąc")

ggplot(przejazdy, aes(x = Wilgotność, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Wilg. [%]", option = "D") +  
  theme_ridges() + labs(title = 'Wilgotność powietrza w zależności od miesiąca') + xlab("Wilgotność powietrza") +
  ylab("Miesiąc")

ggplot(zad9, aes(x = Wilgotność, y = Opady_dzień)) + geom_hex(bins = 70)  + 
  scale_fill_gradient(low = "lightblue3", high = "deeppink3") +
  theme_minimal() + 
  ylab("Suma opadów w ciągu dnia [mm]") + xlab("Wilgotność [%]") +
  ggtitle('Zależność pomiędzy sumą opadów w ciągu dnia i wilgotnością') +
  theme(axis.title.x = element_text(color="Grey40", size=16),
        axis.title.y = element_text(color="Grey40", size=16),
        plot.title = element_text(color="grey40", size=19, family="serif", face = "bold"))

# Kalendarz
doKalendarza <- przejazdy4[przejazdy4$Year == "2019" & przejazdy4$Ciśnienie_stacja > 1022.0, ]
unique(doKalendarza$Data)
yday(unique(doKalendarza$Data))

calendR(year = 2019,
        start = "M",
        special.days = c(3, 6, 7, 35,  36, 37, 45, 46, 47, 53, 54,  
                         55, 56, 57, 79, 80, 81, 82,  
                         86, 87, 88, 91, 101, 102, 103, 104, 105, 106, 107,
                        108, 109, 110, 111, 112, 132, 133, 134, 135, 150, 160, 
                        174, 175, 176, 233, 234, 235, 236, 237, 238, 255, 256, 257, 
                        263, 303, 304, 361, 362, 363),
        special.col = "deepskyblue3",
        low.col = "white",
        weeknames.col = "white",
        weeknames.size = 4,
        lty = 2,
        title = "Dni w 2019 roku z ciśnieniem powietrza powyżej 1022.0 hPa",
        title.size = 20,
        title.col = 'darkslateblue',
        mbg.col = 'lightgoldenrod3',
        bg.img = 'pressure.jpg',
        font.family = "sans",
        font.style = "plain") 
