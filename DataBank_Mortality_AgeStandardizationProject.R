library(dplyr)
library(tidyverse)
df_raw <- read.csv("firstWorldBankProject_RawData.csv")

# Rename Columns & General Data Organization
df <- df_raw %>% 
        rename(
                year = Year,
                var = Series.Name,
                US = United.States..USA.,
                EU = European.Union..EUU.,
                AU = Australia..AUS.,
                LA = Latin.America...Caribbean..LCN.,
                SSA = Sub.Saharan.Africa..SSF.,
                ME = Middle.East..North.Africa..Afghanistan...Pakistan..MEA.,
                asia = East.Asia...Pacific..EAS.,
                euro = Europe...Central.Asia..ECS.
        ) %>%
        select(
                -Year.Code,
                -Series.Code
        ) %>%
        filter(
                var == "Population ages 00-14, total" |
                        var == "Population ages 15-64, total" |
                        var == "Population ages 65 and above, total" |
                        var == "Death rate, crude (per 1,000 people)"
        ) %>%
        filter(
                year != 2024
        )
df$var[df$var == "Population ages 00-14, total"] <- "0-14"
df$var[df$var == "Population ages 15-64, total"] <- "15-64"
df$var[df$var == "Population ages 65 and above, total"] <- "65+"
df$var[df$var == "Death rate, crude (per 1,000 people)"] <- "mortality"
df$US <- as.numeric(df$US)
df$EU <- as.numeric(df$EU)
df$AU <- as.numeric(df$AU)
df$LA <- as.numeric(df$LA)
df$SSA <- as.numeric(df$SSA)
df$ME <- as.numeric(df$ME)
df$asia <- as.numeric(df$asia)
df$euro <- as.numeric(df$euro)

# Defining Standard Mortality Rates (USA | 2023 | CDC)
stdMort <- data.frame(
        age = c("1-14", "15-64", "65+"),
        # mort = c(0.000147, 0.00850, 0.02050)
        mort = c(15/100000, 450/100000, 6000/100000)
)

# Indirect Age Standardization - Calculating Expected Mortality
std <- function(colString) {
        young <- seq(1, 96, by = 4)
        middle <- seq(2, 96, by = 4)
        old <- seq(3, 96, by = 4)
        mort <- seq(4, 96, by = 4)
        
        country <- numeric(96)
        countrymort <- numeric(96/4)
        country[young] <- df[young,colString] * stdMort[1,2]
        country[middle] <- df[middle,colString] * stdMort[2,2]
        country[old] <- df[old,colString] * stdMort[3,2]
        for (i in mort) {
                poptot <- numeric(96/4)
                poptot[i/4] <- sum(df[i-3,colString], df[i-2,colString], df[i-1,colString])
                country[i] <- sum(country[i-3], country[i-2], country[i-1]) / poptot[i/4] * 1000
        }
        return(country)
}

df$USe <- std("US")
df$EUe <- std("EU")
df$AUe <- std("AU")
df$LAe <- std("LA")
df$SSAe <- std("SSA")
df$MEe <- std("ME")
df$asiae <- std("asia")
df$euroe <- std("euro")

# Cleaning Up The Data
df2 <- df %>%
        filter(
                var == "mortality"
        ) %>%
        select(
                -var
        )

# Plotting The Data
library(ggplot2)
library(ggrepel)
library(ggtext)

# Helper function to add lines and points with correct colors
add_line <- function(df, colname, label) {
        list(
                geom_line(aes_string(y = colname, color = shQuote(label)), size = 1),
                geom_point(aes_string(y = colname, color = shQuote(label)))
        )
}

# Calculate SMR values and reshape the data
dfsmr <- df2
dfsmr$USe <- dfsmr$US / dfsmr$USe
dfsmr$EUe <- dfsmr$EU / dfsmr$EUe
dfsmr$AUe <- dfsmr$AU / dfsmr$AUe
dfsmr$LAe <- dfsmr$LA / dfsmr$LAe
dfsmr$SSAe <- dfsmr$SSA / dfsmr$SSAe
dfsmr$MEe <- dfsmr$ME / dfsmr$MEe
dfsmr$asiae <- dfsmr$asia / dfsmr$asiae
dfsmr$euroe <- dfsmr$euro / dfsmr$euroe
dfsmr <- dfsmr %>%
        select(
                year, USe, MEe, SSAe, LAe, AUe, euroe, asiae, EUe
        ) %>%
        rename(
                US = USe,
                ME = MEe,
                SSA = SSAe,
                LA = LAe,
                AU = AUe,
                euro = euroe,
                asia = asiae,
                EU = EUe
        ) %>%
        gather(
                country, SMR, -year
        ) %>%
        rename(
                Country = country
        )

# Making the plot (SMR)
caption_text <- paste(
        "**Figure 2:** Annual standardized mortality ratios are provided from an indirect age-standardization analysis where the",
        "random but realistic standard mortality rates were selected. Therefore, the y-axis represents",
        "the ratio between observed mortality and expected mortality if the standard mortality rates were to equal the real mortality rates for",
        "that country. Sub-Saharan Africa historically has had the highest age-standardized",
        "mortality rates, fortunately with signs of amelioration."
)

# Wrap text and convert newlines to <br>
wrapped_caption <- str_wrap(caption_text, width = 150)
wrapped_caption <- gsub("\n", "<br>", wrapped_caption)

(p_smr <- ggplot(dfsmr, aes(x = year, y = SMR, group = Country, color = Country)) + 
        geom_line(size = 1) + 
        geom_point(size = 2) +
        scale_color_brewer(
                palette = "Paired",
                labels = c("East Asia", "Australia", "European Union", "Europe & Central Asia",
                           "Latin America", "Middle East", "Sub-Saharan Africa", "United States")
        ) +
        theme_classic(base_size = 15) +
        theme(
                legend.position = c(0.9, 0.85),
                legend.background = element_rect(fill = "gray85"),
                plot.title = ggtext::element_markdown(size = 20, hjust = 0),
                plot.caption = ggtext::element_markdown(hjust = 0, size = 12, lineheight = 1.1),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) +
        labs(
                x = "Year",
                y = "SMR (Observed Mortality / Expected Mortality)",
                title = "**Age-Standardized Country Mortality Rates**",
                subtitle = "DataBank | The World Bank Group",
                caption = wrapped_caption
        ))

ggsave(filename = "p_smr.svg", 
       plot = p_smr,
       width = 30, height = 21, units = "cm",
       dpi = 600)


# Making the plot (crude data)
df <- df %>%
        select(
                year, var, US, EU, AU, LA, SSA, ME, asia, euro
        ) %>%
        filter(
                var == "mortality"
        ) %>%
        select(
                -var
        ) %>%
        gather(
                country, mortality, -year
        ) %>%
        rename(
                Country = country
        )

caption_text <- paste(
        "**Figure 1:** Crude mortality rates prior to indirect age-standardization.",
        "Reasonable comparisons cannot be drawn because the countries have distinct",
        "age structures."
)

# Wrap text and convert newlines to <br>
wrapped_caption <- str_wrap(caption_text, width = 150)
wrapped_caption <- gsub("\n", "<br>", wrapped_caption)

(p_crude <- ggplot(df, aes(x=year, y=mortality, group=Country, color=Country)) + 
        geom_line(size = 1) + geom_point(size = 2) +
        scale_color_brewer(
                palette="Paired",
                labels = c("East Asia", "Australia", "European Union", "Europe & Central Asia", 
                           "Latin America", "Middle East", "Sub-Saharan Africa", "United States")) +
        theme_classic(base_size = 15) +
        theme(legend.position = c(0.80, 0.90),
              legend.background = element_rect(fill = "gray85"),
              plot.title = ggtext::element_markdown(size = 20, hjust = 0),
              plot.caption = ggtext::element_markdown(hjust = 0, size = 12, lineheight = 1.1),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold")
              ) +
        labs(
                x = "Year",
                y = "Annual Mortality Rate per 1,000",
                title = "**Annual Country Crude Mortality Rates**",
                subtitle = "DataBank | The World Bank Group",
                caption = wrapped_caption
        ) +
        guides(color = guide_legend(ncol = 2)))
ggsave(filename = "p_crude.svg", 
       plot = p_crude,
       width = 30, height = 21, units = "cm", 
       dpi = 600)
























