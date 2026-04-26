##########################
# --- 0. Pacotes Necessários ---
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(sf)
library(tidyr)
library(tibble)
library(scales)


# --- 1. Dados de Colaboração (MANTIDOS DA MATRIZ 30x30 ANTERIOR) ---

nomes_paises_colab <- rownames(NetMatrix)
matriz_colab_raw <- as.matrix(NetMatrix)
matriz_colab <- apply(matriz_colab_raw, 2, function(x) gsub("\\.", "0", x))
matriz_colab <- apply(matriz_colab, 2, as.numeric)
rownames(matriz_colab) <- nomes_paises_colab
colnames(matriz_colab) <- nomes_paises_colab


# --- 2. Preparação dos Dados de Produção (NOVO VETOR) ---

production_vector <- results$Countries

production_data <- data.frame(
  Country = names(production_vector),
  production_count = as.numeric(production_vector)
)


# --- 3. Função de Harmonização de Nomes (CORRIGIDA PARA USA) ---

harmonizar_nomes <- function(country_name) {
  case_when(
    # Mapeamentos que PRECISAM de correção:
    country_name == "USA" ~ "United States of America",
    country_name == "UNITED KINGDOM" ~ "United Kingdom",
    country_name == "CZECH REPUBLIC" ~ "Czechia",
    
    # Países que já estão corretos ou são aceitos no formato 'rnaturalearth':
    country_name == "CHINA" ~ "China",
    country_name == "AUSTRALIA" ~ "Australia",
    country_name == "FRANCE" ~ "France",
    country_name == "BRAZIL" ~ "Brazil",
    country_name == "GERMANY" ~ "Germany",
    country_name == "JAPAN" ~ "Japan",
    country_name == "INDIA" ~ "India",
    country_name == "MEXICO" ~ "Mexico",
    country_name == "NETHERLANDS" ~ "Netherlands",
    country_name == "SPAIN" ~ "Spain",
    country_name == "FINLAND" ~ "Finland",
    country_name == "CANADA" ~ "Canada",
    country_name == "BELGIUM" ~ "Belgium",
    country_name == "DENMARK" ~ "Denmark",
    country_name == "COLOMBIA" ~ "Colombia",
    country_name == "PORTUGAL" ~ "Portugal",
    country_name == "NEW ZEALAND" ~ "New Zealand",
    country_name == "SWEDEN" ~ "Sweden",
    country_name == "ITALY" ~ "Italy",
    country_name == "AUSTRIA" ~ "Austria",
    country_name == "IRAN" ~ "Iran",
    country_name == "PHILIPPINES" ~ "Philippines",
    country_name == "ARGENTINA" ~ "Argentina",
    country_name == "SWITZERLAND" ~ "Switzerland",
    country_name == "ISRAEL" ~ "Israel",
    country_name == "THAILAND" ~ "Thailand",
    country_name == "INDONESIA" ~ "Indonesia",
    country_name == "NORWAY" ~ "Norway",
    country_name == "POLAND" ~ "Poland",
    country_name == "HUNGARY" ~ "Hungary",
    country_name == "SYRIA" ~ "Syria",
    country_name == "TURKEY" ~ "Turkey",
    country_name == "BANGLADESH" ~ "Bangladesh",
    country_name == "BELARUS" ~ "Belarus",
    country_name == "CHILE" ~ "Chile",
    country_name == "CUBA" ~ "Cuba",
    country_name == "EGYPT" ~ "Egypt",
    country_name == "HAITI" ~ "Haiti",
    country_name == "ICELAND" ~ "Iceland",
    country_name == "IRAQ" ~ "Iraq",
    country_name == "MOROCCO" ~ "Morocco",
    country_name == "MYANMAR" ~ "Myanmar",
    country_name == "NIGERIA" ~ "Nigeria",
    country_name == "PAKISTAN" ~ "Pakistan",
    country_name == "SAUDI ARABIA" ~ "Saudi Arabia",
    country_name == "SENEGAL" ~ "Senegal",
    country_name == "SLOVENIA" ~ "Slovenia",
    country_name == "SOUTH AFRICA" ~ "South Africa",
    country_name == "SRI LANKA" ~ "Sri Lanka",
    country_name == "URUGUAY" ~ "Uruguay",
    
    # Default: mantém o nome original (segurança)
    .default = country_name
  )
}

production_data <- production_data %>%
  mutate(Country_Harmonized = harmonizar_nomes(Country))


# --- 4. Preparação dos Dados do Mapa (Choropleth) e Centroides ---

world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Calcula Centroides para as Linhas
country_centroids <- world_map %>%
  st_centroid() %>%
  mutate(
    long = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2]
  ) %>%
  select(name, long, lat) %>%
  st_drop_geometry()

# Juntar os NOVOS dados de Produção ao mapa para a camada de coroplético
merged_data <- world_map %>%
  left_join(production_data, by = c("name" = "Country_Harmonized")) %>%
  # Países sem dados de produção recebem 0
  mutate(production_count = replace_na(production_count, 0)) 


# --- 5. Preparação dos Dados da Rede (Lines - baseada na matriz 30x30) ---

edge_data_raw <- as.data.frame(matriz_colab) %>%
  rownames_to_column("Source") %>%
  pivot_longer(
    cols = -Source, 
    names_to = "Target", 
    values_to = "Weight"
  )

final_edges <- edge_data_raw %>%
  filter(Source != Target) %>% 
  rowwise() %>%
  mutate(
    Pair_Key = paste(sort(c(Source, Target)), collapse = "-")
  ) %>%
  ungroup() %>%
  group_by(Pair_Key) %>%
  summarise(
    Source = first(Source),
    Target = first(Target),
    Total_Weight = sum(Weight),
    .groups = 'drop'
  ) %>%
  # FILTRO REQUERIDO: Colaboração total maior que 5
  filter(Total_Weight > 5) %>%
  # Harmoniza os nomes para buscar os centroides
  mutate(
    Source_Harmonized = harmonizar_nomes(Source),
    Target_Harmonized = harmonizar_nomes(Target)
  )

# Adiciona as Coordenadas (Centroides)
source_centroids <- country_centroids %>% 
  rename(long_start = long, lat_start = lat, Source_Harmonized = name)
target_centroids <- country_centroids %>% 
  rename(long_end = long, lat_end = lat, Target_Harmonized = name)

final_edges <- final_edges %>%
  left_join(source_centroids, by = "Source_Harmonized") %>%
  left_join(target_centroids, by = "Target_Harmonized") %>%
  filter(!is.na(long_start) & !is.na(long_end))


# --- 6. Geração do Gráfico Final (Com RColorBrewer 'Blues' Contínua) ---

# 🎨 Cores definidas:
# Cor de Fundo do Oceano (Mar)
AZUL_MAR <- "white"    # Azul bem clarinho
# Cor para Linhas
AMARELO_OURO <- "#FFD700" 

# Garante que o pacote RColorBrewer esteja carregado
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
library(RColorBrewer)

max_prod <- max(production_data$production_count)
# Define os breaks para a escala
breaks_prod <- c(0, 20, 50, 100, max_prod)

# Mapeamento para RColorBrewer:
# 1. Obter a paleta Blues completa (9 cores)
blues_palette_raw <- brewer.pal(n = 9, name = "Blues")
# 2. Estender a paleta para usar o número necessário de cores, garantindo o tom mais claro para o zero.
blues_palette_extended <- colorRampPalette(blues_palette_raw)(length(breaks_prod))


mapa_plot<- ggplot(data = merged_data) +
  
  # 1. Camada de Mapa (Coroplético - COR: Produção Científica)
  geom_sf(aes(fill = production_count), color = "gray20", linewidth = 0.1) +
  
  # 2. Camada de Rede (Linhas - PESO: Colaboração)
  geom_segment(
    data = final_edges,
    aes(
      x = long_start, 
      y = lat_start, 
      xend = long_end, 
      yend = lat_end, 
      linewidth = Total_Weight 
    ),
    color = AMARELO_OURO, 
    alpha = 0.7,   
    lineend = "round" 
  ) +
  
  # 3. Escala para o Coroplético (Cor de Fundo - Produção)
  # Usamos scale_fill_stepsn com a paleta Blues completa. 
  # O valor 0 será mapeado para a primeira cor (o azul mais claro).
  scale_fill_stepsn(
    colors = blues_palette_extended, 
    values = scales::rescale(breaks_prod),
    breaks = breaks_prod,
    name = "Nº de publicações",
    limits = c(0, max_prod),
    labels = comma,
    # Se algum país não for encontrado (NA), ele será o tom mais claro
    na.value = blues_palette_extended[1] 
  ) +
  
  # 4. Escala para a Espessura da Linha (Colaboração)
  scale_linewidth_continuous(
    name = "Colaboração internacional",
    range = c(0.5, 3.5), 
    breaks = c(10, 20, 30, 40) 
  ) +
  
  # 5. Tema e Rótulos
  theme_void() +
  theme(
    plot.background = element_rect(fill = AZUL_MAR, color = NA), 
    panel.background = element_rect(fill = AZUL_MAR, color = NA),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, color = "gray20"),
    plot.caption = element_text(color = "gray40")
  )
ggsave(filename = "mapa_plot.png",
       plot = mapa_plot,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300,
       scale = 2)
