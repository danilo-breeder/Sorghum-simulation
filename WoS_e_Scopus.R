######Analise Bibliometrica R Bibliometrix ######

############## Misturando as duas bases de dados WoS e Scopus #########
# O Scopus nao permite analise de citacoes tao completa, por isso sera a analise de citacoes
# sera feita apenas com base nos dados da WoS


#Pacotes para instalar caso necessario
#install.packages("bibliometrix")
#install.packages("remotes")
#library(remotes)
#remotes::install_github("massimoaria/bibliometrix")


##### Pacotes utilizados #####
library(bibliometrix)
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)
library(forcats)
library(viridis)
library(wordcloud)
library(ggpubr)
library(tidyverse)

#### Análise Formal ####

setwd("~/Analise.bibliometrica.WoSeScopus")

# WoS
M <- convert2df("wos.bib", dbsource = "isi", format = "bibtex")
M <- M %>%
  filter(PY<2025)  # Retirando as publicacoes de 2025 da Analise

# Scopus 
S <- convert2df('scopus.bib', dbsource = "scopus", format = "bibtex")

# Merged 

database = mergeDbSources(M, S, remove.duplicated = T)
##write.xlsx(database, "Merged.WoS.Scopus.xlsx")

# Analise #
results <- biblioAnalysis(database, sep = ";")
options(width = 100)
resumo <- summary(object = results, k=10, pause= FALSE)

# Main Information
resumo$MainInformationDF
#write.xlsx(resumo$MainInformationDF, "main.information.xlsx")

# Collaboration index (CI). E calculado como o Co-author per doc, contando
# apenas os artigos com mais de um autor
(CI = (2821-37)/(733-37))

#Collaboration Coefficient
(CC = 1 - (733/2821))

# Indice H
(indice_h <- Hindex(database, field = "author", elements = NULL, sep = ";")$H)
(most_influencial_author <- indice_h %>%
  filter(g_index>=11))
#write.xlsx(indice_h, "indice_h.xlsx")

(most_influencial_author = most_influencial_author %>%
  mutate(Element = fct_reorder(Element, g_index)))
(df_most_influencial <- gather(most_influencial_author,
                              event, total, h_index:g_index)) #Create long format

niveis.para.ggarrange = levels(df_most_influencial$Element)
(plot.most.influencial <- ggplot(df_most_influencial,
                                 aes(y = Element,
                                     x = total,
                                     fill = event)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    labs(x = NULL, 
         fill = NULL, 
         y = "Autor") +
    scale_fill_discrete(labels = c("Indice g", "Indice h")) + 
    
    # --- Início dos Ajustes de Espaço (Otimização) ---
    guides(fill = guide_legend(
      # Reduz o tamanho dos quadrados (chaves)
      keywidth = 0.4, 
      keyheight = 0.4, 
      # Opcional: move o título para o topo
      title.position = "top", 
      title.hjust = 0.5 
    )) +
    theme(
      # Posição e Orientação
      legend.position = "bottom",
      legend.direction = "horizontal",
      
      # Redução da Fonte (mantendo o que já funcionou)
      legend.title = element_text(size = 7), # Reduzindo um pouco mais o título
      legend.text = element_text(size = 7),  # Reduzindo um pouco mais o texto
      
      # Remove a margem externa ao redor do box da legenda
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      
      # Remove o espaço entre os itens na horizontal
      legend.spacing.x = unit(0.1, 'cm'),
      
      # Remove o espaço entre as linhas na vertical (se fosse multi-linha)
      legend.spacing.y = unit(0.1, 'cm') 
    )+
    theme(
      # Remove os nomes dos autores (rótulos do eixo vertical, que é o 'axis.text.x' original)
      axis.text.y = element_blank(), 
      # Remove o título do eixo Y (o que era o X original: "Autor")
      axis.title.y = element_blank(), 
      # Opcional: Remove a linha do eixo Y para um visual mais limpo
      axis.line.y = element_blank(),
      # Remove os "ticks" do eixo Y
      axis.ticks.y = element_blank()
    ) +
    # Remove o título do eixo Y (Autores) no labs, que já é o padrão da sua função, 
    # mas garante que não há nenhum título residual.
    labs(y = NULL)
)

(plot.most.influencial_alinhado <- ggplot(df_most_influencial,
                                          aes(y = Element,
                                              x = total,
                                              fill = event)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    # Removendo o título Y ("Autor")
    labs(x = NULL, fill = NULL, y = NULL) + 
    scale_fill_discrete(labels = c("Indice g", "Indice h")) +
    
    # --- Tema e Ajustes de Legenda ---
    guides(fill = guide_legend(
      keywidth = 0.4, keyheight = 0.4,
      title.position = "top", title.hjust = 0.5
    )) +
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.spacing.y = unit(0.1, 'cm'),
      
      # --- AJUSTES ESSENCIAIS PARA ALINHAMENTO ---
      # Remove os nomes dos autores (rótulos do eixo Y)
      axis.text.y = element_blank(), 
      # Remove as "marcas" do eixo Y
      axis.ticks.y = element_blank() 
    )
)

A4_WIDTH_IN = 8.27
A4_HEIGHT_IN = 11.69
ggsave(
  filename = "autoresindicegh.png",
  plot = plot.most.influencial,
  width = A4_WIDTH_IN/2,
  height = A4_HEIGHT_IN/4,
  units = "in",        # Unidades em polegadas
  dpi = 300            # Resolução padrão de alta qualidade
)

(productionovertime = authorProdOverTime(database, k=11))
resultados <- authorProdOverTime(M = database, graph = FALSE)

p_prod_over_time <- resultados$graph # **CORREÇÃO DO ERRO 1: EXTRAIR O GRÁFICO DA LISTA**

# 2. Gráfico da Direita (plot.most.influencial_alinhado)
#    (Usando o código de alinhamento que removeu os rótulos do eixo Y)

(plot.most.influencial_alinhado <- ggplot(df_most_influencial,
                                          aes(y = Element, x = total, fill = event)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = NULL, fill = NULL, y = NULL) + 
    scale_fill_discrete(labels = c("Indice g", "Indice h")) +
    guides(fill = guide_legend(keywidth = 0.4, keyheight = 0.4,
                               title.position = "top", title.hjust = 0.5)) +
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_text(size = 7), legend.text = element_text(size = 7),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      legend.spacing.x = unit(0.1, 'cm'), legend.spacing.y = unit(0.1, 'cm'),
      
      # AJUSTES ESSENCIAIS PARA ALINHAMENTO
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank() 
    )
)

# 3. Combinar os gráficos com alinhamento forçado
#    **CORREÇÃO DO ERRO 2: ADICIONAR axis = "l"**

library(ggpubr) 

# Combine os gráficos, garantindo o alinhamento
ggarrange(
  p_prod_over_time,                  
  plot.most.influencial_alinhado,    
  ncol = 2, 
  align = "v",                       
  axis = "l"                         
)

library(patchwork)
library(patchwork)
library(ggplot2) # Necessário para o element_text() e theme()

library(patchwork)
library(ggplot2) 

(p_prod_over_time + plot.most.influencial_alinhado) +
  
  # PASSO 1: Configurar o layout (AQUI SÓ FICAM ARGUMENTOS DE LAYOUT)
  plot_layout(
    guides = "collect",          # Coleta todas as legendas em um único bloco
    ncol = 2,                   
    widths = c(1.5, 1)          # (Opcional) Ajuste de largura relativa
  ) & 
  
  # PASSO 2: Aplicar o Tema Global (AQUI VÃO TODOS OS ARGUMENTOS DE THEME, INCLUINDO A POSIÇÃO DA LEGENDA)
  theme(
    # Move o bloco de legenda unificado para a parte inferior
    legend.position = "bottom", 
    
    # Reduz o tamanho da fonte do título da legenda
    legend.title = element_text(size = 8), 
    # Reduz o tamanho da fonte dos rótulos da legenda
    legend.text = element_text(size = 7),  
    # Reduz o tamanho da margem (mantido dos ajustes anteriores)
    legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0, unit = "cm")
  )

# A sintaxe de combinação é muito mais simples
p_prod_over_time + plot.most.influencial_alinhado +
  # Adiciona um tema geral ao layout (opcional)
  plot_layout(guides = "collect", ncol = 2)



# Numero de Artigos Citados
(NCP = length(results$MostCitedPapers$TC) - sum(results$MostCitedPapers$TC<= 0))
(PCP = 100* sum(results$MostCitedPapers$TC> 0) / length(results$MostCitedPapers$TC))

just.cited.pubs = results$MostCitedPapers %>% filter(TC> 0)
(CCP = sum(just.cited.pubs$TC) / length(just.cited.pubs$TC))


# Grafico de Categorias Stacked Bar Plot #
library(ggplot2)
library(tidyverse)
#install.packages("tidyverse")
#install.packages("ggplot2")

#### Leitura da Planilha #####
library(readxl)
data <-read_excel("Complete.information.bibliography.no.duduplicityarea.xlsx.xlsx", 
                  sheet = "savedrecs")
data = data %>%
  filter(Publication.Year < 2025)
###### Agregando
# De acordo com publicacoes e categorias
(df <- data %>%
    count(Publication.Year, WoS.Categories))

#De acordo com publicacoes
(df1 <- data %>%
    count(Publication.Year))

# De acordo comcategorias
(df2 <- data %>%
    count(WoS.Categories))

#### Construindo o gráfico de publicações por ano de acordo com cada categoria ####

ggplot(data = df, aes(x=Publication.Year, y=n, fill = WoS.Categories  )) + geom_bar(stat = "identity")+
  scale_x_continuous("Publication Year", labels = as.character(df$Publication.Year), breaks = df$Publication.Year)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(fill = "WoS Category", y= "Number of Publications")+
  scale_fill_viridis(discrete = T)
#ggsave("category.plot.png", scale = 1.5,dpi = 600)


### Grafico de Afiliacoes mais relevantes ###

#Inicialmente precisei corrigir as principais afiliacoes pois tem univ com o mesmo nome
# sendo citada de formas diferentes 'UNIV' e 'UNIVERSITY';
write.xlsx(results$Affiliations, "affiliations.xlsx")
write.xlsx(results$Aff_frac, "affiliations_frac.xlsx")

df_aff = read_excel("top15inst.padronizadas.xlsx")


df_aff = df_aff %>%
  filter(Frequency > 8.0)

df_aff = df_aff %>%
  mutate(Affiliation = fct_reorder(Affiliation, Frequency))

ggplot(data = df_aff, aes(y = Affiliation, x = Frequency, fill = Affiliation)) +
  geom_bar(stat = "identity", width = 0.5) +
  guides(fill = "none") +  # Remove a legenda
  theme_bw() +             # Define o fundo como branco com linhas de grade suaves
  theme(
    axis.text.y = element_text(color = "black"), # Garante que as letras do eixo Y sejam pretas
    panel.grid.major = element_blank(),          # Opcional: remove as linhas de grade principais
    panel.grid.minor = element_blank()           # Opcional: remove as linhas de grade secundárias
  ) +
  labs(x = "Frequência",
       y= "Afiliação")
ggsave("princ.affiliation.plot.png", scale = 1.5,dpi = 300)


### WordCloud ####
df_key = as.data.frame(results$DE)
wordcloud(words = df_key$Tab, freq = df_key$Freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.1,
          colors=brewer.pal(8, "Dark2"))


### Grafico de Paises Mais Influentes ####

df_country <- as.data.frame(results$Countries) %>%
  filter (Freq >= 10)

ggplot(df_country, aes(x = Tab, y = Freq, size = Freq, colour = Tab))+
  geom_segment(aes(x= Tab, xend = Tab, y=0, yend = Freq), size = 1, colour = "gray")+
  geom_point() +
  scale_size_area(max_size = 12)+
  coord_flip()+
  guides(colour = FALSE, size = FALSE)

ggsave("princ.countries.plot.png", scale = 1.5,dpi = 600, width = 4,
       height = 2.5)


### Co-citation network ###
cocitationnetwork = biblioNetwork(M, analysis = "co-citation", network = "references")
networkPlot(cocitationnetwork, n = 50, type = "kamada", Title = "Co-citation Network",labelsize=1)

### Coupling Network ###
couplingnetwork = biblioNetwork(M, analysis = "coupling", network = "references")
networkPlot(couplingnetwork, n = 50, type = "kamada", Title = "Co-citation Network",labelsize=1)


#### NetMatrix #####
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=2)
?biblioNetwork


#### Biblioshiny ####
#Biblioshiny
biblioshiny()


data(scientometrics, package = "bibliometrixData")
NetMatrix <- biblioNetwork(scientometrics, analysis = "collaboration", 
                           network = "countries", sep = ";")

net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5)


M_countries <- metaTagExtraction(scientometrics, Field = "AU_CO")


##### Criando Mapa ###########

# Option C: Manually create a choropleth world map using ggplot2 (Advanced)
# This involves joining your data to spatial map data from the 'maps' package.
# This requires significant data wrangling to match country names/codes accurately. 
# An example snippet is as follows:


M_countries <- metaTagExtraction(database, Field = "AU_CO")
NetMatrix <- biblioNetwork(M_countries, analysis = "collaboration", network = "countries", sep = ";")
country_data <- data.frame(Country = rownames(NetMatrix), collaboration_count = rowSums(NetMatrix))
world_map_data <- map_data("world")
merged_data <- left_join(world_map_data, country_data, by = c("region" = "Country"))
ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
   geom_polygon(aes(fill = collaboration_count), color = "black") + 
   scale_fill_viridis_c(option = "plasma") + 
   theme_minimal() + labs(title = "Country Collaboration Map")




################# Analise do Brasil #####################
M_brasil_inclusivo <- database[grepl("BRAZIL|BRASIL", database$C1, ignore.case = TRUE), ]
write.xlsx(M_brasil_inclusivo, file = "artigos.brasil.xlsx")


###### Codigo para padronizar as fontes #####
# Supondo que seu vetor se chame 'vetor_fontes'
vetor_fontes <- database$SO # seus dados aqui

# 1. Corrigir o escape da barra invertida (\\&)
vetor_fontes <- gsub("\\\\\\&", "&", vetor_fontes)

# 2. Padronizar o periódico G3 (que possui muitas variações)
# Isso agrupa: G3: GENES, GENOMES, GENETICS / G3-GENES GENOMES GENETICS / G3 (BETHESDA, MD.)
vetor_fontes[grepl("^G3", vetor_fontes)] <- "G3: GENES, GENOMES, GENETICS"

# 3. Padronizar "AND" vs "&" em periódicos específicos
vetor_fontes <- gsub("CROP AND PASTURE SCIENCE", "CROP & PASTURE SCIENCE", vetor_fontes)
vetor_fontes <- gsub("AUSTRALIAN AND NEW ZEALAND JOURNAL OF STATISTICS", "AUSTRALIAN & NEW ZEALAND JOURNAL OF STATISTICS", vetor_fontes)

# 4. Remover volumes ou complementos que aparecem grudados ao nome (Ex: Vol 58, Vol 116)
vetor_fontes <- gsub(", VOL \\d+", "", vetor_fontes)

# 5. Limpar espaços extras que podem ter sobrado
vetor_fontes <- trimws(vetor_fontes)

# Visualizar o resultado
unique(vetor_fontes)
write.xlsx(vetor_fontes, "sources.corrigido.xlsx")

biblioshiny()



##### Fontes mais relevantes ######
library(ggplot2)
library(dplyr)

# 1. Criar o dataframe de contagem (substitua 'vetor_fontes' pelo seu vetor limpo)
df_sources <- as.data.frame(table(vetor_fontes)) %>%
  rename(Sources = vetor_fontes, n = Freq) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) # Filtrar as 10 principais como no exemplo

# 2. Gerar o gráfico
grafico_fontes = ggplot(df_sources, aes(x = n, y = reorder(Sources, n))) +
  # Desenha a linha cinza (o cabo do pirulito)
  geom_segment(aes(x = 0, xend = n, y = reorder(Sources, n), yend = reorder(Sources, n)), 
               color = "grey50") +
  # Desenha o círculo azul com o valor dentro
  geom_point(aes(size = n), color = "#31688E") + 
  # Adiciona os números dentro dos círculos
  geom_text(aes(label = n), color = "white", size = 3, fontface = "bold") +
  # Estética e Cores
  scale_size_continuous(range = c(5, 12)) + # Ajusta o tamanho das bolinhas
  labs(x = "Nº de Publicações", y = "Periódico") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), # Remove as grades horizontais para destacar o "pirulito"
    axis.text.y = element_text(color = "black", size = 9),
    axis.text.x = element_text(color = "grey30"),
    legend.position = "none" # Remove a legenda do tamanho
  )
ggsave("sources.plot.png", grafico_fontes, width = 15, height = 10, units = "cm", dpi = 300, scale = 1.2)
