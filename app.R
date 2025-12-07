# --- app.R: Dashboard Shiny PM2.5 Bahia COMPLETO (FINAL ESTÁTICO) ---

# --- 0. Bibliotecas Necessárias ---
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly) # Mantemos no library, mas só é usado se usarmos renderPlotly
library(readxl) 
library(lubridate) 
library(reshape2) 
library(janitor) 
library(sf)     
library(geobr)  
library(tmap)   
library(DT)     
library(stringr)

# Cores da Bandeira da Bahia
COR_AZUL <- "#007F56" 
COR_BRANCO <- "#FFFFFF" 
COR_VERMELHO <- "#FF0000"
COR_AMARELO <- "#FFCB05"

# =======================================================
# --- 1. BLOCO DE TRATAMENTO DE DADOS E GEOLOCALIZAÇÃO ---
# =======================================================

# A. LEITURA DOS DADOS
dadoscams2023 <- read.csv2("daily_pm25_all_regions_2023.csv", header = TRUE, stringsAsFactors = FALSE)
dadoscams2022 <- read.csv2("daily_pm25_all_regions_2022.csv", header = TRUE, stringsAsFactors = FALSE)

# Leitura e limpeza inicial Donkelar
dadosdon2023 <- read_excel("Donkelar_dados_completos_consolidado_2023.xlsx") %>% janitor::clean_names()
dadosdon2022 <- read_excel("Donkelar_dados_completos_consolidado_2022.xlsx") %>% janitor::clean_names()


# B. PROCESSAMENTO DOS DADOS CAMS
processar_cams_anual <- function(dados, ano_base) {
  long <- dados %>%
    reshape2::melt(id.vars="Date") 
  
  colnames(long) <- c("Date", "CodRes", "media_pm25_diaria")
  
  long <- long %>%
    mutate(
      cd_mun = as.character(substring(CodRes, 4, 10)),
      date_raw = lubridate::dmy(Date), 
      ano = lubridate::year(date_raw),
      mes = lubridate::month(date_raw),
      media_pm25_diaria = as.numeric(gsub(",", ".", as.character(media_pm25_diaria)))
    )
  
  long <- long %>%
    group_by(cd_mun, ano, mes) %>%
    summarise(
      media_pm25 = mean(media_pm25_diaria, na.rm = TRUE),
      desvio_padrao_pm25 = sd(media_pm25_diaria, na.rm = TRUE),
      min_pm25 = min(media_pm25_diaria, na.rm = TRUE),
      max_pm25 = max(media_pm25_diaria, na.rm = TRUE), 
      .groups = 'drop'
    ) %>%
    mutate(Fonte = "CAMS", cd_uf = substr(cd_mun, 1, 2))
  return(long)
}

dadoscams_2022_proc <- processar_cams_anual(dadoscams2022, 2022)
dadoscams_2023_proc <- processar_cams_anual(dadoscams2023, 2023)

dadoscams_agr <- bind_rows(
  dadoscams_2022_proc,
  dadoscams_2023_proc
) %>%
  filter(!is.nan(media_pm25) & !is.na(media_pm25))


# C. PROCESSAMENTO DOS DADOS DONKELAR
cols_padrao <- c("cd_mun", "nm_mun", "sigla_uf", "media_pm25", "desvio_padrao_pm25", "min_pm25", "max_pm25", "ano", "mes")

processar_donkelar <- function(dados) {
  
  if ("sigla_uf_2" %in% names(dados)) {
    dados <- dados %>% rename(sigla_uf = sigla_uf_2) 
  }
  
  dados <- dados %>%
    select(any_of(cols_padrao)) %>%
    mutate(
      cd_mun = as.character(cd_mun), 
      sigla_uf = as.character(sigla_uf), 
      media_pm25 = as.numeric(as.character(media_pm25)) 
    ) %>%
    drop_na(media_pm25) 
  
  if (!"sigla_uf" %in% names(dados)) {
    dados <- dados %>% mutate(sigla_uf = NA_character_)
  }
  
  return(dados)
}

dadosdon <- bind_rows(
  processar_donkelar(dadosdon2022),
  processar_donkelar(dadosdon2023)
) %>%
  mutate(
    Fonte = "Donkelar", 
    sigla_uf = toupper(sigla_uf), 
    cd_uf = substr(cd_mun, 1, 2) 
  ) %>%
  select(all_of(cols_padrao), Fonte, cd_uf) %>% 
  drop_na(media_pm25) %>%
  # Limpeza Robusta para Donkelar antes da junção:
  filter(nchar(cd_mun) == 7, !is.na(as.numeric(cd_mun)))


# D. COMBINAR AS DUAS FONTES E FILTRAR PARA BAHIA (BA)
cols_comuns <- c("cd_mun", "cd_uf", "media_pm25", "desvio_padrao_pm25", "min_pm25", "max_pm25", "ano", "mes", "Fonte")

dados_don_cams_mes_full <- bind_rows(
  dadosdon %>% select(all_of(cols_comuns), nm_mun, sigla_uf),
  dadoscams_agr %>% select(all_of(cols_comuns))
)

lookup_mun <- dados_don_cams_mes_full %>% 
  distinct(cd_mun, .keep_all = TRUE) %>% 
  select(cd_mun, nm_mun, sigla_uf) %>% 
  drop_na(cd_mun)

# DATASET FINAL USADO NO SHINY: dados_bahia
dados_bahia <- dados_don_cams_mes_full %>%
  left_join(lookup_mun, by = "cd_mun", suffix = c("", ".lookup")) %>%
  mutate(
    nm_mun = coalesce(nm_mun, nm_mun.lookup), 
    sigla_uf = coalesce(sigla_uf, sigla_uf.lookup)
  ) %>%
  select(-nm_mun.lookup, -sigla_uf.lookup) %>%
  filter(cd_uf == "29") %>% 
  drop_na(cd_mun, nm_mun, media_pm25)


# E. PREPARAÇÃO DE DADOS GEOGRÁFICOS (Para Mapas)
limites_bahia <- geobr::read_municipality(code_muni = "BA", year = 2020) %>% 
  janitor::clean_names() %>%
  # CORREÇÃO CRÍTICA DO MAPA: Renomear e forçar tipo CHARACTER nas chaves
  select(geom, code_muni, name_muni) %>%
  rename(COD_MUNICIPIO = code_muni,
         MUNICIPIO_NOME = name_muni) %>%
  mutate(COD_MUNICIPIO = as.character(COD_MUNICIPIO),
         MUNICIPIO_NOME = as.character(MUNICIPIO_NOME))

# Calcula a média anual consolidada
dados_anuais_mapa <- dados_bahia %>%
  group_by(cd_mun, Fonte, ano) %>%
  summarise(
    pm25_anual_medio = mean(media_pm25, na.rm = TRUE),
    .groups = 'drop'
  )

# Junta os dados de PM2.5 com os limites geográficos
mapa_dados_bahia <- limites_bahia %>%
  left_join(dados_anuais_mapa %>% mutate(cd_mun = as.character(cd_mun)), 
            by = c("COD_MUNICIPIO" = "cd_mun"))


# =======================================================
# --- 2. USER INTERFACE (UI) ---
# =======================================================
ui <- fluidPage(
  # Estilo personalizado com as cores da Bahia
  tags$head(
    tags$style(HTML(paste0("
            .main-header {
                background-color: ", COR_AZUL, ";
                color: ", COR_BRANCO, ";
                padding: 10px;
                text-align: center;
                font-weight: bold;
                font-size: 24px;
            }
            .sidebar-panel {
                background-color: #f5f5f5;
                padding: 15px;
            }
        ")))
  ),
  
  # Cabeçalho
  div(class = "main-header", 
      HTML("ANÁLISE INTERATIVA DE PM2.5 – ESTADO DA BAHIA (2022-2023)")
  ),
  
  # Layout Principal
  sidebarLayout(
    # Barra Lateral (Controles de Filtro)
    sidebarPanel(class = "sidebar-panel",
                 h4("Controles de Análise"),
                 
                 # Filtro de Fonte de Dados
                 selectInput("fonte_selecionada", "Fonte de Dados (Para todos os gráficos/mapas):",
                             choices = unique(dados_bahia$Fonte),
                             selected = "Donkelar"),
                 
                 # Filtro de Ano
                 checkboxGroupInput("ano_selecionado", "Ano (Para todos os gráficos/mapas):",
                                    choices = unique(dados_bahia$ano),
                                    selected = unique(dados_bahia$ano)),
                 
                 # Filtro de Município (APENAS para Boxplot Sazonal Detalhado)
                 selectInput("municipio_selecionado", "Município (Para Detalhe do Boxplot):",
                             choices = sort(unique(dados_bahia$nm_mun)), 
                             selected = "Salvador") 
    ),
    
    # Painel Principal (Gráficos)
    mainPanel(
      tabsetPanel(
        # --- NOVO: MAPAS TEMÁTICOS ---
        tabPanel("Mapas Temáticos", 
                 h3("Média Anual de PM2.5 por Município (Visão Consolidada)"),
                 tmapOutput("mapa_pm25")),
        
        # --- Tab 1: Boxplot Sazonal (Município Selecionado) ---
        tabPanel("Sazonalidade (Boxplots)", 
                 h3(textOutput("titulo_boxplot")), # Título dinâmico
                 plotOutput("boxplot_sazonal")),
        
        # --- Tab 2: Gráfico de Dispersão (Tendência ESTADUAL) ---
        tabPanel("Tendência Mensal Estadual", 
                 h3("Tendência Mensal da Média Estadual (Filtro por Ano/Fonte)"),
                 # ALTERADO PARA plotOutput (Gráfico Estático)
                 plotOutput("dispersao_pm25")),
        
        # --- Tab 3: Comparação de Fontes (Agregado) ---
        tabPanel("Comparação de Fontes", 
                 h3("Distribuição Agregada CAMS vs Donkelar"),
                 plotOutput("comparacao_fonte")),
        
        # --- Tab 4: DADOS CONSOLIDADOS (Tabela) ---
        tabPanel("Dados Consolidados (Tabela)", 
                 h3("Resumo Anual de PM2.5 - Estado da Bahia"),
                 DT::dataTableOutput("tabela_resumo_estado"), # Tabela de Resumo 
                 hr(),
                 h3("Média Mensal Detalhada por Município"),
                 DT::dataTableOutput("tabela_consolidada") # Tabela por Município
        )
      )
    )
  )
)

# =======================================================
# --- 3. SERVER (LÓGICA) ---
# =======================================================
server <- function(input, output) {
  
  # Título dinâmico para o Boxplot
  output$titulo_boxplot <- renderText({
    paste("Sazonalidade Mensal de PM2.5 em:", input$municipio_selecionado)
  })
  
  # Filtro Reativo Principal (Usado no Boxplot Sazonal - Município E Fonte/Ano)
  dados_filtrados_boxplot <- reactive({
    dados_bahia %>%
      filter(Fonte %in% input$fonte_selecionada,
             ano %in% input$ano_selecionado,
             nm_mun == input$municipio_selecionado)
  })
  
  # --- GRÁFICO 1: BOXPLOT SAZONAL (Município Selecionado) ---
  output$boxplot_sazonal <- renderPlot({
    req(nrow(dados_filtrados_boxplot()) > 0)
    
    ggplot(dados_filtrados_boxplot(), aes(x = factor(mes), y = media_pm25)) +
      facet_wrap(~ ano, ncol = 1) + 
      geom_boxplot() + 
      geom_hline(yintercept = 5, linetype = "dashed", color = COR_VERMELHO, linewidth = 0.7) +
      coord_cartesian(ylim = c(0, 30)) +
      labs(x = "Mês",
           y = expression(PM[2.5]~(mu*g/m^3))) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 12))
  })
  
  # --- GRÁFICO 2: GRÁFICO DE DISPERSÃO ESTÁTICO (Tendência Mensal ESTADUAL) ---
  # ALTERADO DE renderPlotly PARA renderPlot
  output$dispersao_pm25 <- renderPlot({
    req(input$fonte_selecionada, input$ano_selecionado)
    
    dados_dispersao_estado <- dados_bahia %>%
      filter(Fonte %in% input$fonte_selecionada,
             ano %in% input$ano_selecionado) %>%
      group_by(mes, ano, Fonte) %>%
      summarise(
        media_pm25 = mean(media_pm25, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Remove linhas com NA/NaN na média
      drop_na(media_pm25) %>%
      mutate(
        ano_n = as.numeric(as.character(ano)),
        mes_n = as.numeric(as.character(mes)),
        Data = lubridate::make_date(ano_n, mes_n, 1)
      )
    
    if (nrow(dados_dispersao_estado) == 0) {
      # Retorna um gráfico vazio ou uma mensagem simples se não houver dados
      return(ggplot() + 
               geom_text(aes(x=0, y=0, label="Nenhum dado válido para os filtros selecionados."), size=5) +
               theme_void())
    }
    
    # GRÁFICO GGPLOT ESTÁTICO
    g <- ggplot(dados_dispersao_estado, aes(x = Data, y = media_pm25, color = Fonte)) + # Removido o 'text'
      geom_point(size = 3) +
      geom_line(linewidth = 0.5) +
      geom_hline(yintercept = 5, linetype = "dashed", color = COR_VERMELHO, linewidth = 0.7) +
      labs(title = "Tendência Mensal da Média Estadual (Média dos Municípios)",
           x = "Data",
           y = expression(PM[2.5]~(mu*g/m^3))) +
      theme_minimal() +
      coord_cartesian(ylim = c(0, max(dados_dispersao_estado$media_pm25, na.rm = TRUE) * 1.1))
    
    # Retorna o objeto ggplot estático 'g'
    g
  })
  
  # --- GRÁFICO 3: COMPARAÇÃO AGREGADA POR FONTE ---
  output$comparacao_fonte <- renderPlot({
    req(nrow(dados_bahia) > 0)
    
    ggplot(dados_bahia, aes(x = Fonte, y = media_pm25, fill = Fonte)) +
      geom_boxplot() +
      geom_hline(yintercept = 5, linetype = "dashed", color = COR_VERMELHO, linewidth = 0.7) +
      labs(title = "Distribuição Agregada de PM2.5: CAMS vs Donkelar (Bahia, 2022-2023)",
           x = "Fonte de Dados",
           y = expression(PM[2.5]~(mu*g/m^3))) +
      theme_minimal()
  })
  
  # --- GRÁFICO 4: MAPA TEMÁTICO (tmap) ---
  output$mapa_pm25 <- tmap::renderTmap({
    req(input$fonte_selecionada, input$ano_selecionado)
    
    mapa_base_plot <- mapa_dados_bahia %>%
      filter(Fonte %in% input$fonte_selecionada,
             ano %in% input$ano_selecionado)
    
    # 2. Se múltiplos anos ou fontes, agregamos a média
    if (length(input$ano_selecionado) > 1 || length(input$fonte_selecionada) > 1) {
      
      dados_agregados_para_plot <- mapa_base_plot %>%
        sf::st_drop_geometry() %>% 
        mutate(COD_MUNICIPIO = as.character(COD_MUNICIPIO)) %>% 
        group_by(COD_MUNICIPIO) %>% 
        summarise(pm25_anual_medio = mean(pm25_anual_medio, na.rm = TRUE),
                  .groups = 'drop')
      
      mapa_base_plot <- limites_bahia %>% 
        left_join(dados_agregados_para_plot, by = "COD_MUNICIPIO")
    }
    
    # 3. Plota o mapa
    tm_shape(mapa_base_plot) +
      tm_fill(col = "pm25_anual_medio", 
              title = expression(PM[2.5]~Anual~(mu*g/m^3)), 
              style = "jenks", 
              palette = "YlOrRd",
              n = 6,
              # Usa o nome da coluna MUNICIPIO_NOME para o popup
              popup.vars = c("Município" = "MUNICIPIO_NOME", "Média Anual" = "pm25_anual_medio")) +
      tm_borders(col = "gray", lwd = 0.5) +
      tm_layout(title = paste("Média Anual PM2.5 - Bahia (", paste(input$ano_selecionado, collapse = ", "), ")"),
                legend.outside = TRUE,
                legend.title.size = 1.2)
  })
  
  # --- GRÁFICO 5: TABELA CONSOLIDADA POR MUNICÍPIO ---
  output$tabela_consolidada <- DT::renderDataTable({
    dados_bahia %>%
      group_by(nm_mun, Fonte, ano) %>%
      summarise(
        Media_PM25_Mensal = round(mean(media_pm25, na.rm = TRUE), 2),
        Min_PM25_Mensal = round(min(min_pm25, na.rm = TRUE), 2),
        Max_PM25_Mensal = round(max(max_pm25, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE), 
                    caption = "Média Mensal Detalhada de PM2.5 por Município")
  })
  
  # --- GRÁFICO 6: TABELA RESUMIDA POR ESTADO ---
  output$tabela_resumo_estado <- DT::renderDataTable({
    dados_bahia %>%
      group_by(Fonte, ano) %>%
      summarise(
        Media_PM25_Estado = round(mean(media_pm25, na.rm = TRUE), 2),
        Desvio_Padrao_Estado = round(sd(media_pm25, na.rm = TRUE), 2),
        Min_PM25_Estado = round(min(min_pm25, na.rm = TRUE), 2),
        Max_PM25_Estado = round(max(max_pm25, na.rm = TRUE), 2),
        Municipios_Cobertos = n_distinct(cd_mun),
        .groups = 'drop'
      ) %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE), 
                    caption = "Resumo Anual de PM2.5 Agregado por Estado e Fonte")
  })
  
}

# --- 4. Execução ---
shinyApp(ui = ui, server = server)