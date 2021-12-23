library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# leitura de dados
areaRec <- read.csv('./Area_Rec_Ibama_20211015/areaRecuperacao.csv',
                    encoding = 'UTF-8')
# limpeza PA
areaRec2 <- read.csv('./Area_Rec_Ibama_20211016/areaRecuperacao.csv',
                    encoding = 'UTF-8')

# Obter processos e areas duplicados
duplicados <- areaRec %>%
        count(n_processo, hectares) %>%
        filter(n > 1)

write.csv2(duplicados, './registros_duplicados_CASV.csv', row.names = FALSE)

# uf com areas duplicadas
uf_duplicado <- areaRec2 %>%
        count(uf, hectares) %>%
        filter(n > 1) %>%
        count(uf) %>%
        arrange(desc(n))

ggplot(uf_duplicado, aes(reorder(uf, n), n)) +
        geom_col(fill = 'steelblue') +
        geom_label(aes(label = n), vjust = 1, color = 'red', size = 3.5) +
        labs(title = 'Estados com Registros Duplicados no CASV',
             y = '',
             x = 'UF') +
        theme(plot.title = element_text(hjust = 0.5))
                
        
# limpeza de dados
areaRec_clean <- areaRec %>%
        #group_by(n_ai, n_embargo) %>%
        distinct(n_processo, hectares, .keep_all = TRUE)



sum(duplicated(areaRec))
dim(areaRec)        
dim(areaRec_clean)


options(digits = 14)

areaRec %>%
        summarize(hectares = sum(hectares))

areaRec_clean %>%
        summarize(hectares = sum(hectares))


# duplicados
areaRec2 %>%
        group_by(uf) %>%
        summarize(total = sum(hectares)) %>%
        ggplot(aes(reorder(uf, total), total)) +
        geom_col(fill = 'darkgreen') + coord_flip() +
        scale_y_continuous(labels =scales::label_number(big.mark = '.')) +
        labs(title = 'Áreas em Recuperação - Dados Duplicados',
             x = '', 
             y = 'Área (ha)') +
        theme(plot.title = element_text(hjust = 0.5))

# Sem dados duplicados
areaRec_clean %>%
        group_by(uf) %>%
        summarize(total = sum(hectares)) %>%
        ggplot(aes(reorder(uf, total), total)) +
        geom_col(fill = 'darkgreen') + coord_flip() +
        scale_y_continuous(labels =scales::label_number(big.mark = '.')) +
        labs(title = 'Áreas em Recuperação - Sem Dados Duplicados',
             x = '', 
             y = 'Área (ha)') +
        theme(plot.title = element_text(hjust = 0.5))
