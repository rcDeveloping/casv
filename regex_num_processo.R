a <- read.csv('C:/nubio_pa/analise/casv/AreaRec_CASV_ciclo_2020-2021.csv', encoding = 'UTF-8')
head(a)

pa <- a %>%
        filter(uf == 'PA') %>%
        mutate(n_processo = as.character(n_processo)) %>%
        mutate(n_processo = paste0('0', n_processo)) %>%
        mutate(n_processo = if_else(nchar(n_processo) <= 16,
                                    paste0('0', substr(str_proc_jud, 1, 4), '.',
                                           substr(str_proc_jud, 5, 10), '/',
                                           substr(str_proc_jud, 11, 14), '-',
                                           substr(str_proc_jud, 15, 16)), 
                                    paste0(substr(n_processo, 1, 5), '.', 
                                           substr(n_processo, 6, 11), '/', 
                                           substr(n_processo, 12, 15), '-', 
                                           substr(n_processo, 16, 17))))
