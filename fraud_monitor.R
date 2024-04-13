# Limpio la memoria
rm(list=ls())
gc()
Sys.time()

# Instalaci?n condicional de paquetes
# Package names
packages <- c("dplyr", "lubridate", "data.table", "googledrive", "readxl", "gtools", 
              "googlesheets4", "naniar", "stringr", "writexl", "fuzzyjoin")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}

# Copiar a portapapeles
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

directorio_out = "W:\\Mi unidad\\Fraud\\Approval Funnel\\data\\"
directorio_ref = "W:\\Mi unidad\\Fraud\\Approval Funnel\\ref\\"

# Guardo el archivo de trabajo
file_out = "data_af_usd_effective.csv"
file_cbks = "cbks.csv"
salida = paste(directorio_out, file_out, sep = "")

data_af = fread(salida, sep = "\t")

#data_af %>% group_by(strategy) %>% summarise(count = n())


gross_card = data_af[which(data_af$strategy == "unique checkout attempts"),] 
gross_nc = data_af[which(data_af$strategy == "non_card_attempts" | data_af$strategy == "change to non-card"),]
gross = data_af[which(data_af$strategy == "non_card_attempts" | data_af$strategy == "unique checkout attempts"),]

gross_card = gross_card %>% group_by(date_attempt) %>% summarise(gmv = sum(gmv_usd), 
                                                       checkouts = n_distinct(checkout_id),
                                                       orders = sum(n_orders))

gross_nc = gross_nc %>% group_by(date_attempt) %>% summarise(gmv_nc = sum(gmv_usd), 
                                                            checkouts_nc = n_distinct(checkout_id),
                                                            orders_nc = sum(n_orders))

gross = gross %>% group_by(date_attempt) %>% summarise(gmv_gross = sum(gmv_usd), 
                                                       checkouts_gross = n_distinct(checkout_id),
                                                       orders_gross = sum(n_orders))

# ctnc = data_af[which(data_af$strategy == "change to non-card"),]
# ctnc = ctnc %>% group_by(date_attempt) %>% summarise(gmv_ctnc = sum(gmv_usd), 
#                                                      checkouts_ctnc = n_distinct(checkout_id),
#                                                      orders_ctnc = sum(n_orders))

cancel_before = data_af[which(data_af$strategy == "change to non-card" | data_af$strategy == "error" | data_af$strategy == "cancelled"),]
cancel_before = cancel_before %>% group_by(date_attempt) %>% summarise(gmv_cb = sum(gmv_usd), 
                                                                       checkouts_cb = n_distinct(checkout_id),
                                                                       orders_cb = sum(n_orders))


issuer = data_af[which(data_af$strategy == "issuer"),]
issuer = issuer %>% group_by(date_attempt) %>% summarise(gmv_issuer = sum(gmv_usd), 
                                                     checkouts_issuer = n_distinct(checkout_id),
                                                     orders_issuer = sum(n_orders))

strategy_fraud = c("cybs reject", "ds model", "online rule", "other af rules", "spm")
cancel_fraud = data_af[which(data_af$strategy %in% strategy_fraud),]
cancel_fraud = cancel_fraud %>% group_by(date_attempt) %>% summarise(gmv_fraud = sum(gmv_usd), 
                                                                     checkouts_fraud = n_distinct(checkout_id),
                                                                     orders_fraud = sum(n_orders))


paid = data_af[which(data_af$strategy == "paid" | data_af$strategy == "non_card_paid"),]
paid_card = paid[which(paid$strategy == "paid"),]
paid_nc = paid[which(paid$strategy == "non_card_paid"),]

paid_card = paid_card %>% group_by(date_attempt) %>% summarise(gmv_paid = sum(gmv_usd), 
                                                     checkouts_paid = n_distinct(checkout_id),
                                                     orders_paid = sum(n_orders))

paid_nc = paid_nc %>% group_by(date_attempt) %>% summarise(gmv_paid_nc = sum(gmv_usd), 
                                                          checkouts_paid_nc = n_distinct(checkout_id),
                                                          orders_paid_nc = sum(n_orders))

file_out = "ref_cancellations.txt"
salida = paste(directorio_ref, file_out, sep = "")
ref_cancel = fread(salida, sep = "\t")

ref_cancel = ref_cancel %>% distinct(Cancel_Reason, .keep_all = TRUE)
canc_after = paid
canc_after$cancel_reason = toupper(canc_after$cancel_reason)
canc_after = merge(canc_after, ref_cancel, by.x = "cancel_reason", by.y = "Cancel_Reason", all.x = TRUE)

canc_after_c = canc_after[which(canc_after$strategy == "paid" & canc_after$Cancelled_When == "After"),]
canc_after_nc = canc_after[which(canc_after$strategy == "non_card_paid" & canc_after$Cancelled_When == "After"),]

canc_after_c = canc_after_c %>% group_by(date_attempt) %>% summarise(gmv_canc = sum(gmv_usd_cancelled), 
                                                                 checkouts_canc = n_distinct(checkout_id),
                                                                 orders_canc = sum(n_orders))

canc_after_nc = canc_after_nc %>% group_by(date_attempt) %>% summarise(gmv_canc_nc = sum(gmv_usd_cancelled), 
                                                                     checkouts_canc_nc = n_distinct(checkout_id),
                                                                     orders_canc_nc = sum(n_orders))


# ------------------------------------------------------------------------------------------------
# CHARGEBACKS
file = file_cbks
archivo = paste(directorio_out, file, sep = "")
cbks = fread(archivo,
             header = TRUE, 
             sep = "\t",
             encoding = "UTF-8",
             na.strings = c("",NA))

cbks = cbks %>% 
  group_by(date_attempt) %>%
  summarise(cbk_qty = sum(is_cbk),
            cbk_qty_proj = sum(cbk_qty_proj),
            cbk_gmv_net = sum(gmv_usd_net),
            cbk_gmv_net_proj = sum(cbk_gmv_proj_usd_net))

# ------------------------------------------------------------------------------------------------
# METODOS DE PAGO
share_gross = data_af[which(data_af$strategy == "unique checkout attempts" | 
                              data_af$strategy == "non_card_attempts"),] %>%
  group_by(date_attempt) %>% 
  summarise(share_cc = sum(gmv_usd[payment_be_channel == "label_credit_card"]),
            share_dc = sum(gmv_usd[payment_be_channel == "label_debit_card"]),
            share_pf = sum(gmv_usd[payment_be_channel == "label_pagofacil_otc"]),
            share_rp = sum(gmv_usd[payment_be_channel == "label_rapipago_otc"]))

share_paid = data_af[which(data_af$strategy == "paid" | 
                              data_af$strategy == "non_card_paid"),] %>%
  group_by(date_attempt) %>% 
  summarise(share_cc_paid = sum(gmv_usd[payment_be_channel == "label_credit_card"]),
            share_dc_paid = sum(gmv_usd[payment_be_channel == "label_debit_card"]),
            share_pf_paid = sum(gmv_usd[payment_be_channel == "label_pagofacil_otc"]),
            share_rp_paid = sum(gmv_usd[payment_be_channel == "label_rapipago_otc"]))

# ------------------------------------------------------------------------------------------------
# FLUJOS
file_ref_flows = "ref_flows.txt"
file = file_ref_flows
archivo = paste(directorio_ref, file, sep = "")

flows = fread(archivo,
              header = TRUE, 
              sep = "\t",
              encoding = "UTF-8",
              na.strings = c("",NA)) 

data_af = merge(data_af, flows, by.x = "ab_test", by.y = "ab_test", all.x = TRUE)

share_flujos = data_af[which(data_af$strategy == "unique checkout attempts"),] %>%
  group_by(date_attempt) %>% 
  summarise(share_cybs = sum(gmv_usd[(flow != "DS") | is.na(flow)]),
            share_dsm = sum(gmv_usd[flow == "DS"]))

# ----------------------------------------------------------------------------------------------------------
#put all data frames into list
df_list = list(gross, gross_card, gross_nc, 
               cancel_before, 
               cancel_fraud, 
               issuer, 
               paid_card, paid_nc, 
               canc_after_c, canc_after_nc,
               cbks,
               share_gross, share_paid,
               share_flujos)

#merge all data frames in list
data = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
data[is.na(data)] = 0


data$gmv_net_card = data$gmv_paid - data$gmv_canc
data$checkouts_net_card = data$checkouts_paid - data$checkouts_canc
data$orders_net_card = data$orders_paid - data$orders_canc

data$gmv_net_nc = data$gmv_paid_nc - data$gmv_canc_nc
data$checkouts_net_nc = data$checkouts_paid_nc - data$checkouts_canc_nc
data$orders_net_nc = data$orders_paid_nc - data$orders_canc_nc


# Metricas principales
# --------------------------------------------------------------------------------
calcula_metricas = function(data){
  data$g2n_gmv_card = data$gmv_net_card / data$gmv
  data$g2n_qty_card = data$orders_net_card / data$orders
  
  data$g2n_gmv_overall = (data$gmv_net_card + data$gmv_net_nc) / (data$gmv_gross)
  data$g2n_qty_overall = (data$orders_net_card + data$orders_net_nc) / (data$orders_gross)
  
  data$conv_nc_gmv = data$gmv_net_nc / data$gmv_nc
  data$conv_nc_qty = data$orders_net_nc / data$orders_nc
  
  data$app_cancelled_before = 1 - (data$gmv_cb / data$gmv)
  data$app_fraud = 1 - (data$gmv_fraud / (data$gmv_paid + data$gmv_issuer + data$gmv_fraud))
  data$app_issuer = 1 - (data$gmv_issuer / (data$gmv_paid + data$gmv_issuer))
  data$app_cancelled_after = (data$gmv_net_card / data$gmv_paid)
  
  data$abs_gross = (data$gmv + data$gmv_nc) / (data$orders_gross)
  data$abs_net = (data$gmv_net_card + data$gmv_net_nc) / (data$orders_net_card + data$orders_net_nc)
  data$abs_gap = data$abs_net / data$abs_gross
  
  data$share_gross_cc = data$share_cc / data$gmv_gross
  data$share_gross_dc = data$share_dc / data$gmv_gross
  data$share_gross_pf = data$share_pf / data$gmv_gross
  data$share_gross_rp = 1 - data$share_gross_cc - data$share_gross_dc - data$share_gross_pf
  
  data$share_paid_cc = data$share_cc_paid / (data$gmv_paid + data$gmv_paid_nc)
  data$share_paid_dc = data$share_dc_paid / (data$gmv_paid + data$gmv_paid_nc)
  data$share_paid_pf = data$share_pf_paid / (data$gmv_paid + data$gmv_paid_nc)
  data$share_paid_rp = 1 - data$share_paid_cc - data$share_paid_dc - data$share_paid_pf
  
  data$share_cybs = data$share_cybs / data$gmv
  data$share_dsm = data$share_dsm / data$gmv
  
  data$cbk_rate = data$cbk_gmv_net / data$gmv_paid
  data$cbk_rate_proj = data$cbk_gmv_net_proj / data$gmv_paid

  data = data %>% arrange(desc(ymd(data$date_attempt)))
  
  columnas = c('date_attempt', 'g2n_gmv_card', 'g2n_gmv_overall', 'g2n_qty_card', 'g2n_qty_overall', 'conv_nc_gmv', 'conv_nc_qty', 'app_cancelled_before', 'app_fraud', 'app_issuer', 'app_cancelled_after', 'cbk_qty', 'cbk_qty_proj', 'cbk_rate', 'cbk_rate_proj', 'abs_gross', 'abs_net', 'abs_gap', 'share_gross_cc', 'share_gross_dc', 'share_gross_pf', 'share_gross_rp', 'share_paid_cc', 'share_paid_dc', 'share_paid_pf', 'share_paid_rp', 'share_cybs', 'share_dsm')

  data = data %>% select(all_of(columnas))
  
  nombres_columnas = c('Fecha', 'G2N Card', 'G2N Overall', 'G2N QTY Card', 'G2N QTY Overall', 'Conv NonCard GMV', 'Conv NonCard QTY', 'Approval Before', 'Approval Fraud', 'Approval Issuer', 'Approval After', 'CBK QTY', 'CBK QTY Proj', 'CBK Rate', 'CBK Rate Proj', 'ABS Gross', 'ABS Paid', 'ABS Gap', 'CC', 'DC', 'PF', 'RP', 'CC', 'DC', 'PF', 'RP', 'CYBS', 'DS')
  colnames(data) = nombres_columnas
  
  return (data)
}

data_d = calcula_metricas(data)

data_w = data
data_w$date_attempt = floor_date(data_w$date_attempt, "weeks", week_start = 1)

data_w = data_w %>% 
  group_by(date_attempt) %>% 
  summarise_each(list(sum)) %>%
  calcula_metricas()

data_m = data
data_m$date_attempt = floor_date(data_m$date_attempt, "month")

data_m = data_m %>% 
  group_by(date_attempt) %>% 
  summarise_each(funs(sum))%>%
  calcula_metricas()

#cb(data_m)

data_overall = data
data_overall$date_attempt = "1900-01-01"

data_overall = data_overall %>% 
  group_by(date_attempt) %>% 
  summarise_each(funs(sum))%>%
  calcula_metricas()

data_overall = data_overall[,2:ncol(data_overall)]

# Escribo el resultado en el sheets
# --------------------------------------------------------------------------------
options(gargle_oob_default = TRUE)
gs4_auth(email = "ar.fraud.analytics@shopee.com")
id_status = "1GTcj5GYwTEWNi6thO1Y7fJ0KghG-nNK7UMaS3Qd4740"

tryCatch({
  range_write(
    ss = id_status,
    data = data_m,
    sheet = 'Fraud_m',
    range = "A5",
    col_names = TRUE,
    reformat = FALSE
  )
  
  range_write(
    ss = id_status,
    data = data_overall,
    sheet = 'Fraud_m',
    range = "B1",
    col_names = FALSE,
    reformat = FALSE
  )
}, error=function(e){})

tryCatch({
  range_write(
    ss = id_status,
    data = data_w,
    sheet = 'Fraud_w',
    range = "A5",
    col_names = TRUE,
    reformat = FALSE
  )
  
  range_write(
    ss = id_status,
    data = data_overall,
    sheet = 'Fraud_w',
    range = "B1",
    col_names = FALSE,
    reformat = FALSE
  )
}, error=function(e){})

tryCatch({
  range_write(
    ss = id_status,
    data = data_d,
    sheet = 'Fraud_d',
    range = "A5",
    col_names = TRUE,
    reformat = FALSE
  )
  
  range_write(
    ss = id_status,
    data = data_overall,
    sheet = 'Fraud_d',
    range = "B1",
    col_names = FALSE,
    reformat = FALSE
  )
}, error=function(e){})



# Escribo status ejecucion en hoja "Status"
# --------------------------------------------------------------------------------
options(gargle_oob_default = TRUE)
gs4_auth(email = "ar.fraud.analytics@shopee.com")
id_status = "1lIEtOXHyHeAUeMFl7zF1lDBGlu_WoPqHlVS6ojZG55Y"
df_hora_guardado = as.data.frame(Sys.time() - hours(3))
colnames(df_hora_guardado) = "FechaHoraActual"

codigo_r = "Fraud_Payments_Monitor"

status = read_sheet(id_status)
fila = match(codigo_r, status$Script)+1
celda = paste("E", fila, sep="")


if (!is.na(fila)){
  celda = paste("E", fila, sep="")
  
  tryCatch({
    range_write(
      ss = id_status,
      data = df_hora_guardado,
      sheet = 'Status',
      range = celda,
      col_names = FALSE,
      reformat = FALSE
    )
  }, error=function(e){})
}
