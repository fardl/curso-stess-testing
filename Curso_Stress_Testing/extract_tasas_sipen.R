data_dir <- "C:/Users/t490/Documents/MacroPol/stress-testing/data/data_sipen"
out_file <- "tasas_sipen_instrumentos.csv"

month_map <- c(
  enero = 1, febrero = 2, marzo = 3, abril = 4,
  mayo = 5, junio = 6, julio = 7, agosto = 8,
  septiembre = 9, setiembre = 9, octubre = 10,
  noviembre = 11, diciembre = 12
)

empty_result <- function() {
  data.frame(
    archivo = character(),
    anio_archivo = integer(),
    fecha_corte = as.Date(character()),
    mes = as.Date(character()),
    tipo_instrumento = character(),
    fondo_pension = character(),
    monto_rd = numeric(),
    tipp = numeric(),
    stringsAsFactors = FALSE
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", x))
}

xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x
}

extract_matches <- function(x, pattern) {
  m <- gregexpr(pattern, x, perl = TRUE)
  out <- regmatches(x, m)[[1]]
  if (length(out) == 1 && out[1] == "-1") character() else out
}

extract_first <- function(x, pattern) {
  out <- extract_matches(x, pattern)
  if (length(out) == 0) "" else out[1]
}

get_attr <- function(x, attr) {
  hit <- extract_first(x, paste0(attr, "=\"[^\"]*\""))
  if (hit == "") return("")
  sub(paste0("^", attr, "=\""), "", sub("\"$", "", hit))
}

col_to_int <- function(col) {
  letters <- strsplit(col, "", fixed = TRUE)[[1]]
  sum((match(letters, LETTERS)) * 26^rev(seq_along(letters) - 1))
}

read_xlsx_sheet1 <- function(path) {
  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(path, files = c("xl/sharedStrings.xml", "xl/worksheets/sheet1.xml"), exdir = tmp)

  shared_path <- file.path(tmp, "xl/sharedStrings.xml")
  strings <- character()

  if (file.exists(shared_path)) {
    shared_xml <- paste(readLines(shared_path, encoding = "UTF-8", warn = FALSE), collapse = "")
    si_blocks <- extract_matches(shared_xml, "<si[\\s\\S]*?</si>")

    strings <- vapply(si_blocks, function(si) {
      t_tags <- extract_matches(si, "<t[^>]*>[\\s\\S]*?</t>")
      pieces <- sub("^<t[^>]*>", "", sub("</t>$", "", t_tags))
      xml_unescape(paste(pieces, collapse = ""))
    }, character(1), USE.NAMES = FALSE)
  }

  sheet_path <- file.path(tmp, "xl/worksheets/sheet1.xml")
  sheet_xml <- paste(readLines(sheet_path, encoding = "UTF-8", warn = FALSE), collapse = "")
  row_tags <- extract_matches(sheet_xml, "<row[^>]*>[\\s\\S]*?</row>")

  cells <- list()
  max_row <- 0L
  max_col <- 0L

  for (row_tag in row_tags) {
    row_num <- as.integer(get_attr(row_tag, "r"))
    cell_tags <- extract_matches(row_tag, "<c[^>]*(/>|>[\\s\\S]*?</c>)")

    for (cell_tag in cell_tags) {
      ref <- get_attr(cell_tag, "r")
      if (ref == "") next

      col_letters <- gsub("[0-9]", "", ref)
      col_num <- col_to_int(col_letters)
      type <- get_attr(cell_tag, "t")
      value <- ""

      if (grepl("<v>", cell_tag, fixed = TRUE)) {
        value <- sub("^<v>", "", sub("</v>$", "", extract_first(cell_tag, "<v>[\\s\\S]*?</v>")))
        if (type == "s" && value != "") {
          value <- strings[as.integer(value) + 1L]
        }
      } else if (type == "inlineStr") {
        t_tag <- extract_first(cell_tag, "<t[^>]*>[\\s\\S]*?</t>")
        value <- sub("^<t[^>]*>", "", sub("</t>$", "", t_tag))
      }

      cells[[length(cells) + 1L]] <- list(row = row_num, col = col_num, value = xml_unescape(value))
      max_row <- max(max_row, row_num)
      max_col <- max(max_col, col_num)
    }
  }

  out <- matrix("", nrow = max_row, ncol = max_col)
  for (cell in cells) {
    out[cell$row, cell$col] <- cell$value
  }

  out
}

parse_fecha_corte <- function(raw_sheet) {
  top <- raw_sheet[seq_len(min(8, nrow(raw_sheet))), , drop = FALSE]
  top_text <- clean_text(as.vector(t(top)))
  top_text <- top_text[top_text != ""]

  hit <- regmatches(
    top_text,
    regexpr("Al\\s+\\d{1,2}\\s+de\\s+[[:alpha:]]+\\s+(de\\s+)?\\d{4}", top_text, ignore.case = TRUE, perl = TRUE)
  )
  hit <- hit[nzchar(hit)][1]

  if (is.na(hit) || length(hit) == 0) return(as.Date(NA))

  parts <- regexec(
    "al\\s+(\\d{1,2})\\s+de\\s+([[:alpha:]]+)\\s+(de\\s+)?(\\d{4})",
    tolower(hit),
    perl = TRUE
  )
  pieces <- regmatches(tolower(hit), parts)[[1]]

  day <- as.integer(pieces[2])
  month <- unname(month_map[pieces[3]])
  year <- as.integer(pieces[5])

  as.Date(sprintf("%04d-%02d-%02d", year, month, day))
}

fill_fund_names <- function(x) {
  x <- clean_text(x)
  x[x == ""] <- NA_character_

  current <- NA_character_
  out <- rep(NA_character_, length(x))

  for (i in seq_along(x)) {
    if (!is.na(x[i])) current <- x[i]
    out[i] <- current
  }

  out
}

standardize_measure <- function(x) {
  x <- toupper(clean_text(x))
  out <- rep(NA_character_, length(x))
  out[x %in% c("RD$", "RD", "$RD", "$RD MM", "MONTO")] <- "monto_rd"
  out[x %in% c("TIPP", "TASA", "%")] <- "tipp"
  out
}

as_number <- function(x) {
  x <- clean_text(x)
  x[x %in% c("", "-", "NA", "N/A")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x)))
}

skip_instrument <- function(x) {
  x <- clean_text(x)
  x == "" ||
    grepl("^(nota|notas|fuente|al\\s+\\d|\\d+/|\\$rd)", x, ignore.case = TRUE, perl = TRUE)
}

read_tasas_file <- function(path) {
  raw <- read_xlsx_sheet1(path)

  header_row <- which(apply(raw, 1, function(z) {
    any(toupper(clean_text(z)) == "TIPO DE INSTRUMENTO")
  }))[1]

  if (is.na(header_row)) {
    warning("No encontre la fila de encabezado en: ", basename(path))
    return(empty_result())
  }

  header <- raw[header_row, ]
  window_rows <- seq(header_row + 1L, min(header_row + 5L, nrow(raw)))
  subheader_row <- NA_integer_

  for (r in window_rows) {
    if (any(!is.na(standardize_measure(raw[r, ])))) {
      subheader_row <- r
      break
    }
  }

  subheader <- if (is.na(subheader_row)) rep("", ncol(raw)) else raw[subheader_row, ]
  fund_names <- fill_fund_names(header)
  measures <- standardize_measure(subheader)

  if (!any(measures == "monto_rd", na.rm = TRUE)) {
    missing_measure <- seq_along(measures) > 1L & is.na(measures)
    measures[missing_measure] <- "tipp"
  }

  col_map <- data.frame(
    col_id = seq_along(header),
    fondo_pension = fund_names,
    medida = measures,
    stringsAsFactors = FALSE
  )

  col_map <- col_map[
    col_map$col_id > 1L &
      !is.na(col_map$fondo_pension) &
      !is.na(col_map$medida) &
      toupper(col_map$fondo_pension) != "TIPO DE INSTRUMENTO",
  ]

  data_start <- max(c(header_row, subheader_row), na.rm = TRUE) + 1L
  records <- list()

  for (r in seq(data_start, nrow(raw))) {
    instrument <- clean_text(raw[r, 1])
    if (skip_instrument(instrument)) next

    for (i in seq_len(nrow(col_map))) {
      value <- as_number(raw[r, col_map$col_id[i]])
      if (is.na(value)) next

      records[[length(records) + 1L]] <- data.frame(
        row_id = r,
        tipo_instrumento = instrument,
        fondo_pension = col_map$fondo_pension[i],
        medida = col_map$medida[i],
        valor = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(records) == 0) return(empty_result())

  long <- do.call(rbind, records)
  groups <- split(long, paste(long$row_id, long$tipo_instrumento, long$fondo_pension, sep = "||"))

  wide <- do.call(rbind, lapply(groups, function(g) {
    monto <- g$valor[g$medida == "monto_rd"][1]
    tipp <- g$valor[g$medida == "tipp"][1]

    data.frame(
      tipo_instrumento = g$tipo_instrumento[1],
      fondo_pension = g$fondo_pension[1],
      monto_rd = ifelse(length(monto) == 0, NA_real_, monto),
      tipp = ifelse(length(tipp) == 0, NA_real_, tipp),
      stringsAsFactors = FALSE
    )
  }))

  fecha <- parse_fecha_corte(raw)
  archivo <- basename(path)
  anio <- as.integer(sub("^tasas_(\\d{4})_.*$", "\\1", archivo))

  data.frame(
    archivo = archivo,
    anio_archivo = anio,
    fecha_corte = fecha,
    mes = as.Date(format(fecha, "%Y-%m-01")),
    wide,
    stringsAsFactors = FALSE
  )
}

files <- list.files(data_dir, pattern = "^tasas_.*\\.xlsx$", full.names = TRUE)
tasas <- do.call(rbind, lapply(files, read_tasas_file))
tasas <- unique(tasas)

group_key <- paste(tasas$archivo, tasas$mes, tasas$fondo_pension, sep = "||")
tasas$total_portafolio_rd <- ave(tasas$monto_rd, group_key, FUN = function(x) sum(x, na.rm = TRUE))
tasas$peso_relativo <- ifelse(
  !is.na(tasas$monto_rd) & tasas$total_portafolio_rd > 0,
  tasas$monto_rd / tasas$total_portafolio_rd,
  NA_real_
)

write.csv(tasas, out_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

message("Archivo creado: ", normalizePath(out_file, winslash = "/"))
message("Filas extraidas: ", nrow(tasas))
