#### Helper functions for the primeR web database ####


#' plot_blast
#'
#' @param dna string. Input query sequence
#' @param blast_out csv file path. BLAST output in tabular format (outfmt = 6) 
#'
#' @return An interactive plot with similar features as on the NCBI BLAST webpage
#' @export
#'
#' @author Gergo Palfalvi
#' 
#' @examples 
plot_blast <- function(dna, blast_out){
  
  dna = qeury
  
  blast_out <- read_csv(file = "",
                        col_names = c("qseqid",
                                      "sseqid",
                                      "pident",
                                      "length",
                                      "mismatch",
                                      "gapopen",
                                      "qstart",
                                      "qend",
                                      "sstart",
                                      "send",
                                      "evalue",
                                      "bitscore"),
                        trim_ws = TRUE)
  
  dna_l <- tibble(
    y = 0,
    x = 1,
    xend = stringr::str_length(dna)
  )
  
  
  blast_out %>% group_by(qseqid, sseqid) %>% nest() -> blast_nest
  
  n <- nrow(blast_nest)
  
  blast_nest %>% mutate(y = seq_len(n)) %>% unnest() -> blast_unnest
  
  ggplot() +
    geom_segment(data = dna_l, aes(x = x , xend = xend, y = y, yend = y), color = "grey10", size = 5) +
    geom_segment(data = blast_unnest, aes(x = qstart, xend = qend, y = y, yend = y, color = bitscore, 
                                          text = paste("Match:", sseqid, "<br>",
                                                       "E-value:", evalue)), size = 3, alpha = 0.7) +
    theme_classic() +
    theme(axis.ticks.x = element_line(color = "black"),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    scale_color_gradientn(colours=rainbow(5),limits = c(0,200), na.value = "violet") +
    scale_x_continuous(breaks = c(1,seq(200,dna_l$xend, by = 200)), position = 'top') +
    scale_y_reverse(lim = c(10,0)) + xlab("Query") -> blast_plot
  
  ggplotly(blast_plot, tooltip = "text") %>% layout(xaxis = list(position = 'top'))
}

#' fasta_validator
#'
#' @param fasta input fasta sequence (with header)
#' @param type 'nucl'/'prot'. Type of fasta. 
#'
#' @return TRUE if the fasta contains valid format (header starting with '>', new line after header, valid IUPAC notations)
#' @import stringr::str_detect()
#' @author Gergo Palfalvi
#' @examples
fasta_validator <- function(fasta,type='nucl') {
  if (type == 'nucl'){
    fastaRegEx = '[^abcdghkmnrstuvwyABCDGHKMNRSTUVWY]'
  } else if (type == 'prot') {
    fastaRegEx = '[^a-zA-Z]'
  }
  
  if (substr(fasta,1,1) != ">" ) {
    seq <- gsub("\n", "",gsub(" ","",substr(fasta,as.numeric(gregexpr('\\n',fasta)[[1]][1]),nchar(fasta))))
    return(!stringr::str_detect(fasta, fastaRegEx))
  } else {
    return(FALSE)
  }
}



#' blast_options
#' 
#' @description Creating BLAST option arguments.
#'
#' @param query 
#' @param blast_db 
#' @param ungapped 
#'
#' @return
#' @export
#' @author Gergo Palfalvi
#'
#' @examples blast_op <- blast_options() \n blast_op$query <- "path/to/query" \n blast_op$blast_db <- "path/to/blastdb" \n blast_op$evalue <- 10e-3
#' @seealso blast_search
blast_options <- function(query = "", blast_db = "", ungapped = TRUE, evalue = 1000) {
  if (ungapped == FALSE) {
  list(
    "query" = query,
    "db" = blast_db,
    "outfmt" = "'6 qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore qseq sseq'",
    "evalue" = evalue,
    "word_size" = 7,
    "num_threads" = 1,
    "best_hit_overhang" = 0.1,
    "best_hit_score_edge" = 0.1,
    "max_target_seqs" = 100
  ) 
    } else {
    list(
      "query" = query,
      "db" = blast_db,
      "outfmt" = "'6 qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore qseq sseq'",
      "evalue" = evalue,
      "word_size" = 7,
      "num_threads" = 1,
      "best_hit_overhang" = 0.1,
      "best_hit_score_edge" = 0.1,
      "max_target_seqs" = 100
    )
  }
}


#' blast_search
#' 
#' @description Run a BLAST search by invoking a pre-installed 
#'
#' @param blast_op List of BLAST options. See blast_options()
#' @param blast_type The type of the blast search. 'blastn', 'blastx', 'tblastx', 'tblastn', 'blastp'
#' @param blast_bin The binary path to the blast to use.
#' @param query ONLY IF blast_op is not provided.
#' @param blast_db ONLY IF blast_op is not provided.
#'
#' @return
#' @export
#'
#' @examples
blast_search <- function(blast_op = NULL, blast_type = c('blastn', 'blastx', 'tblastx', 'tblastn', 'blastp'), blast_bin = "", query = NULL, blast_db = NULL) {
  # If query is not a valid fasta, break and return an error message.
  
  if (is.null(blast_op)) {
    blast_op <- blast_options(query = query, blast_db = blast_db, ungapped = FALSE)
  }
  
  
  # Convert blast_op to a character vector
  blast_op_vector <- trimws(paste("-", names(blast_op), " ", blast_op,  sep = ""))
  
  blast_out <- system2(command = print("blastn", quote = FALSE), 
                       args = blast_op_vector,
                       stdout = TRUE,
                       wait = TRUE)
  
  
  blast_out_read <- try(read.table(textConnection(blast_out),sep='\t'),silent=TRUE)
  
  if (is.null(blast_out_read)) {
    return(cat("Sorry, we could not find any hit.\nTry to modify the BLAST options or use another query."))
  }
  
  blast_out_read <- as_tibble(blast_out_read)
  colnames(blast_out_read) <- c("qseqid",
                                "sseqid",
                                "pident",
                                "length",
                                "mismatch",
                                "gapopen",
                                "qstart",
                                "qend",
                                "sstart",
                                "send",
                                "evalue",
                                "bitscore",
                                "qseq",
                                "sseq")
  
  return(blast_out_read %>% as_data_frame())
  
}


#' create_blast_db
#'
#' @param input Input fasta/blastdb name to be recreated (fasta should be newer then blastdb)
#' @param dbtype Type of the database. Default "nucl".
#'
#' @return
#' @export
#'
#' @examples
create_blast_db <- function(input, dbtype = "nucl") {
  
  system2(command = "makeblastdb",
    args = c(
      "-in", input,
      "-dbtype", dbtype,
      "-parse_seqids"),
    stdout = paste("blast_db_update", ".out", sep=""),
    wait = TRUE
    )
  
}


#' Read Fasmac report csv file and reformat for the primer database
#'
#' @param file input file. download csv report from fasmac's website.
#'
#' @return
#' @export
#'
#' @examples
read_fasmac <- function(file) {
  read_csv(file, 
           skip = 1,
           col_types = cols(col_skip(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_character(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_character(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_character(),
                            col_character(),
                            col_character(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_skip(),
                            col_number(),
                            col_skip()),
           col_names = c("name",
                         "seq",
                         "modified_5",
                         "modified_3",
                         "all_modified",
                         "conc")) %>%
    mutate(comm = paste(if_else(is.na(modified_5), "", paste("5' modification:", modified_5, "\n")), 
                            if_else(is.na(modified_3), "", paste("3' modification:", modified_3, "\n")), 
                            if_else(is.na(all_modified), "", paste("modification:", all_modified, "\n"))),
           seq = seq %>% stringr::str_to_lower() %>% stringr::str_replace_all(" ", "")) %>%
    select(name,
           seq,
           conc,
           comm) %>%
    filter(!is.na(name))
  
}



