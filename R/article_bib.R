#' Preparer of main references for automatic insertion in scientific articles
#'
#' The article_bib function prepares the metadata of the main scientific references to be automatically included in a scientific article. Initially, the user obtains the list of main references using the principal_lister function from the bibliorefer package.  Then, read the articles, choose the ones you prefer, and display the positions of these articles in the input parameter, position_artic, of the article_bib function. The article_bib function internally calls the gerard_lister and bibtex_lister functions. The gerard_lister function separates and organizes the metadata, of each of the articles in the list chosen by the user, and stores them in a dataframe. Then, the bibtex_lister function is applied to the set of metadata organized in the dataframe and converts it to the BibTex style. The output of the article_bib function is the file with the .bib extension, containing the list of references in BibTex format, prepared to be automatically included in the reference list of a scientific paper.
#'
#' @param input_date is a dataframe with the scientific production database obtained of colection WoS, Scopus and others
#' @param input_tam is the length of the dataframe with the main scientifics articles, obtained using package bibliorefer.
#' @param position_artic is a parameter that shows the positions of the articles in the main list, obtained using package bibliorefer, chosen to be included in the reference list of a scientific paper. If part of the list is used, the set of articles is presented through a sequence or a concatenated set. If the complete list is used, the complete sequence is created
#' @param total_list is the parameter that defines whether all articles from the main list, obtained using package bibliorefer, will be used or not. This parameter contains the logical values TRUE or FALSE. If the full list is used, the value is TRUE. Otherwise, if a part of the list is used, the value is FALSE
#'
#' @return This function returns a file with the .bib extension, containing the list of references in bibtex format, prepared to be automatically included in the reference list of a scientific paper.
#' @importFrom utils read.csv2
#' @importFrom bibliorefer principal_lister
#' @export
#'
#' @references
#' 1 - Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#' 2 - Bibliometric indicators to evaluate scientific activity. C García-Villar, J M García-Santos. Radiologia (Engl Ed). 2021 May-Jun;63(3):228-235.
#'
#'
#' @examples
#'
#' # Example 1 - Concatenated position article
#'
#' # File of database
#' file_db <- system.file("extdata","example_database.csv", package = "gerefer")
#' separator <- ","
#'
#' # Parameters of the function
#' input_date <- example_database(file_db, separator)
#' input_tam <- 50
#' total_list <- FALSE
#' position_artic <- c(1, 2, 3, 5, 6, 8, 10, 12, 15, 17, 19, 23, 24,
#'      26, 28, 29, 30, 32, 33, 35, 36, 37, 38, 39, 40, 42, 44, 46, 47, 48)
#'
#' #Calls the function article_bib
#' lister_bibtex <- article_bib(input_date, input_tam, position_artic, total_list)
#' lister_bibtex
#'
#' # Example 2 - Position article in sequence
#'
#' # File of database
#' file_db <- system.file("extdata","example_database.csv", package = "gerefer")
#' separator <- ","
#'
#' # Parameters of the function
#' input_date <- example_database(file_db, separator)
#' input_tam <- 50
#' total_list <- FALSE
#' position_artic <- seq(6, 35, 1)
#'
#' # Call the function article_bib
#' lister_bibtex <- article_bib(input_date, input_tam, position_artic, total_list)
#' lister_bibtex
#'
#' # Example 3 - Total list article
#'
#' # File of database
#' file_db <- system.file("extdata","example_database.csv", package = "gerefer")
#' separator <- ","
#'
#' # Parameters of the function
#' input_date <- example_database(file_db, separator)
#' input_tam <- 50
#' total_list <- TRUE
#' position_artic <- seq(1, input_tam, 1)
#'
#' # Call the function article_bib
#' lister_bibtex <- article_bib(input_date, input_tam, position_artic, total_list)
#' lister_bibtex
#'
#'
article_bib  <- function(input_date, input_tam, position_artic, total_list){

  # Call the function that generates the intermediate information
  gerad_lister <- function(input_date, input_tam, position_artic, total_list){

    # Generate the main list of articles
    lister_princ <- principal_lister(input_date,input_tam)

    # Generate the main list of articles
    lista_prin <- lister_princ[[1]]
    lista_prin <- as.data.frame(lista_prin)
    titulos <- lista_prin$Article
    posicaovazio <- which(titulos != "")
    titu_art <- titulos[posicaovazio]

    # Generate article positions
    posic <- lister_princ[[1]]$`Position of article`
    posicvazio <- which(posic != "")
    posicao <- posic[posicaovazio]
    posicaoartigo <- as.numeric(posicao)
    artigos <- titu_art[posicaoartigo]

    # Create the table with the chosen articles
    tabela_artic <- cbind(posicaoartigo,titu_art,artigos)
    tabela_artic <- as.data.frame(tabela_artic)

    # Parameter input
    art_esc <- position_artic
    parame_lister <- total_list
    n <- length(art_esc)

    # Take the test to create a list of titles
    if(parame_lister == F|FALSE){

      # Create the list of titles
      sequenc_artic <- art_esc
      tabela_articredu <- tabela_artic[sequenc_artic,]
      titu_art <- tabela_articredu$artigos

    }else{

      # Create the list of titles
      sequenc_artic <- art_esc
      titu_art <- tabela_artic$artigos
    }

    # Base of entry for title comparison
    base_scopuswos <- input_date
    titu_base <- base_scopuswos$TI

    # Create spaces to store variables
    author <- numeric(n)
    journal <- numeric(n)
    volum <- numeric(n)
    pages <- numeric(n)
    year <- numeric(n)
    publisher <- numeric(n)

    # Compare article titles
    for(i in 1:n){

      j <- 0

      repeat {

        j <- j + 1

        if(titu_art[i] == titu_base[j])
          break
      }

      # Store the values in each round
      author[i] <- base_scopuswos$AU[j]
      journal[i] <- base_scopuswos$SO[j]
      volum[i] <- base_scopuswos$VL[j]
      pages[i] <- base_scopuswos$PP[j]
      year[i] <- base_scopuswos$PY[j]
      publisher[i] <- base_scopuswos$PU[j]
    }

    # Create variables
    autor_cita <- numeric(n)
    prim_autor <- numeric(n)
    soprim_autor <- numeric(n)
    sobprim_autor <- numeric(n)
    autor_minu <- numeric(n)

    # Separate authors
    pr_autor <- strsplit(author,split = ";")

    # Loop to create first author and citation
    for(i in 1:n){

      prim_autor[i] <- pr_autor[[i]][1]
      soprim_autor[i] <- strsplit(prim_autor[i],split = " ")
      sobprim_autor[i] <- soprim_autor[[i]][1]
      autor_minu[i] <- tolower(sobprim_autor[i])
      autor_cita[i] <- paste(autor_minu[i],year[i],
                             sep = "")
    }

    # Function that changes the title to lowercase
    # Calculate variable sizes
    titulo_maius <- numeric(n)
    journal_maius <- numeric(n)
    editor_maius <- numeric(n)
    titulo <- numeric(n)
    jornais <- numeric(n)
    editor <- numeric(n)

    # Create new variable for titles
    titulo_artigo <- titu_art

    # Create the loop
    for(i in 1:n){

      # Input with capital letter
      entrada_titulo <- titulo_artigo[i]
      entrada_journal <- journal[i]
      entrada_editor <- publisher[i]

      # Entry indexed with capital letter
      titulo_maius[i] <- entrada_titulo
      journal_maius[i] <- entrada_journal
      editor_maius[i] <- entrada_editor

      # Change the format to lowercase
      titulo_minu <- tolower(entrada_titulo)
      journal_minu <- tolower(entrada_journal)
      editor_minu <- tolower(entrada_editor)

      # Separate words
      titulo_separ <- strsplit(titulo_minu," ")
      journal_separ <- strsplit(journal_minu," ")
      editor_separ <- strsplit(editor_minu," ")

      # Select first word
      primei_palati <- titulo_separ[[1]][1]
      primei_palajour <- journal_separ[[1]][1]
      primei_palaedi <- editor_separ[[1]][1]

      # Separate letters from words
      plt <- strsplit(primei_palati,"")
      pljour <- strsplit(primei_palajour,"")
      pledi <- strsplit(primei_palaedi,"")

      # Select first letter of words
      primeira_letrati <- plt[[1]][1]
      primeira_palajour <- pljour[[1]][1]
      primeira_palaedi  <- pledi[[1]][1]

      # Capitalize the first letter
      primeira_maiuti <- toupper(primeira_letrati)
      primeira_maiujour <- toupper(primeira_palajour)
      primeira_maiuedi <- toupper(primeira_palaedi)

      # Replace the lowercase letter with the uppercase letter
      titu <- sub(primeira_letrati,primeira_maiuti,titulo_minu)
      jour <- sub(primeira_palajour,primeira_maiujour,journal_minu)
      edi <- sub(primeira_palaedi,primeira_maiuedi,editor_minu)

      # Create indexed variable
      titulo[i] <- titu
      jornais[i] <- jour
      editor[i] <- edi
    }

    # Create variables for the table
    titu_art <- titulo
    journal <- jornais
    publisher <- editor

    # Create the table with the variables
    tabel_bibtex <- cbind(autor_cita,titu_art, author, journal, volum, pages, year, publisher)

    # Modify column names and format
    colnames(tabel_bibtex) <- c("fonte_ano","titulos","autores","jornais",
                                "volumes","paginas","anos","publicacoes")

    # Modify to table format
    tabel_bibtex <- as.data.frame(tabel_bibtex)

    # Returns information in bibtex format
    return(tabel_bibtex)
  }

  # Function that generates the file in bibtex format
  bibtex_lister <- function(tabel_bibtex,indic){

    # Data entry
    input_tabel <- tabel_bibtex
    tabela <- input_tabel
    linha <- indic

    # Reduce database size
    tabela_redu <- tabela[linha,]
    tabela_ent <- tabela_redu

    # Assign input information to variables
    simbolo_entrada <- "@article{"
    fonte_ano <- tabela_ent$fonte_ano
    titulos <- tabela_ent$titulos
    autores <- tabela_ent$autores
    jornais <- tabela_ent$jornais
    volumes <- tabela_ent$volumes
    paginas <- tabela_ent$paginas
    anos <- tabela_ent$anos
    publicacoes <- tabela_ent$publicacoes

    # Create the elements of the bibtex format structure
    espaco <- "  "
    virgula <- ","
    tituloart <- "title="
    autorart <- "author="
    jornalart <- "journal="
    volumeart <- "volume="
    paginasart <- "pages="
    anosart <- "year="
    publicacaoart <- "publisher="
    chave_entrada <- "{"
    chave_saida <- "}"

    # Create the bibtex structure for each variable
    chave <- paste0(simbolo_entrada,fonte_ano,virgula)
    titulo <- paste0(tituloart,chave_entrada,titulos,
                     chave_saida)
    autor <- paste0(autorart,chave_entrada,autores,
                    chave_saida)
    jornal <- paste0(jornalart,chave_entrada,jornais,
                     chave_saida)
    volume <- paste0(volumeart,chave_entrada,volumes,
                     chave_saida)
    pagina <- paste0(paginasart,chave_entrada,paginas,
                     chave_saida)
    ano <- paste0(anosart,chave_entrada,anos,
                  chave_saida)
    publicacao <- paste0(publicacaoart,chave_entrada,
                         publicacoes, chave_saida)

    # Create bibtex format for articles
    referen_bibtex <- paste0(chave,"\n",
                             espaco,titulo, virgula,"\n",
                             espaco,autor, virgula, "\n",
                             espaco,jornal, virgula,"\n",
                             espaco,volume, virgula,"\n",
                             espaco,pagina, virgula,"\n",
                             espaco,ano, virgula,"\n",
                             espaco,publicacao,"\n", chave_saida,
                             espaco,collapse = ",\n")

    # Returns the file in bibtex format
    return(referen_bibtex)
  }

  # Call the function that generates the information
  tabel_bibtex <- gerad_lister(input_date, input_tam, position_artic, total_list)

  # Calculate file list size
  nlista <- length(position_artic)

  # Create the variable to store values
  arquivo_bibtex <- c()

  # Create the loop to generate the list of bibtex files
  for(i in 1:nlista){

    # Create the parameter linked to the index
    indic <- i

    # Call the function that converts to bibtex format
    refer_bibtex <- bibtex_lister(tabel_bibtex,indic)
    arquivo_bibtex[i] <- refer_bibtex

  }

  # Return the value of the variable
  return(arquivo_bibtex)
}
