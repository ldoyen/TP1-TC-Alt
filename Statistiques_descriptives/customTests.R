# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

submit_log <- function(){
  selection <- getState()$val
  res<-(selection=="Oui")

if(res){

  demande_num<-"Quel est votre num\xE9ro d'\xE9tudiant ? "
  Encoding(demande_num) <- "latin1"
  num_etud <- readline(demande_num)
  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLScp9cm0k_HLtV80Ko0yRuWv1jhLTtbIO0IWTox08ayub4002w/viewform?usp=pp_url&entry.2086698556="

  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }

  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame( p(log_$question_number, nrow_, NA),
                         p(log_$correct, nrow_, NA),
                         p(log_$attempt, nrow_, NA),
                         p(log_$skipped, nrow_, NA),
                         p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  names(log_tbl) <- c(num_etud, nom_etud, prenom_etud,log_$lesson_name,"t")
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  e <- get("e", parent.frame())
  e$url_googleForm<-paste0(pre_fill_link, encoded_log)
  #browseURL(paste0(pre_fill_link, encoded_log)
  readline("Swirl va maintenant ouvrir un Google Form dans votre navigateur web. Tapez sur la touche Entrer.")
  browseURL(e$url_googleForm)

  e <- get("e", parent.frame())
  if(selection %in% c(1,2,3)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
  e$sujet_email<-paste0("**TP1-TC-CI**"," G",selection,", ",log_$lesson_name,", ", nom_etud,collapse="")
  e$corp_email<-encoded_log


}
  return(res)
}

googleForm_log<-function(){
  e <- get("e", parent.frame())
  if(e$val=="Non"){
    browseURL(e$url_googleForm)
  } else {
   readline("Swirl va maintenant ouvrir un email dans votre logicel de messagerie. Tapez sur la touche Entrer.")
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(e$val=="Oui")
}


email_log<-function(){
  e <- get("e", parent.frame())
  if(e$val=="Non"){
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(e$val=="Oui")
}


#answear test to known if the value of the answear is between b_inf and b_sup
test_between <- function(b_inf,b_sup){
  n<-length(b_inf)
  res<-TRUE
  e <- get("e", parent.frame())
  e<-e$value
  for(i in 1:n){
    res<-res&(e[i] >= b_inf[i])&(e[i] <= b_sup[i])
  }
  return(res)
}
