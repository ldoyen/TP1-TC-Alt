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

cmoninit <- function(){
  e <- get("e", parent.frame())
  e$log$skipped<-c()#pour corriger un bugg swirl: quand on fait deux leçons d'affile, il y a FALSE à l'initialisation de skipped, alors que ce n'est pas le cas pour la première leçon ???

  return(TRUE)
}

submit_log <- function(){
  res<-FALSE
  selection <- getState()$val
if(selection %in% 1:5){
  res<-TRUE

  demande_num<-"Quel est votre num\xE9ro d'\xE9tudiant ? "
  Encoding(demande_num) <- "latin1"
  num_etud <- readline(demande_num)
  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSedErETojgRotoYLiI8ynXy6pcg7n_zMHOm40IJuBP5Ucm7Aw/viewform?usp=pp_url&entry.396843662="
  pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSd1F_R0PF2TlNGt2fWokYQXXHI04VUmGuXAt28Zg3dLtrkZvw/viewform?usp=pp_url&entry.1337846868="
  pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLScu_WmWluTBJQm7sJXyVVkb0WM0W2RyhAUE6ZQYA77HgilMzw/viewform?usp=pp_url&entry.419018389="
  pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLScii7wiYhvVPqXJUwtoLmEuKitUDWBpk1tzJwNI050nq1XD5g/viewform?usp=pp_url&entry.1843359488="
  pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSeALcMgvPVUbOglitGCfmLWEc5NaKk3Cp1dVhJvE2fZtedNcA/viewform?usp=pp_url&entry.1206664424="

  pre_fill_link <- switch(selection,
    pre_fill_link1,
    pre_fill_link2,
    pre_fill_link3,
    pre_fill_link4,
    pre_fill_link5
  )

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
  readline("Swirl va maintenant ouvrir un Google Form dans votre navigateur web. Tapez sur la touche Entrée.")
  browseURL(e$url_googleForm)

  e <- get("e", parent.frame())
    if(selection %in% c(3,4,5)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
    e$sujet_email<-paste0("**TP1-TC-CI**"," G",selection,", ",log_$lesson_name,", ", nom_etud,collapse="")
    e$corp_email<-encoded_log
  }
  return(res)
}

submit_log_alt <- function(){
  res<-FALSE
  selection <- getState()$val
#if(selection %in% 1:5){
  res<-TRUE

  demande_num<-"Quel est votre num\xE9ro d'\xE9tudiant ? "
  Encoding(demande_num) <- "latin1"
  num_etud <- readline(demande_num)
  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  #pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSedErETojgRotoYLiI8ynXy6pcg7n_zMHOm40IJuBP5Ucm7Aw/viewform?usp=pp_url&entry.396843662="
  #pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSd1F_R0PF2TlNGt2fWokYQXXHI04VUmGuXAt28Zg3dLtrkZvw/viewform?usp=pp_url&entry.1337846868="
  #pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLScu_WmWluTBJQm7sJXyVVkb0WM0W2RyhAUE6ZQYA77HgilMzw/viewform?usp=pp_url&entry.419018389="
  #pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLScii7wiYhvVPqXJUwtoLmEuKitUDWBpk1tzJwNI050nq1XD5g/viewform?usp=pp_url&entry.1843359488="
  #pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSeALcMgvPVUbOglitGCfmLWEc5NaKk3Cp1dVhJvE2fZtedNcA/viewform?usp=pp_url&entry.1206664424="

  #pre_fill_link <- switch(selection,
  #  pre_fill_link1,
  #  pre_fill_link2,
  #  pre_fill_link3,
  #  pre_fill_link4,
  #  pre_fill_link5
  #)
  pre_fill_link <-"https://docs.google.com/forms/d/e/1FAIpQLScp9cm0k_HLtV80Ko0yRuWv1jhLTtbIO0IWTox08ayub4002w/viewform?usp=pp_url&entry.2086698556="

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
  readline("Swirl va maintenant ouvrir un Google Form dans votre navigateur web. Tapez sur la touche Entrée.")
  browseURL(e$url_googleForm)

  e <- get("e", parent.frame())
    #if(selection %in% c(3,4,5)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
    #e$sujet_email<-paste0("**TP1-TC-CI**"," G",selection,", ",log_$lesson_name,", ", nom_etud,collapse="")
    e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr"
    e$sujet_email<-paste0("**TP1-TC-CI**"," Alt, ",log_$lesson_name,", ", nom_etud,collapse="")
    e$corp_email<-encoded_log
  #}
  return(res)
}


googleForm_log<-function(){
  e <- get("e", parent.frame())
  if(regexpr("Google Form",e$val)!=-1){
    res<-FALSE
    browseURL(e$url_googleForm)
  } else {
    res<-TRUE
   readline("Swirl va maintenant ouvrir un email dans votre logiciel de messagerie. Tapez sur la touche Entrée.")
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(res)
}


email_log<-function(){
  e <- get("e", parent.frame())
  res<-TRUE
  if(regexpr("email",e$val)!=-1){
    res<-FALSE
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(res)
}

sauve_log<-function(){
  demande<-"Appuyez sur Entr\xE9, puis choississez un r\xE9pertoire dans lequel sauver votre cl\xE9. Attention, dans les salles machine de l'IUT, choississez un r\xE9pertoire personnel."
  Encoding(demande) <- "latin1"
  rep <- readline(demande)
  path <- choose_dir()
  if(length(path)==0){
    return(FALSE)
  } else {
    setwd(path)
    e <- get("e", parent.frame())
    encoded_log<-e$encoded_log
    log_tbl<-e$log_tbl
    log_ <- getLog()
    e$fichier<-paste0("TP2",log_$lesson_name,".R")

    save(log_tbl,encoded_log,file=e$fichier)
    demande<-paste0("Votre cl\xE9, est sauv\xE9 dans le fichier ",e$fichier," Tapez sur la touche Entr\xE9e pour continuer.")
    Encoding(demande) <- "latin1"
    rep <- readline(demande)
    return(TRUE)
  }
}

qsauve_log<-function(){
e <- get("e", parent.frame())
if(e$val=="Oui"){
  return(TRUE)
} else {
  demande<-"Appuyez sur Entr\xE9, puis choississez un r\xE9pertoire dans lequel sauver votre cl\xE9. Attention, dans les salles machine de l'IUT, choississez un r\xE9pertoire personnel."
  Encoding(demande) <- "latin1"
  rep <- readline(demande)
  path <- choose_dir()
  if(length(path)==0){
    return(FALSE)
  } else {
    setwd(path)
    e <- get("e", parent.frame())
    encoded_log<-e$encoded_log
    log_tbl<-e$log_tbl

    save(log_tbl,encoded_log,file=e$fichier)
    demande<-paste0("Votre cl\xE9, est sauv\xE9 dans le fichier ",e$fichier," Tapez sur la touche Entr\xE9e pour continuer.")
    Encoding(demande) <- "latin1"
    rep <- readline(demande)
    return(FALSE)
  }
}
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
