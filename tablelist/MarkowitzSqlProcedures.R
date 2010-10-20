AA_seleziona_matrice_covarianza <- function(referenceCurrency,odbcConnection=odbcCon)
  {
    query <- "DROP TABLE G_covarianze_azionarie"
    result <- sqlCommand(channel=odbcConnection,query=query,
                         errorText="Function AA_seleziona_matrice_covarianza:\nDROP TABLE G_covarianze_azionarie")

    query <- paste("SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                   "INTO G_covarianze_azionarie ",
                   "FROM G_covarianze_azionarie_",referenceCurrency,
                   sep=""
                  )
    result <- sqlCommand(channel=odbcConnection,query=query,
                         errorText="Function AA_seleziona_matrice_covarianza:\nDROP TABLE G_covarianze_azionarie")

    query <- "DROP TABLE G_covarianze_azionarie_moneta_rif"
    result <- sqlCommand(channel=odbcConnection,query=query,
                         errorText="Function AA_seleziona_matrice_covarianza:\nDROP TABLE G_covarianze_azionarie")

    query <- paste("SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                   "INTO G_covarianze_azionarie_moneta_rif ",
                   "FROM G_covarianze_azionarie_moneta_",referenceCurrency,
                   sep=""
                  )
    result <- sqlCommand(channel=odbcConnection,query=query,
                         errorText="Function AA_seleziona_matrice_covarianza:\nDROP TABLE G_covarianze_azionarie")
  }


preparazione_dati_ottimizzazione_step_1 <- function(client,moneta_investimento,channel)
  {
    indice_restrizioni_esatte = -1
    numero_vincolo = 1

#/////////////////////////////////////////////////////////////////////////////////////
#////////// Creazione tabella "D_universo_markowitz" dei fattori di rischio //////////
#/////////////////////////////////////////////////////////////////////////////////////

#popola la tabella D_universo_markowitz
    query <- paste("DELETE FROM D_universo_markowitz WHERE Cliente LIKE '",client,"'",sep="")
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query1")



    query <- paste("INSERT INTO D_universo_markowitz (Cliente, Moneta, Branche, MonetaBranche,Ticker)",
                   "SELECT Cliente, Moneta, 'Moneta' AS Branche, Moneta+'_Moneta', Moneta AS Ticker ",
                   "FROM C_Restrizioni_monete ",
                   "WHERE Cliente LIKE '",client,"' AND Moneta <> '",moneta_investimento,"'",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query2")


    query <- paste("INSERT INTO D_universo_markowitz (Cliente, Moneta, Branche,MonetaBranche,Ticker,Company) ",
                   "SELECT Cliente, Moneta, Branche, Moneta+'_'+Branche,Ticker,Company FROM A_titoli_selezionati ",
                   "WHERE Cliente LIKE '",client,"'",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query3")

    # popola la tabella D_universo_markowitz_monete_settori_passo_1
    query <- paste("DELETE FROM D_universo_markowitz_monete_settori_passo_1 WHERE Cliente LIKE '",client,"'",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query4")

    query <- paste("INSERT INTO D_universo_markowitz_monete_settori_passo_1 (Cliente, Moneta, Branche) ",
                   "SELECT Cliente, Moneta, 'Moneta' AS Branche ",
                   "FROM C_Restrizioni_monete ",
                   "WHERE Cliente LIKE '",client,"' AND Moneta <> '",moneta_investimento,"'",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query5")

    query <- paste("INSERT INTO D_universo_markowitz_monete_settori_passo_1 ",
                   "SELECT DISTINCT Cliente, Moneta, Branche FROM A_titoli_selezionati ",
                   "WHERE Cliente LIKE '",client,"'",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query6")

    #/////////////////////////////////////////////////////////////////////////////////////
    #/////////////// Creazione tabella "D_restrizioni_markowitz_passo_1" /////////////////
    #/////////////////////////////////////////////////////////////////////////////////////
    query <- paste("DELETE FROM D_restrizioni_markowitz_passo_1 ",
                   "WHERE Cliente LIKE '",client,"'",
                   sep="")
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_dati_ottimizzazione_step_1:\n query7")

    # determina se il risk free nella moneta di investimento è desiderato ed il tipo di vincoli
    query <- paste("SELECT * FROM C_Restrizioni_risk_free ",
                   "WHERE Moneta LIKE '",moneta_investimento,"' AND ",
                   "Cliente Like '",client,"'",
                   sep=""
                  )
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is empty
    if (nrow(df.rs) == 0)
      {
        #W_home = 0 !
        # metti la restrizione sui pesi delle altre valute + i pesi delle azioni nella valuta di riferimento = 1
        query <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                       "SELECT '",client,"', ",indice_restrizioni_esatte,", ",
                       "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' OR Moneta LIKE '",
                       moneta_investimento,"' THEN 1 ELSE 0 END, 1, 0 ",
                       "FROM D_universo_markowitz_monete_settori_passo_1 ",
                       "WHERE Cliente LIKE '",client,"'",
                       sep=""
                      )
        result <- sqlCommand(channel=channel,query=query,
                             errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
        indice_restrizioni_esatte = indice_restrizioni_esatte - 1
      }
    else
      {
        if (is.na(df.rs[1,"Min"]))
          {
            query <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                           "SELECT '",client,"', ",numero_vincolo,", ",
                           "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' OR Moneta LIKE '",
                           moneta_investimento,"' THEN -1 ELSE 0 END, -1, -1 ",
                           "FROM D_universo_markowitz_monete_settori_passo_1 ",
                           "WHERE Cliente LIKE '",client,"'",
                           sep=""
                          )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
            numero_vincolo = numero_vincolo + 1
          }
        else
          {
            # fai l'insert nella tabella D_restrizioni_markowitz_passo_1 della restrizione >= min
            query <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                           "SELECT '",client,"', ",numero_vincolo,", ",
                           "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' OR Moneta LIKE '",
                           moneta_investimento,"' THEN -1 ELSE 0 END, ",df.rs[1,"Min"] ," - 1, -1 ",
                           "FROM D_universo_markowitz_monete_settori_passo_1 ",
                           "WHERE Cliente LIKE '",client,"'",
                           sep=""
                          )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")

            numero_vincolo = numero_vincolo + 1
          }

        if (!is.na(df.rs[1,"Max"]))
          {
            # fai l'insert nella tabella D_restrizioni_markowitz_passo_1 della restrizione <= max
            query <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                           "SELECT '",client,"', ",numero_vincolo,", ",
                           "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' OR Moneta LIKE '",
                           moneta_investimento,"' THEN 1 ELSE 0 END, 1 - ",df.rs[1,"Max"],", 1 ",
                           "FROM D_universo_markowitz_monete_settori_passo_1 ",
                           "WHERE Cliente LIKE '",client,"'",
                           sep=""
                          )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")

            numero_vincolo = numero_vincolo + 1
          }
      }

    rm(df.rs)

    # fai un loop su tutti i cambi tranne quello in moneta di riferimento e, se il rf della moneta
    # in questione è presente allora la restrizione che w_fx - sum(azioni_fx) >=0 sara' inserita dopo, quando
    # si tratteranno le restrizioni riguardanti i risk free interest rates, mentre
    # se il rf non è presente allora inserisci la restrizione che w_fx - sum(azioni_fx) = 0

    query <- paste("SELECT dbo.D_universo_markowitz_monete_settori_passo_1.Cliente, dbo.D_universo_markowitz_monete_settori_passo_1.Moneta AS Moneta1, dbo.A_risk_free_selezionati.Moneta AS Moneta2 ",
                   "FROM  dbo.D_universo_markowitz_monete_settori_passo_1 LEFT OUTER JOIN ",
                   "dbo.A_risk_free_selezionati ON dbo.D_universo_markowitz_monete_settori_passo_1.Moneta = dbo.A_risk_free_selezionati.Moneta AND ",
                   "dbo.D_universo_markowitz_monete_settori_passo_1.Cliente = dbo.A_risk_free_selezionati.Cliente ",
                   "GROUP BY dbo.D_universo_markowitz_monete_settori_passo_1.Cliente, dbo.D_universo_markowitz_monete_settori_passo_1.Moneta, dbo.A_risk_free_selezionati.Moneta ",
                   "HAVING (dbo.D_universo_markowitz_monete_settori_passo_1.Cliente = '",client,"') AND ",
                   "(dbo.D_universo_markowitz_monete_settori_passo_1.Moneta NOT LIKE '",moneta_investimento,"')",
                   sep=""
                  )
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is not empty
    if (nrow(df.rs) != 0)
      {
        # fai un loop sulle monete e, per ciascuna di esse, inserisci le restrizioni tra fx e titoli nella stessa moneta.
        notAvailable <- is.na(df.rs[,"Moneta2"])
        nbNotAvailable <- sum(notAvailable)
        if (nbNotAvailable>0)
          {
            index <- seq(from=indice_restrizioni_esatte, to=indice_restrizioni_esatte-nbNotAvailable+1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[notAvailable,"Moneta1"],"' THEN 1 WHEN Branche NOT LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[notAvailable,"Moneta1"],"' THEN -1 ELSE 0 END, 0, 0 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            indice_restrizioni_esatte <- indice_restrizioni_esatte-nbNotAvailable
            rm(index,queries)
          }
        rm(notAvailable,nbNotAvailable)
      }
    rm(df.rs)

                                        # fai un loop sui risk free restanti

    query <- paste("SELECT * FROM C_Restrizioni_risk_free ",
                   "WHERE Moneta NOT LIKE '",moneta_investimento,"' AND ",
                   "Cliente Like '",client,"'",
                   sep=""
                   )
    df.rs <- sql.get.table(myConnection=channel, query=query)
                                        # if the query is not empty
    if (nrow(df.rs) != 0)
      {
                                        # ci sono altri risk free oltre quello in moneta di riferimento che vanno considerati
                                        # inizia un loop sui vari risk_free

        isNaMin <- is.na(df.rs[,"Min"])
        nbNaMin <- sum(isNaMin)
        if (nbNaMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbNaMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isNaMin,"Moneta"],"' THEN 1 WHEN Branche NOT LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isNaMin,"Moneta"],"' THEN -1 ELSE 0 END, 0, -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbNaMin
            rm(index,queries)
          }

        # fai l'insert nella tabella D_universo_markowitz_passo_1 della restrizione >= min
        nbNaMin <- sum(!isNaMin) # very important, in this case nbNaMin is misleading
        if (nbNaMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbNaMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[!isNaMin,"Moneta"],"' THEN 1 WHEN Branche NOT LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[!isNaMin,"Moneta"],"' THEN -1 ELSE 0 END, ",df.rs[!isNaMin,"Min"],", -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbNaMin
            rm(index,queries)
          }
        rm(nbNaMin,isNaMin)

        # now consider the Max constraint
        isValidMax <- !is.na(df.rs[,"Max"])
        nbValidMax <- sum(isValidMax)
        if (nbValidMax>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMax-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isValidMax,"Moneta"],"' THEN -1 WHEN Branche NOT LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isValidMax,"Moneta"],"' THEN 1 ELSE 0 END, -1 *", df.rs[isValidMax,"Max"],", 1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                             )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMax
            rm(index,queries)
          }
      } # end loop on the risk_free
    rm(df.rs)




    # inizio introduzione restrizioni moneta
    # Dapprima consideriamo solo le monete non di investimento

    query <- paste("SELECT * FROM C_Restrizioni_monete ",
                   "WHERE Cliente LIKE '",client,"' AND Moneta NOT LIKE '",
                   moneta_investimento,"'",
                   sep=""
                  )
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is not empty
    if (nrow(df.rs) != 0)
      {
        isValidMin <- !is.na(df.rs[,"Min"])
        nbValidMin <- sum(isValidMin)
        if (nbValidMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isValidMin,"Moneta"],"' THEN 1 ELSE 0 END, ",df.rs[isValidMin,"Min"],", -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMin
            rm(index,queries)
          }
        rm(isValidMin,nbValidMin)

        # now the Max constraints
        isValidMax <- !is.na(df.rs[,"Max"])
        nbValidMax <- sum(isValidMax)
        if (nbValidMax>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMax-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta LIKE '",
                             df.rs[isValidMax,"Moneta"],"' THEN -1 ELSE 0 END, -1 * ",df.rs[isValidMax,"Max"],", 1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMax
            rm(index,queries)
          }
        rm(isValidMax,nbValidMax)
      }
    rm(df.rs)


    # ora verifichiamo se è necessario imporre restrizioni monetarie sulla moneta di riferimento
    query <- paste("SELECT * FROM C_Restrizioni_monete ",
                   "WHERE Cliente LIKE '",client,"' AND Moneta LIKE '",
                   moneta_investimento,"'",
                   sep=""
                   )
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is not empty
    if (nrow(df.rs) != 0)
      {
        isValidMin <- !is.na(df.rs[,"Min"])
        nbValidMin <- sum(isValidMin)
        if (nbValidMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta NOT LIKE '",
                             moneta_investimento,"' THEN -1 ELSE 0 END, ",df.rs[isValidMin,"Min"]," - 1, -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMin
            rm(index,queries)
          }

        # now the Max constraints
        isValidMax <- !is.na(df.rs[,"Max"])
        nbValidMax <- sum(isValidMax)
        if (nbValidMax>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMax-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", ",
                             "Moneta, Branche, CASE WHEN Branche LIKE 'Moneta' AND Moneta NOT LIKE '",
                             moneta_investimento,"' THEN 1 ELSE 0 END, 1 - ",df.rs[isValidMax,"Max"],", 1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMax
            rm(index,queries)
          }
        rm(isValidMax,nbValidMax)
      }
    rm(df.rs)




    # Accodamento restrizioni settoriali
    query <- paste("SELECT * FROM C_Restrizioni_settore WHERE Cliente LIKE '",client,"'",sep="")
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is not empty
    if (nrow(df.rs) != 0)
      {
        isValidMin <- !is.na(df.rs[,"Min"])
        nbValidMin <- sum(isValidMin)
        if (nbValidMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", Moneta , Branche, ",
                             "CASE WHEN Branche = '",df.rs[isValidMin,"Branche"],"' THEN 1 ELSE 0 END, ",
                             df.rs[isValidMin,"Min"],", -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                             )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMin
            rm(index,queries)
          }
        rm(isValidMin,nbValidMin)

                                        # now the constraints Max
        isValidMax <- !is.na(df.rs[,"Max"])
        nbValidMax <- sum(isValidMax)
        if (nbValidMax>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMax-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", Moneta , Branche, ",
                             "CASE WHEN Branche = '",df.rs[isValidMax,"Branche"],
                             "' THEN -1 ELSE 0 END, -1 * ",df.rs[isValidMax,"Max"],", 1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMax
            rm(index,queries)
          }
        rm(isValidMax,nbValidMax)
      }
    rm(df.rs)


                                        #Accodamento restrizioni monetarie e settoriali
    query <- paste("SELECT * FROM C_Restrizioni_monete_e_settori WHERE Cliente LIKE '",client,"'",sep="")
    df.rs <- sql.get.table(myConnection=channel, query=query)

    # if the query is not empty
    if (nrow(df.rs) != 0)
      {
        isNaMin <- is.na(df.rs[,"Min"])
        nbNaMin <- sum(isNaMin)
        if (nbNaMin>0)
      {
        index <- seq(from=numero_vincolo, to=numero_vincolo+nbNaMin-1)
        queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                         "SELECT '",client,"', ",index,", Moneta , Branche, ",
                         "CASE WHEN Moneta = '",df.rs[isNaMin,"Moneta"],"' ",
                         "AND Branche = '",df.rs[isNaMin,"Branche"],
                         "' THEN 1 ELSE 0 END, 0, -1 ",
                         "FROM D_universo_markowitz_monete_settori_passo_1 ",
                         "WHERE Cliente LIKE '",client,"'",
                         sep=""
                        )
        for (query in queries)
          {
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
          }
        numero_vincolo <- numero_vincolo+nbNaMin
        rm(index,queries)
      }
        rm(nbNaMin)


     # now the !isNaMin
        isValidMin <- !isNaMin
        nbValidMin <- sum(isValidMin)
        if (nbValidMin>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMin-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", Moneta , Branche, ",
                             "CASE WHEN Moneta = '",df.rs[isValidMin,"Moneta"],"' ",
                             "AND Branche = '",df.rs[isValidMin,"Branche"],"' THEN 1 ELSE 0 END, ",
                             df.rs[isValidMin,"Min"],", -1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMin
            rm(index,queries)
          }
        rm(isNaMin,isValidMin,nbValidMin)

                                        # now the Max constraints
        isValidMax <- !is.na(df.rs[,"Max"])
        nbValidMax <- sum(isValidMax)
        if (nbValidMax>0)
          {
            index <- seq(from=numero_vincolo, to=numero_vincolo+nbValidMax-1)
            queries <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                             "SELECT '",client,"', ",index,", Moneta , Branche, ",
                             "CASE WHEN Moneta = '",df.rs[isValidMax,"Moneta"],"' ",
                             "AND Branche = '",df.rs[isValidMax,"Branche"],
                             "' THEN -1 ELSE 0 END, -1 * ", df.rs[isValidMax,"Max"],", 1 ",
                             "FROM D_universo_markowitz_monete_settori_passo_1 ",
                             "WHERE Cliente LIKE '",client,"'",
                             sep=""
                            )
            for (query in queries)
              {
                result <- sqlCommand(channel=channel,query=query,
                                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
              }
            numero_vincolo <- numero_vincolo+nbValidMax
            rm(index,queries)
          }
        rm(isValidMax,nbValidMax)
      }
    rm(df.rs)


    # Accodamento restrizioni multiple

    index_restrizione_multipla = 1

    # In questo loop si utilizza una variabile "indice" che assume il valore della variabile
    # numero_vincolo oppure indice_restrizioni_esatte a seconda che la restrizione sia o non sia esatta

    query <- paste("SELECT DISTINCT NumeroRestrizione, TipoRestrizione, ValoreRestrizione ",
                   "FROM C_Restrizioni_multiple WHERE Cliente LIKE '",client,"' ",
                   sep=""
                  )
    df.rs <- sql.get.table(myConnection=channel,query=query,as.is=c(F,T,F))

    # determine the type of constraints, i.e. ">=", "<=" or "="
    isLessEq <- df.rs[,"TipoRestrizione"] == "<="
    isBigEq <- df.rs[,"TipoRestrizione"] == ">="
    isEq <- df.rs[,"TipoRestrizione"] == "="

    # consider the "<="
    nbLessEq <- sum(isLessEq)
    if (nbLessEq > 0)
      {
        dummy = -1
        Tipo_restrizione = 1
        index_restrizione_multipla = seq(from=max(index_restrizione_multipla),
                                 to=max(index_restrizione_multipla)+nbLessEq-1)
        index = seq(from=numero_vincolo,to=numero_vincolo+nbLessEq-1)
        queries1 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 (Cliente, Numero_vincolo, Moneta, Branche, Valore, b, Tipo_restrizione)",
                          "SELECT '",client,"', ",index,", Moneta, 'Moneta' AS Branche, 0, ",dummy * df.rs[isLessEq,"ValoreRestrizione"],", ",
                          Tipo_restrizione,
                          "FROM C_Restrizioni_monete ",
                          "WHERE Cliente LIKE '",client,"' AND Moneta <> '",moneta_investimento,"'",
                          sep=""
                          )

        queries2 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                          "SELECT '",client,"', ",index,", Moneta , Branche, ",
                          dummy,"* (CASE WHEN Selezionato = 0 THEN 0 ELSE 1 END), ",
                          dummy * df.rs[isLessEq,"ValoreRestrizione"],", ",Tipo_restrizione,
                          "FROM C_Restrizioni_multiple WHERE Cliente LIKE '",client,"' ",
                          "AND NumeroRestrizione = ",index_restrizione_multipla,
                          sep=""
                          )
        for (query in c(queries1,queries2))
          {
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
          }

        numero_vincolo <- numero_vincolo + nbLessEq
        index_restrizione_multipla = max(index_restrizione_multipla)+1
      }

    # consider the ">="
    nbBigEq <- sum(isBigEq)
    if (nbBigEq > 0)
      {
        dummy = 1
        Tipo_restrizione = -1
        index_restrizione_multipla = seq(from=max(index_restrizione_multipla),
                 to=max(index_restrizione_multipla)+nbBigEq-1)
        index = seq(from=numero_vincolo,to=numero_vincolo+nbBigEq-1)
        queries1 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 (Cliente, Numero_vincolo, Moneta, Branche, Valore, b, Tipo_restrizione)",
                          "SELECT '",client,"', ",index,", Moneta, 'Moneta' AS Branche, 0, ",dummy * df.rs[isBigEq,"ValoreRestrizione"],", ",
                          Tipo_restrizione,
                          "FROM C_Restrizioni_monete ",
                          "WHERE Cliente LIKE '",client,"' AND Moneta <> '",moneta_investimento,"'",
                          sep=""
                          )

        queries2 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                          "SELECT '",client,"', ",index,", Moneta , Branche, ",
                          dummy,"* (CASE WHEN Selezionato = 0 THEN 0 ELSE 1 END), ",
                          dummy * df.rs[isBigEq,"ValoreRestrizione"],", ",Tipo_restrizione,
                          "FROM C_Restrizioni_multiple WHERE Cliente LIKE '",client,"' ",
                          "AND NumeroRestrizione = ",index_restrizione_multipla,
                          sep=""
                     )

        for (query in c(queries1,queries2))
          {
            result <- sqlCommand(channel=channel,query=query,
                     errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
          }

        numero_vincolo <- numero_vincolo + nbBigEq
        index_restrizione_multipla = max(index_restrizione_multipla)+1
      }



                                        # finally consider the "="
    nbEq <- sum(isEq)
    if (nbEq > 0)
      {
        dummy = 1
        Tipo_restrizione = 0
        index_restrizione_multipla = seq(from=max(index_restrizione_multipla),
                                to=max(index_restrizione_multipla)+nbEq-1)
        index = seq(from=index_restrizioni_esatte,to=index_restrizioni_esatte-nbEq+1)

        queries1 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 (Cliente, Numero_vincolo, Moneta, Branche, Valore, b, Tipo_restrizione)",
                          "SELECT '",client,"', ",index,", Moneta, 'Moneta' AS Branche, 0, ",dummy * df.rs[isEq,"ValoreRestrizione"],", ",
                          Tipo_restrizione,
                          "FROM C_Restrizioni_monete ",
                          "WHERE Cliente LIKE '",client,"' AND Moneta <> '",moneta_investimento,"'",
                          sep=""
                          )

        queries2 <- paste("INSERT INTO D_restrizioni_markowitz_passo_1 ",
                          "SELECT '",client,"', ",index,", Moneta , Branche, ",
                          dummy,"* (CASE WHEN Selezionato = 0 THEN 0 ELSE 1 END), ",
                          dummy * df.rs[isEq,"ValoreRestrizione"],", ",Tipo_restrizione,
                          "FROM C_Restrizioni_multiple WHERE Cliente LIKE '",client,"' ",
                          "AND NumeroRestrizione = ",index_restrizione_multipla,
                          sep=""
                          )

        for (query in c(queries1,queries2))
          {
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_dati_ottimizzazione_step_1:\n query")
          }

        index_restrizioni_esatte <- index_restrizioni_esatte - nbEq
        index_restrizione_multipla = max(index_restrizione_multipla)+nbEq
      }
    rm(df.rs)

  }


preparazione_covarianze_settoriali <- function(client,moneta_riferimento,channel)
  {
    # crea la matrice delle varianze covarianze in moneta CHF
    query <- "DROP TABLE E_covarianze_settoriali_CHF"
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_covarianze_settoriali:\n query")

    query <- paste("SELECT Ticker1, Ticker2, Covarianza ",
                   "INTO E_covarianze_settoriali_CHF ",
                   "FROM [Prezzi storici azioni (VAR)].dbo.Covarianze_settori ",
                   "WHERE (Ticker1 IN ",
                   "(SELECT DISTINCT MonetaBranche AS Ticker1 ",
                   "FROM D_universo_markowitz WHERE Cliente LIKE '", client, "') ",
                   "AND Ticker2 IN ",
                   "(SELECT DISTINCT MonetaBranche AS Ticker2 ",
                   "FROM D_universo_markowitz WHERE Cliente LIKE '", client, "'))",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_covarianze_settoriali:\n query")

    # crea la matrice delle varianze covarianze in moneta di riferimento
    query <- "DROP TABLE E_covarianze_settoriali"
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_covarianze_settoriali:\n query")

    query <- paste("SELECT Ticker1, Ticker2, Covarianza ",
                   "INTO E_covarianze_settoriali ",
                   "FROM E_covarianze_settoriali_CHF ",
                   sep=""
                  )
    result <- sqlCommand(channel=channel,query=query,
                         errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # Aggiusta, se necessario, la matrice delle varianze covarianze in moneta di riferimento

    if (moneta_riferimento != "CHF")
      {
                                        # verifica che la moneta di riferimento sia già stata inserita nella tabella E_covarianze_settoriali e se
                                        # ciò non fosse il caso inseriscila

        query <- paste("SELECT Covarianza FROM E_covarianze_settoriali WHERE Ticker1 LIKE '",
                       moneta_riferimento, "_Moneta' ",
                       "AND Ticker2 LIKE '", moneta_riferimento, "_Moneta'",
                       sep=""
                       )
        df.rs <- sql.get.table(myConnection=channel, query=query)
        if (nrow(df.rs)==0)
          {
                                        # seleziona la lista di tickers per cui è necessario estrarre la covarianza con la moneta di riferimento
            query <- "SELECT DISTINCT Ticker1 INTO DBTmp FROM E_covarianze_settoriali"
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # inserisci la varianza
            query <- paste("INSERT INTO E_covarianze_settoriali ",
                           "SELECT Ticker1, Ticker2, Covarianza ",
                           "FROM [Prezzi storici azioni (VAR)].dbo.Covarianze_settori ",
                           "WHERE Ticker1 LIKE '",moneta_riferimento,"_Moneta' AND Ticker2 LIKE '",
                           moneta_riferimento, "_Moneta'",
                           sep=""
                           )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # inserisci covarianza sinistra
            query <- paste("INSERT INTO E_covarianze_settoriali ",
                           "SELECT Ticker1, Ticker2, Covarianza ",
                           "FROM [Prezzi storici azioni (VAR)].dbo.Covarianze_settori ",
                           "WHERE (Ticker1 LIKE '", moneta_riferimento, "_Moneta' ",
                           "AND Ticker2 IN (SELECT * FROM DBTmp)) ",
                           sep=""
                           )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # inserisci covarianza destra
            query <- paste("INSERT INTO E_covarianze_settoriali ",
                           "SELECT Ticker1, Ticker2, Covarianza ",
                           "FROM [Prezzi storici azioni (VAR)].dbo.Covarianze_settori ",
                           "WHERE (Ticker2 LIKE '", moneta_riferimento, "_Moneta' ",
                           "AND Ticker1 IN (SELECT * FROM DBTmp)) ",
                           sep=""
                           )
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

            query <- "DROP TABLE DBTmp"
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # repeat the preceding query
            query <- paste("SELECT Covarianza FROM E_covarianze_settoriali WHERE Ticker1 LIKE '",
                           moneta_riferimento, "_Moneta' AND Ticker2 LIKE '",
                           moneta_riferimento, "_Moneta'",
                           sep=""
                           )
            df.rs <- sql.get.table(myConnection=channel, query=query)
          }


        varianza_moneta_riferimento = df.rs[1,"Covarianza"]
        rm(df.rs)

        query <- paste("UPDATE E_covarianze_settoriali SET Covarianza = -1 * Covarianza WHERE Ticker1 LIKE '",
                       moneta_riferimento, "_Moneta'",
                       sep=""
                       )
        result <- sqlCommand(channel=channel,query=query,
                             errorText="Function preparazione_covarianze_settoriali:\n query")

        query <- paste("UPDATE E_covarianze_settoriali SET Covarianza = -1 * Covarianza WHERE Ticker2 LIKE '",
                       moneta_riferimento, "_Moneta'",
                       sep=""
                       )
        result <- sqlCommand(channel=channel,query=query,
                             errorText="Function preparazione_covarianze_settoriali:\n query")

                                        # seleziona tutte le monete diverse da quella di riferimento che ci sono in portafoglio
        query <- paste("SELECT DISTINCT Moneta FROM D_universo_markowitz WHERE Branche = 'Moneta' ",
                       "AND Moneta <> '", moneta_riferimento, "' AND Cliente = '",
                       client, "'",
                       sep=""
                       )
        df.rs <- sql.get.table(myConnection=channel, query=query)

        if (nrow(df.rs)>0)
          {
            query1 <-
              paste("SELECT A.Ticker1, A.Ticker2, A.Covarianza AS Covarianza1, B.Covarianza AS Covarianza2 ",
                    "INTO DBTmp1 ",
                "FROM E_covarianze_settoriali AS A INNER JOIN E_covarianze_settoriali AS B ON A.Ticker1 = B.Ticker1 ",
                    "WHERE B.Ticker2 LIKE '", moneta_riferimento, "_Moneta'",
                    sep=""
                    )

            query2 <- "SELECT * INTO DBTmp2 FROM E_covarianze_settoriali"

            query3 <- "DROP TABLE E_covarianze_settoriali"

            query4 <-
              paste("SELECT Ticker1, Ticker2, CASE WHEN Ticker2 LIKE '", df.rs[,"Moneta"], "_Moneta' ",
                    "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END AS Covarianza ",
                    "INTO E_covarianze_settoriali ",
                    "FROM DBTmp1",
                    sep=""
                    )

            query5 <- "DROP TABLE DBTmp1"

            query6 <-
              paste("SELECT A.Ticker1, A.Ticker2, A.Covarianza AS Covarianza1, B.Covarianza AS Covarianza2 ",
                    "INTO DBTmp ",
                    "FROM E_covarianze_settoriali AS A INNER JOIN DBTmp2 AS B ON A.Ticker2 = B.Ticker2 ",
                    "WHERE B.Ticker1 LIKE '", moneta_riferimento, "_Moneta'",
                    sep=""
                    )
            query7 <- "DROP TABLE DBTmp2"
            query8 <- "DROP TABLE E_covarianze_settoriali"
            query9 <-
              paste("SELECT Ticker1, Ticker2, CASE WHEN Ticker1 LIKE '", df.rs[,"Moneta"], "_Moneta' ",
                    "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END AS Covarianza ",
                    "INTO E_covarianze_settoriali ",
                    "FROM DBTmp",
                    sep=""
                    )
            query10 <- "DROP TABLE DBTmp"

            query11 <-
              paste("UPDATE E_covarianze_settoriali SET Covarianza = Covarianza + ",
                    varianza_moneta_riferimento,
                    " WHERE Ticker1 LIKE '",
                    df.rs[,"Moneta"], "_Moneta' AND Ticker2 LIKE '", df.rs[,"Moneta"], "_Moneta'",
                    sep=""
                    )
            query <- paste(query1,query2,query3,query4,query5,query6,query7,query8,
                           query9,query10,query11,sep=";",collapse=";")
            result <- sqlCommand(channel=channel,query=query,
                                 errorText="Function preparazione_covarianze_settoriali:\n query")

            rm(query1,query2,query3,query4,query5,query6,query7,query8,query9,query10,query11)
          }
        rm(df.rs)

        # modifica il nome del fattore di rischio da quello della moneta di riferimento a CHF.
        query1 <- paste("UPDATE E_covarianze_settoriali SET Ticker1 = 'CHF_Moneta' ",
                        "WHERE Ticker1 LIKE '", moneta_riferimento, "_Moneta'",
                        sep=""
                        )

        query2 <- paste("UPDATE E_covarianze_settoriali SET Ticker2 = 'CHF_Moneta' ",
                        "WHERE Ticker2 LIKE '", moneta_riferimento, "_Moneta'",
                        sep=""
                        )
        result <- sqlCommand(channel=channel,query=paste(query1,";",query2),
                             errorText="Function preparazione_covarianze_settoriali:\n query")
        rm(query1,query2)

                                        # controlla che CHF_Moneta sia richiesto e se non è il caso eliminalo!
        query <- paste("SELECT COUNT(Moneta) AS nb FROM D_universo_markowitz ",
                       "WHERE Moneta = 'CHF' AND Cliente = '",client, "'",
                       sep=""
                       )
        nbRows <- select.result(channel=channel, query=query)

        if (nbRows==0)
          {
            query <- "DELETE FROM E_covarianze_settoriali WHERE Ticker1='CHF_Moneta' OR Ticker2='CHF_Moneta'"
            result <- sqlCommand(channel=channel,query=query,
                      errorText="Function preparazione_covarianze_settoriali:\n query")
          }
      }
  }

