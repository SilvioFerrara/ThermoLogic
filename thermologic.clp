;==============================
; Thermo Logic
;==============================

;;Autore: Silvio Ferrara - MAT. 609341


;Questo progetto `e stato ideato per ottenere una situazione confortevole all’interno di
;un ambiente domestico per raggiungere una temperatura mite e un’umidita' media (situazione
;ottimale), contribuendo ad un consumo congruo che si adatti al risparmio sulle
;spese per quanto riguarda la fornitura di energia elettrica, quindi un risparmio in bolletta.


;=========
;TEMPLATE
;========= 
;Template: La definizione dei template fornisce una struttura per i fatti che verranno utilizzati nel sistema. 
;In questo caso, il template "rule" è utilizzato per definire le regole del sistema.

(deftemplate rule 
   (multislot if)
   (multislot then))


;=========
;FUNZIONI
;========= 
;Funzioni: Vengono definite alcune funzioni utili per il calcolo e la gestione dei dati, come "umid" per valutare l'umidità, 
;"temper" per valutare la temperatura e "temp-avg" per calcolare la media tra temperatura interna ed esterna.

;il costrutto bind viene utilizzato per associare un valore a una variabile

;Umidita'
;La funzione umid riceve un parametro ?um, che rappresenta il livello di umidità. 
;In base al valore di ?um, determina se l'umidità è bassa, media o alta. 
;Se ?um è compreso tra 0 e 45, l'umidità viene considerata bassa. 
;Se è compreso tra 65 e 100, è considerata alta. 
;Altrimenti, se è compreso tra 45 e 65, è considerata media.

(deffunction umid (?um)
  (if (and (>= ?um 0) (< ?um 45)) then
    (bind ?umidity bassa)
   else
    (if (and (>= ?um 65) (<= ?um 100)) then
     (bind ?umidity alta)
     else
       (if (and (>= ?um 45) (< ?um 65)) then
        (bind ?umidity media)
)))
?umidity
)


;Temperatura
;La funzione temper riceve un parametro ?t, che rappresenta la temperatura. 
;In base al valore di ?t, determina se la temperatura è fredda, mite o calda. 
;Se ?t è inferiore a 15,la temperatura viene considerata fredda. 
;Se è superiore o uguale a 23, è considerata calda. 
;Altrimenti, se è compresa tra 15 e 23, è considerata mite.

(deffunction temper (?t)
  (if (< ?t 15) then
    (bind ?temperature fredda)
   else
    (if (>= ?t 23) then
    (bind ?temperature calda)
     else
       (if (and (>= ?t 15) (< ?t 23)) then
         (bind ?temperature mite)
)))
?temperature
)



;Differenza di temperatura interna ed esterna
;La funzione temp-avg riceve due parametri ?tempInt e ?tempEst, 
;che rappresentano rispettivamente la temperatura interna ed esterna. 
;Calcola la media tra le due temperature, utilizzando il valore assoluto della differenza tra la temperatura interna e esterna. 
;Restituisce la media calcolata.

(deffunction temp-avg (?tempInt ?tempEst)
  (bind ?avg (/ (+ ?tempInt (abs ?tempEst)) 2))
  ?avg
)


;=======
;REGOLE
;=======
;Regole: Le regole del sistema determinano il comportamento del termostato in base alle condizioni ambientali. 
;Le regole sono suddivise in diverse categorie, come l'inizio del sistema, la propagazione dei goal, 
;il soddisfacimento dei goal, la rimozione delle regole senza corrispondenza, la modifica delle regole, ecc.

;In CLIPS,la keyword salience è utilizzata in CLIPS per specificare la priorità relativa delle regole all'interno di un ambiente di produzione.
;In CLIPS, ?f è una variabile che viene utilizzata comunemente per fare riferimento ai fatti all'interno delle regole. Quando si utilizza il costrutto ?f all'interno di una regola, si sta effettivamente assegnando alla variabile ?f il fatto che soddisfa il modello specificato all'interno dei parentesi angolari.
;In CLIPS, $? è un costrutto che viene utilizzato per effettuare il pattern matching in modo flessibile all'interno di un modello. Viene utilizzato per fare riferimento a un numero variabile di elementi che possono comparire in un certo punto all'interno del modello.

;Regola di Inizio (system-banner-start): Questa regola viene eseguita all'avvio del sistema. Stampa un banner di benvenuto e chiede all'utente di inserire la data e l'ora nel formato specificato. Quindi, le informazioni inserite vengono memorizzate come fatti.

(defrule system-banner-start "Inizio"
  (declare (salience 10))
  =>
  ;(load-facts "facts.txt")
  (printout t crlf crlf)
  (printout t "TERMOSTATO INTELLIGENTE - SISTEMA ESPERTO - BASATO SU REGOLE")
  (printout t crlf crlf)
  (printout t "Immettere data nel seguente formato:  gg-mm-aa" crlf)
  (bind ?date (read))
  (printout t "Immettere orario nel seguente formato:  hh:mm" crlf)
  (bind ?time (read))
  (assert (date ?date))
  (assert (time ?time))
)



;Regole per la gestione dei goal (propagate-goal, goal-satisfied, remove-rule-no-match, modify-rule-match, rule-satisfied-1, rule-satisfied-2, ask-question-no-validsymbol, ask-question-validsymbol, ask-question-validnumber-umidity, ask-question-number-temperature): Queste regole gestiscono la propagazione e la soddisfazione dei goal, la modifica e la rimozione delle regole in base al successo o al fallimento delle azioni, e la presentazione di domande all'utente con la gestione delle risposte.

;Regola propagate-goal: Questa regola viene attivata ogni volta che un nuovo goal viene asserito nel sistema. La regola cerca regole associate al goal asserito e asserisce nuovi goal corrispondenti.
(defrule propagate-goal "Avanzamento programma una volta asserito goal"
   (goal is ?goal)
   (rule (if ?state $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?state))
)

;Regola goal-satisfied: Questa regola viene attivata quando un goal viene soddisfatto con successo. Essa cerca il goal soddisfatto e la risposta associata, quindi stampa la risposta sul terminale.
(defrule goal-satified "Azione avvenuta con successo"
   (declare (salience 30))
   ?f <- (goal is ?goal)
   (state ?goal ?value)
   (answer $? ?text ?goal)
   =>
   (retract ?f)
   (format t "%s%s%n" ?text ?value)
)

;Regola remove-rule-no-match: Questa regola rimuove le regole che non corrispondono ai fatti presenti nello stato del sistema.
(defrule remove-rule-no-match "Regola senza corrispondenza"
   (declare (salience 20))
   (state ?state ?value)
   ?f <- (rule (if ?state ? ~?value $?))
   =>
   (retract ?f)
)

;Regola modify-rule-match: Questa regola modifica le regole in base ai nuovi fatti presenti nello stato del sistema.
(defrule modify-rule-match "Modifica regola"
   (declare (salience 20))
   (state ?state ?value)
   ?f <- (rule (if ?state ? ?value and $?rest))
   =>
   (modify ?f (if ?rest))
)


;Regole rule-satisfied-1 e rule-satisfied-2: Queste regole vengono attivate quando una regola viene soddisfatta con successo, asserendo nuovi fatti nello stato del sistema.

(defrule rule-satisfied-1 "Regola accettata con singolo goal"
   (declare (salience 20))
   (state ?state ?value)
   ?f <- (rule (if ?state ? ?value)
               (then ?goal ? ?goal-value))
   =>
   (retract ?f)
   (assert (state ?goal ?goal-value))
)

(defrule rule-satisfied-2 "Regola accettata con due goal"
   (declare (salience 20))
   (state ?state ?value)
   ?f <- (rule (if ?state ? ?value)
               (then ?goal1 ? ?goal-value1 and ?goal2 ? ?goal-value2))
   =>
   (retract ?f)
   (assert (state ?goal1 ?goal-value1))
   (assert (state ?goal2 ?goal-value2))
)

;Regole ask-question-no-validsymbol, ask-question-validsymbol, ask-question-validnumber-umidity e ask-question-number-temperature: Queste regole gestiscono la richiesta di informazioni all'utente. Le prime due regole sono attivate quando non ci sono simboli validi per le risposte o quando ci sono simboli validi per le risposte. Le ultime due regole gestiscono domande specifiche sulla temperatura e sull'umidità.
(defrule ask-question-no-validsymbol "Regola senza simboli validi"
   (declare (salience 10))
   (not (validanswers $?))
   ?f1 <- (goal is ?state)
   ?f2 <- (question ?state ? ?text and validanswers ? $?answers where hints ? $?msg) 
   =>
   (retract ?f1 ?f2)
   (format t "%s " ?text)
   (assert (state ?state (read)))
)

;Regole ask-question-no-validsymbol, ask-question-validsymbol, ask-question-validnumber-umidity e ask-question-number-temperature: Queste regole gestiscono la richiesta di informazioni all'utente. Le prime due regole sono attivate quando non ci sono simboli validi per le risposte o quando ci sono simboli validi per le risposte. Le ultime due regole gestiscono domande specifiche sulla temperatura e sull'umidità.
(defrule ask-question-validsymbol "Regola con risposte valide con simboli"
   (declare (salience 10))
   ?f1 <- (goal is ?state)
   ?f2 <- (question ?state ? ?text and validanswers ? $?answers where hints ? $?msg)
   =>
   (retract ?f1) 
   (printout t crlf crlf)     
   (format t "%s " ?text)
   (printout t "LE RISPOSTE POSSIBILI SONO :: " ?answers " ")
   (printout t crlf crlf)
   (printout t "====================SPIEGAZIONE====================" crlf)
   (printout t ?msg crlf "")
   (printout t "===================================================" crlf)
   (printout t crlf crlf)
   
   (bind ?reply (read))
   (printout t crlf crlf)
   (if (member$ (lowcase ?reply) ?answers) 
     then (assert (state ?state ?reply))
          (retract ?f2)
     else (assert (goal is ?state)))
)


(defrule ask-question-validnumber-umidity "Regola con definizione umidita'"
   (declare (salience 10))
   ?f1 <- (goal is umidity)
   ?f2 <- (question umidity ? ?text where hints $?msg)
   =>
   (retract ?f1)
   (printout t crlf crlf)      
   (format t "%s " ?text)
   (printout t "IMMETTERE L'UMIDITA' PERCEPITA (in percentuale %) : " crlf ?msg " ")
   (printout t crlf crlf)
   
   (bind ?reply (read))
  (while (and (>= ?reply 1) (<= ?reply 100))
   (printout t crlf crlf)
    (assert (state umidity (umid ?reply)))
    (retract ?f2)
  (bind ?reply -50))
)

(defrule ask-question-number-temperature "Regola con definizione temperature"
   (declare (salience 10))
   ?f1 <- (goal is temperature)
   ?f2 <- (question temperature ? ?text where hints $?msg)
   => 
   (retract ?f1)      
   
   (bind ?i 0)
   (while (< ?i 3)
      (if (= ?i 0) then
       (printout t crlf crlf)
       (format t "%s " ?text)
       (printout t "IMMETTERE LA TEMPERATURA INTERNA (in centigradi ) : " crlf ?msg " ")
       (printout t crlf crlf)
       (bind ?reply1 (read))
       (assert (state tempInt ?reply1))
       (bind ?i (+ ?i 1))
      else
       (if (= ?i 1) then
        (printout t crlf crlf)
        (format t "%s " ?text)
        (printout t "IMMETTERE LA TEMPERATURA ESTERNA (in centigradi ) : " crlf ?msg " ")
        (printout t crlf crlf)
        (bind ?reply2 (read))
        (assert (state tempExt ?reply2))
        (bind ?avg (temp-avg ?reply1 ?reply2))
        (bind ?i (+ ?i 1))
        else
         (if (= ?i 2) then
          (assert (state temperature (temper ?avg)))
	  (bind ?i (+ ?i 1))
          (retract ?f2)
)))
)
)


;Regole di Fine (system-banner-end): Questa regola viene eseguita alla fine del processo e stampa un messaggio di chiusura del sistema.

(defrule system-banner-end "Fine"
  (declare (salience -10))
  =>
  (printout t crlf crlf)
  (printout t "TERMOSTATO INTELLIGENTE : risposte" crlf crlf)
  (printout t crlf crlf))


;Regole per liberare la memoria (free-mem-goal, free-mem-ans, free-mem-question): Queste regole sono utilizzate per liberare la memoria eliminando i fatti associati ai goal, alle risposte e alle domande una volta che non sono più necessari.
(defrule free-mem-goal "libera memoria-goal"
  (declare (salience -20))
  ?g <- (goal is ?goal)
  =>
  (retract ?g)
)

(defrule free-mem-ans "libera memoria-answers"
  (declare (salience -20))
  ?ans <- (answer is ?text ?goal) 
  =>
  (retract ?ans)
)

(defrule free-mem-question "libera memoria-questions"
  (declare (salience -20))
  ?q <- (question ?p is $?text) 
  =>
  (retract ?q)
)


;Regola per le risposte del sistema (system-answers): Questa regola gestisce le risposte del sistema, creando una lista di risposte e valori associati, che possono essere stampati o salvati in un file.
;da modificare
(defrule system-answers "Risposte del sistema"
  (declare (salience -15))
  (state ?goal ?value)
  (answer is ?text ?goal) 
  ?ans <- (answer is ?text ?goal)
  =>
  (retract ?ans)
  (if (not (bind ?a-list (create$))) then
   (bind ?a-list (create$))  
  else 
  (bind ?a-list (insert$ ?a-list 1 ?text ?value))
  (printout t "" ?a-list crlf)
  )
  ;(open "results.txt" newFile "a")
  ;(printout newFile ?a-list)
  ;(save-facts "facts.txt")
  ;(save "rules.txt")
  ;(close newFile)
)


;=========================
;STAMPA RISULTATI SU FILE
;=========================  
;Stampa risultati su file: Viene definita una regola per stampare i risultati su un file di log, 
;che include informazioni come data, ora, temperatura, umidità, stato del termostato e impostazioni.

(defrule print-file "Stampa i risultati su file"
  (declare (salience -30))
  ?data <- (date ?date)
  ?timer <- (time ?time)
  ?fc <- (state phase-time ?phase)
  ?temp <- (state temperature ?temperature)
  ?mh <- (state mode-heater ?mode-heater)
  ?mc <- (state mode-clima ?mode-clima)
  ?um <- (state umidity ?umidity)
  ?st <- (state status ?status)
  ?md <- (state mode-deum ?mode-deum)
  ;(exists (or (state mode-heater ?mode-heater)  (state mode-clima ?mode-clima)))
  =>
  (retract ?data ?timer ?fc ?temp ?mh ?mc ?um ?st ?md)
  (open "log.txt" ris "a")
  (printout ris "In Data: " ?date crlf)
  (printout ris "durante le " ?time " quindi nella " ?phase " oraria")
  (printout ris crlf)

  (printout ris crlf)
  (printout ris "La temperatura registrata e': " ?temperature)
  (printout ris crlf)

  (printout ris crlf)
  (printout ris "L'umidita' percepita: " ?umidity)
  (printout ris crlf)

  (printout ris crlf)
  (printout ris "Il termostato segna lo status : " ?status)
  (printout ris crlf)

  (printout ris crlf)
  (printout ris "Quindi il termostato ha impostato i seguenti: " crlf)
  (printout ris "Riscaldamenti: " ?mode-heater crlf)
  (printout ris "Climatizzatore: " ?mode-clima crlf)
  (printout ris "Deumidificatore: " ?mode-deum crlf)
  (printout ris crlf crlf)
  (printout ris "--------------------------------------------------------" crlf crlf) 
  (printout ris crlf)

  (close ris)

)

;Continuazione del programma: Questa regola gestisce la continuazione del programma, 
;consentendo all'utente di aggiornare i dati del termostato o uscire dal programma.

(defrule continue "Continuazione del programma"
  (declare (salience -30))
  =>
  (printout t crlf crlf)
  (printout t "Vuoi aggiornare i dati del termostato? (si oppure no)")
  (printout t crlf crlf)
  (bind ?answer (read))
  (if (eq ?answer si) then
    (reset)
    (run)
   else
    (exit));or (halt))
)


;===================
;CONOSCENZA DI BASE
;=================== 
;Conoscenza di base: Questa sezione contiene i fatti iniziali e le regole di base 
;che il sistema utilizzerà per prendere decisioni. 
;Include regole per definire la stagione in base al mese, il giorno della settimana, la fascia oraria, 
;la temperatura, l'umidità e le impostazioni ideali del termostato in base alla stagione e alla fase oraria.

(deffacts knowledge-base "Conoscenza di base, raccolta informazioni principali"

   ;Goal
   (goal is set-gradi)
   (goal is mode-deum)
   (goal is mode-heater)
   (goal is mode-clima)
   (goal is status)

   ;Regole per definire la stagione in base al mese

   (rule (if month is gen) 
         (then season is inverno))
   (rule (if month is feb) 
         (then season is inverno))
   (rule (if month is mar) 
         (then season is primavera))
   (rule (if month is apr) 
         (then season is primavera))
   (rule (if month is mag) 
         (then season is primavera))
   (rule (if month is giu) 
         (then season is estate))
   (rule (if month is lug) 
         (then season is estate))
   (rule (if month is ago) 
         (then season is estate))
   (rule (if month is set) 
         (then season is autunno))
   (rule (if month is ott) 
         (then season is autunno))
   (rule (if month is nov) 
         (then season is autunno))
   (rule (if month is dic) 
         (then season is inverno))
   
   (question month is "Mese dell'anno: " and validanswers are gen feb mar apr mag giu lug ago set ott nov dic
    where hints are gen sta per GENNAIO feb sta per FEBBRAIO e cosi' via)  

   ;Regole per definire giorno della settimana  
   
   (rule (if day is lun) 
         (then today is feriale))
   (rule (if day is mar) 
         (then today is feriale))
   (rule (if day is mer) 
         (then today is feriale))
   (rule (if day is gio) 
         (then today is feriale))
   (rule (if day is ven) 
         (then today is feriale))
   (rule (if day is sab) 
         (then today is fine-settimana))
   (rule (if day is dom) 
         (then today is fine-settimana))
   
   (question day is "Giorno della settimana: " and validanswers are lun mar mer gio ven sab dom
    where hints are lun sta per LUNEDI mar sta per MARTEDI e cosi' via)

   ;Regole per definire la fascia oraria

   (rule (if today is fine-settimana) 
         (then phase is no-durante-orario-lavorativo))   
   (rule (if today is feriale and phase-time is fascia-1) 
         (then phase is durante-orario-lavorativo))      
   (rule (if today is feriale and phase-time is fascia-2) 
         (then phase is no-durante-orario-lavorativo))
   (rule (if today is feriale and phase-time is fascia-3) 
         (then phase is no-durante-orario-lavorativo))
   
   (question phase-time is "Fascia oraria del giorno in cui ci troviamo? " and validanswers are fascia-1 fascia-2 fascia-3
    where hints are fascia-1 sta "TRA LE 8 E LE 19" , fascia-2 sta "TRA LE 19 E LE 23" e , fascia-3 sta "DOPO LE 23 FINO ALLE 8")

   ;Regole per definire la temperatura
   
   (rule (if temperature is calda)
         (then mode-heater is off and mode-clima is on))
   (rule (if temperature is mite)
         (then mode-heater is off and mode-clima is off))
   (rule (if temperature is fredda)
         (then mode-heater is on and mode-clima is off))
   
   (question temperature is "Qual e' la temperatura in questo momento?" where hints da -20 a 50)

   (answer is "Quindi imposta i riscaldamenti su " mode-heater)
   (answer is "Quindi imposta il climatizzatore su " mode-clima)

   ;Regole per definire umidità
   
   (rule (if umidity is alta)
         (then mode-deum is on))
   (rule (if umidity is media)
         (then mode-deum is off))
   (rule (if umidity is bassa)
         (then mode-deum is on))

   (question umidity is "Che umidita' si percepisce?" where hints da 1 a 100)
   
   (answer is "Quindi imposta il deumidificatore su " mode-deum)
  
  ;Regole per gli stati (combinazione di temperatureXumidity , totale di 9 differenti status, ordinati per temperatura)

  ;Temperatura calda
  (rule (if temperature is calda and umidity is alta)
 	(then status is afoso)) 

  (rule (if temperature is calda and umidity is media)
 	(then status is caloroso)) 

  (rule (if temperature is calda and umidity is bassa)
 	(then status is ardente))

  ;Temeperatura mite
  (rule (if temperature is mite and umidity is alta)
 	(then status is tiepido))

  ;Migliore situazione
  (rule (if temperature is mite and umidity is media)
 	(then status is confortevole))

  (rule (if temperature is mite and umidity is bassa)
 	(then status is asciutto))
  
  ;Temperatura fredda
  (rule (if temperature is fredda and umidity is alta)
 	(then status is gelido))

  (rule (if temperature is fredda and umidity is media)
 	(then status is fresco))

  (rule (if temperature is fredda and umidity is bassa)
 	(then status is freddo))

  (answer is "Con queste temperature e umidita' lo stato e' cambiato in " status)

   ;Regole per impostare temperature 
   
   ;Primaverili
   (rule (if season in primavera and phase is durante-orario-lavorativo)
         (then set-gradi is 18-gradi))
   (rule (if season in primavera and phase is no-durante-orario-lavorativo)
         (then set-gradi is 20-gradi))

   ;Estive
   (rule (if season in estate and phase is durante-orario-lavorativo)
         (then set-gradi is 22-gradi))
   (rule (if season in estate and phase is no-durante-orario-lavorativo)
         (then set-gradi is 18-gradi))

   ;Autunnali
   (rule (if season in autunno and phase is durante-orario-lavorativo)
         (then set-gradi is 20-gradi))
   (rule (if season in autunno and phase is no-durante-orario-lavorativo)
         (then set-gradi is 18-gradi))
   
   ;Invernali
   (rule (if season in inverno and phase is durante-orario-lavorativo)
         (then set-gradi is 18-gradi))
   (rule (if season in inverno and phase is no-durante-orario-lavorativo)
         (then set-gradi is 22-gradi))
 
   (answer is "La temperatura ideale per impostare il termostato e' " set-gradi)

)