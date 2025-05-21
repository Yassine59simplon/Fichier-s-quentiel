      ******************************************************************
      *    Le programme ouvre le fichier pour lire les données ligne
      *    par ligne et garde toutes les informations dans un
      *    tableau en mémoire (jusqu'à 1000 élèves et 20 matières
      *    par élève).
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BULLTIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.txt'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

       DATA DIVISION.
         FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2            PIC 9(02).
       01  REC-F-INPUT-10           PIC X(10).
       01  REC-F-INPUT-100          PIC X(100).
       01  REC-F-INPUT-1000         PIC X(1000).

       01  REC-STUDENT.
           03 R-S-KEY               PIC 9(02).       
           03 R-LASTNAME            PIC X(07).       
           03 R-FIRSTNAME           PIC X(06).       
           03 R-AGE                 PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY               PIC 9(02).       
           03 R-LABEL               PIC X(21).       
           03 R-COEF                PIC 9,9.       
           03 R-GRADE               PIC 99,99.

      ******************************************************************        

       WORKING-STORAGE SECTION.
      * 
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK     VALUE '00'.        
           88 F-INPUT-STATUS-EOF    VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01 WS-FIN-FICHIER            PIC X VALUE 'N'.
       01 WS-CODE-LIGNE             PIC 9(02).
       01 WS-IDX-ELEVE              PIC 99 VALUE 0.
       01 WS-IDX-MATIERE            PIC 99 VALUE 0.

      *  Table des élèves et des matières
       01 TABLE-ELEVES.
          05 ELEVE OCCURS 1000 TIMES.
             10 WS-NOM-ELEVE        PIC X(10).
             10 WS-PRENOM-ELEVE     PIC X(10).
             10 WS-AGE-ELEVE        PIC 99.
             10 WS-NB-MATIERES      PIC 99.
             10 WS-LISTE-MATIERES OCCURS 20 TIMES.
                15 WS-NOM-MATIERE   PIC X(25).
                15 WS-COEFF         PIC 9,9.
                15 WS-NOTE          PIC 99,99.
      
      ******************************************************************
      ******************************************************************          

       PROCEDURE DIVISION.

      *    Lire le fichier bulltin.txt et remplir le TABLE-ELEVES
           PERFORM 6010-LIRE-FICHIER-DEB
              THRU 6010-LIRE-FICHIER-FIN.

      *    Afficher les élèves  matières avec les notes
           PERFORM 6020-AFFICHAGE-ELEVES-DEB
              THRU 6020-AFFICHAGE-ELEVES-FIN.

           STOP RUN.

      
      ******************************************************************
      ****************Lecture des lignes du fichier ********************
      ******************************************************************
       6010-LIRE-FICHIER-DEB.
      *    Ouvrir le fichier F-INPUT
       OPEN INPUT F-INPUT
          
       PERFORM UNTIL F-INPUT-STATUS-EOF
       READ F-INPUT
      *     AT END
      *         MOVE 'O' TO WS-FIN-FICHIER
           NOT AT END
           MOVE REC-F-INPUT-2 TO WS-CODE-LIGNE
           EVALUATE WS-CODE-LIGNE
      * Si la variable WS-CODE-LIGNE est 01 on enregistre 
      * le nom, prénome el l'âge     
            WHEN "01"
              ADD 1            TO WS-IDX-ELEVE
              MOVE R-LASTNAME  TO WS-NOM-ELEVE(WS-IDX-ELEVE)
              MOVE R-FIRSTNAME TO WS-PRENOM-ELEVE(WS-IDX-ELEVE)
              MOVE R-AGE       TO WS-AGE-ELEVE(WS-IDX-ELEVE)
              MOVE 0           TO WS-NB-MATIERES(WS-IDX-ELEVE)
              MOVE 0           TO WS-IDX-MATIERE
      * Si la variable WS-CODE-LIGNE est 02 on enregistre 
      * le Matiere et Note                
            WHEN "02"
              ADD 1 TO WS-IDX-MATIERE
             MOVE WS-IDX-MATIERE TO WS-NB-MATIERES(WS-IDX-ELEVE)

              MOVE R-LABEL 
              TO WS-NOM-MATIERE(WS-IDX-ELEVE, WS-IDX-MATIERE)

              MOVE R-COEF 
                TO WS-COEFF(WS-IDX-ELEVE, WS-IDX-MATIERE)

              MOVE R-GRADE 
                TO WS-NOTE(WS-IDX-ELEVE, WS-IDX-MATIERE)
           END-EVALUATE
           END-READ
       END-PERFORM.
      * Fermer le fichier
           CLOSE F-INPUT.
       6010-LIRE-FICHIER-FIN.
           EXIT.

      ******************************************************************
      ************Affichage des élèves et de leurs matières*************
      ******************************************************************
       6020-AFFICHAGE-ELEVES-DEB.
       DISPLAY " AFFICHAGE "
      * Le boucle pour Parcourir les élèves un par un
       PERFORM VARYING WS-IDX-ELEVE FROM 1 BY 1 UNTIL WS-IDX-ELEVE > 20
           IF WS-NOM-ELEVE(WS-IDX-ELEVE) NOT = SPACES
               DISPLAY  WS-NOM-ELEVE(WS-IDX-ELEVE)
                        WS-PRENOM-ELEVE(WS-IDX-ELEVE)
                        WS-AGE-ELEVE(WS-IDX-ELEVE)
      * le boucle pour parcourir les matiéres          
               PERFORM VARYING WS-IDX-MATIERE FROM 1 BY 1 
               UNTIL WS-IDX-MATIERE > WS-NB-MATIERES(WS-IDX-ELEVE)
                   DISPLAY  WS-NOM-MATIERE(WS-IDX-ELEVE, WS-IDX-MATIERE)       
                            WS-COEFF(WS-IDX-ELEVE, WS-IDX-MATIERE)
                            WS-NOTE(WS-IDX-ELEVE, WS-IDX-MATIERE)
               END-PERFORM
               DISPLAY "......................."
               
           END-IF
       END-PERFORM.

       6020-AFFICHAGE-ELEVES-FIN.
           EXIT.
