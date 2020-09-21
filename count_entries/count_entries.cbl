       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-ENTRIES.
       DATE-WRITTEN. SEPTEMBER 21ST 2020.
       AUTHOR. KEVIN DE NOTARIIS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE1 ASSIGN TO "inputfile1.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-IN-FILE1.
           SELECT IN-FILE2 ASSIGN TO "inputfile2.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-IN-FILE2.
           SELECT IN-FILE3 ASSIGN TO "inputfile3.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-IN-FILE3.
           SELECT IN-FILE4 ASSIGN TO "inputfile4.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-IN-FILE4.

       DATA DIVISION.
       FILE SECTION.
           FD IN-FILE1.
               01 IN-FILE1-REC PIC X(50).
                   *>02 NAME-IN-FILE1    PIC A(20).
                   *>02 SURNAME-IN-FILE1 PIC A(20).
                   *>02 PHONE-IN-FILE1   PIC 9(10).

           FD IN-FILE2.
               01 IN-FILE2-REC PIC A(40).
                   *>02 COUNTRY-IN-FILE2 PIC A(20).
                   *>02 CAPITAL-IN-FILE2 PIC A(20).

           FD IN-FILE3.
               01 IN-FILE3-REC PIC X(41).
                   *>02 NAME-IN-FILE3            PIC X(20).
                   *>02 NUMB-ANIMALS-IN-FILE3    PIC 9.
                   *>02 PREFERRED-IN-FILE3       PIC X(20).
       
           FD IN-FILE4.
               01 IN-FILE3-REC PIC X(64).
                   *>02 SURNAME-IN-FILE4             PIC X(20).
                   *>02 INVENTION-IN-FILE4           PIC X(40).
                   *>02 YEAR-OF-DISCOVERY-IN-FILE4   PIC 9(4).

       WORKING-STORAGE SECTION.

           *> define the file entries.
           01 FE-IN-FILE1.
             02 FE-NAME-IN-FILE1    PIC A(20).
             02 FE-SURNAME-IN-FILE1 PIC A(20).
             02 FE-PHONE-IN-FILE1   PIC 9(10).

           01 FE-IN-FILE2.
             02 FE-COUNTRY-IN-FILE2 PIC A(20).
             02 FE-CAPITAL-IN-FILE2 PIC A(20).

           01 FE-IN-FILE3.
             02 FE-NAME-IN-FILE3            PIC X(20).
             02 FE-NUMB-ANIMALS-IN-FILE3    PIC 9.
             02 FE-PREFERRED-IN-FILE3       PIC X(20).
       
           01 FE-IN-FILE4.
             02 FE-SURNAME-IN-FILE4             PIC X(20).
             02 FE-INVENTION-IN-FILE4           PIC X(40).
             02 FE-YEAR-OF-DISCOVERY-IN-FILE4   PIC 9(4).

           *> define the file status flags
           01 FS-IN-FILE1 PIC 99.
           01 FS-IN-FILE2 PIC 99.
           01 FS-IN-FILE3 PIC 99.
           01 FS-IN-FILE4 PIC 99.
           
           *> define the counters
           01 COUNTER-IN-FILE1 PIC 9(4) VALUE ZERO.
           01 COUNTER-IN-FILE2 PIC 9(4) VALUE ZERO.
           01 COUNTER-IN-FILE3 PIC 9(4) VALUE ZERO.
           01 COUNTER-IN-FILE4 PIC 9(4) VALUE ZERO.

           *> define the end of the loop
           01 LOOP-EXIT-FLAG PIC 9 VALUE ZERO.
               88  EXIT-FLAG VALUE 1.

       PROCEDURE DIVISION.

           PERFORM 050-OPEN-INPUT-FILES.

           PERFORM 070-COUNT-ENTRIES-IN-FILES.

           PERFORM 300-CLOSE-INPUT-FILES.

           PERFORM 350-DISPLAY-RESULTS.

       STOP RUN.

       050-OPEN-INPUT-FILES.
           OPEN INPUT IN-FILE1
           IF FS-IN-FILE1 NOT EQUAL '00'THEN
               DISPLAY "SOMETHING WRONG WHEN OPENING FILE 1"
           END-IF
           
           OPEN INPUT IN-FILE2
           IF FS-IN-FILE2 NOT EQUAL '00'THEN
               DISPLAY "SOMETHING WRONG WHEN OPENING FILE 2"
           END-IF

           OPEN INPUT IN-FILE3
           IF FS-IN-FILE3 NOT EQUAL '00' THEN
               DISPLAY "SOMETHING WRONG WHEN OPENING FILE 3"
           END-IF


           OPEN INPUT IN-FILE4
           IF FS-IN-FILE4 NOT EQUAL '00'THEN
               DISPLAY "SOMETHING WRONG WHEN OPENING FILE 4"
           END-IF.


       070-COUNT-ENTRIES-IN-FILES.
           PERFORM 100-COUNT-ENTRIES-FILE1 UNTIL EXIT-FLAG
           MOVE ZERO TO LOOP-EXIT-FLAG

           PERFORM 150-COUNT-ENTRIES-FILE2 UNTIL EXIT-FLAG
           MOVE ZERO TO LOOP-EXIT-FLAG

           PERFORM 200-COUNT-ENTRIES-FILE3 UNTIL EXIT-FLAG
           MOVE ZERO TO LOOP-EXIT-FLAG

           PERFORM 250-COUNT-ENTRIES-FILE4 UNTIL EXIT-FLAG
           MOVE ZERO TO LOOP-EXIT-FLAG.

       100-COUNT-ENTRIES-FILE1.
           READ IN-FILE1 INTO FE-IN-FILE1
           
           IF FS-IN-FILE1 NOT EQUAL '00' AND '10' THEN
               DISPLAY "SOMETHING WENT WRONG WHEN READING FILE1"
               STOP RUN
           END-IF

           IF FS-IN-FILE1 EQUAL '00' THEN
               ADD 1 TO COUNTER-IN-FILE1
           END-IF

           IF FS-IN-FILE1 EQUAL '10' THEN
               SET EXIT-FLAG TO TRUE
           END-IF.

       150-COUNT-ENTRIES-FILE2.
           READ IN-FILE2 INTO FE-IN-FILE2
           
           IF FS-IN-FILE2 NOT EQUAL '00' AND '10' THEN
               DISPLAY "SOMETHING WENT WRONG WHEN READING FILE2"
               STOP RUN
           END-IF

           IF FS-IN-FILE2 EQUAL '00' THEN
               ADD 1 TO COUNTER-IN-FILE2
           END-IF

           IF FS-IN-FILE2 EQUAL '10' THEN
               SET EXIT-FLAG TO TRUE
           END-IF.

       200-COUNT-ENTRIES-FILE3.
           READ IN-FILE3 INTO FE-IN-FILE3
           
           IF FS-IN-FILE3 NOT EQUAL '00' AND '10' THEN
               DISPLAY "SOMETHING WENT WRONG WHEN READING FILE3"
               STOP RUN
           END-IF

           IF FS-IN-FILE3 EQUAL '00' THEN
               ADD 1 TO COUNTER-IN-FILE3
           END-IF

           IF FS-IN-FILE3 EQUAL '10' THEN
               SET EXIT-FLAG TO TRUE
           END-IF.

       250-COUNT-ENTRIES-FILE4.
           READ IN-FILE4 INTO FE-IN-FILE4
           
           IF FS-IN-FILE4 NOT EQUAL '00' AND '10' THEN
               DISPLAY "SOMETHING WENT WRONG WHEN READING FILE4"
               STOP RUN
           END-IF

           IF FS-IN-FILE4 EQUAL '00' THEN
               ADD 1 TO COUNTER-IN-FILE4
           END-IF

           IF FS-IN-FILE4 EQUAL '10' THEN
               SET EXIT-FLAG TO TRUE
           END-IF.

       300-CLOSE-INPUT-FILES.
           CLOSE IN-FILE1
           CLOSE IN-FILE2
           CLOSE IN-FILE3
           CLOSE IN-FILE4.

       350-DISPLAY-RESULTS.
           DISPLAY "Number of records in file 1: "
           DISPLAY COUNTER-IN-FILE1
           DISPLAY "Number of records in file 2: "
           DISPLAY COUNTER-IN-FILE2
           DISPLAY "Number of records in file 3: "
           DISPLAY COUNTER-IN-FILE3
           DISPLAY "Number of records in file 4: "
           DISPLAY COUNTER-IN-FILE4.
