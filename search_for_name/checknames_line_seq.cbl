       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKNAMES_LINE_SEQ.
       DATE-WRITTEN. SEPTEMBER 21ST 2020.
       AUTHOR. KEVIN DE NOTARIIS.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT IN-FILE ASSIGN TO "inputfile_line_seq.txt" 
               FILE STATUS IS FS-IN-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT OUT-FILE ASSIGN TO "outputfile_line_seq".
           
       
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
           01 FEC-IN-FILE PIC X(40).
               
       FD OUT-FILE.
           01 OUT-FILE-STRING    PIC A(11).
       
       WORKING-STORAGE SECTION.
           *> we define here the file status of the input file.
           01 FS-IN-FILE   PIC 9(2).
           
           *> define the name we would like to search for in the file.
           01 NAME-TO-SRCHFOR PIC X(20) VALUE 'MARIA               '.
           
           *> define the variables in which we read from input.
           01 FILE-ENTRIES.
               02 FE-NAME      PIC X(20).
               02 FE-SURNAME   PIC X(20).
           
           *> define the condition to exit the loop.
           01 LOOP-EXIT-FLAG   PIC 9 VALUE ZERO.
               88  EXIT-FLAG  VALUE 1.
               
           *> define the strings to be written in the output file.
           01 OUTPUT-STRING PIC A(11).
           
       PROCEDURE DIVISION.
           
           PERFORM 050-OPEN-INPUT-FILE.
           
           PERFORM 100-SRCHFOR-NAME UNTIL EXIT-FLAG.
           
           CLOSE IN-FILE.
           
       STOP RUN.
       
       050-OPEN-INPUT-FILE.
           OPEN INPUT IN-FILE
               IF FS-IN-FILE NOT EQUAL '00' THEN
                   DISPLAY "SOMETHING WRONG WHEN TRYING TO OPEN IN-FILE"
                   STOP RUN
               END-IF.
       
       100-SRCHFOR-NAME.
           READ IN-FILE INTO FILE-ENTRIES.
           
           IF FS-IN-FILE NOT EQUAL'00' AND FS-IN-FILE NOT EQUAL'10' THEN
               DISPLAY "SOMETHING WRONG WHEN READING THE INPUT FILE"
               CLOSE IN-FILE
               STOP RUN
           END-IF.
           
           IF FS-IN-FILE EQUAL '00' AND 
             FE-NAME EQUAL NAME-TO-SRCHFOR THEN
               SET EXIT-FLAG TO TRUE
               MOVE 'trovato    ' TO OUTPUT-STRING
               PERFORM 150-WRITE-OUTPUT
           END-IF.
           
           IF FS-IN-FILE EQUAL '10' THEN
               SET EXIT-FLAG TO TRUE
               MOVE 'non trovato' TO OUTPUT-STRING
               PERFORM 150-WRITE-OUTPUT
           END-IF.
           
       *> open the output file, write it and then closes it.
       150-WRITE-OUTPUT.
           OPEN OUTPUT OUT-FILE.
               WRITE OUT-FILE-STRING FROM OUTPUT-STRING.    
           CLOSE OUT-FILE.