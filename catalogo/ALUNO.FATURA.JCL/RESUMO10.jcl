//RESUMO10 JOB (ACCT),'FATRESUM AULA 10',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* AJUSTE OS QUALIFICADORES CONFORME O SEU AMBIENTE
//*--------------------------------------------------------------------
//         SET HLQ=&SYSUID
//*--------------------------------------------------------------------
//* STEP010 - COMPILA E LINKEDITA O FATRESUM
//*--------------------------------------------------------------------
//STEP010  EXEC IGYWCL
//COBOL.SYSIN   DD DISP=SHR,DSN=&HLQ..FATURA.COBOL(FATRESUM)
//COBOL.SYSLIB  DD DISP=SHR,DSN=&HLQ..FATURA.COPY
//LKED.SYSLMOD  DD DISP=SHR,DSN=&HLQ..FATURA.LOAD(FATRESUM)
//*--------------------------------------------------------------------
//* STEP020 - EXECUTA O FATRESUM (SOMENTE SE A COMPILACAO FOI OK)
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=FATRESUM,
//             COND=((4,LT,STEP010.COBOL),(4,LT,STEP010.LKED))
//STEPLIB  DD DISP=SHR,DSN=&HLQ..FATURA.LOAD
//DIVERG   DD DISP=SHR,DSN=&HLQ..FATURA.DIVERG
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//