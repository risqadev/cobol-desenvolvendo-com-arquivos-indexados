#!/bin/sh

rm ./crm.bin ./modules/*;
cobc -I ./src/copybooks/ -c ./src/CRMP-ALT.cbl -o ./modules/CRMP-ALT.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-CAD.cbl -o ./modules/CRMP-CAD.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-CON.cbl -o ./modules/CRMP-CON.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-EXC.cbl -o ./modules/CRMP-EXC.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-LAT.cbl -o ./modules/CRMP-LAT.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-LIN.cbl -o ./modules/CRMP-LIN.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-MAN.cbl -o ./modules/CRMP-MAN.o;
cobc -I ./src/copybooks/ -c ./src/CRMP-SIT.cbl -o ./modules/CRMP-SIT.o;
cobc -I ./src/copybooks/ -x ./src/CRMP-000.cbl ./modules/*.o -o ./crm.bin;
