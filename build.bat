rgbasm -o main.o main.asm
rgblink -o forth.gb -n gbt_test.sym main.o 
rgbfix -v -p 0xFF forth.gb
del *.o
pause
bgb.exe forth.gb
