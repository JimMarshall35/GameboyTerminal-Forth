rgbasm -o main.o main.asm
rgblink -o forth.gb -n gbt_test.sym main.o 
rgbfix -v -p 0xFF forth.gb
del *.o
pause
Start bgb.exe forth.gb -listen 127.0.0.1:5678
terminal.bat


