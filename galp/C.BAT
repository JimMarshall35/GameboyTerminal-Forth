rgbasm -oexample.obj example.asm
rgblink -mmap example.lnk
rgbfix -v example
del *.obj

