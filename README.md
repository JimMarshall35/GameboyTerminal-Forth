- Emulated keyboard peripheral for gameboy, using BGB's serial port emulation
- C# program connects to it and acts like it's a keyboard peripheral
- Terminal with getc and putc functions
- Need to re-write the actual "Forth" aspect of it

This is the start of a project that could be a version of forth running the gameboy - at the moment its an incredibly buggy and bad version.
The keyboard peripheral, I assume could be made into real hardware. I plan for the assembly code to be used to make a gameboy + forth IDE. You could write assembly code or a mixture of assembly and forth on your pc and have it create a rom that includes the forth kernel - which also includes some debugging capability over serial connection which could work either with an emulated gameboy or a real one - something like this is the ultimate goal. At the moment its a proof of concept which needs serious work - the forth interpreter works on some level - other implementations I've seen are programmed differently and no doubt better with far less "push" and "pop" instructions, they reserve certain registers for certain forth VM variables
