## WHAT IS THIS?

This module allows you to write ELF
( [Executable and Linkable Format](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format) )
files with nothing but pure perl!  Combined with [CPU::x86_64::InstructionWriter]
(https://github.com/nrdvana/perl-CPU-x86_64-InstructionWriter) you can build your
own binaries without touching gcc or binutils or any other megalithic toolchain.

Implementing a compiler is left as an exercise for the reader.

## BUT WHY, DAMMIT? WHY?

If you have to ask why, you are not a member of the intended audience.
Please go on about your business and accept my apologies for this distraction.

## THAT'S CRAZY... BUT UM, WHERE CAN I LEARN ABOUT THIS?

Brian Raiter has a very nice writeup about [diving into the details of ELF]
(http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html) which I found
intriguing and educational, and refer back to any time I need to remember some
of this stuff.

Once you see what he's doing, you will understand the inspiration for this module.
