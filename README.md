

# Hack Virtual Machine to MiMa Translator
A program to translate the Hack Virtual Machine language to MiMa Assembly. The Hack VM, as described in the book The Elements of Computing Systems (TECS), is an intermediate stack based language which is generated by compiling the Java like language Jack. Written in Haskell.

## Background:
This program is part two of a project of mine, creating a fully functional CPU on an FPGA. To run some proper programs on the [FPGA MiMa](https://github.com/mkiesinger/mimaFPGA) I needed a compiler that can transform high level programs to machine code. I decided to use [TECS](https://www.nand2tetris.org/) as inspiration. In the book Nisan and Shocken explain how to compile a high level Java like language Jack first to an intermediate representation, a virtual stack based processor, and then translate that code into assembly. Instead of using the in the book described Hack CPU architecture, I decided to translate the VM to MiMa assembly, thus being able to have any program written in Jack run on the MiMa.
I chose MiMa over an own design, because there already exist good and fast simulators for the MiMa architecture written by students from the KIT, and it might help others to understand how the whole process works.

## Workflow:
- Download the [TECS Software Suite](https://www.nand2tetris.org/software)
- Write your own program in Jack or take any project from [GitHub](https://github.com/search?o=desc&q=nand2tetris&s=stars&type=Repositories)
- Compile the classes using the [TECS provided compiler](https://www.nand2tetris.org/software)  from *\nand2tetris\tools*
- Put the generated VM Files in a folder called `program`, then run the executable from the parent folder. You probably have to add the OS VM files from *\nand2tetris\tools\OS* as well
- `output.asm` then contains the MiMa Assembly
- Compile the assembly file to machine code using cbdevnet's [assembler](https://github.com/cbdevnet/mima):
`C:\> mimasm output.asm out.mima`
- Run `out.mima` using cbdevnet's [simulator](https://github.com/cbdevnet/mima):
`C:\> mimasim -v out.mima`
- Convert `out.mima` to a `.coe` file and run the code on my [FPGA MiMa implementation](https://github.com/mkiesinger/mimaFPGA)

## Examples
I have prepared some examples [here](https://github.com/mkiesinger/HackVM-to-MiMa/tree/master/examples).  In the program directory are the Jack sources and the compiled Hack VM files. `output.asm` contains the assembly for the MiMa and in the `out.simu` is the result after simulation.
- The fibonacci program calculates the 7nth fibonacci number and stores it in memory location 16384, which is the first word of the memory mapped screen. Simulation took a little under 6000 steps. 
- The quicksort program is a fast, in place implementation of quicksort that sorts the 20 elements in an array from location 16384. Simulation took about 17500 steps to complete.
