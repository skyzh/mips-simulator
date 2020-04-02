# MIPS-Simulator

A functional MIPS CPU simulator implemented in Haskell.

A year ago, I implemented
[a RISC-V simulator in C++](https://github.com/skyzh/RISCV-Simulator).
I had long dreamed of using a functional programming language to express
the circuit of a CPU. But at that time, implementing lazy-evalution
in C++ is slow and painful. So the semester in Computer Architecture
course, I made this MIPS simulator.

## Roadmap
* Single-Cycle Processor
    - [x] R type
    - [x] I type
    - [x] ALU test
    - [ ] Support HI and LO
    - [ ] J type
    - [ ] Memory operations
* Multi-Cycle Processor
    - [ ] Data path
    - [ ] Stalling
    - [ ] Forwarding
    - [ ] Branch Prediction
* Dynamic Scheduling
    - [ ] Tomasulo
    - [ ] Speculation

## Reference

* https://uweb.engr.arizona.edu/~ece369/Resources/spim/MIPSReference.pdf
