# MIPS-Simulator

![build](https://github.com/skyzh/mips-simulator/workflows/build/badge.svg)

A functional MIPS CPU simulator implemented in Haskell.

A year ago, I implemented
[a RISC-V simulator in C++](https://github.com/skyzh/RISCV-Simulator).
I had long dreamed of using a functional programming language to express
the circuit of a CPU. But at that time, implementing lazy-evalution
in C++ is slow and painful. So the semester in Computer Architecture
course, I made this MIPS simulator.

As functional programming language works well in expressing this
hardware circuit logic, I'm implementing [A MIPS CPU](https://github.com/skyzh/mips-cpu),
just by directly translating Haskell into Verilog.

## Usage

Currently I haven't implemented a command-line interface for interacting with
this simulator. You can only run tests or hack it yourself for now.

Install [Stack](https://docs.haskellstack.org/en/stable/README/) and simply run:

```bash
stack test
```

## Roadmap
* Single-Cycle Processor
    - [x] R type
    - [x] I type
    - [x] ALU test
    - [x] Compare and shift
    - [ ] Support HI and LO, multiply and division
    - [x] Branch instruction
    - [x] jump and link
    - [x] J type
    - [x] Memory operations
    - [ ] unsigned / high addr memory operations
* Multi-Cycle Processor
    - [ ] Data path
    - [ ] Stalling
    - [ ] Forwarding
    - [ ] Branch Prediction
* Dynamic Scheduling
    - [ ] Tomasulo
    - [ ] Speculation

## Circuit Design

I'm still working on this. I've completed IF and decode stage.

![image](https://user-images.githubusercontent.com/4198311/78424395-c9753200-769f-11ea-966b-bf0304bdd5e2.png)

## Reference

* https://uweb.engr.arizona.edu/~ece369/Resources/spim/MIPSReference.pdf (some opcode is wrong in this pdf)
* http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html
