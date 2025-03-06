.entry
    !_start
.program
    !_start
    movim r1,$1
    movim r2,$2
    push r1
    push r2
    // stack 2 1
    movim r3,$8
    add sp,sp,r3 // move stack pointer back one position
    pop r4
    mult r3,r3,r2 // 16
    sub sp,sp,r3
    pop r5
    // should do 1 2
