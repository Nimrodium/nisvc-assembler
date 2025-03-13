.program
    // push start address
    // then length
    // then call
    !std_write
    // push r1 and r2 to stack
    //
    push r1
    push r2
    push r3
    push r4
    push r5
    push r6
    push
    push r8
    push r9
    mov r9,sp // cache stack pointer
    movim r4,$8
    movim r3,$10
    mult r3,r4,r3 // 8*9 == r1-r8 offset + return address
    add sp,sp,r3 // move stack pointer back 9 positions

    pop r2 // string pointer
    pop r1 // string length

    movim r3,$5 // write
    movim r4,$4 // cursor

    add r7,r1,r2 // last address

    !_std_write_loop
        movim r5,$1 // one
        xor r6,r6,r6 // zero
        load r8,r5,r6 // read from event pump so window stays responsive

        load r6,r5,r1 // load char from string pointer
        store r3,r5,r6 // write char to display
        store r4,r5,r5 // move cursor right

        inc r1
        sub r6,r7,r1 // r6 is chars left
        jifnz r6,$!_std_write_loop
    movim r1,$2
    store r5,r5,r1 // refresh display
    // movim r1,$88 // offset traversed

    //add sp,sp,r1 // move sp back to top of stack
    mov sp,r9 // restore stack pointer
    pop r9
    pop r8
    pop r7
    pop r6
    pop r5
    pop r4
    pop r3
    pop r2
    pop r1
    ret

    // 0 + args(8*2) + retaddr(8)
