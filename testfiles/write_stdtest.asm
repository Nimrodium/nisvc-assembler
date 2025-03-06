.entry
    !start
.data
    #//hello_world str "hello world from nisvc",$x0a,"now with stdlib!"
    hello_world str "hello world from nisvc",$xa,"now with stack and subroutines!"
    len equ . - $!hello_world
.program
    !start
    movim r15,@!hello_world
    movim r14,$!len

    movim r1,$1
    store r1,r1,r1 #// show display
    add r2,r1,r1
    store r1,r1,r2 #// refresh display

    push r15
    push r14
    call $!std_write
    #// store r1,r1,r2 // refresh display
    #//movim r2,$0
    xor r2,r2,r2
    movim r1,$1
    !keep_alive
        load r15,r1,r2 #// read event pump (just so the window stays responsive)
        jmp $!keep_alive
