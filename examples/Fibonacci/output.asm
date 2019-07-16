* = 0
    JMP  START
* = 1
LCL: DS 0
ARG: DS 0
THIS: DS 0
THAT: DS 0
TEMP0: DS 0
TEMP1: DS 0
TEMP2: DS 0
TEMP3: DS 0
TEMP4: DS 0
TEMP5: DS 0
TEMP6: DS 0
TEMP7: DS 0
SP: DS 0
T0: DS 0
T1: DS 0
* = 16
* = 32768
START:    JMP  Setup
MONE: DS 65535
MONECF: DS -1
MFIVECF: DS -5
NFLAG: DS 32768
Setup:    LDC  256
    STV  SP
    JMP  Sys.init
Sys.halt:    AND  MONECF
Sys.halt.WHILE_EXP0:    LDC  0
    ADD  MONECF
    NOT
    JMN  Sys.halt.WHILE_END0
    JMP  Sys.halt.WHILE_EXP0
Sys.halt.WHILE_END0:    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
Sys.init:    LDC  Sys.Sys.init.ret.0
    STIV SP
    LDC  Main.main
    STV  T0
    LDC  4
    STV  T1
    JMP  CALL
Sys.Sys.init.ret.0:    LDV  MONECF
    ADD  SP
    STV  SP
    LDIV SP
    STV  TEMP0
    LDC  Sys.Sys.init.ret.2
    STIV SP
    LDC  Sys.halt
    STV  T0
    LDC  4
    STV  T1
    JMP  CALL
Sys.Sys.init.ret.2:    LDV  MONECF
    ADD  SP
    STV  SP
    LDIV SP
    STV  TEMP0
    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
Main.fibonacci:    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  1
    ADD  MONE
    NOT
    STV  T1
    LDC  0
    ADD  ARG
    STV  T0
    LDIV T0
    ADD  T1
    AND  NFLAG
    EQL  NFLAG
    AND  MONE
    ADD  MONECF
    NOT
    JMN  Main.fibonacci.IF_TRUE0
    JMP  Main.fibonacci.IF_FALSE0
Main.fibonacci.IF_TRUE0:    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
Main.fibonacci.IF_FALSE0:    LDC  0
    ADD  ARG
    STV  T0
    LDIV T0
    STV  T0
    LDC  1
    EQL  T0
    AND  MONE
    ADD  MONECF
    NOT
    JMN  Main.fibonacci.IF_TRUE1
    JMP  Main.fibonacci.IF_FALSE1
Main.fibonacci.IF_TRUE1:    LDC  1
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
Main.fibonacci.IF_FALSE1:    LDC  1
    ADD  MONE
    NOT
    STV  T1
    LDC  0
    ADD  ARG
    STV  T0
    LDIV T0
    ADD  T1
    AND  MONE
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  Main.Main.fibonacci.IF_FALSE1.ret.1
    STIV SP
    LDC  Main.fibonacci
    STV  T0
    LDC  5
    STV  T1
    JMP  CALL
Main.Main.fibonacci.IF_FALSE1.ret.1:    LDV  MONECF
    ADD  SP
    STV  SP
    LDC  0
    ADD  LCL
    STV  T0
    LDIV SP
    STIV T0
    LDC  2
    ADD  MONE
    NOT
    STV  T1
    LDC  0
    ADD  ARG
    STV  T0
    LDIV T0
    ADD  T1
    AND  MONE
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  Main.Main.fibonacci.IF_FALSE1.ret.4
    STIV SP
    LDC  Main.fibonacci
    STV  T0
    LDC  5
    STV  T1
    JMP  CALL
Main.Main.fibonacci.IF_FALSE1.ret.4:    LDV  MONECF
    ADD  SP
    STV  SP
    LDC  1
    ADD  LCL
    STV  T0
    LDIV SP
    STIV T0
    LDC  0
    ADD  LCL
    STV  T0
    LDIV T0
    STV  T0
    LDC  1
    ADD  LCL
    STV  T1
    LDIV T1
    ADD  T0
    AND  MONE
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
Main.main:    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  0
    STIV SP
    LDC  0
    ADD  LCL
    STV  T0
    LDIV SP
    STIV T0
    LDC  7
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  Main.Main.main.ret.3
    STIV SP
    LDC  Main.fibonacci
    STV  T0
    LDC  5
    STV  T1
    JMP  CALL
Main.Main.main.ret.3:    LDV  MONECF
    ADD  SP
    STV  SP
    LDC  1
    ADD  LCL
    STV  T0
    LDIV SP
    STIV T0
    LDC  16384
    STV  T0
    LDC  0
    ADD  LCL
    STV  T1
    LDIV T1
    ADD  T0
    AND  MONE
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDC  1
    ADD  LCL
    STV  T0
    LDIV T0
    STV  TEMP0
    LDV  MONECF
    ADD  SP
    STV  SP
    LDIV SP
    STV  THAT
    LDV  TEMP0
    STIV SP
    LDC  0
    ADD  THAT
    STV  T0
    LDIV SP
    STIV T0
    LDC  0
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    JMP  RETURN
CALL:    LDC  1
    ADD  SP
    STV  SP
    LDV  LCL
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDV  ARG
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDV  THIS
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDV  THAT
    STIV SP
    LDC  1
    ADD  SP
    STV  SP
    LDV  T1
    NOT
    ADD  SP
    STV  ARG
    LDV  SP
    STV  LCL
    JIND T0
RETURN:    LDV  LCL
    STV  T0
    ADD  MFIVECF
    STV  T1
    LDIV T1
    STV  T1
    LDV  MONECF
    ADD  SP
    STV  SP
    LDIV SP
    STIV ARG
    LDC  1
    ADD  ARG
    STV  SP
    LDV  MONECF
    ADD  T0
    STV  T0
    LDIV T0
    STV  THAT
    LDV  MONECF
    ADD  T0
    STV  T0
    LDIV T0
    STV  THIS
    LDV  MONECF
    ADD  T0
    STV  T0
    LDIV T0
    STV  ARG
    LDV  MONECF
    ADD  T0
    STV  T0
    LDIV T0
    STV  LCL
    JIND T1

  * = 16384     // added this part for easier lookup after simulation
  SCREEN: DS 0
