module CodeWriter where
import Translator
import Parser

write :: [MimaInstruction] -> String
write [] = error "No instructions to write!"
write is = concatMap writeInstr is

writeInstr :: MimaInstruction -> String
writeInstr i | ILdc val <- i          = "    LDC  " ++ show val ++ nl
             | IXLdc symb <- i        = "    LDC  " ++ symb ++ nl
             | ILdv val <- i          = "    LDV  " ++ show val ++ nl
             | IXLdv symb <- i        = "    LDV  " ++ symb ++ nl
             | IStv val <- i          = "    STV  " ++ show val ++ nl
             | IXStv symb <- i        = "    STV  " ++ symb ++ nl
             | IAdd val <- i          = "    ADD  " ++ show val ++ nl
             | IXAdd symb <- i        = "    ADD  " ++ symb ++ nl
             | IAnd val <- i          = "    AND  " ++ show val ++ nl
             | IXAnd symb <- i        = "    AND  " ++ symb ++ nl
             | IOr val <- i           = "    OR   " ++ show val ++ nl
             | IXOr symb <- i         = "    OR   " ++ symb ++ nl
             | IXor val <- i          = "    XOR  " ++ show val ++ nl
             | IEql val <- i          = "    EQL  " ++ show val ++ nl
             | IXEql symb <- i        = "    EQL  " ++ symb ++ nl
             | IJmp val <- i          = "    JMP  " ++ show val ++ nl
             | IXJmp symb <- i        = "    JMP  " ++ symb ++ nl
             | IJmn val <- i          = "    JMN  " ++ show val ++ nl
             | IXJmn symb <- i        = "    JMN  " ++ symb ++ nl
             | ILdiv val <- i         = "    LDIV " ++ show val ++ nl
             | IStiv val <- i         = "    STIV " ++ show val ++ nl
             | IXLdiv symb <- i       = "    LDIV " ++ symb ++ nl
             | IXStiv symb <- i       = "    STIV " ++ symb ++ nl
             | IJms val <- i          = "    JMS  " ++ show val ++ nl
             | IJind val <- i         = "    JIND " ++ show val ++ nl
             | IXJind symb <- i       = "    JIND " ++ symb ++ nl
             | IHalt <- i             = "    HALT" ++ nl
             | INot <- i              = "    NOT" ++ nl
             | IRar <- i              = "    RAR" ++ nl
             | IXLds symb <- i        = "    LDV  " ++ symb ++ nl
             | IXSts symb <- i        = "    STV  " ++ symb ++ nl
             | IXStatic symb val <- i = symb ++ ": DS " ++ show val ++ nl
             | IXSegment val <- i     = "* = " ++ show val ++ nl
             | IXLabel symb <- i      = symb ++ ":"
             | IXComment symb <- i    = " ;" ++ symb ++ nl
  where nl = "\n"
