#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "parser.h" 
#include "utils.h"
#include "ad.h"
#include "at.h"
#include "vm.h"
#include "gc.h"

int main() {
    char *inbuf = loadFile("tests/testgc.c");
    if (inbuf == NULL) {
        fprintf(stderr, "Failed to load file\n");
        return 1;
    }

    Token *tokens = tokenize(inbuf);
    if (tokens == NULL) {
        fprintf(stderr, "Failed to tokenize the input\n");
        free(inbuf);
        return 1;
    }

    //showTokens(tokens);

    pushDomain();
    vmInit();
    parse(tokens);
    ///Instr *testCode=genTestProgram(); 
    //run(testCode); 
    //showDomain(symTable,"global");

   // dropDomain();
    Symbol *symMain = findSymbolInDomain(symTable, "main");

    if (!symMain) {
        err("missing main function");
    }

    Instr *entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
    run(entryCode);
    printf("Parsing completed successfully.\n");



    return 0;
}
