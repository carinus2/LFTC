#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "parser.h" 
#include "utils.h"
#include "ad.h"
#include "at.h"

int main() {
    char *inbuf = loadFile("tests/testat.c");
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

    showTokens(tokens);

    pushDomain();

    parse(tokens);
   
    showDomain(symTable,"global");

    dropDomain();

    printf("Parsing completed successfully.\n");



    return 0;
}
