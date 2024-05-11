#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"
#include "utils.h"
#include "ad.h"
#include "lexer.h"

Token *iTk;        // the iterator in the tokens list
Token *consumedTk; // the last consumed token
Token *start;
Symbol *owner = NULL; // the current owner of the symbols

bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *);
bool arrayDecl(Type *);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool);
bool expr();
bool exprAssign();
bool exprOr();
bool exprOrPrim();
bool exprAnd();
bool exprAndPrim();
bool exprEq();
bool exprEqPrim();
bool exprRel();
bool exprRelPrim();
bool exprAdd();
bool exprAddPrim();
bool exprMul();
bool exprMulPrim();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();

void tkerr(const char *fmt, ...)
{
    fprintf(stderr, "error in line %d: ", iTk->line);
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

bool consume(int code)
{
    printf("consume(%s)", tkCodeName(code));
    if (iTk->code == code)
    {
        consumedTk = iTk;
        iTk = iTk->next;
        printf(" => consumed\n");
        return true;
    }
    printf(" => found %s\n", tkCodeName(iTk->code));
    return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit()
{
    printf("#unit: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    for (;;)
    {
        if (structDef())
        {
        }
        else if (fnDef())
        {
        }
        else if (varDef())
        {
        }
        else
            break;
    }
    if (consume(END))
    {
        return true;
    }
    iTk = start;
    return false;
}

// STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef()
{
    start = iTk;
    printf("E structDef?\n");
    if (consume(STRUCT))
    {
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            if (consume(LACC))
            {
                Symbol *s = findSymbolInDomain(symTable, tkName->text);
                if (s)
                    tkerr("symbol redefinition: %s", tkName->text);
                s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
                s->type.tb = TB_STRUCT;
                s->type.s = s;
                s->type.n = -1;
                pushDomain();
                owner = s;
                for (;;)
                {
                    if (varDef())
                    {
                    }
                    else
                        break;
                }
                if (consume(RACC))
                {
                    if (consume(SEMICOLON))
                    {
                        owner = NULL;
                        dropDomain();
                        return true;
                    }
                    else
                        tkerr("Lipseste ; dupa definirea structurii");
                }
                else
                    tkerr("Lipseste } din definirea structurii");
            }
        }
    }
    iTk = start;
    return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef()
{
    printf("varDef: %s\n", tkCodeName(iTk->code));
    Type t;
    Token *start = iTk;
    if (typeBase(&t))
    {
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            if (arrayDecl(&t))
            {
                if (t.n == 0)
                    tkerr("vectorul trebuie sa aiba o dimensiune specificata");
                if (consume(SEMICOLON))
                {
                    Symbol *var = findSymbolInDomain(symTable, tkName->text);
                    if (var)
                        tkerr("symbol redefinition: %s", tkName->text);
                    var = newSymbol(tkName->text, SK_VAR);
                    var->type = t;
                    var->owner = owner;
                    addSymbolToDomain(symTable, var);
                    if (owner)
                    {
                        switch (owner->kind)
                        {
                        case SK_FN:
                            var->varIdx = symbolsLen(owner->fn.locals);
                            addSymbolToList(&owner->fn.locals, dupSymbol(var));
                            break;
                        case SK_STRUCT:
                            var->varIdx = typeSize(&owner->type);
                            addSymbolToList(&owner->structMembers, dupSymbol(var));
                            break;
                        }
                    }
                    else
                    {
                        var->varMem = safeAlloc(typeSize(&t));
                    }
                    return true;
                }
                else
                    tkerr("Lipseste ; din declararea variabilei");
            }

            if (consume(SEMICOLON))
            {
                Symbol *var = findSymbolInDomain(symTable, tkName->text);
                if (var)
                    tkerr("symbol redefinition: %s", tkName->text);
                var = newSymbol(tkName->text, SK_VAR);
                var->type = t;
                var->owner = owner;
                addSymbolToDomain(symTable, var);
                if (owner)
                {
                    switch (owner->kind)
                    {
                    case SK_FN:
                        var->varIdx = symbolsLen(owner->fn.locals);
                        addSymbolToList(&owner->fn.locals, dupSymbol(var));
                        break;
                    case SK_STRUCT:
                        var->varIdx = typeSize(&owner->type);
                        addSymbolToList(&owner->structMembers, dupSymbol(var));
                        break;
                    }
                }
                else
                {
                    var->varMem = safeAlloc(typeSize(&t));
                }
                return true;
            }
            else
                tkerr("Lipseste ; din declararea variabilei");
        }
        else
            tkerr("Lipseste identificatorul din declararea variabilei");
    }
    iTk = start;
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t)
{
    t->n = -1;
    start = iTk;
    printf("typeBase?\n");
    if (consume(TYPE_INT))
    {
        t->tb = TB_INT;
        return true;
    }
    if (consume(TYPE_DOUBLE))
    {
        t->tb = TB_DOUBLE;
        return true;
    }
    if (consume(TYPE_CHAR))
    {
        t->tb = TB_CHAR;
        return true;
    }
    if (consume(STRUCT))
    {
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            t->tb = TB_STRUCT;
            t->s = findSymbol(tkName->text);
            if (!t->s)
                tkerr("structura nedefinita: %s", tkName->text);
            return true;
        }
        else
        {
            tkerr("Lipseste numele structurii");
        }
    }
    iTk = start;
    return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t)
{
    start = iTk;
    printf("arrayDecl?\n");
    if (consume(LBRACKET))
    {
        if (consume(INT))
        {
            Token *tkSize = consumedTk;
            t->n = tkSize->i;
        }
        else
        {
            t->n = 0;
        }
        if (consume(RBRACKET))
        {
            return true;
        }
        else
        {
            tkerr("Lipseste ] dupa [ ");
        }
    }
    iTk = start;
    return false;
}

// fnDef: ( typeBase | VOID ) ID
//               LPAR ( fnParam ( COMMA fnParam )* )? RPAR
//               stmCompound
bool fnDef()
{
    printf("#fnDef: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Type t;

    if (consume(VOID))
    {
        t.tb = TB_VOID;
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            if (consume(LPAR))
            {
                Symbol *fn = findSymbolInDomain(symTable, tkName->text);
                if (fn)
                    tkerr("symbol redefinition: %s", tkName->text);
                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();
                if (fnParam())
                {
                    for (;;)
                    {
                        if (consume(COMMA))
                        {
                            if (fnParam())
                            {
                            }
                            else
                            {
                                tkerr("Lipseste parametrul dupa , in definirea functiei");
                                break;
                            }
                        }
                        else
                            break;
                    }
                }
                if (consume(RPAR))
                {
                    if (stmCompound(false))
                    {
                        dropDomain();
                        owner = NULL;
                        return true;
                    }
                }
            }
            else
                tkerr("Lipseste ( in definirea functiei");
        }
        else
            tkerr("Lipseste identificatorul in definirea functiei");
    }
    else if (typeBase(&t))
    {
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            if (consume(LPAR))
            {
                Symbol *fn = findSymbolInDomain(symTable, tkName->text);
                if (fn)
                    tkerr("symbol redefinition: %s", tkName->text);
                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();
                if (fnParam())
                {
                    for (;;)
                    {
                        if (consume(COMMA))
                        {
                            if (fnParam())
                            {
                            }
                            else
                            {
                                tkerr("Lipseste parametrul dupa , in definirea functiei");
                                break;
                            }
                        }
                        else
                            break;
                    }
                }
                if (consume(RPAR))
                {
                    if (stmCompound(false))
                    {
                        dropDomain();
                        owner = NULL;
                        return true;
                    }
                }
                else
                    tkerr("Lipseste ) in definirea functiei");
            }
        }
        else
            tkerr("Lipseste identificatorul in definirea functiei");
    }
    iTk = start;
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam()
{
    printf("#fnParam: %s\n", tkCodeName(iTk->code));
    Type t;
    Token *start = iTk;
    if (typeBase(&t))
    {
        if (consume(ID))
        {
            Token *tkName = consumedTk;
            if (arrayDecl(&t))
            {
                t.n = 0;
                return true;
            }
            Symbol *param = findSymbolInDomain(symTable, tkName->text);
            if (param)
                tkerr("symbol redefinition: %s", tkName->text);
            param = newSymbol(tkName->text, SK_PARAM);
            param->type = t;
            param->owner = owner;
            param->paramIdx = symbolsLen(owner->fn.params);
            // parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToDomain(symTable, param);
            addSymbolToList(&owner->fn.params, dupSymbol(param));
            return true;
        }
        else
            tkerr("Lipseste identificatorul in parametrul functiei");
    }
    iTk = start;
    return false;
}

// stm: stmCompound
//      | IF LPAR expr RPAR stm ( ELSE stm )?
//      | WHILE LPAR expr RPAR stm
//      | RETURN expr? SEMICOLON
//      | expr? SEMICOLON
bool stm()
{
    printf("#stm: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (stmCompound(true))
    {
        return true;
    }
    // | IF LPAR expr RPAR stm ( ELSE stm )?
    if (consume(IF))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                if (consume(RPAR))
                {
                    if (stm())
                    {
                        if (consume(ELSE))
                        {
                            if (stm())
                            {
                                return true;
                            }
                            else
                                tkerr("Lipseste statement dupa conditia else");
                        }
                        return true;
                    }
                    else
                        tkerr("Lipseste statement dupa conditia if");
                }
                else
                    tkerr("Lipseste ) in if");
            }
            else
                tkerr("Lipseste expresia in if");
        }
        else
            tkerr("Lipseste ( in if");
    }
    // | WHILE LPAR expr RPAR stm
    if (consume(WHILE))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                if (consume(RPAR))
                {
                    if (stm())
                    {
                        return true;
                    }
                    else
                        tkerr("Lipseste statement in while");
                }
                else
                    tkerr("Lipseste ) in while");
            }
            else
                tkerr("Lipseste expresia in while");
        }
        else
            tkerr("Lipseste ( in while");
    }
    // | RETURN expr? SEMICOLON
    if (consume(RETURN))
    {
        if (expr())
        {
            if (consume(SEMICOLON))
            {
                return true;
            }
            else
                tkerr("Lipseste ; in return");
        }
        if (consume(SEMICOLON))
            return true;
    }
    // | expr? SEMICOLON
    if (expr())
    {
        if (consume(SEMICOLON))
        {
            return true;
        }
        else
            tkerr("Lipseste ; in expresie");
    }
    else if (consume(SEMICOLON))
        return true;

    iTk = start;
    return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain)
{
    printf("#stmCompound: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(LACC))
    {
        if (newDomain)
            pushDomain();
        for (;;)
        {
            if (varDef())
            {
            }
            else if (stm())
            {
            }
            else
                break;
        }
        if (consume(RACC))
        {
            if (newDomain)
                dropDomain();
            return true;
        }
        else
            tkerr("Lipseste } in compound statement");
    }
    iTk = start;
    return false;
}

// expr: exprAssign
bool expr()
{
    printf("#expr: %s\n", tkCodeName(iTk->code));
    if (exprAssign())
    {
        return true;
    }
    return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign()
{
    printf("#exprAssign: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprUnary())
    {
        if (consume(ASSIGN))
        {
            if (exprAssign())
            {
                return true;
            }
            else
                tkerr("Lipseste expresia dupa operatorul =");
        }
    }
    iTk = start;
    if (exprOr())
    {
        return true;
    }
    iTk = start;
    return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
bool exprOr()
{
    printf("#exprOr: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprAnd())
    {
        if (exprOrPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprOrPrim()
{
    printf("#exprOrPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(OR))
    {
        if (exprAnd())
        {
            if (exprOrPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul ||");
    }
    iTk = start;
    return true; // epsilon
}

// exprAnd: exprAnd AND exprEq | exprEq
// exprAnd -> exprEq exprAnd'
// exprAnd' -> AND exprEq exprAnd' | epsilon

bool exprAnd()
{
    printf("#exprAnd: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprEq())
    {
        if (exprAndPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprAndPrim()
{
    printf("#exprAndPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(AND))
    {
        if (exprEq())
        {
            if (exprAndPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul &&");
    }
    iTk = start;
    return true; // epsilon
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// alfa1 = ( EQUAL | NOTEQ ) exprRel     beta1 = exprRel
// exprEq = exprRel exprEqPrim
// exprEqPrim = ( EQUAL | NOTEQ ) exprRel exprEqPrim | epsilon
bool exprEq()
{
    printf("#exprEq: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprRel())
    {
        if (exprEqPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprEqPrim()
{
    printf("#exprEqPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(EQUAL))
    {
        if (exprRel())
        {
            if (exprEqPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul ==");
    }
    else if (consume(NOTEQ))
    {
        if (exprRel())
        {
            if (exprEqPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul !=");
    }
    iTk = start;
    return true; // epsilon
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// exprRel: exprAdd exprRelPrim
// exprRelPrim: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim | epsilon
bool exprRel()
{
    printf("#exprRel: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprAdd())
    {
        if (exprRelPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprRelPrim()
{
    printf("#exprRelPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    // merge oare asa? SAU trebuie if(consume(LESS)) else if(consume(LESSEQ) ) else if(consume(GREATER))...
    if (consume(LESS))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul <");
    }
    else if (consume(LESSEQ))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul <=");
    }
    else if (consume(GREATER))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul >");
    }
    else if (consume(GREATEREQ))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul >=");
    }
    iTk = start;
    return true; // epsilon
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// exprAdd: exprMul exprAddPrim
// exprAddPrim: (ADD | SUB) exprMul exprAddPrim | epsilon
bool exprAdd()
{
    printf("#exprAdd: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprMul())
    {
        if (exprAddPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprAddPrim()
{
    printf("#exprAddPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(ADD))
    {
        if (exprMul())
        {
            if (exprAddPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul +");
    }
    else if (consume(SUB))
    {
        if (exprMul())
        {
            if (exprAddPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul -");
    }
    iTk = start;
    return true; // epsilon
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast eliminam recursivitatea la stanga
// alfa1 = ( MUL | DIV ) exprCast    beta1 = exprCast
// exprMul = beta1 exprMulPrim   --->   exprMul = exprCast exprMulPrim
// exprMulPrim = alfa1 exprMulPrim | epsilon   --->   exprMulPrim = ( MUL | DIV ) exprCast exprMulPrim | epsilon
bool exprMul()
{
    printf("#exprMul: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprCast())
    {
        if (exprMulPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprMulPrim()
{
    printf("#exprMulPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(MUL))
    {
        if (exprCast())
        {
            if (exprMulPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul *");
    }
    else if (consume(DIV))
    {
        if (exprCast())
        {
            if (exprMulPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul /");
    }
    iTk = start;
    return true; // epsilon
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast()
{
    printf("#exprCast: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(LPAR))
    {
        Type t;
        if (typeBase(&t))
        {
            if (arrayDecl(&t))
            {
                if (consume(RPAR))
                {
                    if (exprCast())
                    {
                        return true;
                    }
                }
                else
                    tkerr("Lipseste ) in expresia de cast");
            }
            // arrayDecl? - tratare caz optionalitate
            if (consume(RPAR))
            {
                if (exprCast())
                {
                    return true;
                }
            }
        }
        else
            tkerr("Lipseste sau este gresit tipul din expresia de cast");
    }
    iTk = start;
    if (exprUnary())
    {
        return true;
    }
    iTk = start;
    return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary()
{
    printf("#exprUnary: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(SUB))
    {
        if (exprUnary())
        {
            return true;
        }
        else
            tkerr("Lipseste expresia dupa operatorul -");
    }
    else if (consume(NOT))
    {
        if (exprUnary())
        {
            return true;
        }
        else
            tkerr("Lipseste expresia dupa operatorul !");
    }
    iTk = start;
    if (exprPostfix())
    {
        return true;
    }
    iTk = start;
    return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//      | exprPostfix DOT ID
//      | exprPrimary
// alfa1 = LBRACKET expr RBRACKET    alfa2 = DOT ID   beta1 = exprPrimary
// exprPostfix = beta1 exprPostfixPrim   --->   exprPrimary exprPosfixPrim
// exprPostfixPrim = alfa1 exprPostfixPrim | alfa2 exprPostfixPrim | epsilon
// exprPostfixPrim   --->   LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | epsilon

bool exprPostfix()
{
    printf("#exprPostfix: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprPrimary())
    {
        if (exprPostfixPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprPostfixPrim()
{
    printf("#exprPostfixPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(LBRACKET))
    {
        if (expr())
        {
            if (consume(RBRACKET))
            {
                if (exprPostfixPrim())
                {
                    return true;
                }
            }
            else
                tkerr("Lipseste ] din accesarea vectorului");
        }
        else
            tkerr("Lipseste expresia din accesarea vectorului");
    }
    if (consume(DOT))
    {
        if (consume(ID))
        {
            if (exprPostfixPrim())
            {
                return true;
            }
        }
        else
            tkerr("Lipseste identificatorul dupa operatorul .");
    }
    iTk = start;
    return true;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//      | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary()
{
    printf("#exprPrimary %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(ID))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                for (;;)
                {
                    if (consume(COMMA))
                    {
                        if (expr())
                        {
                        }
                        else
                        {
                            tkerr("Lipseste expresia dupa , in apelul functiei");
                            break;
                        }
                    }
                    else
                        break;
                }
            }
            if (consume(RPAR))
            {
                return true;
            }
            else
                tkerr("Lipseste ) in apelul functiei");
        }
        return true;
    }
    if (consume(INT))
    {
        return true;
    }
    else if (consume(DOUBLE))
    {
        return true;
    }
    else if (consume(CHAR))
    {
        return true;
    }
    else if (consume(STRING))
    {
        return true;
    }
    if (consume(LPAR))
    {
        if (expr())
        {
            if (consume(RPAR))
            {
                return true;
            }
            else
                tkerr("Lipseste ) in apelul functiei");
        }
    }
    iTk = start;
    return false;
}

void parse(Token *tokens)
{
    iTk = tokens;
    if (!unit())
        tkerr("syntax error");
}