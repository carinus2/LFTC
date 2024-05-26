#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"
#include "utils.h"
#include "ad.h"
#include "lexer.h"
#include "at.h"
#include <string.h>
#include "vm.h"
#include "gc.h"

Token *iTk;        // the iterator in the tokens list
Token *consumedTk; // the last consumed token
Token *start;
Symbol *owner; // the current owner of the symbols

bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *);
bool arrayDecl(Type *);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool);
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);

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
                while(1)
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
    start = iTk;
    t->n = -1;
   
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
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    
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
                    addInstr(&fn->fn.instr, OP_ENTER);
                    if (stmCompound(false))
                    {
                        fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
                        if (fn->type.tb == TB_VOID)
                        {
                            addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
                        }
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
                    addInstr(&fn->fn.instr, OP_ENTER);
                    if (stmCompound(false))
                    {
                        fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
                        if (fn->type.tb == TB_VOID)
                        {
                            addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
                        }
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
    if (owner) {
        delInstrAfter(startInstr);
    }
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() { 
	Type t;
	if(typeBase(&t))
	{
		if(consume(ID))
		{
			Token *tkName = consumedTk;

			if(arrayDecl(&t))
			{
				t.n = 0;
			}

			Symbol *param = findSymbolInDomain(symTable, tkName->text);

            if (param) 
			{
                tkerr("Symbol is already defined: %s", tkName->text); //numele parametrului trebuie sa fie unic in domeniu
            }

            param = newSymbol(tkName->text, SK_PARAM);
            param->type = t;
            param->owner = owner;
            param->paramIdx = symbolsLen(owner->fn.params); //parametrii pot fi vectori cu dimensiune data, dar in acest caz li se sterge dimensiunea ( int v[10] -> int v[] )
            addSymbolToDomain(symTable, param); //parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToList(&owner->fn.params, dupSymbol(param));

			return true;
		}
		else 
		{
			tkerr("Missing variable name\n");
		}
	}
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
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    
    Ret rCond, rExpr;
    if (stmCompound(true))
    {
        return true;
    }
    // | IF LPAR expr RPAR stm ( ELSE stm )?
    if (consume(IF))
    {
        if (consume(LPAR))
        {
            if (expr(&rCond))
            {
                if (!canBeScalar(&rCond)) 
				{
                    tkerr("The if condition must be a scalar value");
                }

                if (consume(RPAR))
                {
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *ifJF = addInstr(&owner->fn.instr, OP_JF);

                    if (stm())
                    {
                        if (consume(ELSE))
                        {
                            Instr *ifJMP = addInstr(&owner->fn.instr, OP_JMP);
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);

                            if (stm())
                            {
                                ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                            }
                            else
                                tkerr("Lipseste statement dupa conditia else");
                        } 
                        else 
                        {
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
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
        Instr *beforeWhileCond = lastInstr(owner->fn.instr);

        if (consume(LPAR))
        {
            if (expr(&rCond))
            {
                if(!canBeScalar(&rCond))
					tkerr("the while condition must be a scalar value");
                if (consume(RPAR))
                {
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *whileJF = addInstr(&owner->fn.instr, OP_JF);

                    if (stm())
                    {
                        addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
                        whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
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
        if (expr(&rExpr))
        {
            if (owner->type.tb == TB_VOID) 
			{
				tkerr("a void function cannot return a value");
			}

			if (!canBeScalar(&rExpr)) 
			{
				tkerr("the return value must be a scalar value");
			}

			if (!convTo(&rExpr.type, &owner->type)) 
			{
				tkerr("cannot convert the return expression type to the function return type");
			}

			addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
            addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
        } 
		else 
		{
			if(owner->type.tb != TB_VOID) {
				tkerr("a non-void function must return a value");
			}
            addInstr(&owner->fn.instr, OP_RET_VOID);
        }
        if (consume(SEMICOLON))
        {
            return true;
        }
        else
            tkerr("Lipseste ; in return");
    }
    // | expr? SEMICOLON
    if (expr(&rExpr))
    {
        if (rExpr.type.tb != TB_VOID) 
        {
            addInstr(&owner->fn.instr, OP_DROP);
        }
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
    if (owner) 
    {
        delInstrAfter(startInstr);
    }
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
bool expr(Ret *r)
{
    printf("#expr: %s\n", tkCodeName(iTk->code));
    if (exprAssign(r))
    {
        return true;
    }
    return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret *r)
{
    printf("#exprAssign: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    Ret rDst;
    if (exprUnary(&rDst))
    {
        if (consume(ASSIGN))
        {
            if (exprAssign(r))
            {
                if (!rDst.lval) 
				{
					tkerr("the assign destination must be a left-value");
				}

				if (rDst.ct) 
				{
					tkerr("the assign destination cannot be constant");
				}

				if (!canBeScalar(&rDst)) 
				{
					tkerr("the assign destination must be scalar");
				}

				if (!canBeScalar(r)) 
				{
					tkerr("the assign source must be scalar");
				}

				if (!convTo(&r->type,&rDst.type)) 
				{
					tkerr("the assign source cannot be converted to destination");
				}

				r->lval = false;
				r->ct = true;

				addRVal(&owner->fn.instr, r->lval, &r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
                switch (rDst.type.tb) 
                {
                    case TB_INT:
                        addInstr(&owner->fn.instr, OP_STORE_I);
                        break;
                    case TB_DOUBLE:
                        addInstr(&owner->fn.instr, OP_STORE_F);
                        break;
                }

                return true;
            }
            else
                tkerr("Lipseste expresia dupa operatorul =");
        }
    }
    iTk = start;
    if (owner) 
    {
        delInstrAfter(startInstr);
    }
    if (exprOr(r)) 
    {
        return true;
    }

    iTk = start;
    if (owner) 
    {
        delInstrAfter(startInstr);
    }
    return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
bool exprOr(Ret *r)
{
    printf("#exprOr: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprAnd(r))
    {
        if (exprOrPrim(r))
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprOrPrim(Ret *r)
{
    printf("#exprOrPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Ret right;
    if (consume(OR))
    {
        if (exprAnd(&right))
        {
            Type tDst;

			if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for ||");
			}

			*r = (Ret){{TB_INT,NULL,-1},false,true};


            if (exprOrPrim(r))
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

bool exprAnd(Ret *r)
{
    printf("#exprAnd: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprEq(r))
    {
        if (exprAndPrim(r))
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprAndPrim(Ret *r)
{
    printf("#exprAndPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Ret right;
    if (consume(AND))
    {
        if (exprEq(&right))
        {
            Type tDst;

			if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for &&"); 
			}

			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprAndPrim(r))
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
bool exprEq(Ret *r)
{
    printf("#exprEq: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprRel(r))
    {
        if (exprEqPrim(r))
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprEqPrim(Ret *r)
{
    printf("#exprEqPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(EQUAL) || consume(NOTEQ))
    {
        Ret right;
        if (exprRel(&right))
        {
            Type tDst;

            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for == or !="); 
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprEqPrim(r))
            {
                return true;
            }
        }
    }
    iTk = start;
    return true; 
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// exprRel: exprAdd exprRelPrim
// exprRelPrim: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim | epsilon
bool exprRel(Ret *r)
{
    printf("#exprRel: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

    if (exprAdd(r))
    {
        if (exprRelPrim(r))
        {
            return true;
        }
    }

    iTk = start;
    if (owner) 
    {
        delInstrAfter(startInstr);
    }
    return false;
}

bool exprRelPrim(Ret *r){
	Token *op;
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) ||consume(GREATEREQ)) 
    {
        Ret right;

        op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprAdd(&right)) 
        {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
            {
                tkerr("Invalid operand type for <, <=, >,>=");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) 
            {
                case LESS:
                    switch (tDst.tb) 
                    {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                    }
                    break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};

            if (exprRelPrim(r)) 
            {
                return true;
            }
        } 
        else 
        {
            tkerr("Invalid expression after comparison");
        }
    }

    return true;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// exprAdd: exprMul exprAddPrim
// exprAddPrim: (ADD | SUB) exprMul exprAddPrim | epsilon
bool exprAdd(Ret *r)
{
    printf("#exprAdd: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

    if (exprMul(r)) 
    {
        if (exprAddPrim(r)) 
        {
            return true;
        }
    }

    iTk = start;
    if (owner) 
    {
        delInstrAfter(startInstr);
    }
    return false;
}

bool exprAddPrim(Ret *r)
{
    printf("#exprAddPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(ADD) || consume(SUB))
    {
        Ret right;
        Token *op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprMul(&right))
        {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
            {
                tkerr("Invalid operand type for + or -");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) 
            {
                case ADD:
                    switch (tDst.tb) 
                    {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_ADD_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_ADD_F);
                            break;
                    }
                    break;
                case SUB:
                    switch (tDst.tb) 
                    {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_SUB_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_SUB_F);
                            break;
                    }
                    break;
            }

            *r = (Ret){tDst, false, true};

            if (exprAddPrim(r))
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul aditiv");
    }
    iTk = start;
    return true; // epsilon
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast eliminam recursivitatea la stanga
// alfa1 = ( MUL | DIV ) exprCast    beta1 = exprCast
// exprMul = beta1 exprMulPrim   --->   exprMul = exprCast exprMulPrim
// exprMulPrim = alfa1 exprMulPrim | epsilon   --->   exprMulPrim = ( MUL | DIV ) exprCast exprMulPrim | epsilon
bool exprMul(Ret *r)
{
    printf("#exprMul: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

    if (exprCast(r)) 
    {
        if (exprMulPrim(r)) 
        {
            return true;
        }
    }

    iTk = start;

    if (owner) 
    {
        delInstrAfter(startInstr);
    }
    return false;
}

bool exprMulPrim(Ret *r)
{
    printf("#exprMulPrim: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (consume(MUL) || consume(DIV))
    {
        Ret right;
        Token *op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprCast(&right))
        {
            Type tDst;

            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
            {
                tkerr("Invalid operand type for * or /");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) 
            {
                case MUL:
                    switch (tDst.tb) 
                    {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb) 
                    {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                    }
                    break;
            }

            *r = (Ret){tDst, false, true};

            if (exprMulPrim(r))
            {
                return true;
            }
        }
        else
            tkerr("Lipseste expresia dupa operatorul multiplicativ");
    }
    iTk = start;
    return true; // epsilon
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret *r) {

	Token* start=iTk;

	if(consume(LPAR))
	{
		Type t;
        Ret op;

		if(typeBase(&t))
		{
			if(arrayDecl(&t))
			{
			
			}
			if(consume(RPAR))
			{
			
				if(exprCast(&op))
				{	
					if (t.tb == TB_STRUCT) 
					{
						tkerr("cannot convert to a struct type"); 
					}

					if (op.type.tb == TB_STRUCT) 
					{
						tkerr("cannot convert a struct");
					} 

					if (op.type.n >= 0 && t.n < 0) 
					{
						tkerr("an array can be converted only to another array"); 
					}

					if (op.type.n < 0 && t.n >= 0) 
					{
						tkerr("a scalar can be converted only to another scalar"); 
					}
					*r = (Ret){t,false,true};

					addRVal(&owner->fn.instr, op.lval, &op.type);
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &op.type, &t);

					return true;
				}
			}
		}
		iTk=start;
	}
	if(exprUnary(r))
	{
		return true;
	}
	iTk=start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(Ret *r) { //--a
	Token* start=iTk;

	if(consume(SUB) || consume(NOT))
	{
		if(exprUnary(r))
		{
			if (!canBeScalar(r)) 
			{
				tkerr("unary - or ! must have a scalar operand");
			}
			r->lval = false;
			r->ct = true;

			return true;
		}
		iTk=start;
	}
	if(exprPostfix(r))
	{
		return true;
	}

	iTk=start;
	return false;
}


// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//      | exprPostfix DOT ID
//      | exprPrimary
// alfa1 = LBRACKET expr RBRACKET    alfa2 = DOT ID   beta1 = exprPrimary
// exprPostfix = beta1 exprPostfixPrim   --->   exprPrimary exprPosfixPrim
// exprPostfixPrim = alfa1 exprPostfixPrim | alfa2 exprPostfixPrim | epsilon
// exprPostfixPrim   --->   LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | epsilon

bool exprPostfix(Ret *r)
{
    printf("#exprPostfix: %s\n", tkCodeName(iTk->code));
    Token *start = iTk;
    if (exprPrimary(r))
    {
        if (exprPostfixPrim(r))
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprPostfixPrim(Ret *r){
	Token* start=iTk;

	if(consume(LBRACKET))
	{
		Ret idx;

		if(expr(&idx))
		{
			if(consume(RBRACKET))
			{
				if (r->type.n < 0) 
				{
					tkerr("only an array can be indexed");
				}

				Type tInt = {TB_INT,NULL,-1};

                if (!convTo(&idx.type, &tInt)) 
				{
					tkerr("the index is not convertible to int");
				}

				r->type.n = -1;
                r->lval = true;
                r->ct = false;

				if(exprPostfixPrim(r))
				{
					return true;
				}
			}
		}
		iTk=start;
	}
	if(consume(DOT))
	{
		if(consume(ID))
		{
			Token *tkName = consumedTk;

			 if (r->type.tb != TB_STRUCT) 
			 {
				tkerr("a field can only be selected from a struct");
			}

			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);

            if (!s) 
			{
				tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			}

			*r = (Ret){s->type,true,s->type.n>=0};

			if(exprPostfixPrim(r))
			{
				return true;
			}
		}
		iTk=start;
	}
	return true;
}
// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//      | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r) { 
	Token* start=iTk;

	if(consume(ID))
	{
		Token *tkName = consumedTk;
		Symbol *s = findSymbol(tkName->text);

		if (!s) 
		{
			tkerr("undefined id: %s", tkName->text);
		}

		if(consume(LPAR))
		{
			if(s->kind!=SK_FN)
				tkerr("only a function can be called");

			Ret rArg;
			Symbol *param=s->fn.params;

			if(expr(&rArg))
			{
				if (!param) 
				{
					tkerr("too many arguments in function call");
				}

				if (!convTo(&rArg.type, &param->type)) 
				{
					tkerr("in call, cannot convert the argument type to the parameter type");
				}

				addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);

				param=param->next;

				for(;;)
				{
					if(consume(COMMA))
					{
						if(expr(&rArg))
						{
							if (!param) 
							{
								tkerr("too many arguments in function call");
							}

							if (!convTo(&rArg.type, &param->type)) 
							{
								tkerr("in call, cannot convert the argument type to the parameter type");
							}

							addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                            insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
							
							param=param->next;
						}
						else 
						{
							tkerr("Missing expression after \',\'\n");
						}
					}
					else 
					{
						break;
					}
				}
			}
			if(consume(RPAR))
			{
				if (param) 
				{
					tkerr("too few arguments in function call");
				}

				*r = (Ret){s->type,false,true};

				if (s->fn.extFnPtr) 
                {
                    addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
                } 
                else 
                {
                    addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
                }

				return true;
			}
			else
			{
				if (s->kind == SK_FN) 
				{
					tkerr("a function can only be called");
				}

				*r = (Ret){s->type,true,s->type.n>=0};
			}
		}
		else 
		{
			if(s->kind==SK_FN) 
			{
				tkerr("a function can only be called"); 
			}
			*r = (Ret){s->type,true,s->type.n>=0};

			if (s->kind == SK_VAR) 
            {
                if (s->owner == NULL) 
                {// global variables
                    addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
                } 
                else 
                {// local variables
                    switch (s->type.tb) 
                    {
                        case TB_INT:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1);
                            break;
                        case TB_DOUBLE:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1);
                            break;
                    }
                }
            }

            if (s->kind == SK_PARAM) 
            {
                switch (s->type.tb) 
                {
                    case TB_INT:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                    case TB_DOUBLE:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                }
            }
		}

		return true;
	}
	if(consume(INT))
	{
		*r = (Ret){{TB_INT,NULL,-1},false,true};
		Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
		return true;
	}
	if(consume(DOUBLE))
	{
		*r = (Ret){{TB_DOUBLE,NULL,-1},false,true};
		Token *ct = consumedTk;
        addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
		return true;
	}
	if(consume(CHAR))
	{
		*r = (Ret){{TB_CHAR,NULL,-1},false,true};
		return true;
	}
	if(consume(STRING))
	{
		*r = (Ret){{TB_CHAR,NULL,0},false,true};
		return true;
	}
	if(consume(LPAR))
	{
		if(expr(r))
		{
			if(consume(RPAR))
			{
				return true;
			}
		}
	}
	
	iTk=start;
	return false;
}

void parse(Token *tokens)
{
    iTk = tokens;
    if (!unit())
        tkerr("syntax error");
}
