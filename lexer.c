#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin, const char *end) 
{
	size_t length = end - begin; 

	char *substring = safeAlloc(length + 1);  
	strncpy(substring, begin, length); 
	substring[length] = '\0'; 

	return substring;
}

Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
			case ' ' :
			case '\t':
				pch++;
				break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':
				addTk(END);
				return tokens;
			case ',':
				addTk(COMMA);
				pch++;
				break;
			case '(' :
				addTk(LPAR);
				pch++;
				break;
			case ')':
				addTk(RPAR);
				pch++;
				break;
			case '{':
				addTk(LACC);
				pch++;
				break;
			case '}':
				addTk(RACC);
				pch++;
				break;
			case '[':
				addTk(LBRACKET);
				pch++;
				break;
			case ']':
				addTk(RBRACKET);
				pch++;
				break;
			case ';':
				addTk(SEMICOLON);
				pch++;
				break;
			case '-':
				addTk(SUB);
				pch++;
				break;
			case '*':
				addTk(MUL);
				pch++;
				break;
			case '+':
				addTk(ADD);
				pch++;
				break;
			case '!':
				if(pch[1]== '=')
				{
					addTk(NOTEQ);
					pch = pch+2;
					break;
				}
				else 
				{
					addTk(NOT);
					pch++;
					break;
				}
			case '|':
			if (pch[1] == '|')
			{
				addTk(OR);
				pch += 2;
			}
			break;
			case '<':
				if(pch[1] == '=')
				{
					addTk(LESSEQ);
					pch=pch+2;
					break;
				}
				else
				{
					addTk(LESS);
					pch++;
					break;
				}
			case '>':
				if(pch[1] == '=')
				{
					addTk(GREATEREQ);
					pch=pch+2;
					break;
				}
				else
				{
					addTk(GREATER);
					pch++;
					break;
				}
			case '&':
				if (pch[1] == '&')
				{
					addTk(AND);
					pch=pch+2;
				}
				else
   				{
       			err("Error: '&' found without a pair on line %d", line);
        		pch++; 
    			}
    				break;	
			case '/':
				if(pch[1] == '/')
				{
				while (*pch != '\0' && *pch != '\n')
					pch++;
				}
				else
				{
					addTk(DIV);
					pch++;
				}
					break;
			case '=':
				if(pch[1]=='=')
				{
					addTk(EQUAL);
					pch+=2;
				}
				else
				{
					addTk(ASSIGN);
					pch++;
				}
				break;
			case '.':
				addTk(DOT);
				pch++;
				break;
			default:
				if(isalpha(*pch)||*pch=='_')
                {
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
                        char *text=extract(start,pch);

					if(strcmp(text,"char")==0)
                        addTk(TYPE_CHAR);
					else
                        if(strcmp(text,"double")==0)
                            addTk(TYPE_DOUBLE);
                        else
                            if(strcmp(text,"else")==0)
                                addTk(ELSE);
                            else
                                if(strcmp(text,"if")==0)
                                    addTk(IF);
                                else
                                    if(strcmp(text,"int")==0)
                                        addTk(TYPE_INT);
                                    else
                                        if(strcmp(text,"return")==0)
                                            addTk(RETURN);
                                        else
                                            if(strcmp(text,"struct")==0)
                                                addTk(STRUCT);
                                            else
                                                if(strcmp(text,"void")==0)
                                                    addTk(VOID);
                                                else
                                                    if(strcmp(text,"while")==0)
                                                        addTk(WHILE);
                                                    else{
                                                        tk=addTk(ID);
                                                        tk->text=text;
                                                    }
                }
                else
                    if(*pch=='\'')
                    {
                        pch++;
                        char a;
                        if(*pch != '\'')
                            a = *pch;
                        pch++;

                        if(*pch=='\'')
                        {
                            tk=addTk(CHAR);
                            tk->c=a;
                            pch++;
                        }
                        else
                            err("invalid char: %c (%d)",*pch,*pch);
                    }
                    else
 						if(*pch=='"') {
    						start = ++pch; // Sărim peste ghilimeaua de deschidere și începem să colectăm caracterele
    						while(*pch != '"' && *pch != '\0' && *pch != '\n') {
        						pch++;
    						}
    					if(*pch == '"') { // Dacă am găsit ghilimeaua de închidere
        					char *text = extract(start, pch);
        					tk = addTk(STRING);
        					tk->text = text;
        					pch++; // Sărim peste ghilimeaua de închidere
    					} else {
        					// Dacă șirul de caractere nu este închis corespunzător sau întâlnim sfârșitul codului sursă
        					err("String literal not properly closed or unexpected end of file");
    					}							
					}
			else

if(isdigit(*pch)){
    for(start=pch++;isdigit(*pch);pch++){}
    char *begin=start;
    char *text;
    if(pch[0]=='.')
    {
        if(isdigit(pch[1]))
        {
            for(pch++;isdigit(*pch);pch++){} // Continuăm după punct.
            if(pch[0]=='e'||pch[0]=='E')
            {
                if(pch[1]=='+' || pch[1]=='-') pch++;
                if(isdigit(pch[1])) // Verificăm dacă urmează cifre după e sau E.
                {
                    for(pch++;isdigit(*pch);pch++){}
                }
                else
                {
                    err("invalid double: missing digits after exponent"); // Eroare dacă lipsește partea numerică după exponent.
                    continue;
                }
            }
            text=extract(begin,pch);
            tk=addTk(DOUBLE);tk->d=atof(text);
        }
        else
        {
            err("invalid double: missing digits after decimal point"); // Eroare dacă lipsește partea numerică după punctul decimal.
            continue;
        }
    }
    else if(pch[0]=='e'||pch[0]=='E')
    {
        if(pch[1]=='+' || pch[1]=='-') pch++;
        if(isdigit(pch[1]))
        {
            for(pch++;isdigit(*pch);pch++){}
            text=extract(begin,pch);
            tk=addTk(DOUBLE);tk->d=atof(text);
        }
        else
        {
            err("invalid double: missing digits after exponent"); // Eroare dacă lipsește partea numerică după exponent.
            continue;
        }
    }
    else
    {
        text=extract(begin,pch);
        tk=addTk(INT);tk->i=atoi(text);
    }
}
	else err("invalid char: %c (%d)",*pch,*pch);
		}
	}
}


void showTokens(const Token *tokens)
{
	int line_counter = 1;
	for (const Token *tk = tokens; tk; tk = tk->next)
	{
		printf("%d\t", tk->line);
		switch (tk->code)
		{
		case TYPE_INT:
			printf("TYPE_INT\n");
			break;
		case TYPE_CHAR:
			printf("TYPE_CHAR\n");
			break;
		case TYPE_DOUBLE:
			printf("TYPE_DOUBLE\n");
			break;
		case ID:
			printf("ID:%s\n", tk->text);
			break;
		case LPAR:
			printf("LPAR\n");
			break;
		case RPAR:
			printf("RPAR\n");
			break;
		case LACC:
			printf("LACC\n");
			break;
		case RACC:
			printf("RACC\n");
			break;
		case LBRACKET:
			printf("LBRACKET\n");
			break;
		case RBRACKET:
			printf("RBRACKET\n");
			break;
		case SEMICOLON:
			printf("SEMICOLON\n");
			break;
		case INT:
			printf("INT:%d\n", tk->i);
			break;
		case DOUBLE:
			printf("DOUBLE:%.1f\n", tk->d);
			break;
		case STRING:
			printf("STRING:%s\n", tk->text);
			break;
		case CHAR:
			printf("CHAR:%c\n", tk->c);
			break;
		case WHILE:
			printf("WHILE\n");
			break;
		case LESS:
			printf("LESS\n");
			break;
		case DIV:
			printf("DIV\n");
			break;
		case ADD:
			printf("ADD\n");
			break;
		case AND:
			printf("AND\n");
			break;
		case MUL:
			printf("MUL\n");
			break;
		case IF:
			printf("IF\n");
			break;
		case ASSIGN:
			printf("ASSIGN\n");
			break;
		case EQUAL:
			printf("EQUAL\n");
			break;
		case RETURN:
			printf("RETURN\n");
			break;
		case END:
			printf("END\n");
			break;
		case ELSE:
			printf("ELSE\n");
			break;
		case STRUCT:
			printf("STRUCT\n");
			break;
		case VOID:
			printf("VOID\n");
			break;
		case SUB:
			printf("SUB\n");
			break;
		case OR:
			printf("OR\n");
			break;
		case NOT:
			printf("NOT\n");
			break;
		case NOTEQ:
			printf("NOTEQ\n");
			break;
		case LESSEQ:
			printf("LESSEQ\n");
			break;
		case GREATER:
			printf("GREATER\n");
			break;
		case GREATEREQ:
			printf("GREATEREQ\n");
			break;
		case DOT:
			printf("DOT\n");
			break;

		default:
			break;
		}
		line_counter += 1;
	}
}



char *tkCodeName(int code)
{
    char *Names[] = {"ID",
                     "TYPE-CHAR", "TYPE-DOUBLE", "ELSE", "IF", "TYPE-INT", "RETURN", "STRUCT", "VOID", "WHILE",
                     "INT", "DOUBLE", "CHAR", "STRING",
                     "COMMA", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC", "END",
                     "ADD", "SUB", "MUL", "DIV", "DOT", "AND", "OR", "NOT", "ASSIGN", "EQUAL",
                     "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ"};
    return Names[code];
}