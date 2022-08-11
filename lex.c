// Written by Sean Bennett

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "compiler.h"
#define MAX_NUMBER_TOKENS 1000
#define MAX_IDENT_LEN 11
#define MAX_NUMBER_LEN 5

lexeme *list;
int lex_index;

int alphatoken();
int numbertoken();
int symboltoken();
int comment();
int reservedcheck(char *buffer);
void printlexerror(int type);
void printtokens();


lexeme *lexanalyzer(char *input, int printFlag)
{
    // Allocating memory for the list and setting tracker variables
	list = malloc(sizeof(lexeme) * MAX_NUMBER_TOKENS);
	lex_index = 0;
	int str_index = 0;
	// Loops until we have processed all input
	while(str_index < strlen(input))
    {
        switch(input[str_index])
        {
        // Cases that dont require look ahead
        case '.':
            list[lex_index].type = periodsym;
            lex_index++;
            break;
        case '[':
            list[lex_index].type = lbracketsym;
            lex_index++;
            break;
        case ']':
            list[lex_index].type = rbracketsym;
            lex_index++;
            break;
        case ',':
            list[lex_index].type = commasym;
            lex_index++;
            break;
        case ';':
            list[lex_index].type = semicolonsym;
            lex_index++;
            break;
        case '?':
            list[lex_index].type = questionsym;
            lex_index++;
            break;
        case '(':
            list[lex_index].type = lparenthesissym;
            lex_index++;
            break;
        case ')':
            list[lex_index].type = rparenthesissym;
            lex_index++;
            break;
        case '%':
            list[lex_index].type = modsym;
            lex_index++;
            break;
        case '*':
            list[lex_index].type = multsym;
            lex_index++;
            break;
        case '-':
            list[lex_index].type = subsym;
            lex_index++;
            break;
        case '+':
            list[lex_index].type = addsym;
            lex_index++;
            break;

        // If = is the start of a token, the only valid possibility is ==
        case '=':
            if (input[str_index+1] == '=')
            {
                list[lex_index].type = eqlsym;
                lex_index++;
                // Have to increment an additional time to start next token
                str_index++;
            }
            else
            {
                printlexerror(4);
                return NULL;
            }
            break;

        // : can be a standalone symbol or :=
        case ':':
            if (input[str_index+1] == '=')
            {
                list[lex_index].type = assignsym;
                lex_index++;
                str_index++;
            }
            else
            {
                list[lex_index].type = colonsym;
                lex_index++;
            }
            break;

        // < can be standalone, <= or <>
        case '<':
            if (input[str_index+1] == '=')
            {
                list[lex_index].type = leqsym;
                lex_index++;
                str_index++;
            }
            else if (input[str_index+1] == '>')
            {
                list[lex_index].type = neqsym;
                lex_index++;
                str_index++;
            }
            else
            {
                list[lex_index].type = lsssym;
                lex_index++;
            }
            break;

        // > can be standalone or >=
        case '>':
            if (input[str_index+1] == '=')
            {
                list[lex_index].type = geqsym;
                lex_index++;
                str_index++;
            }
            else
            {
                list[lex_index].type = gtrsym;
                lex_index++;
            }
            break;

        // / can be standalone, or the start of a comment
        case '/':
            if (input[str_index+1] != '/')
            {
                list[lex_index].type = divsym;
                lex_index++;
            }
            else
            {
                // If we are reading a comment, continue until we reach the end of it
                while(!iscntrl(input[str_index]) && input[str_index] != EOF)
                    str_index++;
            }
            break;

        // Ignore whitespace
        case ' ':
            break;

        // There are numerous cases where we enter default
        default:
            // If the token starts with a letter
            if (isalpha(input[str_index]))
            {
                int start = str_index;
                int counter = 1;
                str_index++;
                // Continue reading until the character is not a digit or letter
                while (isalpha(input[str_index]) || isdigit(input[str_index]))
                {
                    counter++;
                    str_index++;
                }
                // Check if the identifier is too long
                if (counter > MAX_IDENT_LEN)
                {
                    printlexerror(3);
                    return NULL;
                }
                else
                {
                    // Store the full "word", which may be a keyword or identifier
                    strncpy(list[lex_index].name, &input[start], counter);
                    list[lex_index].name[counter] = '\0';
                    // Check if it is a keyword
                    if (strcmp(list[lex_index].name, "var") == 0)
                        list[lex_index].type = varsym;
                    else if (strcmp(list[lex_index].name, "procedure") == 0)
                        list[lex_index].type = procsym;
                    else if (strcmp(list[lex_index].name, "call") == 0)
                        list[lex_index].type = callsym;
                    else if (strcmp(list[lex_index].name, "begin") == 0)
                        list[lex_index].type = beginsym;
                    else if (strcmp(list[lex_index].name, "end") == 0)
                        list[lex_index].type = endsym;
                    else if (strcmp(list[lex_index].name, "if") == 0)
                        list[lex_index].type = ifsym;
                    else if (strcmp(list[lex_index].name, "do") == 0)
                        list[lex_index].type = dosym;
                    else if (strcmp(list[lex_index].name, "read") == 0)
                        list[lex_index].type = readsym;
                    else if (strcmp(list[lex_index].name, "write") == 0)
                        list[lex_index].type = writesym;
                    else if (strcmp(list[lex_index].name, "while") == 0)
                        list[lex_index].type = whilesym;
                    // If not a keyword, must be an identifier
                    else
                        list[lex_index].type = identsym;
                    lex_index++;
                    // Have to decrement str_index due to look ahead
                    str_index--;
                }
            }
            // If token starts with digit
            else if (isdigit(input[str_index]))
            {
                int start = str_index;
                int counter = 1;
                str_index++;
                // Look ahead until we don't find a digit
                while (isdigit(input[str_index]))
                {
                    counter++;
                    str_index++;
                }
                // Check if number is too long
                if (counter > MAX_NUMBER_LEN)
                {
                    printlexerror(2);
                    return NULL;
                }
                // Check if number is followed by a letter
                else if (isalpha(input[str_index]))
                {
                    printlexerror(1);
                    return NULL;
                }
                // If we have a valid number
                else
                {
                    strncpy(list[lex_index].name, &input[start], counter);
                    list[lex_index].name[counter] = '\0';
                    list[lex_index].value = atoi(list[lex_index].name);
                    list[lex_index].type = numbersym;
                    lex_index++;
                    str_index--;
                }
            }
            // If a token starts with a non-control character
            // If we got here and did not fall into a different category
            // We have an invalid symbol
            else if (!iscntrl(input[str_index]))
            {
                printlexerror(4);
                return NULL;
            }
            break;
        }
        // Move on to start reading the next token
        str_index++;
    }


	if (printFlag)
		printtokens();
	list[lex_index++].type = -1;
	return list;
}

void printtokens()
{
	int i;
	printf("Lexeme Table:\n");
	printf("lexeme\t\ttoken type\n");
	for (i = 0; i < lex_index; i++)
	{
		switch (list[i].type)
		{
			case periodsym:
				printf("%11s\t%d", ".", periodsym);
				break;
			case varsym:
				printf("%11s\t%d", "var", varsym);
				break;
			case lbracketsym:
				printf("%11s\t%d", "[", lbracketsym);
				break;
			case procsym:
				printf("%11s\t%d", "procedure", procsym);
				break;
			case rbracketsym:
				printf("%11s\t%d", "]", rbracketsym);
				break;
			case callsym:
				printf("%11s\t%d", "call", callsym);
				break;
			case commasym:
				printf("%11s\t%d", ",", commasym);
				break;
			case beginsym:
				printf("%11s\t%d", "begin", beginsym);
				break;
			case semicolonsym:
				printf("%11s\t%d", ";", semicolonsym);
				break;
			case endsym:
				printf("%11s\t%d", "end", endsym);
				break;
			case assignsym:
				printf("%11s\t%d", ":=", assignsym);
				break;
			case ifsym:
				printf("%11s\t%d", "if", ifsym);
				break;
			case questionsym:
				printf("%11s\t%d", "?", questionsym);
				break;
			case dosym:
				printf("%11s\t%d", "do", dosym);
				break;
			case colonsym:
				printf("%11s\t%d", ":", colonsym);
				break;
			case readsym:
				printf("%11s\t%d", "read", readsym);
				break;
			case lparenthesissym:
				printf("%11s\t%d", "(", lparenthesissym);
				break;
			case writesym:
				printf("%11s\t%d", "write", writesym);
				break;
			case rparenthesissym:
				printf("%11s\t%d", ")", rparenthesissym);
				break;
			case identsym:
				printf("%11s\t%d", list[i].name, identsym);
				break;
			case eqlsym:
				printf("%11s\t%d", "==", eqlsym);
				break;
			case numbersym:
				printf("%11d\t%d", list[i].value, numbersym);
				break;
			case neqsym:
				printf("%11s\t%d", "<>", neqsym);
				break;
			case modsym:
				printf("%11s\t%d", "%", modsym);
				break;
			case lsssym:
				printf("%11s\t%d", "<", lsssym);
				break;
			case divsym:
				printf("%11s\t%d", "/", divsym);
				break;
			case leqsym:
				printf("%11s\t%d", "<=", leqsym);
				break;
			case multsym:
				printf("%11s\t%d", "*", multsym);
				break;
			case gtrsym:
				printf("%11s\t%d", ">", gtrsym);
				break;
			case subsym:
				printf("%11s\t%d", "-", subsym);
				break;
			case geqsym:
				printf("%11s\t%d", ">=", geqsym);
				break;
			case addsym:
				printf("%11s\t%d", "+", addsym);
				break;
			case whilesym:
				printf("%11s\t%d", "while", whilesym);
				break;
			default:
				printf("%11s\t%s", "err", "err");
				break;
		}
		printf("\n");
	}
	printf("\n");
}

void printlexerror(int type)
{
	if (type == 1)
		printf("Lexical Analyzer Error: Invalid Identifier\n");
	else if (type == 2)
		printf("Lexical Analyzer Error: Number Length\n");
	else if (type == 3)
		printf("Lexical Analyzer Error: Identifier Length\n");
	else if (type == 4)
		printf("Lexical Analyzer Error: Invalid Symbol\n");
	else
		printf("Implementation Error: Unrecognized Error Type\n");

	free(list);
	return;
}
