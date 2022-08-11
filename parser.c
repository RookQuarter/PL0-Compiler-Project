// Written by Sean Bennett

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "compiler.h"

#define MAX_CODE_LENGTH 200
#define MAX_SYMBOL_COUNT 50
#define MAX_REG_COUNT 10

// generated code
instruction *code;
int cIndex;

// symbol table
symbol *table;
int tIndex;

// Tracker variables
int lIndex;
int level;
int registerCounter;

// Function definitions
void emit(int opname, int reg, int level, int mvalue);
void addToSymbolTable(int k, char n[], int s, int l, int a, int m);
void mark();
int multipledeclarationcheck(char name[]);
int findsymbol(char name[], int kind);
void printparseerror(int err_code);
void printsymboltable();
void printassemblycode();

int block(lexeme * list);
int varDeclaration(lexeme* list);
int procedureDeclaration(lexeme * list);
int statement(lexeme * list);
int condition(lexeme * list);
int expression(lexeme * list);
int term(lexeme * list);
int factor(lexeme * list);

instruction *parse(lexeme *list, int printTable, int printCode)
{
	// set up program variables
	code = malloc(sizeof(instruction) * MAX_CODE_LENGTH);
	cIndex = 0;
	table = malloc(sizeof(symbol) * MAX_SYMBOL_COUNT);
	tIndex = 0;

	lIndex = 0;
	registerCounter = -1;

	// emit the first jump (with placeholder value)
	emit(7, 0, 0, 0);
	// put main in the symbol table
	addToSymbolTable(3, "main", 0, 0, 0, 0);
	level = -1;

	if (block(list) == -1) return NULL;
	// check for ending period
	if (list[lIndex].type != periodsym)
    {
        printparseerror(1);
        return NULL;
    }

    // emit halt
	emit(11, 0, 0, 0);
	// set actual value for first jump
	code[0].m = table[0].addr;
	// set actual value for cal instructions
	for (int i = 0; i < cIndex; i++)
    {
        if (code[i].opcode == 5)
            code[i].m = table[code[i].m].addr;
    }
	// print off table and code
	if (printTable)
		printsymboltable();
	if (printCode)
		printassemblycode();

	// mark the end of the code
	code[cIndex].opcode = -1;
	return code;
}

int block(lexeme * list)
{
    level++;
    // Grabbing the index of the current procedure from the symbol table
    int procedureIndex = tIndex - 1;
    // Determining amount of memory that needs to be reserved and setting up vars in sym table
    int x = varDeclaration(list);
    if (x == -1) return -1;
    // Setting up procedures in sym table
    if (procedureDeclaration(list) == -1) return -1;
    // Setting the correct address for the procedure for later
    table[procedureIndex].addr = cIndex;
    // emit inc based on how much memory needed to be reserved
    emit(6, 0, 0, x);
    if (statement(list) == -1) return -1;
    // mark the variables for the block now that we are done
    mark();
    level--;
    return 1;
}

int varDeclaration(lexeme * list)
{
    // base memory needed for an AR
    int memorySize = 3;
    char symbolName[15];
    int arraySize;
    if (list[lIndex].type == varsym)
    {
        do
        {
            lIndex++;
            if (list[lIndex].type != identsym)
            {
                printparseerror(2);
                return -1;
            }
            if (multipledeclarationcheck(list[lIndex].name) != -1)
            {
                printparseerror(3);
                return -1;
            }
            strcpy(symbolName, list[lIndex].name);
            lIndex++;
            // if the variable is an array
            if (list[lIndex].type == lbracketsym)
            {
                lIndex++;
                if (list[lIndex].type != numbersym || list[lIndex].value == 0)
                {
                    printparseerror(4);
                    return -1;
                }
                arraySize = list[lIndex].value;
                lIndex++;
                if (list[lIndex].type == multsym || list[lIndex].type == divsym ||
                    list[lIndex].type == modsym || list[lIndex].type == addsym ||
                    list[lIndex].type == subsym)
                {
                    printparseerror(4);
                    return -1;
                }
                if (list[lIndex].type != rbracketsym)
                {
                    printparseerror(5);
                    return -1;
                }
                lIndex++;
                addToSymbolTable(2, symbolName, arraySize, level, memorySize, 0);
                memorySize += arraySize;
            }else
            {
                addToSymbolTable(1, symbolName, 0, level, memorySize, 0);
                memorySize++;
            }
        }while(list[lIndex].type == commasym);
        if (list[lIndex].type == identsym)
        {
            printparseerror(6);
            return -1;
        }
        if (list[lIndex].type != semicolonsym)
        {
            printparseerror(7);
            return -1;
        }
        lIndex++;
    }
    return memorySize;
}

int procedureDeclaration(lexeme * list)
{
    char symbolName[15];
    while(list[lIndex].type == procsym)
    {
        lIndex++;
        if (list[lIndex].type != identsym)
        {
            printparseerror(2);
            return -1;
        }
        if (multipledeclarationcheck(list[lIndex].name) != -1)
        {
            printparseerror(3);
            return -1;
        }
        strcpy(symbolName, list[lIndex].name);
        lIndex++;
        if (list[lIndex].type != semicolonsym)
        {
            printparseerror(8);
            return -1;
        }
        lIndex++;
        addToSymbolTable(3, symbolName, 0, level, 0, 0);
        if (block(list) == -1) return -1;
        if (list[lIndex].type != semicolonsym)
        {
            printparseerror(7);
            return -1;
        }
        lIndex++;
        // emit ret
        emit(2, 0, 0, 0);
    }
    return 1;
}

int statement(lexeme * list)
{
    char symbolName[15];
    int symIndex;
    int arrIdxReg;
    int varLocReg;
    int jpcIndex;
    int jmpIndex;
    int loopIndex;
    // assignment case
    if (list[lIndex].type == identsym)
    {
        strcpy(symbolName, list[lIndex].name);
        lIndex++;
        // if assigning a value to an array element
        if (list[lIndex].type == lbracketsym)
        {
            lIndex++;
            symIndex = findsymbol(symbolName, 2);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 1) != -1)
                {
                    printparseerror(11);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            // calculating the index of the array
            if (expression(list) == -1) return -1;
            arrIdxReg = registerCounter;
            if (list[lIndex].type != rbracketsym)
            {
                printparseerror(5);
                return -1;
            }
            lIndex++;
            if (list[lIndex].type != assignsym)
            {
                printparseerror(13);
                return -1;
            }
            lIndex++;
            // calculating the value we are assigning the element to
            if (expression(list) == -1) return -1;
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            // emit add
            emit(13, arrIdxReg, arrIdxReg, registerCounter);
            registerCounter--;
            // emit store
            emit(4, registerCounter, level-table[symIndex].level, arrIdxReg);
            registerCounter -= 2;
        }else
        {
            // assigning to a variable
            symIndex = findsymbol(symbolName, 1);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 2) != -1)
                {
                    printparseerror(12);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            varLocReg = registerCounter;
            if (list[lIndex].type != assignsym)
            {
                printparseerror(13);
                return -1;
            }
            lIndex++;
            // calculating the value we are assigning to the variable
            if (expression(list) == -1) return -1;
            // emit store
            emit(4, registerCounter, level-table[symIndex].level, varLocReg);
            registerCounter -= 2;
        }
    }
    // call case
    else if (list[lIndex].type == callsym)
    {
        lIndex++;
        if (list[lIndex].type != identsym)
        {
            printparseerror(15);
            return -1;
        }
        symIndex = findsymbol(list[lIndex].name, 3);
        if (symIndex == -1)
        {
            if (findsymbol(list[lIndex].name, 1) != -1 || findsymbol(list[lIndex].name, 2) != -1)
            {
                printparseerror(15);
                return -1;
            }
            printparseerror(10);
            return -1;
        }
        // emit call
        emit(5, 0, level-table[symIndex].level, symIndex);
        lIndex++;
    }
    // begin-end case
    else if (list[lIndex].type == beginsym)
    {
        do
        {
            lIndex++;
            if (statement(list) == -1) return -1;
        }while(list[lIndex].type == semicolonsym);
        if (list[lIndex].type != endsym)
        {
            if (list[lIndex].type == identsym || list[lIndex].type == callsym
                || list[lIndex].type == beginsym || list[lIndex].type == ifsym
                || list[lIndex].type == dosym || list[lIndex].type == readsym
                || list[lIndex].type == writesym)
            {
                printparseerror(16);
                return -1;
            }
            printparseerror(17);
            return -1;
        }
        lIndex++;
    }
    // if case
    else if (list[lIndex].type == ifsym)
    {
        lIndex++;
        if (condition(list) == -1) return -1;
        jpcIndex = cIndex;
        // emit jpc with placeholder
        emit(8, registerCounter, 0, 0);
        registerCounter--;
        if (list[lIndex].type != questionsym)
        {
            printparseerror(18);
            return -1;
        }
        lIndex++;
        if (statement(list) == -1) return -1;
        // if there is an else
        if (list[lIndex].type == colonsym)
        {
            lIndex++;
            jmpIndex = cIndex;
            // emit jump placeholder
            emit(7, 0, 0, 0);
            // fix jpc placeholder value
            code[jpcIndex].m = cIndex;
            if (statement(list) == -1) return -1;
            // fix jump placeholder
            code[jmpIndex].m = cIndex;
        }
        else
        {
            // fix jpc placeholder when there is not an else
            code[jpcIndex].m = cIndex;
        }
    }
    // do-while case
    else if (list[lIndex].type == dosym)
    {
        lIndex++;
        loopIndex = cIndex;
        if (statement(list) == -1) return -1;
        if (list[lIndex].type != whilesym)
        {
            printparseerror(19);
            return -1;
        }
        lIndex++;
        if (condition(list) == -1) return -1;
        registerCounter++;
        if (registerCounter >= 10)
        {
            printparseerror(14);
            return -1;
        }
        // emit lit
        emit(1, registerCounter, 0, 0);
        // emit eql (setting up condition for jpc)
        emit(18, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
        // emit jpc
        emit(8, registerCounter, 0, loopIndex);
        registerCounter--;
    }
    // read case
    else if (list[lIndex].type == readsym)
    {
        lIndex++;
        if (list[lIndex].type != identsym)
        {
            printparseerror(20);
            return -1;
        }
        strcpy(symbolName, list[lIndex].name);
        lIndex++;
        if (list[lIndex].type == lbracketsym)
        {
            lIndex++;
            symIndex = findsymbol(symbolName, 2);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 1) != -1)
                {
                    printparseerror(11);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            // calculating index of the array
            if (expression(list) == -1) return -1;
            arrIdxReg = registerCounter;
            if (list[lIndex].type != rbracketsym)
            {
                printparseerror(5);
                return -1;
            }
            lIndex++;
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit read
            emit(10, registerCounter, 0, 0);
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            // emit add
            emit(13, arrIdxReg, arrIdxReg, registerCounter);
            registerCounter--;
            // emit store
            emit(4, registerCounter, level-table[symIndex].level, arrIdxReg);
            registerCounter -= 2;
        } else
        {
            // variable case
            symIndex = findsymbol(symbolName, 1);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 2) != -1)
                {
                    printparseerror(12);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            varLocReg = registerCounter;
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // emit read
            emit(10, registerCounter, 0, 0);
            // emit store
            emit(4, registerCounter, level-table[symIndex].level, varLocReg);
            registerCounter -= 2;
        }
    }
    // write case
    else if (list[lIndex].type == writesym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        // emit write
        emit(9, registerCounter, 0, 0);
        registerCounter--;
    }
    return 1;
}

int condition(lexeme * list)
{
    if (expression(list) == -1) return -1;
    if (list[lIndex].type == eqlsym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(18, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else if (list[lIndex].type == neqsym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(19, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else if (list[lIndex].type == lsssym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(20, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else if (list[lIndex].type == leqsym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(21, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else if (list[lIndex].type == gtrsym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(22, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else if (list[lIndex].type == geqsym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        emit(23, registerCounter-1, registerCounter-1, registerCounter);
        registerCounter--;
    }
    else
    {
        printparseerror(21);
        return -1;
    }
    return 1;
}

int expression (lexeme * list)
{
    if (list[lIndex].type == subsym)
    {
        lIndex++;
        if (term(list) == -1) return -1;
        // emit negate
        emit(12, registerCounter, 0, registerCounter);
        while (list[lIndex].type == addsym || list[lIndex].type == subsym)
        {
            if (list[lIndex].type == addsym)
            {
                lIndex++;
                if (term(list) == -1) return -1;
                // emit add
                emit(13, registerCounter-1, registerCounter-1, registerCounter);
                registerCounter--;
            }
            else
            {
                lIndex++;
                if (term(list) == -1) return -1;
                // emit sub
                emit(14, registerCounter-1, registerCounter-1, registerCounter);
                registerCounter--;
            }
        }
    }
    else
    {
        // if expression doesnt start with negation
        if (term(list) == -1) return -1;
        while (list[lIndex].type == addsym || list[lIndex].type == subsym)
        {
            if (list[lIndex].type == addsym)
            {
                lIndex++;
                if (term(list) == -1) return -1;
                emit(13, registerCounter-1, registerCounter-1, registerCounter);
                registerCounter--;
            }
            else
            {
                lIndex++;
                if (term(list) == -1) return -1;
                emit(14, registerCounter-1, registerCounter-1, registerCounter);
                registerCounter--;
            }
        }
    }
    if (list[lIndex].type == lparenthesissym || list[lIndex].type == identsym || list[lIndex].type == numbersym)
    {
        printparseerror(22);
        return -1;
    }
    return 1;
}

int term(lexeme * list)
{
    if (factor(list) == -1) return -1;
    while (list[lIndex].type == multsym || list[lIndex].type == divsym || list[lIndex].type == modsym)
    {
        if (list[lIndex].type == multsym)
        {
            lIndex++;
            if (factor(list) == -1) return -1;
            emit(15, registerCounter-1, registerCounter-1, registerCounter);
            registerCounter--;
        }
        else if (list[lIndex].type == divsym)
        {
            lIndex++;
            if (factor(list) == -1) return -1;
            emit(16, registerCounter-1, registerCounter-1, registerCounter);
            registerCounter--;
        }
        else
        {
            lIndex++;
            if (factor(list) == -1) return -1;
            emit(17, registerCounter-1, registerCounter-1, registerCounter);
            registerCounter--;
        }
    }
    return 1;
}

int factor(lexeme * list)
{
    char symbolName[15];
    int symIndex;
    int arrIdxReg;
    int varLocReg;
    if (list[lIndex].type == identsym)
    {
        strcpy(symbolName, list[lIndex].name);
        lIndex++;
        if (list[lIndex].type == lbracketsym)
        {
            lIndex++;
            symIndex = findsymbol(symbolName, 2);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 1) != -1)
                {
                    printparseerror(11);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            // calculate array index
            if (expression(list) == -1) return -1;
            arrIdxReg = registerCounter;
            if (list[lIndex].type != rbracketsym)
            {
                printparseerror(5);
                return -1;
            }
            lIndex++;
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            // add
            emit(13, arrIdxReg, arrIdxReg, registerCounter);
            registerCounter--;
            // load
            emit(3, registerCounter, level-table[symIndex].level, arrIdxReg);
        } else
        {
            symIndex = findsymbol(symbolName, 1);
            if (symIndex == -1)
            {
                if (findsymbol(symbolName, 2) != -1)
                {
                    printparseerror(12);
                    return -1;
                }
                if (findsymbol(symbolName, 3) != -1)
                {
                    printparseerror(9);
                    return -1;
                }
                printparseerror(10);
                return -1;
            }
            registerCounter++;
            if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
            // lit
            emit(1, registerCounter, 0, table[symIndex].addr);
            varLocReg = registerCounter;
            // load
            emit(3, registerCounter, level-table[symIndex].level, varLocReg);
        }
    }
    else if (list[lIndex].type == numbersym)
    {
        registerCounter++;
        if (registerCounter >= 10)
            {
                printparseerror(14);
                return -1;
            }
        // lit
        emit(1, registerCounter, 0, list[lIndex].value);
        lIndex++;
    }
    else if (list[lIndex].type == lparenthesissym)
    {
        lIndex++;
        if (expression(list) == -1) return -1;
        if (list[lIndex].type != rparenthesissym)
        {
            printparseerror(23);
            return -1;
        }
        lIndex++;
    }
    else
    {
        printparseerror(24);
        return -1;
    }
    return 1;
}

void emit(int opname, int reg, int level, int mvalue)
{
	code[cIndex].opcode = opname;
	code[cIndex].r = reg;
	code[cIndex].l = level;
	code[cIndex].m = mvalue;
	cIndex++;
}

void addToSymbolTable(int k, char n[], int s, int l, int a, int m)
{
	table[tIndex].kind = k;
	strcpy(table[tIndex].name, n);
	table[tIndex].size = s;
	table[tIndex].level = l;
	table[tIndex].addr = a;
	table[tIndex].mark = m;
	tIndex++;
}

void mark()
{
	int i;
	for (i = tIndex - 1; i >= 0; i--)
	{
		if (table[i].mark == 1)
			continue;
		if (table[i].level < level)
			return;
		table[i].mark = 1;
	}
}

int multipledeclarationcheck(char name[])
{
	int i;
	for (i = 0; i < tIndex; i++)
		if (table[i].mark == 0 && table[i].level == level && strcmp(name, table[i].name) == 0)
			return i;
	return -1;
}

int findsymbol(char name[], int kind)
{
	int i;
	int max_idx = -1;
	int max_lvl = -1;
	for (i = 0; i < tIndex; i++)
	{
		if (table[i].mark == 0 && table[i].kind == kind && strcmp(name, table[i].name) == 0)
		{
			if (max_idx == -1 || table[i].level > max_lvl)
			{
				max_idx = i;
				max_lvl = table[i].level;
			}
		}
	}
	return max_idx;
}

void printparseerror(int err_code)
{
	switch (err_code)
	{
		case 1:
			printf("Parser Error: Program must be closed by a period\n");
			break;
		case 2:
			printf("Parser Error: Symbol names must be identifiers\n");
			break;
		case 3:
			printf("Parser Error: Confliciting symbol declarations\n");
			break;
		case 4:
			printf("Parser Error: Array sizes must be given as a single, nonzero number\n");
			break;
		case 5:
			printf("Parser Error: [ must be followed by ]\n");
			break;
		case 6:
			printf("Parser Error: Multiple symbols in var declaration must be separated by commas\n");
			break;
		case 7:
			printf("Parser Error: Symbol declarations should close with a semicolon\n");
			break;
		case 8:
			printf("Parser Error: Procedure declarations should contain a semicolon before the body of the procedure begins\n");
			break;
		case 9:
			printf("Parser Error: Procedures may not be assigned to, read, or used in arithmetic\n");
			break;
		case 10:
			printf("Parser Error: Undeclared identifier\n");
			break;
		case 11:
			printf("Parser Error: Variables cannot be indexed\n");
			break;
		case 12:
			printf("Parserr Error: Arrays must be indexed\n");
			break;
		case 13:
			printf("Parser Error: Assignment operator missing\n");
			break;
		case 14:
			printf("Parser Error: Register Overflow Error\n");
			break;
		case 15:
			printf("Parser Error: call must be followed by a procedure identifier\n");
			break;
		case 16:
			printf("Parser Error: Statements within begin-end must be separated by a semicolon\n");
			break;
		case 17:
			printf("Parser Error: begin must be followed by end\n");
			break;
		case 18:
			printf("Parser Error: if must be followed by ?\n");
			break;
		case 19:
			printf("Parser Error: do must be followed by while\n");
			break;
		case 20:
			printf("Parser Error: read must be followed by a var or array identifier\n");
			break;
		case 21:
			printf("Parser Error: Relational operator missing from condition\n");
			break;
		case 22:
			printf("Parser Error: Bad arithmetic\n");
			break;
		case 23:
			printf("Parser Error: ( must be followed by )\n");
			break;
		case 24:
			printf("Parser Error: Arithmetic expressions may only contain arithmetic operators, numbers, parentheses, and variables\n");
			break;
		default:
			printf("Implementation Error: unrecognized error code\n");
			break;
	}

	free(code);
	free(table);
}

void printsymboltable()
{
	int i;
	printf("Symbol Table:\n");
	printf("Kind | Name        | Size | Level | Address | Mark\n");
	printf("---------------------------------------------------\n");
	for (i = 0; i < tIndex; i++)
		printf("%4d | %11s | %5d | %4d | %5d | %5d\n", table[i].kind, table[i].name, table[i].size, table[i].level, table[i].addr, table[i].mark);

	free(table);
	table = NULL;
}

void printassemblycode()
{
	int i;
	printf("Line\tOP Code\tOP Name\tR\tL\tM\n");
	for (i = 0; i < cIndex; i++)
	{
		printf("%d\t", i);
		printf("%d\t", code[i].opcode);
		switch (code[i].opcode)
		{
			case 1:
				printf("LIT\t");
				break;
			case 2:
				printf("RET\t");
				break;
			case 3:
				printf("LOD\t");
				break;
			case 4:
				printf("STO\t");
				break;
			case 5:
				printf("CAL\t");
				break;
			case 6:
				printf("INC\t");
				break;
			case 7:
				printf("JMP\t");
				break;
			case 8:
				printf("JPC\t");
				break;
			case 9:
				printf("WRT\t");
				break;
			case 10:
				printf("RED\t");
				break;
			case 11:
				printf("HLT\t");
				break;
			case 12:
				printf("NEG\t");
				break;
			case 13:
				printf("ADD\t");
				break;
			case 14:
				printf("SUB\t");
				break;
			case 15:
				printf("MUL\t");
				break;
			case 16:
				printf("DIV\t");
				break;
			case 17:
				printf("MOD\t");
				break;
			case 18:
				printf("EQL\t");
				break;
			case 19:
				printf("NEQ\t");
				break;
			case 20:
				printf("LSS\t");
				break;
			case 21:
				printf("LEQ\t");
				break;
			case 22:
				printf("GTR\t");
				break;
			case 23:
				printf("GEQ\t");
				break;
			default:
				printf("err\t");
				break;
		}
		printf("%d\t%d\t%d\n", code[i].r, code[i].l, code[i].m);
	}

	if (table != NULL)
		free(table);
}
