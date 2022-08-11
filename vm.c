// Written by Sean Bennett

/*
	You can use these two print statements for the errors:
		printf("Virtual Machine Error: Stack Overflow Error\n");
		printf("Virtual Machine Error: Out of Bounds Access Error\n");
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "compiler.h"
#define REG_FILE_SIZE 10
#define MAX_STACK_LENGTH 100

void fetch(instruction* code, int* PC, instruction* IR);
void execute(instruction IR, int* PC, int* BP, int* SP, int* RF, int* stack, int* haltFlag);

void print_execution(int line, char *opname, instruction IR, int PC, int BP, int SP, int *stack, int *RF)
{
	int i;
	// print out instruction and registers
	printf("%2d\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t\t", line, opname, IR.r, IR.l, IR.m, PC, SP, BP);

	// print register file
	for (i = 0; i < REG_FILE_SIZE; i++)
		printf("%d ", RF[i]);
	printf("\n");

	// print stack
	printf("stack:\t");
	for (i = MAX_STACK_LENGTH - 1; i >= SP; i--)
		printf("%d ", stack[i]);
	printf("\n");
}

int base(int L, int BP, int *stack)
{
	int ctr = L;
	int rtn = BP;
	while (ctr > 0)
	{
		rtn = stack[rtn];
		ctr--;
	}
	return rtn;
}

void execute_program(instruction *code, int printFlag)
{
    // Initializing variables
    int BP = MAX_STACK_LENGTH-1;
	int SP = BP+1;
	int PC = 0;
	instruction IR;
	int *RF = calloc(REG_FILE_SIZE, sizeof(int));
	int *stack = calloc(MAX_STACK_LENGTH, sizeof(int));
	int haltFlag = 0;
	int line = 0;

	char* names[] = {"Invalid", "LIT", "RET", "LOD", "STO", "CAL", "INC", "JMP", "JPC", "WRT", "RED", "HLT", "NEG", "ADD", "SUB", "MUL", "DIV", "MOD", "EQL", "NEQ", "LSS", "LEQ", "GTR", "GEQ"};

	// keep this
	if (printFlag)
	{
		printf("\t\t\t\t\tPC\tSP\tBP\n");
		printf("Initial values:\t\t\t\t%d\t%d\t%d\n", PC, SP, BP);
	}

	// Continue until execution is halted by error or HLT instruction
	while (!haltFlag)
    {
        // Fetch instruction
        fetch(code, &PC, &IR);
        // Execute instruction
        execute(IR, &PC, &BP, &SP, RF, stack, &haltFlag);
        // Check if we need to print and update line counter
        if (printFlag && (!haltFlag || IR.opcode == 11))
            print_execution(line, names[IR.opcode], IR, PC, BP, SP, stack, RF);
        line = PC;
    }

}

void fetch(instruction* code, int* PC, instruction* IR)
{
    // Load the next instruction and increment PC
    IR->opcode=code[*PC].opcode;
    IR->l=code[*PC].l;
    IR->m=code[*PC].m;
    IR->r=code[*PC].r;
    *PC += 1;
}

void execute(instruction IR, int* PC, int* BP, int* SP, int* RF, int* stack, int* haltFlag)
{
    // Determine which instruction to run via opcode
    switch(IR.opcode)
    {
    // LIT
    case 1:
        RF[IR.r] = IR.m;
        break;

    // RET
    case 2:
        *SP = *BP + 1;
        *BP = stack[*SP-2];
        *PC = stack[*SP-3];
        break;

    // LOD
    case 3:
        if (base(IR.l, *BP, stack)-IR.m < 0 || base(IR.l, *BP, stack)-IR.m >= MAX_STACK_LENGTH)
        {
            *haltFlag = 1;
            printf("Virtual Machine Error: Out of Bounds Access Error\n");
        }
        else
        {
            RF[IR.r] = stack[base(IR.l, *BP, stack)-RF[IR.m]];
        }
        break;

    // STO
    case 4:
        if (base(IR.l, *BP, stack)-IR.m < 0 || base(IR.l, *BP, stack)-IR.m >= MAX_STACK_LENGTH)
        {
            *haltFlag = 1;
            printf("Virtual Machine Error: Out of Bounds Access Error\n");
        }
        else
        {
            stack[base(IR.l, *BP, stack)-RF[IR.m]] = RF[IR.r];
        }
        break;

    // CAL
    case 5:
        stack[*SP-1] = base(IR.l, *BP, stack);
        stack[*SP-2] = *BP;
        stack[*SP-3] = *PC;
        *BP = *SP-1;
        *PC = IR.m;
        break;

    // INC
    case 6:
        *SP = *SP - IR.m;
        if (*SP < 0)
        {
            *haltFlag = 1;
            printf("Virtual Machine Error: Stack Overflow Error\n");
        }
        break;

    // JMP
    case 7:
        *PC = IR.m;
        break;

    // JPC
    case 8:
        if (RF[IR.r] == 0)
            *PC = IR.m;
        break;

    // WRT
    case 9:
        printf("Write Value: %d\n", RF[IR.r]);
        break;

    // RED
    case 10:
        printf("Please Enter a Value: ");
        scanf("%d", &RF[IR.r]);
        printf("\n");
        break;

    // HLT
    case 11:
        *haltFlag = 1;
        break;

    // NEG
    case 12:
        RF[IR.r] = -RF[IR.r];
        break;

    // ADD
    case 13:
        RF[IR.r] = RF[IR.l] + RF[IR.m];
        break;

    // SUB
    case 14:
        RF[IR.r] = RF[IR.l] - RF[IR.m];
        break;

    // MUL
    case 15:
        RF[IR.r] = RF[IR.l] * RF[IR.m];
        break;

    // DIV
    case 16:
        RF[IR.r] = RF[IR.l] / RF[IR.m];
        break;

    // MOD
    case 17:
        RF[IR.r] = RF[IR.l] % RF[IR.m];
        break;

    // EQL
    case 18:
        if (RF[IR.l] == RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    // NEQ
    case 19:
        if (RF[IR.l] != RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    // LSS
    case 20:
        if (RF[IR.l] < RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    // LEQ
    case 21:
        if (RF[IR.l] <= RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    // GTR
    case 22:
        if (RF[IR.l] > RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    // GEQ
    case 23:
        if (RF[IR.l] >= RF[IR.m])
            RF[IR.r] = 1;
        else
            RF[IR.r] = 0;
        break;

    }
}
