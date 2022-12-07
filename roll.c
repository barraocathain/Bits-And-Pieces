// roll.c: A command line dice roller.
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct roll
{
	unsigned diceSides;
	unsigned numberOfDice;
} roll;

roll parseRollString(char * rollString)
{
	char * numberString, * sidesString, * currentPointer;
	currentPointer = rollString;
	numberString = strtok_r(rollString, "dD", &currentPointer);
	sidesString = strtok_r(NULL, "dD", &currentPointer);
	roll roll;
	roll.numberOfDice = (unsigned)atoi(numberString);
	roll.diceSides = (unsigned)atoi(sidesString);
	return roll;
}

int rollDice(roll roll)
{
	int rolledValue = 0;
	for(unsigned rolls = 0; rolls < roll.numberOfDice; rolls++)
	{
		rolledValue += (rand() % roll.diceSides) + 1;
	}
	return rolledValue;
}

int main(int argc, char ** argv)
{
	srand((unsigned)time(NULL) * getpid());
	roll roll;
	if(argc < 1)
	{
		roll = parseRollString(argv[1]);
	}
	else
	{
		roll.diceSides = 20;
		roll.numberOfDice = 1;
	}
	int result = rollDice(roll);
	printf("You rolled a: %d\n", result);
	return result;
}
