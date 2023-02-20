// A small utility to get the hash of a string for SilverMUD's command evaluator:
#include <string.h>
#include <stdio.h>
#include <ctype.h>

unsigned int hashCommand(char * command, unsigned int commandLength)
{
	// Lowercase the string:
	for (char * character = command; *character; ++character)
	{
		*character = tolower(*character);
	}

	// Hash the string:
	char * currentCharacter = command;
	unsigned int hash = 0;
	
	for (unsigned int index = 0; index < commandLength && *currentCharacter != '\0'; currentCharacter++)
	{
		hash = 37 * hash + *currentCharacter;
	}

	return hash;
}

void main(int argc, char ** argv)
{
	if(argc >= 2)
	{
		printf("%u\n", hashCommand(argv[1], strlen(argv[1])));
	}
	else
	{
		printf("Please provide a string to hash as argument one.\n");
	}
}
