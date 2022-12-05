#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>

unsigned long long divideAndCarry(unsigned long long numerator, unsigned long long divisor, unsigned long long * remainder)
{
	*remainder = numerator % divisor;
	return numerator / divisor;
}

void main()
{
	time_t currentTime;
	time(&currentTime);
	unsigned long long remaining_time, remainder, years, months, days, hours, minutes, seconds;
	while(1)
	{
		time(&currentTime);
		remainder = (unsigned long long)difftime(INT_MAX, currentTime);
		years = divideAndCarry(remainder, 31557600, &remainder);
		months = divideAndCarry(remainder, 2629800, &remainder);
		days = divideAndCarry(remainder, 86400, &remainder);
		hours =	divideAndCarry(remainder, 3600, &remainder);
		minutes =  divideAndCarry(remainder, 60, &remainder);
		seconds = remainder % 60;
		printf("\e[1;1H\e[2J");
		printf("= TIME UNTIL THE YEAR 2038 ENDS ALL LIFE ON EARTH =\n\n");
		printf("\tYears: %02d, Months:  %02d, Days:    %02d\n"
			   "\tHours: %02d, Minutes: %02d, Seconds: %02d\n\n",
			   years, months, days, hours, minutes, seconds);
		printf("= AAAAAAAAAAAH! IT'S SO VERY SCARY! AAAAAAAAAAAH! =\n");
		sleep(1);

	}
}
