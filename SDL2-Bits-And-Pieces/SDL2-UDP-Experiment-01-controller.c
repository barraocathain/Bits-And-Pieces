#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
typedef struct position
{
	int x;
	int y;		
} position;

int main()
{
	// Create the socket:
	int socketFileDesc = socket(AF_INET, SOCK_DGRAM, 0);
	if (socketFileDesc < 0)
	{
		fprintf(stderr, "\tSocket Creation is:\t\033[33;40mRED.\033[0m Aborting launch.\n");
		exit(0);
	}
	printf("\tSocket Creation is:\t\033[32;40mGREEN.\033[0m\n");

	// Create and fill the information needed to bind to the socket:
	struct sockaddr_in serverAddress;
	serverAddress.sin_family = AF_INET; // IPv4
	serverAddress.sin_addr.s_addr = inet_addr("127.0.0.1");
	serverAddress.sin_port = htons(12000);

	position posToSend = {0, 0};

	// Send data at the server:
	while (1)
	{
		scanf("%d %d", &posToSend.x, &posToSend.y);
		sendto(socketFileDesc, &posToSend, sizeof(posToSend), 0, (const struct sockaddr *)&serverAddress, sizeof(serverAddress));
	}
}
