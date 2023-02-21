// SDL/UDP Experiment 01, Barra Ó Catháin.
// =======================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

void DrawCircle(SDL_Renderer * renderer, int32_t centreX, int32_t centreY, int32_t radius)
{
   const int32_t diameter = (radius * 2);

   int32_t x = (radius - 1);
   int32_t y = 0;
   int32_t tx = 1;
   int32_t ty = 1;
   int32_t error = (tx - diameter);

   while (x >= y)
   {
	 //  Each of the following renders an octant of the circle
      SDL_RenderDrawPoint(renderer, centreX + x, centreY - y);
      SDL_RenderDrawPoint(renderer, centreX + x, centreY + y);
      SDL_RenderDrawPoint(renderer, centreX - x, centreY - y);
      SDL_RenderDrawPoint(renderer, centreX - x, centreY + y);
      SDL_RenderDrawPoint(renderer, centreX + y, centreY - x);
      SDL_RenderDrawPoint(renderer, centreX + y, centreY + x);
      SDL_RenderDrawPoint(renderer, centreX - y, centreY - x);
      SDL_RenderDrawPoint(renderer, centreX - y, centreY + x);

      if (error <= 0)
      {
         ++y;
         error += ty;
         ty += 2;
      }

      if (error > 0)
      {
         --x;
         tx += 2;
         error += (tx - diameter);
      }
   }
}

// Get the largest radius for a circle that can fit in the width and height of a rectangle:
static inline int getRadius(int width, int height)
{
	return (width/2 < height/2) ? width/2 : height/2;
}


int main(int argc, char ** argv)
{
	SDL_Event event;
	int positionX = 0, positionY = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	
	// Initialize the SDL library, video, sound, and input:
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL Initialization Error: %s\n", SDL_GetError());
	}
	
	// Create an SDL window and rendering context in that window:
	SDL_Window * window = SDL_CreateWindow("SDL_TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 640, 0);
	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, rendererFlags);
	
	// Enable resizing the window:
	SDL_SetWindowResizable(window, SDL_TRUE);

	// Create the socket:
	int socketFileDesc = socket(AF_INET, SOCK_DGRAM, 0);
	if (socketFileDesc < 0)
	{
		fprintf(stderr, "\tSocket Creation is:\t\033[33;40mRED.\033[0m Aborting launch.\n");
		exit(0);
	}
	printf("\tSocket Creation is:\t\033[32;40mGREEN.\033[0m\n");

	// Make the socket timeout:
	struct timeval read_timeout;
	read_timeout.tv_sec = 0;
	read_timeout.tv_usec = 10;
	setsockopt(socketFileDesc, SOL_SOCKET, SO_RCVTIMEO, &read_timeout, sizeof read_timeout);
	
	// Create and fill the information needed to bind to the socket:
	struct sockaddr_in serverAddress;
	memset(&serverAddress, 0, sizeof(serverAddress));
	serverAddress.sin_family = AF_INET; // IPv4
	serverAddress.sin_addr.s_addr = INADDR_ANY;
	serverAddress.sin_port = htons(12000);

	// Bind to the socket:
	if (bind(socketFileDesc, (const struct sockaddr *)&serverAddress, sizeof(serverAddress)) < 0)
    {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }

	// A struct to transfer the position data:
	typedef struct position
	{
		int x;
		int y;		
	} position;

	position currentPosition = {0, 0};
	
	while (true)
	{
		// Receive data from the socket:
		recvfrom(socketFileDesc, &currentPosition, sizeof(position), 0, NULL, NULL);

		// Set the position to the received one:
		positionX = currentPosition.x;
		positionY = currentPosition.y;
		
		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);
		
		// Draw a circle around the position pointer:
		DrawCircle(renderer, positionX, positionY, 15);

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);

		// Delay enough so that we run at 144 frames:
		SDL_Delay(1000 / 144);
	}
	return 0;
}
// =============================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-UDP-Experiment-01.c"
// End:
