// SDL Experiment 15, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

// A 2D vector:
typedef struct xyVector
{
	double xComponent;
	double yComponent;
} xyVector;

// A struct storing the needed data to draw a ship:
typedef struct ship
{
	SDL_Rect rectangle;
	xyVector position;
	xyVector velocity;
	xyVector gravity;
	int number;
} ship;

// Get the angle between vectors:
static inline double angleBetweenVectors(xyVector * vectorA, xyVector * vectorB)
{
	double dotProduct = (vectorA->xComponent * vectorB->xComponent) + (vectorA->yComponent * vectorB->yComponent);
	double determinant = (vectorA->xComponent * vectorB->yComponent) - (vectorA->yComponent * vectorB->xComponent);

	return atan2(dotProduct, determinant) / 0.01745329;
}

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

int main(int argc, char ** argv)
{
	SDL_Event event;
	bool quit = false;
	int width = 0, height = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	uint64_t thisFrameTime = SDL_GetPerformanceCounter(), lastFrameTime = 0;
	long positionX = 512, positionY = 512, starPositionX = 0, starPositionY = 0;
	xyVector positionVector = {512, 512}, velocityVector = {1, 0}, gravityVector = {0, 0},
		engineVector = {0.04, 0}, upVector = {0, 0.1}, starPosition = {0, 0};

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

	// Get the initial
	ship shipA;
	ship shipB;
	
	// Initialize the SDL library, video, sound, and input:
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL Initialization Error: %s\n", SDL_GetError());
	}

	// Initialize image loading:
	IMG_Init(IMG_INIT_PNG);
	SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "2");

	// Create an SDL window and rendering context in that window:
	SDL_Window * window = SDL_CreateWindow("SDL_TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 700, 700, 0);
	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, rendererFlags);
	SDL_SetWindowTitle(window, "Spacewar!");
	
	// Load in all of our textures:
	SDL_Texture * idleTexture, * acceleratingTexture, * clockwiseTexture, * anticlockwiseTexture, * currentTexture,
	    * acceleratingTexture2;
	
	idleTexture = IMG_LoadTexture(renderer, "./Experiment-15-Images/Ship-Idle.png");
	clockwiseTexture = IMG_LoadTexture(renderer, "./Experiment-15-Images/Ship-Clockwise.png");
	acceleratingTexture = IMG_LoadTexture(renderer, "./Experiment-15-Images/Ship-Accelerating.png");
	anticlockwiseTexture = IMG_LoadTexture(renderer, "./Experiment-15-Images/Ship-Anticlockwise.png");
	acceleratingTexture2 = IMG_LoadTexture(renderer, "./Experiment-15-Images/Ship-Accelerating-Frame-2.png");

	// Enable resizing the window:
	SDL_SetWindowResizable(window, SDL_TRUE);
	ship Temp;
	bool shipAUpdated, shipBUpdated;
	while (!quit)
	{
		while(!(shipAUpdated && shipBUpdated))
		{
			// Receive data from the socket:
			recvfrom(socketFileDesc, &Temp, sizeof(ship), 0, NULL, NULL);
			if(Temp.number == 0)
			{
				shipA = Temp;
				shipAUpdated = true;
			}
			if(Temp.number == 1)
			{
				shipB = Temp;
				shipBUpdated = true;
			}
		}
		
		// Check if the user wants to quit:
		while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
				case SDL_QUIT:
				{
					quit = true;
					break;
				}
				default:
				{
					break;
				}
            }
        }

        // Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);

		// Set the texture to idle:
		currentTexture = idleTexture;
		
		// Calculate the position of the sprites:
		shipB.rectangle.x = (width/2) - 16 - (shipB.velocity.xComponent * 15);
		shipB.rectangle.y = (height/2) - 16 - (shipB.velocity.yComponent * 15);

		shipA.rectangle.x = (long)((((shipA.position.xComponent - shipB.position.xComponent) - 32) + width/2) - (shipB.velocity.xComponent * 15));
		shipA.rectangle.y = (long)((((shipA.position.yComponent - shipB.position.yComponent) - 32) + height/2) - (shipB.velocity.yComponent * 15));
		
		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Draw the ship:
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipA.rectangle,
						 angleBetweenVectors(&shipA.velocity, &upVector) + 90, NULL, 0);
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipB.rectangle,
						 angleBetweenVectors(&shipB.velocity, &upVector) + 90, NULL, 0);
		
		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Draw a circle as the star:
		DrawCircle(renderer, (long)(starPositionX - shipB.position.xComponent) + width/2  - (shipB.velocity.xComponent * 15),
				   (long)(starPositionY - shipB.position.yComponent) + height/2  - (shipB.velocity.yComponent * 15), 50);

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);
		shipAUpdated = false;
		shipBUpdated = false;
	}
	return 0;
}
// ========================================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-15-Client.c -lSDL2_image -lm -o 'Spacewar Client!'"
// End:
