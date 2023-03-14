// SDL Experiment 19, Barra Ó Catháin.
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
#include "xyVector.h"
#include "spacewarPlayer.h"

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

void calculateGravity(xyVector * starPosition, ship * shipUnderGravity)
{
	// Calculate the vector between the star and ship:
	xyVectorBetweenPoints(shipUnderGravity->position.xComponent, shipUnderGravity->position.yComponent,
						  starPosition->xComponent, starPosition->yComponent, &shipUnderGravity->gravity);
		
	// Make it into a unit vector:
	double gravityMagnitude = normalizeXYVector(&shipUnderGravity->gravity);
	double gravityAcceleration = 0;
	
	// Calculate the gravity between the star and ship:
	if(gravityMagnitude != 0)
	{
		if(gravityMagnitude >= 116)
		{
			gravityAcceleration = pow(2, (3000 / (pow(gravityMagnitude, 2)))) / 8;
		}
		else
		{
			gravityAcceleration = 1;
		}
	}
	else
	{
		gravityAcceleration = 1;
	}
	
	if(gravityAcceleration < 0.01)
	{
		gravityAcceleration = 0.01;
	}
		
	// Scale the vector:
	multiplyXYVector(&shipUnderGravity->gravity, gravityAcceleration);	
}

// Create a ship with the given parameters:
ship createShip(int width, int height, double positionX, double positionY, double velocityX, double velocityY, int number)
{
	ship newShip;

	// Player number:
	newShip.number = number;
	
	// Rectangle to show the ship in:
	newShip.rectangle.w = width;
	newShip.rectangle.h = height;
	
	// Position:
	newShip.position.xComponent = positionX;
	newShip.position.yComponent = positionY;

	// Velocity:
	newShip.velocity.xComponent = velocityX;
	newShip.velocity.yComponent = velocityY;

	// Gravity:
	newShip.gravity.xComponent = 0;
	newShip.gravity.yComponent = 0;

	// Engine:
	newShip.engine.yComponent = 0;
	newShip.engine.xComponent = 0.1;   	
	return newShip;
}

playerController createShipPlayerController(ship * ship)
{
	playerController newController;
	newController.number = ship->number;
	return newController;
}

static inline void takeNetworkInput(playerController * controller, int descriptor)
{
	recvfrom(descriptor, controller, sizeof(playerController), 0, NULL, NULL);
}


static inline void getPlayerInput(playerController * controller, int playerNumber)
{
	SDL_PumpEvents();
	const uint8_t * keyboardState = SDL_GetKeyboardState(NULL);
	if(keyboardState[SDL_SCANCODE_UP] == 1)
	{
		controller->accelerating = true;
	}
	else
	{
		controller->accelerating = false;
	}
	if(keyboardState[SDL_SCANCODE_LEFT] == 1)
	{
		controller->turningAnticlockwise = true;
	}
	else
	{
		controller->turningAnticlockwise = false;;
	}
	if(keyboardState[SDL_SCANCODE_RIGHT] == 1)
	{
		controller->turningClockwise = true;
	}
	else
	{
		controller->turningClockwise = false;
	}
	if(controller->joystick != NULL)
	{
		controller->turningAmount = SDL_JoystickGetAxis(controller->joystick, 0);
		controller->acceleratingAmount = SDL_JoystickGetAxis(controller->joystick, 5);
	}
}

void doShipInput(playerController * controller, ship * ship, xyVector starPosition, double deltaTime)
{
	if(controller->number == ship->number)
	{
		// Calculate the gravity for the ships:
		calculateGravity(&starPosition, ship);
				
		// Rotate the engine vector if needed:
		if (controller->turningClockwise)
		{
			rotateXYVector(&ship->engine, 0.25 * deltaTime);
		}
		else if (controller->turningAmount > 2500)
		{
			double rotationalSpeed = (controller->turningAmount / 20000);
			rotateXYVector(&ship->engine, 0.25 * deltaTime * rotationalSpeed);
		}
		
		if (controller->turningAnticlockwise)
		{
			rotateXYVector(&ship->engine, -0.25 * deltaTime);	
		}
		else if (controller->turningAmount < -2500)
		{
			double rotationalSpeed = (controller->turningAmount / 20000);
			rotateXYVector(&ship->engine, 0.25 * deltaTime * rotationalSpeed);
		}
		
		// Calculate the new current velocity:
		addXYVectorDeltaScaled(&ship->velocity, &ship->gravity, deltaTime);

		if (controller->acceleratingAmount > 2500)
		{
			xyVector temporary = ship->engine;
			multiplyXYVector(&ship->engine, controller->acceleratingAmount/ 32748);
			SDL_HapticRumblePlay(controller->haptic, (float)controller->acceleratingAmount / 32768, 20);
			addXYVectorDeltaScaled(&ship->velocity, &ship->engine, deltaTime);
			ship->engine = temporary;
		}
		
		else if (controller->accelerating)
		{
			addXYVectorDeltaScaled(&ship->velocity, &ship->engine, deltaTime);
		}
		
		// Calculate the new position:
		addXYVectorDeltaScaled(&ship->position, &ship->velocity, deltaTime);
	}
}

int main(int argc, char ** argv)
{
	SDL_Event event;
	int width = 0, height = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	uint64_t thisFrameTime = SDL_GetPerformanceCounter(), lastFrameTime = 0;
	long starPositionX = 0, starPositionY = 0;
	double deltaTime = 0, frameAccumulator = 0;	
	bool quit = false, rotatingClockwise = false, rotatingAnticlockwise = false, accelerating = false;
	xyVector engineVector = {0.85, 0}, upVector = {0, 0.1}, starPosition = {0, 0};

	// Create the socket:
	int sendSocket = socket(AF_INET, SOCK_DGRAM, 0);
	if (sendSocket < 0)
	{
		fprintf(stderr, "\tSocket Creation is:\t\033[33;40mRED.\033[0m Aborting launch.\n");
		exit(0);
	}
	printf("\tSocket Creation is:\t\033[32;40mGREEN.\033[0m\n");

	// Create and fill the information needed to bind to the socket:
	struct sockaddr_in sendAddress;
	sendAddress.sin_family = AF_INET; // IPv4
	sendAddress.sin_addr.s_addr = inet_addr("127.0.0.1");
	sendAddress.sin_port = htons(12000);

	int receiveSocket = socket(AF_INET, SOCK_DGRAM, 0);
	if (receiveSocket < 0)
	{
		fprintf(stderr, "\tSocket Creation is:\t\033[33;40mRED.\033[0m Aborting launch.\n");
		exit(0);
	}
	printf("\tSocket Creation is:\t\033[32;40mGREEN.\033[0m\n");

	// Make the socket timeout:
	struct timeval readTimeout;
	readTimeout.tv_sec = 0;
	readTimeout.tv_usec = 800;
	setsockopt(receiveSocket, SOL_SOCKET, SO_RCVTIMEO, &readTimeout, sizeof(readTimeout));
	
	// Create and fill the information needed to bind to the socket:
	struct sockaddr_in receiveAddress;
	memset(&receiveAddress, 0, sizeof(receiveAddress));
	receiveAddress.sin_family = AF_INET; // IPv4
	receiveAddress.sin_addr.s_addr = INADDR_ANY;
	receiveAddress.sin_port = htons(12001);

	// Bind to the socket:
	if (bind(receiveSocket, (const struct sockaddr *)&receiveAddress, sizeof(receiveAddress)) < 0)
    {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }

	ship shipA = createShip(32, 32, 512, 512, 1, 0, 0);
	ship shipB = createShip(32, 32, -512, -512, 0, 1, 1);
	
	// Initialize the SDL library, video, sound, and input:
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL Initialization Error: %s\n", SDL_GetError());
	}
	
	// Initialize image loading:
	IMG_Init(IMG_INIT_PNG);
	SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "2");
	SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, "1");
	
	playerController playerOne = createShipPlayerController(&shipA);
	playerController playerTwo = createShipPlayerController(&shipB);

	// Check for joysticks:
	if (SDL_NumJoysticks() < 1 )
	{
		printf( "Warning: No joysticks connected!\n" );
	}
	else
	{
		// Load all joysticks:
		int joystickListLength = SDL_NumJoysticks();
		SDL_Joystick ** joysticksList = calloc(joystickListLength, sizeof(SDL_Joystick*));
		
		for(int index = 0; index < SDL_NumJoysticks(); index++)
		{
			joysticksList[index] = SDL_JoystickOpen(index);
		}

		// Choose a player joystick:
		printf("Please press button zero on the controller you wish to use. \n");

		int joystickIndex = 0;
		while(SDL_JoystickGetButton(joysticksList[joystickIndex], 0) == 0)
		{
			SDL_PumpEvents();
			joystickIndex++;
			if(joystickIndex >= joystickListLength)
			{
				joystickIndex = 0;
			}
		}
		
		// Load joystick
		playerOne.joystick = joysticksList[joystickIndex];
		if (playerOne.joystick == NULL )
		{
			printf( "Warning: Unable to open game controller! SDL Error: %s\n", SDL_GetError() );
		}
		playerOne.haptic = SDL_HapticOpenFromJoystick(playerOne.joystick);
		SDL_HapticRumbleInit(playerOne.haptic);
	}

		// Create an SDL window and rendering context in that window:
	SDL_Window * window = SDL_CreateWindow("SDL_TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 700, 700, 0);
	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, rendererFlags);
	SDL_SetWindowTitle(window, "Spacewar!");
	
	// Load in all of our textures:
	SDL_Texture * idleTexture, * acceleratingTexture, * clockwiseTexture, * anticlockwiseTexture, * currentTexture,
	    * acceleratingTexture2;
	
	idleTexture = IMG_LoadTexture(renderer, "./Images/Ship-Idle.png");
	clockwiseTexture = IMG_LoadTexture(renderer, "./Images/Ship-Clockwise.png");
	acceleratingTexture = IMG_LoadTexture(renderer, "./Images/Ship-Accelerating.png");
	anticlockwiseTexture = IMG_LoadTexture(renderer, "./Images/Ship-Anticlockwise.png");
	acceleratingTexture2 = IMG_LoadTexture(renderer, "./Images/Ship-Accelerating-Frame-2.png");
	currentTexture = acceleratingTexture;

	// Enable resizing the window:
	SDL_SetWindowResizable(window, SDL_TRUE);

	lastFrameTime = SDL_GetPerformanceCounter();
	thisFrameTime = SDL_GetPerformanceCounter();
	
	while (!quit)
	{
		lastFrameTime = thisFrameTime;
		thisFrameTime = SDL_GetPerformanceCounter();
		deltaTime = (double)(((thisFrameTime - lastFrameTime) * 1000) / (double)SDL_GetPerformanceFrequency());

		sendto(sendSocket, &shipA, sizeof(ship), 0, (const struct sockaddr *)&sendAddress, sizeof(sendAddress));
		sendto(sendSocket, &shipB, sizeof(ship), 0, (const struct sockaddr *)&sendAddress, sizeof(sendAddress));
		// Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);
		
		// Check input:
		while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
				case SDL_QUIT:
				{
					quit = true;
					break;
				}
			}
		}
		
		// Wrap the positions if the ship goes interstellar:
		if(shipA.position.xComponent > 4096)
		{
			shipA.position.xComponent = -2000;
		}
		else if(shipA.position.xComponent < -4096)
		{
			shipA.position.xComponent = 2000;
		}
		if(shipA.position.yComponent > 4096)
		{
			shipA.position.yComponent = -2000;
		}
		else if(shipA.position.yComponent < -4096)
		{
			shipA.position.yComponent = 2000;
		}

		if(shipB.position.xComponent > 4096)
		{
			shipB.position.xComponent = -2000;
			shipB.velocity.xComponent *= 0.9;
		}
		else if(shipB.position.xComponent < -4096)
		{
			shipB.position.xComponent = 2000;
			shipB.velocity.xComponent *= 0.9;
		}
		if(shipB.position.yComponent > 4096)
		{
			shipB.position.yComponent = -2000;
			shipB.velocity.yComponent *= 0.9;
		}
		else if(shipB.position.yComponent < -4096)
		{
			shipB.position.yComponent = 2000;
			shipB.velocity.yComponent *= 0.9;
		}

		// Get the needed input:
		getPlayerInput(&playerOne, 0);
		takeNetworkInput(&playerTwo, receiveSocket);

		// Do the needed input:
		doShipInput(&playerOne, &shipA, starPosition, deltaTime);
		doShipInput(&playerTwo, &shipB, starPosition, deltaTime);
		
		shipA.rectangle.x = (width/2) - 16 - (shipA.velocity.xComponent * 15);
		shipA.rectangle.y = (height/2) - 16 - (shipA.velocity.yComponent * 15);

		shipB.rectangle.x = (long)((((shipB.position.xComponent - shipA.position.xComponent) - 32) + width/2) - (shipA.velocity.xComponent * 15));
		shipB.rectangle.y = (long)((((shipB.position.yComponent - shipA.position.yComponent) - 32) + height/2) - (shipA.velocity.yComponent * 15));
		

		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Draw the ship:
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipA.rectangle,
						 angleBetweenVectors(&shipA.engine, &upVector) + 90, NULL, 0);
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipB.rectangle,
						 angleBetweenVectors(&shipB.engine, &upVector) + 90, NULL, 0);

		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Draw a circle as the star:
		DrawCircle(renderer, (long)(starPositionX - shipA.position.xComponent) + width/2  - (shipA.velocity.xComponent * 15),
				   (long)(starPositionY - shipA.position.yComponent) + height/2  - (shipA.velocity.yComponent * 15), 50);

		// Draw a line representing the velocity:
		SDL_RenderDrawLine(renderer, width/2 - (shipA.velocity.xComponent * 15),
						   height/2  - (shipA.velocity.yComponent * 15),
						   (long)((width/2) + shipA.velocity.xComponent * 15)  - (shipA.velocity.xComponent * 15),
	   					   (long)((height/2) + shipA.velocity.yComponent * 15)  - (shipA.velocity.yComponent * 15));

		// Set the colour to blue:
		SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);

		// Draw a line representing the direction of the star:
		normalizeXYVector(&shipA.gravity);
		multiplyXYVector(&shipA.gravity, 100);
		SDL_RenderDrawLine(renderer,
						   width/2  - (shipA.velocity.xComponent * 15),
						   height/2  - (shipA.velocity.yComponent * 15),
						   (width/2  - (shipA.velocity.xComponent * 15)) + shipA.gravity.xComponent,
						   ((height/2) - (shipA.velocity.yComponent * 15)) + shipA.gravity.yComponent);

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);
	}
	return 0;
}
// ========================================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-19.c -lSDL2_image -lm -o 'Spacewar!'"
// End:
