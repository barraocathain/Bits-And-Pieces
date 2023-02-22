// SDL Experiment 13, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

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
} ship;

// Calculate the vector from point A to point B:
static inline void xyVectorBetweenPoints(long ax, long ay, long bx, long by, xyVector * vector)
{
	vector->xComponent = bx - ax;
	vector->yComponent = by - ay;
}

// Normalize a vector, returning the magnitude:
static inline double normalizeXYVector(xyVector * vector)
{
	double magnitude = sqrt(pow(vector->xComponent, 2) + pow(vector->yComponent, 2));
	if(magnitude != 0)
	{
		vector->xComponent /= magnitude;
		vector->yComponent /= magnitude;
	}
	return magnitude;
}

// Get the angle between vectors:
static inline double angleBetweenVectors(xyVector * vectorA, xyVector * vectorB)
{
	double dotProduct = (vectorA->xComponent * vectorB->xComponent) + (vectorA->yComponent * vectorB->yComponent);
	double determinant = (vectorA->xComponent * vectorB->yComponent) - (vectorA->yComponent * vectorB->xComponent);

	return atan2(dotProduct, determinant) / 0.01745329;
}

// Rotate XY vector by a given number of degrees:
static inline void rotateXYVector(xyVector * vector, double degrees)
{
	double xComponent = vector->xComponent, yComponent = vector->yComponent;
	vector->xComponent = (cos(degrees * 0.01745329) * xComponent) - (sin(degrees * 0.01745329) * yComponent);
	vector->yComponent = (sin(degrees * 0.01745329) * xComponent) + (cos(degrees * 0.01745329) * yComponent);
}

// Add vector B to vector A:
static inline void addXYVector(xyVector * vectorA, xyVector * vectorB)
{
	vectorA->xComponent += vectorB->xComponent;
	vectorA->yComponent += vectorB->yComponent; 
}

// Add vector B to vector A, scaled for units per frame:
static inline void addXYVectorDeltaScaled(xyVector * vectorA, xyVector * vectorB, double deltaTime)
{
	vectorA->xComponent += vectorB->xComponent * (0.001 * deltaTime) * 60;
	vectorA->yComponent += vectorB->yComponent * (0.001 * deltaTime) * 60; 
}

// Multiply a vector by a scalar constant:
static inline void multiplyXYVector(xyVector * vector, double scalar)
{	
	vector->xComponent *= scalar;
	vector->yComponent *= scalar;
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
			gravityAcceleration = 0.1 * (35000 / (pow(gravityMagnitude, 2)));
		}
		else
		{
			gravityAcceleration = 0;
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
ship createShip(int width, int height, double positionX, double positionY, double velocityX, double velocityY)
{
	ship newShip;
	newShip.rectangle.w = width;
	newShip.rectangle.h = height;
	newShip.position.xComponent = positionX;
	newShip.position.yComponent = positionY;
	newShip.velocity.xComponent = velocityX;
	newShip.velocity.yComponent = velocityY;
	newShip.gravity.xComponent = 0;
	newShip.gravity.yComponent = 0;
	return newShip;
}

int main(int argc, char ** argv)
{
	SDL_Event event;
	int width = 0, height = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	uint64_t thisFrameTime = SDL_GetPerformanceCounter(), lastFrameTime = 0;
	long positionX = 512, positionY = 512, starPositionX = 0, starPositionY = 0;
	double deltaTime = 0, gravityMagnitude = 0, gravityAcceleration = 0, frameAccumulator = 0;	
	bool quit = false, rotatingClockwise = false, rotatingAnticlockwise = false, accelerating = false;
	xyVector positionVector = {512, 512}, velocityVector = {1, 0}, gravityVector = {0, 0},
		engineVector = {0.04, 0}, upVector = {0, 0.1}, starPosition = {0, 0};

	// Get the initial
	ship shipA = createShip(32, 32, 512, 512, 1, 0);
	ship shipB = createShip(32, 32, -512, -512, 0, 1);
	
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

	// Load in all of our textures:
	SDL_Texture * idleTexture, * acceleratingTexture, * clockwiseTexture, * anticlockwiseTexture, * currentTexture,
	    * acceleratingTexture2;
	
	idleTexture = IMG_LoadTexture(renderer, "./Experiment-13-Images/Ship-Idle.png");
	clockwiseTexture = IMG_LoadTexture(renderer, "./Experiment-13-Images/Ship-Clockwise.png");
	acceleratingTexture = IMG_LoadTexture(renderer, "./Experiment-13-Images/Ship-Accelerating.png");
	anticlockwiseTexture = IMG_LoadTexture(renderer, "./Experiment-13-Images/Ship-Anticlockwise.png");
	acceleratingTexture2 = IMG_LoadTexture(renderer, "./Experiment-13-Images/Ship-Accelerating-Frame-2.png");

	// Enable resizing the window:
	SDL_SetWindowResizable(window, SDL_TRUE);

	while (!quit)
	{
		lastFrameTime = thisFrameTime;
		thisFrameTime = SDL_GetPerformanceCounter();
		deltaTime = (double)(((thisFrameTime - lastFrameTime) * 1000) / (double)SDL_GetPerformanceFrequency());
		
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
				case SDL_KEYDOWN:
				{
					switch (event.key.keysym.sym)
					{
						case SDLK_LEFT:
						{						
							rotatingAnticlockwise = true;
							break;
						}
						case SDLK_RIGHT:
						{
							rotatingClockwise = true;
							break;
						}
						case SDLK_UP:
						{
							accelerating = true;
							break;
						}
						default:
						{
							break;
						}
					}
					break;
				}
				case SDL_KEYUP:
				{
					switch (event.key.keysym.sym)
					{
						case SDLK_LEFT:
						{
							rotatingAnticlockwise = false;
							break;
						}
						case SDLK_RIGHT:
						{
							rotatingClockwise = false;
							break;
						}
						case SDLK_UP:
						{
							accelerating = false;
							frameAccumulator = 0;
							break;
						}
						default:
						{
							break;
						}
					}
					break;
				}
				default:
				{
					break;
				}
            }
        }

		// Wrap the positions if the ship goes interstellar:
		if(shipA.position.xComponent > 4096)
		{
			shipA.position.xComponent = -2000;
			shipA.velocity.xComponent *= 0.9;
		}
		else if(shipA.position.xComponent < -4096)
		{
			shipA.position.xComponent = 2000;
			shipA.velocity.xComponent *= 0.9;
		}
		if(shipA.position.yComponent > 4096)
		{
			shipA.position.yComponent = -2000;
			shipA.velocity.yComponent *= 0.9;
		}
		else if(shipA.position.yComponent < -4096)
		{
			shipA.position.yComponent = 2000;
			shipA.velocity.yComponent *= 0.9;
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
		
        // Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);

		// Calculate the gravity for the ships:
		calculateGravity(&starPosition, &shipA);
		calculateGravity(&starPosition, &shipB);
		
		// Set the texture to idle:
		currentTexture = idleTexture;
		
		// Rotate the engine vector if needed:
		if(rotatingClockwise)
		{
			rotateXYVector(&engineVector, 0.25 * deltaTime);
			currentTexture = clockwiseTexture;
		}
		if(rotatingAnticlockwise)
		{
			rotateXYVector(&engineVector, -0.25 * deltaTime);
			currentTexture = anticlockwiseTexture;
		}
		
		// Calculate the new current velocity:
		addXYVectorDeltaScaled(&shipA.velocity, &shipA.gravity, deltaTime);
		addXYVectorDeltaScaled(&shipB.velocity, &shipB.gravity, deltaTime);
		
		if(accelerating)
		{
			addXYVectorDeltaScaled(&shipA.velocity, &engineVector, deltaTime);
			frameAccumulator += deltaTime;
			currentTexture = acceleratingTexture;
			if((long)frameAccumulator % 4)
			{
				currentTexture = acceleratingTexture2;
			}
		}
		
		// Calculate the new position:
		addXYVectorDeltaScaled(&shipA.position, &shipA.velocity, deltaTime);
		addXYVectorDeltaScaled(&shipB.position, &shipB.velocity, deltaTime);
		
		// Calculate the position of the sprites:
		shipA.rectangle.x = (width/2) - 16;
		shipA.rectangle.y = (height/2) - 16;

		shipB.rectangle.x = (long)(((shipB.position.xComponent - shipA.position.xComponent) - 32) + width/2);
		shipB.rectangle.y = (long)(((shipB.position.yComponent - shipA.position.yComponent) - 32) + height/2);
		
		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Draw the ship:
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipA.rectangle,
						 angleBetweenVectors(&engineVector, &upVector) + 90, NULL, 0);
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipB.rectangle,
						 angleBetweenVectors(&shipB.velocity, &upVector) + 90, NULL, 0);
		
		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Draw a circle as the star:
		DrawCircle(renderer, (long)(starPositionX - shipA.position.xComponent) + width/2,
				   (long)(starPositionY - shipA.position.yComponent) + height/2, 50);

		// Draw a line representing the velocity:
		SDL_RenderDrawLine(renderer, width/2, height/2,
						   (long)((width/2) + shipA.velocity.xComponent * 15),
	   					   (long)((height/2) + shipA.velocity.yComponent * 15));

		// Set the colour to blue:
		SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);

		// Draw a line representing the direction of the star:
		normalizeXYVector(&shipA.gravity);
		multiplyXYVector(&shipA.gravity, 100);
		SDL_RenderDrawLine(renderer, width/2, height/2,
						   (long)((width/2) + shipA.gravity.xComponent),
						   (long)((height/2) + shipA.gravity.yComponent));

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);
	}
	return 0;
}
// ===========================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-13.c -lSDL2_image -lm" 
// End:
