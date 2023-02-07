// SDL Experiment 11, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct xyVector
{
	double xComponent;
	double yComponent;
} xyVector;

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
	vector->xComponent /= magnitude;
	vector->yComponent /= magnitude;
	return magnitude;
}

// Get the angle between vectors:
static inline double angleBetweenVectors(xyVector * vectorA, xyVector * vectorB)
{
	double dotProduct = (vectorA->xComponent * vectorB->xComponent) + (vectorA->yComponent * vectorB->yComponent);
	double determinant = (vectorA->xComponent * vectorB->yComponent) - (vectorA->yComponent * vectorB->xComponent);

	return atan2(dotProduct, determinant) / 0.01745329;
}

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

int main(int argc, char ** argv)
{
	SDL_Event event;
	int width = 0, height = 0;
	long positionX = 0, positionY = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	uint64_t thisFrameTime = SDL_GetPerformanceCounter(), lastFrameTime = 0;
	double deltaTime = 0, gravityMagnitude = 0, gravityAcceleration = 0, frameAccumulator = 0;	
	bool quit = false, rotatingClockwise = false, rotatingAnticlockwise = false, accelerating = false;
	xyVector positionVector = {100, 100}, velocityVector = {0, 0}, gravityVector = {0, 0}, engineVector = {0.08, 0}, upVector = {0, 0.1};

	// Initialize the SDL library, video, sound, and input:
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL Initialization Error: %s\n", SDL_GetError());
	}

	// Initialize image loading:
	IMG_Init(IMG_INIT_PNG);
	SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "2");
	
	// Create a rectangle to put the ship in:
	SDL_Rect shipRect;
	shipRect.w = 32;
	shipRect.h = 32;
	
	// Create an SDL window and rendering context in that window:
	SDL_Window * window = SDL_CreateWindow("SDL_TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 640, 0);
	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, rendererFlags);

	// Load in all of our textures:
	SDL_Texture * idleTexture, * acceleratingTexture, * clockwiseTexture, * anticlockwiseTexture, * currentTexture,
	    * acceleratingTexture2;
	
	idleTexture = IMG_LoadTexture(renderer, "./11-Ship-Idle.png");
	clockwiseTexture = IMG_LoadTexture(renderer, "./11-Ship-Clockwise.png");
	acceleratingTexture = IMG_LoadTexture(renderer, "./11-Ship-Accelerating.png");
	anticlockwiseTexture = IMG_LoadTexture(renderer, "./11-Ship-Anticlockwise.png");
	acceleratingTexture2 = IMG_LoadTexture(renderer, "./11-Ship-Accelerating-Frame-2.png");

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

        // Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);

		// Calculate the vector between the star and ship:
		xyVectorBetweenPoints(positionVector.xComponent, positionVector.yComponent, width/2, height/2, &gravityVector);

		// Make it into a unit vector:
		gravityMagnitude = normalizeXYVector(&gravityVector);

		// Calculate the gravity between the star and ship:
		gravityAcceleration = (gravityMagnitude >= 45) ?
			(2500 / pow(gravityMagnitude, 2)):
			0;
		
		// Scale the vector:
		multiplyXYVector(&gravityVector, gravityAcceleration);
		
		// Wrap the position if the ship goes off-screen:
		if(positionVector.xComponent > width + 15)
		{
			positionVector.xComponent = 0;
			velocityVector.xComponent *= 0.6;
		}
		if(positionVector.yComponent > height + 15)
		{
			positionVector.yComponent = 0;
			velocityVector.yComponent *= 0.6;
		}
		if(positionVector.xComponent < -15)
		{
			positionVector.xComponent = width;
			velocityVector.xComponent *= 0.6;
		}
		if(positionVector.yComponent < -15)
		{
			positionVector.yComponent = height;
			velocityVector.yComponent *= 0.6;
		}

		// Set the texture to idle:
		currentTexture = idleTexture;
		
		// Rotate the engine vector if needed:
		if(rotatingClockwise)
		{
			rotateXYVector(&engineVector, 0.1 * deltaTime);
			currentTexture = clockwiseTexture;
		}
		if(rotatingAnticlockwise)
		{
			rotateXYVector(&engineVector, -0.1 * deltaTime);
			currentTexture = anticlockwiseTexture;
		}
		
		// Calculate the new current velocity:
		addXYVectorDeltaScaled(&velocityVector, &gravityVector, deltaTime);
		if(accelerating)
		{
			addXYVectorDeltaScaled(&velocityVector, &engineVector, deltaTime);
			frameAccumulator += deltaTime;
			currentTexture = acceleratingTexture;
			if((long)frameAccumulator % 4)
			{
				currentTexture = acceleratingTexture2;
			}
		}
		
		// Calculate the new position:
		addXYVectorDeltaScaled(&positionVector, &velocityVector, deltaTime);
		
		positionX = (long)positionVector.xComponent;
		positionY = (long)positionVector.yComponent;

		// Calculate the position of the sprite:
		shipRect.x = positionX - 15;
		shipRect.y = positionY - 15;
		
		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Draw the ship:
		SDL_RenderCopyEx(renderer, currentTexture, NULL, &shipRect, angleBetweenVectors(&engineVector, &upVector) + 90, NULL, 0);
		
		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Draw a circle in the center as the star:
		DrawCircle(renderer, width/2, height/2, 30);

		// Draw a line representing the velocity:
		SDL_RenderDrawLine(renderer, positionX, positionY,
						   (long)(positionVector.xComponent + velocityVector.xComponent * 15),
						   (long)(positionVector.yComponent + velocityVector.yComponent * 15));

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);
	}
	return 0;
}
// ===========================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-11.c -lSDL2_image -lm" 
// End:
