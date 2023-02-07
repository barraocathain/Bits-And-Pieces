// SDL Experiment 05, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_timer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

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
	double posX = 0, posY = 0;
	long positionX = 0, positionY = 0;
	double velocityX = 20, velocityY = 0, gravityX = 0, gravityY = 0, gravityMagnitude = 0, gravityAcceleration = 0;

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

	while (!quit)
	{
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

		// Calculate the gravity vector:
		// Calculate the vector between the star and ship:
		gravityX = (width/2 - posX);
		gravityY = (height/2 - posY);

		// Make it into a unit vector:
		gravityMagnitude = sqrt(pow(gravityX, 2) + pow(gravityY, 2));
		gravityX /= gravityMagnitude;
		gravityY /= gravityMagnitude;
		
		// Calculate the gravity between them and scale the vector:
  		if(gravityMagnitude > 15)
		{
			gravityAcceleration = 2 * (2500 / pow(gravityMagnitude, 2));
			gravityX *= gravityAcceleration;
			gravityY *= gravityAcceleration;
		}
		else
		{
			gravityAcceleration = 0.02 * (2500 / pow(gravityMagnitude, 2));
			gravityX *= gravityAcceleration;
			gravityY *= gravityAcceleration;
		}

		// Wrap the position if the ship goes off-screen:
		if(posX > width + 15)
		{
			posX = 0;
			velocityX *= 0.6;
		}
		if(posY > height + 15)
		{
			posY = 0;
			velocityY *= 0.6;
		}
		if(posX < -15)
		{
			posX = width;
			velocityX *= 0.6;
		}
		if(posY < -15)
		{
			posY = height;
			velocityY *= 0.6;
		}

		// Calculate the new current velocity:
		velocityX += gravityX;
		velocityY += gravityY;

		// Calculate the new position:
		posX += velocityX;
		posY += velocityY;

		positionX = (long)posX;
		positionY = (long)posY;

		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Set the colour to green:
		SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);

		// Draw a circle "ship" around the current position:
		DrawCircle(renderer, positionX, positionY, 15);
		
		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Draw a circle in the center as the star:
		DrawCircle(renderer, width/2, height/2, 30);

		// Draw a line representing the velocity:
		SDL_RenderDrawLine(renderer, positionX, positionY, (long)((posX + velocityX * 3)), (long)((posY + velocityY * 3)));
		
		// Present the rendered graphics:
		SDL_RenderPresent(renderer);

		// Delay enough so that we run at 60 frames:
		SDL_Delay(1000 / 60);
	}
	return 0;
}
// ===========================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-05.c  -lm"
// End:
