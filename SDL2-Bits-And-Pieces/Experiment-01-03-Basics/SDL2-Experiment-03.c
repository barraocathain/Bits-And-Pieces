// SDL Experiment 03, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_timer.h>
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

// Get the largest radius for a circle that can fit in the width and height of a rectangle:
static inline int getRadius(int width, int height)
{
	return (width/2 < height/2) ? width/2 : height/2;
}


int main(int argc, char ** argv)
{
	SDL_Event event;
	bool quit = false;
	int width = 0, height = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	long positionX = 320, positionY = 320, velocityX = 0, velocityY = 0;
	bool accelLeft = false, accelRight = false, accelUp = false, accelDown = false;

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
				// Begin accelerating in the direction that is pressed:
				case SDL_KEYDOWN:
				{
					switch (event.key.keysym.sym)
					{
						case SDLK_LEFT:
						{
							accelLeft = true;
							break;
						}
						case SDLK_RIGHT:
						{
							accelRight = true;
							break;
						}
						case SDLK_UP:
						{
							accelUp = true;
							break;
						}
						case SDLK_DOWN:
						{
							accelDown = true;
							break;
						}					  
						default:
						{
							break;
						}
					}
					break;
				}
				// Stop accelerating in the direction that is released:
				case SDL_KEYUP:
				{
					switch (event.key.keysym.sym)
					{
						case SDLK_LEFT:
						{
							accelLeft = false;
							break;
						}
						case SDLK_RIGHT:
						{
							accelRight = false;
							break;
						}
						case SDLK_UP:
						{
							accelUp = false;
							break;
						}
						case SDLK_DOWN:
						{
							accelDown = false;
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

		// Accelerate:
		if(accelLeft)
		{
			velocityX -= 2;
		}

		if(accelRight)
		{
			velocityX += 2;
		}

		if(accelUp)
		{
			velocityY -= 2;
		}

		if(accelDown)
		{
			velocityY += 2;
		}

		// Limit velocity:
		if(velocityX > 10)
		{
			velocityX = 10;
		}
		if(velocityY > 10)
		{
			velocityY = 10;
		}
		if(velocityX < -10)
		{
			velocityX = -10;
		}
		if(velocityY < -10)
		{
			velocityY = -10;
		}
		
		// Deccelerate:
		if(velocityX != 0)
		{
			velocityX = (velocityX < 0) ? velocityX + 1 : velocityX - 1;
		}

		if(velocityY != 0)
		{
			velocityY = (velocityY < 0) ? velocityY + 1 : velocityY - 1;
		}
		
		// Move the position:
		if(velocityX != 0)
		{
			positionX += velocityX;
			if(positionX < 0)
			{
				positionX = 0;
			}
			if(positionX > 640)
			{
				positionX = 640;
			}
		}
		if(velocityY != 0)
		{
			positionY += velocityY;
			if(positionY < 0)
			{
				positionY = 0;
			}
			if(positionY > 640)
			{
				positionY = 640;
			}
		}
		
		// Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);

		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);
		
		// Draw a line from the center of the window to the position pointer:
		SDL_RenderDrawLine(renderer, width/2, height/2, positionX, positionY);

		// Draw a circle around the position pointer:
		DrawCircle(renderer, positionX, positionY, 15);

		// Present the rendered graphics:
		SDL_RenderPresent(renderer);

		// Delay enough so that we run at 144 frames:
		SDL_Delay(1000 / 60);
	}
	return 0;
}
// ===========================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-03.c  -lm"
// End:
