// SDL Experiment 01, Barra Ó Catháin.
// ===================================
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
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
	int width = 0, height = 0, mouseX = 0, mouseY = 0;
	uint32_t rendererFlags = SDL_RENDERER_ACCELERATED;
	
	// Initialize the SDL library, video, sound, and input:
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL Initialization Error: %s\n", SDL_GetError());
	}

	IMG_Init(IMG_INIT_PNG);
	
	// Create an SDL window and rendering context in that window:
	SDL_Window * window = SDL_CreateWindow("SDL_TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 640, 0);
	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, rendererFlags);

	SDL_Texture * targetTexture;
	targetTexture = IMG_LoadTexture(renderer, "./01-TGT.png");
	SDL_Rect targetText;
	targetText.w = 16;
	targetText.h = 8;
	
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
                quit = true;
                break;
            }
        }

		// Store the window's current width and height:
		SDL_GetWindowSize(window, &width, &height);

		// Set the colour to black:
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

		// Clear the screen, filling it with black:
		SDL_RenderClear(renderer);

		// Set the colour to neon green:
		SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);

		// Draw the "radar circle" as large as possible, centered in the window:
		DrawCircle(renderer, width/2, height/2, getRadius(width, height));

		// Draw 16 vertical grid lines, spaced evenly from each other and the window borders:
		for(int verticalPosition = (height - ((height/16) * 16)) / 2;
			verticalPosition <= height; verticalPosition += (height / 16))
		{
			SDL_RenderDrawLine(renderer, 0, verticalPosition, width, verticalPosition);
		}

		// Draw 16 horizontal grid lines, spaced evenly from each other and the window borders:
		for(int horizontalPosition = (width - ((width/16) * 16)) / 2;
			horizontalPosition <= width; horizontalPosition += (width / 16))
		{
			SDL_RenderDrawLine(renderer, horizontalPosition, 0, horizontalPosition, height);
		}

		// Set the colour to yellow:
		SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);

		// Store the mouse pointer's current position in the window:
		SDL_GetMouseState(&mouseX, &mouseY);

		// Draw a line from the center of the window to the mouse pointer:
		SDL_RenderDrawLine(renderer, width/2, height/2, mouseX, mouseY);

		// Draw a circle around the mouse pointer:
		DrawCircle(renderer, mouseX, mouseY, 15);

		// Set the rect at the correct position to put the TGT down:
		targetText.x = mouseX + 20;
		targetText.y = mouseY - 4;
		if(mouseX < width/2)
		{
			targetText.x = mouseX - 36;
		}
		SDL_RenderCopy(renderer, targetTexture, NULL, &targetText);
		// Present the rendered graphics:
		SDL_RenderPresent(renderer);

		// Delay enough so that we run at 144 frames:
		SDL_Delay(1000 / 144);
	}
	return 0;
}
// ===========================================================================================
// Local Variables:
// compile-command: "gcc `sdl2-config --libs --cflags` SDL2-Experiment-01.c  -lSDL2_image -lm"
// End:
