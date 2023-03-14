#ifndef SPACEWARPLAYER_H
#define SPACEWARPLAYER_H
#include "xyVector.h"

// A struct storing the needed data to draw a ship:
typedef struct ship
{
	int number;
	xyVector engine;
	xyVector gravity;
	xyVector position;
	xyVector velocity;
	SDL_Rect rectangle;
} ship;

// A struct to store the input state for one player:
typedef struct playerController
{
	SDL_Joystick * joystick;
	SDL_Haptic * haptic;
	int number;
	double turningAmount, acceleratingAmount;
	bool turningClockwise, turningAnticlockwise, accelerating;	
} playerController;

#endif
