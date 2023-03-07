#ifndef XYVECTOR_H
#define XYVECTOR_H
#include <math.h>

// A 2D vector:
typedef struct xyVector
{
	double xComponent;
	double yComponent;
} xyVector;

static inline double angleBetweenVectors(xyVector * vectorA, xyVector * vectorB)
{
	double dotProduct = (vectorA->xComponent * vectorB->xComponent) + (vectorA->yComponent * vectorB->yComponent);
	double determinant = (vectorA->xComponent * vectorB->yComponent) - (vectorA->yComponent * vectorB->xComponent);

	return atan2(dotProduct, determinant) / 0.01745329;
}

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

#endif
