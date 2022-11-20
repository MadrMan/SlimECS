#include "../../include/slimecs/slimecs.h"
#include <string>

using namespace slimecs;

struct Position
{
	float x;
	float y;
	float z;
};

struct Velocity
{
	float x;
	float y;
};

struct Composition
{
	std::string material;
	int mass;
};

class sqlite3
{
	std::string obi;
};

SLIMECS_COMPONENT_DEFINE(Position);
SLIMECS_COMPONENT_DEFINE(Velocity);
SLIMECS_COMPONENT_DEFINE(Composition);

// Program to showcase every single feature of SlimeECS
int main(int argc, char** argv)
{
	ECSManager em;
	ECSInstance brick = em.CreateInstance(Position{ 1.0f, 2.0f, 3.0f }, Velocity{ 10.2f, 6.0f }, Composition{ "Brick", 60 });
	ECSInstance ufo = em.CreateInstance(Velocity{ 7000.9f, -10.0f }, Composition{ "UFO", 57000 });

	sqlite3* hello = new sqlite3();

	for (auto& [pos, vel] : em.Iterate<Position, Velocity>())
	{
		pos.x += vel.x;
		pos.y += vel.y;
	}
}