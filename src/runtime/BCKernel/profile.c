
#include "platform.h"
#include "profile.h"

#ifdef PROFILE

clock_t startTime;

struct TimeCount
{
	int Times;
	clock_t Duration;
};

struct TimeCount Instructions[256];
clock_t LastStart;
int LastInstruction = -1;

int clock_val()
{
	return clock();
}

void profile_begin()
{
	int i;
	startTime = clock();

	for (i = 0; i < 256; i++)
	{
		Instructions[i].Times = 0;
		Instructions[i].Duration = 0;
	}
}

void profile_end()
{
	int i;
	clock_t endTime = clock_val();
	fprintf(stderr, "Time taken, %li\n\n", endTime - startTime);

	for (i = 0; i < 256; i++)
	{
		if (Instructions[i].Times != 0)
		{
			fprintf(stderr, "Instruction %i\t%i\t%li\n", i, Instructions[i].Times, Instructions[i].Duration);
		}
	}
	fprintf(stderr, "Done!\n");
}

void profile_record(int Tag)
{
	clock_t Now = clock_val();
	if (LastInstruction != -1)
	{
		Instructions[LastInstruction].Times++;
		if (Now - LastStart > 1)
			Instructions[LastInstruction].Duration += (Now - LastStart);
	}
	LastInstruction = Tag;
	LastStart = Now;
}


#endif /* PROFILE */
