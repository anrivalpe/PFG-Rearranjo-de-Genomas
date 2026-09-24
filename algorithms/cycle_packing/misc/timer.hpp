#pragma once

#include <chrono>

#define MAX_RUNTIME 100.0

class Timer {
	std::chrono::time_point<std::chrono::high_resolution_clock> begin;
	std::chrono::time_point<std::chrono::high_resolution_clock> mark;

	public:
	Timer();
    void mark_time();
    double since_last_mark();
	double remaning_time();
	double elapsed_time();
	bool done();
};
