#include <algorithm>
#include "timer.hpp"
using namespace std::chrono;

Timer::Timer() {
	begin = high_resolution_clock::now();
    mark = begin;
}

void Timer::mark_time() {
	mark = high_resolution_clock::now();
}

double Timer::since_last_mark() {
	auto current = high_resolution_clock::now();
	auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(current - mark);
	return elapsed.count() * 1e-9;
}

double Timer::remaning_time() {
	return std::max(0.0, MAX_RUNTIME - elapsed_time());
}

double Timer::elapsed_time() {
	auto current = high_resolution_clock::now();
	auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(current - begin);
	return elapsed.count() * 1e-9;
}

bool Timer::done() {
	if (elapsed_time() >= MAX_RUNTIME) return true;
	else return false;
}
