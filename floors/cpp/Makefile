# Makefile for Floor 4 - C++ Jurisdiction
# Production-grade build configuration

CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -Werror -pedantic -O3 -march=native
CXXFLAGS_DEBUG = -std=c++17 -Wall -Wextra -g -fsanitize=address -fsanitize=undefined
TARGET = department_floor
SRC = department_floor.cpp

.PHONY: all clean debug test install benchmark

all: $(TARGET)

$(TARGET): $(SRC)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRC)

debug: $(SRC)
	$(CXX) $(CXXFLAGS_DEBUG) -o $(TARGET)_debug $(SRC)

clean:
	rm -f $(TARGET) $(TARGET)_debug *.o

test: $(TARGET)
	@echo "Testing Floor 4 - C++ Department..."
	@echo '{"method": "get_info"}' | ./$(TARGET) | head -20
	@echo ""
	@echo '{"method": "add_agent", "params": {"agent_id": "test_1", "name": "TestAgent", "role": "Tester"}}' | ./$(TARGET)
	@echo ""
	@echo '{"method": "process_code", "params": {"code": "class Foo { public: void bar() {} };", "operation": "analyze"}}' | ./$(TARGET)

benchmark: $(TARGET)
	@echo "Running performance benchmark..."
	@time -p bash -c 'for i in {1..1000}; do echo "{\"method\": \"get_info\"}" | ./$(TARGET) > /dev/null; done'

install: $(TARGET)
	install -m 755 $(TARGET) /usr/local/bin/cpp_department_floor

.SILENT: test
