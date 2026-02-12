# Makefile for Floor 3 - C Jurisdiction
# Production-grade build configuration

CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -Werror -pedantic -O2 -D_POSIX_C_SOURCE=200809L
CFLAGS_DEBUG = -std=c11 -Wall -Wextra -g -fsanitize=address -fsanitize=undefined
TARGET = department_floor
SRC = department_floor.c

.PHONY: all clean debug test install

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC)

debug: $(SRC)
	$(CC) $(CFLAGS_DEBUG) -o $(TARGET)_debug $(SRC)

clean:
	rm -f $(TARGET) $(TARGET)_debug *.o

test: $(TARGET)
	@echo "Testing Floor 3 - C Department..."
	@echo '{"method": "get_info"}' | ./$(TARGET) | head -20
	@echo ""
	@echo '{"method": "add_agent", "params": {"agent_id": "test_1", "name": "TestAgent", "role": "Tester"}}' | ./$(TARGET)
	@echo ""
	@echo '{"method": "process_code", "params": {"code": "#include <stdio.h>\\nint main() { return 0; }", "operation": "analyze"}}' | ./$(TARGET)

install: $(TARGET)
	install -m 755 $(TARGET) /usr/local/bin/c_department_floor

.SILENT: test
