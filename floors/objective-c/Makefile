# Makefile for Floor 14 - Objective-C Department Floor

# Compiler and flags
CC = clang
OBJC_FLAGS = -fblocks -Wall -Wextra

# Detect platform
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
    # macOS - use Foundation framework and ARC
    LDFLAGS = -framework Foundation
    OBJC_FLAGS += -framework Foundation -fobjc-arc
else ifeq ($(UNAME_S),Linux)
    # Linux - use GNUstep (no ARC support on legacy runtime)
    OBJC_FLAGS += $(shell gnustep-config --objc-flags 2>/dev/null || echo "")
    LDFLAGS = $(shell gnustep-config --base-libs 2>/dev/null || echo "-lgnustep-base -lobjc") -ldispatch
    # Note: GNUstep uses legacy runtime, ARC not supported
endif

# Source files
SOURCES = department_floor.m
HEADERS = department_floor.h
TARGET = department_floor

# Build target
all: $(TARGET)

$(TARGET): $(SOURCES) $(HEADERS)
	$(CC) $(OBJC_FLAGS) -o $(TARGET) $(SOURCES) $(LDFLAGS)

# Run target
run: $(TARGET)
	./$(TARGET)

# Test target
test: $(TARGET)
	@echo '{"method":"get_info","params":{}}' | ./$(TARGET)

# Clean target
clean:
	rm -f $(TARGET)
	rm -f *.o

# Install GNUstep on Linux (requires sudo)
install-deps-linux:
	sudo apt-get update
	sudo apt-get install -y gnustep-devel gobjc libobjc-4-dev clang libgnustep-base-dev libblocksruntime-dev

# Help target
help:
	@echo "Objective-C Department Floor - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all              - Build the department floor (default)"
	@echo "  run              - Build and run the floor"
	@echo "  test             - Build and test with sample request"
	@echo "  clean            - Remove built files"
	@echo "  install-deps-linux - Install GNUstep dependencies (Linux only, requires sudo)"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Platform: $(UNAME_S)"
	@echo "Note: ARC is only supported on macOS. Linux/GNUstep uses manual reference counting."

.PHONY: all run test clean install-deps-linux help
