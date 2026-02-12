# Fortran Floor 24 - Scientific Computation Jurisdiction
# Makefile for building the department floor

FC = gfortran
FFLAGS = -std=f2008 -Wall -Wextra -fcheck=all -fbounds-check -O2
TARGET = department_floor
SOURCE = department_floor.f90

.PHONY: all clean test

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(FC) $(FFLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET) *.o *.mod

test: $(TARGET)
	@echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | ./$(TARGET)
	@echo ""
	@echo '{"jsonrpc":"2.0","method":"get_floor_info","id":2}' | ./$(TARGET)
	@echo ""

install:
	@echo "Fortran floor built successfully"
	@echo "Executable: $(TARGET)"

.DEFAULT_GOAL := all
