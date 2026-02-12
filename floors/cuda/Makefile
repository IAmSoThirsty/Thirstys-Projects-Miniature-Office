# CUDA/GPU Floor 26 - Parallel Compute Jurisdiction
# Makefile for building the department floor

NVCC = nvcc
NVCC_FLAGS = -std=c++11 -O2 -arch=sm_35 -Xcompiler -Wall
TARGET = department_floor
SOURCE = department_floor.cu

.PHONY: all clean test info

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(NVCC) $(NVCC_FLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET) *.o

test: $(TARGET)
	@echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | ./$(TARGET)
	@echo ""
	@echo '{"jsonrpc":"2.0","method":"get_floor_info","id":2}' | ./$(TARGET)
	@echo ""

info:
	@echo "CUDA Floor 26 Build Information"
	@echo "================================"
	@which nvcc || echo "nvcc not found - CUDA Toolkit required"
	@nvcc --version 2>/dev/null || echo "Cannot query CUDA version"
	@nvidia-smi -L 2>/dev/null || echo "No NVIDIA GPUs detected"

install:
	@echo "CUDA floor built successfully"
	@echo "Executable: $(TARGET)"
	@echo "Note: Requires NVIDIA GPU with CUDA support"

# Advanced build options
debug: NVCC_FLAGS += -g -G -lineinfo
debug: $(TARGET)

# Build for specific GPU architecture
sm_50: NVCC_FLAGS = -std=c++11 -O2 -arch=sm_50 -Xcompiler -Wall
sm_50: $(TARGET)

sm_60: NVCC_FLAGS = -std=c++11 -O2 -arch=sm_60 -Xcompiler -Wall
sm_60: $(TARGET)

sm_70: NVCC_FLAGS = -std=c++11 -O2 -arch=sm_70 -Xcompiler -Wall
sm_70: $(TARGET)

sm_80: NVCC_FLAGS = -std=c++11 -O2 -arch=sm_80 -Xcompiler -Wall
sm_80: $(TARGET)

.DEFAULT_GOAL := all
