FC = gfortran
FFLAGS ?= -cpp -fbacktrace -ffree-line-length-none -O2 -static -s
ifeq ($(OS),Windows_NT)
FFLAGS += -D_WIN32
endif

SRC_DIR := src
BUILD_DIR := build
BIN_DIR := bin


TARGET := $(BIN_DIR)/ArrayMath.exe
MODFLAGS := -J$(BUILD_DIR) -I$(BUILD_DIR)

OBJECTS := \
	$(BUILD_DIR)/str_upper.o \
	$(BUILD_DIR)/f90getopt.o \
	$(BUILD_DIR)/mrgref.o \
	$(BUILD_DIR)/ArrayMathStats.o \
	$(BUILD_DIR)/ArrayMathTable.o \
	$(BUILD_DIR)/ArrayMath.o

.PHONY: all clean test dirs

all: $(TARGET)

dirs:
	mkdir -p $(BUILD_DIR) $(BIN_DIR)

$(TARGET): $(OBJECTS) | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -o $@ $(OBJECTS)

$(BUILD_DIR)/str_upper.o: $(SRC_DIR)/str_upper.f90 | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

$(BUILD_DIR)/f90getopt.o: $(SRC_DIR)/f90getopt.F90 | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

$(BUILD_DIR)/mrgref.o: $(SRC_DIR)/mrgref.f90 | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

$(BUILD_DIR)/ArrayMathStats.o: $(SRC_DIR)/ArrayMathStats.f90 | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

$(BUILD_DIR)/ArrayMathTable.o: $(SRC_DIR)/ArrayMathTable.f90 | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

$(BUILD_DIR)/ArrayMath.o: $(SRC_DIR)/ArrayMath.f90 $(BUILD_DIR)/f90getopt.o $(BUILD_DIR)/mrgref.o $(BUILD_DIR)/ArrayMathStats.o $(BUILD_DIR)/ArrayMathTable.o | dirs
	$(FC) $(FFLAGS) $(MODFLAGS) -c $< -o $@

test: $(TARGET)
	powershell -NoProfile -ExecutionPolicy Bypass -File 02_autotest.ps1 -ExePath $(TARGET)

clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)
